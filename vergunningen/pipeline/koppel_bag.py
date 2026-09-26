"""Koppelt BAG-panden met een vergunningdatum aan zaken uit de bekendmakingen, op plaats en tijd.

Voor elk pand met Bouwvergunning verleend vanaf juni 2021 (de landelijke reeks bekendmakingen begint 5 juli 2021) wordt
binnen STRAAL meter de zaak gezocht waarvan het tijdvenster de vergunningdatum van het pand omvat: van veertien dagen
voor de aanvraagpublicatie tot 180 dagen na de uitkomstpublicatie (of 400 dagen na de aanvraag als er geen uitkomst is).
Bij meer kandidaten wint de zaak met uitkomst verleend, dan de kleinste afstand in dagen tussen uitkomst en
vergunningdatum, dan de kleinste afstand in meters. Een zaak kan meer panden krijgen (een rij woningen is een zaak).

Uitvoer: tabel zaak_bag in de database (een rij per gekoppeld pand) en twee csv's in de analysemap:
zaak_bag.csv (per zaak: panden, woningen, eerste bouwstart, niet gerealiseerd) en bag_dekking_gemeente_jaar.csv
(per gemeentecode en jaar van vergunning: vergunde woonpanden, waarvan met een zaak binnen de straal). De dekking is de
toets of een gemeente haar besluiten publiceert waar de BAG ze telt (het Amersfoort-punt uit het ontwerp).

Gebruik: py -3.13 koppel_bag.py [straal_m]
"""
import csv
import datetime as dt
import sqlite3
import sys
from collections import defaultdict

from config import DATA_DIR, DB_PAD

PANDEN = DATA_DIR / "analyse" / "bag_panden.csv"
UIT_ZAAK = DATA_DIR / "analyse" / "zaak_bag.csv"
UIT_DEKKING = DATA_DIR / "analyse" / "bag_dekking_gemeente_jaar.csv"
VANAF = "2021-06-01"
CEL = 100.0  # rastercel van de ruimtelijke index in meters; de straal mag niet groter zijn dan de cel


def datum(s):
    return dt.date.fromisoformat(s[:10]) if s else None


def main(argv):
    straal = float(argv[0]) if argv else 100.0
    assert straal <= CEL
    conn = sqlite3.connect(DB_PAD, timeout=900)
    conn.execute("DROP TABLE IF EXISTS zaak_bag")
    conn.execute("""CREATE TABLE zaak_bag (zaak_id INTEGER, pand_id TEXT, afstand_m REAL, dagen_uitkomst_vergunning INTEGER,
                    n_woon INTEGER, verg_begin TEXT, start_begin TEXT, gebruik_begin TEXT, niet_gerealiseerd_begin TEXT)""")
    # ruimtelijke index op de zaken met locatie: cel -> lijst (zaak_id, x, y, begin, einde, verleend, uitkomstdatum)
    rooster = defaultdict(list)
    n_zaken = 0
    for zaak_id, x, y, d_aanvr, d_uitk, uitkomst, eerste, laatste in conn.execute(
        "SELECT zaak_id, x, y, datum_aanvraag, datum_uitkomst, uitkomst, eerste_datum, laatste_datum FROM zaak "
        "WHERE x IS NOT NULL AND organisatietype = 'gemeente'"
    ):
        begin = datum(d_aanvr) or datum(eerste)
        uit = datum(d_uitk)
        einde = (uit + dt.timedelta(days=180)) if uit else (begin + dt.timedelta(days=400))
        rooster[(int(x // CEL), int(y // CEL))].append((zaak_id, x, y, begin - dt.timedelta(days=14), einde, uitkomst == "verleend", uit))
        n_zaken += 1
    print(f"{n_zaken} zaken in de index")
    rijen = []
    dekking = defaultdict(lambda: [0, 0, 0, 0])  # (gemeentecode, jaar) -> panden, gekoppeld, woonpanden, woon gekoppeld
    per_zaak = defaultdict(lambda: {"panden": 0, "woningen": 0, "vergunning": None, "bouwstart": None, "gebruik": None, "nietger": 0})
    n_panden = 0
    with open(PANDEN, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f):
            if r["verg_begin"] < VANAF or not r["x"] or not r["gemeentecode"]:
                continue
            n_panden += 1
            x, y = float(r["x"]), float(r["y"])
            v = datum(r["verg_begin"])
            n_woon = int(r["n_woon"] or 0)
            sleutel = (r["gemeentecode"], r["verg_begin"][:4])
            dekking[sleutel][0] += 1
            if n_woon:
                dekking[sleutel][2] += 1
            cx, cy = int(x // CEL), int(y // CEL)
            beste = None
            for dx in (-1, 0, 1):
                for dy in (-1, 0, 1):
                    for zaak_id, zx, zy, begin, einde, verleend, uit in rooster.get((cx + dx, cy + dy), ()):
                        if not (begin <= v <= einde):
                            continue
                        afst = ((zx - x) ** 2 + (zy - y) ** 2) ** 0.5
                        if afst > straal:
                            continue
                        dagen = abs((uit - v).days) if uit else 9999
                        kandidaat = (0 if verleend else 1, dagen, afst, zaak_id, uit)
                        if beste is None or kandidaat < beste:
                            beste = kandidaat
            if beste is None:
                continue
            _, dagen, afst, zaak_id, uit = beste
            dekking[sleutel][1] += 1
            if n_woon:
                dekking[sleutel][3] += 1
            rijen.append((zaak_id, r["identificatie"], round(afst, 1), None if dagen == 9999 else (uit - v).days, n_woon,
                          r["verg_begin"], r["start_begin"] or None, r["gebruik_begin"] or None, r["niet_gerealiseerd_begin"] or None))
            z = per_zaak[zaak_id]
            z["panden"] += 1
            z["woningen"] += n_woon
            if z["vergunning"] is None or r["verg_begin"] < z["vergunning"]:
                z["vergunning"] = r["verg_begin"]
            bouw = r["start_begin"] or r["gebruik_begin"]
            if bouw and (z["bouwstart"] is None or bouw < z["bouwstart"]):
                z["bouwstart"] = bouw
            if r["gebruik_begin"] and (z["gebruik"] is None or r["gebruik_begin"] < z["gebruik"]):
                z["gebruik"] = r["gebruik_begin"]
            if r["niet_gerealiseerd_begin"]:
                z["nietger"] += 1
            if len(rijen) >= 50000:
                conn.executemany("INSERT INTO zaak_bag VALUES (?,?,?,?,?,?,?,?,?)", rijen)
                conn.commit()
                rijen = []
    conn.executemany("INSERT INTO zaak_bag VALUES (?,?,?,?,?,?,?,?,?)", rijen)
    conn.execute("CREATE INDEX IF NOT EXISTS ix_zaak_bag_zaak ON zaak_bag(zaak_id)")
    conn.commit()
    n_gekoppeld = conn.execute("SELECT count(*) FROM zaak_bag").fetchone()[0]
    print(f"{n_panden} panden vergund vanaf {VANAF}, {n_gekoppeld} gekoppeld aan {len(per_zaak)} zaken (straal {straal:.0f} m)")
    with open(UIT_ZAAK, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["zaak_id", "bag_panden", "bag_woningen", "bag_vergunning", "bag_bouwstart", "bag_gebruik", "bag_niet_gerealiseerd"])
        for zaak_id, z in sorted(per_zaak.items()):
            w.writerow([zaak_id, z["panden"], z["woningen"], z["vergunning"] or "", z["bouwstart"] or "", z["gebruik"] or "", z["nietger"]])
    with open(UIT_DEKKING, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["gemeentecode", "jaar", "panden", "panden_gekoppeld", "woonpanden", "woonpanden_gekoppeld"])
        for (gm, jaar), (a, b, c, d) in sorted(dekking.items()):
            w.writerow([gm, jaar, a, b, c, d])
    tot = [sum(v[i] for v in dekking.values()) for i in range(4)]
    print(f"dekking landelijk: panden {tot[1]} van {tot[0]} ({100 * tot[1] / max(tot[0], 1):.1f} procent), "
          f"woonpanden {tot[3]} van {tot[2]} ({100 * tot[3] / max(tot[2], 1):.1f} procent)")
    conn.close()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
