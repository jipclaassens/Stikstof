"""Grote woningbouwprojecten op projectniveau: welke projecten, waar, met welke natuurprocedure en welk beroep, en hoe lang.

Een project is een zaak met een aanvraagpublicatie die volgens de titel of volgens de gekoppelde BAG-panden minstens
tien woningen omvat (de analyse kiest daarna de drempel, standaard twintig, en twaalf als ondergrens van de voorrangsregel
van de Raad van State). Per project komen erbij:
- ligging en blootstelling uit zaken.csv en zaak_kenmerken.csv (afstand tot habitat, depositie 2018, veehouderij);
- het BAG-vervolg uit zaak_bag.csv (woningen, vroegste vergunning, bouwstart, ingebruikname, niet gerealiseerd);
- provinciale natuurbesluiten uit de natuurstroom (natuur\\natuurvergunningen.sqlite): het aantal publicaties over
  gebiedsbescherming of de Wet natuurbescherming binnen 500 meter van het project en binnen drie jaar voor tot drie jaar na
  de aanvraag, de vroegste datum daarvan, en apart die met een woningbouwwoord in de titel binnen 1 kilometer; ook de
  soortenontheffingen binnen 250 meter;
- beroep uit de kandidaatkoppeling van uitspraken aan zaken (tabel uitspraak_zaak, rechtspraak.py koppel) met de
  verdiepte lezing per uitspraak (bron\\rechtspraak_verdieping\\uitspraken_verdiept.csv): aantal uitspraken, of stikstof
  als grond is aangevoerd en of die slaagde.
De koppeling met natuurbesluiten is op plaats en tijd, niet op sleutel; een provinciaal besluit binnen 500 meter kan over
een naburige boerderij gaan. Het veld met een woningbouwwoord in de titel is de strengere variant.
Het veld telt zegt of de zaak als project meetelt: het woningaantal uit de titel telt tenzij de zaak sloop of kap is (dan
gaat het om gesloopte woningen), het aantal uit de BAG alleen bij nieuwbouw of een titel zonder soort, want een sloop-,
kap- of verbouwzaak kan het nieuwe pand naast zich toegewezen krijgen als de eigen bouwaanvraag ontbreekt. Het veld
dubbel_van wijst naar een eerdere zaak in dezelfde gemeente met hetzelfde woningaantal (uit titel of BAG) binnen 250 meter
en twee jaar: dezelfde aanvraag twee keer gepubliceerd, een tweede fase, of een zaak met het aantal in de titel naast de zaak
die de BAG-panden kreeg.

Gebruik: py -3.13 projecten.py
Uitvoer: analyse\\projecten.csv (puntkomma), een rij per project.
"""
import csv
import datetime as dt
import os
import re
import sqlite3
import sys
from collections import defaultdict

from config import DATA_DIR, DB_PAD

ANALYSE = DATA_DIR / "analyse"
NATUUR_DB = DATA_DIR / "natuur" / "natuurvergunningen.sqlite"
VERDIEPT = DATA_DIR / "bron" / "rechtspraak_verdieping" / "uitspraken_verdiept.csv"
UIT = ANALYSE / "projecten.csv"
DREMPEL = 10
CEL = 1000.0

GEBIED = re.compile(r"natura|gebiedsbescherming|stikstof|natuurvergunning|\bnbw\b|nb-wet|natuurbeschermingswet|\bwnb\b|passende beoordeling|salder|natuurbescherming", re.I)
SOORTEN = re.compile(r"\b(ontheffing(en)?|soorten(bescherming)?|flora|fauna|ff-?wet|fauna-activiteit|vleermuis|huismus|gierzwaluw|rugstreeppad|nesten|rustplaatsen)\b", re.I)
STIKSTOF = re.compile(r"natura|stikstof|gebiedsbescherming|passende beoordeling|salder|aerius|depositie", re.I)
WONING = re.compile(r"\b(woning(en|bouw)?|woonwijk|appartement(en)?|wooneenhe|nieuwbouwwijk|woontoren|huisvesting)\b", re.I)


def datum(s):
    try:
        return dt.date.fromisoformat(s[:10]) if s else None
    except ValueError:
        return None


def getal(s):
    try:
        return float(s) if s not in (None, "") else 0.0
    except ValueError:
        return 0.0


def main(argv):
    zb = {}
    with open(ANALYSE / "zaak_bag.csv", encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            zb[r["zaak_id"]] = r
    zk = {}
    with open(ANALYSE / "zaak_kenmerken.csv", encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            zk[r["sleutel"]] = r
    projecten = {}
    with open(ANALYSE / "zaken.csv", encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            wt = getal(r["woningen"])
            wb = getal(zb.get(r["zaak_id"], {}).get("bag_woningen"))
            if max(wt, wb) >= DREMPEL:
                projecten[r["zaak_id"]] = {"z": r, "wt": wt, "wb": wb}
    print(f"{len(projecten)} projecten met minstens {DREMPEL} woningen volgens titel of BAG")
    conn = sqlite3.connect(DB_PAD, timeout=900)
    ids = list(projecten)
    for i in range(0, len(ids), 900):
        deel = ids[i:i + 900]
        for zid, x, y in conn.execute(f"SELECT zaak_id, x, y FROM zaak WHERE zaak_id IN ({','.join('?' * len(deel))})", deel):
            projecten[str(zid)]["x"], projecten[str(zid)]["y"] = x, y
    # beroep: kandidaatkoppeling plus verdiepte lezing
    verdiept = {}
    with open(VERDIEPT, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            verdiept[r["ecli"]] = r
    beroep = defaultdict(list)
    for ecli, zid in conn.execute("SELECT ecli, zaak_id FROM uitspraak_zaak"):
        if str(zid) in projecten:
            beroep[str(zid)].append(verdiept.get(ecli, {"ecli": ecli}))
    conn.close()
    # natuurbesluiten van provincies en Rijk met locatie. Een titel "Wet natuurbescherming, locatie ..." is in stedelijk gebied
    # meestal een soortenontheffing bij renovatie of sloop; een stikstofprocedure is het pas als de titel of de tekst
    # stikstof, AERIUS, Natura 2000, passende beoordeling of salderen noemt (tekstvlaggen uit tekstvlag.py)
    conn = sqlite3.connect(DB_PAD, timeout=900)
    stikstof_ids = {r[0] for r in conn.execute(
        "SELECT identifier FROM publicatie_tekstvlag WHERE term IN ('stikstof','stikstofdepositie','stikstofberekening','aerius',"
        "'natura2000','natura2000_activiteit','passende_beoordeling','intern_salderen','extern_salderen')")}
    conn.close()
    print(f"{len(stikstof_ids)} publicaties met een stikstofvlag in de tekst")
    nconn = sqlite3.connect(f"file:{NATUUR_DB}?mode=ro", uri=True)
    rooster = defaultdict(list)
    n_nat = 0
    q = """SELECT p.identifier, p.available, p.titel, l.x, l.y FROM publicatie p JOIN publicatie_locatie l ON l.identifier = p.identifier
           WHERE p.organisatietype IN ('provincie', 'ministerie') AND l.x IS NOT NULL
             AND (l.locatietype IN ('Punt', 'Adres', 'Perceel') OR (l.locatietype = 'Vlak' AND l.uitgestrektheid_m <= 1000))"""
    # alleen precieze locaties: woonplaats-, gemeente-, buurt- en provinciemarkeringen leggen een punt midden in een kern
    # en zouden elk project in die kern aan het besluit koppelen
    for ident, av, titel, x, y in nconn.execute(q):
        titel = titel or ""
        if ident in stikstof_ids or STIKSTOF.search(titel):
            soort = "gebied"
        elif SOORTEN.search(titel) or GEBIED.search(titel):
            soort = "soorten"  # soortenontheffing of Wet natuurbescherming zonder stikstof in titel of tekst
        else:
            continue
        if not av:
            continue
        rooster[(int(x // CEL), int(y // CEL))].append((ident, datum(av), x, y, soort, bool(WONING.search(titel))))
        n_nat += 1
    nconn.close()
    print(f"{n_nat} locaties van natuurbesluiten in de index")
    # welk woningaantal telt: dat uit de titel behalve bij sloop en kap, dat uit de BAG alleen bij nieuwbouw of een titel zonder soort
    for p in projecten.values():
        p["soort"] = p["z"].get("soort", "")
        p["wt_eff"] = p["wt"] if p["soort"] != "sloop_kap" else 0
        p["wb_eff"] = p["wb"] if p["soort"] in ("nieuwbouw_woning", "nieuwbouw_overig", "overig") else 0
        p["eff"] = max(p["wt_eff"], p["wb_eff"])
    # dubbele projecten: zelfde gemeente, zelfde woningaantal, binnen 250 meter en twee jaar. Het aantal kan bij de ene zaak uit
    # de titel en bij de andere uit de BAG komen: een pand hoort bij een zaak, dus twee zaken met hetzelfde aantal op dezelfde plek
    # zijn een aanvraag en een tweede publicatie of fase van hetzelfde project
    dubbel = {}
    per_sleutel = defaultdict(list)
    for zid, p in sorted(projecten.items(), key=lambda kv: (kv[1]["z"]["datum_aanvraag"], kv[0])):
        if p["eff"] < DREMPEL or p.get("x") is None:
            continue
        d0 = datum(p["z"]["datum_aanvraag"])
        for ander, ax, ay, ad in per_sleutel[(p["z"]["gemeente"], p["eff"])]:
            if d0 and ad and (d0 - ad).days <= 730 and ((p["x"] - ax) ** 2 + (p["y"] - ay) ** 2) ** 0.5 <= 250:
                dubbel[zid] = ander
                break
        per_sleutel[(p["z"]["gemeente"], p["eff"])].append((dubbel.get(zid, zid), p["x"], p["y"], d0))
    print(f"{len(dubbel)} projecten zijn een tweede publicatie van een eerder project")
    kop = ["zaak_id", "gemeente", "datum_aanvraag", "kwartaal", "soort", "telt", "dubbel_van", "x", "y", "woningen_titel", "bag_woningen", "woningen", "nabij", "dist_m",
           "ndep_2018", "nh3_1km_kg", "uitkomst", "verleend", "geweigerd", "ingetrokken", "geen", "doorlooptijd", "datum_uitkomst",
           "bag_vergunning", "bag_bouwstart", "bag_gebruik", "bag_niet_gerealiseerd", "dagen_aanvraag_bagvergunning",
           "n_natuur_gebied_500m", "eerste_natuur_gebied", "n_natuur_woningbouw_1km", "n_natuur_soorten_250m",
           "n_uitspraken", "stikstof_grond", "stikstof_gegrond", "beroep_uitkomsten"]
    with open(UIT, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(kop)
        for zid, p in projecten.items():
            z = p["z"]
            x, y = p.get("x"), p.get("y")
            d0 = datum(z["datum_aanvraag"])
            n_g = n_wb = n_s = 0
            eerste = None
            if x is not None and d0:
                van, tot = d0 - dt.timedelta(days=3 * 365), d0 + dt.timedelta(days=3 * 365)
                cx, cy = int(x // CEL), int(y // CEL)
                gezien = set()
                for dx in (-1, 0, 1):
                    for dy in (-1, 0, 1):
                        for ident, dn, nx, ny, soort, wb in rooster.get((cx + dx, cy + dy), ()):
                            if ident in gezien or not dn or not (van <= dn <= tot):
                                continue
                            afst = ((nx - x) ** 2 + (ny - y) ** 2) ** 0.5
                            if soort == "gebied" and afst <= 500:
                                n_g += 1
                                gezien.add(ident)
                                eerste = dn if eerste is None or dn < eerste else eerste
                            if soort == "gebied" and wb and afst <= 1000:
                                n_wb += 1
                            if soort == "soorten" and afst <= 250:
                                n_s += 1
                                gezien.add(ident)
            br = beroep.get(zid, [])
            st_grond = int(any(b.get("stikstof_grond") in ("1", "ja", "True") for b in br))
            st_gegrond = int(any((b.get("stikstof_oordeel") or "").startswith("gegrond") for b in br))
            zbr = zb.get(zid, {})
            dv = datum(zbr.get("bag_vergunning"))
            k = zk.get(zid, {})
            w.writerow([zid, z["gemeente"], z["datum_aanvraag"], z["kwartaal"], p["soort"], int(p["eff"] >= DREMPEL), dubbel.get(zid, ""),
                        "" if x is None else round(x), "" if y is None else round(y),
                        int(p["wt"]), int(p["wb"]), int(p["eff"]), z["nabij"], z["dist_m"], k.get("ndep_2018", ""), k.get("nh3_1km_kg", ""),
                        z["uitkomst"], z["verleend"], z["geweigerd"], z["ingetrokken"], z["geen"], z["doorlooptijd"], z["datum_uitkomst"],
                        zbr.get("bag_vergunning", ""), zbr.get("bag_bouwstart", ""), zbr.get("bag_gebruik", ""), zbr.get("bag_niet_gerealiseerd", ""),
                        (dv - d0).days if dv and d0 else "", n_g, eerste.isoformat() if eerste else "", n_wb, n_s,
                        len(br), st_grond, st_gegrond, ",".join(sorted({b.get("uitkomst", "") for b in br if b.get("uitkomst")}))])
    print(f"geschreven: {UIT}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
