"""Meetuitvoer voor de stikstofanalyse: zaken per gemeente, kwartaal en afstandsklasse tot stikstofgevoelig habitat.

Gebruik: py -3.13 meet.py [uitvoermap]
Schrijft naar de uitvoermap (standaard <datamap>/uitvoer):
  zaken_gemeente_kwartaal_klasse.csv   per gemeente, kwartaal van de eerste aanvraagpublicatie en afstandsklasse:
                                        aantal aanvragen, uitkomsten, mediaan doorlooptijd, verlengingen
  zaken_kwartaal_klasse.csv            hetzelfde landelijk (alle gemeenten samen), plus de regimeperiode
  dekking_gemeente_jaar.csv            per gemeente en jaargang: publicaties, zaken, aandeel fase uit titel en tekst,
                                        aandeel aanvragen met uitkomst
De klasse komt uit afstand.py (dist2n2000_m op de hoofdlocatie van de zaak); zaken zonder locatie krijgen "onbekend".
Doorlooptijd is aanvraagpublicatie tot uitkomstpublicatie, mediaan in dagen, alleen over zaken met uitkomst verleend.

Regimeperioden voor de stikstofvraag (grenzen op de datum van de eerste aanvraagpublicatie): voor de PAS-uitspraak
(tot 29 mei 2019), na de PAS-uitspraak, bouwvrijstelling (1 juli 2021 tot 2 november 2022), na Porthos, Omgevingswet
(vanaf 1 januari 2024), na de uitspraak over intern salderen (vanaf 18 december 2024).
"""
import csv
import datetime as dt
import statistics
import sys
from collections import defaultdict
from pathlib import Path

import sqlite3

from config import DATA_DIR, DB_PAD

PERIODEN = [
    ("2019-05-29", "voor_PAS_uitspraak"),
    ("2021-07-01", "na_PAS_uitspraak"),
    ("2022-11-02", "bouwvrijstelling"),
    ("2024-01-01", "na_Porthos"),
    ("2024-12-18", "Omgevingswet"),
    ("9999-12-31", "na_intern_salderen"),
]
UITKOMSTEN = ("verleend", "geweigerd", "ingetrokken", "vergunningvrij", "besluit_onbekend")


def periode(datum):
    for grens, naam in PERIODEN:
        if datum < grens:
            return naam
    return PERIODEN[-1][1]


def kwartaal(datum):
    d = dt.date.fromisoformat(datum)
    return f"{d.year}K{(d.month - 1) // 3 + 1}"


def tel(rijen, sleutel):
    """Aggregeert zaken met een aanvraagdatum naar de gegeven sleutelfunctie."""
    agg = defaultdict(lambda: {"aanvragen": 0, "verlengingen": 0, "doorlooptijden": [], **{u: 0 for u in UITKOMSTEN}, "geen": 0})
    for creator, datum_aanvraag, uitkomst, doorlooptijd, n_verl, klasse in rijen:
        a = agg[sleutel(creator, datum_aanvraag, klasse)]
        a["aanvragen"] += 1
        a["verlengingen"] += n_verl or 0
        if uitkomst in UITKOMSTEN:
            a[uitkomst] += 1
        else:
            a["geen"] += 1
        if uitkomst == "verleend" and doorlooptijd is not None:
            a["doorlooptijden"].append(doorlooptijd)
    return agg


def schrijf(pad, kop, agg, sleutelnamen):
    with open(pad, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(sleutelnamen + kop)
        for sleutel in sorted(agg):
            a = agg[sleutel]
            med = round(statistics.median(a["doorlooptijden"])) if a["doorlooptijden"] else ""
            w.writerow(list(sleutel) + [a["aanvragen"]] + [a[u] for u in UITKOMSTEN] + [a["geen"], med, a["verlengingen"]])


def main(argv):
    uit = Path(argv[0]) if argv else DATA_DIR / "uitvoer"
    uit.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(DB_PAD, timeout=120)
    kolommen = [r[1] for r in conn.execute("PRAGMA table_info(zaak)")]
    klasse_expr = "COALESCE(n2000_klasse, 'onbekend')" if "n2000_klasse" in kolommen else "'onbekend'"
    rijen = conn.execute(f"""SELECT creator, datum_aanvraag, uitkomst, doorlooptijd_dagen, n_verlengingen, {klasse_expr}
                             FROM zaak WHERE organisatietype='gemeente' AND datum_aanvraag IS NOT NULL""").fetchall()
    kop = ["aanvragen"] + list(UITKOMSTEN) + ["geen_uitkomst", "mediaan_dagen_verleend", "verlengingen"]
    schrijf(uit / "zaken_gemeente_kwartaal_klasse.csv", kop,
            tel(rijen, lambda c, d, k: (c, kwartaal(d), k)), ["gemeente", "kwartaal", "n2000_klasse"])
    schrijf(uit / "zaken_kwartaal_klasse.csv", kop,
            tel(rijen, lambda c, d, k: (kwartaal(d), periode(d), k)), ["kwartaal", "periode", "n2000_klasse"])
    # dekking per gemeente en jaargang
    with open(uit / "dekking_gemeente_jaar.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["gemeente", "jaargang", "publicaties", "fase_uit_titel", "fase_uit_tekst", "onbepaald_of_besluit",
                    "aanvragen", "aanvragen_met_uitkomst"])
        heeft_tekst = "fase_tekst" in [r[1] for r in conn.execute("PRAGMA table_info(publicatie)")]
        tekst_expr = "SUM(fase_tekst IS NOT NULL)" if heeft_tekst else "0"
        rest_expr = ("SUM(fase_tekst IS NULL AND fase_titel IN ('besluit','onbepaald'))" if heeft_tekst
                     else "SUM(fase_titel IN ('besluit','onbepaald'))")
        pub = {(c, j): (n, t, r) for c, j, n, t, r in conn.execute(f"""
            SELECT creator, jaargang, COUNT(*), SUM(fase_titel NOT IN ('besluit','onbepaald')), {rest_expr}
            FROM publicatie WHERE organisatietype='gemeente' GROUP BY 1, 2""")}
        tekst = {(c, j): t for c, j, t in conn.execute(f"SELECT creator, jaargang, {tekst_expr} FROM publicatie WHERE organisatietype='gemeente' GROUP BY 1, 2")}
        za = {(c, j): (n, mu) for c, j, n, mu in conn.execute("""
            SELECT creator, CAST(substr(datum_aanvraag,1,4) AS INTEGER), COUNT(*), SUM(uitkomst IS NOT NULL)
            FROM zaak WHERE organisatietype='gemeente' AND datum_aanvraag IS NOT NULL GROUP BY 1, 2""")}
        for (c, j), (n, t, r) in sorted(pub.items()):
            zn, zmu = za.get((c, j), (0, 0))
            w.writerow([c, j, n, t, tekst.get((c, j), 0), r, zn, zmu])
    landelijk = tel(rijen, lambda c, d, k: (kwartaal(d), k))
    print(f"geschreven naar {uit}: {len(rijen)} zaken met aanvraag")
    print("kwartaal, klasse: aanvragen / verleend / geweigerd / ingetrokken / geen, mediaan dagen")
    for sleutel in sorted(landelijk):
        a = landelijk[sleutel]
        med = round(statistics.median(a["doorlooptijden"])) if a["doorlooptijden"] else "-"
        print(f"  {sleutel[0]} {sleutel[1]:10s} {a['aanvragen']:6d} / {a['verleend']:6d} / {a['geweigerd']:4d} / {a['ingetrokken']:5d} / {a['geen']:5d}  {med}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
