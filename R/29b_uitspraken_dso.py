"""Welke plannen uit de DSO-oogst een gerechtelijke uitspraak aan zich hebben hangen.

Gebruik: py -3.13 R/29b_uitspraken_dso.py <dso.sqlite> <uitvoer.csv>

Een gemeente publiceert een uitspraak van de Raad van State als eigen plan van het type gerechtelijke uitspraak als die
uitspraak het plan verandert (vernietiging, zelf voorzien, een opdracht tot herstel). De relaties tussen plannen
(tabel plan_relatie van bron\\dso\\werk\\dso_plannen.py, beide richtingen) leggen die uitspraak exact aan het plan vast.
Een uitspraak die het beroep ongegrond verklaart, komt er dus niet in: dit is de maat voor een beroep met gevolgen.

uitvoer.csv  dossier;uitspraak_id;uitspraak_naam;uitspraak_datum, een rij per dossier en uitspraak
Alleen gelezen: de database wordt read-only geopend.
"""
import csv
import sqlite3
import sys
from pathlib import Path

db, uit = Path(sys.argv[1]), Path(sys.argv[2])
con = sqlite3.connect(f"file:{db.as_posix()}?mode=ro", uri=True)
gu = {r[0]: (r[1], r[2]) for r in con.execute(
    "SELECT identificatie, naam, planstatus_datum FROM plan WHERE type = 'gerechtelijke uitspraak'")}
plan_dossier = {r[0]: r[1] for r in con.execute(
    "SELECT identificatie, dossier_id FROM plan WHERE type IN ('bestemmingsplan', 'wijzigingsplan', 'uitwerkingsplan', 'inpassingsplan')")}
rijen = set()
## een uitspraak die als versie in het dossier van het plan zelf staat (identificatie van het dossier met -GU01 en dergelijke)
dossiers = set(plan_dossier.values())
for gid, dos in con.execute("SELECT identificatie, dossier_id FROM plan WHERE type = 'gerechtelijke uitspraak'"):
    if dos and dos in dossiers:
        rijen.add((dos, gid, gu[gid][0], gu[gid][1]))
## en een uitspraak die via een relatie aan een versie van het plan hangt
for van, naar in con.execute("SELECT van_id, naar_id FROM plan_relatie"):
    for plan_id, ander in ((van, naar), (naar, van)):
        if plan_id in plan_dossier and ander in gu and plan_dossier[plan_id]:
            naam, datum = gu[ander]
            rijen.add((plan_dossier[plan_id], ander, naam, datum))
con.close()
with open(uit, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["dossier", "uitspraak_id", "uitspraak_naam", "uitspraak_datum"])
    w.writerows(sorted(rijen))
print(f"{len(gu)} gerechtelijke uitspraken; {len(rijen)} koppelingen aan {len({r[0] for r in rijen})} dossiers; geschreven: {uit}")
