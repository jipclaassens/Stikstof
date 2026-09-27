"""Exporteert de koppeling van BAG-panden aan zaken uit de vergunningendatabase, met de datums van de zaak.

Gebruik: py -3.13 R/17a_pand_zaak.py <uitvoer.csv> [datamap]

De koppeling zelf maakt koppel_bag.py in het vergunningenspoor (vergunningen/pipeline): elk pand met de BAG-status
Bouwvergunning verleend vanaf juni 2021 krijgt binnen 100 meter de zaak uit de gemeentelijke bekendmakingen waarvan
het tijdvenster de vergunningdatum omvat, van veertien dagen voor de aanvraag tot 180 dagen na de uitkomst; bij meer
kandidaten wint een verleende zaak, dan de kleinste afstand in dagen. Een BAG-registratie die meer dan 180 dagen na
de uitkomst valt, koppelt dus niet aan die zaak.

Dit script leest alleen: de database wordt geopend als immutable, zodat er geen -wal- of -shm-bestand naast de
database in de gedeelde map komt. Draai het niet terwijl het vergunningenspoor de database bijwerkt.

uitvoer.csv  pand_id;zaak_id;afstand_m;verg_begin;datum_aanvraag;datum_uitkomst;uitkomst
"""
import csv
import os
import sqlite3
import sys
from pathlib import Path

uit = Path(sys.argv[1])
data = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
db = data / "vergunningen.sqlite"
if db.with_name(db.name + "-wal").exists():
    raise SystemExit(f"{db}-wal bestaat: de database wordt bijgewerkt, probeer het later")
con = sqlite3.connect(f"file:{db.as_posix()}?immutable=1", uri=True)
rijen = con.execute("""SELECT b.pand_id, b.zaak_id, b.afstand_m, b.verg_begin, z.datum_aanvraag, z.datum_uitkomst, z.uitkomst
                       FROM zaak_bag b JOIN zaak z ON z.zaak_id = b.zaak_id""").fetchall()
# controle: de opgeslagen afstand in dagen moet kloppen met de huidige zaaktabel (zaak.py nummert zaken opnieuw)
fout = con.execute("""SELECT count(*) FROM zaak_bag b JOIN zaak z ON z.zaak_id = b.zaak_id
                      WHERE b.dagen_uitkomst_vergunning IS NOT NULL
                        AND b.dagen_uitkomst_vergunning != julianday(substr(z.datum_uitkomst, 1, 10)) - julianday(substr(b.verg_begin, 1, 10))""").fetchone()[0]
totaal = con.execute("SELECT count(*) FROM zaak_bag").fetchone()[0]
con.close()
if fout:
    raise SystemExit(f"{fout} koppelingen passen niet bij de zaaktabel: draai koppel_bag.py opnieuw")
with open(uit, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["pand_id", "zaak_id", "afstand_m", "verg_begin", "datum_aanvraag", "datum_uitkomst", "uitkomst"])
    w.writerows(rijen)
print(f"{len(rijen)} van {totaal} koppelingen met een bestaande zaak geschreven naar {uit}")
