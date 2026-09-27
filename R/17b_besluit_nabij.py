"""Zoekt per BAG-pand of er binnen 100 meter een verleende zaak ligt, recent of ouder dan een half jaar.

Gebruik: py -3.13 R/17b_besluit_nabij.py <panden.csv> <uitvoer.csv> [datamap]

panden.csv   pand_id;bvv;x;y (bvv = datum Bouwvergunning verleend, x en y in RD), geschreven door 17_registratie.R
uitvoer.csv  pand_id;recent;oud
  recent     1 als er binnen 100 meter een verleende gemeentelijke zaak ligt met een uitkomst van 60 dagen na tot
             180 dagen voor de BAG-datum: het venster waarin koppel_bag.py koppelt
  oud        1 als er binnen 100 meter een verleende zaak ligt met een uitkomst van 181 dagen tot drie jaar voor de
             BAG-datum

Een inhaalslag, waarin gemeenten oudere vergunningen alsnog registreren, geeft panden zonder recent besluit maar met
een oud besluit in de buurt. De database wordt alleen gelezen (immutable), zie 17a_pand_zaak.py.
"""
import csv
import datetime as dt
import os
import sys
from collections import defaultdict
from pathlib import Path

panden, uit = Path(sys.argv[1]), Path(sys.argv[2])
data = Path(sys.argv[3]) if len(sys.argv) > 3 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
db = data / "vergunningen.sqlite"
if db.with_name(db.name + "-wal").exists():
    raise SystemExit(f"{db}-wal bestaat: de database wordt bijgewerkt, probeer het later")

import sqlite3  # noqa: E402

CEL = 100.0
con = sqlite3.connect(f"file:{db.as_posix()}?immutable=1", uri=True)
rooster = defaultdict(list)
for x, y, du in con.execute("SELECT x, y, datum_uitkomst FROM zaak WHERE x IS NOT NULL AND organisatietype = 'gemeente' "
                            "AND uitkomst = 'verleend' AND datum_uitkomst >= '2020-01-01'"):
    rooster[(int(x // CEL), int(y // CEL))].append((x, y, dt.date.fromisoformat(du[:10])))
con.close()
n = 0
with open(panden, encoding="utf-8") as f, open(uit, "w", newline="", encoding="utf-8") as g:
    w = csv.writer(g, delimiter=";")
    w.writerow(["pand_id", "recent", "oud"])
    for r in csv.DictReader(f, delimiter=";"):
        b = dt.date.fromisoformat(r["bvv"][:10]); x = float(r["x"]); y = float(r["y"])
        recent = oud = 0
        cx, cy = int(x // CEL), int(y // CEL)
        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                for zx, zy, du in rooster.get((cx + dx, cy + dy), ()):
                    if (zx - x) ** 2 + (zy - y) ** 2 > CEL ** 2:
                        continue
                    dagen = (b - du).days
                    if -60 <= dagen <= 180:
                        recent = 1
                    elif 181 <= dagen <= 3 * 365:
                        oud = 1
        w.writerow([r["pand_id"], recent, oud])
        n += 1
print(f"{n} panden, geschreven naar {uit}")
