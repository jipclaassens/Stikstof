"""Afstand tot stikstofgevoelig habitat en projectsoort van de omgevingsvergunningen die gemeenten in de DSO-LV registreren
(vooral vergunningen voor een buitenplanse omgevingsplanactiviteit, sinds 2024).

Gebruik (Python van QGIS: numpy, GDAL en shapely 2):
  python-qgis-ltr.bat R/32a_vergunningen_lv.py <dso_lv.sqlite> <habitat.gpkg> <uitvoer.csv>

dso_lv.sqlite  de oogst van bron\\dso\\werk\\dso_lv_omgevingsplan.py vergunningen (tabel vergunning), alleen gelezen
habitat.gpkg   bron\\aerius_relevant_habitats.gpkg (zoals 29a en 30a)

Het vergunningsgebied komt als omhullende rechthoek mee (mediaan ongeveer 1.100 m2); de afstand is die van die rechthoek tot het
habitat. De projectsoort komt uit dezelfde titelregel als bij de bekendmakingen (vergunningen/pipeline/analyse_export.py).
Uitvoer: identificatie;gemeente;begin_inwerking;registratie;titel;soort;opp_m2;x;y;afstand_m
"""
import csv
import sqlite3
import sys
import time
from pathlib import Path

import numpy as np
import shapely
from osgeo import ogr

HIER = Path(__file__).resolve().parent
sys.path.insert(0, str(HIER.parent / "vergunningen" / "pipeline"))
from analyse_export import projectsoort  # noqa: E402

ogr.UseExceptions()
dso_lv, habitat, uit = (Path(a) for a in sys.argv[1:4])
t0 = time.time()


def log(tekst):
    print(f"[{time.time() - t0:6.0f} s] {tekst}", flush=True)


ds = ogr.Open(str(habitat))
lyr = ds.GetLayer(0)
wkbs = [bytes(f.GetGeometryRef().ExportToWkb()) for f in lyr if f.GetGeometryRef() is not None]
lyr = None
ds = None
hab = shapely.from_wkb(wkbs)
fout = ~shapely.is_valid(hab)
if fout.any():
    hab[fout] = shapely.make_valid(hab[fout])
delen = shapely.get_parts(hab)
delen = delen[shapely.get_type_id(delen) == 3]
boom = shapely.STRtree(delen)
log(f"habitat: {len(hab)} objecten, {len(delen)} polygonen")

con = sqlite3.connect(f"file:{dso_lv.as_posix()}?mode=ro", uri=True)
rijen = con.execute("""SELECT identificatie, gemeente, begin_inwerking, registratie, titel, minx, miny, maxx, maxy
                       FROM vergunning WHERE minx IS NOT NULL AND bestuurslaag = 'gemeente'""").fetchall()
con.close()
b = np.array([r[5:9] for r in rijen], dtype=float)
# een rechthoek van nul breed (een punt of lijn) blijft een geldige geometrie voor de afstand
geom = shapely.box(b[:, 0], b[:, 1], b[:, 2], b[:, 3])
idx, afstand = boom.query_nearest(geom, return_distance=True, all_matches=False)
afst = np.full(len(geom), np.nan)
afst[idx[0]] = afstand
log(f"{len(rijen)} vergunningen, afstanden berekend")
with open(uit, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["identificatie", "gemeente", "begin_inwerking", "registratie", "titel", "soort", "opp_m2", "x", "y", "afstand_m"])
    for i, r in enumerate(rijen):
        w.writerow([r[0], (r[1] or "").replace("gm", ""), r[2], r[3], r[4], projectsoort(r[4] or ""),
                    round(float((b[i, 2] - b[i, 0]) * (b[i, 3] - b[i, 1])), 1), round(float((b[i, 0] + b[i, 2]) / 2), 1),
                    round(float((b[i, 1] + b[i, 3]) / 2), 1), "" if np.isnan(afst[i]) else round(float(afst[i]), 1)])
log(f"geschreven: {uit}")
