"""Afstand van de plannen uit de DSO-oogst tot stikstofgevoelig habitat, en de woonbestemmingen per plan.

Gebruik (Python van QGIS: numpy, GDAL en shapely 2):
  python-qgis-ltr.bat R/29a_plannen_dso.py <dso.sqlite> <habitat.gpkg> <Enkelbestemming.gpkg> <uitvoer.csv>

dso.sqlite            de oogst van de API Ruimtelijke plannen v4 (bron\\dso\\werk\\dso_plannen.py), alleen gelezen
habitat.gpkg          bron\\aerius_relevant_habitats.gpkg, de AERIUS relevante habitatkartering (zoals de rest van dit onderzoek)
Enkelbestemming.gpkg  de PDOK-laag van 1 juli 2026 (C:\\LocalData\\Stikstof\\plannen\\gpkg, zie 28a_bestemming.py)

Per planversie van de typen bestemmingsplan, wijzigingsplan, uitwerkingsplan en inpassingsplan met een contour:
identificatie;dossier;type;overheidscode;overheidsnaam;naam;planstatus;datum;beroep_en_bezwaar;dossierstatus;
opp_ha;x;y;afstand_m;eb_vlakken;eb_wonen
afstand_m is de kleinste afstand van de plancontour tot het habitat (0 als ze elkaar raken); eb_vlakken en eb_wonen tellen
de vlakken van de enkelbestemming van die versie in de PDOK-download en die met de hoofdgroep wonen of woongebied (leeg als
de versie niet in de download staat, zoals ontwerpen en vervangen versies).
"""
import collections
import csv
import sqlite3
import sys
import time
from pathlib import Path

import numpy as np
import shapely
from osgeo import ogr

ogr.UseExceptions()
dso, habitat, eb, uit = (Path(a) for a in sys.argv[1:5])
t0 = time.time()


def log(tekst):
    print(f"[{time.time() - t0:6.0f} s] {tekst}", flush=True)


# ---- habitat, opgesplitst in losse polygonen zodat de afstand per plan snel blijft
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
delen = delen[shapely.get_type_id(delen) == 3]   # alleen polygonen
boom = shapely.STRtree(delen)
log(f"habitat: {len(hab)} objecten, {len(delen)} polygonen")

# ---- plannen met contour
con = sqlite3.connect(f"file:{dso.as_posix()}?mode=ro", uri=True)
rijen = con.execute("""SELECT p.identificatie, p.dossier_id, p.type, p.overheidscode, p.overheidsnaam, p.naam, p.planstatus,
                              p.planstatus_datum, p.beroep_en_bezwaar, p.dossierstatus, g.wkt
                       FROM plan p JOIN plan_geometrie g ON g.identificatie = p.identificatie
                       WHERE p.type IN ('bestemmingsplan', 'wijzigingsplan', 'uitwerkingsplan', 'inpassingsplan')
                         AND g.wkt IS NOT NULL""").fetchall()
con.close()
log(f"{len(rijen)} planversies met contour")
geom = shapely.from_wkt(np.array([r[10] for r in rijen], dtype=object))   # object: geen tekstarray met vaste breedte
fout = ~shapely.is_valid(geom)
if fout.any():
    geom[fout] = shapely.make_valid(geom[fout])
log(f"{int(fout.sum())} ongeldige contouren hersteld")
opp = shapely.area(geom) / 1e4
zw = shapely.centroid(geom)
idx, afstand = boom.query_nearest(geom, return_distance=True, all_matches=False)
afst = np.full(len(geom), np.nan)
afst[idx[0]] = afstand
log("afstanden berekend")

# ---- woonbestemmingen per planversie uit de PDOK-laag
gp = sqlite3.connect(f"file:{eb.as_posix()}?mode=ro", uri=True)
tel = collections.defaultdict(lambda: [0, 0])
for plan, hg, n in gp.execute('SELECT plangebied, bestemmingshoofdgroep, count(*) FROM "Enkelbestemming" GROUP BY 1, 2'):
    tel[plan][0] += n
    if hg in ("wonen", "woongebied"):
        tel[plan][1] += n
gp.close()
log(f"enkelbestemming: {len(tel)} planversies in de download")

with open(uit, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["identificatie", "dossier", "type", "overheidscode", "overheidsnaam", "naam", "planstatus", "datum",
                "beroep_en_bezwaar", "dossierstatus", "opp_ha", "x", "y", "afstand_m", "eb_vlakken", "eb_wonen"])
    for i, r in enumerate(rijen):
        t = tel.get(r[0])
        w.writerow(list(r[:10]) + [round(float(opp[i]), 4), round(float(shapely.get_x(zw[i])), 1), round(float(shapely.get_y(zw[i])), 1),
                                   "" if np.isnan(afst[i]) else round(float(afst[i]), 1),
                                   t[0] if t else "", t[1] if t else ""])
log(f"geschreven: {uit}")
