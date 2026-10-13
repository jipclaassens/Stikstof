"""Gebied en afstand tot stikstofgevoelig habitat van de wijzigingen van gemeentelijke omgevingsplannen in de nieuwe
standaard (STOP/TPOD), uit de oogst van de DSO-LV.

Gebruik (Python van QGIS: numpy, GDAL en shapely 2):
  python-qgis-ltr.bat R/30a_wijzigingen_lv.py <dso_lv.sqlite> <habitat.gpkg> <uitvoer.csv>

dso_lv.sqlite  de oogst van bron\\dso\\werk\\dso_lv_omgevingsplan.py (ontwerpen, besluiten, gewijzigde locaties uit de
               renvooi en hun geometrie), alleen gelezen
habitat.gpkg   bron\\aerius_relevant_habitats.gpkg, de AERIUS relevante habitatkartering (zoals 29a_plannen_dso.py)

Per ontwerp en besluit: de vereniging van de geometrieen van de gewijzigde enkelvoudige locaties (gebied, lijn, punt; ook
de leden die aan een groep zijn toegevoegd en de locaties van gewijzigde regels), de oppervlakte, het zwaartepunt en de
kleinste afstand tot het habitat. Zonder zo'n locatie (alleen het ambtsgebied of groepen gewijzigd, zoals bij de
bruidsschat) of met meer dan 200 (groot) blijft de afstand leeg.
Uitvoer: soort;technisch_id;gemeente;gemeentenaam;titel;bekend_op;vaststelling;begin_inwerking;renvooi;n_locaties;
n_geometrie;groot;alleen_groep;opp_ha;x;y;afstand_m
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
dso_lv, habitat, uit = (Path(a) for a in sys.argv[1:4])
t0 = time.time()
MAX_LOC = 200
ENKEL = ("Gebied", "Lijn", "Punt")


def log(tekst):
    print(f"[{time.time() - t0:6.0f} s] {tekst}", flush=True)


# ---- habitat, opgesplitst in losse polygonen (zoals 29a_plannen_dso.py)
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
docs = con.execute("""SELECT d.soort, d.technisch_id, d.gemeente, d.gemeentenaam, d.titel, d.bekend_op, d.vaststelling,
                             d.begin_inwerking, r.status
                      FROM document d LEFT JOIN renvooi r ON r.soort = d.soort AND r.technisch_id = d.technisch_id
                      ORDER BY d.soort, d.bekend_op, d.technisch_id""").fetchall()
loc = collections.defaultdict(list)
groep = collections.Counter()
for soort, tid, typ, gid in con.execute("SELECT soort, technisch_id, locatie_type, geometrie_id FROM wijziging"):
    if typ in ENKEL:
        loc[(soort, tid)].append(gid)
    else:
        groep[(soort, tid)] += 1
geo = dict(con.execute("SELECT geometrie_id, wkt FROM geometrie WHERE status = 200 AND wkt IS NOT NULL"))
con.close()
log(f"{len(docs)} documenten, {sum(len(v) for v in loc.values())} gewijzigde enkelvoudige locaties, {len(geo)} geometrieen")

with open(uit, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["soort", "technisch_id", "gemeente", "gemeentenaam", "titel", "bekend_op", "vaststelling", "begin_inwerking",
                "renvooi", "n_locaties", "n_geometrie", "groot", "alleen_groep", "opp_ha", "x", "y", "afstand_m"])
    n_af = 0
    for soort, tid, gem, gemnaam, titel, bekend, vast, inw, status in docs:
        ids = loc.get((soort, tid), [])
        groot = len(ids) > MAX_LOC
        wkts = [geo[g] for g in ids if g in geo] if not groot else []
        opp = x = y = afst = ""
        if wkts:
            gg = shapely.from_wkt(np.array(wkts, dtype=object))
            ok = ~shapely.is_valid(gg)
            if ok.any():
                gg[ok] = shapely.make_valid(gg[ok])
            u = shapely.union_all(gg)
            opp = round(float(shapely.area(u)) / 1e4, 4)
            zw = shapely.centroid(u)
            x, y = round(float(shapely.get_x(zw)), 1), round(float(shapely.get_y(zw)), 1)
            _, d = boom.query_nearest(u, return_distance=True, all_matches=False)
            afst = round(float(d[0]), 1)
            n_af += 1
        w.writerow([soort, tid, (gem or "").replace("gm", ""), gemnaam, titel, bekend, vast, inw, status, len(ids), len(wkts),
                    int(groot), int(not ids and groep[(soort, tid)] > 0), opp, x, y, afst])
log(f"geschreven: {uit} ({n_af} documenten met afstand)")
