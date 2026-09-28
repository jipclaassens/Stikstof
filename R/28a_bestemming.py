"""Koppelt de objectpunten van het herontwikkelingspaper aan de Wro-bestemmingsplannen uit de PDOK-download.

Gebruik (Python van QGIS: numpy, GDAL en shapely 2):
  python-qgis-ltr.bat R/28a_bestemming.py <punten.csv> <gpkg-map> <uitvoermap>

punten.csv  pid;x;y in RD, de unieke punten van de bestaande opstal (28_bestemming_sites.R schrijft ze)
gpkg-map    een GeoPackage per laag uit de PDOK-feed Ruimtelijke plannen, stand 1 juli 2026
            (C:\\LocalData\\Stikstof\\plannen\\gpkg, gemaakt met omzetten_laag.cmd)

Per laag schrijft het script in de uitvoermap:
  <laag>_paren.csv    pid;fid, elk vlak dat het punt raakt (shapely STRtree, predicaat intersects)
  <laag>_vlakken.csv  fid;datum;planstatus;dossierstatus;historisch;typeplan;dossier;plan plus de waarden van de laag:
                      Enkelbestemming   hoofdgroep;naam
                      Bouwaanduiding    woningtype: gestapeld, aaneengebouwd, twee-aaneen of vrijstaand (in die volgorde
                                        van voorrang als de naam er meer noemt); andere bouwaanduidingen vallen weg
                      Maatvoering       bouwhoogte;goothoogte;wooneenheden;bouwlagen;bebouwingspct (maxima, uit de lijst
                                        sleutel=waarde; de varianten van een sleutel samengevoegd)
                      Bouwvlak          geen
Alleen plannen die zijn vastgesteld: ontwerp, voorontwerp en concept tellen niet. Vervallen en historische versies
blijven erin, zodat 28_bestemming_sites.R per datum het plan kan kiezen dat toen gold.
"""
import csv
import re
import sys
import time
from pathlib import Path

import numpy as np
import shapely
from osgeo import ogr

ogr.UseExceptions()
punten, gpkg_map, uit = Path(sys.argv[1]), Path(sys.argv[2]), Path(sys.argv[3])
uit.mkdir(parents=True, exist_ok=True)
t0 = time.time()


def log(tekst):
    print(f"[{time.time() - t0:7.0f} s] {tekst}", flush=True)


xy = np.loadtxt(punten, delimiter=";", skiprows=1)
pid = xy[:, 0].astype(np.int64)
pts = shapely.points(xy[:, 1], xy[:, 2])
log(f"{len(pid)} punten gelezen")

ALGEMEEN = ["datum", "planstatus", "dossierStatus", "historisch", "typePlan", "dossierID", "plangebied"]
MAAT = {"maximum bouwhoogte (m)": "bouwhoogte", "maximum goothoogte (m)": "goothoogte",
        "maximum aantal wooneenheden": "wooneenheden", "maximaal aantal wooneenheden": "wooneenheden",
        "maximum aantal woningen": "wooneenheden", "aantal wooneenheden": "wooneenheden",
        "maximum aantal bouwlagen": "bouwlagen",
        "maximum bebouwingspercentage (%)": "bebouwingspct", "maximum bebouwingspercentage terrein (%)": "bebouwingspct"}
MAAT_KOL = ["bouwhoogte", "goothoogte", "wooneenheden", "bouwlagen", "bebouwingspct"]
PAAR = re.compile(r'"([^"]+)"="([^"]*)"')


def getal(w):
    try:
        return float(w.strip().replace(",", "."))
    except ValueError:
        return None


def maatvoering(tekst):
    uitkomst = {}
    for k, w in PAAR.findall(tekst or ""):
        k = k.lower().strip()
        if k.startswith("other:"):
            k = k[6:].strip()
        k = k.split(";")[-1].strip()
        naam = MAAT.get(k)
        g = getal(w)
        if naam and g is not None and naam not in uitkomst:
            uitkomst[naam] = g
    return [uitkomst.get(k) for k in MAAT_KOL]


def woningtype(naam):
    n = (naam or "").lower()
    for t in ("gestapeld", "aaneengebouwd", "twee-aaneen", "vrijstaand"):
        if t in n or (t == "twee-aaneen" and "twee aaneen" in n):
            return t
    return None


LAGEN = {
    "Enkelbestemming": (["bestemmingshoofdgroep", "naam"], ["hoofdgroep", "naam"], lambda v: v),
    "Bouwaanduiding": (["naam"], ["woningtype"], lambda v: [woningtype(v[0])] if woningtype(v[0]) else None),
    "Maatvoering": (["maatvoering"], MAAT_KOL, lambda v: maatvoering(v[0]) if any(x is not None for x in maatvoering(v[0])) else None),
    "Bouwvlak": ([], [], lambda v: []),
}

for laag, (velden, kolommen, waarden) in LAGEN.items():
    ds = ogr.Open(str(gpkg_map / f"{laag}.gpkg"))
    lyr = ds.GetLayer(0)
    lyr.SetAttributeFilter("planstatus IS NULL OR planstatus NOT IN ('ontwerp', 'voorontwerp', 'concept')")
    fids, wkbs, rijen = [], [], []
    for f in lyr:
        g = f.GetGeometryRef()
        if g is None:
            continue
        w = waarden([f.GetField(v) for v in velden])
        if w is None:
            continue
        fids.append(f.GetFID())
        wkbs.append(bytes(g.ExportToWkb()))
        rijen.append([f.GetField(v) for v in ALGEMEEN] + list(w))
    lyr = None
    ds = None
    log(f"{laag}: {len(fids)} vlakken gelezen")
    vlakken = shapely.from_wkb(wkbs)
    del wkbs
    fout = ~shapely.is_valid(vlakken)
    if fout.any():
        vlakken[fout] = shapely.make_valid(vlakken[fout])
    log(f"{laag}: {int(fout.sum())} ongeldige vlakken hersteld")
    boom = shapely.STRtree(vlakken)
    i_pt, i_vl = boom.query(pts, predicate="intersects")
    log(f"{laag}: {len(i_pt)} paren")
    fid_arr = np.asarray(fids, dtype=np.int64)
    np.savetxt(uit / f"{laag}_paren.csv", np.column_stack([pid[i_pt], fid_arr[i_vl]]), fmt="%d", delimiter=";",
               header="pid;fid", comments="")
    with open(uit / f"{laag}_vlakken.csv", "w", newline="", encoding="utf-8") as fh:
        wr = csv.writer(fh, delimiter=";")
        wr.writerow(["fid", "datum", "planstatus", "dossierstatus", "historisch", "typeplan", "dossier", "plan"] + kolommen)
        for f_id, r in zip(fids, rijen):
            wr.writerow([f_id] + ["" if x is None else x for x in r])
    del vlakken, boom, i_pt, i_vl, rijen, fids
    log(f"{laag}: geschreven")
log("klaar")
