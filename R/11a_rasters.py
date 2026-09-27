"""Leest landelijke rasters af op de locaties van de BAG-objecten.

Gebruik (met de Python van QGIS, want GDAL is nodig):
  "C:\\Program Files\\QGIS 3.40.7\\bin\\python-qgis-ltr.bat" R/11a_rasters.py <punten.csv> <uitvoer.csv> [datamap]

punten.csv   id;x;y in RD (EPSG:28992), geschreven door 11_ruimte.R
uitvoer.csv  id plus een kolom per raster; -1 buiten het raster of nodata
datamap      de datamap van het vergunningenspoor (standaard VERGUNNINGEN_DATA)

De rasters (bron/CATALOGUS.md, thema's afgeleid en depositie):
  stedelijk     1 binnen de verstedelijkingscontour van de provincie, 0 erbuiten
                (stand september 2026; Gelderland: bebouwde kom als vervanger)
  contourbron   CBS-provinciecode van de provincie wier contour telt; 125 voor
                Gelderland met de vervangende laag
  cong_2024, cong_2025, cong_2026
                congestie voor afname op 17-04-2024, 11-07-2025 en 21-09-2026:
                0 capaciteit, 1 beperkt, 2 rood, 255 onbekend
  cong_eerste   eerste stand waarop de cel rood was: 1 april 2024, 2 juli 2025,
                3 september 2026, 0 nooit
  ndep_2018     totale stikstofdepositie 2018 in mol N per ha per jaar (RIVM,
                consistente reeks, 1 km)
  stedelijk_vroeg
                1 binnen de contour van 2010 tot 2014 (vijf provincies), 0
                erbuiten, -1 elders; uit _derived/stedelijk_vroeg_25m.tif van
                11b_contour_vroeg.py, alleen als dat bestand er is

Elk raster wordt in zijn geheel gelezen en met zijn eigen geotransformatie
afgelezen: kolom = floor((x - x0) / dx), rij = floor((y - y0) / dy).
"""
import csv
import math
import os
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal

gdal.UseExceptions()

punten, uit = Path(sys.argv[1]), Path(sys.argv[2])
data = Path(sys.argv[3]) if len(sys.argv) > 3 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
bron = data / "bron"
RASTERS = [
    ("stedelijk", bron / "afgeleid" / "stedelijk_gebied_nl_25m.tif"),
    ("contourbron", bron / "afgeleid" / "contourbron_nl_25m.tif"),
    ("cong_2024", bron / "afgeleid" / "congestie_afname_20240417_25m.tif"),
    ("cong_2025", bron / "afgeleid" / "congestie_afname_20250711_25m.tif"),
    ("cong_2026", bron / "afgeleid" / "congestie_afname_20260921_25m.tif"),
    ("cong_eerste", bron / "afgeleid" / "congestie_afname_eerste_25m.tif"),
    ("ndep_2018", bron / "depositie" / "gdn_depositie_historische_reeks" / "ndep_historisch_2018.tif"),
]
## de vroege contour uit 11b_contour_vroeg.py, als die er is
_vroeg = Path(__file__).resolve().parent / "_derived" / "stedelijk_vroeg_25m.tif"
if _vroeg.exists():
    RASTERS.append(("stedelijk_vroeg", _vroeg))


def main():
    ids, xs, ys = [], [], []
    with open(punten, encoding="utf-8") as f:
        for rij in csv.DictReader(f, delimiter=";"):
            ids.append(rij["id"]); xs.append(float(rij["x"])); ys.append(float(rij["y"]))
    x = np.array(xs); y = np.array(ys)
    kolommen = {}
    for naam, pad in RASTERS:
        ds = gdal.Open(str(pad))
        band = ds.GetRasterBand(1)
        gt = ds.GetGeoTransform()
        if gt[2] != 0 or gt[4] != 0:
            raise SystemExit(f"{pad}: gedraaid raster wordt niet ondersteund")
        arr = band.ReadAsArray()
        nd = band.GetNoDataValue()
        kol = np.floor((x - gt[0]) / gt[1]).astype(np.int64)
        rij = np.floor((y - gt[3]) / gt[5]).astype(np.int64)
        binnen = (kol >= 0) & (kol < ds.RasterXSize) & (rij >= 0) & (rij < ds.RasterYSize)
        waarde = np.full(x.shape, -1.0)
        waarde[binnen] = arr[rij[binnen], kol[binnen]]
        if nd is not None and not math.isnan(nd):
            waarde[binnen & (waarde == nd)] = -1.0
        kolommen[naam] = waarde
        print(f"{naam:12s} {ds.RasterXSize} x {ds.RasterYSize}, cel {gt[1]:g} m, nodata {nd}; "
              f"binnen {binnen.sum()} van {len(x)}, geldig {(waarde >= 0).sum()}", flush=True)
        ds = None
    with open(uit, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["id"] + [n for n, _ in RASTERS])
        for i in range(len(ids)):
            w.writerow([ids[i]] + [("%g" % kolommen[n][i]) for n, _ in RASTERS])
    print(f"geschreven: {uit}")


if __name__ == "__main__":
    main()
