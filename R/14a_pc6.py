"""Legt de postcodevlakken (PC6) van de capaciteitskaart op de BAG-objecten.

Gebruik (met de Python van QGIS, want GDAL is nodig):
  "C:\\Program Files\\QGIS 3.40.7\\bin\\python-qgis-ltr.bat" R/14a_pc6.py <punten.csv> <uitvoer.csv> [datamap]

punten.csv   id;x;y in RD (EPSG:28992), geschreven door 11_ruimte.R (_derived/vbo_xy.csv)
uitvoer.csv  id;postcode; leeg buiten elk postcodevlak
datamap      de datamap van het vergunningenspoor (standaard VERGUNNINGEN_DATA)

De vlakken komen uit bron/netcongestie/capaciteitskaart_netbeheernederland_20260921.gpkg,
laag congestie_pc6_vlakken (465.174 vlakken, stand 21 september 2026). Ze worden op
een raster van 25 m gezet met het fid als waarde (0 = geen vlak), op hetzelfde grid als
de rasters in bron/afgeleid, en dat raster wordt afgelezen zoals in 11a_rasters.py.
Een punt vlak bij een grens kan zo het buurvlak krijgen; voor de congestiestatus, die
per voedingsgebied geldt, maakt dat zelden uit.
"""
import csv
import os
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr

gdal.UseExceptions()
ogr.UseExceptions()

punten, uit = Path(sys.argv[1]), Path(sys.argv[2])
data = Path(sys.argv[3]) if len(sys.argv) > 3 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
GPKG = data / "bron" / "netcongestie" / "capaciteitskaart_netbeheernederland_20260921.gpkg"
GRID = data / "bron" / "afgeleid" / "stedelijk_gebied_nl_25m.tif"


def main():
    ref = gdal.Open(str(GRID))
    gt, nx, ny = ref.GetGeoTransform(), ref.RasterXSize, ref.RasterYSize
    srs = ref.GetProjection()
    ref = None

    vec = ogr.Open(str(GPKG))
    ## fid + 0: een gewoon veld, want een kale fid wordt in het resultaat de FID
    laag = vec.ExecuteSQL("SELECT fid, fid + 0 AS nr, postcode, geom FROM congestie_pc6_vlakken")
    postcode = {}
    for f in laag:
        postcode[f.GetField("nr")] = f.GetField("postcode")
    laag.ResetReading()
    print(f"postcodevlakken: {len(postcode)}", flush=True)

    mem = gdal.GetDriverByName("MEM").Create("", nx, ny, 1, gdal.GDT_UInt32)
    mem.SetGeoTransform(gt)
    mem.SetProjection(srs)
    mem.GetRasterBand(1).Fill(0)
    gdal.RasterizeLayer(mem, [1], laag, options=["ATTRIBUTE=nr"])
    vec.ReleaseResultSet(laag)
    arr = mem.GetRasterBand(1).ReadAsArray()
    mem = None
    print(f"raster {nx} x {ny}, cellen met een vlak: {int((arr > 0).sum())}", flush=True)

    ids, xs, ys = [], [], []
    with open(punten, encoding="utf-8") as f:
        for rij in csv.DictReader(f, delimiter=";"):
            ids.append(rij["id"]); xs.append(float(rij["x"])); ys.append(float(rij["y"]))
    x = np.array(xs); y = np.array(ys)
    kol = np.floor((x - gt[0]) / gt[1]).astype(np.int64)
    rij = np.floor((y - gt[3]) / gt[5]).astype(np.int64)
    binnen = (kol >= 0) & (kol < nx) & (rij >= 0) & (rij < ny)
    waarde = np.zeros(x.shape, dtype=np.int64)
    waarde[binnen] = arr[rij[binnen], kol[binnen]]
    gevonden = 0
    with open(uit, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["id", "postcode"])
        for i, v in zip(ids, waarde):
            pc = postcode.get(int(v), "") if v > 0 else ""
            gevonden += pc != ""
            w.writerow([i, pc])
    print(f"punten: {len(ids)}, met postcode: {gevonden}", flush=True)


if __name__ == "__main__":
    main()
