"""Afstand van de BAG-objecten tot het Natuurnetwerk Nederland (NNN), met en zonder Natura 2000.

Gebruik (met de Python van QGIS, want GDAL is nodig):
  "C:\\Program Files\\QGIS 3.40.7\\bin\\python-qgis-ltr.bat" R/18a_nnn.py <punten.csv> <uitvoer.csv> [datamap]

punten.csv   id;x;y in RD (EPSG:28992), geschreven door 11_ruimte.R
uitvoer.csv  id plus afstanden in meters (0 binnen het gebied, 20001 verder dan 20 km, -1 buiten het raster):
  d_nnn        tot het NNN volgens de provinciale lagen (bron/natuur/nnn_provincies.gpkg, twaalf lagen)
  d_nnn_x      idem, zonder de delen die ook Natura 2000 zijn: het NNN waar geen stikstoftoets geldt
  d_n2000      tot een Natura 2000-gebied (bron/natuur/natura2000_pdok.gpkg), stikstofgevoelig of niet
  d_cdda       tot het NNN volgens de rijksrapportage (bron/natuur/cdda_pdok.gpkg, namespac NN.99..)
  d_cdda_x     idem, zonder Natura 2000
datamap      de datamap van het vergunningenspoor (standaard VERGUNNINGEN_DATA)

Het raster is dat van de verstedelijkingscontour (25 meter, 12.000 bij 13.200 cellen vanaf x 0, y 630.000). Vlakken
worden gebrand met ALL_TOUCHED, zodat ook smalle stroken natuur een cel krijgen; de afstand is die tussen
celmiddens (gdal ComputeProximity), dus op 25 meter nauwkeurig.
"""
import csv
import os
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr

gdal.UseExceptions()

punten, uit = Path(sys.argv[1]), Path(sys.argv[2])
data = Path(sys.argv[3]) if len(sys.argv) > 3 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
natuur = data / "bron" / "natuur"
GRID = data / "bron" / "afgeleid" / "stedelijk_gebied_nl_25m.tif"
MAX = 20000


def leeg(ref, dtype):
    ds = gdal.GetDriverByName("MEM").Create("", ref.RasterXSize, ref.RasterYSize, 1, dtype)
    ds.SetGeoTransform(ref.GetGeoTransform())
    ds.SetProjection(ref.GetProjection())
    return ds


def brand(ref, pad, lagen=None, where=None):
    ds = leeg(ref, gdal.GDT_Byte)
    if lagen is None:
        bron = ogr.Open(str(pad))
        lagen = [bron.GetLayer(i).GetName() for i in range(bron.GetLayerCount())]
        bron = None
    for laag in lagen:
        gdal.Rasterize(ds, str(pad), options=gdal.RasterizeOptions(layers=[laag], burnValues=[1], allTouched=True,
                                                                     where=where))
    a = ds.GetRasterBand(1).ReadAsArray()
    print(f"{pad.name} {lagen if len(lagen) < 3 else str(len(lagen)) + ' lagen'}"
          f"{' waar ' + where if where else ''}: {int(a.sum())} cellen ({a.sum() * 625 / 1e4:,.0f} ha)", flush=True)
    return a


def afstand(ref, masker, kol, rij, binnen):
    bron = leeg(ref, gdal.GDT_Byte)
    bron.GetRasterBand(1).WriteArray(masker.astype(np.uint8))
    doel = leeg(ref, gdal.GDT_UInt16)
    gdal.ComputeProximity(bron.GetRasterBand(1), doel.GetRasterBand(1),
                          ["VALUES=1", "DISTUNITS=GEO", f"MAXDIST={MAX}", "NODATA=65535"])
    a = doel.GetRasterBand(1).ReadAsArray()
    w = np.full(kol.shape, -1.0)
    w[binnen] = a[rij[binnen], kol[binnen]]
    w[binnen & (w == 65535)] = MAX + 1
    return w


def main():
    ids, xs, ys = [], [], []
    with open(punten, encoding="utf-8") as f:
        for r in csv.DictReader(f, delimiter=";"):
            ids.append(r["id"]); xs.append(float(r["x"])); ys.append(float(r["y"]))
    x = np.array(xs); y = np.array(ys)
    ref = gdal.Open(str(GRID))
    gt = ref.GetGeoTransform()
    kol = np.floor((x - gt[0]) / gt[1]).astype(np.int64)
    rij = np.floor((y - gt[3]) / gt[5]).astype(np.int64)
    binnen = (kol >= 0) & (kol < ref.RasterXSize) & (rij >= 0) & (rij < ref.RasterYSize)
    print(f"{len(x)} punten, {binnen.sum()} binnen het raster", flush=True)

    nnn = brand(ref, natuur / "nnn_provincies.gpkg")
    n2000 = brand(ref, natuur / "natura2000_pdok.gpkg", ["natura2000"])
    cdda = brand(ref, natuur / "cdda_pdok.gpkg", ["cdda"], "namespac LIKE 'NN.99%'")
    maskers = [("d_nnn", nnn), ("d_nnn_x", nnn & (1 - n2000)), ("d_n2000", n2000),
               ("d_cdda", cdda), ("d_cdda_x", cdda & (1 - n2000))]
    kolommen = {}
    for naam, m in maskers:
        kolommen[naam] = afstand(ref, m, kol, rij, binnen)
        v = kolommen[naam][binnen]
        print(f"{naam:9s} mediaan {np.median(v):7.0f} m; binnen 0 m {np.mean(v == 0):.3f}, "
              f"binnen 500 m {np.mean(v <= 500):.3f}, binnen 2.500 m {np.mean(v <= 2500):.3f}", flush=True)
    with open(uit, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["id"] + [n for n, _ in maskers])
        for i in range(len(ids)):
            w.writerow([ids[i]] + ["%g" % kolommen[n][i] for n, _ in maskers])
    print(f"geschreven: {uit}")


if __name__ == "__main__":
    main()
