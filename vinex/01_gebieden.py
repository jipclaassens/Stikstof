"""Maakt van het 0/1-masker VinexPlans.tif een raster met een nummer per aaneengesloten gebied,
op hetzelfde 100 meter-grid als de GeoDMS-configuratie (x 0 tot 280.000, y 300.000 tot 625.000).

Gebieden die binnen 300 meter van elkaar liggen horen bij elkaar: een snelweg of een waterloop
knipt een wijk anders in stukken. De nummers staan alleen op cellen die echt Vinex zijn.

Uitvoer: vinex_gebieden_100m.tif (int32, 0 = geen Vinex) en vinex_gebieden.csv in C:\\LocalData\\Stikstof\\Vinex,
waar Analyse/Vinex in de GeoDMS-configuratie het raster leest.

Draaien met de Python van QGIS (GDAL en scipy):
  "C:\\Program Files\\QGIS 3.40.4\\bin\\python-qgis-ltr.bat" vinex\\01_gebieden.py
"""
import csv
import os

import numpy as np
from osgeo import gdal, osr
from scipy import ndimage

gdal.UseExceptions()

# VinexPlans.tif staat in Overig/Beleid van de gedeelde SourceData; alleen lezen
BRON = os.environ.get("VINEX_PLANS", r"D:\Objectvision\Object Vision - SourceData\Overig\Beleid\VinexPlans.tif")
MAP = r"C:\LocalData\Stikstof\Vinex"
UIT_TIF = os.path.join(MAP, "vinex_gebieden_100m.tif")
UIT_CSV = os.path.join(MAP, "vinex_gebieden.csv")
os.makedirs(MAP, exist_ok=True)

# het grid van de configuratie: geography/NL_grid/domain_100m
X_MIN, X_MAX, Y_MIN, Y_MAX, CEL = 0, 280000, 300000, 625000, 100
KOL, RIJ = (X_MAX - X_MIN) // CEL, (Y_MAX - Y_MIN) // CEL

ds = gdal.Open(BRON)
gt = ds.GetGeoTransform()
assert gt[1] == CEL and abs(gt[5]) == CEL, f"celgrootte {gt[1]} past niet op {CEL}"
masker_bron = ds.GetRasterBand(1).ReadAsArray() == 1

# het bronraster in het doelgrid leggen; beide hebben de bovenkant op y_max
dx = int(round((gt[0] - X_MIN) / CEL))
dy = int(round((Y_MAX - gt[3]) / CEL))
masker = np.zeros((RIJ, KOL), dtype=bool)
masker[dy:dy + masker_bron.shape[0], dx:dx + masker_bron.shape[1]] = masker_bron
print(f"doelgrid {KOL} x {RIJ}, bron ingelegd op kolom {dx}, rij {dy}; {masker.sum():,} Vinex-cellen")

gesloten = ndimage.binary_closing(masker, structure=np.ones((7, 7), bool))
lab, n = ndimage.label(gesloten, structure=np.ones((3, 3), bool))
lab[~masker] = 0                      # nummers alleen op echte Vinex-cellen

# hernummeren op grootte, zodat gebied 1 het grootste is
tel = np.bincount(lab.ravel(), minlength=n + 1)
tel[0] = 0
volgorde = np.argsort(tel)[::-1]
nieuw = np.zeros(n + 1, dtype=np.int32)
nieuw[volgorde[tel[volgorde] > 0]] = np.arange(1, (tel > 0).sum() + 1, dtype=np.int32)
lab = nieuw[lab]
n = int(lab.max())
print(f"{n} gebieden")

drv = gdal.GetDriverByName("GTiff")
uit = drv.Create(UIT_TIF, KOL, RIJ, 1, gdal.GDT_Int32, options=["COMPRESS=LZW", "TILED=YES"])
uit.SetGeoTransform((X_MIN, CEL, 0, Y_MAX, 0, -CEL))
srs = osr.SpatialReference()
srs.ImportFromEPSG(28992)
uit.SetProjection(srs.ExportToWkt())
uit.GetRasterBand(1).WriteArray(lab)
uit.GetRasterBand(1).SetNoDataValue(0)
uit.FlushCache()
uit = None

with open(UIT_CSV, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["gebied", "hectare", "x_midden", "y_midden", "x_min", "x_max", "y_min", "y_max"])
    for i in range(1, n + 1):
        ys, xs = np.where(lab == i)
        w.writerow([i, len(xs),
                    round(X_MIN + (xs.mean() + .5) * CEL), round(Y_MAX - (ys.mean() + .5) * CEL),
                    X_MIN + xs.min() * CEL, X_MIN + (xs.max() + 1) * CEL,
                    Y_MAX - (ys.max() + 1) * CEL, Y_MAX - ys.min() * CEL])
print(f"geschreven: {UIT_TIF} en {UIT_CSV}")
