"""Verstedelijkingscontour van voor de bouwperiode, als raster op het projectgrid.

Gebruik (met de Python van QGIS):
  "C:\\Program Files\\QGIS 3.40.7\\bin\\python-qgis-ltr.bat" R/11b_contour_vroeg.py <uitvoer.tif> [datamap]

De contour in afgeleid/stedelijk_gebied_nl_25m.tif is die van september
2026: hij is gegroeid waar gebouwd is, dus wijken die in 2015 als uitleg
begonnen, liggen er nu binnen. Voor een toets op verschuiving van uitleg naar
binnenstedelijk bouwen is een contour nodig die vastlag voordat er gebouwd
werd. Die bestaat voor vijf provincies (bron: planregistraties/contouren,
de gedateerde IMRO-reeks en de Zuid-Hollandse BSD-reeks):

  Noord-Holland  Provinciale Ruimtelijke Verordening, 2 november 2010,
                 Bestaand bebouwd gebied
  Fryslan        Verordening Romte, 15 juni 2011, Bestaand stedelijk gebied
  Utrecht        Provinciale Ruimtelijke Verordening, 20 maart 2012,
                 Wonen stedelijk gebied
  Zuid-Holland   Bestaand stads- en dorpsgebied 2014
  Overijssel     bebouwde kommen van 2013 (de huidige laag voor Overijssel is
                 al die van 2013, dus hier overgenomen uit het landelijke
                 raster)

Uitvoer: Byte, 1 binnen de vroege contour, 0 erbuiten binnen deze vijf
provincies (volgens contourbron_nl_25m.tif), 255 elders.
"""
import os
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr

gdal.UseExceptions(); ogr.UseExceptions()

uit = Path(sys.argv[1])
data = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
bron = data / "bron"
ref = gdal.Open(str(bron / "afgeleid" / "stedelijk_gebied_nl_25m.tif"))
nx, ny, gt, proj = ref.RasterXSize, ref.RasterYSize, ref.GetGeoTransform(), ref.GetProjection()
huidig = ref.GetRasterBand(1).ReadAsArray()
ds_prov = gdal.Open(str(bron / "afgeleid" / "contourbron_nl_25m.tif"))   # referentie houden, anders ruimt GDAL de band op
provincie = ds_prov.GetRasterBand(1).ReadAsArray()

mem = gdal.GetDriverByName("MEM").Create("", nx, ny, 1, gdal.GDT_Byte)
mem.SetGeoTransform(gt); mem.SetProjection(proj)
mem.GetRasterBand(1).Fill(0)

C = bron / "planregistraties" / "contouren"
imro = ogr.Open(str(C / "pdok_verordeningen_imro_contouren_selectie_20260701.gpkg"))
lyr = imro.GetLayer("besluitvlak_contouren")
selecties = [
    ("Noord-Holland 2010", "datum_verordening = '2010-11-02' AND naam = 'Bestaand bebouwd gebied'"),
    ("Fryslan 2011", "datum_verordening = '2011-06-15' AND naam = 'Bestaand stedelijk gebied'"),
    ("Utrecht 2012", "datum_verordening = '2012-03-20' AND naam = 'Wonen stedelijk gebied'"),
]
for naam, waar in selecties:
    lyr.SetAttributeFilter(waar)
    n = lyr.GetFeatureCount()
    if n == 0:
        raise SystemExit(f"{naam}: geen vlakken gevonden met {waar}")
    gdal.RasterizeLayer(mem, [1], lyr, burn_values=[1])
    print(f"{naam}: {n} vlakken", flush=True)
lyr.SetAttributeFilter(None)
## De BSD-laag is een MULTISURFACE (met bogen) en in 2014 niet geldig
## (zelfdoorsnijding): lineair maken, geldig maken en via een laag in het
## geheugen rasteriseren.
ds_zh = ogr.Open(str(C / "zuidholland_bsd_reeks.gpkg"))   # referentie houden
zh = ds_zh.GetLayer("bsd_2014")
tmp = ogr.GetDriverByName("Memory").CreateDataSource("zh")
tl = tmp.CreateLayer("bsd", srs=zh.GetSpatialRef(), geom_type=ogr.wkbMultiPolygon)
ha = 0.0
for f in zh:
    g = f.GetGeometryRef().GetLinearGeometry()
    if not g.IsValid():
        g = g.MakeValid()
    ha += g.GetArea() / 1e4
    nf = ogr.Feature(tl.GetLayerDefn()); nf.SetGeometry(g); tl.CreateFeature(nf)
gdal.RasterizeLayer(mem, [1], tl, burn_values=[1])
print(f"Zuid-Holland BSD 2014: {zh.GetFeatureCount()} vlak, {ha:.0f} ha na lineair en geldig maken", flush=True)

vroeg = mem.GetRasterBand(1).ReadAsArray()
# CBS-provinciecodes: 21 Fryslan, 23 Overijssel, 26 Utrecht, 27 Noord-Holland, 28 Zuid-Holland
uitv = np.full((ny, nx), 255, dtype=np.uint8)
for code in (21, 26, 27, 28):
    m = provincie == code
    uitv[m] = vroeg[m]
m = provincie == 23
uitv[m] = np.where(huidig[m] == 1, 1, 0)
for code, naam in ((21, "Fryslan"), (23, "Overijssel"), (26, "Utrecht"), (27, "Noord-Holland"), (28, "Zuid-Holland")):
    m = provincie == code
    ha_vroeg = (uitv[m] == 1).sum() * 25 * 25 / 1e4
    ha_nu = (huidig[m] == 1).sum() * 25 * 25 / 1e4
    print(f"{naam:14s} binnen vroege contour {ha_vroeg:9.0f} ha, binnen huidige contour {ha_nu:9.0f} ha")

uit.parent.mkdir(parents=True, exist_ok=True)
drv = gdal.GetDriverByName("GTiff")
ds = drv.Create(str(uit), nx, ny, 1, gdal.GDT_Byte, options=["COMPRESS=DEFLATE", "TILED=YES"])
ds.SetGeoTransform(gt); ds.SetProjection(proj)
b = ds.GetRasterBand(1); b.WriteArray(uitv); b.SetNoDataValue(255)
ds = None
print(f"geschreven: {uit}")
