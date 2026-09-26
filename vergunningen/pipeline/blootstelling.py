"""Blootstelling van elke zaak aan stikstofbeleid, uit de AERIUS-hexagonen in plaats van uit de habitatkaart.

Draait onder de Python van QGIS (GDAL, numpy, scipy):
  "C:\\Program Files\\QGIS 3.38.1\\bin\\python-qgis.bat" blootstelling.py [hexagonen.gpkg]

Twee maten, beide op de hoofdlocatie van de zaak:
1. dist_overbelast_m: afstand tot het dichtstbijzijnde overbelaste hexagon (above_cl = 1: depositie boven de kritische
   depositiewaarde), via een raster van 25 meter en gdal ComputeProximity, afgekapt op 10 kilometer; klasse in dezelfde
   indeling als de habitatafstand (0 tot 250, 250 tot 500, 500 tot 1.000, 1.000 tot 2.500, 2.500 tot 5.000, verder).
2. ovb_max_1km, ovb_max_5km, ovb_max_25km: de grootste overbelasting (depositie min KDW, mol N per hectare per jaar,
   AERIUS 2025, depositie 2023) binnen een vierkant venster van 1, 5 en 25 kilometer om de zaak, op een raster van
   250 meter; 25 kilometer is de rekenafstand van AERIUS sinds 2021. Nul betekent geen overbelast hexagon in het venster.

De rasters worden naast het hexagonenbestand bewaard en hergebruikt.
"""
import sqlite3
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr
from scipy.ndimage import maximum_filter

gdal.UseExceptions()
ogr.UseExceptions()
sys.path.insert(0, str(Path(__file__).parent))
from config import DB_PAD  # noqa: E402
from parse import zorg_kolom  # noqa: E402

X0, Y0, X1, Y1 = 0.0, 300000.0, 300000.0, 630000.0
MAXDIST_M = 10000.0
KLASSEN = ((250, "0_250"), (500, "250_500"), (1000, "500_1000"), (2500, "1000_2500"), (5000, "2500_5000"))
VENSTERS_KM = (1, 5, 25)


def klasse(d):
    for grens, naam in KLASSEN:
        if d < grens:
            return naam
    return "5000_"


def maak_rasters(bron: str):
    ds = ogr.Open(bron)
    laag = ds.GetLayer("hexagonen")
    drv = gdal.GetDriverByName("GTiff")
    srs = laag.GetSpatialRef().ExportToWkt()
    afst = Path(bron).with_name("afstand_overbelast_25m.tif")
    ovb = Path(bron).with_name("overbelasting_250m.tif")
    if not afst.exists():
        cel = 25.0
        kol, rij = int((X1 - X0) / cel), int((Y1 - Y0) / cel)
        laag.SetAttributeFilter("above_cl = 1")
        print(f"rasteren van {laag.GetFeatureCount()} overbelaste hexagonen op {cel} m", flush=True)
        masker = drv.Create(str(afst.with_name("overbelast_masker_25m.tif")), kol, rij, 1, gdal.GDT_Byte, options=["COMPRESS=DEFLATE", "TILED=YES"])
        masker.SetGeoTransform((X0, cel, 0, Y1, 0, -cel))
        masker.SetProjection(srs)
        gdal.RasterizeLayer(masker, [1], laag, burn_values=[1], options=["ALL_TOUCHED=TRUE"])
        print("afstand berekenen", flush=True)
        a = drv.Create(str(afst), kol, rij, 1, gdal.GDT_Float32, options=["COMPRESS=DEFLATE", "TILED=YES"])
        a.SetGeoTransform(masker.GetGeoTransform())
        a.SetProjection(srs)
        gdal.ComputeProximity(masker.GetRasterBand(1), a.GetRasterBand(1), options=["VALUES=1", "DISTUNITS=GEO", f"MAXDIST={MAXDIST_M}", f"NODATA={MAXDIST_M}"])
        a.FlushCache()
        a = None
        masker = None
        laag.SetAttributeFilter(None)
    if not ovb.exists():
        cel = 250.0
        kol, rij = int((X1 - X0) / cel), int((Y1 - Y0) / cel)
        laag.SetAttributeFilter("above_cl = 1")
        print(f"overbelasting rasteren op {cel} m", flush=True)
        o = drv.Create(str(ovb), kol, rij, 1, gdal.GDT_Float32, options=["COMPRESS=DEFLATE", "TILED=YES"])
        o.SetGeoTransform((X0, cel, 0, Y1, 0, -cel))
        o.SetProjection(srs)
        o.GetRasterBand(1).Fill(0)
        gdal.RasterizeLayer(o, [1], laag, options=["ATTRIBUTE=overbelasting_2023", "ALL_TOUCHED=TRUE"])
        o.FlushCache()
        o = None
        laag.SetAttributeFilter(None)
    return afst, ovb


def main(argv):
    bron = argv[0] if argv else r"C:\LocalData\vergunningen\bron\natuur\aerius_hexagonen_overbelasting.gpkg"
    afst, ovb = maak_rasters(bron)
    da = gdal.Open(str(afst))
    arr_a = da.GetRasterBand(1).ReadAsArray()
    gt_a = da.GetGeoTransform()
    do = gdal.Open(str(ovb))
    arr_o = do.GetRasterBand(1).ReadAsArray()
    gt_o = do.GetGeoTransform()
    arr_o = np.where(arr_o > 0, arr_o, 0).astype(np.float32)
    vensters = {}
    for km in VENSTERS_KM:
        halve = int(round(km * 1000 / gt_o[1]))
        vensters[km] = maximum_filter(arr_o, size=2 * halve + 1, mode="constant", cval=0)
        print(f"venster {km} km klaar", flush=True)
    conn = sqlite3.connect(DB_PAD, timeout=120)
    for k, t in (("dist_overbelast_m", "REAL"), ("overbelast_klasse", "TEXT"), ("ovb_max_1km", "REAL"), ("ovb_max_5km", "REAL"), ("ovb_max_25km", "REAL")):
        zorg_kolom(conn, "zaak", k, t)
    rijen = conn.execute("SELECT zaak_id, x, y FROM zaak WHERE x IS NOT NULL").fetchall()
    ids = np.array([r[0] for r in rijen])
    xs = np.array([r[1] for r in rijen], dtype=float)
    ys = np.array([r[2] for r in rijen], dtype=float)

    def lees(arr, gt):
        kol = ((xs - gt[0]) / gt[1]).astype(int)
        rij = ((ys - gt[3]) / gt[5]).astype(int)
        ok = (kol >= 0) & (kol < arr.shape[1]) & (rij >= 0) & (rij < arr.shape[0])
        uit = np.full(len(ids), np.nan)
        uit[ok] = arr[rij[ok], kol[ok]]
        return uit
    d = np.minimum(np.nan_to_num(lees(arr_a, gt_a), nan=MAXDIST_M), MAXDIST_M)
    o1, o5, o25 = (np.nan_to_num(lees(vensters[km], gt_o), nan=0.0) for km in VENSTERS_KM)
    conn.executemany("UPDATE zaak SET dist_overbelast_m=?, overbelast_klasse=?, ovb_max_1km=?, ovb_max_5km=?, ovb_max_25km=? WHERE zaak_id=?",
                     [(round(float(a), 1), klasse(float(a)), round(float(b), 1), round(float(c), 1), round(float(e), 1), int(z))
                      for a, b, c, e, z in zip(d, o1, o5, o25, ids)])
    conn.commit()
    print("klassen overbelast:", conn.execute("SELECT overbelast_klasse, COUNT(*) FROM zaak WHERE overbelast_klasse IS NOT NULL GROUP BY 1 ORDER BY 1").fetchall())
    print("ovb_max_25km > 0:", conn.execute("SELECT COUNT(*) FROM zaak WHERE ovb_max_25km > 0").fetchone()[0], "van", len(ids))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
