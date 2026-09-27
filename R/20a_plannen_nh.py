"""Afstand van de woningbouwplannen in de Noord-Hollandse monitor plancapaciteit tot stikstofgevoelig habitat.

Gebruik (met de Python van QGIS, want GDAL is nodig):
  "C:\\Program Files\\QGIS 3.40.7\\bin\\python-qgis-ltr.bat" R/20a_plannen_nh.py <uitvoer.csv> [datamap]

Twee standen van dezelfde bron, beide openbaar, met Almere en Lelystad erbij:
  2023  bron/rs_plancapaciteit/afgeleid/noord_holland_monitor_plancapaciteit_20230719.gpkg
        (laag monitor_plancapaciteit_2023_openbaar, 19 juli 2023)
  2025  bron/plannen/plancapaciteit_nh_fl/monitor_plancapaciteit_shapefiles_nov2025.zip
        (shapefile plancapnov2025EPSG28992, november 2025; rechtstreeks uit de zip gelezen)
Plannen met de vlag vertrouwelijk (vertwlk = 1, alleen in 2025) blijven buiten de uitvoer.

De afstand is die tot de AERIUS relevante habitatkartering (bron/aerius_relevant_habitats.gpkg), dezelfde grootheid
als dist2n2000_m van de BAG-objecten, op het raster van 25 meter: afstand_min is de kleinste afstand over de cellen
die het planvlak raakt, zoals een project in de BAG op zijn dichtstbijzijnde object telt.

uitvoer.csv  stand;plannr;cbs_code;gemeente;totaal;hard;b_bsg;app;afstand_min
  hard   1 als de status hard is (1A tot en met 2C: onherroepelijk of vastgesteld), anders 0
  app    appartementen volgens de woningtypen (alleen 2025; anders leeg)
"""
import csv
import os
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr

gdal.UseExceptions()

uit = Path(sys.argv[1])
data = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
bron = data / "bron"
GRID = bron / "afgeleid" / "stedelijk_gebied_nl_25m.tif"
HABITAT = bron / "aerius_relevant_habitats.gpkg"
STANDEN = [
    ("2023", str(bron / "rs_plancapaciteit" / "afgeleid" / "noord_holland_monitor_plancapaciteit_20230719.gpkg"),
     "monitor_plancapaciteit_2023_openbaar"),
    ("2025", "/vsizip/" + str(bron / "plannen" / "plancapaciteit_nh_fl" / "monitor_plancapaciteit_shapefiles_nov2025.zip").replace("\\", "/")
     + "/ShapefilentoelichtingNov2025/shapefile/plancapnov2025EPSG28992.shp", "plancapnov2025EPSG28992"),
]
MAX = 20000


def main():
    ref = gdal.Open(str(GRID))
    gt = ref.GetGeoTransform()
    nx, ny = ref.RasterXSize, ref.RasterYSize
    mem = gdal.GetDriverByName("MEM")
    hab = mem.Create("", nx, ny, 1, gdal.GDT_Byte)
    hab.SetGeoTransform(gt); hab.SetProjection(ref.GetProjection())
    bronlaag = ogr.Open(str(HABITAT))
    laag = bronlaag.GetLayer(0).GetName()
    bronlaag = None
    gdal.Rasterize(hab, str(HABITAT), options=gdal.RasterizeOptions(layers=[laag], burnValues=[1], allTouched=True))
    afst = mem.Create("", nx, ny, 1, gdal.GDT_UInt16)
    afst.SetGeoTransform(gt); afst.SetProjection(ref.GetProjection())
    gdal.ComputeProximity(hab.GetRasterBand(1), afst.GetRasterBand(1), ["VALUES=1", "DISTUNITS=GEO", f"MAXDIST={MAX}", "NODATA=65535"])
    A = afst.GetRasterBand(1).ReadAsArray()
    print(f"habitat {laag}: {int(hab.GetRasterBand(1).ReadAsArray().sum())} cellen; afstandsraster klaar", flush=True)
    hab = afst = None

    rijen = []
    for stand, pad, laagnaam in STANDEN:
        ds = ogr.Open(pad)
        lg = ds.GetLayerByName(laagnaam)
        velden = [lg.GetLayerDefn().GetFieldDefn(i).GetName() for i in range(lg.GetLayerDefn().GetFieldCount())]
        n = weg = 0
        for f in lg:
            if "vertwlk" in velden and (f.GetField("vertwlk") or 0) == 1:
                weg += 1
                continue
            g = f.GetGeometryRef()
            if g is None or g.IsEmpty():
                continue
            minx, maxx, miny, maxy = g.GetEnvelope()
            # venster op het raster rond het plan
            c0 = max(int(np.floor((minx - gt[0]) / gt[1])) - 1, 0)
            c1 = min(int(np.floor((maxx - gt[0]) / gt[1])) + 1, nx - 1)
            r0 = max(int(np.floor((maxy - gt[3]) / gt[5])) - 1, 0)
            r1 = min(int(np.floor((miny - gt[3]) / gt[5])) + 1, ny - 1)
            w, h = c1 - c0 + 1, r1 - r0 + 1
            vak = mem.Create("", w, h, 1, gdal.GDT_Byte)
            vak.SetGeoTransform((gt[0] + c0 * gt[1], gt[1], 0, gt[3] + r0 * gt[5], 0, gt[5]))
            vak.SetProjection(ref.GetProjection())
            tmp = ogr.GetDriverByName("Memory").CreateDataSource("")
            tl = tmp.CreateLayer("p", srs=lg.GetSpatialRef(), geom_type=ogr.wkbMultiPolygon)
            ft = ogr.Feature(tl.GetLayerDefn()); ft.SetGeometry(g.Clone()); tl.CreateFeature(ft)
            gdal.RasterizeLayer(vak, [1], tl, burn_values=[1], options=["ALL_TOUCHED=TRUE"])
            m = vak.GetRasterBand(1).ReadAsArray().astype(bool)
            waarden = A[r0:r1 + 1, c0:c1 + 1][m]
            dmin = int(waarden.min()) if waarden.size else -1
            if dmin == 65535:
                dmin = MAX + 1
            status = (f.GetField("status") or "").lower()
            hard = 1 if status[:1] in ("1", "2") else 0   # 1A tot en met 2C: onherroepelijk of vastgesteld
            app = ""
            if "wtypAPP" in velden:
                app = f.GetField("wtypAPP") or 0
            rijen.append([stand, f.GetField("Plannr"), f.GetField("cbs_code"), f.GetField("gemeente"), f.GetField("totaal") or 0,
                          hard, f.GetField("b_bsg"), app, dmin])
            n += 1
        print(f"stand {stand}: {n} plannen, {weg} vertrouwelijk overgeslagen", flush=True)
        ds = None
    with open(uit, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["stand", "plannr", "cbs_code", "gemeente", "totaal", "hard", "b_bsg", "app", "afstand_min"])
        w.writerows(rijen)
    print(f"geschreven: {uit}")


if __name__ == "__main__":
    main()
