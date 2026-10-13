"""Afstand tot stikstofgevoelig habitat voor punten en planvlakken, op een raster van 25 meter.

Gebruik (met de Python van QGIS, want GDAL is nodig):
  "C:\\Program Files\\QGIS 3.40.7\\bin\\python-qgis-ltr.bat" R/22a_habitat.py punten <punten.csv> <uitvoer.csv> [datamap]
  "C:\\Program Files\\QGIS 3.40.7\\bin\\python-qgis-ltr.bat" R/22a_habitat.py plannen <projecten.geojson> <uitvoer.csv> [datamap]

De afstand is die tot de AERIUS relevante habitatkartering (bron/aerius_relevant_habitats.gpkg), dezelfde grootheid als
dist2n2000_m van de BAG-objecten en als in 20a_plannen_nh.py: het habitat wordt op het landelijke raster van 25 meter
gebrand (alle geraakte cellen), en gdal ComputeProximity geeft per cel de afstand tot het midden van de dichtstbijzijnde
habitatcel, tot 20 km. Het afstandsraster wordt een keer gemaakt en bewaard in _derived/habitat_afstand_25m.tif.

punten   punten.csv id;x;y in RD (EPSG:28992); uitvoer id;afstand;ndep_2018: de afstand (-1 buiten het raster, 20001
         verder dan 20 km) en de totale stikstofdepositie van 2018 op de plek (RIVM, 1 km, zoals in 11a_rasters.py; -1
         zonder waarde)
plannen  de projectvlakken van de nationale woningbouwkaart (VRO, levering 18 augustus 2026, GeoJSON in WGS84); per vlak
         de kleinste afstand over de cellen die het vlak raakt, zoals een project in de BAG op zijn dichtstbijzijnde
         object telt, en de afstand van het zwaartepunt; uitvoer met de velden van het project
ringen   (invoer wordt genegeerd) de oppervlakte per ring van 0 tot 1, 1 tot 2,5, 2,5 tot 5 en verder dan 5 km, in heel
         Nederland (water inbegrepen) en binnen de verstedelijkingscontour
"""
import csv
import os
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr, osr

gdal.UseExceptions()
osr.UseExceptions()

HIER = Path(__file__).resolve().parent
CACHE = HIER / "_derived" / "habitat_afstand_25m.tif"
MAX = 20000


def afstandsraster(bron):
    """Het afstandsraster als array plus geotransformatie; maakt het de eerste keer."""
    if not CACHE.exists():
        ref = gdal.Open(str(bron / "afgeleid" / "stedelijk_gebied_nl_25m.tif"))
        gt, nx, ny = ref.GetGeoTransform(), ref.RasterXSize, ref.RasterYSize
        mem = gdal.GetDriverByName("MEM")
        hab = mem.Create("", nx, ny, 1, gdal.GDT_Byte)
        hab.SetGeoTransform(gt); hab.SetProjection(ref.GetProjection())
        habitat = bron / "aerius_relevant_habitats.gpkg"
        ds = ogr.Open(str(habitat)); laag = ds.GetLayer(0).GetName(); ds = None
        gdal.Rasterize(hab, str(habitat), options=gdal.RasterizeOptions(layers=[laag], burnValues=[1], allTouched=True))
        uit = gdal.GetDriverByName("GTiff").Create(str(CACHE), nx, ny, 1, gdal.GDT_UInt16, ["COMPRESS=DEFLATE", "TILED=YES"])
        uit.SetGeoTransform(gt); uit.SetProjection(ref.GetProjection())
        gdal.ComputeProximity(hab.GetRasterBand(1), uit.GetRasterBand(1), ["VALUES=1", "DISTUNITS=GEO", f"MAXDIST={MAX}", "NODATA=65535"])
        uit.FlushCache(); uit = hab = None
        print(f"afstandsraster gemaakt: {CACHE}", flush=True)
    r = gdal.Open(str(CACHE))
    return r.GetRasterBand(1).ReadAsArray(), r.GetGeoTransform(), r.GetProjection()


def punten(invoer, uitvoer, bron):
    A, gt, _ = afstandsraster(bron)
    ny, nx = A.shape
    ids, xs, ys = [], [], []
    with open(invoer, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            ids.append(r["id"]); xs.append(float(r["x"])); ys.append(float(r["y"]))
    x, y = np.array(xs), np.array(ys)
    kol = np.floor((x - gt[0]) / gt[1]).astype(np.int64)
    rij = np.floor((y - gt[3]) / gt[5]).astype(np.int64)
    binnen = (kol >= 0) & (kol < nx) & (rij >= 0) & (rij < ny)
    a = np.full(len(ids), -1, dtype=np.int64)
    a[binnen] = A[rij[binnen], kol[binnen]]
    a[a == 65535] = MAX + 1
    # de totale stikstofdepositie van 2018 op de plek (RIVM, 1 km), zoals in 11a_rasters.py; -1 buiten het raster
    n = lees(bron / "depositie" / "gdn_depositie_historische_reeks" / "ndep_historisch_2018.tif", x, y)
    # de afstand tot het dichtstbijzijnde overbelaste hexagon van AERIUS (depositie 2023 boven de KDW, 25 m, afgekapt op
    # 10 km; blootstelling.py van de bekendmakingen) en de vroege verstedelijkingscontour (11b_contour_vroeg.py: 1 binnen,
    # 0 erbuiten in de vijf provincies met een contour van 2010 tot en met 2014); -1 buiten het raster of zonder waarde
    o = lees(bron / "natuur" / "afstand_overbelast_25m.tif", x, y)
    vroeg = HIER / "_derived" / "stedelijk_vroeg_25m.tif"
    c = lees(vroeg, x, y) if vroeg.exists() else np.full(len(ids), -1.0)
    with open(uitvoer, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["id", "afstand", "ndep_2018", "afst_overbelast", "stedelijk_vroeg"])
        w.writerows(zip(ids, a.tolist(), np.round(n, 1).tolist(), np.round(o).astype(np.int64).tolist(), c.astype(np.int64).tolist()))
    print(f"{len(ids)} punten, {int((~binnen).sum())} buiten het afstandsraster, {int((n < 0).sum())} zonder depositie, "
          f"{int((o < 0).sum())} zonder afstand tot overbelasting, {int((c >= 0).sum())} in een provincie met vroege contour: {uitvoer}")


def lees(pad, x, y):
    """Een raster aflezen op punten met zijn eigen geotransformatie; -1 buiten het raster, bij nodata of negatief."""
    ds = gdal.Open(str(pad))
    b = ds.GetRasterBand(1); R = b.ReadAsArray().astype(np.float64); nd = b.GetNoDataValue(); g = ds.GetGeoTransform(); ds = None
    k = np.floor((x - g[0]) / g[1]).astype(np.int64); r = np.floor((y - g[3]) / g[5]).astype(np.int64)
    ok = (k >= 0) & (k < R.shape[1]) & (r >= 0) & (r < R.shape[0])
    v = np.full(len(x), -1.0)
    v[ok] = R[r[ok], k[ok]]
    v[(v < 0) | ((v == nd) if nd is not None else False) | np.isnan(v)] = -1
    return v


VELDEN = ["project_id", "gemeente_code", "gemeente_naam", "provincie_code", "Planstatus", "peilmoment",
          "Netto Plancapaciteit", "Bruto Plancapaciteit", "Eengezins", "Meergezins"]


def plannen(invoer, uitvoer, bron):
    A, gt, proj = afstandsraster(bron)
    ny, nx = A.shape
    rd = osr.SpatialReference(); rd.ImportFromEPSG(28992)
    wgs = osr.SpatialReference(); wgs.ImportFromEPSG(4326)
    wgs.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    rd.SetAxisMappingStrategy(osr.OAMS_TRADITIONAL_GIS_ORDER)
    naar_rd = osr.CoordinateTransformation(wgs, rd)
    mem = gdal.GetDriverByName("MEM")
    ds = ogr.Open(str(invoer))
    lg = ds.GetLayer(0)
    defn = lg.GetLayerDefn()
    alle_velden = [defn.GetFieldDefn(i).GetName() for i in range(defn.GetFieldCount())]
    perioden = [v for v in alle_velden if "t/m" in v or v.lower().startswith("na ") or v.lower().startswith("voor ")]
    print("velden:", alle_velden, flush=True)
    rijen, leeg = [], 0
    for f in lg:
        g = f.GetGeometryRef()
        if g is None or g.IsEmpty():
            leeg += 1
            continue
        g = g.Clone(); g.Transform(naar_rd)
        minx, maxx, miny, maxy = g.GetEnvelope()
        c0 = max(int(np.floor((minx - gt[0]) / gt[1])) - 1, 0)
        c1 = min(int(np.floor((maxx - gt[0]) / gt[1])) + 1, nx - 1)
        r0 = max(int(np.floor((maxy - gt[3]) / gt[5])) - 1, 0)
        r1 = min(int(np.floor((miny - gt[3]) / gt[5])) + 1, ny - 1)
        vak = mem.Create("", c1 - c0 + 1, r1 - r0 + 1, 1, gdal.GDT_Byte)
        vak.SetGeoTransform((gt[0] + c0 * gt[1], gt[1], 0, gt[3] + r0 * gt[5], 0, gt[5])); vak.SetProjection(proj)
        tmp = ogr.GetDriverByName("Memory").CreateDataSource("")
        tl = tmp.CreateLayer("p", srs=rd, geom_type=ogr.wkbMultiPolygon)
        ft = ogr.Feature(tl.GetLayerDefn()); ft.SetGeometry(g); tl.CreateFeature(ft)
        gdal.RasterizeLayer(vak, [1], tl, burn_values=[1], options=["ALL_TOUCHED=TRUE"])
        m = vak.GetRasterBand(1).ReadAsArray().astype(bool)
        w = A[r0:r1 + 1, c0:c1 + 1][m]
        dmin = int(w.min()) if w.size else -1
        dmin = MAX + 1 if dmin == 65535 else dmin
        c = g.Centroid()
        kc, rc = int(np.floor((c.GetX() - gt[0]) / gt[1])), int(np.floor((c.GetY() - gt[3]) / gt[5]))
        dc = int(A[rc, kc]) if 0 <= kc < nx and 0 <= rc < ny else -1
        dc = MAX + 1 if dc == 65535 else dc
        rijen.append([f.GetField(v) for v in VELDEN] + [f.GetField(v) for v in perioden]
                     + [round(g.GetArea() / 1e4, 2), round(c.GetX()), round(c.GetY()), dmin, dc])
        vak = tmp = None
    with open(uitvoer, "w", newline="", encoding="utf-8") as fo:
        w = csv.writer(fo, delimiter=";")
        w.writerow([v.lower().replace(" ", "_") for v in VELDEN] + ["periode_" + v.replace(" ", "_").replace("/", "") for v in perioden]
                   + ["ha", "x", "y", "afstand_min", "afstand_midden"])
        w.writerows(rijen)
    print(f"{len(rijen)} projecten, {leeg} zonder geometrie: {uitvoer}")


def ringen(_, uitvoer, bron):
    """Oppervlakte per ring in km2: alle cellen binnen Nederland (het landelijke raster, water inbegrepen) en de cellen
    binnen de verstedelijkingscontour."""
    A, gt, _ = afstandsraster(bron)
    ds = gdal.Open(str(bron / "afgeleid" / "stedelijk_gebied_nl_25m.tif"))
    s = ds.GetRasterBand(1).ReadAsArray()
    ds = None
    grenzen = [(0, 1000, "0 tot 1 km"), (1001, 2500, "1 tot 2,5 km"), (2501, 5000, "2,5 tot 5 km"), (5001, 70000, "verder dan 5 km")]
    km2 = gt[1] * -gt[5] / 1e6
    with open(uitvoer, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["ring", "km2_nederland", "km2_contour"])
        for lo, hi, naam in grenzen:
            r = (A >= lo) & (A <= hi)
            w.writerow([naam, round(float(((s != 255) & r).sum()) * km2, 1), round(float(((s == 1) & r).sum()) * km2, 1)])
    print(f"geschreven: {uitvoer}")


if __name__ == "__main__":
    modus, invoer, uitvoer = sys.argv[1], Path(sys.argv[2]), Path(sys.argv[3])
    data = Path(sys.argv[4]) if len(sys.argv) > 4 else Path(os.environ.get(
        "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
    {"punten": punten, "plannen": plannen, "ringen": ringen}[modus](invoer, uitvoer, data / "bron")
