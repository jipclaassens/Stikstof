"""Afstand van elke zaak tot het dichtstbijzijnde stikstofgevoelige habitat.

Dezelfde grootheid als dist2N2000_m in cfg/main/Analyse/N2000_Analyse.dms (afstand van de locatie tot de AERIUS
relevante habitatkartering), maar dan per zaak uit de bekendmakingen in plaats van per BAG-object, zodat aanvragen,
weigeringen, intrekkingen en doorlooptijden per afstandsklasse te vergelijken zijn met de opleveringen uit de BAG.

Draait onder de Python van QGIS (GDAL):
  "C:\\Program Files\\QGIS 3.38.1\\bin\\python-qgis.bat" afstand.py <habitatbestand> [laag] [--cel 20]

Werkwijze: de habitatvlakken worden eenmalig gerasterd op een cel van 20 meter over heel Nederland en daarvan wordt een
afstandsraster gemaakt (gdal ComputeProximity, in meters, afgekapt op 10 kilometer); dat raster wordt naast het
habitatbestand bewaard en hergebruikt. Elke zaak leest de afstand af op zijn hoofdlocatie. De onzekerheid is een halve
cel, ruim binnen de klassegrenzen van 250 meter en verder. Een exacte vectorafstand op de 1.366 vlakken (samen 237 MB
geometrie) kostte minuten per duizend zaken en is daarom losgelaten.

Schrijft de kolom dist2n2000_m in de tabel zaak en de klassen uit de bestaande analyse (0 tot 250, 250 tot 500,
500 tot 1.000, 1.000 tot 2.500, 2.500 tot 5.000, verder).
"""
import sqlite3
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr

gdal.UseExceptions()
ogr.UseExceptions()
sys.path.insert(0, str(Path(__file__).parent))
from config import DB_PAD  # noqa: E402
from parse import zorg_kolom  # noqa: E402

MAXDIST_M = 10000.0
KLASSEN = ((250, "0_250"), (500, "250_500"), (1000, "500_1000"), (2500, "1000_2500"), (5000, "2500_5000"))
# omhullende van Nederland in RD, met marge
X0, Y0, X1, Y1 = 0.0, 300000.0, 300000.0, 630000.0


def klasse(d):
    for grens, naam in KLASSEN:
        if d < grens:
            return naam
    return "5000_"


def afstandsraster(bron: str, laagnaam, cel: float) -> Path:
    """Maakt (of hergebruikt) het afstandsraster naast het habitatbestand."""
    uit = Path(bron).with_name(f"{Path(bron).stem}_afstand_{int(cel)}m.tif")
    if uit.exists():
        return uit
    ds = ogr.Open(bron)
    laag = ds.GetLayer(laagnaam) if laagnaam else ds.GetLayer(0)
    kol, rij = int((X1 - X0) / cel), int((Y1 - Y0) / cel)
    print(f"rasteren van {laag.GetFeatureCount()} vlakken op {cel} m: {kol} x {rij} cellen", flush=True)
    drv = gdal.GetDriverByName("GTiff")
    masker = drv.Create(str(uit.with_name(uit.stem + "_masker.tif")), kol, rij, 1, gdal.GDT_Byte,
                        options=["COMPRESS=DEFLATE", "TILED=YES"])
    masker.SetGeoTransform((X0, cel, 0, Y1, 0, -cel))
    masker.SetProjection(laag.GetSpatialRef().ExportToWkt())
    gdal.RasterizeLayer(masker, [1], laag, burn_values=[1], options=["ALL_TOUCHED=TRUE"])
    print("afstand berekenen", flush=True)
    afst = drv.Create(str(uit), kol, rij, 1, gdal.GDT_Float32, options=["COMPRESS=DEFLATE", "TILED=YES"])
    afst.SetGeoTransform(masker.GetGeoTransform())
    afst.SetProjection(masker.GetProjection())
    gdal.ComputeProximity(masker.GetRasterBand(1), afst.GetRasterBand(1),
                          options=["VALUES=1", "DISTUNITS=GEO", f"MAXDIST={MAXDIST_M}", f"NODATA={MAXDIST_M}"])
    afst.FlushCache()
    afst = None
    masker = None
    return uit


def main(argv):
    if not argv:
        print(__doc__)
        return 2
    bron = argv[0]
    laagnaam = argv[1] if len(argv) > 1 and not argv[1].startswith("--") else None
    cel = float(argv[argv.index("--cel") + 1]) if "--cel" in argv else 20.0
    raster = afstandsraster(bron, laagnaam, cel)
    ds = gdal.Open(str(raster))
    band = ds.GetRasterBand(1)
    gt = ds.GetGeoTransform()
    arr = band.ReadAsArray()
    print(f"afstandsraster {raster.name}: {arr.shape}, habitatcellen {(arr == 0).sum()}", flush=True)
    conn = sqlite3.connect(DB_PAD, timeout=120)
    zorg_kolom(conn, "zaak", "dist2n2000_m", "REAL")
    zorg_kolom(conn, "zaak", "n2000_klasse", "TEXT")
    rijen = conn.execute("SELECT zaak_id, x, y FROM zaak WHERE x IS NOT NULL").fetchall()
    print(f"{len(rijen)} zaken met locatie", flush=True)
    ids = np.array([r[0] for r in rijen])
    xs = np.array([r[1] for r in rijen], dtype=float)
    ys = np.array([r[2] for r in rijen], dtype=float)
    kol = ((xs - gt[0]) / gt[1]).astype(int)
    rij = ((ys - gt[3]) / gt[5]).astype(int)
    binnen = (kol >= 0) & (kol < arr.shape[1]) & (rij >= 0) & (rij < arr.shape[0])
    d = np.full(len(ids), MAXDIST_M, dtype=float)
    d[binnen] = arr[rij[binnen], kol[binnen]]
    d = np.minimum(d, MAXDIST_M)
    conn.executemany("UPDATE zaak SET dist2n2000_m = ?, n2000_klasse = ? WHERE zaak_id = ?",
                     [(round(float(v), 1), klasse(float(v)), int(z)) for v, z in zip(d, ids)])
    conn.commit()
    print("buiten het raster:", int((~binnen).sum()))
    print("klassen:", conn.execute("SELECT n2000_klasse, COUNT(*) FROM zaak WHERE n2000_klasse IS NOT NULL GROUP BY 1 ORDER BY 1").fetchall())
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
