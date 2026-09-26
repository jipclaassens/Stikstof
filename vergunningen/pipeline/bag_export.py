"""Exporteert de BAG-pandhistorie als analysebestand: per pand met een vergunningdatum sinds 2005 de eerste datum
per fase (vergunning, bouwstart, in gebruik, niet gerealiseerd, sloop), het aantal verblijfsobjecten met
woonfunctie, de gemeente in de huidige indeling en de afstand tot AERIUS-habitats en overbelaste hexagonen.

Bron is bron\\gebouwen\\bag_pand_historie.sqlite, afgeleid uit het LV BAG-extract (Kadaster, CC0) met de scripts in
bron\\gebouwen\\scripts. De BAG registreert alleen nieuwe panden (status Bouwvergunning verleend, Bouw gestart, Pand in
gebruik), dus dit is de nieuwbouwkant van de vergunningen, landelijk vanaf de start van de BAG (2010) en daarmee ook
voor de gemeenten die pas in 2021 bekendmakingen gingen publiceren. De afstandsrasters komen uit afstand.py en
blootstelling.py en liggen op hetzelfde 25 meter-grid; de gemeente komt uit een rasterisatie van de bestuurlijke
gebieden (PDOK) op dat grid, eenmalig gemaakt en bewaard naast de grenzen.

Draaien met de QGIS-python: "C:\\Program Files\\QGIS 3.38.1\\bin\\python-qgis.bat" bag_export.py
Uitvoer: analyse\\bag_panden.csv (een rij per pand).
"""
import csv
import sqlite3
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr, osr

from afstand import klasse
from config import DATA_DIR

BAG_DB = DATA_DIR / "bron" / "gebouwen" / "bag_pand_historie.sqlite"
GRENZEN = DATA_DIR / "bron" / "gebouwen" / "grenzen" / "bestuurlijke_gebieden.gpkg"
GEMEENTE_RASTER = DATA_DIR / "bron" / "gebouwen" / "grenzen" / "gemeente_25m.tif"
AFSTAND_HABITAT = DATA_DIR / "bron" / "aerius_relevant_habitats_afstand_25m.tif"
AFSTAND_OVERBELAST = DATA_DIR / "bron" / "natuur" / "afstand_overbelast_25m.tif"
OVERBELASTING = DATA_DIR / "bron" / "natuur" / "overbelasting_250m.tif"
UIT = DATA_DIR / "analyse" / "bag_panden.csv"
VANAF = "2005-01-01"  # de BAG begint in 2009 tot 2011; oudere vergunningdatums zijn conversieartefacten

# Het 25 meter-grid van afstand.py: 12.000 bij 13.200 cellen vanaf (0, 630000), heel Nederland.
X0, Y0, CEL, NX, NY = 0.0, 630000.0, 25.0, 12000, 13200


def gemeenteraster() -> Path:
    """Rasteriseert de gemeentecode (als geheel getal, 0 is buiten Nederland) op het 25 meter-grid; eenmalig."""
    if GEMEENTE_RASTER.exists():
        return GEMEENTE_RASTER
    ds = ogr.Open(str(GRENZEN))
    laag = ds.GetLayerByName("bestuurlijkegebieden:Gemeentegebied")
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(28992)
    mem = ogr.GetDriverByName("Memory").CreateDataSource("gemeenten")
    ml = mem.CreateLayer("gemeenten", srs, ogr.wkbMultiPolygon)
    ml.CreateField(ogr.FieldDefn("gm", ogr.OFTInteger))
    for f in laag:
        nf = ogr.Feature(ml.GetLayerDefn())
        nf.SetGeometry(f.GetGeometryRef())
        nf.SetField("gm", int(f.GetField("code")))
        ml.CreateFeature(nf)
    doel = gdal.GetDriverByName("GTiff").Create(str(GEMEENTE_RASTER), NX, NY, 1, gdal.GDT_Int32,
                                                options=["COMPRESS=DEFLATE", "TILED=YES"])
    doel.SetGeoTransform((X0, CEL, 0, Y0, 0, -CEL))
    doel.SetProjection(srs.ExportToWkt())
    doel.GetRasterBand(1).SetNoDataValue(0)
    doel.GetRasterBand(1).Fill(0)
    gdal.RasterizeLayer(doel, [1], ml, options=["ATTRIBUTE=gm"])
    doel.FlushCache()
    print(f"gemeenteraster geschreven: {GEMEENTE_RASTER}")
    return GEMEENTE_RASTER


def bemonster(pad: Path, x: np.ndarray, y: np.ndarray, buiten):
    """Leest het raster in het geheugen en geeft per punt de celwaarde; buiten het raster de waarde buiten."""
    ds = gdal.Open(str(pad))
    band = ds.GetRasterBand(1)
    arr = band.ReadAsArray()
    gt = ds.GetGeoTransform()
    kol = np.floor((x - gt[0]) / gt[1]).astype(np.int64)
    rij = np.floor((y - gt[3]) / gt[5]).astype(np.int64)
    ok = (kol >= 0) & (kol < arr.shape[1]) & (rij >= 0) & (rij < arr.shape[0])
    uit = np.full(x.shape, buiten, dtype=arr.dtype if np.issubdtype(arr.dtype, np.floating) else np.int64)
    uit[ok] = arr[rij[ok], kol[ok]]
    return uit


def main(argv):
    bron = Path(argv[0]) if argv else BAG_DB
    conn = sqlite3.connect(bron)
    conn.execute("CREATE TEMP TABLE sel AS SELECT identificatie FROM pand_fasen WHERE verg_begin >= ? AND verg_begin < '2027-01-01'", (VANAF,))
    conn.execute("CREATE INDEX temp.ix_sel ON sel(identificatie)")
    n = conn.execute("SELECT count(*) FROM sel").fetchone()[0]
    print(f"{n} panden met een vergunningdatum vanaf {VANAF}")
    # Verblijfsobjecten per pand: alle statussen tellen mee, want bij een vergund pand zijn ze nog 'gevormd'.
    vbo = {}
    for pid, n_vbo, n_woon, opp in conn.execute(
        "SELECT v.pand_identificatie, count(*), sum(v.gebruiksdoel LIKE '%woonfunctie%'), sum(CASE WHEN v.gebruiksdoel LIKE '%woonfunctie%' THEN v.oppervlakte END) "
        "FROM vbo_actueel v JOIN sel s ON s.identificatie = v.pand_identificatie GROUP BY v.pand_identificatie"
    ):
        vbo[pid] = (n_vbo, n_woon or 0, opp or 0)
    print(f"verblijfsobjecten gekoppeld voor {len(vbo)} panden")
    rijen = conn.execute(
        "SELECT f.identificatie, f.verg_begin, f.verg_documentdatum, f.start_begin, f.gebruik_begin, f.niet_gerealiseerd_begin, "
        "f.sloopverg_begin, f.gesloopt_begin, f.verbouwing_begin, f.huidige_status, f.bouwjaar, f.x, f.y, f.n_voorkomens "
        "FROM pand_fasen f JOIN sel s ON s.identificatie = f.identificatie ORDER BY f.verg_begin, f.identificatie"
    ).fetchall()
    conn.close()
    x = np.array([float(r[11]) if r[11] not in (None, "") else np.nan for r in rijen])
    y = np.array([float(r[12]) if r[12] not in (None, "") else np.nan for r in rijen])
    geldig = np.isfinite(x) & np.isfinite(y)
    x = np.where(geldig, x, -1e6)
    y = np.where(geldig, y, -1e6)
    print(f"{int((~geldig).sum())} panden zonder coordinaat")
    gm = bemonster(gemeenteraster(), x, y, 0)
    d_hab = bemonster(AFSTAND_HABITAT, x, y, np.nan)
    d_ovb = bemonster(AFSTAND_OVERBELAST, x, y, np.nan)
    ovb = bemonster(OVERBELASTING, x, y, np.nan)
    UIT.parent.mkdir(parents=True, exist_ok=True)
    kop = ["identificatie", "verg_begin", "verg_documentdatum", "start_begin", "gebruik_begin", "niet_gerealiseerd_begin",
           "sloopverg_begin", "gesloopt_begin", "verbouwing_begin", "huidige_status", "bouwjaar", "x", "y", "n_voorkomens",
           "n_vbo", "n_woon", "opp_woon", "gemeentecode", "dist2n2000_m", "n2000_klasse", "dist_overbelast_m", "ovb_250"]
    with open(UIT, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(kop)
        for i, r in enumerate(rijen):
            n_vbo, n_woon, opp = vbo.get(r[0], (0, 0, 0))
            dh = float(d_hab[i])
            w.writerow(list(r[:11]) + [r[11], r[12], r[13], n_vbo, n_woon, round(opp) if opp else 0,
                                       f"{int(gm[i]):04d}" if gm[i] > 0 else "",
                                       "" if not np.isfinite(dh) else round(dh),
                                       "" if not np.isfinite(dh) else klasse(dh),
                                       "" if not np.isfinite(d_ovb[i]) else round(float(d_ovb[i])),
                                       "" if not np.isfinite(ovb[i]) else round(float(ovb[i]), 1)])
    print(f"geschreven: {UIT} ({len(rijen)} rijen; {int((gm > 0).sum())} met gemeente, "
          f"{int(np.isfinite(d_hab).sum())} met afstand tot habitat)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
