"""Exporteert de BAG-pandhistorie als analysebestand: per pand met een vergunningdatum sinds 2005 de eerste datum
per fase (vergunning, bouwstart, in gebruik, niet gerealiseerd, sloop), het aantal verblijfsobjecten met
woonfunctie, de gemeente in de huidige indeling en de afstand tot AERIUS-habitats en overbelaste hexagonen.

Bron is de pandexport van de Stikstof-configuratie (cfg/main/Output.dms, Output/Vergunningenspoor/Export): een parquet
op dezelfde BAG-levering als de objectexports, uit de store van BAG-Tools die ook de RuimteScanner leest (#31). Het pad
komt uit de omgevingsvariabele VERGUNNINGEN_BAG of uit het eerste argument. De BAG registreert alleen nieuwe panden (status Bouwvergunning verleend, Bouw gestart, Pand in
gebruik), dus dit is de nieuwbouwkant van de vergunningen, landelijk vanaf de start van de BAG (2010) en daarmee ook
voor de gemeenten die pas in 2021 bekendmakingen gingen publiceren. De afstandsrasters komen uit afstand.py en
blootstelling.py en liggen op hetzelfde 25 meter-grid; de gemeente komt uit een rasterisatie van de bestuurlijke
gebieden (PDOK) op dat grid, eenmalig gemaakt en bewaard naast de grenzen.

Draaien met de QGIS-python: "C:\\Program Files\\QGIS 3.38.1\\bin\\python-qgis.bat" bag_export.py [pand_fasen.parquet]
Uitvoer: analyse\\bag_panden.csv (een rij per pand).
"""
import csv
import os
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr, osr

from afstand import klasse
from config import DATA_DIR

BAG_FASEN = os.environ.get("VERGUNNINGEN_BAG")  # Pand_Fasen_<BAG-datum>.parquet uit de Stikstof-configuratie
GRENZEN = DATA_DIR / "bron" / "gebouwen" / "grenzen" / "bestuurlijke_gebieden.gpkg"
GEMEENTE_RASTER = DATA_DIR / "bron" / "gebouwen" / "grenzen" / "gemeente_25m.tif"
AFSTAND_HABITAT = DATA_DIR / "bron" / "aerius_relevant_habitats_afstand_25m.tif"
AFSTAND_OVERBELAST = DATA_DIR / "bron" / "natuur" / "afstand_overbelast_25m.tif"
OVERBELASTING = DATA_DIR / "bron" / "natuur" / "overbelasting_250m.tif"
UIT = DATA_DIR / "analyse" / "bag_panden.csv"
VANAF = "2005-01-01"  # de BAG begint in 2009 tot 2011; oudere vergunningdatums zijn conversieartefacten
GEEN_DATUM = 20500101  # de BAG-lezer zet een voorkomen zonder begindatum op deze dag

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


def iso(v):
    """Een datum als JJJJMMDD-getal naar JJJJ-MM-DD; leeg, nul of de dag zonder begindatum wordt leeg."""
    if v is None:
        return None
    v = int(v)
    if v <= 0 or v >= GEEN_DATUM:
        return None
    return f"{v // 10000:04d}-{v // 100 % 100:02d}-{v % 100:02d}"


def lees_fasen(bron: Path):
    """De pandexport als rijen in de volgorde en kolommen van de oude tabel pand_fasen, plus de verblijfsobjecten."""
    ds = ogr.Open(str(bron))
    if ds is None:
        raise SystemExit(f"kan {bron} niet openen")
    laag = ds.GetLayer(0)
    rijen, vbo = [], {}
    for f in laag:
        verg = iso(f.GetField("verg_begin"))
        if verg is None or not (VANAF <= verg < "2027-01-01"):
            continue
        pid = f"{int(f.GetField('identificatie')):016d}"
        bouwjaar = f.GetField("bouwjaar")
        rijen.append((pid, verg, None, iso(f.GetField("start_begin")), iso(f.GetField("gebruik_begin")),
                      iso(f.GetField("niet_gerealiseerd_begin")), iso(f.GetField("sloopverg_begin")),
                      iso(f.GetField("gesloopt_begin")), iso(f.GetField("verbouwing_begin")), f.GetField("huidige_status"),
                      bouwjaar if bouwjaar else None, f.GetField("x"), f.GetField("y"), f.GetField("n_voorkomens")))
        vbo[pid] = (f.GetField("n_vbo") or 0, f.GetField("n_woon") or 0, f.GetField("opp_woon") or 0)
    ds = None
    rijen.sort(key=lambda r: (r[1], r[0]))
    return rijen, vbo


def main(argv):
    bron = Path(argv[0]) if argv else (Path(BAG_FASEN) if BAG_FASEN else None)
    if bron is None or not bron.exists():
        raise SystemExit("geef de pandexport (Pand_Fasen_<datum>.parquet) als argument of in VERGUNNINGEN_BAG")
    rijen, vbo = lees_fasen(bron)
    print(f"{len(rijen)} panden met een vergunningdatum vanaf {VANAF} uit {bron.name}")
    print(f"verblijfsobjecten gekoppeld voor {sum(1 for v in vbo.values() if v[0])} panden")
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
