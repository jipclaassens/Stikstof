"""Exporteer de database naar een GeoPackage, voor QGIS en voor de ruimtelijke koppelingen.

Draait onder de Python van QGIS, want die heeft GDAL:
  "C:\\Program Files\\QGIS 3.38.1\\bin\\python-qgis.bat" export_gpkg.py [pad-naar-sqlite] [pad-naar-gpkg]

Vier lagen in EPSG:28992. Drie lagen met een rij per gebiedsmarkering, gescheiden naar geometrietype (punt, vlak,
lijn) omdat een GeoPackage-laag een geometrietype heeft. En een laag hoofdlocatie met precies een punt per publicatie:
de eerste Punt- of Adres-markering, anders het midden van de omhullende rechthoek van de eerste markering; daarmee
telt een publicatie in een kaart of een afstandsberekening een keer, met de precisieklasse ernaast.
"""
import sqlite3
import sys
from pathlib import Path

from osgeo import ogr, osr

ogr.UseExceptions()
osr.UseExceptions()
sys.path.insert(0, str(Path(__file__).parent))
from config import DATA_DIR, DB_PAD  # noqa: E402

VELDEN = [
    ("identifier", ogr.OFTString), ("volgnr", ogr.OFTInteger), ("locatietype", ogr.OFTString),
    ("creator", ogr.OFTString), ("organisatietype", ogr.OFTString), ("publicatienaam", ogr.OFTString),
    ("jaargang", ogr.OFTInteger), ("available", ogr.OFTString), ("fase_titel", ogr.OFTString),
    ("activiteiten", ogr.OFTString), ("titel", ogr.OFTString), ("label", ogr.OFTString),
    ("ligt_in_gemeente", ogr.OFTString), ("uitgestrektheid_m", ogr.OFTReal), ("precisieklasse", ogr.OFTString),
    ("url_html", ogr.OFTString),
]

QUERY = """
SELECT l.identifier, l.volgnr, l.locatietype, p.creator, p.organisatietype, p.publicatienaam, p.jaargang, p.available,
       p.fase_titel, (SELECT group_concat(activiteit, ';') FROM publicatie_activiteit a WHERE a.identifier = p.identifier),
       p.titel, l.label, l.ligt_in_gemeente, l.uitgestrektheid_m, l.wkt_rd, l.geomtype, l.x, l.y, p.url_html
FROM publicatie_locatie l JOIN publicatie p ON p.identifier = l.identifier
ORDER BY l.identifier, l.volgnr
"""


def precisieklasse(locatietype, uitgestrektheid):
    """Grof oordeel over hoe precies de markering de plek van het werk aanwijst; de meting per gemeente komt later."""
    if locatietype in ("Adres", "Punt", "Perceel"):
        return "adres"
    if uitgestrektheid is None:
        return "onbekend"
    if uitgestrektheid <= 100:
        return "perceel"
    if uitgestrektheid <= 1000:
        return "buurt"
    return "gebied"


def maak_laag(ds, naam, geomtype, srs):
    laag = ds.CreateLayer(naam, srs, geomtype, options=["SPATIAL_INDEX=YES"])
    for veld, t in VELDEN:
        d = ogr.FieldDefn(veld, t)
        if t == ogr.OFTString:
            d.SetWidth(0)
        laag.CreateField(d)
    return laag


def vul(feature, rij, laagdef):
    (identifier, volgnr, locatietype, creator, orgtype, blad, jaargang, available, fase, activiteiten, titel, label,
     gemeente, uitg, wkt, geomtype, x, y, url) = rij
    waarden = dict(identifier=identifier, volgnr=volgnr, locatietype=locatietype, creator=creator,
                   organisatietype=orgtype, publicatienaam=blad, jaargang=jaargang, available=available,
                   fase_titel=fase, activiteiten=activiteiten, titel=titel, label=label, ligt_in_gemeente=gemeente,
                   uitgestrektheid_m=uitg, precisieklasse=precisieklasse(locatietype, uitg), url_html=url)
    for veld, _ in VELDEN:
        w = waarden[veld]
        if w is not None:
            feature.SetField(veld, w)


def main(argv):
    db = Path(argv[0]) if argv else DB_PAD
    uit = Path(argv[1]) if len(argv) > 1 else DATA_DIR / "vergunningen.gpkg"
    if uit.exists():
        uit.unlink()
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(28992)
    ds = ogr.GetDriverByName("GPKG").CreateDataSource(str(uit))
    lagen = {
        "punt": maak_laag(ds, "markering_punt", ogr.wkbPoint, srs),
        "vlak": maak_laag(ds, "markering_vlak", ogr.wkbMultiPolygon, srs),
        "lijn": maak_laag(ds, "markering_lijn", ogr.wkbMultiLineString, srs),
        "hoofd": maak_laag(ds, "publicatie_hoofdlocatie", ogr.wkbPoint, srs),
    }
    conn = sqlite3.connect(f"file:{db}?mode=ro", uri=True, timeout=120)
    telling = {k: 0 for k in lagen}
    overgeslagen = 0
    vorige_id, hoofd_gezet = None, False
    ds.StartTransaction()  # een transactie per datasource; per laag geeft GPKG "Transaction already established"
    for rij in conn.execute(QUERY):
        identifier, volgnr, locatietype = rij[0], rij[1], rij[2]
        wkt, geomtype, x, y = rij[14], rij[15], rij[16], rij[17]
        if identifier != vorige_id:
            vorige_id, hoofd_gezet = identifier, False
        geom = ogr.CreateGeometryFromWkt(wkt) if wkt else None
        if geom is None:
            overgeslagen += 1
            continue
        gt = ogr.GT_Flatten(geom.GetGeometryType())
        if gt == ogr.wkbPoint:
            sleutel = "punt"
        elif gt in (ogr.wkbPolygon, ogr.wkbMultiPolygon):
            sleutel = "vlak"
            geom = ogr.ForceToMultiPolygon(geom)
        elif gt in (ogr.wkbLineString, ogr.wkbMultiLineString):
            sleutel = "lijn"
            geom = ogr.ForceToMultiLineString(geom)
        else:
            overgeslagen += 1
            continue
        laag = lagen[sleutel]
        f = ogr.Feature(laag.GetLayerDefn())
        vul(f, rij, laag.GetLayerDefn())
        f.SetGeometry(geom)
        laag.CreateFeature(f)
        telling[sleutel] += 1
        # hoofdlocatie: de eerste Punt of Adres wint; anders het midden van de eerste markering, tot een punt volgt
        if not hoofd_gezet or (locatietype in ("Punt", "Adres") and hoofd_gezet == "midden"):
            if hoofd_gezet == "midden":
                lagen["hoofd"].DeleteFeature(hoofd_fid)
                telling["hoofd"] -= 1
            hf = ogr.Feature(lagen["hoofd"].GetLayerDefn())
            vul(hf, rij, lagen["hoofd"].GetLayerDefn())
            if sleutel == "punt":
                hf.SetGeometry(geom)
                hoofd_gezet = "punt" if locatietype in ("Punt", "Adres") else "midden"
            else:
                p = ogr.Geometry(ogr.wkbPoint)
                p.AddPoint_2D(x, y)
                hf.SetGeometry(p)
                hoofd_gezet = "midden"
            lagen["hoofd"].CreateFeature(hf)
            hoofd_fid = hf.GetFID()
            telling["hoofd"] += 1
    ds.CommitTransaction()
    ds = None
    print(f"{uit}: " + ", ".join(f"{k} {v}" for k, v in telling.items()) + f", overgeslagen {overgeslagen}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
