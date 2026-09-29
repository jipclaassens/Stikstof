"""Andere beperkingen voor bouwen: in welke gebieden met een beperking van Rijk of provincie liggen de BAG-objecten, en hoeveel
van het land nabij en ver van stikstofgevoelig habitat valt eronder?

Gebruik (Python van QGIS: numpy, GDAL en shapely 2):
  python-qgis-ltr.bat R/37a_beperkingen.py <punten.csv> <uitvoer_punten.csv> <uitvoer_oppervlakte.csv> [datamap]

punten.csv  id;x;y in RD (EPSG:28992), vbo_xy.csv van 11_ruimte.R
datamap     de datamap van het vergunningenspoor (standaard VERGUNNINGEN_DATA); de lagen staan in bron\\restricties en
            bron\\rs_restricties (zie hun BRONNEN.md), het landmasker in bron\\waterbodem\\klimaateffectatlas

Groepen, een bit per groep:
  0 natuurnetwerk         het NNN volgens de provinciale lagen, met de Natura 2000-gebieden erin
  1 stilte en grondwater  stiltegebieden en grondwaterbeschermingsgebieden (met de waterwingebieden) van de provincies
  2 landschap             nationale parken, provinciaal landschapsbeleid (onder meer de Nationale Landschappen), aardkundige
                          waarden en de weidevogelgebieden van Gelderland, Zuid-Holland, Limburg, Groningen en Fryslân
  3 water en kust         rijksregels uit de Omgevingsregeling: rivierbed van de grote rivieren, reserveringen voor Maas en
                          Rijntakken, beperkingengebied en keringen van rijkswaterkeringen, vrijwaringsgebied, kustfundament,
                          waddengebied en IJsselmeergebied
  4 externe veiligheid    de contour van het plaatsgebonden risico 10-6 van inrichtingen, buisleidingen en het Basisnet (REV,
                          alleen vlakken)
  5 defensie              militaire terreinen, onveilige gebieden bij schietbanen, militaire en civiele explosieaandachts-
                          gebieden A, B en C en civiele opslagplaatsen (Omgevingsregeling, stand maart 2025)
  6 geluid en luchtvaart  Schiphol: de vijf zones van het Luchthavenindelingbesluit (2018) en de LIB 5-zone (2024); regionale
                          luchthavens vanaf 48 dB(A) Lden of 35 Ke; de geluidzones van kleine luchthavens (provincies)
  7 infrastructuur        beperkingengebieden van hoofdspoorwegen en rijkswegen, reserveringen voor autowegen, locaties voor
                          hoogspanning van 220 kV en meer, grootschalige elektriciteitsopwekking en kernenergie, en de zone
                          rond alle bovengrondse hoogspanningslijnen (de rekenafstand van de netkaart 2024 als buffer)
  8 erfgoed               beschermde stads- en dorpsgezichten en werelderfgoed

Een punt valt in een groep als het in of op de rand van een vlak ligt (exact, met shapely). Voor de oppervlakte valt een cel
in een groep als haar middelpunt in een vlak ligt (ALL_TOUCHED=FALSE), op het grid van 25 meter van dit project; voor de
punten zou die regel te grof zijn, want nieuwbouw ligt vaak tegen de rand van het NNN aan (van de punten die zo in het NNN
vallen, ligt ongeveer een derde tot 18 meter erbuiten). Het logboek geeft per groep beide tellingen.
Uitvoer punten: id;beperking, de bits als geheel getal (-1 buiten het grid).
Uitvoer oppervlakte: beperking;kant;contour;cellen;ha voor het land binnen Nederland (de Basiskaart Natuurlijk Systeem, land
met kleine binnenwateren, zonder de grote wateren), met kant habitat (afstand 0), nabij (tot en met 2.500 meter) of ver, en
contour 1 binnen de verstedelijkingscontour (in Gelderland de bebouwde kom), 0 erbuiten.
"""
import csv
import os
import sys
import time
from pathlib import Path

import numpy as np
import shapely
from osgeo import gdal, ogr

gdal.UseExceptions()
ogr.UseExceptions()
punten, uit_p, uit_o = Path(sys.argv[1]), Path(sys.argv[2]), Path(sys.argv[3])
data = Path(sys.argv[4]) if len(sys.argv) > 4 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
R = data / "bron" / "restricties"
RS = data / "bron" / "rs_restricties" / "afgeleid"
t0 = time.time()


def log(tekst):
    print(f"[{time.time() - t0:6.0f} s] {tekst}", flush=True)


def ev(pad, laag=None, filter=None, buffer=None):
    return {"pad": pad, "laag": laag, "filter": filter, "buffer": buffer}


def namen(*n):
    return "werkingsgebied IN (" + ", ".join("'" + x + "'" for x in n) + ")"


OR = RS / "amvb_werkingsgebieden_2022_2025.gpkg"
ORL = "omgevingsregeling_werkingsgebieden_2025"
RWS = R / "rijk_omgevingsregeling_rws.gpkg"
WEIDE = R / "prov_weidevogelgebieden.gpkg"
GROEPEN = [
    ("natuurnetwerk", [ev(R / "prov_natuurnetwerk_nederland.gpkg")]),
    ("stilte en grondwater", [ev(R / "prov_stiltegebieden.gpkg"), ev(R / "prov_grondwaterbeschermingsgebieden.gpkg")]),
    ("landschap", [ev(R / "nationale_parken.gpkg"), ev(R / "prov_provinciaal_landschapsbeleid.gpkg"), ev(R / "prov_aardkundige_waarden.gpkg"),
                   ev(WEIDE, "gelderland_weidevogelgebieden"), ev(WEIDE, "zuidholland_belangrijk_kansrijk_weidevogelgebied"),
                   ev(WEIDE, "limburg_weidevogels_grasland_2023"), ev(WEIDE, "groningen_leefgebieden_weidevogels"),
                   ev(WEIDE, "fryslan_weidevogelkansgebied")]),
    ("water en kust", [ev(RWS, x) for x in ("grote_rivieren_rivierbed", "reservering_maas", "reservering_rijntakken",
                                           "waterkeringen_beperkingengebied_rijk", "keringen_primaire_niet_primaire_rijk",
                                           "vrijwaringsgebieden_water_rijk", "kustfundament_rijk", "waddengebied", "ijsselmeergebied")]),
    ("externe veiligheid", [ev(R / "rev_ev_activiteiten.gpkg", "ev_pr10_6"), ev(R / "rev_buisleidingen.gpkg", "bl_pr10_6"),
                            ev(R / "rev_basisnet.gpkg", "bn_pr10_6")]),
    ("defensie", [ev(OR, ORL, namen("Militaire terreinen en terreinen met een militair object", "Onveilige gebieden bij militaire schietbanen",
                                    "Militaire explosieaandachtsgebieden A", "Militaire explosieaandachtsgebieden B",
                                    "Militaire explosieaandachtsgebieden C", "Civiele explosieaandachtsgebieden A",
                                    "Civiele explosieaandachtsgebieden B", "Civiele explosieaandachtsgebieden C", "Civiele opslagplaatsen"))]),
    ("geluid en luchtvaart", [ev(R / "luchtvaart_schiphol_zuidholland.gpkg", "luchth_schiphol_bebouw_beperk_2018"),
                              ev(RS / "regionale_luchthavens_2012_2024.gpkg", None,
                                 "(maat = 'Lden' AND waarde >= 48) OR (maat = 'Ke' AND waarde >= 35) OR luchthaven LIKE 'Schiphol%'"),
                              ev(R / "prov_geluidzones_vliegvelden.gpkg")]),
    ("infrastructuur", [ev(OR, ORL, namen("Beperkingengebieden hoofdspoorwegen",
                                          "Locaties voor een hoogspanningsverbinding met een spanning van ten minste 220 kV",
                                          "Reserveringsgebieden voor de aanleg van een autoweg of autosnelweg",
                                          "Reserveringsgebieden voor de uitbreiding van een autoweg of autosnelweg",
                                          "Locaties voor grootschalige elektriciteitsopwekking", "Locaties voor een kernenergiecentrale")),
                        ev(RWS, "wegen_beperkingengebied_rijk"),
                        ev(RS / "hoogspanning_2023_2024.gpkg", "netkaart_2024_v5_20240311", None, "rekenafstand_m")]),
    ("erfgoed", [ev(R / "rce_stads_dorpsgezichten.gpkg", "stads_dorpsgezichten"), ev(R / "rce_werelderfgoed.gpkg")]),
]
assert len(GROEPEN) <= 16

ref = gdal.Open(str(data / "bron" / "afgeleid" / "stedelijk_gebied_nl_25m.tif"))
gt = ref.GetGeoTransform()
NX, NY = ref.RasterXSize, ref.RasterYSize


def naar_mem(pad, laag, filter, buffer):
    """Kopie van de laag met rechte, geldige vlakken; lijnen alleen met een buffer uit het veld buffer."""
    ds = ogr.Open(str(pad))
    lyr = ds.GetLayerByName(laag) if laag else ds.GetLayer(0)
    if filter:
        lyr.SetAttributeFilter(filter)
    mem = ogr.GetDriverByName("Memory").CreateDataSource("")
    ml = mem.CreateLayer("x", lyr.GetSpatialRef(), ogr.wkbUnknown)
    n = overgeslagen = 0
    for f in lyr:
        g = f.GetGeometryRef()
        if g is None:
            continue
        g = g.GetLinearGeometry()
        g.FlattenTo2D()
        if buffer:
            r = f.GetField(buffer)
            if not r:
                overgeslagen += 1
                continue
            g = g.Buffer(float(r), 8)
        elif ogr.GT_Flatten(g.GetGeometryType()) not in (ogr.wkbPolygon, ogr.wkbMultiPolygon):
            overgeslagen += 1
            continue
        if not g.IsValid():
            g = g.MakeValid()
        nf = ogr.Feature(ml.GetLayerDefn())
        nf.SetGeometry(g)
        ml.CreateFeature(nf)
        wkb.append(bytes(g.ExportToWkb()))
        n += 1
    return ds, mem, ml, n, overgeslagen


def brand(lagen):
    """Het raster van de groep (celmidden in een vlak) en de vlakken zelf, voor de exacte toets van de punten."""
    ds = gdal.GetDriverByName("MEM").Create("", NX, NY, 1, gdal.GDT_Byte)
    ds.SetGeoTransform(gt)
    ds.SetProjection(ref.GetProjection())
    wkb.clear()
    for spec in lagen:
        bron, mem, ml, n, over = naar_mem(**spec)
        gdal.RasterizeLayer(ds, [1], ml, burn_values=[1], options=["ALL_TOUCHED=FALSE"])
        log(f"  {Path(spec['pad']).name} {spec['laag'] or ''}: {n} objecten gebrand, {over} overgeslagen (geen vlak of geen buffer)")
        bron = mem = ml = None
    a = ds.GetRasterBand(1).ReadAsArray()
    ds = None
    return a, shapely.from_wkb(wkb)


wkb = []
ids, xs, ys = [], [], []
with open(punten, encoding="utf-8") as f:
    for r in csv.DictReader(f, delimiter=";"):
        ids.append(r["id"])
        xs.append(float(r["x"]))
        ys.append(float(r["y"]))
xs, ys = np.array(xs), np.array(ys)
kol, rij = ((xs - gt[0]) // gt[1]).astype(np.int64), ((ys - gt[3]) // gt[5]).astype(np.int64)
op_grid = (rij >= 0) & (rij < NY) & (kol >= 0) & (kol < NX)
boom = shapely.STRtree(shapely.points(xs, ys))
pbits = np.zeros(len(ids), dtype=np.uint16)
cbits = np.zeros(len(ids), dtype=np.uint16)
bits = np.zeros((NY, NX), dtype=np.uint16)
for g, (naam, lagen) in enumerate(GROEPEN):
    log(f"groep {g}: {naam}")
    a, vlakken = brand(lagen)
    bits |= (a > 0).astype(np.uint16) << g
    # exact: een punt op of in een vlak; de vlakken als invoer, zodat elk vlak een keer wordt voorbereid
    paren = boom.query(vlakken, predicate="intersects")
    raak = np.zeros(len(ids), dtype=bool)
    raak[paren[1]] = True
    pbits |= raak.astype(np.uint16) << g
    cel = np.zeros(len(ids), dtype=bool)
    cel[op_grid] = a[rij[op_grid], kol[op_grid]] > 0
    cbits |= cel.astype(np.uint16) << g
    log(f"  {int((a > 0).sum()) * 0.0625:,.0f} ha; punten exact {int(raak.sum()):,}, via de cel {int(cel.sum()):,}, "
        f"alleen exact {int((raak & ~cel).sum()):,}, alleen via de cel {int((cel & ~raak).sum()):,}")
    del a, vlakken
land, _ = brand([ev(data / "bron" / "waterbodem" / "klimaateffectatlas" / "kea_basiskaarten.gpkg", "bknsn_hoofdklasse")])
land = land > 0
log(f"landmasker: {int(land.sum()) * 0.0625:,.0f} ha")

# ---- punten: exact, en -1 buiten het grid
with open(uit_p, "w", newline="", encoding="utf-8") as g:
    w = csv.writer(g, delimiter=";")
    w.writerow(["id", "beperking"])
    for i, v, ok in zip(ids, pbits.tolist(), op_grid.tolist()):
        w.writerow([i, v if ok else -1])
log(f"punten: {len(ids):,}, waarvan {int((pbits > 0).sum()):,} in minstens een groep (via de cel {int((cbits > 0).sum()):,})")

# ---- oppervlakte van het land per combinatie van groepen, kant en contour, in blokken van rijen
afst_ds = gdal.Open(str(data / "bron" / "aerius_relevant_habitats_afstand_25m.tif"))   # dataset vasthouden, anders vervalt de band
afst = afst_ds.GetRasterBand(1)
cont = ref.GetRasterBand(1)
tel = np.zeros(65536 * 6, dtype=np.int64)
BLOK = 1100
for r0 in range(0, NY, BLOK):
    h = min(BLOK, NY - r0)
    d = afst.ReadAsArray(0, r0, NX, h)
    c = cont.ReadAsArray(0, r0, NX, h)
    ok = land[r0:r0 + h] & (c != 255)
    kant = np.where(d <= 0, 0, np.where(d <= 2500, 1, 2))
    sleutel = bits[r0:r0 + h].astype(np.int64) * 6 + kant * 2 + (c == 1)
    tel += np.bincount(sleutel[ok], minlength=tel.size)
with open(uit_o, "w", newline="", encoding="utf-8") as g:
    w = csv.writer(g, delimiter=";")
    w.writerow(["beperking", "kant", "contour", "cellen", "ha"])
    for k in np.flatnonzero(tel):
        w.writerow([k // 6, ("habitat", "nabij", "ver")[(k % 6) // 2], k % 2, int(tel[k]), round(int(tel[k]) * 0.0625, 4)])
log(f"land binnen Nederland: {int(tel.sum()) * 0.0625:,.0f} ha; geschreven: {uit_p} en {uit_o}")
