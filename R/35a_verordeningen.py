"""Stedelijk en landelijk gebied in de provinciale omgevingsverordeningen op vier peildatums, uit de zips van de DSO-LV.

Gebruik (Python van QGIS: numpy, GDAL en shapely 2):
  python-qgis-ltr.bat R/35a_verordeningen.py <lv_download-map> <habitat.gpkg> <uitvoermap>

lv_download-map  bron\\dso\\lv_download (dso_lv_downloaden.py): per provincie een zip op 1 januari 2024, 2025 en 2026 en actueel
                 (28 september 2026), met de OW-objecten en de GIO's; alleen gelezen
habitat.gpkg     bron\\aerius_relevant_habitats.gpkg (zoals 29a, 30a en 32a)

Welke gebiedsaanwijzingen: van het type ruimtelijk gebruik met een groep voor stedelijk gebied (stedelijk gebied, groen stedelijk,
ontwikkeling stedelijke functies, bebouwingscontour) of landelijk gebied (alle groepen die met landelijk gebied beginnen), van het
type functie met de groep ontwikkeling stedelijke functies (Fryslân), en van het type ruimtelijk gebruik met groep overig en een naam
voor bebouwd gebied of kernen (Overijssel). Elke provincie noemt haar contour anders; de naam en groep staan in de uitvoer. Een
gebiedsaanwijzing wijst via een gebied of gebiedengroep naar geometrieen in de GIO's.

Wijziging tussen twee opeenvolgende peildatums: wat er bij de aanwijzing (zelfde groep en naam) bij kwam en wat eraf ging, in delen
van minstens 0,1 ha (kleinere verschillen zijn meestal hertekening); een deel telt als nabij als het binnen 2.500 meter van het
habitat ligt (zoals een plancontour in de rest van het onderzoek).

Uitvoer in de uitvoermap:
  verordening_aanwijzingen.csv  provincie;peildatum;soort;groep;naam;n_gebieden;opp_ha;opp_nabij_ha (nabij alleen bij stedelijk)
  verordening_wijzigingen.csv   provincie;soort;groep;naam;van;tot;erbij_ha;eraf_ha;erbij_nabij_ha;eraf_nabij_ha;erbij_delen;eraf_delen
  verordening_wijzigingen.gpkg  laag wijziging: de delen die erbij kwamen of eraf gingen, met provincie, aanwijzing, periode, richting
                                en afstand tot het habitat
"""
import collections
import csv
import re
import sys
import time
import zipfile
from pathlib import Path

import numpy as np
import shapely
from osgeo import ogr, osr

ogr.UseExceptions()
map_, habitat, uit = (Path(a) for a in sys.argv[1:4])
t0 = time.time()
MIN_DEEL_M2 = 1000.0
PEILDATUMS = ["2024-01-01", "2025-01-01", "2026-01-01", "actueel_2026-09-28"]


def log(tekst):
    print(f"[{time.time() - t0:6.0f} s] {tekst}", flush=True)


def laatste(uri):
    return uri.rstrip("/").rsplit("/", 1)[-1] if uri else ""


def soort_van(typ, groep, naam):
    n = (naam or "").lower()
    if typ == "RuimtelijkGebruik":
        if groep.startswith("StedelijkGebied") or groep in ("OntwikkelingStedelijkeFuncties", "Bebouwingscontour"):
            return "stedelijk"
        if groep.startswith("LandelijkGebied"):
            return "landelijk"
        if groep == "Overig" and re.search(r"bebouwd gebied|^kern|kernrandzone", n):
            return "stedelijk"
    if typ == "Functie" and groep == "OntwikkelingStedelijkeFuncties":
        return "landelijk" if n.startswith("buiten") else "stedelijk"
    return None


RX_GA = re.compile(r"<ga:Gebiedsaanwijzing>(.*?)</ga:Gebiedsaanwijzing>", re.S)
RX_GEBIED = re.compile(r"<l:Gebied>.*?<l:identificatie>([^<]+)</l:identificatie>.*?<l:GeometrieRef xlink:href=\"([^\"]+)\"", re.S)
RX_GROEP = re.compile(r"<l:Gebiedengroep>(.*?)</l:Gebiedengroep>", re.S)
# het voorvoegsel van de basisgeometrie verschilt per provincie (basisgeo of owgeo, zelfde namespace)
RX_GEO = re.compile(r"<(\w+):Geometrie[ >][^>]*>?\s*<\1:id>([^<]+)</\1:id>\s*<\1:geometrie[^>]*>(.*?)</\1:geometrie>", re.S)


def lees_zip(pad):
    """Geeft per aanwijzing (soort, groep, naam) de vereniging van haar geometrieen en het aantal gebieden."""
    z = zipfile.ZipFile(pad)
    ga_xml = z.read("OW-bestanden/gebiedsaanwijzingen.xml").decode("utf-8")
    geb = dict(RX_GEBIED.findall(z.read("OW-bestanden/gebieden.xml").decode("utf-8"))) if "OW-bestanden/gebieden.xml" in z.namelist() else {}
    groepen = {}
    if "OW-bestanden/gebiedengroepen.xml" in z.namelist():
        for blok in RX_GROEP.findall(z.read("OW-bestanden/gebiedengroepen.xml").decode("utf-8")):
            gid = re.search(r"<l:identificatie>([^<]+)</l:identificatie>", blok).group(1)
            groepen[gid] = re.findall(r"<l:GebiedRef xlink:href=\"([^\"]+)\"", blok)
    aanw = collections.defaultdict(set)
    for blok in RX_GA.findall(ga_xml):
        typ = laatste(re.search(r"<ga:type>([^<]*)</ga:type>", blok).group(1))
        groep = laatste((re.search(r"<ga:groep>([^<]*)</ga:groep>", blok) or [None, ""])[1])
        naam = (re.search(r"<ga:naam>([^<]*)</ga:naam>", blok) or [None, ""])[1].strip()
        s = soort_van(typ, groep, naam)
        if not s:
            continue
        for ref in re.findall(r"<l:LocatieRef xlink:href=\"([^\"]+)\"", blok):
            leden = groepen.get(ref, [ref])
            for lid in leden:
                if lid in geb:
                    aanw[(s, groep, naam)].add(geb[lid])
    nodig = set().union(*aanw.values()) if aanw else set()
    geoms = {}
    for n in z.namelist():
        if not n.lower().endswith(".gml") or not nodig - set(geoms):
            continue
        tekst = z.read(n).decode("utf-8")
        if not any(i in tekst for i in list(nodig - set(geoms))[:50]) and not any(i in tekst for i in nodig - set(geoms)):
            continue
        for _, gid, gml in RX_GEO.findall(tekst):
            if gid in nodig and gid not in geoms:
                g = ogr.CreateGeometryFromGML(gml.strip())
                if g is not None:
                    geoms[gid] = shapely.from_wkb(bytes(g.ExportToIsoWkb()))
    uitk = {}
    for k, ids in aanw.items():
        gg = [geoms[i] for i in ids if i in geoms]
        if not gg:
            continue
        a = np.array(gg, dtype=object)
        fout = ~shapely.is_valid(a)
        if fout.any():
            a[fout] = shapely.make_valid(a[fout])
        u = shapely.union_all(a)
        uitk[k] = (u, len(ids), len(gg))
    return uitk


# ---- habitat
ds = ogr.Open(str(habitat))
lyr = ds.GetLayer(0)
wkbs = [bytes(f.GetGeometryRef().ExportToWkb()) for f in lyr if f.GetGeometryRef() is not None]
lyr = None
ds = None
hab = shapely.from_wkb(wkbs)
fout = ~shapely.is_valid(hab)
if fout.any():
    hab[fout] = shapely.make_valid(hab[fout])
delen = shapely.get_parts(hab)
delen = delen[shapely.get_type_id(delen) == 3]
boom = shapely.STRtree(delen)
log(f"habitat: {len(delen)} polygonen")

uit.mkdir(parents=True, exist_ok=True)
provincies = sorted({p.name.split("_")[1] for p in map_.glob("omgevingsverordening_pv*_*.zip")})
rij_a, rij_w, gpkg_rijen = [], [], []
for pv in provincies:
    per_datum = {}
    for peil in PEILDATUMS:
        pad = map_ / f"omgevingsverordening_{pv}_{peil}.zip"
        if not pad.exists():
            continue
        per_datum[peil] = lees_zip(pad)
        for (s, groep, naam), (u, n_ids, n_geo) in per_datum[peil].items():
            # het deel nabij: bij stedelijk gebied per los vlak (een kern), een vlak telt als nabij binnen 2.500 meter van het habitat;
            # bij landelijk gebied zegt dat niets (een vlak beslaat een hele regio) en blijft het leeg
            nabij = ""
            if s == "stedelijk":
                p = shapely.get_parts(u)
                p = p[shapely.get_type_id(p) == 3]
                if len(p):
                    _, d = boom.query_nearest(p, return_distance=True, all_matches=False)
                    nabij = round(float((shapely.area(p) * (d <= 2500)).sum()) / 1e4, 1)
            rij_a.append([pv, peil[:10] if peil[0].isdigit() else "2026-09-28", s, groep, naam, n_ids, round(float(shapely.area(u)) / 1e4, 1), nabij])
        log(f"{pv} {peil}: {len(per_datum[peil])} aanwijzingen")
    datums = [d for d in PEILDATUMS if d in per_datum]
    for van, tot in zip(datums, datums[1:]):
        for k in sorted(set(per_datum[van]) | set(per_datum[tot])):
            if k not in per_datum[van] or k not in per_datum[tot]:
                rij_w.append([pv, *k, van[:10] if van[0].isdigit() else "2026-09-28", tot[:10] if tot[0].isdigit() else "2026-09-28",
                              "", "", "", "", "aanwijzing nieuw" if k not in per_datum[van] else "aanwijzing vervallen", ""])
                continue
            a, b = per_datum[van][k][0], per_datum[tot][k][0]
            if shapely.equals_exact(a, b, tolerance=0.0) or shapely.equals(a, b):
                rij_w.append([pv, *k, van[:10] if van[0].isdigit() else "2026-09-28", tot[:10] if tot[0].isdigit() else "2026-09-28", 0, 0, 0, 0, 0, 0])
                continue
            res = {}
            for richting, g in (("erbij", shapely.difference(b, a)), ("eraf", shapely.difference(a, b))):
                p = shapely.get_parts(g)
                p = p[(shapely.get_type_id(p) == 3) & (shapely.area(p) >= MIN_DEEL_M2)] if len(p) else p
                if len(p):
                    _, d = boom.query_nearest(p, return_distance=True, all_matches=False)
                else:
                    d = np.array([])
                opp = shapely.area(p) / 1e4 if len(p) else np.array([])
                res[richting] = (float(opp.sum()) if len(opp) else 0.0, float(opp[d <= 2500].sum()) if len(opp) else 0.0, len(p))
                for i in range(len(p)):
                    gpkg_rijen.append((pv, k[0], k[1], k[2], van, tot, richting, float(opp[i]), float(d[i]), bytes(shapely.to_wkb(p[i]))))
            rij_w.append([pv, *k, van[:10] if van[0].isdigit() else "2026-09-28", tot[:10] if tot[0].isdigit() else "2026-09-28",
                          round(res["erbij"][0], 1), round(res["eraf"][0], 1), round(res["erbij"][1], 1), round(res["eraf"][1], 1),
                          res["erbij"][2], res["eraf"][2]])

with open(uit / "verordening_aanwijzingen.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["provincie", "peildatum", "soort", "groep", "naam", "n_gebieden", "opp_ha", "opp_nabij_ha"])
    w.writerows(rij_a)
with open(uit / "verordening_wijzigingen.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["provincie", "soort", "groep", "naam", "van", "tot", "erbij_ha", "eraf_ha", "erbij_nabij_ha", "eraf_nabij_ha", "erbij_delen", "eraf_delen"])
    w.writerows(rij_w)
drv = ogr.GetDriverByName("GPKG")
pad = uit / "verordening_wijzigingen.gpkg"
if pad.exists():
    drv.DeleteDataSource(str(pad))
out = drv.CreateDataSource(str(pad))
srs = osr.SpatialReference()
srs.ImportFromEPSG(28992)
lg = out.CreateLayer("wijziging", srs, ogr.wkbMultiPolygon)
for naam, typ in (("provincie", ogr.OFTString), ("soort", ogr.OFTString), ("groep", ogr.OFTString), ("naam", ogr.OFTString),
                  ("van", ogr.OFTString), ("tot", ogr.OFTString), ("richting", ogr.OFTString), ("opp_ha", ogr.OFTReal), ("afstand_m", ogr.OFTReal)):
    lg.CreateField(ogr.FieldDefn(naam, typ))
lg.StartTransaction()
for r in gpkg_rijen:
    ft = ogr.Feature(lg.GetLayerDefn())
    for i, v in enumerate(r[:9]):
        ft.SetField(i, v)
    ft.SetGeometry(ogr.ForceToMultiPolygon(ogr.CreateGeometryFromWkb(r[9])))
    lg.CreateFeature(ft)
lg.CommitTransaction()
out = None
log(f"geschreven: {len(rij_a)} aanwijzingen, {len(rij_w)} vergelijkingen, {len(gpkg_rijen)} gewijzigde delen")
