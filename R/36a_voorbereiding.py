"""Voorbereidingsbesluiten en exploitatieplannen van gemeenten uit de kennisgevingen, met hun afstand tot stikstofgevoelig habitat.

Gebruik (Python van QGIS: numpy, GDAL en shapely 2):
  python-qgis-ltr.bat R/36a_voorbereiding.py <plannen_sru.sqlite> <dso_overig.sqlite> <dso.sqlite> <habitat.gpkg> <uitvoer.csv>

plannen_sru.sqlite  de kennisgevingen van ruimtelijke plannen (SRU-stroom ruimtelijk, zie 31a_kennisgevingen.py), alleen gelezen
dso_overig.sqlite   de overige plantypen uit de API Ruimtelijke plannen v4 (dso_plannen.py --typen), met de contour van
                    voorbereidingsbesluiten en exploitatieplannen die nog in het register staan, alleen gelezen
dso.sqlite          de bestemmingsplannen uit dezelfde API (29a_plannen_dso.py), alleen gelezen: een kennisgeving van een
                    exploitatieplan noemt vaak het bestemmingsplan dat erbij hoort
habitat.gpkg        bron\\aerius_relevant_habitats.gpkg, de AERIUS relevante habitatkartering

Een gebeurtenis is een voorbereidingsbesluit of een exploitatieplan, herkend aan het woord in de titel van een kennisgeving
van een gemeente (een titel met beide woorden telt voor beide). Kennisgevingen van hetzelfde plandossier (het IMRO-nummer
zonder versiecode) vormen een gebeurtenis, met de datum van de eerste kennisgeving; zonder IMRO-nummer telt de gemeente met
de titel zonder de woorden kennisgeving, bekendmaking, ontwerp, vastgesteld en dergelijke. De plek komt eerst uit de contour
van het genoemde plan in het register (elke versie van het dossier), anders uit de precieze locaties van de kennisgevingen
(punt, adres, perceel, vlak, weg, lijn of postcode; niet gemeente, woonplaats, wijk, buurt of provincie).

Uitvoer: sleutel;soort;variant;datum;gemeente;n_kennisgevingen;plek;afstand_m;opp_ha;uitgestrekt_m;titel
  soort      voorbereidingsbesluit of exploitatieplan
  variant    per kennisgeving uit de titel: bij een exploitatieplan nieuw, herziening (ook actualisatie) of geen (een besluit om
             geen exploitatieplan vast te stellen, omdat het kostenverhaal anders verzekerd is); bij een voorbereidingsbesluit
             besluit of intrekking
  plek       plan (contour uit het register), kennisgeving (locaties van de kennisgeving) of leeg
  afstand_m  kleinste afstand tot het habitat (0 als ze elkaar raken), leeg zonder plek
  opp_ha     oppervlakte van de plek (0 bij punten), uitgestrekt_m de diagonaal van haar omhullende rechthoek: een besluit voor
             de hele gemeente of het hele buitengebied ligt altijd ergens dicht bij natuur, dus vergelijk nabij en ver op
             kleine plekken
"""
import collections
import csv
import json
import re
import sqlite3
import sys
from pathlib import Path

import numpy as np
import shapely
from osgeo import ogr

ogr.UseExceptions()
sru, overig, dso, habitat, uit = (Path(a) for a in sys.argv[1:6])
SOORTEN = {"voorbereidingsbesluit": re.compile(r"voorbereidingsbesluit", re.I),
           "exploitatieplan": re.compile(r"exploitatieplan", re.I)}
PRECIES = {"Punt", "Adres", "Perceel", "Vlak", "Weg", "Lijn", "Postcode"}
# variant per kennisgeving: een besluit om geen exploitatieplan vast te stellen (kostenverhaal anders verzekerd), een herziening
# of actualisatie van een bestaand exploitatieplan, een intrekking, of anders een nieuw plan; bij voorbereidingsbesluiten de
# intrekking apart
RX_GEEN = re.compile(r"\bgeen\b[^.;,]{0,40}exploitatieplan|exploitatieplan[^.;,]{0,40}\bniet\s+(vast\s+te\s+stellen|vaststellen|vastgesteld)"
                     r"|niet\s+(vast\s+te\s+stellen|vaststellen)\s+(van\s+)?(een\s+)?exploitatieplan|af\s*te\s*zien[^.;,]{0,40}exploitatieplan"
                     r"|afzien[^.;,]{0,40}exploitatieplan|exploitatieplan[^.;,]{0,40}achterwege", re.I)
RX_HERZ = re.compile(r"(herzien|herziening|actualis|geactualiseerd|wijziging)\w*(\s+\d{4})?\s+(van\s+)?(het\s+)?exploitatieplan"
                     r"|exploitatieplan[^.;]{0,80}(herzien|herziening|actualis|geactualiseerd)|\bgewijzigd\s+exploitatieplan", re.I)
RX_INTREK = re.compile(r"intrek", re.I)


def variant(soort, titel):
    if soort == "exploitatieplan":
        return ("geen" if RX_GEEN.search(titel) else "intrekking" if RX_INTREK.search(titel)
                else "herziening" if RX_HERZ.search(titel) else "nieuw")
    return "intrekking" if RX_INTREK.search(titel) else "besluit"


RX_RUIS = re.compile(r"\b(kennisgeving|bekendmaking|ontwerp|ontwerpbesluit|vastgesteld|vaststelling|terinzagelegging|ter inzage|"
                     r"gemeente|het|de|van|en|tot|inzake)\b|[^a-z0-9 ]", re.I)


def dossier_van(plan_id):
    return plan_id.rsplit("-", 1)[0] if "-" in plan_id else plan_id


def ro(p):
    return sqlite3.connect(f"file:{p.as_posix()}?mode=ro", uri=True)


# ---- contouren per dossier: eerst de overige plantypen, dan de bestemmingsplannen
contour = {}
for pad in (overig, dso):
    c = ro(pad)
    for pid, wkt in c.execute("SELECT p.identificatie, g.wkt FROM plan p JOIN plan_geometrie g ON g.identificatie = p.identificatie "
                              "WHERE g.wkt IS NOT NULL ORDER BY p.identificatie DESC"):
        contour.setdefault(dossier_van(pid), wkt)
    c.close()
print(f"contouren: {len(contour):,} dossiers")

# ---- kennisgevingen
c = ro(sru)
loc = collections.defaultdict(list)
for ident, lt, wkt in c.execute("SELECT identifier, locatietype, wkt_rd FROM publicatie_locatie WHERE wkt_rd IS NOT NULL"):
    if lt in PRECIES:
        loc[ident].append(wkt)
geb = {}
for ident, titel, creator, beschikbaar, extra in c.execute(
        "SELECT identifier, titel, creator, available, extra_json FROM publicatie WHERE organisatietype = 'gemeente'"):
    if not titel:
        continue
    e = json.loads(extra) if extra else {}
    ids = sorted({x.strip() for x in e.get("bekendmakingBetreffendePlan", []) if x and x.strip().startswith("NL.IMRO.")})
    for soort, rx in SOORTEN.items():
        if not rx.search(titel):
            continue
        if ids:
            sleutel = "|".join(sorted({dossier_van(p) for p in ids}))
        else:
            sleutel = f"{creator}|{' '.join(RX_RUIS.sub(' ', titel.lower()).split())}"
        g = geb.setdefault((soort, variant(soort, titel), sleutel),
                           {"datum": None, "gemeente": creator, "n": 0, "ids": set(), "loc": [], "titel": titel})
        g["n"] += 1
        g["ids"].update(dossier_van(p) for p in ids)
        g["loc"].extend(loc.get(ident, []))
        d = (beschikbaar or "")[:10]
        if d and (g["datum"] is None or d < g["datum"]):
            g["datum"], g["titel"] = d, titel
c.close()
print(f"gebeurtenissen: {collections.Counter(k[:2] for k in geb)}")

# ---- plek en afstand
ds = ogr.Open(str(habitat))
lyr = ds.GetLayer(0)
hab = shapely.from_wkb([bytes(f.GetGeometryRef().ExportToWkb()) for f in lyr if f.GetGeometryRef() is not None])
lyr = ds = None
fout = ~shapely.is_valid(hab)
if fout.any():
    hab[fout] = shapely.make_valid(hab[fout])
delen = shapely.get_parts(hab)
delen = delen[shapely.get_type_id(delen) == 3]
boom = shapely.STRtree(delen)
sleutels = sorted(geb)
geoms, plek = [], []
for k in sleutels:
    g = geb[k]
    wkts = [contour[d] for d in sorted(g["ids"]) if d in contour]
    if wkts:
        plek.append("plan")
    elif g["loc"]:
        wkts = g["loc"]
        plek.append("kennisgeving")
    else:
        plek.append("")
        geoms.append(None)
        continue
    geom = shapely.from_wkt(np.array(wkts, dtype=object))
    fout = ~shapely.is_valid(geom)
    if fout.any():
        geom[fout] = shapely.make_valid(geom[fout])
    geoms.append(shapely.union_all(geom))
heeft = np.array([x is not None for x in geoms])
afst = np.full(len(sleutels), np.nan)
if heeft.any():
    idx, afstand = boom.query_nearest(np.array([x for x in geoms if x is not None], dtype=object), return_distance=True, all_matches=False)
    afst[np.flatnonzero(heeft)[idx[0]]] = afstand
with open(uit, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["sleutel", "soort", "variant", "datum", "gemeente", "n_kennisgevingen", "plek", "afstand_m", "opp_ha", "uitgestrekt_m", "titel"])
    for i, k in enumerate(sleutels):
        g = geb[k]
        opp = uitg = ""
        if geoms[i] is not None:
            x0, y0, x1, y1 = shapely.bounds(geoms[i])
            opp, uitg = round(float(shapely.area(geoms[i])) / 1e4, 3), round(float(np.hypot(x1 - x0, y1 - y0)), 1)
        w.writerow([k[2], k[0], k[1], g["datum"], g["gemeente"], g["n"], plek[i], "" if np.isnan(afst[i]) else round(float(afst[i]), 1),
                    opp, uitg, g["titel"]])
print("plek:", collections.Counter((k[0], k[1], p) for k, p in zip(sleutels, plek)).most_common())
print(f"geschreven: {uit}")
