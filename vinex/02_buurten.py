"""Koppelt elk Vinex-woonobject aan zijn CBS-gemeente, -wijk en -buurt (wijk- en buurtkaart 2024).

Per Vinex-gebied haalt het script de buurten en wijken in de omhullende rechthoek op bij de
OGC API van PDOK, en zoekt dan per object de buurt waar het in valt. Alleen lezen bij PDOK.

Invoer : C:\\LocalData\\Stikstof\\Vinex\\vinex_woonobjecten_20260710.csv (GeoDMS-export)
Uitvoer: C:\\LocalData\\Stikstof\\Vinex\\vinex_woonobjecten_buurt.csv (komma, punt als decimaal)
         C:\\LocalData\\Stikstof\\Vinex\\cbs_buurten_vinex.json (de opgehaalde buurten, voor herhaling)
"""
import csv
import json
import os
import time
import urllib.parse
import urllib.request
from collections import defaultdict

import numpy as np
from shapely import STRtree, points
from shapely.geometry import shape

MAP = r"C:\LocalData\Stikstof\Vinex"
INVOER = os.path.join(MAP, "vinex_woonobjecten_20260710.csv")
UIT = os.path.join(MAP, "vinex_woonobjecten_buurt.csv")
CACHE = os.path.join(MAP, "cbs_buurten_vinex.json")

API = "https://api.pdok.nl/cbs/wijken-en-buurten-2024/ogc/v1/collections/{}/items"
RD = "http://www.opengis.net/def/crs/EPSG/0/28992"
MARGE_M = 200


def haal(collectie, bbox):
    """Alle features van een collectie binnen een bbox in RD, met paginering."""
    uit, url = [], API.format(collectie) + "?" + urllib.parse.urlencode(
        {"bbox": ",".join(str(int(v)) for v in bbox), "bbox-crs": RD, "crs": RD, "f": "json", "limit": 1000})
    while url:
        with urllib.request.urlopen(url, timeout=120) as r:
            d = json.load(r)
        uit += d.get("features", [])
        url = next((l["href"] for l in d.get("links", []) if l.get("rel") == "next"), None)
    return uit


# objecten inlezen
obj = []
with open(INVOER, encoding="utf-8") as f:
    for r in csv.DictReader(f):
        obj.append(r)
print(f"{len(obj):,} objecten")

per_gebied = defaultdict(list)
for i, r in enumerate(obj):
    per_gebied[int(r["vinex_gebied"])].append(i)

# buurten en wijken per gebied ophalen (of uit de cache)
if os.path.exists(CACHE):
    with open(CACHE, encoding="utf-8") as f:
        cache = json.load(f)
else:
    cache = {}
buurt_geo, buurt_attr, wijknaam = {}, {}, {}
for g, idx in sorted(per_gebied.items()):
    sleutel = str(g)
    if sleutel not in cache:
        xs = np.array([float(obj[i]["x"]) for i in idx])
        ys = np.array([float(obj[i]["y"]) for i in idx])
        bbox = (xs.min() - MARGE_M, ys.min() - MARGE_M, xs.max() + MARGE_M, ys.max() + MARGE_M)
        cache[sleutel] = {"buurten": haal("buurten", bbox), "wijken": haal("wijken", bbox)}
        time.sleep(0.2)
    for ft in cache[sleutel]["buurten"]:
        p = ft["properties"]
        code = p["buurtcode"]
        if code not in buurt_geo:
            buurt_geo[code] = shape(ft["geometry"])
            buurt_attr[code] = (p.get("gemeentenaam") or "", p.get("wijkcode") or "", p.get("buurtnaam") or "")
    for ft in cache[sleutel]["wijken"]:
        p = ft["properties"]
        wijknaam[p["wijkcode"]] = p.get("wijknaam") or ""
with open(CACHE, "w", encoding="utf-8") as f:
    json.dump(cache, f)
print(f"{len(buurt_geo):,} buurten en {len(wijknaam):,} wijken opgehaald")

# punt in buurt
codes = list(buurt_geo)
boom = STRtree([buurt_geo[c] for c in codes])
pts = points(np.array([[float(r["x"]), float(r["y"])] for r in obj]))
paren = boom.query(pts, predicate="within")          # (index punt, index buurt)
toewijzing = {}
for ip, ib in zip(*paren):
    toewijzing.setdefault(int(ip), codes[int(ib)])
print(f"{len(toewijzing):,} van {len(obj):,} objecten in een buurt")

velden = ["vbo_id", "pand_id", "vinex_gebied", "pand_bouwjaar", "vbo_oppervlakte", "vbo_status",
          "postcode", "gemeente", "wijk", "buurt", "buurtcode", "x", "y"]
with open(UIT, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(velden)
    for i, r in enumerate(obj):
        code = toewijzing.get(i, "")
        gem, wijkcode, buurt = buurt_attr.get(code, ("", "", ""))
        w.writerow([r["vbo_id"], r["pand_id"], r["vinex_gebied"], r["pand_bouwjaar"], r["vbo_oppervlakte"],
                    r["vbo_status"], r["postcode"], gem, wijknaam.get(wijkcode, ""), buurt, code, r["x"], r["y"]])
print(f"geschreven: {UIT}")
