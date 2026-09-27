"""Kernen uit de verstedelijkingscontour, met inwoners en de hoofdkern per gemeente.

Gebruik (met de Python van QGIS, want GDAL en scipy zijn nodig):
  "C:\\Program Files\\QGIS 3.40.7\\bin\\python-qgis-ltr.bat" R/16a_kernen.py <punten.csv> <uitvoer.csv> [datamap]

punten.csv   id;x;y in RD (EPSG:28992), geschreven door 11_ruimte.R (_derived/vbo_xy.csv)
uitvoer.csv  id;kern;kern_inw;kern_gm: de kern waarin het punt ligt (0 = buiten de contour),
             de inwoners van die kern en de gemeente met de meeste inwoners in die kern
Daarnaast, in dezelfde map:
  kern_hoofd.csv   gemeentecode;kern;inw_eigen;aandeel_eigen: per gemeente de hoofdkern, de kern
                   met de meeste inwoners van die gemeente zelf (een kern kan zo de hoofdkern van
                   meer gemeenten zijn, zoals een samengegroeide stad en haar buurgemeenten)
  kernen_meta.json het aantal kernen, de inwoners in kernen en het aantal gemeenten
datamap      de datamap van het vergunningenspoor (standaard VERGUNNINGEN_DATA)

Werkwijze
- De contour is bron/afgeleid/stedelijk_gebied_nl_25m.tif (1 = binnen de contour van de
  provincie, stand september 2026; Gelderland: de bebouwde kom als vervanger).
- Een kern is een verbonden stuk contour. Stukken die minder dan 100 m uit elkaar liggen, tellen
  als een kern (de contour wordt voor het labelen 50 m opgerekt), zodat een weg of kanaal een
  stad niet in tweeen deelt.
- Inwoners: de buurten van de CBS wijk- en buurtkaart 2024 met hun aantal inwoners (bron/
  waterbodem/klimaateffectatlas/kea_buurtdashboard_20260312.gpkg, AANT_INW). Elke buurt verdeelt
  haar inwoners gelijk over haar cellen binnen de contour; een kern telt de inwoners van haar cellen.
- Per gemeente is de hoofdkern de kern met de meeste van haar eigen inwoners. Stukken contour die
  aan elkaar vastgroeien, zoals een stad en haar buurgemeenten, vormen een kern; die kern is dan de
  hoofdkern van elk van die gemeenten.
Een kern van nu is ruimer dan die van 2012, omdat de contour meegroeit met wat gebouwd is; voor de
vraag welke kern een project bij hoort, maakt dat weinig uit.
"""
import csv
import json
import os
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal, ogr
from scipy import ndimage

gdal.UseExceptions()
ogr.UseExceptions()

punten, uit = Path(sys.argv[1]), Path(sys.argv[2])
data = Path(sys.argv[3]) if len(sys.argv) > 3 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
CONTOUR = data / "bron" / "afgeleid" / "stedelijk_gebied_nl_25m.tif"
BUURT = data / "bron" / "waterbodem" / "klimaateffectatlas" / "kea_buurtdashboard_20260312.gpkg"


def main():
    ds = gdal.Open(str(CONTOUR))
    gt, nx, ny, srs = ds.GetGeoTransform(), ds.RasterXSize, ds.RasterYSize, ds.GetProjection()
    band = ds.GetRasterBand(1)
    binnen = band.ReadAsArray() == 1
    ds = None
    print(f"contour: {int(binnen.sum())} cellen van 25 m", flush=True)

    ## kernen: labelen op de 50 m opgerekte contour, daarna terug naar de contour zelf
    opgerekt = ndimage.binary_dilation(binnen, structure=np.ones((3, 3), bool), iterations=2)
    lab, n = ndimage.label(opgerekt, structure=np.ones((3, 3), bool))
    lab[~binnen] = 0
    del opgerekt
    print(f"kernen: {n}", flush=True)

    ## buurten op hetzelfde raster: index van de buurt (1..m)
    vec = ogr.Open(str(BUURT))
    laag = vec.ExecuteSQL("SELECT fid + 0 AS nr, AANT_INW, gemeentecode, Shape FROM buurtdashboard_20260312")
    inw, gm = {}, {}
    for f in laag:
        nr = f.GetField("nr")
        v = f.GetField("AANT_INW")
        inw[nr] = float(v) if v is not None and float(v) > 0 else 0.0
        code = f.GetField("gemeentecode") or ""
        gm[nr] = int(code[2:]) if code.startswith("GM") and code[2:].isdigit() else 0
    laag.ResetReading()
    mem = gdal.GetDriverByName("MEM").Create("", nx, ny, 1, gdal.GDT_UInt32)
    mem.SetGeoTransform(gt); mem.SetProjection(srs); mem.GetRasterBand(1).Fill(0)
    gdal.RasterizeLayer(mem, [1], laag, options=["ATTRIBUTE=nr"])
    vec.ReleaseResultSet(laag)
    bu = mem.GetRasterBand(1).ReadAsArray()
    mem = None
    print(f"buurten: {len(inw)}", flush=True)

    ## inwoners per cel: elke buurt over haar cellen binnen de contour
    sel = (lab > 0) & (bu > 0)
    bu_sel, kern_sel = bu[sel].astype(np.int64), lab[sel].astype(np.int64)
    m = max(inw) + 1
    cellen = np.bincount(bu_sel, minlength=m)
    inw_arr = np.zeros(m); gm_arr = np.zeros(m, dtype=np.int64)
    for k, v in inw.items():
        inw_arr[k] = v; gm_arr[k] = gm[k]
    per_cel = np.where(cellen[bu_sel] > 0, inw_arr[bu_sel] / np.maximum(cellen[bu_sel], 1), 0.0)
    kern_inw = np.bincount(kern_sel, weights=per_cel, minlength=n + 1)
    ## inwoners per kern en gemeente
    gm_sel = gm_arr[bu_sel]
    sleutel = kern_sel * 10000 + gm_sel
    uniek, inv = np.unique(sleutel, return_inverse=True)
    som = np.bincount(inv, weights=per_cel)
    kern_gm = np.zeros(n + 1, dtype=np.int64); beste = np.full(n + 1, -1.0)
    per_gm = {}
    for s_, w in zip(uniek, som):
        k, g = divmod(int(s_), 10000)
        if w > beste[k]:
            beste[k] = w; kern_gm[k] = g
        if g and (g not in per_gm or w > per_gm[g][1]):
            per_gm[g] = (k, w)
    ## hoofdkern per gemeente: de kern met de meeste eigen inwoners
    gm_totaal = {}
    for k_, v in inw.items():
        gm_totaal[gm[k_]] = gm_totaal.get(gm[k_], 0.0) + v
    with open(uit.parent / "kern_hoofd.csv", "w", encoding="utf-8", newline="") as f:
        w_ = csv.writer(f, delimiter=";")
        w_.writerow(["gemeentecode", "kern", "inw_eigen", "aandeel_eigen"])
        for g, (k, w) in sorted(per_gm.items()):
            w_.writerow([g, k, round(w), round(w / gm_totaal[g], 4) if gm_totaal.get(g) else ""])
    meta = {"kernen": int(n), "inwoners_in_kernen": round(float(kern_inw.sum())), "inwoners_buurten": round(sum(inw.values())),
            "gemeenten": len({g for g in gm.values() if g}), "gemeenten_met_hoofdkern": len(per_gm),
            "kernen_die_hoofdkern_zijn": len({k for k, _ in per_gm.values()})}
    (uit.parent / "kernen_meta.json").write_text(json.dumps(meta), encoding="utf-8")
    print(json.dumps(meta), flush=True)

    ids, xs, ys = [], [], []
    with open(punten, encoding="utf-8") as f:
        for rij in csv.DictReader(f, delimiter=";"):
            ids.append(rij["id"]); xs.append(float(rij["x"])); ys.append(float(rij["y"]))
    x = np.array(xs); y = np.array(ys)
    kol = np.floor((x - gt[0]) / gt[1]).astype(np.int64)
    rij = np.floor((y - gt[3]) / gt[5]).astype(np.int64)
    ok = (kol >= 0) & (kol < nx) & (rij >= 0) & (rij < ny)
    kern = np.zeros(x.shape, dtype=np.int64)
    kern[ok] = lab[rij[ok], kol[ok]]
    with open(uit, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["id", "kern", "kern_inw", "kern_gm"])
        for i, k in zip(ids, kern):
            w.writerow([i, int(k), round(float(kern_inw[k])) if k else 0, int(kern_gm[k]) if k else 0])
    print(f"punten: {len(ids)}, in een kern: {int((kern > 0).sum())}", flush=True)


if __name__ == "__main__":
    main()
