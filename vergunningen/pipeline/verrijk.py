"""Verrijking: leest per zaak en per BAG-pand de kaartlagen uit de themamappen af, als invoer voor de tweede ronde modellen.

Per locatie:
- stikstofdepositie (RIVM GDN, mol N per ha per jaar, 1 km): de consistente historische reeks 2005 tot 2024 en de
  kaart van 2025, in het jaar van de aanvraag of vergunning (ndep_jaar) en in 2018, het laatste jaar voor de
  PAS-uitspraak (ndep_2018), als vaste blootstelling die niet door de uitkomst kan zijn beinvloed;
- veehouderij (afgeleid uit de Woo-stalbestanden, bron\\ftm_stallen): NH3-stalemissie binnen 1 km en afstand tot de
  dichtstbijzijnde veehouderij met vee in 2022, en het aantal PAS-melders in het 5 bij 5 km-blok rond de locatie;
- klimaat en water (Klimaateffectatlas): bodemdaling 2020 tot 2050 bij hoog scenario, plaatsgebonden
  overstromingskans huidig voor 20 cm, maximale waterdiepte bij een kleine kans;
- beleid en net (bron\\afgeleid, als die rasters er zijn): binnen of buiten de verstedelijkingscontour van de provincie
  en de afnamecongestie van het elektriciteitsnet op drie standen en de eerste stand met rood.
Rasters die ontbreken worden overgeslagen, zodat het script opnieuw kan draaien als ze er wel zijn.

Draaien met de QGIS-python: "C:\\Program Files\\QGIS 3.38.1\\bin\\python-qgis.bat" verrijk.py
Uitvoer: analyse\\zaak_kenmerken.csv en analyse\\pand_kenmerken.csv (puntkomma).
"""
import csv
import sqlite3
import sys
from pathlib import Path

import numpy as np
from osgeo import gdal

from config import DATA_DIR, DB_PAD

BRON = DATA_DIR / "bron"
ANALYSE = DATA_DIR / "analyse"
KEA = BRON / "waterbodem" / "klimaateffectatlas"
AFG = BRON / "afgeleid"

# (kolom, pad, type) op het 25 meter-grid
RASTERS_25M = [
    ("nh3_1km_kg", BRON / "ftm_stallen" / "nh3_binnen_1km_2022_25m.tif", "getal"),
    ("afstand_veehouderij_m", BRON / "ftm_stallen" / "afstand_veehouderij_2022_25m.tif", "getal"),
    ("bodemdaling_2050_hoog_cm", KEA / "kea_droogte_bodemdaling_2020_2050_hoog_100m_25m.tif", "getal"),
    ("overstromingskans_20cm", KEA / "kea_overstroming_plaatsgebonden_kans_huidig_20cm_20251120_5m_25m.tif", "klasse"),
    ("waterdiepte_kleine_kans_m", KEA / "kea_overstroming_maximale_waterdiepte_kleine_kans_20260128_5m_25m.tif", "getal"),
    ("stedelijk_gebied", AFG / "stedelijk_gebied_nl_25m.tif", "klasse"),
    ("congestie_afname_20240417", AFG / "congestie_afname_20240417_25m.tif", "klasse"),
    ("congestie_afname_20250711", AFG / "congestie_afname_20250711_25m.tif", "klasse"),
    ("congestie_afname_20260921", AFG / "congestie_afname_20260921_25m.tif", "klasse"),
    ("congestie_afname_eerste", AFG / "congestie_afname_eerste_25m.tif", "klasse"),
]
GDN_HIST = BRON / "depositie" / "gdn_depositie_historische_reeks"
GDN = BRON / "depositie" / "gdn_depositie"
PAS_KM = BRON / "ftm_stallen" / "pas_melders_km.csv"


def lees_raster(pad: Path):
    ds = gdal.Open(str(pad))
    band = ds.GetRasterBand(1)
    return band.ReadAsArray(), ds.GetGeoTransform(), band.GetNoDataValue()


def bemonster(arr, gt, nodata, x, y):
    kol = np.floor((x - gt[0]) / gt[1]).astype(np.int64)
    rij = np.floor((y - gt[3]) / gt[5]).astype(np.int64)
    ok = np.isfinite(x) & np.isfinite(y) & (kol >= 0) & (kol < arr.shape[1]) & (rij >= 0) & (rij < arr.shape[0])
    uit = np.full(x.shape, np.nan)
    uit[ok] = arr[rij[ok], kol[ok]].astype(np.float64)
    if nodata is not None:
        uit[uit == nodata] = np.nan
    return uit


def depositie(jaren: np.ndarray, x: np.ndarray, y: np.ndarray):
    """Depositie in het gegeven jaar (historische reeks tot 2024, kaart 2025 daarna) en in 2018."""
    kaarten = {}
    for j in range(2005, 2025):
        p = GDN_HIST / f"ndep_historisch_{j}.tif"
        if p.exists():
            kaarten[j] = lees_raster(p)
    p = GDN / "gdn_ntot_2025.tif"
    if p.exists():
        kaarten[2025] = lees_raster(p)
    uit = np.full(x.shape, np.nan)
    for j in np.unique(jaren[np.isfinite(jaren)]).astype(int):
        k = min(max(j, 2005), max(kaarten))
        sel = jaren == j
        arr, gt, nd = kaarten[k]
        uit[sel] = bemonster(arr, gt, nd, x[sel], y[sel])
    arr, gt, nd = kaarten[2018]
    return uit, bemonster(arr, gt, nd, x, y)


def pas_blok(x, y, straal_cellen=2):
    """Aantal PAS-melders in het blok van (2 maal straal plus 1) kilometercellen rond de locatie."""
    if not PAS_KM.exists():
        return np.full(x.shape, np.nan)
    rooster = np.zeros((330, 290), dtype=np.int32)  # rij = (630 - y_km), kol = x_km
    with open(PAS_KM, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            kx, ky = int(float(r["x_mid"]) // 1000), int(float(r["y_mid"]) // 1000)
            if 0 <= kx < 290 and 0 <= 629 - ky < 330:
                rooster[629 - ky, kx] += int(r["n_melders"])
    # somfilter via cumulatieve som
    s = straal_cellen
    pad = np.pad(rooster, s, mode="constant")
    cs = pad.cumsum(0).cumsum(1)
    cs = np.pad(cs, ((1, 0), (1, 0)), mode="constant")
    h, w = rooster.shape
    blok = cs[2 * s + 1:2 * s + 1 + h, 2 * s + 1:2 * s + 1 + w] - cs[:h, 2 * s + 1:2 * s + 1 + w] - cs[2 * s + 1:2 * s + 1 + h, :w] + cs[:h, :w]
    kx = np.floor(x / 1000).astype(np.int64)
    ky = 629 - np.floor(y / 1000).astype(np.int64)
    ok = np.isfinite(x) & np.isfinite(y) & (kx >= 0) & (kx < 290) & (ky >= 0) & (ky < 330)
    uit = np.full(x.shape, np.nan)
    uit[ok] = blok[ky[ok], kx[ok]]
    return uit


def verrijk(sleutels, x, y, jaren, uit_pad: Path):
    kolommen = {}
    kolommen["ndep_jaar"], kolommen["ndep_2018"] = depositie(jaren, x, y)
    kolommen["pas_melders_5km"] = pas_blok(x, y)
    for naam, pad, soort in RASTERS_25M:
        if not pad.exists():
            print(f"  {naam}: {pad.name} ontbreekt, overgeslagen")
            continue
        arr, gt, nd = lees_raster(pad)
        kolommen[naam] = bemonster(arr, gt, nd, x, y)
        del arr
        print(f"  {naam}: {int(np.isfinite(kolommen[naam]).sum())} waarden")
    with open(uit_pad, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["sleutel"] + list(kolommen))
        for i, s in enumerate(sleutels):
            rij = [s]
            for k in kolommen:
                v = kolommen[k][i]
                rij.append("" if not np.isfinite(v) else (int(v) if float(v).is_integer() else round(float(v), 3)))
            w.writerow(rij)
    print(f"geschreven: {uit_pad} ({len(sleutels)} rijen, kolommen {', '.join(kolommen)})")


def main(argv):
    conn = sqlite3.connect(DB_PAD, timeout=900)
    rijen = conn.execute("SELECT zaak_id, x, y, coalesce(datum_aanvraag, eerste_datum) FROM zaak WHERE x IS NOT NULL").fetchall()
    conn.close()
    print(f"zaken: {len(rijen)}")
    sleutels = [r[0] for r in rijen]
    x = np.array([r[1] for r in rijen], dtype=np.float64)
    y = np.array([r[2] for r in rijen], dtype=np.float64)
    jaren = np.array([float(r[3][:4]) if r[3] else np.nan for r in rijen])
    verrijk(sleutels, x, y, jaren, ANALYSE / "zaak_kenmerken.csv")
    sleutels, xs, ys, js = [], [], [], []
    with open(ANALYSE / "bag_panden.csv", encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f):
            if not r["x"] or not r["verg_begin"]:
                continue
            sleutels.append(r["identificatie"])
            xs.append(float(r["x"]))
            ys.append(float(r["y"]))
            js.append(float(r["verg_begin"][:4]))
    print(f"panden: {len(sleutels)}")
    verrijk(sleutels, np.array(xs), np.array(ys), np.array(js), ANALYSE / "pand_kenmerken.csv")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
