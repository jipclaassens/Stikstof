"""Ligt een zaak binnen of buiten de huidige verstedelijkingscontour? Voor de aanvragen met een volledige tekst (34_tekstvlaggen.R).

Gebruik (Python van QGIS: GDAL):
  python-qgis-ltr.bat R/34a_contour_zaken.py <vergunningen.sqlite> <map bron\\afgeleid> <teksten_vlaggen.csv> <uitvoer.csv>

vergunningen.sqlite  de database van bekendmakingen (tabel zaak, met x, y in RD en het locatietype), alleen gelezen
bron\\afgeleid        stedelijk_gebied_nl_25m.tif (1 binnen de contour van de provincie, 0 erbuiten, 255 buiten Nederland) en
                     contourbron_nl_25m.tif (de provincie wier contour geldt; 125 is Gelderland, waar de bebouwde kom de contour
                     vervangt); zie BRONNEN.md in die map
teksten_vlaggen.csv  24a_teksten.py teksten: welke zaken een volledige tekst hebben
Uitvoer: zaak_id;x;y;locatietype;binnen_contour;contourbron (leeg als de zaak geen locatie heeft of buiten Nederland valt)
"""
import csv
import sqlite3
import sys
from pathlib import Path

from osgeo import gdal

gdal.UseExceptions()
db, afgeleid, vlaggen, uit = (Path(a) for a in sys.argv[1:5])
with open(vlaggen, encoding="utf-8") as f:
    ids = {int(r["zaak_id"]) for r in csv.DictReader(f, delimiter=";")}
con = sqlite3.connect(f"file:{db.as_posix()}?mode=ro", uri=True, timeout=120)
rijen = [r for r in con.execute("SELECT zaak_id, x, y, locatietype FROM zaak") if r[0] in ids]
con.close()


def lees(naam):
    ds = gdal.Open(str(afgeleid / naam))
    b = ds.GetRasterBand(1)
    return ds.GetGeoTransform(), b.ReadAsArray(), b.GetNoDataValue()


gt, contour, nd = lees("stedelijk_gebied_nl_25m.tif")
gt2, bron, nd2 = lees("contourbron_nl_25m.tif")
assert gt == gt2 and contour.shape == bron.shape
n = 0
with open(uit, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["zaak_id", "x", "y", "locatietype", "binnen_contour", "contourbron"])
    for zid, x, y, lt in rijen:
        c = p = ""
        if x is not None and y is not None:
            kol = int((x - gt[0]) // gt[1])
            rij = int((y - gt[3]) // gt[5])
            if 0 <= rij < contour.shape[0] and 0 <= kol < contour.shape[1] and contour[rij, kol] != nd:
                c, p = int(contour[rij, kol]), int(bron[rij, kol])
                n += 1
        w.writerow([zid, x, y, lt, c, p])
print(f"{len(ids)} zaken met tekst, {len(rijen)} in de tabel zaak, {n} met een contourwaarde; geschreven: {uit}")
