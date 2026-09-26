"""CBS-toets: legt per gemeente en jaar drie tellingen van vergunde nieuwbouw naast elkaar.

1. CBS 83671NED: woningen in verleende bouwvergunningen (bouwsom boven 50.000 euro), totaal opdrachtgevers en eigendom,
   kwartalen gesommeerd tot jaren (bron\\gebouwen\\cbs\\83671NED\\TypedDataSet.csv).
2. BAG: verblijfsobjecten met woonfunctie in panden die dat jaar de status Bouwvergunning verleend kregen, en het aantal
   van die panden (analyse\\bag_panden.csv uit bag_export.py).
3. Bekendmakingen: zaken met een aanvraag in dat jaar en het projecttype nieuwbouw van woningen uit de titel, waarvan
   verleend, en het woningaantal dat in de titels staat (analyse\\zaken.csv); de gemeente van een creator komt uit de
   meerderheid van de gemeentecodes van de aan die creator gekoppelde BAG-panden (koppel_bag.py).
Daarnaast de dekking van de BAG-koppeling per gemeente en jaar (analyse\\bag_dekking_gemeente_jaar.csv).

Uitvoer: analyse\\cbs_toets_gemeente_jaar.csv en op het scherm de landelijke sommen per jaar en de correlaties over
gemeenten. Gebruik: py -3.13 cbs_toets.py
"""
import csv
import math
import sqlite3
import sys
from collections import Counter, defaultdict

from config import DATA_DIR, DB_PAD

CBS = DATA_DIR / "bron" / "gebouwen" / "cbs" / "83671NED" / "TypedDataSet.csv"
GRENZEN = DATA_DIR / "bron" / "gebouwen" / "grenzen" / "bestuurlijke_gebieden.gpkg"
PANDEN = DATA_DIR / "analyse" / "bag_panden.csv"
ZAKEN = DATA_DIR / "analyse" / "zaken.csv"
DEKKING = DATA_DIR / "analyse" / "bag_dekking_gemeente_jaar.csv"
UIT = DATA_DIR / "analyse" / "cbs_toets_gemeente_jaar.csv"


def pearson(xs, ys):
    n = len(xs)
    if n < 3:
        return float("nan")
    mx, my = sum(xs) / n, sum(ys) / n
    sxy = sum((x - mx) * (y - my) for x, y in zip(xs, ys))
    sxx = sum((x - mx) ** 2 for x in xs)
    syy = sum((y - my) ** 2 for y in ys)
    return sxy / math.sqrt(sxx * syy) if sxx and syy else float("nan")


def main(argv):
    namen = {c: n for n, c in sqlite3.connect(GRENZEN).execute('select naam, code from "bestuurlijkegebieden:Gemeentegebied"')}
    cbs = defaultdict(int)
    with open(CBS, encoding="utf-8-sig", newline="") as f:
        for r in csv.DictReader(f):
            if r["Opdrachtgever"].strip() != "T001209" or r["Eigendom"].strip() != "T001258" or not r["RegioS"].startswith("GM"):
                continue
            p = r["Perioden"].strip()
            if "KW" not in p:
                continue
            w = r["Woningen_2"].strip()
            cbs[(r["RegioS"].strip()[2:], p[:4])] += int(w) if w else 0
    bag_w, bag_p, pand_gm = defaultdict(int), defaultdict(int), {}
    with open(PANDEN, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f):
            gm = r["gemeentecode"]
            if not gm:
                continue
            pand_gm[r["identificatie"]] = gm
            n_woon = int(r["n_woon"] or 0)
            if n_woon:
                sleutel = (gm, r["verg_begin"][:4])
                bag_w[sleutel] += n_woon
                bag_p[sleutel] += 1
    # creator -> gemeentecode via de gekoppelde panden
    conn = sqlite3.connect(DB_PAD, timeout=900)
    creator_van = dict(conn.execute("SELECT zaak_id, creator FROM zaak WHERE organisatietype = 'gemeente'"))
    stem = defaultdict(Counter)
    for zaak_id, pand_id in conn.execute("SELECT zaak_id, pand_id FROM zaak_bag"):
        cr = creator_van.get(zaak_id)
        gm = pand_gm.get(pand_id)
        if cr and gm:
            stem[cr][gm] += 1
    conn.close()
    code_van = {cr: c.most_common(1)[0][0] for cr, c in stem.items()}
    bek_n, bek_v, bek_w, bek_bouw = defaultdict(int), defaultdict(int), defaultdict(int), defaultdict(int)
    onbekend = Counter()
    with open(ZAKEN, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            gm = code_van.get(r["gemeente"])
            if not gm:
                onbekend[r["gemeente"]] += 1
                continue
            sleutel = (gm, r["datum_aanvraag"][:4])
            if r["bouw"] == "1":
                bek_bouw[sleutel] += 1
            if r["soort"] == "nieuwbouw_woning":
                bek_n[sleutel] += 1
                if r["verleend"] == "1":
                    bek_v[sleutel] += 1
                if r["woningen"]:
                    bek_w[sleutel] += int(float(r["woningen"]))
    dek = {}
    with open(DEKKING, encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            dek[(r["gemeentecode"], r["jaar"])] = (int(r["woonpanden"]), int(r["woonpanden_gekoppeld"]))
    sleutels = sorted(set(cbs) | set(bag_w) | set(bek_n))
    with open(UIT, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["gemeentecode", "gemeente", "jaar", "cbs_woningen", "bag_woningen", "bag_woonpanden", "bek_bouwzaken",
                    "bek_nieuwbouw_zaken", "bek_nieuwbouw_verleend", "bek_titel_woningen", "bag_woonpanden_gekoppeld"])
        for gm, j in sleutels:
            if j < "2012" or gm not in namen:
                continue
            d = dek.get((gm, j), (0, 0))
            w.writerow([gm, namen[gm], j, cbs.get((gm, j), ""), bag_w.get((gm, j), 0), bag_p.get((gm, j), 0), bek_bouw.get((gm, j), 0),
                        bek_n.get((gm, j), 0), bek_v.get((gm, j), 0), bek_w.get((gm, j), 0), d[1]])
    print(f"geschreven: {UIT}; creators zonder gemeentecode: {sum(onbekend.values())} zaken bij {len(onbekend)} creators "
          f"{onbekend.most_common(5)}")
    print("jaar   cbs_woningen  bag_woningen  bag_woonpanden  bek_nieuwbouw_zaken  bek_verleend  bek_titel_woningen  r(cbs,bag)  r(bag_panden,bek_zaken)")
    for j in [str(y) for y in range(2012, 2026)]:
        gms = [gm for gm in namen if (gm, j) in cbs or (gm, j) in bag_w]
        c_tot = sum(cbs.get((gm, j), 0) for gm in gms)
        b_tot = sum(bag_w.get((gm, j), 0) for gm in gms)
        p_tot = sum(bag_p.get((gm, j), 0) for gm in gms)
        n_tot = sum(bek_n.get((gm, j), 0) for gm in gms)
        v_tot = sum(bek_v.get((gm, j), 0) for gm in gms)
        w_tot = sum(bek_w.get((gm, j), 0) for gm in gms)
        r1 = pearson([cbs.get((gm, j), 0) for gm in gms], [bag_w.get((gm, j), 0) for gm in gms])
        r2 = pearson([bag_p.get((gm, j), 0) for gm in gms], [bek_n.get((gm, j), 0) for gm in gms]) if n_tot else float("nan")
        print(f"{j}  {c_tot:12d}  {b_tot:12d}  {p_tot:14d}  {n_tot:19d}  {v_tot:12d}  {w_tot:18d}  {r1:10.3f}  {r2:23.3f}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
