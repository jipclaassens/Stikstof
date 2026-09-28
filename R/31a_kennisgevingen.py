"""Koppelt de kennisgevingen van ruimtelijke plannen (SRU-stroom ruimtelijk) aan de plandossiers uit de DSO-oogst, en geeft
per dossier de datum van het ontwerp, van de vaststelling en van het onherroepelijk worden volgens de bekendmakingen.

Gebruik (Python 3.13, alleen standaardbibliotheek):
  py -3.13 R/31a_kennisgevingen.py <plannen_sru.sqlite> <plannen_dso.csv> <uitvoermap>

plannen_sru.sqlite  de geladen kennisgevingen (vergunningen/pipeline, oogst.py --stroom ruimtelijk en laad.py), alleen gelezen
plannen_dso.csv     elke planversie uit de oogst van de API Ruimtelijke plannen (29a_plannen_dso.py)

Een kennisgeving noemt het plan met zijn IMRO-nummer (bekendmakingBetreffendePlan in de metadata). Het dossier is dat nummer
zonder de versiecode (het deel na het laatste streepje), zoals de API het dossier afleidt. De fase komt eerst uit de API:
noemt de kennisgeving een versie die in de DSO-oogst staat, dan telt de planstatus van die versie (ontwerp, vastgesteld);
anders de titel (ontwerp, voorontwerp, vaststelling, onherroepelijk of in werking, overig).

Uitvoer:
  kennisgevingen_plannen.csv  een rij per kennisgeving en genoemd plan: identifier;datum;blad;gemeente;plan_id;dossier;
                              fase;fase_bron;titel
  kennisgeving_dossier.csv    een rij per dossier: dossier;ontwerp_kg;vaststelling_kg;onherroepelijk_kg;n_kennisgevingen
                              (de eerste datum per fase)
en op het scherm de dekking en de vergelijking met de datums van de API.
"""
import collections
import csv
import json
import re
import sqlite3
import statistics
import sys
from pathlib import Path

sru, dso, uit = Path(sys.argv[1]), Path(sys.argv[2]), Path(sys.argv[3])
RX_VAST = re.compile(r"vastgesteld|vaststelling|vast te stellen", re.I)
RX_ONHERROEP = re.compile(r"onherroepelijk|in werking getreden|inwerkingtreding|treedt in werking", re.I)
RX_VOORONTWERP = re.compile(r"voorontwerp", re.I)
RX_ONTWERP = re.compile(r"ontwerp|ter inzage|terinzagelegging", re.I)


def dossier_van(plan_id):
    return plan_id.rsplit("-", 1)[0] if "-" in plan_id else plan_id


def fase_titel(titel):
    t = titel or ""
    if RX_ONHERROEP.search(t):
        return "onherroepelijk"
    if RX_VAST.search(t):
        return "vaststelling"
    if RX_VOORONTWERP.search(t):
        return "voorontwerp"
    if RX_ONTWERP.search(t):
        return "ontwerp"
    return "overig"


# ---- planversies uit de DSO-oogst: planstatus en datum per versie, en per dossier de eerste ontwerp- en vaststellingsdatum
versie = {}
api = collections.defaultdict(dict)
with open(dso, encoding="utf-8") as f:
    for r in csv.DictReader(f, delimiter=";"):
        versie[r["identificatie"]] = (r["planstatus"], r["datum"][:10])
        if r["planstatus"] in ("ontwerp", "vastgesteld") and r["datum"]:
            d = api[r["dossier"]]
            if r["planstatus"] not in d or r["datum"][:10] < d[r["planstatus"]]:
                d[r["planstatus"]] = r["datum"][:10]

# ---- kennisgevingen
con = sqlite3.connect(f"file:{sru.as_posix()}?mode=ro", uri=True)
rijen = []
zonder_id = 0
for ident, titel, creator, blad, beschikbaar, extra in con.execute(
        "SELECT identifier, titel, creator, publicatienaam, available, extra_json FROM publicatie"):
    e = json.loads(extra) if extra else {}
    ids = sorted({x.strip() for x in e.get("bekendmakingBetreffendePlan", []) if x and x.strip().startswith("NL.IMRO.")})
    if not ids:
        zonder_id += 1
        continue
    for pid in ids:
        if pid in versie and versie[pid][0] in ("ontwerp", "vastgesteld", "voorontwerp"):
            fase = {"ontwerp": "ontwerp", "vastgesteld": "vaststelling", "voorontwerp": "voorontwerp"}[versie[pid][0]]
            # een kennisgeving van onherroepelijk worden noemt vaak de vastgestelde versie
            if fase == "vaststelling" and RX_ONHERROEP.search(titel or ""):
                fase = "onherroepelijk"
            bron = "api"
        else:
            fase, bron = fase_titel(titel), "titel"
        rijen.append((ident, (beschikbaar or "")[:10], blad, creator, pid, dossier_van(pid), fase, bron, titel))
con.close()
uit.mkdir(parents=True, exist_ok=True)
with open(uit / "kennisgevingen_plannen.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["identifier", "datum", "blad", "gemeente", "plan_id", "dossier", "fase", "fase_bron", "titel"])
    w.writerows(rijen)

per = collections.defaultdict(lambda: {"ontwerp": None, "vaststelling": None, "onherroepelijk": None, "n": 0})
for ident, datum, blad, creator, pid, dos, fase, bron, titel in rijen:
    p = per[dos]
    p["n"] += 1
    if fase in ("ontwerp", "vaststelling", "onherroepelijk") and datum and (p[fase] is None or datum < p[fase]):
        p[fase] = datum
with open(uit / "kennisgeving_dossier.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["dossier", "ontwerp_kg", "vaststelling_kg", "onherroepelijk_kg", "n_kennisgevingen"])
    for dos in sorted(per):
        p = per[dos]
        w.writerow([dos, p["ontwerp"] or "", p["vaststelling"] or "", p["onherroepelijk"] or "", p["n"]])

# ---- meting
print(f"kennisgevingen met een IMRO-nummer: {len({r[0] for r in rijen}):,}; zonder: {zonder_id:,}; koppelingen aan een plan: {len(rijen):,}")
print("fase (bron):", collections.Counter((r[6], r[7]) for r in rijen).most_common())
print("blad:", collections.Counter(r[2] for r in rijen).most_common(6))
in_api = sum(1 for r in rijen if r[5] in api)
print(f"koppelingen waarvan het dossier in de DSO-oogst staat: {in_api:,} van {len(rijen):,}")


def verschil(a, b):
    from datetime import date
    return (date.fromisoformat(a) - date.fromisoformat(b)).days


for fase, sleutel in (("ontwerp", "ontwerp"), ("vaststelling", "vastgesteld")):
    v = [verschil(per[d][fase], api[d][sleutel]) for d in per if per[d][fase] and d in api and sleutel in api[d]]
    if v:
        q = statistics.quantiles(v, n=20)
        print(f"{fase}: kennisgeving min API-datum, {len(v):,} dossiers: mediaan {statistics.median(v):.0f} dagen, "
              f"5 procent {q[0]:.0f}, 95 procent {q[-1]:.0f}, binnen 30 dagen {100 * sum(abs(x) <= 30 for x in v) / len(v):.1f} procent")

# dekking van de ontwerpdatum bij de vastgestelde dossiers per jaar van vaststelling (alle dossiers met een vastgestelde versie)
dek = collections.defaultdict(lambda: [0, 0, 0, 0])
for d, a in api.items():
    if "vastgesteld" not in a:
        continue
    j = a["vastgesteld"][:4]
    k = per.get(d)
    heeft_api = "ontwerp" in a and a["ontwerp"] <= a["vastgesteld"]
    heeft_kg = bool(k and k["ontwerp"] and k["ontwerp"] <= a["vastgesteld"])
    dek[j][0] += 1
    dek[j][1] += heeft_api
    dek[j][2] += heeft_kg
    dek[j][3] += heeft_api or heeft_kg
print("jaar van vaststelling: dossiers, met ontwerpdatum uit de API, uit de kennisgevingen, uit een van beide (procent)")
for j in sorted(dek):
    if "2012" <= j <= "2026":
        n, a1, k1, e1 = dek[j]
        print(f"  {j}: {n:6,} {100 * a1 / n:5.1f} {100 * k1 / n:5.1f} {100 * e1 / n:5.1f}")
