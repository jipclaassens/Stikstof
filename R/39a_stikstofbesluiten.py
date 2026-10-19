"""Provinciale publicaties over Natura 2000 en stikstof per provincie en halfjaar, naar fase.

Gebruik (standaard-Python):
  py -3.13 R/39a_stikstofbesluiten.py <uitvoer.json> [datamap]

uitvoer.json   per provincie en halfjaar het aantal aanvragen, ontwerpen en besluiten over
               Natura 2000 of stikstof, sinds 2023
datamap        de datamap van het vergunningenspoor (standaard VERGUNNINGEN_DATA)

Bron: natuur/natuurvergunningen.sqlite (bekendmakingen van provincies) en de tekstvlaggen in
vergunningen.sqlite, beide alleen-lezen. Over Natura 2000 of stikstof: titel of samenvatting
noemt Natura 2000, N2000, stikstof, gebiedsbescherming, passende beoordeling, salderen, AERIUS
of depositie, of de volledige tekst heeft een van de stikstofvlaggen van 15a_natuurbesluiten.py.
Een kale titel over de Wet natuurbescherming telt niet, want die gaat vaak over soorten.
Fase uit de geoogste fase_titel: besluit (ook verleend en geweigerd), aanvraag of ontwerp.

Waarvoor: de toets op de provinciale vergunningstop na intern salderen (39_mediaclaims.R). De
publicatie verschilt sterk per provincie: sinds de Omgevingswet worden aanvragen gepubliceerd, en
in een deel van de provincies noemt de titel van een besluit Natura 2000 niet (Gelderland:
"Toestemming voor ..."), zodat alleen provincies met een volledige tekst of een sprekende titel
iets over de besluiten zeggen.
"""
import json
import os
import re
import sqlite3
import sys
from collections import Counter, defaultdict
from pathlib import Path

uit = Path(sys.argv[1])
data = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
VANAF = "2023-01-01"
STIKSTOF_VLAG = {"stikstof", "stikstofdepositie", "stikstofberekening", "aerius", "natura2000", "natura2000_activiteit",
                 "passende_beoordeling", "intern_salderen", "extern_salderen"}
STIKSTOF_TEKST = re.compile(r"natura|n2000|stikstof|gebiedsbescherming|passende beoordeling|salder|aerius|depositie", re.I)
FASE = {"besluit": "besluit", "verleend": "besluit", "geweigerd": "besluit", "aanvraag": "aanvraag", "ontwerp": "ontwerp"}


def uri(p):
    return "file:" + str(p).replace("\\", "/").replace(" ", "%20") + "?immutable=1"


def halfjaar(d):
    return f"{d[:4]}H{1 if int(d[5:7]) <= 6 else 2}"


def main():
    n = sqlite3.connect(uri(data / "natuur" / "natuurvergunningen.sqlite"), uri=True)
    pubs = {}
    for ident, titel, ab, prov, av, fase in n.execute(
            "SELECT identifier, titel, abstract, creator, available, fase_titel FROM publicatie "
            "WHERE organisatietype = 'provincie' AND available >= ?", (VANAF,)):
        pubs[ident] = {"tekst": (titel or "") + " " + (ab or ""), "prov": prov, "hj": halfjaar(av), "fase": FASE.get(fase), "vlag": set()}
    n.close()
    h = sqlite3.connect(uri(data / "vergunningen.sqlite"), uri=True)
    for ident, term in h.execute("SELECT identifier, term FROM publicatie_tekstvlag"):
        if ident in pubs:
            pubs[ident]["vlag"].add(term)
    h.close()

    tel = defaultdict(Counter)
    for d in pubs.values():
        if d["fase"] and (d["vlag"] & STIKSTOF_VLAG or STIKSTOF_TEKST.search(d["tekst"])):
            tel[(d["prov"], d["hj"])][d["fase"]] += 1
            tel[("alle provincies", d["hj"])][d["fase"]] += 1
    rijen = [{"provincie": p, "halfjaar": hj, "aanvraag": c["aanvraag"], "ontwerp": c["ontwerp"], "besluit": c["besluit"]}
             for (p, hj), c in sorted(tel.items())]
    met_tekst = Counter(d["prov"] for d in pubs.values() if d["vlag"])
    alle = Counter(d["prov"] for d in pubs.values())
    res = {"meta": {"vanaf": VANAF, "publicaties": len(pubs),
                    "aandeel_met_tekstvlag": {p: round(met_tekst[p] / alle[p], 3) for p in sorted(alle)}},
           "per_halfjaar": rijen}
    uit.write_text(json.dumps(res, ensure_ascii=False, indent=1), encoding="utf-8")
    hjs = sorted({r["halfjaar"] for r in rijen})
    print("provincie        fase      " + " ".join(f"{h:>7s}" for h in hjs))
    for p in sorted({r["provincie"] for r in rijen}):
        for f in ("aanvraag", "ontwerp", "besluit"):
            w = {r["halfjaar"]: r[f] for r in rijen if r["provincie"] == p}
            print(f"{p:16s} {f:9s} " + " ".join(f"{w.get(h, 0):7d}" for h in hjs))
    print("aandeel met tekstvlag:", res["meta"]["aandeel_met_tekstvlag"])


if __name__ == "__main__":
    main()
