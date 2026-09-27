"""Provinciale natuurbesluiten over woningnieuwbouw: tellen per jaar en koppelen aan grote BAG-projecten.

Gebruik (standaard-Python):
  py -3.13 R/15a_natuurbesluiten.py <projecten.csv> <uitvoer.json> [datamap]

projecten.csv  een rij per BAG-project van 20 of meer objecten (15_natuurbesluiten.R):
               projectid;objecten;x;y;vergund;jaar;ring;type
uitvoer.json   tellingen per jaar, de koppeling met een kansniveau, en de tijd tussen
               natuurbesluit en BAG-vergunning
datamap        de datamap van het vergunningenspoor (standaard VERGUNNINGEN_DATA)

Bron: natuur/natuurvergunningen.sqlite (bekendmakingen van provincies en Rijk) en de
tekstvlaggen in vergunningen.sqlite, beide alleen-lezen. De indeling volgt de verkenning
van 27 september 2026 (vergunningen/inzichten.md, hoofdstuk 29):
- een natuurbesluit: bevoegd gezag provincie of Rijk, over natuur volgens titel, label of
  tekstvlag, in een rubriek voor beschikkingen; aanvragen en ontwerpen tellen mee;
- woningnieuwbouw: titel plus samenvatting noemt woningen of een woonwijk en nieuwbouw,
  realisatie of ontwikkeling, niet alleen renovatie, sloop of verduurzaming;
- over stikstof of Natura 2000: dat staat er expliciet, in titel, samenvatting of tekstvlag
  (stikstof, Natura 2000, gebiedsbescherming, depositie, AERIUS, passende beoordeling, salderen);
  een kale titel over de Wet natuurbescherming telt niet, want die gaat vaak over soorten;
- over soorten: ontheffing, soorten, flora, fauna of een beschermde soort, zonder stikstof;
- fase uit de titel: aanvraag (ook ontvangen), ontwerp, of anders besluit;
- koppelen alleen met een precieze locatie (punt, adres, perceel, vlak of weg tot 1 km),
  zonder standaardpunten (coordinaten met 20 of meer publicaties), binnen 100 of 250 m van
  het project en van drie jaar voor tot een jaar na de BAG-vergunning. Het kansniveau is
  hetzelfde met alle besluitpunten 1 km verschoven, gemiddeld over acht richtingen.
"""
import csv
import datetime as dt
import json
import math
import os
import re
import sqlite3
import sys
from collections import Counter, defaultdict
from pathlib import Path

projecten_csv, uit = Path(sys.argv[1]), Path(sys.argv[2])
data = Path(sys.argv[3]) if len(sys.argv) > 3 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))

SOORT = [
    ("houtopstanden", re.compile(r"\b(houtopstand(en)?|kap(pen|vergunning|melding)?|herplant(plicht)?|vell(en|ing))\b", re.I)),
    ("soorten", re.compile(r"\b(ontheffing(en)?|soorten(bescherming)?|flora|fauna|ff-?wet|fauna-activiteit|vleermuis|huismus|gierzwaluw|das|rugstreeppad|steenmarter|nesten|rustplaatsen)\b", re.I)),
    ("gebied", re.compile(r"natura|gebiedsbescherming|stikstof|natuurvergunning|\bnbw\b|nb-wet|natuurbeschermingswet|\bwnb\b|passende beoordeling|salder", re.I)),
    ("wnb_onbepaald", re.compile(r"natuurbescherming", re.I)),
]
NATUUR_VLAG = {"natura2000_activiteit", "natura2000", "wet_natuurbescherming", "flora_fauna", "soortenbescherming", "aerius",
               "stikstof", "stikstofdepositie", "stikstofberekening", "passende_beoordeling", "intern_salderen",
               "extern_salderen", "natuurtoets"}
STIKSTOF_VLAG = {"stikstof", "stikstofdepositie", "stikstofberekening", "aerius", "natura2000", "natura2000_activiteit",
                 "passende_beoordeling", "intern_salderen", "extern_salderen"}
STIKSTOF_TITEL = re.compile(r"natura|stikstof|gebiedsbescherming|passende beoordeling|salder|aerius|depositie", re.I)
BESCHIKKING_RUB = {"omgevingsvergunning", "andere vergunning", "andere beschikking", "Vergunningen", "Ontheffingen",
                   "andere melding", "omgevingsmelding", "vergunning voor activiteiten op of in oppervlaktewater",
                   "exploitatievergunning", "evenementenvergunning"}
WON_TOKEN = re.compile(r"\b(\w*)woning(en)?\b", re.I)
UITGESLOTEN = ("bedrijfs", "recreatie", "vakantie", "dienst", "plattelands", "zomer", "agrarische", "boerderij", "bungalow", "burger")
STERK = re.compile(r"woningbouw\w*|appartement\w*|wooneenhe\w*|woonwijk\w*|nieuwbouwwijk\w*|woontoren\w*|woongebouw\w*|"
                   r"woonzorg\w*|woonlocatie\w*|woonbestemming\w*|woonblok\w*|woongebied\w*|woonunit\w*|studentenhuisvesting|"
                   r"seniorenhuisvesting|woonvoorziening\w*|meergezins\w*|eengezins\w*|grondgebonden woning\w*", re.I)
NIEUW = re.compile(r"nieuwbouw\w*|\bbouw(en)?\b|\bbouw van\b|realis\w*|ontwikkel\w*|\baanleg\w*|in ?gebruik ?(name|neming|nemen)|"
                   r"gebruiksfase|exploitatie|woningbouw(?!vereniging|corporatie|stichting)\w*|woonwijk\w*|nieuwbouwwijk|bouwrijp|bouwplan\w*|"
                   r"\bproject\w*|plangebied|\bplan\b|te bouwen|op te richten|oprichten|transformatie|omvorm\w*|herontwikkel\w*", re.I)
PROVINCIES = ["Drenthe", "Flevoland", "Fryslân", "Gelderland", "Groningen", "Limburg", "Noord-Brabant", "Noord-Holland",
              "Overijssel", "Utrecht", "Zeeland", "Zuid-Holland"]
## provincies die sinds 2018 elk jaar minstens 100 natuurbesluiten publiceren (verkenning, dekking)
VAST_2018 = {"Gelderland", "Zuid-Holland", "Utrecht", "Zeeland", "Limburg", "Overijssel", "Fryslân"}


def uri(p):
    return "file:" + str(p).replace("\\", "/").replace(" ", "%20") + "?immutable=1"


def woningwoord(t):
    for m in WON_TOKEN.finditer(t or ""):
        if not any(m.group(1).lower().endswith(u) for u in UITGESLOTEN):
            return True
    return False


def woning_nieuw(t):
    return bool((STERK.search(t) or woningwoord(t)) and NIEUW.search(t))


def soort_van(titel):
    for naam, rx in SOORT:
        if rx.search(titel or ""):
            return naam
    return "niet_natuur"


def laad():
    n = sqlite3.connect(uri(data / "natuur" / "natuurvergunningen.sqlite"), uri=True)
    pubs = {}
    for ident, titel, abstract, creator, org, rubriek, available in n.execute(
            "SELECT identifier, titel, abstract, creator, organisatietype, rubriek, available FROM publicatie"):
        pubs[ident] = {"titel": titel or "", "abstract": abstract or "", "creator": creator, "org": org, "rubriek": rubriek,
                       "datum": dt.date.fromisoformat(available[:10]) if available else None, "act": set(), "vlag": set(), "loc": []}
    for ident, act in n.execute("SELECT identifier, activiteit FROM publicatie_activiteit"):
        if ident in pubs:
            pubs[ident]["act"].add(act)
    for ident, t, geom, x, y, ext in n.execute("SELECT identifier, locatietype, geomtype, x, y, uitgestrektheid_m FROM publicatie_locatie"):
        if ident in pubs and x is not None:
            pubs[ident]["loc"].append((t, geom, x, y, ext))
    n.close()
    h = sqlite3.connect(uri(data / "vergunningen.sqlite"), uri=True)
    for ident, term in h.execute("SELECT identifier, term FROM publicatie_tekstvlag"):
        if ident in pubs:
            pubs[ident]["vlag"].add(term)
    h.close()
    for d in pubs.values():
        d["soort"] = soort_van(d["titel"])
        d["natuur"] = d["soort"] != "niet_natuur" or "natuur" in d["act"] or bool(d["vlag"] & NATUUR_VLAG)
        d["bevoegd"] = d["org"] in ("provincie", "ministerie", "dienst en agentschap")
        d["gebied"] = d["soort"] in ("gebied", "wnb_onbepaald") or bool(d["vlag"] & STIKSTOF_VLAG) or bool(STIKSTOF_TITEL.search(d["titel"]))
        d["woning_nieuw"] = woning_nieuw(d["titel"] + " || " + d["abstract"])
    return pubs


EXPLICIET = re.compile(r"stikstof|natura|gebiedsbescherming|depositie|aerius|passende beoordeling|salder", re.I)
SOORTEN_RX = re.compile(r"soort|ontheffing|flora|fauna|vleermuis|huismus|gierzwaluw|nesten|verblijfplaats", re.I)


def onderwerp(d):
    t = d["titel"] + " " + d["abstract"]
    if d["vlag"] & STIKSTOF_VLAG or STIKSTOF_TITEL.search(d["titel"]) or EXPLICIET.search(t):
        return "stikstof"
    if SOORTEN_RX.search(t) or d["soort"] == "soorten":
        return "soorten"
    return "overig"


def fase(d):
    t = d["titel"].lower()
    if ("aanvraag" in t and "besluit" not in t and "beschikking" not in t) or "ontvangen" in t:
        return "aanvraag"
    if "ontwerp" in t:
        return "ontwerp"
    return "besluit"


def precies(loc):
    t, geom, x, y, ext = loc
    return t in ("Punt", "Adres", "Perceel") or (t in ("Vlak", "Weg", "Lijn") and (ext or 0) <= 1000)


def main():
    pubs = laad()
    besl = {k: d for k, d in pubs.items() if d["bevoegd"] and d["natuur"] and d["rubriek"] in BESCHIKKING_RUB and d["datum"]}
    ## standaardpunten: coordinaten met 20 of meer publicaties
    tel = Counter((round(l[2]), round(l[3])) for d in besl.values() for l in d["loc"])
    std = {k for k, v in tel.items() if v >= 20}

    ## 1. tellingen per jaar
    for d in besl.values():
        d["onderwerp"] = onderwerp(d)
        d["fase"] = fase(d)
    per_jaar = []
    for j in range(2014, 2027):
        for groep, filt in (("alle provincies", lambda d: d["org"] == "provincie"),
                            ("vaste zeven provincies", lambda d: d["org"] == "provincie" and d["creator"] in VAST_2018)):
            s = [d for d in besl.values() if d["datum"].year == j and filt(d)]
            w = [d for d in s if d["woning_nieuw"]]
            st = [d for d in w if d["onderwerp"] == "stikstof"]
            per_jaar.append({"jaar": j, "groep": groep, "natuurbesluiten": len(s),
                             "met_samenvatting": sum(bool(d["abstract"].strip()) for d in s),
                             "woningnieuwbouw": len(w),
                             "woningnieuwbouw_stikstof": len(st),
                             "woningnieuwbouw_soorten": sum(d["onderwerp"] == "soorten" for d in w),
                             "woningnieuwbouw_overig": sum(d["onderwerp"] == "overig" for d in w),
                             "stikstof_aanvraag": sum(d["fase"] == "aanvraag" for d in st),
                             "stikstof_ontwerp": sum(d["fase"] == "ontwerp" for d in st),
                             "stikstof_besluit": sum(d["fase"] == "besluit" for d in st)})
    per_provincie = [{"provincie": p_, "jaar": j, "woningnieuwbouw": sum(1 for d in besl.values() if d["org"] == "provincie"
                                                                        and d["creator"] == p_ and d["datum"].year == j and d["woning_nieuw"])}
                     for p_ in PROVINCIES for j in range(2019, 2027)]

    ## 2. koppeling aan grote BAG-projecten
    punten = {}

    def vul(filt):
        punten.clear()
        for k, d in besl.items():
            if not (d["woning_nieuw"] and filt(d)):
                continue
            pts = [(l[2], l[3]) for l in d["loc"] if precies(l) and (round(l[2]), round(l[3])) not in std]
            if pts:
                punten[k] = pts
    proj = []
    with open(projecten_csv, encoding="utf-8") as f:
        for r in csv.DictReader(f, delimiter=";"):
            proj.append({"id": r["projectid"], "x": float(r["x"]), "y": float(r["y"]), "datum": dt.date.fromisoformat(r["vergund"]),
                         "jaar": int(r["jaar"]), "ring": r["ring"], "type": r["type"], "objecten": int(r["objecten"])})
    CEL = 1000.0

    def index(dx=0.0, dy=0.0):
        r = defaultdict(list)
        for k, pts in punten.items():
            for x, y in pts:
                r[(int((x + dx) // CEL), int((y + dy) // CEL))].append((k, x + dx, y + dy))
        return r

    def koppel(idx, straal):
        treffers = {}
        for p in proj:
            beste = None
            for ddx in (-1, 0, 1):
                for ddy in (-1, 0, 1):
                    for k, nx, ny in idx.get((int(p["x"] // CEL) + ddx, int(p["y"] // CEL) + ddy), ()):
                        if math.hypot(nx - p["x"], ny - p["y"]) > straal:
                            continue
                        dagen = (besl[k]["datum"] - p["datum"]).days
                        if -3 * 365 <= dagen <= 365 and (beste is None or dagen < beste[1]):
                            beste = (k, dagen)
            if beste:
                treffers[p["id"]] = beste
        return treffers

    koppeling = []
    tijd = {}
    for onderw, filt in (("alle", lambda d: True), ("stikstof", lambda d: d["onderwerp"] == "stikstof"),
                         ("niet stikstof", lambda d: d["onderwerp"] != "stikstof")):
        vul(filt)
        echt_idx = index()
        schuif_idx = [index(1000 * math.cos(math.radians(h)), 1000 * math.sin(math.radians(h))) for h in range(0, 360, 45)]
        for straal in (100, 250):
            echt = koppel(echt_idx, straal)
            kans = [koppel(ix, straal) for ix in schuif_idx]
            if straal == 250 and onderw == "alle":
                tijd = echt
            for jaar in sorted({p["jaar"] for p in proj}):
                for ring in sorted({p["ring"] for p in proj}):
                    ids = [p["id"] for p in proj if p["jaar"] == jaar and p["ring"] == ring]
                    if not ids:
                        continue
                    koppeling.append({"onderwerp": onderw, "straal": straal, "jaar": jaar, "ring": ring, "projecten": len(ids),
                                      "treffers": sum(i in echt for i in ids),
                                      "kans": sum(sum(i in k for i in ids) for k in kans) / len(kans)})
    vul(lambda d: True)
    ## een maat als die van het vergunningenspoor: een stikstofbesluit (provincie of Rijk, stikstof in
    ## titel of tekstvlag, precieze locatie) binnen 500 m, zonder tekstfilter en zonder uitsluiting
    ## van standaardpunten, met hetzelfde kansniveau; het venster is hier drie jaar voor tot een jaar
    ## na de BAG-vergunning (projecten.py nam drie jaar om de aanvraag)
    stik = {}
    for k, d in besl.items():
        if d["org"] in ("provincie", "ministerie") and (d["vlag"] & STIKSTOF_VLAG or STIKSTOF_TITEL.search(d["titel"])):
            pts = [(l[2], l[3]) for l in d["loc"] if l[0] in ("Punt", "Adres", "Perceel") or (l[0] == "Vlak" and (l[4] or 0) <= 1000)]
            if pts:
                stik[k] = pts
    bewaar = dict(punten)
    punten.clear(); punten.update(stik)
    oud_echt = koppel(index(), 500)
    oud_kans = [koppel(index(1000 * math.cos(math.radians(h)), 1000 * math.sin(math.radians(h))), 500) for h in range(0, 360, 45)]
    punten.clear(); punten.update(bewaar)
    oude_maat = {"stikstofbesluiten": len(stik), "projecten": len(proj), "treffers": len(oud_echt),
                 "kans": sum(len(k) for k in oud_kans) / len(oud_kans)}
    print("oude maat, stikstofbesluit binnen 500 m:", oude_maat)
    fase_tijd = Counter(besl[v[0]]["fase"] for v in tijd.values())
    dagen = sorted(v[1] for v in tijd.values())
    q = lambda f: dagen[int(f * (len(dagen) - 1))] if dagen else None
    res = {"meta": {"natuurbesluiten": len(besl), "woningnieuwbouw": sum(d["woning_nieuw"] for d in besl.values()),
                    "woningnieuwbouw_met_punt": len(punten), "standaardpunten": len(std), "projecten": len(proj)},
           "per_jaar": per_jaar, "per_provincie": per_provincie, "koppeling": koppeling, "oude_maat": oude_maat,
           "fase_vroegste_250m": dict(fase_tijd),
           "tijd_250m": {"n": len(dagen), "p25": q(.25), "mediaan": q(.5), "p75": q(.75),
                         "aandeel_voor_vergunning": (sum(v < 0 for v in dagen) / len(dagen)) if dagen else None}}
    uit.write_text(json.dumps(res, ensure_ascii=False, indent=1), encoding="utf-8")
    print(json.dumps(res["meta"], ensure_ascii=False))
    print("tijd:", res["tijd_250m"])


if __name__ == "__main__":
    main()
