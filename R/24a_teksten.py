"""Natuur en andere redenen in de uitgebreide procedure: vlaggen per zaak uit titel, samenvatting en volledige tekst.

Gebruik: py -3.13 R/24a_teksten.py samenvatting <uitvoer.txt> [datamap]
         py -3.13 R/24a_teksten.py teksten <uitvoer.csv> [teksten.sqlite]

samenvatting  telt de zaken met een uitgebreide procedure (analyse/zaken.csv, uitgebreid = 1) waarvan een publicatie in
              titel of samenvatting natuur of stikstof noemt (patroon NATUUR hieronder), uit vergunningen.sqlite, alleen
              lezend; datamap standaard VERGUNNINGEN_DATA
teksten       leest de volledige teksten van de gerichte oogst (vergunningen/pipeline/teksten.py; tabel tekst in
              C:/LocalData/Stikstof/teksten/teksten.sqlite of TEKSTEN_DIR, alleen lezend) en schrijft per zaak of een van
              haar teksten het volgende noemt:
  natura      Natura 2000, stikstof(depositie), AERIUS, passende beoordeling, voortoets, (Wet) natuurbescherming,
              natuurvergunning of gebiedsbescherming: de natuurtoets voor gebieden, waaronder stikstof
  stikstof    alleen stikstof of AERIUS
  soorten     flora en fauna, soortenbescherming of ecologisch onderzoek: de natuurtoets voor soorten
  vvgb        een verklaring van geen bedenkingen, en van wie: vvgb_raad (gemeenteraad) of vvgb_prov (gedeputeerde
              staten of de provincie, zoals bij een aangehaakte natuurtoets)
  afwijken    afwijken van het bestemmings- of omgevingsplan, projectafwijkingsbesluit of artikel 2.12 Wabo
  milieu      milieu, inrichting of milieueffectrapportage
  brandveilig brandveilig gebruik
  woningen    het grootste aantal woningen, appartementen of wooneenheden dat genoemd wordt (leeg als geen)
Een tekst is de platte tekst van de XML, dus ook de standaardzinnen van een gemeente tellen mee; de vlaggen zijn
daarom per gemeente te vergelijken, niet als absolute aandelen.
"""
import csv
import os
import re
import sqlite3
import sys
import zlib
from pathlib import Path

NATUUR = re.compile(r"natura\s*2000|stikstof|aerius|natuurbescherming|\bwnb\b|natuurvergunning|passende beoordeling|voortoets|"
                    r"geen bedenkingen|\bvvgb\b|gebiedsbescherming|flora.en.fauna|soortenbescherming|ecolog", re.I)
V = {
    "natura": re.compile(r"natura[\s-]*2000|stikstof|aerius|passende beoordeling|voortoets|natuurbescherming|"
                         r"\bwnb\b|natuurvergunning|gebiedsbescherming", re.I),
    "stikstof": re.compile(r"stikstof|aerius", re.I),
    "soorten": re.compile(r"flora[\s-]*(en|&)[\s-]*fauna|soortenbescherming|ecologisch", re.I),
    "vvgb": re.compile(r"verklaring van geen bedenkingen|\bvvgb\b", re.I),
    "vvgb_raad": re.compile(r"geen bedenkingen.{0,150}(gemeenteraad|de raad\b)|(gemeenteraad|de raad\b).{0,150}geen bedenkingen", re.I),
    "vvgb_prov": re.compile(r"geen bedenkingen.{0,150}(gedeputeerde staten|provincie)|(gedeputeerde staten|provincie).{0,150}geen bedenkingen", re.I),
    "afwijken": re.compile(r"afwijk\w*.{0,80}(bestemmingsplan|omgevingsplan)|projectafwijkingsbesluit|art(ikel|\.)\s*2\.12|buitenplans", re.I),
    "milieu": re.compile(r"\bmilieu|inrichting|m\.e\.r\.|milieueffect", re.I),
    "brandveilig": re.compile(r"brandveilig", re.I),
}
AANTAL = re.compile(r"\b(\d{1,4})\s*(?:\w+\s){0,3}?(woningen|appartementen|wooneenheden|studentenkamers|studio'?s)\b", re.I)


def samenvatting(uit, data):
    zaken = set()
    with open(data / "analyse" / "zaken.csv", encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            if r["uitgebreid"] == "1":
                zaken.add(int(r["zaak_id"]))
    con = sqlite3.connect(f"file:{(data / 'vergunningen.sqlite').as_posix()}?immutable=1", uri=True)
    publicaties = met_samenvatting = 0
    natuur = set()
    for zid, titel, abstract in con.execute("""SELECT pz.zaak_id, p.titel, p.abstract
                                               FROM publicatie_zaak pz JOIN publicatie p ON p.identifier = pz.identifier"""):
        if zid not in zaken:
            continue
        publicaties += 1
        met_samenvatting += bool(abstract)
        if NATUUR.search((titel or "") + " " + (abstract or "")):
            natuur.add(zid)
    con.close()
    regels = [f"zaken met een uitgebreide procedure: {len(zaken)}",
              f"publicaties van die zaken: {publicaties}, met samenvatting: {met_samenvatting}",
              f"zaken waarvan titel of samenvatting natuur of stikstof noemt: {len(natuur)}"]
    uit.write_text("\n".join(regels) + "\n", encoding="utf-8")
    print("\n".join(regels))


def teksten(uit, bron):
    db = sqlite3.connect(f"file:{bron.as_posix()}?mode=ro", uri=True)
    per = {}
    n = 0
    for zid, blob in db.execute("SELECT zaak_id, tekst FROM tekst WHERE status = 200 AND tekst IS NOT NULL"):
        t = zlib.decompress(blob).decode("utf-8", errors="replace")
        z = per.setdefault(zid, {k: 0 for k in V} | {"teksten": 0, "woningen": None})
        z["teksten"] += 1
        for k, r in V.items():
            if not z[k] and r.search(t):
                z[k] = 1
        for m in AANTAL.finditer(t):
            a = int(m.group(1))
            if 0 < a < 5000 and (z["woningen"] is None or a > z["woningen"]):
                z["woningen"] = a
        n += 1
    db.close()
    with open(uit, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["zaak_id", "teksten", *V.keys(), "woningen"])
        for zid, z in sorted(per.items()):
            w.writerow([zid, z["teksten"], *(z[k] for k in V), "" if z["woningen"] is None else z["woningen"]])
    print(f"{n} teksten, {len(per)} zaken: {uit}")


if __name__ == "__main__":
    modus, uit = sys.argv[1], Path(sys.argv[2])
    if modus == "samenvatting":
        samenvatting(uit, Path(sys.argv[3]) if len(sys.argv) > 3 else Path(os.environ.get(
            "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen")))
    else:
        teksten(uit, Path(sys.argv[3]) if len(sys.argv) > 3 else
                Path(os.environ.get("TEKSTEN_DIR", r"C:\LocalData\Stikstof\teksten")) / "teksten.sqlite")
