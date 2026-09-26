"""Meet de stroom natuur: natuurvergunningen en ontheffingen van provincies en Rijk, en de gemeentelijke titels met een
Natura 2000-activiteit sinds de Omgevingswet.

Draait op de eigen database van de stroom (VERGUNNINGEN_DB, standaard natuur\\natuurvergunningen.sqlite, gevuld met
laad.py onder dezelfde omgevingsvariabelen). Uit de titel komen twee indelingen:
- soort toestemming: gebied (Natura 2000, gebiedsbescherming, stikstof, natuurvergunning, Nbw- of Wnb-vergunning),
  soorten (ontheffing soortenbescherming, flora en fauna, Ff-wet), houtopstanden (kap, herplant, velling), overig;
- object: woningbouw, agrarisch, industrie en energie, infrastructuur, recreatie, overig.
De fase komt uit fase_titel (fase.py), dus aanvraag, ontwerp, verleend, geweigerd, ingetrokken, besluit, onbepaald.

Uitvoer in de map van de database: natuur_per_gezag_jaar.csv (creator, jaar, soort, fase, aantal),
natuur_per_kwartaal_object.csv (kwartaal, soort, object, fase, aantal) en de hoofdtellingen op het scherm.
Gebruik: py -3.13 natuur_meet.py
"""
import csv
import re
import sqlite3
import sys
from collections import Counter

from config import DB_PAD

# Volgorde telt: eerst de specifieke woorden, dan de Wet natuurbescherming zonder nadere aanduiding (bij provincies
# meestal een gebiedsvergunning voor een veehouderij, maar niet altijd), en wat overblijft is geen natuurbesluit (de
# provinciale rubriek omgevingsvergunning sinds 2024 bevat ook wegen, BRZO-bedrijven en ontgrondingen).
SOORT = [
    ("houtopstanden", re.compile(r"\b(houtopstand(en)?|kap(pen|vergunning|melding)?|herplant(plicht)?|vell(en|ing))\b", re.I)),
    ("soorten", re.compile(r"\b(ontheffing(en)?|soorten(bescherming)?|flora|fauna|ff-?wet|fauna-activiteit|vleermuis|huismus|gierzwaluw|das|rugstreeppad|steenmarter|nesten|rustplaatsen)\b", re.I)),
    ("gebied", re.compile(r"natura|gebiedsbescherming|stikstof|natuurvergunning|\bnbw\b|nb-wet|natuurbeschermingswet|\bwnb\b|passende beoordeling|salder", re.I)),
    ("wnb_onbepaald", re.compile(r"natuurbescherming", re.I)),
]
OBJECT = [
    ("woningbouw", re.compile(r"\b(woning(en|bouw)?|woonwijk|appartement(en)?|wooneenhe|nieuwbouwwijk|woontoren|huisvesting)\b", re.I)),
    ("agrarisch", re.compile(r"\b(veehouderij|melkvee|rundvee|varkens?|pluimvee|kippen|geiten|kalveren|stal(len)?|agrarisch|melkrundvee|vleeskuiken|mest|paarden|akkerbouw|teelt)\b", re.I)),
    ("industrie en energie", re.compile(r"\b(windpark|windturbine|zonnepark|zonneweide|energie|fabriek|industrie|haven|raffinaderij|centrale|biovergist|waterstof|datacenter|bedrijventerrein|bedrijf)\b", re.I)),
    ("infrastructuur", re.compile(r"\b(weg|snelweg|rijksweg|spoor|brug|dijk|kering|kabel|leiding|hoogspanning|tracé|trace|luchthaven|vliegveld|baggeren|vaargeul|sluis)\b", re.I)),
    ("recreatie", re.compile(r"\b(recreatie|evenement|festival|camping|vakantiepark|jachthaven|strand|golfbaan)\b", re.I)),
]


def indeel(titel, regels, anders):
    for naam, rx in regels:
        if rx.search(titel or ""):
            return naam
    return anders


def main(argv):
    conn = sqlite3.connect(DB_PAD, timeout=900)
    n = conn.execute("SELECT count(*) FROM publicatie").fetchone()[0]
    print(f"{n} publicaties in {DB_PAD}")
    per_gezag, per_kw = Counter(), Counter()
    org, soorten, objecten, fasen, met_loc = Counter(), Counter(), Counter(), Counter(), Counter()
    q = """SELECT p.identifier, p.creator, p.organisatietype, p.publicatienaam, p.titel, p.available, p.fase_titel,
                  (SELECT count(*) FROM publicatie_locatie l WHERE l.identifier = p.identifier AND l.x IS NOT NULL)
           FROM publicatie p"""
    for ident, creator, orgtype, pubnaam, titel, available, fase, nloc in conn.execute(q):
        soort = indeel(titel, SOORT, "niet_natuur")
        obj = indeel(titel, OBJECT, "overig")
        jaar = (available or "")[:4]
        kw = f"{jaar}K{(int(available[5:7]) - 1) // 3 + 1}" if available and len(available) >= 7 else ""
        per_gezag[(creator, orgtype, jaar, soort, fase)] += 1
        per_kw[(kw, orgtype, soort, obj, fase)] += 1
        org[orgtype] += 1
        soorten[soort] += 1
        objecten[obj] += 1
        fasen[fase] += 1
        met_loc[nloc > 0] += 1
    uit = DB_PAD.parent
    with open(uit / "natuur_per_gezag_jaar.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["creator", "organisatietype", "jaar", "soort", "fase", "aantal"])
        for k, v in sorted(per_gezag.items(), key=lambda kv: tuple(str(x) for x in kv[0])):
            w.writerow(list(k) + [v])
    with open(uit / "natuur_per_kwartaal_object.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["kwartaal", "organisatietype", "soort", "object", "fase", "aantal"])
        for k, v in sorted(per_kw.items(), key=lambda kv: tuple(str(x) for x in kv[0])):
            w.writerow(list(k) + [v])
    print("organisatietype:", dict(org.most_common()))
    print("soort:", dict(soorten.most_common()))
    print("object:", dict(objecten.most_common()))
    print("fase:", dict(fasen.most_common()))
    print("met locatie:", dict(met_loc))
    for s in ("gebied", "wnb_onbepaald", "soorten"):
        print(f"per jaar (provincie, {s}):")
        pj = Counter()
        for (creator, orgtype, jaar, soort, fase), v in per_gezag.items():
            if orgtype == "provincie" and soort == s:
                pj[(jaar, fase)] += v
        for jaar in sorted({j for j, _ in pj}):
            print(" ", jaar, sum(v for (j, f), v in pj.items() if j == jaar),
                  {f: pj[(jaar, f)] for f in ("aanvraag", "ontwerp", "verleend", "geweigerd", "ingetrokken", "besluit", "onbepaald") if pj[(jaar, f)]})
    # natuurbesluiten die in de titel een woningbouwproject noemen, per jaar en soort toestemming: een ondergrens voor het
    # aantal woningbouwprojecten met een eigen natuurvergunning of soortenontheffing (veel titels noemen alleen adres en bedrijf)
    wb = Counter()
    for (kw, orgtype, soort, obj, fase), v in per_kw.items():
        if obj == "woningbouw" and orgtype == "provincie":
            wb[(kw[:4], soort)] += v
    with open(uit / "natuur_woningbouw_per_jaar.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["jaar", "gebied", "wnb_onbepaald", "soorten", "houtopstanden", "niet_natuur"])
        for j in sorted({j for j, _ in wb}):
            w.writerow([j] + [wb[(j, s)] for s in ("gebied", "wnb_onbepaald", "soorten", "houtopstanden", "niet_natuur")])
    print("per provincie (gebied plus wnb_onbepaald, alle jaren):")
    pp = Counter()
    for (creator, orgtype, jaar, soort, fase), v in per_gezag.items():
        if orgtype == "provincie" and soort in ("gebied", "wnb_onbepaald"):
            pp[creator] += v
    print(" ", dict(pp.most_common()))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
