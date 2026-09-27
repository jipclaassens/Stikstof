"""Typering van zaken uit hun titels, als aanvulling op de soort in zaken.csv.

Gebruik: py -3.13 R/09a_titels.py [datamap vergunningen] [uitvoerbestand]
         standaard VERGUNNINGEN_DATA en R/_derived/aanvragen_titels.csv

Twee vlaggen per zaak, uit alle titels van de publicaties van die zaak:

streng     alleen voor soort nieuwbouw_woning. Die soort
           (vergunningen/pipeline/analyse_export.py) neemt elke titel met
           een getal plus woningen mee en gaat voor op sloop en verbouw.
           Daardoor vallen ook verbouwen tot appartementen, splitsen,
           verduurzamen van 117 woningen en slopen van woningen erin: in een
           steekproef van 1.500 zaken noemde ruim een vijfde verbouw,
           transformatie of wijzigen. Streng is: een bouwwoord met een
           woonwoord (het eerste deel van de oorspronkelijke regex, zonder de
           route via alleen een aantal) en geen woord voor verbouw,
           splitsing, transformatie, renovatie of uitbreiding.
tijdelijk  tijdelijke woningen, mantelzorg, tiny houses, recreatie.
veestal    een stal of ander gebouw van een veehouderij (stal, ligboxenstal,
           mestopslag, luchtwasser, veehouderij): de activiteit die bij een
           Natura 2000-toets het eerst geraakt wordt, de positieve controle.

Leest vergunningen.sqlite alleen (immutable), dus ook vanaf de OneDrive-map
zonder journaalbestanden. Schrijft zaak_id;streng;tijdelijk;veestal voor elke
zaak in zaken.csv; 09_aanvragen.R leest dat bestand.
"""
import csv
import os
import re
import sqlite3
import sys
import time
from collections import defaultdict
from pathlib import Path

hier = Path(__file__).resolve().parent
data = Path(sys.argv[1]) if len(sys.argv) > 1 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
uit = Path(sys.argv[2]) if len(sys.argv) > 2 else hier / "_derived" / "aanvragen_titels.csv"

NIEUWBOUW = re.compile(
    r"(bouwen|oprichten|realiseren|nieuwbouw) (van )?(een |[0-9]+ |twee |drie |vier |vijf |zes |acht |tien )?"
    r"(nieuwe |vrijstaande |levensloopbestendige |sociale |tijdelijke )?(woning|woningen|woonhuis|appartement|appartementen|"
    r"studentenwoning|wooneenhe|woonzorg|woongebouw|woontoren|woonblok|villa|bungalow|twee-onder-een-kap|rijwoning|"
    r"eengezinswoning|meergezins|flexwoning|mantelzorgwoning)|nieuwbouwwoning|woningbouw|nieuwbouw van|nieuwbouwproject", re.I)
NIET = re.compile(r"verbouw|splits|transform|ombouw|omzetten|omvormen|verduurzam|renov|uitbreid|vergrot|"
                  r"wijzigen van (het )?gebruik|gebruiken als|functiewijziging|bewoning van|inpandig|interne", re.I)
TIJDELIJK = re.compile(r"tijdelijk|mantelzorg|flexwoning|tiny house|woonunit|chalet|recreatie", re.I)
VEESTAL = re.compile(r"(?<![a-z])(stal|stallen)(?![a-z])|ligboxenstal|melkveestal|pluimveestal|varkensstal|rundveestal|"
                     r"geitenstal|kalverstal|jongveestal|schapenstal|vleeskuiken|legkip|veehouderij|luchtwasser|"
                     r"mestsilo|mestopslag|mestkelder|mestbassin|mestplaat|mestzak", re.I)


def main():
    soort = {}
    with open(data / "analyse" / "zaken.csv", encoding="utf-8") as f:
        for rij in csv.DictReader(f, delimiter=";"):
            soort[int(rij["zaak_id"])] = rij["soort"]
    db = "file:" + (data / "vergunningen.sqlite").as_posix() + "?immutable=1"
    con = sqlite3.connect(db, uri=True)
    t0 = time.time()
    titels = defaultdict(list)
    for zid, titel in con.execute("SELECT pz.zaak_id, p.titel FROM publicatie_zaak pz "
                                  "JOIN publicatie p ON p.identifier = pz.identifier"):
        if zid in soort:
            titels[zid].append(titel or "")
    uit.parent.mkdir(parents=True, exist_ok=True)
    n = {"zaken": 0, "streng": 0, "tijdelijk": 0, "veestal": 0, "zonder_titel": 0}
    with open(uit, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["zaak_id", "streng", "tijdelijk", "veestal"])
        for zid, s in soort.items():
            t = " | ".join(titels.get(zid, []))
            if not t:
                n["zonder_titel"] += 1
            st = int(s == "nieuwbouw_woning" and bool(NIEUWBOUW.search(t)) and not NIET.search(t))
            tj = int(s == "nieuwbouw_woning" and bool(TIJDELIJK.search(t)))
            vs = int(bool(VEESTAL.search(t)))
            n["zaken"] += 1; n["streng"] += st; n["tijdelijk"] += tj; n["veestal"] += vs
            w.writerow([zid, st, tj, vs])
    print(f"{n['zaken']} zaken in {time.time() - t0:.0f} s; nieuwbouw streng {n['streng']}, tijdelijk {n['tijdelijk']}, "
          f"veestal {n['veestal']}, zonder titel {n['zonder_titel']}; geschreven: {uit}")


if __name__ == "__main__":
    main()
