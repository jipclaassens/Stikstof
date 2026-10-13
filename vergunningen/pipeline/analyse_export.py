"""Analysebestanden uit de database, als invoer voor analyse.R.

Gebruik: py -3.13 analyse_export.py [uitvoermap]     standaard <datamap>/analyse
Schrijft (puntkomma, utf-8):
  zaken.csv       een rij per zaak van een gemeente met een aanvraagpublicatie: datum, kwartaal, afstand en klasse tot
                  stikstofgevoelig habitat, nabij (binnen 2.500 m), uitkomst, doorlooptijd, verlengingen, bouw (label
                  bouwen of bouwwoord in een titel van de zaak), uitgebreid (een ontwerpbesluit in de zaak), woningen
                  (grootste aantal in de titels), aantal publicaties
  panel.csv       gemeente x kwartaal x nabij: aanvragen, uitkomsten, mediaan doorlooptijd, verlengingen, en dezelfde
                  tellingen voor bouwzaken
  uitspraken.csv  een rij per uitspraak met tekst: datum, instantie, procedure, object, gemeente, besluitdatum, dagen van
                  besluit tot uitspraak, uitkomst, stikstof, Natura 2000, hoger beroep, voorlopige voorziening
"""
import csv
import datetime as dt
import re
import sqlite3
import statistics
import sys
from collections import defaultdict
from pathlib import Path

from config import DATA_DIR, DB_PAD

NABIJ_M = 2500
_BOUW = re.compile(r"bouw|woning|appartement|dakkapel|uitbouw|aanbouw|bijgebouw|schuur|garage|carport|loods|stal\b|stallen|bedrijfspand|bedrijfshal", re.I)

# projecttype uit de titels van de zaak, in volgorde van voorrang; nieuwbouw van woningen is de doelgroep, klein werk de placebo.
# Het bouwwoord staat na een woordgrens (28 september 2026, inzichten.md hoofdstuk 36 en 37): zonder die grens telden verbouwen,
# uitbouwen, aanbouwen, opbouwen en ombouwen van een woning als nieuwbouw. Herbouwen, nieuwbouwen, terugbouwen, afbouwen en
# wederopbouwen blijven nieuwbouw.
BOUWWOORD = r"\b(?:her|nieuw|terug|af|wederop)?(?:bouwen|oprichten|realiseren|nieuwbouw)"
SOORTEN = [
    ("nieuwbouw_woning", re.compile(BOUWWOORD + r" (van )?(een |[0-9]+ |twee |drie |vier |vijf |zes |acht |tien )?"
                                    r"(nieuwe |vrijstaande |levensloopbestendige |sociale |tijdelijke )?(woning|woningen|woonhuis|appartement|appartementen|"
                                    r"studentenwoning|wooneenhe|woonzorg|woongebouw|woontoren|woonblok|villa|bungalow|twee-onder-een-kap|rijwoning|"
                                    r"eengezinswoning|meergezins|flexwoning|mantelzorgwoning)|nieuwbouwwoning|woningbouw|nieuwbouw van|nieuwbouwproject|"
                                    r"[0-9]+ (woningen|appartementen|studentenwoningen|wooneenheden|studio's)", re.I)),
    ("nieuwbouw_overig", re.compile(BOUWWOORD + r" (van )?(een |[0-9]+ )?(bedrijfs|kantoor|loods|stal|hal\b|school|winkel|hotel|"
                                    r"supermarkt|zorg|sporthal|kerk|distributiecentrum|fabriek|bedrijfsgebouw|bedrijfspand|bedrijfshal|kas\b|kassen|"
                                    r"melkveestal|pluimveestal|varkensstal|rundveestal|werktuigenberging|zonnepark|windturbine|windmolen)", re.I)),
    ("klein_verbouw", re.compile(r"dakkapel|aanbouw|uitbouw|dakopbouw|opbouw|bijgebouw|schuur|berging|garage|carport|kozijn|gevel|erfafscheiding|"
                                 r"schutting|zonnepanelen|warmtepomp|airco|veranda|overkapping|tuinhuis|dakterras|verbouw|wijzigen|vergroten|"
                                 r"plaatsen van|vervangen|renover|isoleren|isolatie|dakraam|dakkapellen|balkon|zwembad|reclame|uitrit|inrit|"
                                 r"splitsen|splitsing|verbouwen|interne|kamerverhuur|bed and breakfast|b&b", re.I)),
    ("sloop_kap", re.compile(r"slopen|sloop|kappen|kap van|vellen|rooien|houtopstand", re.I)),
]


def projectsoort(titels: str) -> str:
    t = titels or ""
    for naam, rx in SOORTEN:
        if rx.search(t):
            return naam
    return "overig"
_WONINGEN = re.compile(r"(\d{1,4}) (?:nieuwe |sociale |grondgebonden |vrijstaande )?(woningen|appartementen|studentenwoningen|wooneenheden|studio's|huurwoningen|koopwoningen)", re.I)


def kwartaal(datum):
    d = dt.date.fromisoformat(datum)
    return f"{d.year}K{(d.month - 1) // 3 + 1}"


def main(argv):
    uit = Path(argv[0]) if argv else DATA_DIR / "analyse"
    uit.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(DB_PAD, timeout=120)
    kol = [r[1] for r in conn.execute("PRAGMA table_info(zaak)")]
    dist = "z.dist2n2000_m" if "dist2n2000_m" in kol else "NULL"
    kl = "z.n2000_klasse" if "n2000_klasse" in kol else "NULL"
    extra_kol = ["dist_overbelast_m", "overbelast_klasse", "ovb_max_1km", "ovb_max_5km", "ovb_max_25km"]
    extra_sel = ", ".join(f"z.{k}" if k in kol else "NULL" for k in extra_kol)
    rijen = conn.execute(f"""
        SELECT z.zaak_id, z.creator, z.datum_aanvraag, z.uitkomst, z.doorlooptijd_dagen, z.n_verlengingen, {dist}, {kl},
               z.n_publicaties, z.datum_uitkomst,
               (SELECT group_concat(p.titel, ' | ') FROM publicatie_zaak pz JOIN publicatie p ON p.identifier = pz.identifier WHERE pz.zaak_id = z.zaak_id),
               EXISTS (SELECT 1 FROM publicatie_zaak pz JOIN publicatie_activiteit a ON a.identifier = pz.identifier WHERE pz.zaak_id = z.zaak_id AND a.activiteit = 'bouwen'),
               EXISTS (SELECT 1 FROM publicatie_zaak pz JOIN publicatie p ON p.identifier = pz.identifier WHERE pz.zaak_id = z.zaak_id AND p.fase_titel = 'ontwerp'),
               {extra_sel}
        FROM zaak z WHERE z.organisatietype = 'gemeente' AND z.datum_aanvraag IS NOT NULL""").fetchall()
    print(f"{len(rijen)} zaken met aanvraag", flush=True)
    panel = defaultdict(lambda: defaultdict(float))
    doorloop = defaultdict(list)
    with open(uit / "zaken.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["zaak_id", "gemeente", "datum_aanvraag", "kwartaal", "dist_m", "klasse", "nabij", "uitkomst", "verleend", "geweigerd",
                    "ingetrokken", "vergunningvrij", "besluit_onbekend", "geen", "doorlooptijd", "verlengingen", "bouw", "uitgebreid",
                    "woningen", "n_publicaties", "datum_uitkomst", "soort", "dist_overbelast_m", "overbelast_klasse", "nabij_overbelast",
                    "ovb_max_1km", "ovb_max_5km", "ovb_max_25km"])
        for (zid, gem, datum, uitk, dl, nv, d, klasse, npub, datum_uit, titels, bouw_label, uitgebreid, d_ovb, kl_ovb, o1, o5, o25) in rijen:
            kw = kwartaal(datum)
            nabij = 1 if d is not None and d <= NABIJ_M else 0
            bouw = 1 if bouw_label or _BOUW.search(titels or "") else 0
            won = max((int(m.group(1)) for m in _WONINGEN.finditer(titels or "") if int(m.group(1)) < 10000), default="")
            u = uitk or "geen"
            vl = dict(verleend=int(u == "verleend"), geweigerd=int(u == "geweigerd"), ingetrokken=int(u == "ingetrokken"),
                      vergunningvrij=int(u == "vergunningvrij"), besluit_onbekend=int(u == "besluit_onbekend"), geen=int(u == "geen"))
            w.writerow([zid, gem, datum, kw, d if d is not None else "", klasse or "onbekend", nabij, u, vl["verleend"], vl["geweigerd"],
                        vl["ingetrokken"], vl["vergunningvrij"], vl["besluit_onbekend"], vl["geen"], dl if dl is not None else "",
                        nv or 0, bouw, int(bool(uitgebreid)), won, npub, datum_uit or "", projectsoort(titels),
                        d_ovb if d_ovb is not None else "", kl_ovb or "", (1 if d_ovb is not None and d_ovb <= NABIJ_M else 0) if d_ovb is not None else "",
                        o1 if o1 is not None else "", o5 if o5 is not None else "", o25 if o25 is not None else ""])
            for voorvoegsel, mee in (("", True), ("bouw_", bouw)):
                if not mee:
                    continue
                c = panel[(gem, kw, nabij)]
                c[voorvoegsel + "aanvragen"] += 1
                c[voorvoegsel + "verlengingen"] += nv or 0
                for k, v in vl.items():
                    c[voorvoegsel + k] += v
                if u == "verleend" and dl is not None:
                    doorloop[(gem, kw, nabij, voorvoegsel)].append(dl)
    kolommen = ["aanvragen", "verleend", "geweigerd", "ingetrokken", "vergunningvrij", "besluit_onbekend", "geen", "verlengingen"]
    with open(uit / "panel.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["gemeente", "kwartaal", "nabij"] + kolommen + ["mediaan_dagen"] + ["bouw_" + k for k in kolommen] + ["bouw_mediaan_dagen"])
        for (gem, kw, nabij) in sorted(panel):
            c = panel[(gem, kw, nabij)]
            rij = [gem, kw, nabij] + [int(c[k]) for k in kolommen]
            d = doorloop.get((gem, kw, nabij, ""), [])
            rij.append(round(statistics.median(d)) if d else "")
            rij += [int(c["bouw_" + k]) for k in kolommen]
            d = doorloop.get((gem, kw, nabij, "bouw_"), [])
            rij.append(round(statistics.median(d)) if d else "")
            w.writerow(rij)
    # uitspraken
    tabellen = {r[0] for r in conn.execute("SELECT name FROM sqlite_master WHERE type='table'")}
    n_uit = 0
    with open(uit / "uitspraken.csv", "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["ecli", "datum_uitspraak", "jaar", "instantie", "procedure", "object", "bouw", "woningen", "gemeente", "bestuursorgaan",
                    "besluitdatum", "dagen_besluit_uitspraak", "uitkomst", "stikstof", "natura2000", "hoger_beroep", "voorlopige_voorziening"])
        if "uitspraak" in tabellen:
            for r in conn.execute("""SELECT ecli, datum_uitspraak, instantie, procedure, object, bouw, woningen, creator, bestuursorgaan, besluitdatum,
                                     uitkomst, stikstof, natura2000, hoger_beroep, voorlopige_voorziening FROM uitspraak WHERE tekst_pad IS NOT NULL"""):
                dagen = ""
                if r[1] and r[9]:
                    try:
                        dagen = (dt.date.fromisoformat(r[1]) - dt.date.fromisoformat(r[9])).days
                    except ValueError:
                        dagen = ""
                w.writerow([r[0], r[1], (r[1] or "")[:4], r[2], r[3], r[4], r[5], r[6] or "", r[7] or "", r[8] or "", r[9] or "", dagen,
                            r[10], r[11], r[12], r[13], r[14]])
                n_uit += 1
    print(f"geschreven naar {uit}: zaken {len(rijen)}, panelcellen {len(panel)}, uitspraken {n_uit}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
