"""Derde stroom: uitspraken van de bestuursrechter over omgevingsrecht, uit Rechtspraak Open Data.

Bezwaar en beroep staan niet in de bekendmakingen; beroep staat wel in de gepubliceerde rechtspraak (per jaar 1.900 tot
2.500 uitspraken in het rechtsgebied omgevingsrecht, vrijwel alle van de Afdeling bestuursrechtspraak van de Raad van
State en een selectie van de rechtbanken). Per uitspraak: instantie, datum, procedure (bodemzaak, hoger beroep,
voorlopige voorziening), de inhoudsindicatie en de volledige tekst. De tekst is geanonimiseerd (adressen als
"[locatie]"), maar het bestuursorgaan, de gemeente, de datum van het bestreden besluit, het soort object
(bestemmingsplan, omgevingsvergunning, natuurvergunning), de uitkomst en de beroepsgronden (stikstof, Natura 2000)
staan er in.

Gebruik: py -3.13 rechtspraak.py index 2010 2026        index per maand (een verzoek per maand)
         py -3.13 rechtspraak.py tekst [2019 2018 ...]    tekst en classificatie van relevante uitspraken, per jaar
         py -3.13 rechtspraak.py koppel                   koppeling aan de zaken op gemeente, besluitdatum en omschrijving
Ruwe antwoorden gaan als gzip naar ruw/rechtspraak. Hoogstens een verzoek per seconde.
"""
import datetime as dt
import gzip
import html
import re
import sqlite3
import sys
import time
import urllib.parse
import urllib.request

from config import DB_PAD, RUW_DIR, USER_AGENT

ZOEK_URL = "https://data.rechtspraak.nl/uitspraken/zoeken"
CONTENT_URL = "https://data.rechtspraak.nl/uitspraken/content"
RECHTSGEBIED = "http://psi.rechtspraak.nl/rechtsgebied#bestuursrecht_omgevingsrecht"
INTERVAL_S = 1.0

SCHEMA = """
CREATE TABLE IF NOT EXISTS uitspraak (
  ecli TEXT PRIMARY KEY, instantie TEXT, datum_uitspraak TEXT, zaaknummer TEXT, titel TEXT, samenvatting TEXT,
  gepubliceerd TEXT, relevant INTEGER, procedure TEXT, soort TEXT, tekst_lengte INTEGER, tekst_pad TEXT,
  bestuursorgaan TEXT, gemeente TEXT, creator TEXT, object TEXT, bouw INTEGER, woningen INTEGER, besluitdatum TEXT,
  uitkomst TEXT, uitkomst_frasen TEXT, stikstof INTEGER, natura2000 INTEGER, hoger_beroep INTEGER,
  voorlopige_voorziening INTEGER, geclassificeerd_op TEXT);
CREATE TABLE IF NOT EXISTS uitspraak_oogst (maand TEXT PRIMARY KEY, verwacht INTEGER, ontvangen INTEGER, status TEXT, tijdstip TEXT);
CREATE TABLE IF NOT EXISTS uitspraak_zaak (ecli TEXT, zaak_id INTEGER, score REAL, toelichting TEXT, PRIMARY KEY (ecli, zaak_id));
CREATE INDEX IF NOT EXISTS ix_uitspraak_datum ON uitspraak (datum_uitspraak);
CREATE INDEX IF NOT EXISTS ix_uitspraak_creator ON uitspraak (creator, besluitdatum);
"""

RELEVANT = re.compile(r"omgevingsvergunning|bouwvergunning|bestemmingsplan|omgevingsplan|projectbesluit|inpassingsplan|"
                      r"natuurvergunning|natura 2000|stikstof|\bbouw|woning|wabo|omgevingswet|wet natuurbescherming|"
                      r"natuurbeschermingswet|aerius|uitwerkingsplan|wijzigingsplan", re.I)
MAANDEN = {m: i + 1 for i, m in enumerate(["januari", "februari", "maart", "april", "mei", "juni", "juli", "augustus",
                                             "september", "oktober", "november", "december"])}
_laatste = 0.0


def _get(url: str) -> str:
    global _laatste
    wacht = INTERVAL_S - (time.monotonic() - _laatste)
    if wacht > 0:
        time.sleep(wacht)
    _laatste = time.monotonic()
    for poging in range(3):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers={"User-Agent": USER_AGENT}), timeout=120) as r:
                return r.read().decode("utf-8-sig", "replace")
        except Exception:
            if poging == 2:
                raise
            time.sleep(5 * (poging + 1))


# ---------- index ----------

def maanden(van_jaar: int, tot_jaar: int):
    for j in range(van_jaar, tot_jaar + 1):
        for m in range(1, 13):
            yield f"{j}-{m:02d}"


def oogst_index(conn, van_jaar: int, tot_jaar: int):
    vandaag = dt.date.today().isoformat()
    for maand in maanden(van_jaar, tot_jaar):
        if maand > vandaag[:7]:
            break
        if conn.execute("SELECT status FROM uitspraak_oogst WHERE maand=?", (maand,)).fetchone() in (("ok",),):
            continue
        j, m = int(maand[:4]), int(maand[5:])
        einde = (dt.date(j + (m == 12), m % 12 + 1, 1) - dt.timedelta(days=1)).isoformat()
        ontvangen, verwacht, offset = 0, None, 0
        while True:
            u = ZOEK_URL + "?" + urllib.parse.urlencode(
                {"max": 1000, "from": offset, "date": [f"{maand}-01", einde], "subject": RECHTSGEBIED, "return": "DOC"}, doseq=True)
            d = _get(u)
            if verwacht is None:
                mm = re.search(r"Aantal gevonden ECLI's: (\d+)", d)
                verwacht = int(mm.group(1)) if mm else 0
            entries = re.findall(r"<entry>(.*?)</entry>", d, re.S)
            for e in entries:
                ecli = re.search(r"<id>([^<]+)</id>", e).group(1)
                titel = html.unescape(re.search(r"<title[^>]*>([^<]*)</title>", e).group(1))
                sam = re.search(r"<summary[^>]*>([^<]*)</summary>", e)
                sam = html.unescape(sam.group(1)) if sam else ""
                upd = re.search(r"<updated>([^<]*)</updated>", e)
                delen = [x.strip() for x in titel.split(",")]
                instantie = delen[1] if len(delen) > 1 else None
                datum = None
                if len(delen) > 2 and re.match(r"\d{2}-\d{2}-\d{4}$", delen[2]):
                    datum = f"{delen[2][6:]}-{delen[2][3:5]}-{delen[2][:2]}"
                zaaknr = ", ".join(delen[3:]) if len(delen) > 3 else None
                caribisch = re.search(r":(OGEA|OGHACMB|OGHNAA)", ecli) is not None  # gerechten van de Caribische delen
                conn.execute("""INSERT OR IGNORE INTO uitspraak (ecli, instantie, datum_uitspraak, zaaknummer, titel, samenvatting,
                                gepubliceerd, relevant) VALUES (?,?,?,?,?,?,?,?)""",
                             (ecli, instantie, datum, zaaknr, titel, sam, upd.group(1) if upd else None,
                              0 if caribisch else (1 if RELEVANT.search(titel + " " + sam) or not sam else 0)))
            ontvangen += len(entries)
            offset += 1000
            if not entries or ontvangen >= verwacht:
                break
        status = "ok" if ontvangen == verwacht else f"onvolledig {ontvangen} van {verwacht}"
        conn.execute("INSERT OR REPLACE INTO uitspraak_oogst VALUES (?,?,?,?,?)",
                     (maand, verwacht, ontvangen, status, dt.datetime.now().isoformat(timespec="seconds")))
        conn.commit()
        print(f"{maand}: {ontvangen} uitspraken ({status})", flush=True)


# ---------- tekst en classificatie ----------

_GEMEENTE = re.compile(r"burgemeester en wethouders van (?:de gemeente )?([A-Z][\w'’.-]+(?:[ -](?:[a-z]{2,3} )?[A-Z][\w'’.-]+)*)")
_RAAD = re.compile(r"de raad van de gemeente ([A-Z][\w'’.-]+(?:[ -](?:[a-z]{2,3} )?[A-Z][\w'’.-]+)*)")
_GS = re.compile(r"gedeputeerde staten van ([A-Z][\w-]+(?: [A-Z][\w-]+)?)", re.I)
_BESLUITDATUM = re.compile(r"[Bb]ij (?:het )?(?:bestreden |primaire )?besluit van (\d{1,2}) (\w+) (\d{4})|"
                           r"besluit van (\d{1,2}) (\w+) (\d{4})|[Oo]p (\d{1,2}) (\w+) (\d{4}) heeft (?:het college|de raad|verweerder)")
_WONINGEN = re.compile(r"(\d{1,4}) (?:nieuwe |vrijstaande |sociale |grondgebonden )?(woningen|appartementen|studentenwoningen|wooneenheden|studio's|huurwoningen|koopwoningen)")
OBJECTEN = [
    ("natuurvergunning", r"natuurvergunning|vergunning op grond van (?:artikel 2\.7 van )?de wet natuurbescherming|wnb-vergunning|"
                         r"natura 2000-vergunning|natura 2000-activiteit|vergunning krachtens artikel 19d|natuurbeschermingswet 1998"),
    ("handhaving", r"last onder dwangsom|last onder bestuursdwang|handhavingsverzoek|handhavend op te treden|invordering"),
    ("bestemmingsplan", r"bestemmingsplan|uitwerkingsplan|wijzigingsplan|inpassingsplan|exploitatieplan"),
    ("omgevingsplan", r"omgevingsplan\b|omgevingsplanactiviteit|bopa\b"),
    ("projectbesluit", r"projectbesluit|projectafwijkingsbesluit|tracébesluit|tracebesluit"),
    ("omgevingsvergunning", r"omgevingsvergunning|bouwvergunning|aanlegvergunning|sloopvergunning|kapvergunning"),
]
_BOUW = re.compile(r"\bbouwen\b|\bbouw van\b|te bouwen|bouwplan|nieuwbouw|woning|appartement|bedrijfsgebouw|bedrijfshal|stal\b|stallen|"
                   r"loods|dakkapel|uitbouw|aanbouw|bijgebouw|windturbine|zonnepark", re.I)
_UITKOMST = [
    ("tussenuitspraak", r"tussenuitspraak|draagt (?:het college|de raad|verweerder) op om binnen|bestuurlijke lus"),
    ("hoger_beroep_gegrond", r"verklaart het hoger beroep gegrond|verklaart de hoger beroepen gegrond"),
    ("hoger_beroep_ongegrond", r"verklaart het hoger beroep ongegrond|verklaart de hoger beroepen ongegrond|bevestigt de aangevallen uitspraak|bevestigt de uitspraak"),
    ("gegrond", r"verklaart (?:het|de) bero(?:ep|epen) (?:van [^;.]{1,120} )?(?:gedeeltelijk )?gegrond|bero(?:ep|epen) (?:is|zijn) (?:gedeeltelijk )?gegrond|"
                r"vernietigt (?:het|de) (?:bestreden )?besluit"),
    ("ongegrond", r"verklaart (?:het|de) bero(?:ep|epen) (?:van [^;.]{1,120} )?ongegrond|bero(?:ep|epen) (?:is|zijn) ongegrond"),
    ("niet_ontvankelijk", r"verklaart (?:het|de) bero(?:ep|epen) (?:van [^;.]{1,120} )?niet-ontvankelijk|bero(?:ep|epen) (?:is|zijn) niet-ontvankelijk"),
    ("vv_toegewezen", r"wijst het verzoek toe|treft de voorlopige voorziening|schorst (?:het|de) (?:bestreden )?besluit"),
    ("vv_afgewezen", r"wijst het verzoek (?:om het treffen van een voorlopige voorziening )?af|wijst de verzoeken af"),
    ("verzoek_afgewezen", r"wijst het verzoek af"),
]
_STIKSTOF = re.compile(r"stikstof|aerius|depositie", re.I)
_N2000 = re.compile(r"natura 2000|habitattype|habitatrichtlijn|vogelrichtlijn|passende beoordeling", re.I)


def parse_document(xml: str):
    def veld(tag):
        m = re.search(rf"<{tag}[^>]*>([^<]*)</{tag}>", xml)
        return html.unescape(m.group(1)).strip() if m else None
    proc = veld("psi:procedure")
    soort = veld("dcterms:type")
    t = re.search(r"<(uitspraak|conclusie)[^>]*>(.*)</\1>", xml, re.S)
    tekst = re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", " ", t.group(2)))) if t else ""
    return proc, soort, tekst


def normaliseer_gemeente(naam: str | None, creators: dict):
    """Naar de creator-naam uit de bekendmakingen; bekende afwijkingen en anders een prefixmatch."""
    if not naam:
        return None
    n = naam.strip().rstrip(".,;")
    vast = {"Den Haag": "'s-Gravenhage", "Den Bosch": "'s-Hertogenbosch", "Nuenen": "Nuenen, Gerwen en Nederwetten",
            "Nuenen c.a.": "Nuenen, Gerwen en Nederwetten", "Bergen": None, "Hengelo": "Hengelo (O)", "Middelburg": "Middelburg (Z)"}
    if n in vast:
        return vast[n]
    if n in creators:
        return n
    for c in creators:
        if c.startswith(n + " (") or c.replace("’", "'") == n.replace("’", "'"):
            return c
    return None


def classificeer(tekst: str, samenvatting: str, procedure: str | None, creators: dict):
    kop = tekst[:6000]
    org, gemeente = None, None
    m = _GEMEENTE.search(kop) or _GEMEENTE.search(samenvatting or "")
    if m:
        org, gemeente = "college", m.group(1)
    else:
        m = _RAAD.search(kop) or _RAAD.search(samenvatting or "")
        if m:
            org, gemeente = "gemeenteraad", m.group(1)
        else:
            m = _GS.search(kop)
            if m:
                org, gemeente = "gedeputeerde staten", m.group(1)
            elif re.search(r"de minister|de staatssecretaris", kop):
                org = "minister"
            elif re.search(r"dagelijks bestuur van (?:het )?(?:water|hoogheem)", kop, re.I):
                org = "waterschap"
    creator = normaliseer_gemeente(gemeente, creators) if org in ("college", "gemeenteraad") else None
    lt = (samenvatting or "").lower() + " " + kop.lower()
    tellingen = {naam: len(re.findall(rx, lt)) for naam, rx in OBJECTEN}
    obj = max(tellingen, key=lambda k: (tellingen[k], -[n for n, _ in OBJECTEN].index(k))) if any(tellingen.values()) else "overig"
    bouw = 1 if _BOUW.search(lt) else 0
    won = max((int(x[0]) for x in _WONINGEN.findall(lt) if int(x[0]) < 10000), default=None)
    besluitdatum = None
    bd = _BESLUITDATUM.search(kop)
    if bd:
        d, mnd, j = [g for g in bd.groups() if g is not None][:3]
        if mnd.lower() in MAANDEN:
            besluitdatum = f"{j}-{MAANDEN[mnd.lower()]:02d}-{int(d):02d}"
    staart = tekst[-4000:].lower()
    frasen = [naam for naam, rx in _UITKOMST if re.search(rx, staart) or (naam == "tussenuitspraak" and re.search(rx, kop.lower()[:2000]))]
    uitkomst = frasen[0] if frasen else "onbekend"
    proc_l = (procedure or "").lower()
    return {
        "bestuursorgaan": org, "gemeente": gemeente, "creator": creator, "object": obj, "bouw": bouw, "woningen": won,
        "besluitdatum": besluitdatum, "uitkomst": uitkomst, "uitkomst_frasen": ";".join(frasen) or None,
        "stikstof": 1 if _STIKSTOF.search(tekst) else 0, "natura2000": 1 if _N2000.search(tekst) else 0,
        "hoger_beroep": 1 if "hoger beroep" in proc_l or re.search(r"hangende het hoger beroep|op het hoger beroep van", kop[:1500].lower()) else 0,
        "voorlopige_voorziening": 1 if "voorlopige" in proc_l or "voorzieningenrechter" in kop[:800].lower() else 0,
    }


def oogst_tekst(conn, jaren):
    creators = {r[0]: 1 for r in conn.execute("SELECT DISTINCT creator FROM publicatie WHERE organisatietype='gemeente'")}
    for jaar in jaren:
        todo = conn.execute("""SELECT ecli, samenvatting FROM uitspraak WHERE relevant=1 AND tekst_pad IS NULL
                               AND datum_uitspraak LIKE ? ORDER BY datum_uitspraak""", (f"{jaar}-%",)).fetchall()
        print(f"{jaar}: {len(todo)} uitspraken op te halen", flush=True)
        doel = RUW_DIR / "rechtspraak" / str(jaar)
        doel.mkdir(parents=True, exist_ok=True)
        for i, (ecli, sam) in enumerate(todo, start=1):
            pad = doel / (ecli.replace(":", "_") + ".xml.gz")
            if pad.exists():  # al eerder opgehaald; niet opnieuw belasten
                with gzip.open(pad, "rt", encoding="utf-8") as f:
                    xml = f.read()
            else:
                try:
                    xml = _get(CONTENT_URL + "?id=" + urllib.parse.quote(ecli))
                except Exception as e:
                    print(f"  {ecli}: fout {str(e)[:100]}", flush=True)
                    continue
                with gzip.open(pad, "wt", encoding="utf-8") as f:
                    f.write(xml)
            proc, soort, tekst = parse_document(xml)
            k = classificeer(tekst, sam, proc, creators)
            # een korte transactie per uitspraak: andere schrijvers (oogst, laden, zaakvorming) wachten anders op dit slot
            schrijf_met_geduld(conn, """UPDATE uitspraak SET procedure=?, soort=?, tekst_lengte=?, tekst_pad=?, bestuursorgaan=?, gemeente=?, creator=?,
                            object=?, bouw=?, woningen=?, besluitdatum=?, uitkomst=?, uitkomst_frasen=?, stikstof=?, natura2000=?,
                            hoger_beroep=?, voorlopige_voorziening=?, geclassificeerd_op=? WHERE ecli=?""",
                               (proc, soort, len(tekst), str(pad), k["bestuursorgaan"], k["gemeente"], k["creator"], k["object"], k["bouw"],
                                k["woningen"], k["besluitdatum"], k["uitkomst"], k["uitkomst_frasen"], k["stikstof"], k["natura2000"],
                                k["hoger_beroep"], k["voorlopige_voorziening"], dt.datetime.now().isoformat(timespec="seconds"), ecli))
            if i % 50 == 0:
                print(f"  {jaar}: {i} van {len(todo)}", flush=True)


def schrijf_met_geduld(conn, sql, params, pogingen=20):
    """Schrijft en commit meteen; bij een vergrendelde database (zaakvorming schrijft minutenlang) wachten en opnieuw."""
    for poging in range(pogingen):
        try:
            conn.execute(sql, params)
            conn.commit()
            return
        except sqlite3.OperationalError as e:
            if "locked" not in str(e).lower() and "busy" not in str(e).lower():
                raise
            conn.rollback()
            time.sleep(30)
    raise sqlite3.OperationalError("database bleef vergrendeld")


def herclassificeer(conn):
    """Classificatie opnieuw over de bewaarde teksten, zonder verzoeken (na een wijziging in de regels)."""
    creators = {r[0]: 1 for r in conn.execute("SELECT DISTINCT creator FROM publicatie WHERE organisatietype='gemeente'")}
    rijen = conn.execute("SELECT ecli, samenvatting, tekst_pad FROM uitspraak WHERE tekst_pad IS NOT NULL").fetchall()
    for i, (ecli, sam, pad) in enumerate(rijen, start=1):
        with gzip.open(pad, "rt", encoding="utf-8") as f:
            xml = f.read()
        proc, soort, tekst = parse_document(xml)
        k = classificeer(tekst, sam, proc, creators)
        conn.execute("""UPDATE uitspraak SET bestuursorgaan=?, gemeente=?, creator=?, object=?, bouw=?, woningen=?, besluitdatum=?,
                        uitkomst=?, uitkomst_frasen=?, stikstof=?, natura2000=?, hoger_beroep=?, voorlopige_voorziening=? WHERE ecli=?""",
                     (k["bestuursorgaan"], k["gemeente"], k["creator"], k["object"], k["bouw"], k["woningen"], k["besluitdatum"],
                      k["uitkomst"], k["uitkomst_frasen"], k["stikstof"], k["natura2000"], k["hoger_beroep"], k["voorlopige_voorziening"], ecli))
    conn.commit()
    print(f"{len(rijen)} uitspraken opnieuw geclassificeerd")


# ---------- koppeling ----------

_WOORD = re.compile(r"[a-zà-ÿ]{5,}")
STOP = set("besluit college burgemeester wethouders gemeente omgevingsvergunning verleend verlenen aanvraag bezwaar beroep "
           "verzoek voorlopige voorziening uitspraak rechtbank afdeling bestuursrechtspraak artikel wabo omgevingswet bouwen "
           "perceel locatie plaatse gelegen appellant appellante wederpartij tegen heeft hebben tussen tussen".split())


def koppel(conn):
    """Kandidaatzaken: zelfde gemeente, besluitpublicatie 0 tot 45 dagen na de besluitdatum uit de uitspraak, en overlap
    van inhoudswoorden tussen de samenvatting en de titels van de zaak. Alleen uitspraken over een omgevingsvergunning."""
    conn.execute("DELETE FROM uitspraak_zaak")
    uitspraken = conn.execute("""SELECT ecli, creator, besluitdatum, samenvatting FROM uitspraak
                                 WHERE object='omgevingsvergunning' AND creator IS NOT NULL AND besluitdatum IS NOT NULL""").fetchall()
    n_gekoppeld = 0
    for ecli, creator, bdatum, sam in uitspraken:
        woorden = {w for w in _WOORD.findall((sam or "").lower()) if w not in STOP}
        if not woorden:
            continue
        d0 = dt.date.fromisoformat(bdatum)
        van, tot = d0.isoformat(), (d0 + dt.timedelta(days=45)).isoformat()
        kandidaten = conn.execute("""SELECT z.zaak_id, group_concat(p.titel, ' | ') FROM zaak z
                                     JOIN publicatie_zaak pz ON pz.zaak_id = z.zaak_id JOIN publicatie p ON p.identifier = pz.identifier
                                     WHERE z.creator=? AND z.datum_uitkomst BETWEEN ? AND ? GROUP BY z.zaak_id""", (creator, van, tot)).fetchall()
        beste = []
        for zid, titels in kandidaten:
            tw = {w for w in _WOORD.findall((titels or "").lower()) if w not in STOP}
            gedeeld = woorden & tw
            if len(gedeeld) >= 2:
                beste.append((len(gedeeld) / (len(woorden) ** 0.5), zid, ",".join(sorted(gedeeld))[:200]))
        beste.sort(reverse=True)
        for score, zid, toel in beste[:3]:
            conn.execute("INSERT OR REPLACE INTO uitspraak_zaak VALUES (?,?,?,?)", (ecli, zid, round(score, 3), toel))
        if beste:
            n_gekoppeld += 1
    conn.commit()
    print(f"{len(uitspraken)} uitspraken over een omgevingsvergunning met gemeente en besluitdatum, {n_gekoppeld} met een kandidaatzaak")


def meet(conn):
    q = lambda s, *a: conn.execute(s, a).fetchall()
    print("== per jaar: uitspraken in de index, relevant, met tekst:")
    for r in q("""SELECT substr(datum_uitspraak,1,4), COUNT(*), SUM(relevant), SUM(tekst_pad IS NOT NULL) FROM uitspraak
                  GROUP BY 1 ORDER BY 1"""):
        print("  ", r)
    print("== object x uitkomst (met tekst):")
    for r in q("SELECT object, uitkomst, COUNT(*) FROM uitspraak WHERE tekst_pad IS NOT NULL GROUP BY 1,2 ORDER BY 1,3 DESC"):
        print("  ", r)
    print("== stikstof als grond, per jaar (met tekst):", q("""SELECT substr(datum_uitspraak,1,4), SUM(stikstof), SUM(natura2000), COUNT(*)
                                                              FROM uitspraak WHERE tekst_pad IS NOT NULL GROUP BY 1 ORDER BY 1"""))
    print("== gemeente herkend:", q("SELECT COUNT(*), SUM(creator IS NOT NULL), SUM(besluitdatum IS NOT NULL) FROM uitspraak WHERE tekst_pad IS NOT NULL AND bestuursorgaan IN ('college','gemeenteraad')"))


def main(argv):
    conn = sqlite3.connect(DB_PAD, timeout=900)  # de zaakvorming houdt het schrijfslot minutenlang; wachten in plaats van falen
    conn.executescript(SCHEMA)
    stap = argv[0] if argv else "meet"
    if stap == "index":
        oogst_index(conn, int(argv[1]), int(argv[2]))
    elif stap == "tekst":
        jaren = [int(a) for a in argv[1:]] or [2019, 2018, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010]
        oogst_tekst(conn, jaren)
    elif stap == "herclassificeer":
        herclassificeer(conn)
    elif stap == "koppel":
        koppel(conn)
    meet(conn)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
