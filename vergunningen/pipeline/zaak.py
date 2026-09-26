"""Zaakvorming: publicaties van dezelfde gemeente die over dezelfde aanvraag gaan, samenbrengen tot een zaak.

Gebruik: py -3.13 zaak.py            vormt zaken voor alle publicaties en schrijft de tabellen zaak en publicatie_zaak
         py -3.13 zaak.py --meet     alleen de metingen op de bestaande tabellen

Twee routes die elkaar aanvullen, gemeten op 2024. Een zaaknummer in de titel (17,8 procent van de gemeentelijke
titels; 36 gemeenten vrijwel altijd, 247 vrijwel nooit) koppelt direct. Anders de locatie: dezelfde eerste
gebiedsmarkering op vijf meter, een punt binnen het vlak van de ander, of binnen vijftig meter met hetzelfde
huisnummer in de titel; dat vangt gemeenten die de aanvraag als punt en het besluit als vlak publiceren (Eindhoven,
Lochem). Een record met zaaknummer op de plek van een lopende zaak zonder nummer hoort bij die zaak, en andersom
(Midden-Groningen zet een nummer alleen in de besluittitel). Standaardpunten (een punt dat bij een gemeente in meer dan
een procent van de records of meer dan vijftig keer voorkomt, vaak het gemeentehuis) tellen niet als locatie.

Records uit de rubrieken andere vergunning en andere beschikking (stroom "andere") tellen alleen mee als de titel
over een omgevingsvergunning of bouwwerk gaat, of als ze op zaaknummer of locatie aan een bestaande zaak hangen en de
titel niet duidelijk over iets anders gaat (evenement, horeca, standplaats).

Binnen een zaak begint een nieuwe zaak zodra een aanvraag volgt op een zaak die al een uitkomst heeft, of zodra het
venster van 400 dagen na de eerste publicatie verstreken is. De uitkomst is de eerste definitieve fase na de aanvraag:
verleend, geweigerd, ingetrokken, vergunningvrij; een besluit zonder uitkomst in de titel heet besluit_onbekend en
wacht op de tekstfrasen. De doorlooptijd is het aantal dagen tussen de eerste aanvraagpublicatie en de eerste
uitkomstpublicatie, inclusief de publicatievertraging aan beide kanten (mediaan 13 dagen, per gemeente 1 tot 20).
"""
import collections
import datetime as dt
import math
import re
import sqlite3
import sys

from config import DB_PAD
from parse import zorg_kolom

VENSTER_DAGEN = 400          # langer dan de uitgebreide procedure met verlenging
NA_UITKOMST_DAGEN = 90       # rectificatie, bezwaar of intrekking na een besluit hoort nog bij de zaak
STANDAARDPUNT_AANDEEL = 0.01
STANDAARDPUNT_MINIMUM = 50
RASTER_M = 5                 # zelfde plek
CEL_M = 50                   # zoekcel van de locatie-index
NABIJ_M = 50                 # met huisnummerbewijs
VLAK_MARGE_M = 5

UITKOMSTEN = ("verleend", "geweigerd", "ingetrokken", "vergunningvrij", "besluit")

_TOKEN = re.compile(r"(?<![\w.])(?=[A-Za-z0-9./_-]*\d{4})[A-Za-z]{0,8}[-_/.]?(?:\d{2,4}[-_/.])?\d{3,}(?:[-_/.]\d{2,})*(?![\w])")
_DATUM8 = re.compile(r"^(19|20)\d{2}(0[1-9]|1[0-2])(0[1-9]|[12]\d|3[01])$")
_HUISNR = re.compile(r"(?<![\w-])(\d{1,4}[a-zA-Z]?)(?![\w-])")
_OMG = re.compile(r"omgevingsvergunning|omgevingsplan|omgevingswet|wabo|\bbopa\b|bouw|sloop|slopen|\bkap\b|kappen|uitrit|inrit|"
                  r"dakkapel|woning|aanbouw|uitbouw|dakopbouw|bijgebouw|schuur|garage|carport|gevel|monument|erfafscheiding|"
                  r"zonnepanelen|warmtepomp|airco|reclame|steiger|dam|duiker|brug")
_ANDER = re.compile(r"evenement|drank|horeca|alcohol|standplaats|terras|exploitatie|\bapv\b|loterij|collecte|kansspel|"
                    r"vuurwerk|stook|ontheffing|parkeer|markt|inzamel|kabels|leidingen|verkeer|speelautomaat|prostitutie|"
                    r"huisvestingsvergunning|urgentie|ligplaats|hond|carbid|gehandicaptenparkeer|straatnaam|huisnummerbesluit")

SCHEMA = """
DROP TABLE IF EXISTS zaak;
DROP TABLE IF EXISTS publicatie_zaak;
CREATE TABLE zaak (
  zaak_id INTEGER PRIMARY KEY, creator TEXT, organisatietype TEXT, sleutelsoort TEXT, sleutel TEXT,
  eerste_datum TEXT, laatste_datum TEXT, n_publicaties INTEGER,
  datum_aanvraag TEXT, datum_uitkomst TEXT, uitkomst TEXT, doorlooptijd_dagen INTEGER,
  n_verlengingen INTEGER, x REAL, y REAL, locatietype TEXT);
CREATE TABLE publicatie_zaak (identifier TEXT PRIMARY KEY, zaak_id INTEGER, route TEXT);
CREATE INDEX ix_zaak_creator ON zaak (creator, datum_aanvraag);
CREATE INDEX ix_pz_zaak ON publicatie_zaak (zaak_id);
"""


def zaaknummer(titel: str):
    """Het eerste zaaknummerachtige token in de titel, genormaliseerd; datums en jaartallen tellen niet."""
    for m in _TOKEN.finditer(titel or ""):
        t = m.group(0)
        if sum(ch.isdigit() for ch in t) < 6:
            continue
        kaal = re.sub(r"\D", "", t)
        if len(kaal) == 8 and _DATUM8.match(kaal):
            continue
        return t.upper().replace(" ", "")
    return None


def huisnummers(titel: str) -> set:
    """Huisnummerachtige tokens in de titel, als bijkomend bewijs bij een locatiematch op afstand."""
    uit = set()
    for m in _HUISNR.finditer(titel or ""):
        t = m.group(1)
        if len(t) == 4 and t.isdigit() and (t.startswith("19") or t.startswith("20")):
            continue
        uit.add(t.lower())
    return uit


def laad_publicaties(conn):
    """De fase is de titelfase, of de tekstfase waar de titel geen uitkomst gaf (frasen.py)."""
    zorg_kolom(conn, "publicatie", "fase_tekst", "TEXT")
    return conn.execute("""
        SELECT p.identifier, p.creator, p.organisatietype, p.available, COALESCE(p.fase_tekst, p.fase_titel), p.titel,
               p.rubriek, l.locatietype, l.x, l.y, l.minx, l.miny, l.maxx, l.maxy
        FROM publicatie p LEFT JOIN publicatie_locatie l ON l.identifier = p.identifier AND l.volgnr = 1
        WHERE p.available IS NOT NULL ORDER BY p.creator, p.available, p.identifier""").fetchall()


def standaardpunten(rijen):
    """Per creator de rasterpunten die zo vaak voorkomen dat ze geen locatie van een werk kunnen zijn."""
    per = collections.defaultdict(collections.Counter)
    n = collections.Counter()
    for rij in rijen:
        creator, x, y = rij[1], rij[8], rij[9]
        n[creator] += 1
        if x is not None:
            per[creator][(round(x / RASTER_M), round(y / RASTER_M))] += 1
    return {creator: {k for k, v in cnt.items() if v >= max(STANDAARDPUNT_MINIMUM, STANDAARDPUNT_AANDEEL * n[creator])}
            for creator, cnt in per.items()}


class Zaak:
    __slots__ = ("zaak_id", "creator", "organisatietype", "sleutelsoort", "sleutel", "eerste_datum", "laatste_datum",
                 "n", "datum_aanvraag", "datum_uitkomst", "uitkomst", "n_verlengingen", "x", "y", "locatietype",
                 "geoms", "huisnrs", "doorlooptijd")

    def __init__(self, zaak_id, rij, soort, sleutel):
        self.zaak_id = zaak_id
        self.creator, self.organisatietype = rij[1], rij[2]
        self.sleutelsoort, self.sleutel = soort, sleutel
        self.eerste_datum = self.laatste_datum = rij[3]
        self.n = 0
        self.datum_aanvraag = self.datum_uitkomst = self.uitkomst = None
        self.n_verlengingen = 0
        self.x, self.y, self.locatietype = rij[8], rij[9], rij[7]
        self.geoms, self.huisnrs = [], set()
        self.doorlooptijd = None

    def past(self, rij):
        """Mag deze publicatie nog bij deze zaak: binnen het venster, en geen nieuwe aanvraag na een uitkomst."""
        datum, fase = rij[3], rij[4]
        d0 = dt.date.fromisoformat(self.eerste_datum)
        d1 = dt.date.fromisoformat(datum)
        if (d1 - d0).days > VENSTER_DAGEN:
            return False
        if self.uitkomst is not None:
            if fase == "aanvraag":
                return False
            if fase in UITKOMSTEN and (d1 - dt.date.fromisoformat(self.datum_uitkomst)).days > NA_UITKOMST_DAGEN:
                return False
        return True

    def nabijheid(self, rij):
        """0 dezelfde plek, 1 punt binnen vlak, 2 nabij met hetzelfde huisnummer, anders None."""
        x, y, minx, miny, maxx, maxy = rij[8], rij[9], rij[10], rij[11], rij[12], rij[13]
        beste = None
        for gx, gy, gminx, gminy, gmaxx, gmaxy in self.geoms:
            d = math.hypot(x - gx, y - gy)
            if d <= RASTER_M * 1.5:
                return 0
            if (gminx - VLAK_MARGE_M <= x <= gmaxx + VLAK_MARGE_M and gminy - VLAK_MARGE_M <= y <= gmaxy + VLAK_MARGE_M) or \
               (minx - VLAK_MARGE_M <= gx <= maxx + VLAK_MARGE_M and miny - VLAK_MARGE_M <= gy <= maxy + VLAK_MARGE_M):
                beste = 1 if beste is None else min(beste, 1)
            elif d <= NABIJ_M and self.huisnrs and (self.huisnrs & huisnummers(rij[5])):
                beste = 2 if beste is None else min(beste, 2)
        return beste

    def voeg_toe(self, rij):
        datum, fase = rij[3], rij[4]
        self.n += 1
        self.laatste_datum = max(self.laatste_datum, datum)
        if fase == "aanvraag" and self.datum_aanvraag is None:
            self.datum_aanvraag = datum
        if fase == "verlenging":
            self.n_verlengingen += 1
        if fase in UITKOMSTEN and self.uitkomst is None:
            self.uitkomst = "besluit_onbekend" if fase == "besluit" else fase
            self.datum_uitkomst = datum
        if rij[8] is not None:
            self.geoms.append((rij[8], rij[9], rij[10], rij[11], rij[12], rij[13]))
        self.huisnrs |= huisnummers(rij[5])


def relevantie(rij):
    """'zaak' mag een zaak beginnen, 'aanhaken' mag alleen bij een bestaande zaak, 'nee' doet niet mee."""
    if rij[6] == "omgevingsvergunning":
        return "zaak"
    t = (rij[5] or "").lower()
    if _ANDER.search(t):
        return "nee"
    return "zaak" if _OMG.search(t) else "aanhaken"


def vorm_zaken(rijen):
    sp = standaardpunten(rijen)
    zaken, toewijzing = [], {}
    token_index = {}                              # (creator, token) -> zaak_id
    cellen = collections.defaultdict(list)        # (creator, cx, cy) -> [zaak_id]
    overgeslagen = 0

    def cel(x, y):
        return (int(x // CEL_M), int(y // CEL_M))

    def registreer(z, rij, token):
        if token:
            token_index[(z.creator, token)] = z.zaak_id
        x, y = rij[8], rij[9]
        if x is not None:
            cx, cy = cel(x, y)
            lijst = cellen[(z.creator, cx, cy)]
            if not lijst or lijst[-1] != z.zaak_id:
                lijst.append(z.zaak_id)

    for rij in rijen:
        ident, creator, titel, x, y = rij[0], rij[1], rij[5], rij[8], rij[9]
        rel = relevantie(rij)
        if rel == "nee":
            overgeslagen += 1
            continue
        token = zaaknummer(titel)
        z, route = None, None
        if token and (creator, token) in token_index:
            kandidaat = zaken[token_index[(creator, token)] - 1]
            if kandidaat.past(rij):
                z, route = kandidaat, "zaaknummer"
        loc_ok = x is not None and (round(x / RASTER_M), round(y / RASTER_M)) not in sp.get(creator, ())
        if z is None and loc_ok:
            cx, cy = cel(x, y)
            beste = None
            gezien = set()
            for dx in (-1, 0, 1):
                for dy in (-1, 0, 1):
                    for zid in reversed(cellen.get((creator, cx + dx, cy + dy), ())):
                        if zid in gezien:
                            continue
                        gezien.add(zid)
                        kandidaat = zaken[zid - 1]
                        if not kandidaat.past(rij):
                            continue
                        score = kandidaat.nabijheid(rij)
                        if score is None:
                            continue
                        if beste is None or score < beste[0]:
                            beste = (score, kandidaat)
            if beste is not None:
                z, route = beste[1], "locatie"
        if z is None:
            if rel != "zaak":
                overgeslagen += 1
                continue
            soort = "zaaknummer" if token else ("locatie" if loc_ok else "los")
            sleutel = token if token else (f"{round(x / RASTER_M)}:{round(y / RASTER_M)}" if loc_ok else ident)
            z = Zaak(len(zaken) + 1, rij, soort, sleutel)
            zaken.append(z)
            route = soort
        z.voeg_toe(rij)
        toewijzing[ident] = (z.zaak_id, route)
        registreer(z, rij, token)
    for z in zaken:
        if z.datum_aanvraag and z.datum_uitkomst and z.datum_uitkomst >= z.datum_aanvraag:
            z.doorlooptijd = (dt.date.fromisoformat(z.datum_uitkomst) - dt.date.fromisoformat(z.datum_aanvraag)).days
    return zaken, toewijzing, overgeslagen


def schrijf(conn, zaken, toewijzing):
    conn.executescript(SCHEMA)
    conn.executemany("INSERT INTO zaak VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)", [
        (z.zaak_id, z.creator, z.organisatietype, z.sleutelsoort, z.sleutel, z.eerste_datum, z.laatste_datum, z.n,
         z.datum_aanvraag, z.datum_uitkomst, z.uitkomst, z.doorlooptijd, z.n_verlengingen, z.x, z.y, z.locatietype)
        for z in zaken])
    conn.executemany("INSERT INTO publicatie_zaak VALUES (?,?,?)", [(i, zid, route) for i, (zid, route) in toewijzing.items()])
    conn.commit()


def meet(conn, jaar="2024", tot_maand="06"):
    q = lambda s, *a: conn.execute(s, a).fetchall()
    van, tot = f"{jaar}-01-01", f"{jaar}-{int(tot_maand) + 1:02d}-01"
    print(f"== zaken: {q('SELECT COUNT(*) FROM zaak')[0][0]}, publicaties in een zaak: {q('SELECT COUNT(*) FROM publicatie_zaak')[0][0]} "
          f"van {q('SELECT COUNT(*) FROM publicatie')[0][0]}")
    print("== route:", q("SELECT route, COUNT(*) FROM publicatie_zaak GROUP BY 1 ORDER BY 2 DESC"))
    print("== publicaties per zaak:", q("SELECT MIN(n_publicaties,6), COUNT(*) FROM zaak GROUP BY 1 ORDER BY 1"))
    print(f"== aanvragen van {jaar}-01 tot en met {jaar}-{tot_maand} (gemeenten), uitkomst binnen de data:")
    rows = q("SELECT COALESCE(uitkomst,'geen'), COUNT(*) FROM zaak WHERE organisatietype='gemeente' AND datum_aanvraag >= ? AND datum_aanvraag < ? GROUP BY 1 ORDER BY 2 DESC", van, tot)
    totaal = sum(n for _, n in rows)
    for u, n in rows:
        print(f"   {n:7d} {100 * n / totaal:5.1f}%  {u}")
    print("== doorlooptijd aanvraag tot uitkomst (dagen), per uitkomst: mediaan, p25, p75, n")
    for u in ("verleend", "geweigerd", "ingetrokken", "besluit_onbekend"):
        d = [r[0] for r in q("SELECT doorlooptijd_dagen FROM zaak WHERE organisatietype='gemeente' AND uitkomst=? AND doorlooptijd_dagen IS NOT NULL AND datum_aanvraag >= ? AND datum_aanvraag < ? ORDER BY 1", u, van, tot)]
        if d:
            print(f"   {u:17s} {d[len(d) // 2]:4d} {d[len(d) // 4]:4d} {d[3 * len(d) // 4]:4d}  n={len(d)}")
    print("== per gemeente (n aanvragen >= 200 in dat halfjaar): aandeel met uitkomst, aandeel verleend, mediaan doorlooptijd verleend")
    rows = q("""SELECT creator, COUNT(*) n, SUM(uitkomst IS NOT NULL) mu, SUM(uitkomst='verleend') v FROM zaak
                WHERE organisatietype='gemeente' AND datum_aanvraag >= ? AND datum_aanvraag < ? GROUP BY 1 HAVING n >= 200 ORDER BY 1.0*mu/n DESC""", van, tot)

    def med(cr):
        d = [r[0] for r in q("SELECT doorlooptijd_dagen FROM zaak WHERE creator=? AND uitkomst='verleend' AND doorlooptijd_dagen IS NOT NULL AND datum_aanvraag >= ? AND datum_aanvraag < ? ORDER BY 1", cr, van, tot)]
        return d[len(d) // 2] if d else None
    for cr, n, mu, v in rows[:8] + [("...", 0, 0, 0)] + rows[-8:]:
        if cr == "...":
            print("   ...")
            continue
        print(f"   {100 * mu / n:5.1f}% uitkomst, {100 * v / n:5.1f}% verleend, mediaan {med(cr)} dagen, n={n:5d}  {cr}")
    kl = collections.Counter()
    for cr, n, mu, v in rows:
        a = mu / n
        kl[">=80%" if a >= .8 else ">=60%" if a >= .6 else ">=40%" if a >= .4 else "<40%"] += 1
    print("   gemeenten naar uitkomstaandeel:", dict(kl))


def main(argv):
    conn = sqlite3.connect(DB_PAD, timeout=120)
    if "--meet" not in argv:
        rijen = laad_publicaties(conn)
        zaken, toewijzing, overgeslagen = vorm_zaken(rijen)
        schrijf(conn, zaken, toewijzing)
        print(f"{len(rijen)} publicaties: {len(toewijzing)} in {len(zaken)} zaken, {overgeslagen} niet meegenomen")
    meet(conn)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
