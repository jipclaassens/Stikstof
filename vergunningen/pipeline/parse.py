"""Parser van gzd-antwoorden naar SQLite: een rij per publicatie, een rij per activiteit, een rij per gebiedsmarkering.

De geometrie blijft als WKT in EPSG:28992 staan; x en y zijn het midden van de omhullende rechthoek en de
uitgestrektheid is de grootste zijde ervan (mediaan 19 tot 20 m, p90 136 tot 145 m in 2025). Een echt zwaartepunt en de
oppervlakte komen bij de GeoPackage-export via GDAL. Velden die de parser niet kent gaan als JSON mee, zodat niets
uit het ruwe record verloren gaat zonder dat het schema elke variant hoeft te kennen.
"""
import gzip
import json
import re
import xml.etree.ElementTree as ET

from fase import classificeer_titel

NS = {
    "sru": "http://docs.oasis-open.org/ns/search-ws/sruResponse",
    "gzd": "http://standaarden.overheid.nl/sru",
    "ow": "http://standaarden.overheid.nl/wetgeving/",
    "dcterms": "http://purl.org/dc/terms/",
}

SCHEMA = """
PRAGMA journal_mode=WAL;
CREATE TABLE IF NOT EXISTS publicatie (
  identifier TEXT PRIMARY KEY, titel TEXT, rubriek TEXT, creator TEXT, creator_scheme TEXT, organisatietype TEXT,
  publicatienaam TEXT, jaargang INTEGER, publicatienummer INTEGER, available TEXT, modified TEXT, abstract TEXT,
  subject TEXT, url_html TEXT, url_xml TEXT, url_pdf TEXT, url_metadata TEXT, n_markeringen INTEGER,
  fase_titel TEXT, extra_json TEXT, bron_bestand TEXT, geladen_op TEXT, informatietype TEXT, fase_tekst TEXT);
CREATE TABLE IF NOT EXISTS publicatie_activiteit (
  identifier TEXT, activiteit TEXT, PRIMARY KEY (identifier, activiteit));
CREATE TABLE IF NOT EXISTS publicatie_locatie (
  identifier TEXT, volgnr INTEGER, locatietype TEXT, wkt_rd TEXT, geomtype TEXT, x REAL, y REAL,
  minx REAL, miny REAL, maxx REAL, maxy REAL, uitgestrektheid_m REAL, label TEXT, ligt_in_gemeente TEXT, extra_json TEXT,
  PRIMARY KEY (identifier, volgnr));
CREATE TABLE IF NOT EXISTS oogst (
  dag TEXT, map_naam TEXT, cql TEXT, verwacht INTEGER, ontvangen INTEGER, paginas INTEGER, status TEXT,
  tijdstip TEXT, duur_s REAL, PRIMARY KEY (dag, map_naam));
CREATE TABLE IF NOT EXISTS bestand_geladen (bestand TEXT PRIMARY KEY, records INTEGER, geladen_op TEXT);
CREATE INDEX IF NOT EXISTS ix_pub_creator_available ON publicatie (creator, available);
CREATE INDEX IF NOT EXISTS ix_pub_available ON publicatie (available);
CREATE INDEX IF NOT EXISTS ix_pub_jaargang ON publicatie (jaargang, organisatietype);
CREATE TABLE IF NOT EXISTS publicatie_frase (
  identifier TEXT, frase TEXT, klasse TEXT, creator TEXT, jaargang INTEGER, gevonden_op TEXT,
  PRIMARY KEY (identifier, frase));
CREATE TABLE IF NOT EXISTS frase_oogst (
  creator TEXT, jaargang INTEGER, frase TEXT, verwacht INTEGER, ontvangen INTEGER, status TEXT, tijdstip TEXT,
  PRIMARY KEY (creator, jaargang, frase));
CREATE INDEX IF NOT EXISTS ix_loc_xy ON publicatie_locatie (x, y);
CREATE VIEW IF NOT EXISTS v_fase_per_creator AS
  SELECT creator, organisatietype, jaargang, fase_titel, COUNT(*) AS n
  FROM publicatie GROUP BY creator, organisatietype, jaargang, fase_titel;
"""

def zorg_kolom(conn, tabel: str, kolom: str, soort: str):
    """Voegt een kolom toe als hij ontbreekt; SQLite kent geen ADD COLUMN IF NOT EXISTS."""
    if kolom not in [r[1] for r in conn.execute(f"PRAGMA table_info({tabel})")]:
        conn.execute(f"ALTER TABLE {tabel} ADD COLUMN {kolom} {soort}")
        conn.commit()


_COORD = re.compile(r"(-?\d+(?:\.\d+)?)\s+(-?\d+(?:\.\d+)?)")
_NIET_IN_EXTRA = {"geometrie", "geometrielabel", "ligtInGemeente", "locatiepunt", "locatiegebied"}
_BEKEND_TP = {"activiteit", "jaargang", "organisatietype", "publicatienummer", "publicatienaam", "gebiedsmarkering",
              "product-area", "content-area", "datumTijdstipWijzigingWork", "datumTijdstipWijzigingExpression"}


def _lokaal(tag: str) -> str:
    return tag.split("}")[-1]


def _tekst(el, pad):
    if el is None:
        return None
    e = el.find(pad, NS)
    return e.text.strip() if e is not None and e.text and e.text.strip() else None


def omhullende(wkt: str):
    paren = _COORD.findall(wkt)
    if not paren:
        return None
    xs = [float(a) for a, _ in paren]
    ys = [float(b) for _, b in paren]
    return min(xs), min(ys), max(xs), max(ys)


def parse_record(rec, bron_bestand: str):
    meta = rec.find(".//ow:meta", NS)
    kern = meta.find("ow:owmskern", NS)
    mantel = meta.find("ow:owmsmantel", NS)
    tp = meta.find("ow:tpmeta", NS)
    enriched = rec.find(".//gzd:enrichedData", NS)
    creator_el = kern.find("dcterms:creator", NS)
    urls = {}
    if enriched is not None:
        for iu in enriched.findall("gzd:itemUrl", NS):
            urls[iu.get("manifestation")] = iu.text
    titel = _tekst(kern, "dcterms:title")
    # dcterms:type komt twee keer voor: de rubriek (OVERHEIDop.Rubriek) en, bij records van voor medio 2021, het oude
    # informatietype (OVERHEIDgvop.Informatietype: Beschikkingen | aanvraag of Beschikkingen | afhandeling)
    rubriek, informatietype = None, None
    for t in kern.findall("dcterms:type", NS):
        if not (t.text and t.text.strip()):
            continue
        if "Informatietype" in (t.get("scheme") or ""):
            informatietype = t.text.strip()
        elif rubriek is None:
            rubriek = t.text.strip()
    pub = {
        "identifier": _tekst(kern, "dcterms:identifier"),
        "titel": titel,
        "rubriek": rubriek,
        "informatietype": informatietype,
        "creator": creator_el.text.strip() if creator_el is not None and creator_el.text else None,
        "creator_scheme": creator_el.get("scheme") if creator_el is not None else None,
        "organisatietype": _tekst(tp, "ow:organisatietype"),
        "publicatienaam": _tekst(tp, "ow:publicatienaam"),
        "jaargang": _tekst(tp, "ow:jaargang"),
        "publicatienummer": _tekst(tp, "ow:publicatienummer"),
        "available": _tekst(mantel, "dcterms:available"),
        "modified": _tekst(kern, "dcterms:modified"),
        "abstract": _tekst(mantel, "dcterms:abstract"),
        "subject": _tekst(mantel, "dcterms:subject"),
        "url_html": urls.get("html"),
        "url_xml": urls.get("xml"),
        "url_pdf": urls.get("pdf"),
        "url_metadata": urls.get("metadata"),
        "fase_titel": classificeer_titel(titel or ""),
        "bron_bestand": bron_bestand,
    }
    # het oude informatietype als terugval waar de titel geen fase geeft: aanvraag of een besluit zonder uitkomst
    if pub["fase_titel"] == "onbepaald" and informatietype:
        if informatietype.endswith("aanvraag"):
            pub["fase_titel"] = "aanvraag"
        elif informatietype.endswith("afhandeling"):
            pub["fase_titel"] = "besluit"
    activiteiten = sorted({a.text.strip() for a in tp.findall("ow:activiteit", NS) if a.text and a.text.strip()}) if tp is not None else []
    extra = {}
    if tp is not None:
        for kind in tp:
            naam = _lokaal(kind.tag)
            if naam in _BEKEND_TP or not (kind.text and kind.text.strip()):
                continue
            extra.setdefault(naam, []).append(kind.text.strip())
    pub["extra_json"] = json.dumps(extra, ensure_ascii=False) if extra else None

    locaties = []
    if tp is not None:
        for gm in tp.findall("ow:gebiedsmarkering", NS):
            for vorm in gm:
                wkt = _tekst(vorm, "ow:geometrie")
                velden = {}
                for kind in vorm:
                    naam = _lokaal(kind.tag)
                    if naam in _NIET_IN_EXTRA or not (kind.text and kind.text.strip()):
                        continue
                    velden[naam] = kind.text.strip()
                b = omhullende(wkt) if wkt else None
                locaties.append({
                    "locatietype": _lokaal(vorm.tag),
                    "wkt_rd": wkt,
                    "geomtype": wkt.split("(")[0].strip().upper() if wkt else None,
                    "x": (b[0] + b[2]) / 2 if b else None,
                    "y": (b[1] + b[3]) / 2 if b else None,
                    "minx": b[0] if b else None, "miny": b[1] if b else None,
                    "maxx": b[2] if b else None, "maxy": b[3] if b else None,
                    "uitgestrektheid_m": max(b[2] - b[0], b[3] - b[1]) if b else None,
                    "label": _tekst(vorm, "ow:geometrielabel"),
                    "ligt_in_gemeente": _tekst(vorm, "ow:ligtInGemeente"),
                    "extra_json": json.dumps(velden, ensure_ascii=False) if velden else None,
                })
    pub["n_markeringen"] = len(locaties)
    return pub, activiteiten, locaties


def parse_bestand(pad):
    """Levert (publicatie, activiteiten, locaties) per record uit een gzip-antwoord."""
    with gzip.open(pad, "rb") as f:
        root = ET.fromstring(f.read())
    for rec in root.findall("sru:records/sru:record", NS):
        yield parse_record(rec, str(pad))


def schrijf(conn, pub, activiteiten, locaties, geladen_op):
    pub = dict(pub, geladen_op=geladen_op)
    kolommen = ", ".join(pub)
    plaatsen = ", ".join("?" for _ in pub)
    conn.execute(f"INSERT OR REPLACE INTO publicatie ({kolommen}) VALUES ({plaatsen})", list(pub.values()))
    conn.execute("DELETE FROM publicatie_activiteit WHERE identifier = ?", (pub["identifier"],))
    conn.executemany("INSERT INTO publicatie_activiteit VALUES (?, ?)", [(pub["identifier"], a) for a in activiteiten])
    conn.execute("DELETE FROM publicatie_locatie WHERE identifier = ?", (pub["identifier"],))
    for nr, loc in enumerate(locaties, start=1):
        rij = dict(identifier=pub["identifier"], volgnr=nr, **loc)
        conn.execute(
            f"INSERT INTO publicatie_locatie ({', '.join(rij)}) VALUES ({', '.join('?' for _ in rij)})",
            list(rij.values()),
        )
