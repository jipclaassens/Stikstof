"""Tekstvlaggen: welke bekendmakingen in hun tekst stikstof, AERIUS, Natura 2000, de natuurwetgeving of salderen noemen.

Zelfde techniek als frasen.py: een zoekopdracht op cql.textAndIndexes met het schema short levert per pagina 1.000
identifiers, zodat een term landelijk in enkele honderden verzoeken te oogsten is in plaats van een verzoek per record.
De basis is dezelfde als de drie publicatiestromen (rubriek omgevingsvergunning, andere vergunning en beschikking, de
oude informatietypen van het Gemeenteblad). Per term en jaar een partitie; een jaar boven de grens van 10.000 wordt per
maand gesplitst. De vlag zegt dat de tekst van de kennisgeving de term noemt, niet dat er een natuurvergunning nodig
was: een aanvraag die meldt dat er geen stikstofdepositie is, telt ook. Juist dat maakt het een maat voor de vraag of
stikstof in de zaak ter sprake kwam.

Uitvoer: tabel publicatie_tekstvlag (identifier, term) en tabel tekstvlag_oogst in vergunningen.sqlite, ruwe pagina's
onder ruw/tekstvlag/<term>/<periode>. Herhaalbaar: afgeronde partities worden overgeslagen.
Gebruik: py -3.13 tekstvlag.py [--term aerius] [--opnieuw]
"""
import datetime as dt
import gzip
import re
import sqlite3
import sys

from config import DB_PAD, RUW_DIR, STROMEN
from sru import pagina, tel

BASIS = "(" + " or ".join(STROMEN[s].strip("()") for s in ("rubriek", "andere", "oud")) + ")"
# naam: zoekexpressie op de tekstindex; "adj" voor woordgroepen, "any" voor een woord (de index stemt)
TERMEN = {
    "stikstof": 'cql.textAndIndexes any "stikstof"',
    "stikstofdepositie": 'cql.textAndIndexes any "stikstofdepositie"',
    "stikstofberekening": 'cql.textAndIndexes any "stikstofberekening"',
    "aerius": 'cql.textAndIndexes any "AERIUS"',
    "natura2000": 'cql.textAndIndexes adj "Natura 2000"',
    "natura2000_activiteit": 'cql.textAndIndexes adj "Natura 2000-activiteit"',
    "passende_beoordeling": 'cql.textAndIndexes adj "passende beoordeling"',
    "wet_natuurbescherming": 'cql.textAndIndexes adj "Wet natuurbescherming"',
    "natuurtoets": 'cql.textAndIndexes any "natuurtoets"',
    "flora_fauna": 'cql.textAndIndexes adj "flora- en fauna"',
    "soortenbescherming": 'cql.textAndIndexes any "soortenbescherming"',
    "bouwvrijstelling": 'cql.textAndIndexes any "bouwvrijstelling"',
    "intern_salderen": 'cql.textAndIndexes adj "intern salderen"',
    "extern_salderen": 'cql.textAndIndexes adj "extern salderen"',
}
EERSTE_JAAR, GRENS = 2013, 9500
_ID = re.compile(rb"<dcterms:identifier>([^<]+)</dcterms:identifier>")

SCHEMA = """
CREATE TABLE IF NOT EXISTS publicatie_tekstvlag (identifier TEXT, term TEXT, PRIMARY KEY (identifier, term));
CREATE TABLE IF NOT EXISTS tekstvlag_oogst (term TEXT, periode TEXT, verwacht INTEGER, ontvangen INTEGER, status TEXT,
  tijdstip TEXT, PRIMARY KEY (term, periode));
"""


def perioden(term_cql: str):
    """Jaren, en een jaar boven de grens per maand."""
    vandaag = dt.date.today()
    for j in range(EERSTE_JAAR, vandaag.year + 1):
        van, tot = dt.date(j, 1, 1), min(dt.date(j, 12, 31), vandaag)
        cql = f'{BASIS} and {term_cql} and dt.available within "{van} {tot}"'
        n = tel(cql)
        if n <= GRENS:
            yield str(j), cql, n
            continue
        for m in range(1, 13):
            van = dt.date(j, m, 1)
            if van > vandaag:
                break
            tot = (dt.date(j + (m == 12), m % 12 + 1, 1) - dt.timedelta(days=1))
            cqlm = f'{BASIS} and {term_cql} and dt.available within "{van} {min(tot, vandaag)}"'
            yield f"{j}-{m:02d}", cqlm, tel(cqlm)


def oogst_partitie(term: str, periode: str, cql: str, verwacht: int):
    doel = RUW_DIR / "tekstvlag" / term / periode
    doel.mkdir(parents=True, exist_ok=True)
    ids, start, nr = [], 1, 1
    while len(ids) < verwacht:
        data = pagina(cql, start, schema="http://standaarden.overheid.nl/short")
        gevonden = [i.decode() for i in _ID.findall(data)]
        if not gevonden:
            break
        with gzip.open(doel / f"p{nr}.xml.gz", "wb") as f:
            f.write(data)
        ids += gevonden
        start += len(gevonden)
        nr += 1
    return ids


def main(argv):
    opnieuw = "--opnieuw" in argv
    alleen = argv[argv.index("--term") + 1] if "--term" in argv else None
    conn = sqlite3.connect(DB_PAD, timeout=900)
    conn.executescript(SCHEMA)
    for term, term_cql in TERMEN.items():
        if alleen and term != alleen:
            continue
        totaal = 0
        for periode, cql, verwacht in perioden(term_cql):
            rij = conn.execute("SELECT status FROM tekstvlag_oogst WHERE term = ? AND periode = ?", (term, periode)).fetchone()
            if rij and rij[0] == "ok" and not opnieuw:
                continue
            ids = oogst_partitie(term, periode, cql, verwacht) if verwacht else []
            status = "ok" if len(ids) >= verwacht else f"onvolledig {len(ids)} van {verwacht}"
            conn.executemany("INSERT OR IGNORE INTO publicatie_tekstvlag VALUES (?, ?)", [(i, term) for i in ids])
            conn.execute("INSERT OR REPLACE INTO tekstvlag_oogst VALUES (?, ?, ?, ?, ?, ?)",
                         (term, periode, verwacht, len(ids), status, dt.datetime.now().isoformat(timespec="seconds")))
            conn.commit()
            totaal += len(ids)
            print(f"{term} {periode}: {status} ({len(ids)})", flush=True)
        n = conn.execute("SELECT count(*) FROM publicatie_tekstvlag WHERE term = ?", (term,)).fetchone()[0]
        g = conn.execute("SELECT count(*) FROM publicatie_tekstvlag t JOIN publicatie p USING (identifier) WHERE t.term = ?", (term,)).fetchone()[0]
        print(f"== {term}: {n} identifiers, waarvan {g} in de tabel publicatie", flush=True)
    conn.close()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
