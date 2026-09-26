"""Tekstfrasen: de uitkomst van een besluit uit het tekstlichaam halen zonder elk record op te vragen.

De index cql.textAndIndexes doorzoekt het tekstlichaam en stemt (verlenen, verleent en verleend tellen als een woord).
Per gemeente, jaargang en frase levert een zoekopdracht met het schema short (identifier, titel, geometrie; 1.000 per
pagina) de records waarin de frase staat; dat kost per gemeente en jaargang een tiental verzoeken in plaats van een
verzoek per record. De uitkomst wordt alleen toegepast op records waarvan de titel geen uitkomst gaf (fase_titel
besluit of onbepaald); waar de titel een uitkomst noemt blijft die leidend.

Twee lagen frasen. Landelijke frasen die de uitkomst letterlijk noemen, en frasen per gemeente voor gemeenten die de
uitkomst alleen impliciet opschrijven: Westland zegt "De Omgevingsvergunning ligt tijdens de bezwaartermijn ter
inzage" en nooit verleend (551 van 553 beschikkingen in 2024), Dordrecht "Bent u het niet eens met de vergunning".
De per-gemeente frasen komen uit aanvulling 4 van het ontwerp (precisie 100 procent op 30 teksten per gemeente) en
uit metingen op de oogst; de lijst groeit per gemeente die in de metingen blijft hangen op besluit_onbekend.

Voorrang gaat per klasse, niet per frase: een tekst met "gedeeltelijk geweigerd" en "verleend" is geweigerd.

Gebruik: py -3.13 frasen.py [jaargang] [--minimum 50] [--creator "Westland"]
  Zonder creator: elke gemeente met minstens --minimum records met fase_titel besluit of onbepaald in die jaargang.
"""
import datetime as dt
import gzip
import re
import sqlite3
import sys

from config import CQL_RUBRIEK, DB_PAD, RUW_DIR
from parse import zorg_kolom
from sru import pagina, tel

RANG = {"geweigerd": 0, "ingetrokken": 1, "vergunningvrij": 2, "verleend": 3}

# landelijk: (klasse, frase)
FRASEN = [
    ("geweigerd", "geweigerd"),
    ("ingetrokken", "buiten behandeling"),
    ("ingetrokken", "ingetrokken"),
    ("ingetrokken", "niet-ontvankelijk"),
    ("vergunningvrij", "vergunningvrij"),
    ("verleend", "verleend"),
]

# per gemeente: impliciete formuleringen en vaste regels
FRASEN_PER_CREATOR = {
    "Westland": [("verleend", "ligt tijdens de bezwaartermijn ter inzage")],
    "Dordrecht": [("verleend", "Bent u het niet eens met de vergunning")],
    "Huizen": [("verleend", "Het besluit om de vergunning te verlenen")],
    "'s-Hertogenbosch": [("verleend", "Omgevingsvergunning verleend en treedt direct in werking")],
    "Eindhoven": [("verleend", "Besluit: Verleend"), ("geweigerd", "Besluit: Geweigerd"),
                  ("ingetrokken", "Besluit: Buiten behandeling"), ("ingetrokken", "Besluit: Ingetrokken")],
    "Amsterdam": [("verleend", "Besluit: verleend"), ("verleend", "Besluit: gedeeltelijk verleend"),
                  ("geweigerd", "Besluit: geweigerd"), ("ingetrokken", "Besluit: buiten behandeling"),
                  ("ingetrokken", "Besluit: ingetrokken")],
}

_ID = re.compile(rb"<dcterms:identifier>([^<]+)</dcterms:identifier>")


def frasen_voor(creator: str):
    return FRASEN + FRASEN_PER_CREATOR.get(creator, [])


def haal_identifiers(cql: str, creator: str, jaargang: int, frase: str):
    """Alle identifiers voor een frase; pagina's als gzip bewaard onder ruw/frasen zodat de oogst herhaalbaar is."""
    doel = RUW_DIR / "frasen" / str(jaargang)
    doel.mkdir(parents=True, exist_ok=True)
    naam = re.sub(r"[^A-Za-z0-9]+", "_", f"{creator}_{frase}")[:80]
    ids, start, nr, verwacht = [], 1, 1, tel(cql)
    while len(ids) < verwacht:
        data = pagina(cql, start, schema="http://standaarden.overheid.nl/short")
        gevonden = [i.decode() for i in _ID.findall(data)]
        if not gevonden:
            break
        with gzip.open(doel / f"{naam}_p{nr}.xml.gz", "wb") as f:
            f.write(data)
        ids += gevonden
        start += len(gevonden)
        nr += 1
    return ids, verwacht


def kandidaten(conn, jaargang: int, minimum: int, creator):
    if creator:
        return [creator]
    rijen = conn.execute("""SELECT creator, COUNT(*) n FROM publicatie WHERE jaargang=? AND organisatietype='gemeente'
                            AND fase_titel IN ('besluit','onbepaald') GROUP BY 1 HAVING n >= ? ORDER BY n DESC""",
                         (jaargang, minimum)).fetchall()
    return [r[0] for r in rijen]


def oogst(conn, jaargang: int, minimum: int, creator):
    nu = dt.datetime.now().isoformat(timespec="seconds")
    for cr in kandidaten(conn, jaargang, minimum, creator):
        for klasse, frase in frasen_voor(cr):
            klaar = conn.execute("SELECT status FROM frase_oogst WHERE creator=? AND jaargang=? AND frase=?", (cr, jaargang, frase)).fetchone()
            if klaar and klaar[0] == "ok":
                continue
            cql = f'{CQL_RUBRIEK} and dt.creator=="{cr}" and w.jaargang=={jaargang} and cql.textAndIndexes="{frase}"'
            try:
                ids, verwacht = haal_identifiers(cql, cr, jaargang, frase)
                status = "ok" if len(ids) == verwacht else f"onvolledig {len(ids)} van {verwacht}"
            except Exception as e:
                ids, verwacht, status = [], None, f"fout: {str(e)[:150]}"
            conn.executemany("INSERT OR REPLACE INTO publicatie_frase VALUES (?,?,?,?,?,?)",
                             [(i, frase, klasse, cr, jaargang, nu) for i in ids])
            conn.execute("INSERT OR REPLACE INTO frase_oogst VALUES (?,?,?,?,?,?,?)", (cr, jaargang, frase, verwacht, len(ids), status, nu))
            conn.commit()
            print(f"{cr} {jaargang} {frase!r}: {len(ids)} ({status})", flush=True)


def toepassen(conn):
    """fase_tekst: de klasse met de hoogste voorrang onder de gevonden frasen; alleen waar de titel zweeg."""
    zorg_kolom(conn, "publicatie", "fase_tekst", "TEXT")
    beste = {}
    for ident, klasse in conn.execute("SELECT identifier, klasse FROM publicatie_frase"):
        if ident not in beste or RANG[klasse] < RANG[beste[ident]]:
            beste[ident] = klasse
    conn.execute("UPDATE publicatie SET fase_tekst = NULL")
    conn.executemany("UPDATE publicatie SET fase_tekst = ? WHERE identifier = ? AND fase_titel IN ('besluit','onbepaald')",
                     [(k, i) for i, k in beste.items()])
    conn.commit()
    print("fase_tekst toegepast:", conn.execute("SELECT fase_tekst, COUNT(*) FROM publicatie WHERE fase_tekst IS NOT NULL GROUP BY 1").fetchall())


def main(argv):
    jaargang = int(argv[0]) if argv and not argv[0].startswith("--") else dt.date.today().year
    minimum = int(argv[argv.index("--minimum") + 1]) if "--minimum" in argv else 50
    creator = argv[argv.index("--creator") + 1] if "--creator" in argv else None
    conn = sqlite3.connect(DB_PAD, timeout=120)
    zorg_kolom(conn, "publicatie", "fase_tekst", "TEXT")
    if "--alleen-toepassen" not in argv:
        oogst(conn, jaargang, minimum, creator)
    toepassen(conn)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
