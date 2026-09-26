"""Laad de ruwe gzip-antwoorden in de SQLite-database.

Gebruik: py -3.13 laad.py                 laadt elk bestand onder de ruwe map dat nog niet geladen is
         py -3.13 laad.py --herclassificeer   past alleen fase_titel opnieuw toe op alle titels (na een wijziging in fase.py)
"""
import datetime as dt
import sqlite3
import sys

from config import DB_PAD, RUW_DIR, STROMEN
from fase import classificeer_titel
from parse import SCHEMA, parse_bestand, schrijf, zorg_kolom


def herclassificeer(conn):
    rijen = conn.execute("SELECT identifier, titel FROM publicatie").fetchall()
    conn.executemany("UPDATE publicatie SET fase_titel = ? WHERE identifier = ?",
                     [(classificeer_titel(t or ""), i) for i, t in rijen])
    conn.commit()
    telling = conn.execute("SELECT fase_titel, COUNT(*) FROM publicatie GROUP BY 1 ORDER BY 2 DESC").fetchall()
    for fase, n in telling:
        print(f"{n:8d} {100 * n / len(rijen):5.1f}%  {fase}")
    return 0


def main(argv):
    conn = sqlite3.connect(DB_PAD, timeout=120)  # oogst.py kan tegelijk schrijven; wachten in plaats van falen
    conn.executescript(SCHEMA)
    zorg_kolom(conn, "publicatie", "informatietype", "TEXT")  # bestaande databases van voor de oude stroom
    zorg_kolom(conn, "publicatie", "fase_tekst", "TEXT")
    if "--herclassificeer" in argv:
        return herclassificeer(conn)
    geladen = {r[0] for r in conn.execute("SELECT bestand FROM bestand_geladen")}
    # alleen de publicatiestromen; onder ruw staan ook de frasenpagina's (schema short) en de uitspraken, die hier niet horen
    bestanden = sorted(p for stroom in STROMEN for p in (RUW_DIR / stroom).rglob("*.xml.gz") if str(p) not in geladen)
    nu = dt.datetime.now().isoformat(timespec="seconds")
    totaal = 0
    for pad in bestanden:
        n = 0
        try:
            for pub, activiteiten, locaties in parse_bestand(pad):
                schrijf(conn, pub, activiteiten, locaties, nu)
                n += 1
        except Exception as e:  # een halfgeschreven pagina van een afgebroken oogst; de dag komt bij een herstart opnieuw
            conn.rollback()
            print(f"{pad.name}: overgeslagen, {type(e).__name__}: {str(e)[:120]}")
            continue
        conn.execute("INSERT OR REPLACE INTO bestand_geladen VALUES (?, ?, ?)", (str(pad), n, nu))
        conn.commit()
        totaal += n
        print(f"{pad.name}: {n} records")
    print(f"{len(bestanden)} bestanden, {totaal} records geladen; totaal in database: "
          f"{conn.execute('SELECT COUNT(*) FROM publicatie').fetchone()[0]}")
    conn.close()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
