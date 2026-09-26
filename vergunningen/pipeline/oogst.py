"""Oogst de rubriek omgevingsvergunning per publicatiedag.

Gebruik: py -3.13 oogst.py 2025-03-03 [2025-03-07] [--stroom andere] [--opnieuw]
Een dag of een bereik (beide grenzen inbegrepen). Dagen die al met status ok in de tabel oogst staan worden
overgeslagen, dus het commando is herhaalbaar; met --opnieuw worden ze toch opgehaald. Weekend en feestdagen geven
nul records en kosten een verzoek. De stroom kiest de query uit config.STROMEN (standaard de rubriek
omgevingsvergunning; "andere" haalt de rubrieken andere vergunning en andere beschikking, waar sommige gemeenten
hun besluiten zetten) en bepaalt de map onder ruw en de sleutel in de tabel oogst.
"""
import datetime as dt
import sqlite3
import sys
import time

from config import DATA_DIR, DB_PAD, STROMEN
from parse import SCHEMA
from sru import oogst_dag


def dagen(van: str, tot: str):
    d = dt.date.fromisoformat(van)
    einde = dt.date.fromisoformat(tot)
    while d <= einde:
        yield d.isoformat()
        d += dt.timedelta(days=1)


def main(argv):
    opnieuw = "--opnieuw" in argv
    stroom = argv[argv.index("--stroom") + 1] if "--stroom" in argv else "rubriek"
    argv = [a for i, a in enumerate(argv) if not a.startswith("--") and (i == 0 or argv[i - 1] != "--stroom")]
    if not argv or stroom not in STROMEN:
        print(__doc__)
        return 2
    cql_basis = STROMEN[stroom]
    van, tot = argv[0], argv[1] if len(argv) > 1 else argv[0]
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(DB_PAD, timeout=120)  # laad.py kan tegelijk schrijven; wachten in plaats van falen
    conn.executescript(SCHEMA)
    for dag in dagen(van, tot):
        rij = conn.execute("SELECT status, ontvangen FROM oogst WHERE dag = ? AND map_naam = ?", (dag, stroom)).fetchone()
        if rij and rij[0] == "ok" and not opnieuw:
            print(f"{dag} al geoogst ({rij[1]} records)")
            continue
        t0 = time.monotonic()
        try:
            v = oogst_dag(dag, cql_basis, stroom)
            status = "ok" if v["ontvangen"] == v["verwacht"] else f"onvolledig {v['ontvangen']} van {v['verwacht']}"
        except Exception as e:  # netwerk of dienst; de dag blijft open voor een volgende poging
            v = {"cql": None, "verwacht": None, "ontvangen": None, "paginas": None}
            status = f"fout: {str(e)[:200]}"
        conn.execute(
            "INSERT OR REPLACE INTO oogst VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
            (dag, stroom, v["cql"], v["verwacht"], v["ontvangen"], v["paginas"], status,
             dt.datetime.now().isoformat(timespec="seconds"), round(time.monotonic() - t0, 1)),
        )
        conn.commit()
        print(f"{dag} {status}: {v['ontvangen']} records in {v['paginas']} pagina's, {time.monotonic() - t0:.1f} s")
    conn.close()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
