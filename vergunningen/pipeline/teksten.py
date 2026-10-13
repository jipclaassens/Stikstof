"""Gerichte oogst van de volledige tekst van bekendmakingen, waar titel en samenvatting te weinig zeggen.

Gebruik: py -3.13 teksten.py wachtrij     stelt de wachtrij samen (alleen lezen uit de database)
         py -3.13 teksten.py oogst        haalt de teksten op, hervatbaar, hoogstens een verzoek per seconde
         py -3.13 teksten.py stand        telt wat er binnen is

De titel van een bekendmaking is een zwakke bron voor het soort werk, het aantal woningen en de procedure; de volledige
tekst staat per publicatie als XML in de repository van KOOP (url_xml in de tabel publicatie). Alles ophalen is 3,9
miljoen verzoeken, meer dan een maand; daarom een wachtrij in drie prioriteiten:
  1. zaken met een uitgebreide procedure (uitgebreid = 1 in analyse/zaken.csv): de aanvraag en het ontwerpbesluit,
     om te zien of er een natuurtoets of een verklaring van geen bedenkingen in zat;
  2. zaken van het soort nieuwbouw_woning zonder woningaantal uit de titel: de aanvraag;
  3. zaken met een gekoppeld nieuw BAG-pand (zaak_bag) en een titel zonder herkenbaar soort (overig): de aanvraag.
In prioriteit 2 en 3 worden publicaties waarvan de samenvatting al een woningaantal noemt, overgeslagen; prioriteit 1
gaat helemaal mee, want voor de natuurtoets telt de hele tekst. Binnen elke prioriteit staat de wachtrij in willekeurige
volgorde (vast zaad), zodat wat er op een willekeurig moment binnen is, een steekproef is en geen reeks gemeenten.

De teksten komen in een eigen SQLite-bestand buiten de gespiegelde datamap (standaard C:/LocalData/Stikstof/teksten,
of TEKSTEN_DIR): tabel tekst (identifier, prio, status, tekst als zlib, opgehaald_op). De database van bekendmakingen
wordt alleen gelezen (immutable). Start de oogst buiten de Claude-sessie (REPRODUCEREN.md), want die duurt een tot
twee dagen; ze hervat waar ze was.
"""
import csv
import datetime as dt
import os
import random
import re
import sqlite3
import sys
import time
import urllib.error
import urllib.request
import zlib
from html import unescape
from pathlib import Path

from config import DATA_DIR, DB_PAD

UIT_DIR = Path(os.environ.get("TEKSTEN_DIR", r"C:\LocalData\Stikstof\teksten"))
WACHTRIJ = UIT_DIR / "wachtrij.csv"
TEKSTEN_DB = UIT_DIR / "teksten.sqlite"
LOG = UIT_DIR / "oogst.log"
PAUZE_S = 1.05
ZAAD = 20260927
AGENT = "stikstof-woningbouw-onderzoek (VU Amsterdam, Object Vision); een verzoek per seconde"
_AANTAL = re.compile(r"\b(\d{1,4})\s*(?:\w+\s){0,3}(woningen|appartementen|wooneenheden|woningbouw)", re.I)
_PROCEDURE = re.compile(r"uitgebreide\s+(voorbereidings)?procedure|reguliere\s+(voorbereidings)?procedure", re.I)


def lees_db():
    return sqlite3.connect(f"file:{Path(DB_PAD).as_posix()}?immutable=1", uri=True)


def wachtrij():
    UIT_DIR.mkdir(parents=True, exist_ok=True)
    zaken = {}
    with open(DATA_DIR / "analyse" / "zaken.csv", encoding="utf-8", newline="") as f:
        for r in csv.DictReader(f, delimiter=";"):
            zaken[int(r["zaak_id"])] = r
    con = lees_db()
    bag = {z for (z,) in con.execute("SELECT DISTINCT zaak_id FROM zaak_bag")}
    prio = {}
    for zid, r in zaken.items():
        if r["uitgebreid"] == "1":
            prio[zid] = 1
        elif r["soort"] == "nieuwbouw_woning" and not r["woningen"]:
            prio[zid] = 2
        elif r["soort"] == "overig" and zid in bag:
            prio[zid] = 3
    print(f"zaken per prioriteit: " + ", ".join(f"{p}: {sum(1 for v in prio.values() if v == p)}" for p in (1, 2, 3)), flush=True)
    rijen = []
    n_over = 0
    for ident, zid, fase_t, fase_x, url, abstract in con.execute("""
            SELECT p.identifier, pz.zaak_id, p.fase_titel, p.fase_tekst, p.url_xml, p.abstract
            FROM publicatie_zaak pz JOIN publicatie p ON p.identifier = pz.identifier"""):
        pr = prio.get(zid)
        if pr is None or not url:
            continue
        fase = fase_x or fase_t or ""
        if pr == 1 and fase not in ("aanvraag", "ontwerp"):
            continue
        if pr > 1 and fase != "aanvraag":
            continue
        if pr > 1 and abstract and _AANTAL.search(abstract):
            n_over += 1
            continue
        rijen.append((pr, zid, ident, url))
    con.close()
    rijen.sort()
    random.Random(ZAAD).shuffle(rijen)
    rijen.sort(key=lambda r: r[0])  # stabiel: per prioriteit de willekeurige volgorde
    with open(WACHTRIJ, "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f, delimiter=";")
        w.writerow(["prio", "zaak_id", "identifier", "url"])
        w.writerows(rijen)
    print(f"{len(rijen)} publicaties in de wachtrij ({n_over} overgeslagen omdat de samenvatting al genoeg zegt): {WACHTRIJ}")


def tekst_uit_xml(x):
    x = re.sub(r"<(script|style)[^>]*>.*?</\1>", " ", x, flags=re.S | re.I)
    x = re.sub(r"<[^>]+>", " ", x)
    return re.sub(r"\s+", " ", unescape(x)).strip()


def log(regel):
    with open(LOG, "a", encoding="utf-8") as f:
        f.write(f"{dt.datetime.now():%Y-%m-%d %H:%M:%S} {regel}\n")


def oogst():
    UIT_DIR.mkdir(parents=True, exist_ok=True)
    db = sqlite3.connect(TEKSTEN_DB, timeout=60)
    db.execute("PRAGMA journal_mode=WAL")
    db.execute("""CREATE TABLE IF NOT EXISTS tekst (identifier TEXT PRIMARY KEY, zaak_id INTEGER, prio INTEGER,
                  status INTEGER, tekst BLOB, opgehaald_op TEXT)""")
    klaar = {i for (i,) in db.execute("SELECT identifier FROM tekst WHERE status = 200 OR status = 404")}
    with open(WACHTRIJ, encoding="utf-8", newline="") as f:
        todo = [r for r in csv.DictReader(f, delimiter=";") if r["identifier"] not in klaar]
    log(f"start: {len(todo)} te doen, {len(klaar)} al binnen")
    n = fouten = 0
    vorige = 0.0
    buffer = []
    for r in todo:
        wacht = PAUZE_S - (time.monotonic() - vorige)
        if wacht > 0:
            time.sleep(wacht)
        vorige = time.monotonic()
        status, tekst = None, None
        for poging in range(4):
            try:
                req = urllib.request.Request(r["url"], headers={"User-Agent": AGENT})
                with urllib.request.urlopen(req, timeout=60) as resp:
                    status = resp.status
                    tekst = tekst_uit_xml(resp.read().decode("utf-8", errors="replace"))
                break
            except urllib.error.HTTPError as e:
                status = e.code
                if e.code in (404, 410):
                    break
                time.sleep(10 * (poging + 1))
            except Exception as e:  # netwerk: even wachten en opnieuw
                status = -1
                time.sleep(10 * (poging + 1))
        if status != 200:
            fouten += 1
        buffer.append((r["identifier"], int(r["zaak_id"]), int(r["prio"]), status,
                       zlib.compress(tekst.encode("utf-8")) if tekst else None, dt.datetime.now().isoformat(timespec="seconds")))
        n += 1
        if len(buffer) >= 50:
            db.executemany("INSERT OR REPLACE INTO tekst VALUES (?,?,?,?,?,?)", buffer)
            db.commit()
            buffer = []
        if n % 500 == 0:
            log(f"{n} van {len(todo)} opgehaald, {fouten} niet gelukt")
    if buffer:
        db.executemany("INSERT OR REPLACE INTO tekst VALUES (?,?,?,?,?,?)", buffer)
        db.commit()
    log(f"klaar: {n} opgehaald, {fouten} niet gelukt")
    db.close()


def stand():
    db = sqlite3.connect(TEKSTEN_DB)
    print(db.execute("SELECT prio, status, count(*) FROM tekst GROUP BY 1, 2 ORDER BY 1, 2").fetchall())
    db.close()


if __name__ == "__main__":
    {"wachtrij": wachtrij, "oogst": oogst, "stand": stand}[sys.argv[1]]()
