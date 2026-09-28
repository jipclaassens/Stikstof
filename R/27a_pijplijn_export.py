"""Exporteert uit de vergunningendatabase wat nodig is om zaken aan de sites van het herontwikkelingsonderzoek te koppelen
(issue 30 in de repo Redevelopment): de BAG-koppeling met alle statusdatums, en per gemeentelijke zaak de sleutel, de
datums, de uitkomst, de locatie, de activiteiten, woorden uit de titels en het beroep.

Gebruik: py -3.13 R/27a_pijplijn_export.py <uitvoermap> [datamap]

Dit script leest alleen: de database wordt geopend als immutable, zoals in 17a_pand_zaak.py. Draai het niet terwijl het
vergunningenspoor de database bijwerkt.

<uitvoermap>/pijplijn_pand_zaak.csv  pand_id;zaak_id;afstand_m;n_woon;verg_begin;start_begin;gebruik_begin;niet_gerealiseerd_begin
    de tabel zaak_bag van koppel_bag.py: panden met de BAG-status Bouwvergunning verleend vanaf juni 2021, gekoppeld aan de
    zaak binnen 100 meter waarvan het tijdvenster de vergunningdatum omvat, met de eerste datum van elke latere status.
<uitvoermap>/pijplijn_zaak.csv       een rij per gemeentelijke zaak:
    zaak_id, creator, sleutelsoort, sleutel (bij zaaknummer het zaaknummer), eerste_datum, datum_aanvraag, datum_uitkomst,
    uitkomst, doorlooptijd_dagen, n_verlengingen, n_publicaties;
    locatietype, x, y en breedte_m: de eerste locatie van de eerste publicatie, die zaak.py als plek van de zaak neemt, met
    haar uitgestrektheid; n_zelfde_punt = het aantal zaken van dezelfde gemeente op precies dat punt (een hoog aantal wijst op
    een standaardpunt, zoals het gemeentehuis);
    act_bouwen, act_slopen, act_ro: een publicatie van de zaak heeft die activiteit in de metadata (ro = ruimtelijke ordening);
    t_slopen, t_bouwen, t_woning: een titel van de zaak noemt slopen of sloop, bouwen of oprichten of realiseren of nieuwbouw,
    en een woning, woonhuis, appartement of wooneenheid;
    n_uitspraken en max_score: kandidaatuitspraken van de Raad van State uit rechtspraak.py koppel (hoogstens drie per uitspraak);
    eerste_identifier: de eerste publicatie, een sleutel die een nieuwe zaakvorming overleeft;
    titels: bij een zaak met slopen, of met bouwen en woning, in een titel alle verschillende titels van de zaak, gescheiden
    door | en afgekapt op 600 tekens; anders leeg.
"""
import collections
import csv
import os
import re
import sqlite3
import sys
from pathlib import Path

uit = Path(sys.argv[1])
uit.mkdir(parents=True, exist_ok=True)
data = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(os.environ.get(
    "VERGUNNINGEN_DATA", r"E:\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"))
db = data / "vergunningen.sqlite"
if db.with_name(db.name + "-wal").exists():
    raise SystemExit(f"{db}-wal bestaat: de database wordt bijgewerkt, probeer het later")
con = sqlite3.connect(f"file:{db.as_posix()}?immutable=1", uri=True)

# ---- de BAG-koppeling, met dezelfde controle als 17a_pand_zaak.py
fout = con.execute("""SELECT count(*) FROM zaak_bag b JOIN zaak z ON z.zaak_id = b.zaak_id
                      WHERE b.dagen_uitkomst_vergunning IS NOT NULL
                        AND b.dagen_uitkomst_vergunning != julianday(substr(z.datum_uitkomst, 1, 10)) - julianday(substr(b.verg_begin, 1, 10))""").fetchone()[0]
if fout:
    raise SystemExit(f"{fout} koppelingen passen niet bij de zaaktabel: draai koppel_bag.py opnieuw")
rijen = con.execute("""SELECT pand_id, zaak_id, afstand_m, n_woon, verg_begin, start_begin, gebruik_begin, niet_gerealiseerd_begin
                       FROM zaak_bag ORDER BY pand_id""").fetchall()
with open(uit / "pijplijn_pand_zaak.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["pand_id", "zaak_id", "afstand_m", "n_woon", "verg_begin", "start_begin", "gebruik_begin", "niet_gerealiseerd_begin"])
    w.writerows(rijen)
print(f"{len(rijen)} pandkoppelingen geschreven")

# ---- de gemeentelijke zaken
zaken = {}
for r in con.execute("""SELECT zaak_id, creator, sleutelsoort, sleutel, eerste_datum, datum_aanvraag, datum_uitkomst, uitkomst,
                               doorlooptijd_dagen, n_verlengingen, n_publicaties, locatietype, x, y
                        FROM zaak WHERE organisatietype = 'gemeente'"""):
    zaken[r[0]] = list(r)
print(f"{len(zaken)} gemeentelijke zaken")

# eerste publicatie per zaak (de volgorde van zaak.py: datum, dan identifier), titelwoorden
SLOPEN = re.compile(r"\bslo(?:pen|op)")
BOUWEN = re.compile(r"\b(?:bouwen|oprichten|realiseren|nieuwbouw)")
WONING = re.compile(r"woning|woonhuis|appartement|wooneenhe|woongebouw|woonblok")
eerste = {}
vlag = collections.defaultdict(lambda: [0, 0, 0])
titels = collections.defaultdict(list)
for zid, ident, datum, titel in con.execute("""SELECT pz.zaak_id, p.identifier, p.available, p.titel
                                                FROM publicatie_zaak pz JOIN publicatie p ON p.identifier = pz.identifier
                                                WHERE p.organisatietype = 'gemeente'"""):
    if zid not in zaken:
        continue
    k = (datum or "9999", ident)
    if zid not in eerste or k < eerste[zid]:
        eerste[zid] = k
    t = (titel or "").lower()
    v = vlag[zid]
    s, b, wo = bool(SLOPEN.search(t)), bool(BOUWEN.search(t)), bool(WONING.search(t))
    v[0] |= s
    v[1] |= b
    v[2] |= wo
    t = " ".join((titel or "").split())
    if t and t not in titels[zid]:
        titels[zid].append(t)
print(f"{len(eerste)} zaken met een publicatie")

breedte = {}
ids = {k[1]: zid for zid, k in eerste.items()}
for ident, bw in con.execute("SELECT identifier, uitgestrektheid_m FROM publicatie_locatie WHERE volgnr = 1"):
    if ident in ids:
        breedte[ids[ident]] = bw

act = collections.defaultdict(set)
for zid, a in con.execute("""SELECT pz.zaak_id, a.activiteit FROM publicatie_activiteit a
                             JOIN publicatie_zaak pz ON pz.identifier = a.identifier
                             WHERE a.activiteit IN ('bouwen', 'slopen', 'ruimtelijke ordening')"""):
    if zid in zaken:
        act[zid].add(a)

beroep = {zid: (n, s) for zid, n, s in con.execute("SELECT zaak_id, count(*), max(score) FROM uitspraak_zaak GROUP BY zaak_id")}
con.close()

punt = collections.Counter((r[1], r[12], r[13]) for r in zaken.values() if r[12] is not None)
with open(uit / "pijplijn_zaak.csv", "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f, delimiter=";")
    w.writerow(["zaak_id", "creator", "sleutelsoort", "sleutel", "eerste_datum", "datum_aanvraag", "datum_uitkomst", "uitkomst",
                "doorlooptijd_dagen", "n_verlengingen", "n_publicaties", "locatietype", "x", "y", "breedte_m", "n_zelfde_punt",
                "act_bouwen", "act_slopen", "act_ro", "t_slopen", "t_bouwen", "t_woning", "n_uitspraken", "max_score",
                "eerste_identifier", "titels"])
    for zid, r in zaken.items():
        v = vlag.get(zid, [0, 0, 0])
        tt = " | ".join(titels.get(zid, []))[:600] if (v[0] or (v[1] and v[2])) else ""
        a = act.get(zid, set())
        n, s = beroep.get(zid, (0, None))
        w.writerow(r + [breedte.get(zid), punt.get((r[1], r[12], r[13]), 0) if r[12] is not None else None,
                        int("bouwen" in a), int("slopen" in a), int("ruimtelijke ordening" in a),
                        int(v[0]), int(v[1]), int(v[2]), n, s, eerste[zid][1] if zid in eerste else None, tt])
print(f"{len(zaken)} zaken geschreven naar {uit / 'pijplijn_zaak.csv'}")
