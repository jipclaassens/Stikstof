"""SRU-client voor repository.overheid.nl: tellen, pagineren en ruwe antwoorden bewaren.

Drie afspraken die de dienst en de herhaalbaarheid beschermen: hoogstens een verzoek per seconde, drie pogingen met
oplopende wachttijd bij netwerkfouten en 5xx, en elk antwoord gaat als gzip naar schijf voordat er iets mee gebeurt,
zodat elke parse herhaalbaar is zonder de dienst opnieuw te belasten.
"""
import gzip
import re
import time
import urllib.error
import urllib.parse
import urllib.request

from config import MIN_INTERVAL_S, PAGINA, RUW_DIR, SRU_URL, TIMEOUT_S, USER_AGENT

_laatste_verzoek = 0.0
_NUMREC = re.compile(rb"<sru:numberOfRecords>(\d+)</sru:numberOfRecords>")
_NEXTPOS = re.compile(rb"<sru:nextRecordPosition>(\d+)</sru:nextRecordPosition>")
_DIAG = re.compile(rb"<diag:message>([^<]*)</diag:message>")


def _wacht():
    global _laatste_verzoek
    nu = time.monotonic()
    if nu - _laatste_verzoek < MIN_INTERVAL_S:
        time.sleep(MIN_INTERVAL_S - (nu - _laatste_verzoek))
    _laatste_verzoek = time.monotonic()


def verzoek(params: dict, pogingen: int = 3) -> bytes:
    url = SRU_URL + "?" + urllib.parse.urlencode(params, quote_via=urllib.parse.quote)
    req = urllib.request.Request(url, headers={"User-Agent": USER_AGENT, "Accept": "application/xml"})
    wachttijd = 5
    for poging in range(1, pogingen + 1):
        _wacht()
        try:
            with urllib.request.urlopen(req, timeout=TIMEOUT_S) as antwoord:
                data = antwoord.read()
            m = _DIAG.search(data)
            if m:
                raise RuntimeError(f"SRU-diagnostic: {m.group(1).decode('utf-8', 'replace')} bij {url}")
            return data
        except (urllib.error.HTTPError, urllib.error.URLError, TimeoutError, ConnectionError) as e:
            code = getattr(e, "code", None)
            if poging == pogingen or (code is not None and 400 <= code < 500):
                raise
            time.sleep(wachttijd)
            wachttijd *= 3
    raise RuntimeError("onbereikbaar")


def tel(cql: str) -> int:
    data = verzoek({"version": "2.0", "operation": "searchRetrieve", "query": cql, "maximumRecords": 0})
    m = _NUMREC.search(data)
    if not m:
        raise RuntimeError("geen numberOfRecords in het antwoord")
    return int(m.group(1))


def pagina(cql: str, start: int, n: int = PAGINA, schema: str | None = None) -> bytes:
    p = {"version": "2.0", "operation": "searchRetrieve", "query": cql, "startRecord": start, "maximumRecords": n}
    if schema:
        p["recordSchema"] = schema
    return verzoek(p)


def oogst_dag(dag: str, cql_basis: str, map_naam: str = "rubriek") -> dict:
    """Haalt alle records met dt.available gelijk aan dag op en schrijft elke pagina als gzip weg.

    De dagpartitie sluit (de 31 dagtellingen van maart 2025 sommeren exact tot de maandtelling) en blijft ruim onder
    de grens van 10.000 waar de dienst 504 geeft (grootste dag in 2025: 2.372 records).
    """
    cql = f"{cql_basis} and dt.available=={dag}"
    verwacht = tel(cql)
    doel = RUW_DIR / map_naam / dag[:4]
    doel.mkdir(parents=True, exist_ok=True)
    bestanden, ontvangen, start, nr = [], 0, 1, 1
    while ontvangen < verwacht:
        data = pagina(cql, start)
        aantal = data.count(b"<sru:recordPosition>")
        if aantal == 0:
            break
        pad = doel / f"{dag}_p{nr}.xml.gz"
        with gzip.open(pad, "wb") as f:
            f.write(data)
        bestanden.append(str(pad))
        ontvangen += aantal
        nr += 1
        m = _NEXTPOS.search(data)
        if not m:
            break
        start = int(m.group(1))
    return {"dag": dag, "cql": cql, "verwacht": verwacht, "ontvangen": ontvangen, "paginas": nr - 1, "bestanden": bestanden}
