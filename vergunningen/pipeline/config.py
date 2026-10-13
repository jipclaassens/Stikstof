"""Instellingen van de vergunningenpijplijn.

Data staat buiten de repo (ruwe XML, SQLite, GeoPackage); alles is te overschrijven met omgevingsvariabelen,
zodat dezelfde code op een andere machine met een andere datamap draait.
"""
import os
from pathlib import Path

DATA_DIR = Path(os.environ.get("VERGUNNINGEN_DATA", r"C:\LocalData\vergunningen"))
RUW_DIR = Path(os.environ.get("VERGUNNINGEN_RUW", DATA_DIR / "ruw"))
DB_PAD = Path(os.environ.get("VERGUNNINGEN_DB", DATA_DIR / "vergunningen.sqlite"))  # apart te zetten, voor een werkkopie

SRU_URL = "https://repository.overheid.nl/sru"
USER_AGENT = "vergunningendatabase-stikstof/0.1 (onderzoek; https://github.com/jipclaassens/Stikstof)"
MIN_INTERVAL_S = float(os.environ.get("VERGUNNINGEN_INTERVAL", "1.0"))  # hoogstens een verzoek per seconde (fair use)
TIMEOUT_S = 120
PAGINA = 1000  # maximumRecords per verzoek; startRecord boven 10.000 geeft HTTP 504, vandaar de partitie per dag

# De hele rubriek, niet alleen het label bouwen: 33 gemeenten labelen bouw als ruimtelijke ordening en het label
# daalt over de jaren terwijl de rubriek dat niet doet (inzichten.md, hoofdstuk 4). Bouw komt later uit titel en tekst.
CQL_RUBRIEK = "dt.type==omgevingsvergunning"

# Tweede stroom: gemeenten die hun besluiten onder "andere vergunning" of "andere beschikking" zetten (gemeten 2024:
# Gooise Meren 216 van 242 verleend-titels, Midden-Groningen 241 van 602, Rijssen-Holten 444 titels; landelijk 45.034 van
# 102.927 verleend-titels buiten de rubriek, deels APV en horeca). De zaakvorming filtert op titel en koppeling.
STROMEN = {
    "rubriek": CQL_RUBRIEK,
    "andere": '(dt.type=="andere vergunning" or dt.type=="andere beschikking")',
    # Voor de Wet elektronische publicaties (tot 30 juni 2021): de oude informatietypen van het Gemeenteblad en het
    # Provinciaal blad; per gemeente alles of niets (103 gemeenten in 2014, 303 medio 2021). Records dragen een
    # RD-punt en postcode plus huisnummer; het informatietype is een grove fase (aanvraag of afhandeling).
    "oud": '(dt.type=="Beschikkingen | aanvraag" or dt.type=="Beschikkingen | afhandeling")',
    # Natuurvergunningen en -ontheffingen: de provincies (Provinciaal blad) en het Rijk (Staatscourant) als bevoegd
    # gezag onder de Natuurbeschermingswet 1998, de Flora- en faunawet, de Wet natuurbescherming (2017) en de
    # Natura 2000-activiteit onder de Omgevingswet (2024). Geen open register bestaat; dit is de enige landelijke bron.
    # Draait in een eigen datamap (VERGUNNINGEN_RUW en VERGUNNINGEN_DB), los van de gemeentelijke database.
    # Let op: "any" splitst op woorden, dus een frase met "en" erin matcht alles; daarom losse woorden. Sinds 2024
    # gaat ook de hele rubriek omgevingsvergunning van het Provinciaal blad mee (7.801 in 2024, 12.962 in 2025), omdat
    # de provincies hun Natura 2000- en flora- en fauna-activiteiten daar zonder vaste titelwoorden in zetten.
    "natuur": '((((w.publicatienaam=="Provinciaal blad" or w.publicatienaam=="Staatscourant") and '
              '(dt.title any "natuurbescherming" or dt.title any "Natuurbeschermingswet" or dt.title any "Natura" or '
              'dt.title any "natuurvergunning" or dt.title any "stikstof" or dt.title any "faunawet" or '
              'dt.title any "fauna-activiteit")) or (w.publicatienaam=="Provinciaal blad" and dt.type==omgevingsvergunning)) '
              'or dt.title any "Natura")',
    # Kennisgevingen van ruimtelijke plannen en omgevingsdocumenten (ontwerp, vaststelling, inwerkingtreding van
    # bestemmingsplannen en wijzigingen van omgevingsplannen) in alle bladen: tot medio 2021 meldden gemeenten
    # bestemmingsplannen ook in de Staatscourant (7.000 tot 8.300 per jaar), sindsdien in het Gemeenteblad. Draait
    # in een eigen datamap, zoals de natuurstroom (REPRODUCEREN.md).
    "ruimtelijk": 'dt.type=="ruimtelijk plan of omgevingsdocument"',
}
