# Vergunningen: een ruimtelijk expliciete database van bouwvergunningen

Zelfstandig spoor naast de BAG-analyses in `R/` en `cfg/`: alle officiële bekendmakingen van omgevingsvergunningen (aanvraag, verlenging, besluit, intrekking) met hun geometrie, zo ver mogelijk terug in de tijd, gekoppeld aan de BAG-levensloop, aan natuurvergunningen en aan de CBS-gemeentetotalen. Aanleiding: de stikstofvraag (is er sinds de PAS-uitspraak van 29 mei 2019 minder aangevraagd, vergund of gebouwd, en vooral nabij Natura 2000) is met de BAG-status Bouwvergunning verleend alleen aan de kant van de gehaalde vergunningen te beantwoorden.

## Waar wat staat

In deze repo alleen de code en de documentatie: `README.md`, `CLAUDE.md`, `inzichten.md`, `REPRODUCEREN.md`, `PROMPT.md` en `pipeline/`. Alle data en alle onderzoeksstukken staan in de OneDrive-projectmap `202505-StikstofPaper\vergunningen\` (gesynchroniseerd tussen de machines): `ruw\` (de ruwe antwoorden per publicatiedag en de uitspraken), `bron\` (de habitatkaart en de afstandsrasters, en per thema een map met open data en een `BRONNEN.md`: markt, natuur, plannen, restricties, beleid, netcongestie, waterbodem, gebouwen, corporaties, rijksinstrumenten, bouwcapaciteit; de catalogus over alle thema's is `bron\CATALOGUS.md`), `natuur\` (de oogst van de provinciale natuurvergunningen met eigen SQLite), `vergunningen.sqlite`, `vergunningen.gpkg`, `analyse\` (analysebestanden, modeluitkomsten, rapporttekst en rapport), `uitvoer\` (meetuitvoer), `onderzoek\` (de bronrapporten, weerleggingen en het ontwerp van 25 september 2026) en `data_repo\` (de CBS- en SRU-reeksen). Op een andere machine: zet de omgevingsvariabele `VERGUNNINGEN_DATA` op die map, of kopieer hem naar `C:\LocalData\vergunningen`.

## Inhoud

| Pad | Wat |
|---|---|
| `inzichten.md` | De inzichten van 25 en 26 september 2026: definities, regimetijdlijn, antwoorden op de vijf startvragen, CBS-reeksen, bronnen, toegang en etiquette, werkafspraken, de uitkomsten van de bekendmakingen (hoofdstuk 12), het BAG-spoor sinds 2012 (hoofdstuk 14), de trendtoetsen en de strenge vergelijking (hoofdstukken 24 en 25), de grote projecten (hoofdstuk 26) en de reproductie (hoofdstuk 27). Begin hier. |
| `CLAUDE.md` | Werkafspraken, methode en valkuilen voor een Claude-sessie in deze map. |
| `REPRODUCEREN.md` | Installatie, instellingen, de drie niveaus van reproductie (rapport uit de csv's, keten vanaf de databases, oogst vanaf nul), lange processen en de controlegetallen. |
| `PROMPT.md` | De openingsprompt waarmee een eigen Claude op een andere pc alles inleest, de analyses reproduceert en verder werkt. |
| `pipeline/` | De werkende pijplijn (Python 3.13, standaardbibliotheek; QGIS-python voor rasters en GeoPackage; R met fixest voor de modellen): oogst per publicatiedag met ruwe opslag in vier stromen (rubriek, andere, oud, natuur), parser naar SQLite met geometrie, titelregels en frasen voor de fase, zaakvorming, afstand en blootstelling, export, analyse van de zaken (`analyse.R`) en van de BAG-cohorten (`bag_export.py`, `analyse_bag.R`), rechtspraak, rapportgenerator. Zie `pipeline/README.md`; data buiten de repo. |

## Verder werken op een andere machine

Zie `REPRODUCEREN.md` voor de installatie en de volgorde, en `PROMPT.md` voor de openingsprompt van een nieuwe Claude-sessie. In het kort: Python 3.13 (standaardbibliotheek volstaat voor de oogst: urllib, sqlite3, xml.etree), GDAL voor GeoPackage en coördinaattransformaties (op de onderzoeksmachine via `python-qgis.bat` van QGIS 3.38). Geen PostGIS nodig.

De ingang is `https://repository.overheid.nl/sru`; zie `inzichten.md` hoofdstuk 8 voor indexen, paginering (per dag op `dt.available`, nooit voorbij startRecord 10.000) en etiquette (hoogstens een verzoek per seconde, tellingen met maximumRecords=0, een volledige oogst vooraf bij KOOP melden). Vanaf sommige machines faalt curl op de overheidshosts; urllib werkt.

De BAG komt uit de levering `Vastgoed\VolledigeTabel_20260710` (alle voorkomens met begin- en einddatum) of uit het PDOK-extract met historie; raak de Nextcloud-bronshare niet aan met listings of scripts (`inzichten.md` hoofdstuk 9).

## Herkomst

Onderzoek en teksten van 25 en 26 september 2026, gemaakt in een Claude Code-sessie op OVSRV09 met een workflow van zeven bronagenten, veertien weerleggers, een ontwerpagent, een volledigheidscriticus en vier aanvullingsagenten, gevolgd door de bouw van de pijplijn, de oogst en de eerste analyse in de nacht erna. De bronrapporten en het ontwerp staan in de OneDrive-projectmap onder `vergunningen\onderzoek\`; het rapport met de eerste uitkomsten is gepubliceerd als artifact (link in `inzichten.md`, hoofdstuk 12).
