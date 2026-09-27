# Reproduceren op een andere pc

Deze map bevat de code; de data staat in de OneDrive-projectmap `202505-StikstofPaper\vergunningen\`. Er zijn drie niveaus van reproductie, van minuten tot dagen. Niveau 1 is getest op 26 september 2026 met de datamap op OneDrive: de vijf uitkomstbestanden waren identiek aan de oorspronkelijke run.

| Niveau | Wat | Commando | Duur | Nodig |
|---|---|---|---|---|
| 1 | Modellen en rapport uit de analysebestanden | `verwerk.cmd analyse` | een tot enkele minuten | Python, R, de map `analyse\` plus twee bestanden voor het rapport |
| 2 | De hele keten vanaf de databases | `verwerk.cmd` | enkele uren | daarnaast QGIS, de databases en `bron\`, internet voor de frasestap |
| 3 | Alles vanaf nul, met nieuwe oogst | zie onder | dagen | internet, de bronnen volgens de `BRONNEN.md` per themamap |

## Installatie

- Python 3.13 met de `py`-launcher (python.org of `winget install Python.Python.3.13`). De pijplijn gebruikt alleen de standaardbibliotheek.
- R 4.6 met drie pakketten: `install.packages(c("fixest", "data.table", "jsonlite"))`. Getest met R 4.6.1, fixest 0.14.2, data.table 1.18.6.1 en jsonlite 2.0.0.
- Alleen voor niveau 2 en 3: QGIS 3.38 (standalone of OSGeo4W). De rasterstappen draaien onder `python-qgis.bat` met GDAL 3.9.1, numpy 1.26.4 en scipy 1.13.0: `afstand.py`, `blootstelling.py`, `export_gpkg.py`, `bag_export.py` en `verrijk.py`.
- Schijfruimte: de hele datamap is ongeveer 47 GB (`bron\` 33,5 GB, `vergunningen.sqlite` 6,6 GB, `vergunningen.gpkg` 4,5 GB, `ruw\` 1,5 GB, `analyse\` 0,45 GB, `natuur\` 0,2 GB). Voor niveau 1 volstaan `analyse\`, `bron\rechtspraak_verdieping\` en `natuur\natuur_woningbouw_per_jaar.csv`. Met OneDrive-bestanden op aanvraag kun je alleen die mappen op "altijd op dit apparaat" zetten; de rest komt binnen zodra een script hem leest.

## Instellen

In een opdrachtprompt, eenmalig (`setx` blijft staan voor nieuwe vensters; pas de paden aan de eigen installatie aan):

```
setx VERGUNNINGEN_DATA "C:\Users\<gebruiker>\OneDrive - Objectvision\VU\Projects\202505-StikstofPaper\vergunningen"
setx RSCRIPT "C:\Program Files\R\R-4.6.1\bin\Rscript.exe"
setx QGIS_PY "C:\Program Files\QGIS 3.38.1\bin\python-qgis.bat"
```

`config.py` leest `VERGUNNINGEN_DATA` (en voor de natuurstroom `VERGUNNINGEN_RUW` en `VERGUNNINGEN_DB`); `verwerk.cmd` en `oogst_reeks.cmd` lezen alle drie. De R-scripts krijgen de analysemap als argument.

## Niveau 1: modellen en rapport

```
cd <repo>\vergunningen\pipeline
verwerk.cmd analyse
```

Dit draait `analyse.R`, `analyse_bag.R`, `analyse_blootstelling.R`, `analyse_vastpanel.R` en `analyse_projecten.R` op de csv's in `analyse\`, en daarna `rapport.py`. Uitvoer in `analyse\`: vijf json's (`resultaten`, `bag_resultaten`, `blootstelling_resultaten`, `vastpanel_resultaten`, `projecten_resultaten`), een csv per tabel en `rapport.html`. Het log staat in `verwerk.log` in de datamap. Vergelijk de uitkomst met de controlegetallen hieronder. De interpretatie in het rapport komt uit `analyse\teksten.md` en is met de hand geschreven; na een wijziging in de modellen moeten de getallen daar mee.

## Niveau 2: de keten vanaf de databases

```
verwerk.cmd
```

Volgorde en invoer: `laad.py` (nieuwe ruwe bestanden in `ruw\` naar `vergunningen.sqlite`), `frasen.py` per jaargang 2014 tot 2026 (tekstfrasen via de SRU, ongeveer 700 verzoeken per jaargang, dus ruim twee uur bij een verzoek per seconde), `zaak.py` (zaakvorming, nummert de zaken opnieuw), `rechtspraak.py koppel`, de QGIS-stappen `afstand.py` en `blootstelling.py` (met `bron\aerius_relevant_habitats.gpkg` en `bron\natuur\aerius_hexagonen_overbelasting.gpkg`) en `export_gpkg.py`, `meet.py`, `analyse_export.py`, `bag_export.py` (met `bron\gebouwen\bag_pand_historie.sqlite`, 10,8 GB), `koppel_bag.py`, `cbs_toets.py`, `valideer_registers.py`, `verrijk.py` (kaartlagen uit `bron\depositie`, `bron\ftm_stallen`, `bron\stikstofbeleid`, `bron\waterbodem`, `bron\afgeleid`), `projecten.py` (met `natuur\natuurvergunningen.sqlite` en `bron\rechtspraak_verdieping\uitspraken_verdiept.csv`) en dan niveau 1. Omdat de frasestap de SRU live bevraagt, kunnen de uitkomsten iets verschuiven als er sinds september 2026 publicaties bijkwamen.

## Niveau 3: oogst en bronnen vanaf nul

In deze volgorde, elk hervatbaar (al geoogste dagen worden overgeslagen). Meld een volledige oogst vooraf bij KOOP (`inzichten.md`, hoofdstuk 8). Gebruik als einddatum de laatste volledige publicatiedag, nooit een datum in de toekomst: een dag zonder records telt als geoogst en wordt daarna overgeslagen, tenzij je hem met `--opnieuw` opnieuw haalt. In de voorbeelden staat 2026-09-25, de stand van de eerste oogst.

```
oogst_reeks.cmd 2026-09-25
py -3.13 oogst.py 2021-07-01 2026-09-25 --stroom andere
py -3.13 oogst.py 2013-01-01 2021-06-30 --stroom oud
py -3.13 rechtspraak.py index 2010 2026
py -3.13 rechtspraak.py tekst
py -3.13 rechtspraak.py herclassificeer
py -3.13 laad.py
py -3.13 tekstvlag.py
```

De eerste oogst van de rubriek sinds juli 2021 deed 1.911 publicatiedagen (1,76 miljoen records), de stroom oud 3.103 dagen (1,60 miljoen). De natuurstroom heeft een eigen database; draai hem in een apart venster, zodat de variabelen niet in de andere stappen terechtkomen:

```
set "VERGUNNINGEN_RUW=%VERGUNNINGEN_DATA%\natuur\ruw"
set "VERGUNNINGEN_DB=%VERGUNNINGEN_DATA%\natuur\natuurvergunningen.sqlite"
py -3.13 oogst.py 2012-01-01 2026-09-25 --stroom natuur
py -3.13 laad.py
py -3.13 natuur_meet.py
```

De bronlagen staan per thema in `bron\<thema>\` met een `BRONNEN.md` (herkomst, licentie, datum, grootte) en de scripts waarmee ze zijn opgehaald en afgeleid in `bron\<thema>\werk\`; `bron\CATALOGUS.md` is de index. Voor de keten zijn nodig: de AERIUS relevante habitatkartering (WFS `base_geometries:relevant_habitats` van `connect.aerius.nl/opendata/wfs`, met ogr2ogr naar `bron\aerius_relevant_habitats.gpkg`), de AERIUS-hexagonen met overbelasting (`bron\natuur`), de BAG-historie uit het LV BAG-extract van PDOK (`bron\gebouwen\scripts`), de RIVM-depositie (`bron\depositie\werk`), de verrijkingsrasters (`bron\afgeleid\werk`, stap 1 tot 5), de verdiepte rechtspraak (`bron\rechtspraak_verdieping\werk\verdiep.py` en `tabellen.py`, uit de lokaal bewaarde uitspraken) en de natuurbesluiten. Daarna niveau 2.

Niet opnieuw op te halen: de FTM-stallenbestanden (Woo-documenten die Follow the Money kreeg; de afgeleide lagen zonder adressen staan in `bron\ftm_stallen`, de ruwe bestanden alleen op de onderzoeksmachine), de vertrouwelijke plancapaciteit in `bron\rs_plancapaciteit\priv` en de Datawonen-gegevens (inlog vereist). De DSO-oogster voor Ruimtelijke plannen (`bron\dso`) wacht op een API-sleutel in `dso_apikey.txt` in de datamap. CBS OData was vanaf de onderzoeksmachine op 26 september 2026 niet bereikbaar; op een andere pc haalt `py -3.13 bron\uitvoering\werk\cbs_rechtspraak.py` de rechtspraaktabellen 70764ned en 70229ned (2000 tot 2016) alsnog op.

## Lange processen

Een proces dat een Claude-sessie zelf start, stopt als de sessie stopt. Start een oogst of `verwerk.cmd` daarom los van de sessie, bijvoorbeeld via WMI in PowerShell:

```
Invoke-CimMethod -ClassName Win32_Process -MethodName Create -Arguments @{ CommandLine = 'cmd /c "<repo>\vergunningen\pipeline\verwerk.cmd"' }
```

Zo'n proces krijgt de omgevingsvariabelen uit het gebruikersprofiel (dus die van `setx`), niet die van het lopende venster. Volg de voortgang in `verwerk.log` of `oogst.log` in de datamap; stilstand meet je op de processortijd van het proces, niet op de logregels.

## Controlegetallen

Na niveau 1 met de data van 26 september 2026 moeten deze waarden terugkomen (schattingen maal honderd, in procent of procentpunt). Ze zijn van voor het herstel van `zaak.py` op 27 september 2026 (inzichten.md, hoofdstuk 31): na een nieuwe zaakvorming heeft de database 1.465.374 zaken in plaats van 1.464.997, en verschuiven de zaken van Groningen en Utrecht een fractie.

| Bestand | Grootheid | Waarde |
|---|---|---|
| `resultaten.json` | aanvragen van gemeenten, juli 2021 tot september 2026 | 657.825, waarvan 31,1 procent nabij |
| `resultaten.json` | zaken over alle jaren, uitspraken | 1.103.535 en 12.050 |
| `resultaten.json` | aanvragen nabij na intern salderen, landelijk kwartaaleffect | min 3,02 (p 0,034) |
| `bag_resultaten.json` | panden en woonpanden met een vergunning sinds 2012 | 1.028.484 en 526.604 |
| `bag_resultaten.json` | woonpanden nabij na de PAS-uitspraak | min 10,54 (p 0,044) |
| `blootstelling_resultaten.json` | hoogste depositiekwartiel na de PAS-uitspraak, zonder trend en met trendbreuk | min 55,3 en min 19,7 (p 0,036) |
| `vastpanel_resultaten.json` | panel A: gemeenten en aanvragen | 122 en 502.229 |
| `vastpanel_resultaten.json` | panel A, PAS-uitspraak binnen gemeente en kwartaal | plus 1,02 (p 0,53) |
| `vastpanel_resultaten.json` | alle gemeenten, intern salderen binnen gemeente en kwartaal | min 2,20 (p 0,15) |
| `projecten_resultaten.json` | projecten van twintig woningen of meer sinds juli 2021, woningen | 5.017 en 326.704 |
| `projecten_resultaten.json` | aandeel met een stikstofprocedure, nabij en ver | 10,7 en 3,3 procent |

Wijken de getallen af, kijk dan eerst of de csv's in `analyse\` van dezelfde datum zijn (OneDrive kan achterlopen) en of de pakketversies overeenkomen.
