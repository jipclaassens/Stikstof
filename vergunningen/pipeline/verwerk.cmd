@echo off
rem Volledige verwerking na een oogst: laden, tekstfrasen, zaakvorming, afstand tot habitat en blootstelling, GeoPackage,
rem meetuitvoer, analysebestanden, de R-modellen, het BAG-spoor, de koppeling en de CBS-toets, de projecten en het rapport.
rem Herhaalbaar: laad.py slaat geladen bestanden over, frasen.py slaat afgeronde gemeente-jaargang-frasen over.
rem De frasestap doet SRU-verzoeken (ongeveer 700 per jaargang); de rest is lokaal. Sinds de stroom oud (Gemeenteblad 2013
rem tot medio 2021) lopen de frasen vanaf 2014; 2013 heeft te weinig records per gemeente voor het minimum.
rem
rem Paden via omgevingsvariabelen, zodat dezelfde keten op een andere machine draait (zie REPRODUCEREN.md):
rem   VERGUNNINGEN_DATA  de datamap (standaard C:\LocalData\vergunningen), bijvoorbeeld de OneDrive-projectmap
rem   QGIS_PY            python-qgis.bat van de QGIS-installatie (standaard QGIS 3.38.1)
rem   RSCRIPT            Rscript.exe (standaard R 4.6.1)
rem Alleen het analysedeel draaien, zonder oogst en database-stappen: verwerk.cmd analyse
cd /d "%~dp0"
if defined VERGUNNINGEN_DATA (set "DATA=%VERGUNNINGEN_DATA%") else (set "DATA=C:\LocalData\vergunningen")
if defined QGIS_PY (set "QPY=%QGIS_PY%") else (set "QPY=C:\Program Files\QGIS 3.38.1\bin\python-qgis.bat")
if defined RSCRIPT (set "RS=%RSCRIPT%") else (set "RS=C:\Program Files\R\R-4.6.1\bin\Rscript.exe")
set "VERGUNNINGEN_DATA=%DATA%"
set "LOG=%DATA%\verwerk.log"
echo [%date% %time%] start verwerking (%1) >> "%LOG%"
if /i "%1"=="analyse" goto analyse
py -3.13 -u laad.py >> "%LOG%" 2>&1
for %%j in (2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025 2026) do py -3.13 -u frasen.py %%j --minimum 100 >> "%LOG%" 2>&1
py -3.13 -u zaak.py >> "%LOG%" 2>&1
rem zaak.py nummert de zaken opnieuw, dus de kandidaatkoppeling van uitspraken aan zaken moet daarna opnieuw
py -3.13 -u rechtspraak.py koppel >> "%LOG%" 2>&1
rem python-qgis.bat is zelf een batchbestand: zonder call keert de keten er niet uit terug, en het zet PYTHONHOME en
rem PYTHONPATH op de Python van QGIS; zonder setlocal en endlocal blijft dat staan en breekt py -3.13 daarna op
rem "SRE module mismatch" (gebeurd op 26 september 2026)
setlocal
call "%QPY%" afstand.py "%DATA%\bron\aerius_relevant_habitats.gpkg" relevant_habitats >> "%LOG%" 2>&1
call "%QPY%" blootstelling.py "%DATA%\bron\natuur\aerius_hexagonen_overbelasting.gpkg" >> "%LOG%" 2>&1
call "%QPY%" export_gpkg.py >> "%LOG%" 2>&1
endlocal
py -3.13 -u meet.py >> "%LOG%" 2>&1
py -3.13 -u analyse_export.py >> "%LOG%" 2>&1
rem het BAG-spoor: panden met fasedatums en blootstelling, de koppeling aan zaken en de CBS-toets
setlocal
call "%QPY%" bag_export.py >> "%LOG%" 2>&1
endlocal
py -3.13 -u koppel_bag.py 100 >> "%LOG%" 2>&1
py -3.13 -u cbs_toets.py >> "%LOG%" 2>&1
py -3.13 -u valideer_registers.py >> "%LOG%" 2>&1
rem kaartlagen per zaak en pand aflezen (depositie, vee, PAS-melders, contour, netcongestie)
setlocal
call "%QPY%" verrijk.py >> "%LOG%" 2>&1
endlocal
rem grote woningbouwprojecten: zaken met twintig of meer woningen, natuurbesluiten in de buurt, beroep
py -3.13 -u projecten.py >> "%LOG%" 2>&1
:analyse
rem de modellen en het rapport lezen alleen de csv's in de analysemap
"%RS%" analyse.R "%DATA%\analyse" >> "%LOG%" 2>&1
"%RS%" analyse_bag.R "%DATA%\analyse" >> "%LOG%" 2>&1
"%RS%" analyse_blootstelling.R "%DATA%\analyse" >> "%LOG%" 2>&1
"%RS%" analyse_vastpanel.R "%DATA%\analyse" >> "%LOG%" 2>&1
"%RS%" analyse_projecten.R "%DATA%\analyse" >> "%LOG%" 2>&1
py -3.13 rapport.py "%DATA%\analyse" "%DATA%\analyse\teksten.md" "%DATA%\analyse\rapport.html" >> "%LOG%" 2>&1
echo [%date% %time%] einde verwerking >> "%LOG%"
