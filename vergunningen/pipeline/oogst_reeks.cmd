@echo off
rem Oogst de hele rubriek omgevingsvergunning sinds de Wet elektronische publicaties, sprintjaren eerst.
rem Hervatbaar: oogst.py slaat dagen met status ok over, dus na een onderbreking gewoon opnieuw starten.
rem Start buiten de sessie via WMI (zie REPRODUCEREN.md), zodat een herstart van de Claude-sessie de keten niet meeneemt.
rem De datamap komt uit VERGUNNINGEN_DATA (standaard C:\LocalData\vergunningen); de einddatum is het eerste argument
rem (standaard 2026-09-25, de stand van de eerste oogst).
cd /d "%~dp0"
if defined VERGUNNINGEN_DATA (set "DATA=%VERGUNNINGEN_DATA%") else (set "DATA=C:\LocalData\vergunningen")
set "VERGUNNINGEN_DATA=%DATA%"
if "%~1"=="" (set "EIND=2026-09-25") else (set "EIND=%~1")
if not exist "%DATA%" mkdir "%DATA%"
set "LOG=%DATA%\oogst.log"
echo [%date% %time%] start reeks tot %EIND% >> "%LOG%"
py -3.13 -u oogst.py 2024-01-01 2024-12-31 >> "%LOG%" 2>&1
py -3.13 -u oogst.py 2025-01-01 2025-12-31 >> "%LOG%" 2>&1
py -3.13 -u oogst.py 2026-01-01 %EIND% >> "%LOG%" 2>&1
py -3.13 -u oogst.py 2023-01-01 2023-12-31 >> "%LOG%" 2>&1
py -3.13 -u oogst.py 2022-01-01 2022-12-31 >> "%LOG%" 2>&1
py -3.13 -u oogst.py 2021-07-01 2021-12-31 >> "%LOG%" 2>&1
echo [%date% %time%] einde reeks >> "%LOG%"
