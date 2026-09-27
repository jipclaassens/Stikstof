# CLAUDE.md, map vergunningen

Instructies voor Claude Code bij het werk aan de vergunningendatabase en de stikstofanalyses. Lees daarna `README.md`, `inzichten.md` (de bevindingen en lessen per hoofdstuk), `pipeline/README.md` en `REPRODUCEREN.md`.

## Waar staat wat

- In deze map alleen code en documentatie. `inzichten.md` is het logboek van wat gemeten, besloten en geleerd is (hoofdstukken 1 tot en met 30); `pipeline/README.md` beschrijft elk script; `REPRODUCEREN.md` de installatie, de volgorde en de controlegetallen; `PROMPT.md` de openingsprompt voor een nieuwe sessie.
- De data staat buiten de repo, in de OneDrive-projectmap `202505-StikstofPaper\vergunningen\` (op de onderzoeksmachine ook in `C:\LocalData\vergunningen`). Zet de omgevingsvariabele `VERGUNNINGEN_DATA` op die map; `config.py` en `verwerk.cmd` lezen hem.
- In de datamap: `vergunningen.sqlite` (bekendmakingen, zaken, koppelingen), `natuur\natuurvergunningen.sqlite` (natuurbesluiten van provincies en Rijk), `ruw\` (de ruwe SRU-antwoorden, herlaadbaar), `bron\` (21 themamappen met open data, elk met een `BRONNEN.md`; de index is `bron\CATALOGUS.md`), `analyse\` (analysebestanden, modeluitkomsten als json en csv, `teksten.md` en `rapport.html`), `onderzoek\` (bronrapporten en het ontwerp van 25 september 2026).
- Grote originelen staan alleen op de onderzoeksmachine onder `D:\LocalData_bron\` (zips van de Klimaateffectatlas, de ruwe FTM-stallenbestanden, de landelijke kadastrale kaart, verordeningen); wat eruit nodig was, staat afgeleid in `bron\`.
- Het rapport bouwt `rapport.py` uit de json's in `analyse\` en uit `analyse\teksten.md`. De getallen in `teksten.md` zijn met de hand geschreven: loop na een herberekening de teksten na.

## Werkafspraken

- Schrijfstijl in alle teksten, ook in de chat: Nederlands, geen gedachtenstreepjes (herschrijf de zin of gebruik een komma, dubbele punt of punt), geen vetgedrukte woorden in lopende tekst, geen persoonsnamen in repo-teksten (organisaties wel: VU, Object Vision, PBL, RIVM). Getallen in teksten met punt als duizendtal en komma als decimaalteken.
- Committen en pushen doet de eigenaar van de repo zelf.
- Het werk is verkennend: de vraag is of er iets in zit, niet een paper. Een bevinding telt pas als ze de strenge vergelijking binnen gemeente en kwartaal, een event study met vlakke voorperiode, een trendbreuktoets en de placebo (klein werk) overleeft. Draai eerdere conclusies eerlijk terug als een strengere toets ze weghaalt, en schrijf dat op in `inzichten.md`.
- Bouw eerst de beste lezing en leg haar voor; stel geen vragen over routinekeuzes.
- Vertrouwelijk: `bron\rs_plancapaciteit\priv` is een levering van Zuid-Holland uitsluitend voor het PBL-project, met uitkomsten alleen per regio; gebruik die niet in analyses zonder akkoord van de eigenaar. De ruwe FTM-stallenbestanden bevatten adressen en UBN-nummers; de afgeleide kilometercellen in `bron\ftm_stallen` zijn niet publiceerbaar, extern alleen op gemeenteniveau of in cellen met minstens drie bedrijven. Adressen in de bekendmakingen zijn openbaar en hoeven niet gemaskeerd.
- De sleutel voor de DSO-API staat in `dso_apikey.txt` in de datamap en komt nooit in chat, code, logbestand of repo.
- Verwijderen van data alleen na expliciete bevestiging van de eigenaar.
- Lange ketens (oogst, `verwerk.cmd`) buiten de Claude-sessie starten, zie `REPRODUCEREN.md`: processen die de sessie zelf start, sterven met de sessie.

## Methode in het kort

- Nabij is binnen 2.500 meter van de AERIUS relevante habitatkartering; ver is verder. Wisselingen: PAS-uitspraak 29 mei 2019, bouwvrijstelling 1 juli 2021, Porthos 2 november 2022, Omgevingswet 1 januari 2024, intern salderen 18 december 2024.
- Aantallen met Poisson (`fepois`), aandelen en log-doorlooptijden lineair (`feols`), standaardfouten geclusterd op gemeente. Vergelijk nabij en ver altijd ook binnen gemeente en kwartaal (vaste effecten gemeente maal nabij en gemeente maal kwartaal): met alleen een landelijk kwartaaleffect verschijnen verschillen tussen gemeenten als nabijheidseffect.
- Lees elke difference-in-differences met een lange voorperiode samen met een event study, een trendbreuk (trend voor, sprong, trend na) en een eigen trend per gemeente (`gemeente^groep[t]` in fixest).
- Houd de drie grootheden uit elkaar: bekendmakingen (elke stap van elke aanvraag, alle activiteiten), de BAG-status Bouwvergunning verleend (alleen nieuwbouw van panden) en CBS (verleende vergunningen boven 50.000 euro).
- Een aanvraag is geen project: een dakkapel telt even zwaar als een woonblok. Voor projecten `projecten.py` (twintig of meer woningen uit titel of BAG, met de telregel en ontdubbeling van hoofdstuk 26).

## Valkuilen

- SRU van repository.overheid.nl: hoogstens een verzoek per seconde; `startRecord` boven 10.000 geeft HTTP 504, dus partitioneren per dag op `dt.available`; `dt.title any "a b c"` splitst op woorden (een frase met "en" matcht alles); tellen met `maximumRecords=0`; `cql.textAndIndexes` met schema short voor identifierlijsten; een volledige oogst vooraf bij KOOP melden. Curl faalt soms op de overheidshosts, urllib werkt.
- `zaak.py` nummert de zaken opnieuw: draai daarna altijd `rechtspraak.py koppel`, anders wijst `uitspraak_zaak` naar oude zaken.
- `python-qgis.bat` in een batch altijd met `call` en tussen `setlocal` en `endlocal`: het zet PYTHONHOME en PYTHONPATH, en de volgende `py -3.13` breekt dan op "SRE module mismatch". Zet `GDAL_PAM_ENABLED=NO` om `.aux.xml`-bestanden naast bronbestanden te voorkomen.
- Python-code in een bash-heredoc verliest backslashes (Windows-paden, `\f`, `\a`, `\u`); schrijf zulke scripts als bestand.
- Regeleindes: de werkkopie staat op CRLF (`core.autocrlf true`); bewerk scripts in bytes of met `newline=''` en controleer `git diff --stat`.
- R met data.table: `get(y)` vangt een kolom `y`, dus hernoem coördinaten naar `x_rd` en `y_rd`; `fread` leest datumkolommen als IDate, vergelijk die niet met `""`. In fixest wil `i(var, ref = ...)` een letterlijke referentie in de formule; bouw de formule met `sprintf`.
- De koppeling van BAG-panden aan zaken (`koppel_bag.py`) geeft elk pand aan een zaak, niet per se aan de bouwaanvraag; tel projecten op het effectieve woningaantal en ontdubbel over titel en BAG heen.
- Koppelingen op plaats en tijd alleen met precieze locaties (punt, adres, perceel, klein vlak); een woonplaatsmarkering legt een punt midden in een kern.
- De Nextcloud-bronshare van Object Vision (`C:\SourceData`): geen `ls`, `du`, `find` of agents erop, want elke listing start downloads; alleen met toestemming per map, met PowerShell `Get-ChildItem` op attributen en gerichte `robocopy` van de benodigde submap.
- OneDrive: een zip van een hele gedeelde map kapt rond 20 GB af; download per map, 7-Zip leest ZIP64. `robocopy` geeft exitcode 1 tot 3 bij succes.
- SQLite: de database staat in WAL-modus met een wachttijd; lange transacties van de ene stroom blokkeren de andere, dus commit per bestand.
