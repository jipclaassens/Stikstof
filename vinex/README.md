# Vinex-realisatie uit de BAG

Telt per Vinex-locatie de woningen naar bouwjaar, van 1995 tot nu, als voortzetting van
`realisatie_VINEX.xlsx` (doel en realisatie 1995 tot 2004 per locatie).

## Stappen

1. `01_gebieden.py` (Python van QGIS): maakt van `VinexPlans.tif`, een masker van 0 en 1 op 100 meter,
   een raster met een nummer per aaneengesloten gebied, op het grid van de GeoDMS-configuratie.
   Gebieden binnen 300 meter van elkaar horen bij elkaar. Uitvoer in `C:\LocalData\Stikstof\Vinex`.
2. GeoDMS, item `/Analyse/Vinex/Woonobjecten` in `cfg/main.dms`: alle verblijfsobjecten met een
   woonfunctie die nu in de voorraad staan en in een Vinex-gebied vallen, met het bouwjaar van hun pand,
   uit de volledige BAG-tabel. Getoetst met GeoDMS 20.23.1; de container leest de BAG-mmd's zelf,
   omdat `SourceData/BAG/VolledigeBAG` daar niet op draait.
3. GeoDMS, item `/Analyse/Vinex/NieuwbouwNL`: dezelfde woonobjecten in heel Nederland, binnen en buiten
   de Vinex-gebieden, met een pandbouwjaar vanaf 1995 (parameter `VanafBouwjaar`), als
   `woonobjecten_vanaf1995_<BAG-datum>.parquet`. Ongeveer tien seconden met 20.23.1.
4. `02_buurten.py` (Python van QGIS): koppelt elk object aan zijn CBS-gemeente, -wijk en -buurt
   (wijk- en buurtkaart 2024, opgehaald bij de OGC API van PDOK).
5. `03_koppel_locaties.py` (Python van QGIS): koppelt de CBS-wijken aan de locaties in
   `realisatie_VINEX.xlsx` en schrijft `vinex_realisatie_<BAG-datum>.xlsx` en een objectbestand met
   puntkomma's voor een Nederlandse Excel. Met de export uit stap 3 telt het daarnaast alle woningen
   vanaf 1995 in de gekoppelde wijken, en schrijft het die woningen ook als objectbestand.

## Wat de koppeling wel en niet doet

Een wijk gaat naar een locatie als de wijknaam een naam van de locatie bevat, anders als minstens een
kwart van de woningen in buurten met zo'n naam staat, en anders via de gemeentenaam als maar een locatie
die gemeente noemt. Vijf wijken zijn met de hand gekoppeld (Waalsprong, Castellum, Saendelft, Eschmarke,
Vleuterweide bij Leidsche Rijn); die staan als `handmatig, te controleren` in de kolom `bewijs`.

Controle: de BAG-telling 1995 tot 2004 per locatie hoort dicht bij de Excel te liggen. Voor de BAG van
10 juli 2026 lag 32 van de 45 gekoppelde locaties met minstens 200 woningen binnen 15 procent. Wat niet
of verkeerd gekoppeld is, staat in het tabblad `Niet gekoppeld` en is via `Per wijk per jaar` met de hand
toe te wijzen.

Bouwjaar is het bouwjaar van het pand; woningen die na de bouw zijn gesloopt, tellen niet mee.

## Twee maten: binnen het raster en in de hele wijk

`VinexPlans.tif` is een plangrens, en bij sommige locaties ligt een deel van de latere bebouwing
erbuiten. Bij Waalsprong dekt het raster 297 hectare in de buurt Lent; Ressen en Oosterhout, in
dezelfde wijk Nijmegen-Noord, vallen erbuiten. Binnen het raster staan daar 5.242 woningen vanaf 1995,
in de hele wijk 9.645. Het doel van 6.500 is in de hele wijk in bouwjaar 2021 gehaald en binnen het
raster nog niet.

De werkmap houdt daarom twee maten naast elkaar. De tabbladen `Per locatie`, `Niet gekoppeld` en
`Per wijk per jaar` tellen alleen binnen het raster, zoals in de eerste levering. `Per locatie, hele wijk`
telt alle woningen vanaf 1995 in de gekoppelde CBS-wijken, met het aandeel buiten het raster, en
`Wijken, raster en hele wijk` geeft hetzelfde per wijk. De wijkmaat telt ook nieuwbouw die geen Vinex is,
zoals inbreiding of een dorpsuitbreiding in dezelfde wijk; bij kleine locaties in een grote wijk
(Bloemendalerpolder, Rietlanden, Erica) valt hij daardoor vele malen hoger uit. Een hoog aandeel buiten
het raster is dus een reden om op de kaart te kijken, niet een bewijs dat het raster te krap is.

Controle: de woningen binnen het raster, opnieuw geteld uit de landelijke export, zijn per locatie gelijk
aan `Per locatie` (kolom `controle_raster_uit_landelijke_export`).
