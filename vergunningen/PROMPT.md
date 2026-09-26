# Openingsprompt voor een eigen Claude-sessie

Open Claude Code in de repo Stikstof (de map boven `vergunningen\`) en plak de tekst tussen de lijnen als eerste bericht. Vul onderaan je eigen vraag in, of laat die regel weg om alleen te reproduceren. Zet eerst de omgevingsvariabelen uit `REPRODUCEREN.md` (minstens `VERGUNNINGEN_DATA`), of laat Claude dat voorstellen.

---

```
Je werkt mee aan een verkennend onderzoek naar de vraag of het stikstofdossier de woningbouw remt: is er sinds de
PAS-uitspraak van 29 mei 2019, het einde van de bouwvrijstelling (Porthos, 2 november 2022), de Omgevingswet
(1 januari 2024) en de uitspraak over intern salderen (18 december 2024) minder aangevraagd, vergund of gebouwd,
vooral nabij stikstofgevoelige Natura 2000-natuur? Het is een uitzoekkwestie, nog geen paper: zit er iets in, dan
kijken we verder.

Alles wat eerder is gebouwd, gemeten en geleerd staat in de map vergunningen van deze repo. Lees eerst, in deze
volgorde en helemaal:
1. vergunningen/CLAUDE.md (werkafspraken, methode, valkuilen; houd je daaraan)
2. vergunningen/README.md
3. vergunningen/inzichten.md (het logboek; de hoofdstukken 18 tot en met 27 bevatten de uitkomsten en de lessen)
4. vergunningen/pipeline/README.md (wat elk script doet)
5. vergunningen/REPRODUCEREN.md (installatie, volgorde, controlegetallen)
Lees daarna in de datamap (omgevingsvariabele VERGUNNINGEN_DATA, de OneDrive-projectmap
202505-StikstofPaper\vergunningen) het bestand analyse\teksten.md (de interpretatie per rapporthoofdstuk) en
bron\CATALOGUS.md (de index over de 21 themamappen met bronnen); open een BRONNEN.md pas als je een bron nodig hebt.

Stap 1, reproduceren. Controleer of Python 3.13, R 4.6 met fixest, data.table en jsonlite, en (alleen voor de
volledige keten) QGIS 3.38 aanwezig zijn, en of VERGUNNINGEN_DATA, RSCRIPT en QGIS_PY goed staan. Draai daarna
vergunningen\pipeline\verwerk.cmd analyse en vergelijk de uitkomsten met de controlegetallen in REPRODUCEREN.md.
Meld per controlegetal of het klopt; als iets afwijkt, zoek de oorzaak voordat je verder gaat.

Stap 2, de stand samenvatten in een paar zinnen, zodat ik zie dat je het hebt begrepen. De kern tot nu toe:
in de vergunningenstroom als geheel is geen stikstofeffect te zien zodra je nabij en ver binnen dezelfde gemeente
en hetzelfde kwartaal vergelijkt; eerdere effecten (BAG min 10,5 procent na de PAS-uitspraak, de depositiegradient,
het vaste panel min 4,5 procent) bleken voortrends of verschillen tussen gemeenten. Op projectniveau (projecten van
twintig woningen of meer) verschuiven de uitkomsten nabij evenmin; ongeveer een op de negen grote projecten nabij
habitat heeft een provinciaal stikstofbesluit in de buurt en loopt wat langer. De algemene vertraging (aanvraag tot
vergunning, Raad van State) raakt nabij en ver even hard. Controleer dit zelf aan inzichten.md en teksten.md.

Werkwijze voor alles wat volgt:
- Geen bevinding zonder de strenge vergelijking (vaste effecten gemeente maal nabij en gemeente maal kwartaal), een
  event study met vlakke voorperiode, een trendbreuktoets en de placebo klein werk. Draai eerdere conclusies eerlijk
  terug als een strengere toets ze weghaalt.
- Nieuwe analyses als script in vergunningen\pipeline (Python 3.13 of R met fixest), uitkomsten als json en csv in
  analyse\, een hoofdstuk in rapport.py en een sectie in analyse\teksten.md, een regel in pipeline\README.md, de
  stap in verwerk.cmd, en een nieuw hoofdstuk in inzichten.md met wat je deed, wat eruit kwam en wat je leerde.
- Schrijf Nederlands, zonder gedachtenstreepjes, zonder vet in lopende tekst en zonder persoonsnamen in repo-teksten.
- Vertrouwelijke data (bron\rs_plancapaciteit\priv, de ruwe FTM-stallen) alleen gebruiken na mijn akkoord; de
  DSO-sleutel nooit tonen. Niets verwijderen zonder mijn bevestiging. Committen doe ik zelf.
- Lange processen (oogst, volledige verwerking) los van de sessie starten, zoals REPRODUCEREN.md beschrijft.
- Bouw bij twijfel eerst de beste lezing en leg die voor; vraag niet naar routinekeuzes.

Mijn vraag voor deze sessie: <vul in>
```

---

Mogelijke vervolgvragen, uit het vervolg in `analyse\teksten.md` en de open punten in `inzichten.md`:

- Scheid de structurele verschuiving van nieuwbouw naar binnenstedelijk (sinds 2014) van een effect van de PAS-uitspraak met een synthetische controlegroep per gemeente, of met de verstedelijkingscontour van het moment uit de IMRO-reeks in `bron\planregistraties`.
- Koppel natuurbesluiten op sleutel aan bouwprojecten in plaats van op plaats en tijd, en doorzoek de volledige tekst van de uitspraken op projectnaam en adres.
- Netcongestie in de tijd: koppel de ACM-vooraankondigingen van 2020 tot 2025 aan voedingsgebieden.
- De beroepstijd per bestemmingsplan via de API van Ruimtelijke plannen, zodra de sleutel in `dso_apikey.txt` staat.
- Plancapaciteit als verklaring: de Nationale Woningbouwkaart 2026, de ABF-planinventarisatie en de provinciale registraties in `bron\`.
