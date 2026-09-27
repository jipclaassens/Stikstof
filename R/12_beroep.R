## =====================================================================
## 12_beroep.R -- bezwaar en beroep: hoe lang, hoe vaak, met welke
##                gronden, en hoe het afloopt
##
## Input : <PATHS$verg>/bron/rechtspraak_verdieping/
##           doorlooptijden_raadvanstate.csv  (jaarverslagen Raad van State)
##           uitspraken_verdiept.csv          (12.050 uitspraken met tekst)
##           tabel_per_jaar_instantie.csv     (alleen als controle)
## Output: _output/beroep_<extract>.json, .txt
##
## Twee bronnen met elk een eigen rol. De jaarverslagen van de Raad van
## State geven de volumes en de doorlooptijd van de kamer die over
## ruimtelijke ordening gaat (tot 2020 de Ruimtelijke-ordeningskamer,
## daarna de Omgevingskamer). De uitspraken met tekst zijn een selectie van
## de gepubliceerde uitspraken (bij de Raad van State ongeveer 300 per jaar
## tegen 1.800 tot 2.500 afgedane zaken): ze dienen voor aandelen, zoals hoe
## vaak stikstof een grond is en hoe vaak het besluit standhoudt, niet voor
## volumes.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
options(width = 200)
sink(file.path(PATHS$output, sprintf("beroep_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()
RV <- file.path(PATHS$verg, "bron", "rechtspraak_verdieping")

## ---------------------------------------------------------------------
## 1. Raad van State: instroom, uitstroom, voorraad en doorlooptijd
## ---------------------------------------------------------------------
## Hoofdzaken van de kamer voor ruimtelijke ordening. Kanttekeningen uit de
## jaarverslagen zelf: in 2019 deed de kamer ongeveer 180 PAS-zaken af die
## sinds 2017 waren aangehouden (vandaar de piek in de doorlooptijd), en op
## 19 december 2019 kreeg ze 619 zaken van de Algemene kamer over; per 1
## januari 2020 heet ze Omgevingskamer en behandelt ze meer soorten zaken.
## Volumes van voor en na 2020 zijn dus niet een op een vergelijkbaar.
kop("1. Raad van State, kamer voor ruimtelijke ordening, hoofdzaken")
r <- fread(file.path(RV, "doorlooptijden_raadvanstate.csv"), sep = ";", encoding = "UTF-8")
k <- r[kamer %in% c("Kamer 1 (ruimtelijke ordening)", "Ruimtelijke-ordeningskamer", "Omgevingskamer") &
       categorie == "alle zaken, hoofdzaken" & maat %in% c("doorlooptijd_gemiddeld", "instroom", "uitstroom", "voorraad_eind")]
## per jaar en maat een waarde; waar twee kamernamen hetzelfde jaar dekken
## (2016 tot 2019 in latere jaarverslagen) zijn de waarden gelijk
k <- k[, .(waarde = waarde[1L], n_bron = uniqueN(waarde)), by = .(jaar, maat)]
if (any(k$n_bron > 1)) { cat("LET OP: tegenstrijdige waarden\n"); print(k[n_bron > 1]) }
R$raad_van_state <- dcast(k, jaar ~ maat, value.var = "waarde")[order(jaar)]
setnames(R$raad_van_state, c("doorlooptijd_gemiddeld", "voorraad_eind"), c("doorlooptijd_weken", "voorraad"), skip_absent = TRUE)
print(R$raad_van_state)
R$raad_van_state_noten <- list(
  pas_2019 = "ongeveer 180 PAS-zaken afgedaan in 2019, sinds 2017 aangehouden (jaarverslag 2019)",
  overboeking_2019 = "619 zaken van de Algemene kamer overgeboekt op 19 december 2019; per 1 januari 2020 Omgevingskamer",
  voorrang_woningbouw = "sinds juli 2024 voorrang voor plannen en vergunningen voor twaalf woningen of meer; bijna 400 zaken met ruim 82.000 woningen tot eind 2025 (jaarverslag 2025)")

## ---------------------------------------------------------------------
## 2. Uitspraken over woningbouw: gronden, uitkomst, duur
## ---------------------------------------------------------------------
kop("2. Uitspraken van de Raad van State over woningbouw (selectie met tekst)")
u <- fread(file.path(RV, "uitspraken_verdiept.csv"), sep = ";", encoding = "UTF-8")
u[, rvs := instantie == "Raad van State"]
## de zaak is beslist als de uitspraak een eindoordeel geeft; voorlopige
## voorzieningen en tussenuitspraken tellen niet mee in het aandeel dat
## standhoudt
u[, einduitspraak := uitkomst %in% c("gegrond", "ongegrond", "hoger beroep gegrond", "hoger beroep ongegrond")]
u[, beroep_slaagt := uitkomst %in% c("gegrond", "hoger beroep gegrond")]
per_jaar <- function(x) x[, .(uitspraken = .N, eind = sum(einduitspraak),
                              besluit_houdt_stand = mean(!beroep_slaagt[einduitspraak]),
                              stikstof_genoemd = mean(stikstof_genoemd == 1L), stikstof_grond = sum(stikstof_grond == 1L),
                              stikstof_gegrond = sum(stikstof_oordeel == "gegrond"),
                              woningen_bekend = sum(!is.na(aantal_woningen)), woningen = sum(aantal_woningen, na.rm = TRUE),
                              mediaan_dagen_besluit_uitspraak = as.numeric(median(dagen_besluit_uitspraak[einduitspraak], na.rm = TRUE))),
                          by = jaar][order(jaar)]
R$woningbouw_rvs <- per_jaar(u[rvs == TRUE & projecttype == "woningbouw"])
R$alle_rvs <- per_jaar(u[rvs == TRUE])
print(R$woningbouw_rvs[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
cat("\nalle uitspraken van de Raad van State in de selectie:\n")
print(R$alle_rvs[, .(jaar, uitspraken, stikstof_genoemd = round(stikstof_genoemd, 3), stikstof_grond, stikstof_gegrond,
                     besluit_houdt_stand = round(besluit_houdt_stand, 3), mediaan_dagen_besluit_uitspraak)])
R$totaal <- u[rvs == TRUE & projecttype == "woningbouw" & jaar >= 2020,
              .(uitspraken = .N, stikstof_grond = sum(stikstof_grond == 1L), stikstof_gegrond = sum(stikstof_oordeel == "gegrond"),
                eind = sum(einduitspraak), besluit_houdt_stand = mean(!beroep_slaagt[einduitspraak]))]
cat("\nwoningbouw, Raad van State, 2020 en later:\n"); print(R$totaal)

## tweede route: het afgeleide jaarbestand van de verzamelronde
cmp <- fread(file.path(RV, "tabel_per_jaar_instantie.csv"), sep = ";", encoding = "UTF-8")[instantie == "Raad van State"]
cmp[, jaar := suppressWarnings(as.integer(jaar))]
cmp <- cmp[!is.na(jaar)]
R$woningbouw_rvs[, jaar := as.integer(jaar)]; R$alle_rvs[, jaar := as.integer(jaar)]
chk <- merge(R$alle_rvs[, .(jaar, uitspraken)], cmp[, .(jaar, n_uitspraken, n_woningbouw)], by = "jaar")
chk <- merge(chk, R$woningbouw_rvs[, .(jaar, woningbouw = uitspraken)], by = "jaar")
cat("\ncontrole tegen tabel_per_jaar_instantie.csv (uitspraken, woningbouw): max verschil",
    max(abs(chk$uitspraken - chk$n_uitspraken)), max(abs(chk$woningbouw - chk$n_woningbouw)), "\n")

writeLines(to_json(R), file.path(PATHS$output, sprintf("beroep_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("beroep_%s.json", EXTRACT)))
sink()
