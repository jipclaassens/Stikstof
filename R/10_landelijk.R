## =====================================================================
## 10_landelijk.R -- het landelijke tempo van de woningbouw en wat er
##                   tegelijk gebeurde: rente, bouwkosten, huizenprijzen,
##                   tekorten aan personeel en materiaal
##
## Input : <PATHS$verg>/bron/markt/*.csv   (CBS StatLine, ECB; zie
##           bron/CATALOGUS.md, thema markt)
##         _derived/vbo_analyse.rds        (de eigen BAG-reeksen)
## Output: _output/landelijk_<extract>.json  (voor het rapport)
##         _output/landelijk_<extract>.txt   (leesbare log)
##
## Waarom
## ---------------------------------------------------------------------
## Hoofdstuk 5 tot en met 9 vergelijken nabij en ver: wat landelijk voor
## iedereen gelijk verandert, valt daar weg. Dit script laat juist dat
## landelijke verloop zien, naast de kandidaten die het zouden kunnen
## verklaren. Het zijn tijdreeksen: ze tonen wat tegelijk gebeurde, niet
## wat oorzaak is. Twee dingen maken ze toch bruikbaar. Ten eerste de
## uitsplitsing naar opdrachtgever: corporaties, marktpartijen en
## particulieren hebben allemaal met stikstof te maken, maar niet in
## dezelfde mate met rente en koopprijzen. Ten tweede de eigen
## BAG-reeksen nabij en ver: een landelijke daling die nabij natuur even
## groot is als verderop, is geen stikstofdaling.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
options(width = 200)
sink(file.path(PATHS$output, sprintf("landelijk_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()
R <- list()
MARKT <- file.path(PATHS$verg, "bron", "markt")
lees_cbs <- function(t) {
  f <- file.path(MARKT, paste0(t, ".csv"))
  if (!file.exists(f)) stop("Ontbreekt: ", f)
  fread(f, sep = ";", encoding = "UTF-8", na.strings = c("", "."))
}
## StatLine-perioden: 2019KW02, 2019MM05, 2019JJ00
per_jaar <- function(p) as.integer(substr(p, 1, 4))
per_kw   <- function(p) {
  s <- substr(p, 5, 6); n <- as.integer(substr(p, 7, 8))
  fifelse(s == "KW", n, fifelse(s == "MM", (n - 1L) %/% 3L + 1L, NA_integer_))
}
kw_code <- function(j, k) sprintf("%dQ%d", j, k)

## ---------------------------------------------------------------------
## 1. Vergunde woningen naar opdrachtgever en eigendom (CBS 83671ned)
## ---------------------------------------------------------------------
kop("1. Vergunde woningen naar opdrachtgever (CBS 83671ned)")
v <- lees_cbs("83671ned")
cat("regiocodes:", paste(unique(substr(v$RegioS, 1, 2)), collapse = " "), "\n")
v <- v[substr(RegioS, 1, 2) == "GM" & substr(Perioden, 5, 6) == "KW"]
v[, `:=`(jaar = per_jaar(Perioden), kw = per_kw(Perioden))]
opdr <- c(T001209 = "totaal", A028184 = "corporatie_overheid", A028185 = "markt", A028186 = "particulier")
eig  <- c(T001258 = "totaal", A028867 = "huur", A028868 = "koop")
v <- v[Opdrachtgever %in% names(opdr) & Eigendom %in% names(eig)]
v[, `:=`(opdrachtgever = opdr[Opdrachtgever], eigendom = eig[Eigendom])]
vk <- v[, .(woningen = sum(Woningen_2, na.rm = TRUE)), by = .(jaar, kw, opdrachtgever, eigendom)]
R$vergund_cbs <- dcast(vk[eigendom == "totaal"], jaar + kw ~ opdrachtgever, value.var = "woningen")[order(jaar, kw)]
R$vergund_cbs_koophuur <- dcast(vk[opdrachtgever == "totaal"], jaar + kw ~ eigendom, value.var = "woningen")[order(jaar, kw)]
jr <- vk[eigendom == "totaal", .(woningen = sum(woningen), kwartalen = .N / 4), by = .(jaar, opdrachtgever)]
R$vergund_cbs_jaar <- dcast(jr, jaar ~ opdrachtgever, value.var = "woningen")[order(jaar)]
print(R$vergund_cbs_jaar)
cat("controle tegen vergunningen/inzichten.md tabel 5 (2012 37.237, 2018 67.246, 2024 69.149):",
    paste(R$vergund_cbs_jaar[jaar %in% c(2012, 2018, 2024), totaal], collapse = ", "), "\n")

## ---------------------------------------------------------------------
## 2. Levensloop: vergund, bouw gestart, afgeblazen, gereed (CBS 86098ned)
## ---------------------------------------------------------------------
kop("2. Levensloop van woningen (CBS 86098ned), Nederland")
l <- lees_cbs("86098ned")[RegioS == "NL01" & Gebruiksfunctie == "A045364" & substr(Perioden, 5, 6) == "MM"]
l[, `:=`(jaar = per_jaar(Perioden), kw = per_kw(Perioden))]
R$levensloop <- l[, .(vergund = sum(VergundeNieuwbouw_2, na.rm = TRUE), bouw_gestart = sum(BouwGestart_3, na.rm = TRUE),
                      afgeblazen = sum(AfgeblazenNieuwbouw_5, na.rm = TRUE), gereed = sum(NieuwbouwTotaal_6, na.rm = TRUE),
                      maanden = .N), by = .(jaar, kw)][order(jaar, kw)]
R$levensloop_jaar <- R$levensloop[, .(vergund = sum(vergund), bouw_gestart = sum(bouw_gestart), afgeblazen = sum(afgeblazen),
                                      gereed = sum(gereed), maanden = sum(maanden)), by = jaar]
print(R$levensloop_jaar)

## ---------------------------------------------------------------------
## 3. Pijplijn en doorlooptijd (CBS 82211ned, 86260ned)
## ---------------------------------------------------------------------
kop("3. Pijplijn en doorlooptijd")
pl <- lees_cbs("82211ned")
pl <- pl[RegioS == "NL01" & Gebruiksfunctie == "A045364" & Woningtype == "T001100" & substr(Perioden, 5, 6) == "MM"]
pl[, `:=`(jaar = per_jaar(Perioden), maand = as.integer(substr(Perioden, 7, 8)))]
pl <- pl[maand %in% c(3L, 6L, 9L, 12L)]   # stand aan het eind van het kwartaal
R$pijplijn <- pl[, .(jaar, kw = maand %/% 3L, vergund_niet_gestart = Vergunningspijplijn_3, gestart_niet_gereed = BouwGestartPijplijn_2,
                     vergund_niet_gestart_2jr = Vergunningspijplijn2Jaar_6, totaal = VerblijfsobjectenInDePijplijnTotaal_1)][order(jaar, kw)]
print(tail(R$pijplijn, 6))
dl <- lees_cbs("86260ned")
cat("gebruiksfunctie-codes 86260ned:", paste(unique(paste(dl$Gebruiksfunctie, dl$Gebruiksfunctie_label)), collapse = " | "), "\n")
dl <- dl[Regiokenmerken == "NL01" & Gebruiksfunctie == "A045364" & Woningtype_label == "Totaal" & substr(Perioden, 5, 6) == "KW"]
R$doorlooptijd <- dl[, .(jaar = per_jaar(Perioden), kw = per_kw(Perioden), mediaan_maanden = MediaanDoorlooptijdMaanden_4,
                         p25 = k_25KwantielDoorlooptijdMaanden_3, p75 = k_75KwantielDoorlooptijdMaanden_5, n = NieuwbouwTotaal_1)][order(jaar, kw)]
print(R$doorlooptijd[kw == 2L])

## ---------------------------------------------------------------------
## 4. Belemmeringen in de bouw volgens de bedrijven zelf (CBS 85609ned)
## ---------------------------------------------------------------------
## Conjunctuurenquete, bedrijfstak F Bouwnijverheid, ongecorrigeerd; per
## kwartaal gevraagd in januari, april, juli en oktober. Een categorie voor
## vergunningen of regelgeving bestaat niet: die valt onder andere oorzaken.
kop("4. Belemmeringen volgens bouwbedrijven (CBS 85609ned, F Bouwnijverheid)")
co <- lees_cbs("85609ned")[BedrijfstakkenBranchesSBI2008 == "350000" & Marges == "MW00000" & Seizoencorrectie == "A042501" &
                             !is.na(GeenBelemmeringen_84)]
co[, `:=`(jaar = per_jaar(Perioden), kw = per_kw(Perioden))]
R$belemmeringen <- co[, .(jaar, kw, geen = GeenBelemmeringen_84, vraag = OnvoldoendeVraag_85, personeel = TekortAanArbeidskrachten_86,
                          materiaal = TekortProdMiddelenMateriaalRuimte_87, financiering = FinancieleBeperkingen_88,
                          weer = Weersomstandigheden_89, overig = AndereOorzaken_90)][order(jaar, kw)]
R$belemmeringen_jaar <- R$belemmeringen[, lapply(.SD, mean), by = jaar, .SDcols = c("geen", "vraag", "personeel", "materiaal", "financiering", "weer", "overig")]
R$belemmeringen_jaar[, metingen := R$belemmeringen[, .N, by = jaar]$N]
print(R$belemmeringen_jaar[, lapply(.SD, function(x) round(x, 1))])
## tweede route: het afgeleide jaarbestand uit de catalogus moet hetzelfde geven
af <- fread(file.path(MARKT, "afgeleid_coen_belemmeringen_bouw_per_jaar.csv"), sep = ";", encoding = "UTF-8")
chk <- merge(R$belemmeringen_jaar[, .(jaar, personeel = round(personeel, 1), materiaal = round(materiaal, 1))],
             af[, .(jaar, personeel_af = `tekort aan arbeidskrachten`, materiaal_af = `tekort productiemiddelen, materiaal, ruimte`)], by = "jaar")
cat("afwijking van het afgeleide jaarbestand (max abs, procentpunt): personeel",
    max(abs(chk$personeel - chk$personeel_af)), " materiaal", max(abs(chk$materiaal - chk$materiaal_af)), "\n")

## ---------------------------------------------------------------------
## 5. Rente, bouwkosten en huizenprijzen
## ---------------------------------------------------------------------
kop("5. Hypotheekrente (ECB MIR), bouwkosten (CBS 85728ned, 80444ned) en koopprijzen (CBS 85773ned)")
ecb <- fread(file.path(MARKT, "ecb_mir_hypotheekrente_nl.csv"), sep = ";", encoding = "UTF-8")
ecb[, `:=`(jaar = as.integer(substr(TIME_PERIOD, 1, 4)), maand = as.integer(substr(TIME_PERIOD, 6, 7)))]
ecb[, kw := (maand - 1L) %/% 3L + 1L]
R$rente <- dcast(ecb[rentevaste_periode_code %in% c("AM", "P"), .(r = mean(OBS_VALUE)), by = .(jaar, kw, rentevaste_periode_code)],
                 jaar + kw ~ rentevaste_periode_code, value.var = "r")
setnames(R$rente, c("AM", "P"), c("rente_nieuw", "rente_10jr_plus"))
R$rente <- R$rente[jaar >= 2010][order(jaar, kw)]
## inputprijsindex bouwkosten nieuwbouwwoningen, 2021 = 100; voor 2018 de
## lange reeks (2000 = 100) herschaald op het overlappende jaar 2018
ik <- lees_cbs("85728ned")[Componenten == "T001363" & substr(Perioden, 5, 6) == "MM"]
ik[, `:=`(jaar = per_jaar(Perioden), kw = per_kw(Perioden))]
lang <- lees_cbs("80444ned")[substr(Perioden, 5, 6) == "MM"]
lang[, `:=`(jaar = per_jaar(Perioden), kw = per_kw(Perioden))]
schaal <- ik[jaar == 2018, mean(InputprijsindexBouwkosten_1)] / lang[jaar == 2018, mean(Prijsindex_1)]
cat(sprintf("herschaling lange reeks op 2018: factor %.4f\n", schaal))
kosten <- rbind(lang[jaar < 2018, .(jaar, kw, index = Prijsindex_1 * schaal, bron = "80444ned")],
                ik[, .(jaar, kw, index = InputprijsindexBouwkosten_1, bron = "85728ned")])
R$bouwkosten <- kosten[, .(bouwkosten = mean(index)), by = .(jaar, kw)][jaar >= 2010][order(jaar, kw)]
hp <- lees_cbs("85773ned")
if ("RegioS" %in% names(hp)) hp <- hp[RegioS == "NL01"]
hp <- hp[substr(Perioden, 5, 6) == "MM"]
hp[, `:=`(jaar = per_jaar(Perioden), kw = per_kw(Perioden))]
R$koopprijs <- hp[, .(koopprijs = mean(PrijsindexVerkoopprijzen_1, na.rm = TRUE)), by = .(jaar, kw)][jaar >= 2010][order(jaar, kw)]
print(merge(merge(R$rente, R$bouwkosten, by = c("jaar", "kw")), R$koopprijs, by = c("jaar", "kw"))[kw == 2L])

## ---------------------------------------------------------------------
## 6. Vacaturegraad en faillissementen in de bouw
## ---------------------------------------------------------------------
kop("6. Vacaturegraad (CBS 80567ned) en faillissementen (CBS 82244ned), bouw")
va <- lees_cbs("80567ned")
cat("SBI-codes met bouw:", paste(unique(va[grepl("ouw", SBI2008_label), paste(SBI2008, SBI2008_label)]), collapse = " | "), "\n")
va <- va[grepl("^F ", SBI2008_label) & substr(Perioden, 5, 6) == "KW"]
R$vacaturegraad <- va[, .(jaar = per_jaar(Perioden), kw = per_kw(Perioden), vacaturegraad = Vacaturegraad_1)][order(jaar, kw)]
print(R$vacaturegraad[kw == 2L])
fa <- lees_cbs("82244ned")
sbi <- intersect(c("BedrijfstakkenBranchesSBI2008", "BedrijfskenmerkenSBI2008"), names(fa))[1]
cat("typen gefailleerde:", paste(unique(fa$TypeGefailleerde_label), collapse = " | "), "\n")
fa <- fa[grepl("^F ", get(paste0(sbi, "_label"))) & substr(Perioden, 5, 6) == "MM"]
## alleen het totaal over de typen (A047596: bedrijven, instellingen en
## eenmanszaken); de andere twee typen zijn deelverzamelingen daarvan
fa <- fa[TypeGefailleerde == "A047596"]
fa[, `:=`(jaar = per_jaar(Perioden), kw = per_kw(Perioden))]
R$faillissementen <- fa[, .(faillissementen = sum(UitgesprokenFaillissementen_1, na.rm = TRUE)), by = .(jaar, kw)][order(jaar, kw)]
print(R$faillissementen[, .(faill = sum(faillissementen)), by = jaar])

## ---------------------------------------------------------------------
## 7. De eigen BAG-reeksen, landelijk en nabij tegenover ver
## ---------------------------------------------------------------------
## Dezelfde steekproeven als 06_rapport.R en 08_strikt.R. Nabij is binnen
## 2.500 m van stikstofgevoelig habitat.
kop("7. Eigen BAG-reeksen: vergund en opgeleverd, nabij en ver")
d <- read_derived("vbo_analyse")
KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
s <- d[in_sample == TRUE]
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]
rij <- function(dt, naam, qmax) dt[q >= 1 & q <= qmax, .(n = .N), by = .(q, nabij = as.integer(dist2n2000_m <= 2500))][, reeks := naam][]
bag <- rbind(rij(verg, "vergund", kw_index(as.Date("2026-03-31"))), rij(nwb, "opgeleverd", kw_index(as.Date("2026-06-30"))))
bag[, `:=`(jaar = KW_START + (q - 1L) %/% 4L, kw = (q - 1L) %% 4L + 1L)]
R$bag <- dcast(bag, jaar + kw ~ reeks + nabij, value.var = "n", fill = 0)[order(jaar, kw)]
print(R$bag[, lapply(.SD, sum), by = jaar, .SDcols = patterns("_")])

## De zones van Bouwend Nederland (14 maart 2025): rood binnen 1 km van
## stikstofgevoelig Natura 2000 ("geen vergunningen"), oranje tot 3 km,
## geel tot 5 km. Zelfde afstand als hierboven (tot stikstofgevoelig
## habitat), per jaar van vergunning en van oplevering.
zone <- function(m) fifelse(m <= 1000, "0-1 km", fifelse(m <= 3000, "1-3 km", fifelse(m <= 5000, "3-5 km", "verder")))
zo <- rbind(verg[q >= 1 & q <= kw_index(as.Date("2026-03-31")), .(n = .N), by = .(jaar = KW_START + (q - 1L) %/% 4L, zone = zone(dist2n2000_m))][, reeks := "vergund"],
            nwb[q >= 1 & q <= kw_index(as.Date("2026-06-30")), .(n = .N), by = .(jaar = KW_START + (q - 1L) %/% 4L, zone = zone(dist2n2000_m))][, reeks := "opgeleverd"])
zo[, aandeel := n / sum(n), by = .(reeks, jaar)]
R$zones <- zo[order(reeks, jaar, zone)]
cat("\nvergund per zone (aantal, aandeel):\n")
print(dcast(zo[reeks == "vergund"], jaar ~ zone, value.var = c("n", "aandeel"))[, lapply(.SD, function(v) round(v, 3))])

## ---------------------------------------------------------------------
## 8. Kerncijfers voor de tekst
## ---------------------------------------------------------------------
kop("8. Kerncijfers")
vj <- R$vergund_cbs_jaar
pct <- function(a, b) 100 * (a / b - 1)
R$kern <- list(
  vergund_2021 = vj[jaar == 2021, totaal], vergund_2023 = vj[jaar == 2023, totaal], vergund_2024 = vj[jaar == 2024, totaal],
  vergund_2025 = vj[jaar == 2025, totaal],
  markt_2021_2023_pct = pct(vj[jaar == 2023, markt], vj[jaar == 2021, markt]),
  corporatie_2021_2023_pct = pct(vj[jaar == 2023, corporatie_overheid], vj[jaar == 2021, corporatie_overheid]),
  particulier_2021_2023_pct = pct(vj[jaar == 2023, particulier], vj[jaar == 2021, particulier]),
  rente_2021 = R$rente[jaar == 2021, mean(rente_nieuw)], rente_2023 = R$rente[jaar == 2023, mean(rente_nieuw)],
  bouwkosten_2020 = R$bouwkosten[jaar == 2020, mean(bouwkosten)], bouwkosten_2023 = R$bouwkosten[jaar == 2023, mean(bouwkosten)],
  koopprijs_2022_max = R$koopprijs[jaar == 2022, max(koopprijs)], koopprijs_2023_min = R$koopprijs[jaar == 2023, min(koopprijs)],
  bag_vergund_nabij_2021_2023_pct = pct(R$bag[jaar == 2023, sum(vergund_1)], R$bag[jaar == 2021, sum(vergund_1)]),
  bag_vergund_ver_2021_2023_pct   = pct(R$bag[jaar == 2023, sum(vergund_0)], R$bag[jaar == 2021, sum(vergund_0)]))
str(R$kern)

writeLines(to_json(R), file.path(PATHS$output, sprintf("landelijk_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("landelijk_%s.json", EXTRACT)))
sink()
