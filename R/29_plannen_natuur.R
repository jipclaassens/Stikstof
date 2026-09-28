## =====================================================================
## 29_plannen_natuur.R -- bestemmingsplannen nabij natuur voor en na de
##                        PAS-uitspraak: hoeveel, hoe snel, hoe vaak beroep
##
## Input : _derived/plannen_dso.csv (29a_plannen_dso.py: elke planversie uit de
##         oogst van de API Ruimtelijke plannen v4, met oppervlakte, kleinste
##         afstand tot het habitat en de woonbestemmingen uit de PDOK-laag);
##         _derived/uitspraken_dso.csv (29b_uitspraken_dso.py: de gerechtelijke
##         uitspraken die aan een plan hangen)
## Output: _output/plannen_natuur_<extract>.json en .txt
##
## Eenheid: het dossier van een gemeentelijk bestemmingsplan, met de datum
## van de eerste vastgestelde versie en, waar die in de historie staat, de
## eerste ontwerpversie. Nabij: de plancontour ligt binnen 2.500 meter van
## de AERIUS relevante habitatkartering. Grote plannen (buitengebied, hele
## kernen) liggen bijna altijd nabij; de hoofdmaat is daarom kleine plannen
## tot 10 ha, projectplannen, met tot 50 ha en alle plannen als variant.
##
## A. Aantal vastgestelde plannen per gemeente, kant en kwartaal: Poisson met
##    gemeente x nabij, gemeente x kwartaal en nabij x seizoen (specificatie D
##    van het rapport), 2014Q1 tot en met 2021Q2; een trendbreuk; per jaar tot
##    en met 2023, tegenover 2018.
## B. Tijd van ontwerp tot vaststelling (log dagen), naar de ontwerpdatum
##    (2014 tot en met 2021) en naar de vaststellingsdatum, lineair met
##    gemeente, jaar en nabij als vaste effecten; apart de ontwerpen uit de
##    twaalf maanden voor de uitspraak, die midden in de procedure zaten toen
##    de toets strenger werd; en het aandeel ontwerpen dat nooit werd
##    vastgesteld.
## C. Beroep met gevolgen: een uitspraak van de Raad van State die de
##    gemeente als gerechtelijke uitspraak bij het plan publiceerde (het plan
##    veranderde door de uitspraak), per jaar van vaststelling tot en met 2022.
##    De uitspraken uit analyse/uitspraken.csv zijn niet betrouwbaar aan een
##    plan te koppelen: het gemeenteveld is vaak leeg of verkeerd, en op een
##    dag worden in het land meer plannen vastgesteld.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)
t0 <- Sys.time()
sink(file.path(PATHS$output, sprintf("plannen_natuur_%s.txt", EXTRACT)), split = TRUE)
R <- list()
pct <- function(v) if (!length(v) || all(is.na(v))) NA_real_ else 100 * mean(v, na.rm = TRUE)
ROND <- function(x, d = 3) x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, d) else v)]
KW_START <- 2012L
kw <- function(d) (as.integer(format(d, "%Y")) - KW_START) * 4L + (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L
PAS <- as.IDate("2019-05-29")
Q_PAS <- kw(PAS)   # het kwartaal van de uitspraak telt als na, zoals in de rest van het rapport

## ---- 0. dossiers --------------------------------------------------------
p <- fread(file.path(PATHS$derived, "plannen_dso.csv"), sep = ";", encoding = "UTF-8",
           colClasses = list(character = c("identificatie", "dossier", "overheidscode", "overheidsnaam", "naam", "planstatus",
                                           "beroep_en_bezwaar", "dossierstatus")))
p[, datum := as.IDate(substr(datum, 1, 10))]
## gemeentelijke plannen: een gemeentecode van vier cijfers (provincies beginnen met 99, het Rijk is 0000); de naam begint
## met gemeente, Gemeente of is alleen de naam van de gemeente
p <- p[grepl("^[0-9]{4}$", overheidscode) & !startsWith(overheidscode, "99") & overheidscode != "0000" &
       !grepl("^(provincie|ministerie|rijk)", tolower(overheidsnaam)) & !is.na(dossier) & dossier != ""]
VAST <- c("vastgesteld", "onherroepelijk", "vigerend", "goedgekeurd")
v <- p[planstatus %chin% VAST & !is.na(datum)]
setorder(v, dossier, datum, identificatie)
ds <- v[!duplicated(dossier), .(dossier, type, gemeente = overheidscode, gemnaam = sub("^[Gg]emeente ", "", overheidsnaam), naam,
                                vaststelling = datum, opp_ha, afstand_m, dossierstatus)]
## woonbestemming: een versie van het dossier in de PDOK-download met wonen of woongebied; zonder versie in de download
## de naam van het plan
wo <- p[!is.na(eb_vlakken), .(in_pdok = TRUE, wonen_pdok = any(eb_wonen > 0)), by = dossier]
ds[wo, on = "dossier", `:=`(in_pdok = i.in_pdok, wonen_pdok = i.wonen_pdok)]
RX_WONEN <- "woning|wonen|woon|appartement|woningbouw|nieuwbouw"
ds[, wonen := fifelse(!is.na(in_pdok), wonen_pdok %in% TRUE, grepl(RX_WONEN, naam, ignore.case = TRUE))]
ont <- p[planstatus == "ontwerp" & !is.na(datum), .(ontwerp = min(datum)), by = dossier]
ds[ont, on = "dossier", ontwerp := i.ontwerp]
ds[!is.na(ontwerp) & ontwerp < vaststelling, dagen := as.integer(vaststelling - ontwerp)]
ds[, `:=`(D = as.integer(!is.na(afstand_m) & afstand_m <= 2500), t = kw(vaststelling), jaar = year(vaststelling),
          klein = !is.na(opp_ha) & opp_ha <= 10, middel = !is.na(opp_ha) & opp_ha <= 50)]
ds <- ds[!is.na(afstand_m)]
kop("Dossiers van gemeentelijke plannen met een vastgestelde versie: %s (bestemmingsplan %s)",
    format(nrow(ds), big.mark = "."), format(ds[type == "bestemmingsplan", .N], big.mark = "."))
R$beschrijving <- ds[type == "bestemmingsplan" & jaar %between% c(2014L, 2023L),
                     .(dossiers = .N, klein = sum(klein), pct_nabij_alle = pct(D == 1L), pct_nabij_klein = pct(D[klein] == 1L),
                       pct_wonen_klein = pct(wonen[klein]), pct_met_ontwerp = pct(!is.na(ontwerp)), opp_mediaan = median(opp_ha)), by = jaar][order(jaar)]
print(ROND(R$beschrijving, 1))

## ---- A. aantal vastgestelde plannen --------------------------------------
Q0 <- kw(as.IDate("2014-01-01")); Q1 <- kw(as.IDate("2021-06-30")); Q2 <- kw(as.IDate("2023-12-31"))
paneel <- function(sub, van, tot) {
  cel <- sub[t >= van & t <= tot, .(y = .N), by = .(gemeente, D, t)]
  gem <- unique(ds[t >= van & t <= tot, gemeente])
  p0 <- CJ(gemeente = gem, D = 0:1, t = van:tot)
  p0 <- merge(p0, cel, by = c("gemeente", "D", "t"), all.x = TRUE)
  p0[is.na(y), y := 0L]
  p0[, `:=`(gem_D = paste(gemeente, D), gem_t = paste(gemeente, t), D_szn = paste(D, (t - 1L) %% 4L),
            na = D * as.integer(t >= Q_PAS), jaar = KW_START + (t - 1L) %/% 4L)]
  p0[]
}
ct <- function(m, termen, naam) {
  x <- coeftable(m)
  data.table(groep = naam, term = termen, b = x[termen, 1], se = x[termen, 2], p = x[termen, 4], n = nobs(m))
}
GROEP <- list(`bestemmingsplannen tot 10 ha` = quote(type == "bestemmingsplan" & klein),
              `bestemmingsplannen tot 10 ha met woonbestemming` = quote(type == "bestemmingsplan" & klein & wonen),
              `bestemmingsplannen tot 50 ha` = quote(type == "bestemmingsplan" & middel),
              `alle bestemmingsplannen` = quote(type == "bestemmingsplan"),
              `bestemmingsplannen boven 50 ha` = quote(type == "bestemmingsplan" & !middel),
              `wijzigings- en uitwerkingsplannen tot 10 ha` = quote(type %chin% c("wijzigingsplan", "uitwerkingsplan") & klein))
A <- list(); Tb <- list(); J <- list(); Ruw <- list()
for (g in names(GROEP)) {
  sub <- ds[eval(GROEP[[g]])]
  pa <- paneel(sub, Q0, Q1)
  m <- fepois(y ~ na | gem_D + gem_t + D_szn, data = pa, cluster = ~gemeente, notes = FALSE)
  A[[g]] <- ct(m, "na", g)
  pa[, `:=`(tj = (t - Q_PAS) / 4, post = as.integer(t >= Q_PAS))]
  pa[, `:=`(voortrend = D * tj, sprong = D * post, natrend = D * post * tj)]
  mt <- fepois(y ~ voortrend + sprong + natrend | gem_D + gem_t + D_szn, data = pa, cluster = ~gemeente, notes = FALSE)
  Tb[[g]] <- ct(mt, c("voortrend", "sprong", "natrend"), g)
  pj <- paneel(sub, Q0, Q2)
  mj <- fepois(y ~ i(jaar, D, ref = 2018) | gem_D + gem_t + D_szn, data = pj, cluster = ~gemeente, notes = FALSE)
  x <- coeftable(mj)
  J[[g]] <- data.table(groep = g, jaar = as.integer(sub(".*::([0-9]{4}):D", "\\1", rownames(x))), b = x[, 1], se = x[, 2], p = x[, 4])
  Ruw[[g]] <- pa[, .(per_kwartaal = sum(y) / uniqueN(t)), by = .(D, na_periode = t >= Q_PAS)]
}
R$A_did <- rbindlist(A)[, pct := 100 * (exp(b) - 1)]
R$A_trendbreuk <- rbindlist(Tb)
R$A_jaren <- rbindlist(J)[, pct := 100 * (exp(b) - 1)]
R$A_ruw <- rbindlist(Ruw, idcol = "groep")
cat("\nA. Vastgestelde plannen nabij tegenover ver na de PAS-uitspraak, 2014Q1 tot en met 2021Q2 (Poisson, D-specificatie):\n")
print(ROND(R$A_did)); print(ROND(R$A_trendbreuk))
cat("\nPer jaar tegenover 2018:\n"); print(dcast(ROND(R$A_jaren, 2), jaar ~ groep, value.var = "pct"))
cat("\nRuw, plannen per kwartaal:\n"); print(ROND(R$A_ruw, 2))

## ---- B. tijd van ontwerp tot vaststelling --------------------------------
b <- ds[type == "bestemmingsplan" & !is.na(dagen) & dagen > 0 & dagen < 2000]
b[, `:=`(log_d = log(dagen), jaar_o = year(ontwerp), na_o = as.integer(ontwerp >= PAS), na_v = as.integer(vaststelling >= PAS))]
R$B_dekking <- ds[type == "bestemmingsplan" & jaar %between% c(2014L, 2023L), .(pct_met_ontwerp = pct(!is.na(ontwerp))), by = .(jaar, D)][order(jaar, D)]
cat("\nB. Aandeel vastgestelde bestemmingsplannen met een ontwerp in de historie, per jaar en kant:\n")
print(dcast(ROND(R$B_dekking, 1), jaar ~ D, value.var = "pct_met_ontwerp"))
dur <- function(d, na, naam) {
  d <- copy(d)[, na_x := D * get(na)]
  m1 <- feols(log_d ~ na_x | gemeente + jaar_o + D, data = d, cluster = ~gemeente, notes = FALSE)
  m2 <- feols(log_d ~ na_x | gemeente^D + jaar_o, data = d, cluster = ~gemeente, notes = FALSE)
  rbind(cbind(ct(m1, "na_x", naam), fe = "gemeente, jaar, nabij"), cbind(ct(m2, "na_x", naam), fe = "gemeente x nabij, jaar"))
}
B1 <- b[jaar_o %between% c(2014L, 2021L)]
R$B_duur <- rbind(dur(B1, "na_o", "alle, naar ontwerpdatum 2014 tot en met 2021"),
                  dur(B1[klein == TRUE], "na_o", "tot 10 ha, naar ontwerpdatum"),
                  dur(b[jaar %between% c(2014L, 2023L)], "na_v", "alle, naar vaststellingsdatum 2014 tot en met 2023"))
## ontwerpen uit de twaalf maanden voor de uitspraak apart: die zaten midden in de procedure toen de toets strenger werd
B1[, cohort := fifelse(ontwerp >= PAS, "na", fifelse(ontwerp >= PAS - 365L, "jaar ervoor", "eerder"))]
B1[, `:=`(D_jaar = D * (cohort == "jaar ervoor"), D_na = D * (cohort == "na"))]
mc <- feols(log_d ~ D_jaar + D_na | gemeente + jaar_o + D, data = B1, cluster = ~gemeente, notes = FALSE)
R$B_duur <- rbind(R$B_duur, cbind(ct(mc, c("D_jaar", "D_na"), "alle, ontwerp in het jaar voor de uitspraak en erna, tegenover eerder"),
                                  fe = "gemeente, jaar, nabij"))
R$B_duur[, pct := 100 * (exp(b) - 1)]
R$B_cohort <- B1[, .(plannen = .N, mediaan_dagen = as.numeric(median(dagen))), by = .(D, cohort)][order(D, cohort)]
R$B_mediaan <- B1[, .(plannen = .N, mediaan_dagen = as.numeric(median(dagen))), by = .(D, na = na_o)][order(D, na)]
cat("\nTijd van ontwerp tot vaststelling, nabij tegenover ver na de uitspraak (log dagen):\n"); print(ROND(R$B_duur)); print(R$B_mediaan); print(R$B_cohort)
## ontwerpen die nooit werden vastgesteld (tot september 2026), naar de datum van het ontwerp
o <- p[planstatus == "ontwerp" & type == "bestemmingsplan" & !is.na(datum)][order(dossier, datum)][!duplicated(dossier)]
o[, vastgesteld := dossier %chin% v$dossier]
o[, `:=`(D = as.integer(!is.na(afstand_m) & afstand_m <= 2500), na = as.integer(datum >= PAS), jaar_o = year(datum), gemeente = overheidscode,
         klein = !is.na(opp_ha) & opp_ha <= 10, gestrand = 100 * (!vastgesteld))]
O <- o[jaar_o %between% c(2014L, 2021L)]
mo <- feols(gestrand ~ I(D * na) | gemeente + jaar_o + D, data = O, cluster = ~gemeente, notes = FALSE)
R$B_gestrand <- cbind(ct(mo, "I(D * na)", "ontwerp nooit vastgesteld, procentpunt"),
                      O[, .(pct_nabij_voor = pct(!vastgesteld[D == 1L & na == 0L]), pct_nabij_na = pct(!vastgesteld[D == 1L & na == 1L]),
                            pct_ver_voor = pct(!vastgesteld[D == 0L & na == 0L]), pct_ver_na = pct(!vastgesteld[D == 0L & na == 1L]), ontwerpen = .N)])
print(ROND(R$B_gestrand, 2))

## ---- C. beroep met gevolgen ----------------------------------------------
gu <- fread(file.path(PATHS$derived, "uitspraken_dso.csv"), sep = ";", encoding = "UTF-8")
gu[, uitspraak_datum := as.IDate(substr(uitspraak_datum, 1, 10))]
gu1 <- gu[, .(uitspraak_datum = min(uitspraak_datum)), by = dossier]
ds[gu1, on = "dossier", uitspraak_datum := i.uitspraak_datum]
ds[, beroep := !is.na(uitspraak_datum) & uitspraak_datum >= vaststelling]
R$C_koppeling <- list(uitspraken = uniqueN(gu$uitspraak_id), dossiers = nrow(gu1),
                      bestemmingsplannen_2014_2022 = ds[type == "bestemmingsplan" & jaar %between% c(2014L, 2022L), .N],
                      met_uitspraak = ds[type == "bestemmingsplan" & jaar %between% c(2014L, 2022L), sum(beroep)])
cat(sprintf("\nC. %d gerechtelijke uitspraken aan %d dossiers; van de bestemmingsplannen vastgesteld 2014 tot en met 2022 hebben er %d een\n",
            R$C_koppeling$uitspraken, R$C_koppeling$dossiers, R$C_koppeling$met_uitspraak))
cb <- ds[type == "bestemmingsplan" & jaar %between% c(2014L, 2022L)]
cb[, `:=`(y1 = 100 * beroep, na = as.integer(vaststelling >= PAS))]
beroep_did <- function(d, naam) {
  mm <- feols(y1 ~ I(D * na) | gemeente + jaar + D, data = d, cluster = ~gemeente, notes = FALSE)
  ct(mm, "I(D * na)", naam)
}
R$C_did <- rbind(beroep_did(cb, "alle bestemmingsplannen"), beroep_did(cb[klein == TRUE], "tot 10 ha"),
                 beroep_did(cb[klein == TRUE & wonen == TRUE], "tot 10 ha met woonbestemming"))
R$C_ruw <- cb[, .(plannen = .N, pct_uitspraak = pct(beroep)), by = .(D, na)][order(D, na)]
R$C_jaar <- cb[, .(plannen = .N, pct_uitspraak = pct(beroep)), by = .(jaar, D)][order(jaar, D)]
R$C_duur_tot_uitspraak <- cb[beroep == TRUE, .(plannen = .N, mediaan_dagen = as.numeric(median(uitspraak_datum - vaststelling))), by = .(D, na)][order(D, na)]
cat("\nBeroep met gevolgen, nabij tegenover ver na de uitspraak (procentpunt, lineair):\n"); print(ROND(R$C_did)); print(ROND(R$C_ruw, 2))
print(dcast(ROND(R$C_jaar, 2), jaar ~ D, value.var = "pct_uitspraak")); print(R$C_duur_tot_uitspraak)

R$meta <- list(bron = "API Ruimtelijke plannen v4, oogst van 28 september 2026", pas = format(PAS), nabij_m = 2500,
               gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"))
writeLines(to_json(R), file.path(PATHS$output, sprintf("plannen_natuur_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("plannen_natuur_%s.json", EXTRACT)))
sink()
