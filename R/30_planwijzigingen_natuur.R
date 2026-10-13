## =====================================================================
## 30_planwijzigingen_natuur.R -- planwijzigingen nabij natuur rond de
##                                uitspraken over intern salderen
##
## Input : _derived/plannen_dso.csv (29a_plannen_dso.py: elke planversie uit de
##         oogst van de API Ruimtelijke plannen v4, met oppervlakte en kleinste
##         afstand tot het habitat); _derived/wijzigingen_lv.csv
##         (30a_wijzigingen_lv.py: de ontwerpen en besluiten in de nieuwe
##         standaard uit de DSO-LV, met het gebied van de wijziging)
## Output: _output/planwijzigingen_natuur_<extract>.json en .txt
##
## Sinds 1 januari 2024 verandert een gemeente haar plan langs drie wegen:
##   A. een bestemmingsplan onder overgangsrecht (ontwerp voor 2024 ter inzage),
##      vastgesteld tot en met 2026;
##   B. een TAM-omgevingsplan: een wijziging van het omgevingsplan in IMRO-vorm
##      (tijdelijke alternatieve maatregel), ontwerpen van 2024 tot en met 2025;
##   C. een wijziging van het omgevingsplan in de nieuwe standaard (STOP/TPOD),
##      alleen in de DSO-LV (bron\dso\werk\dso_lv_omgevingsplan.py).
## A en B staan in de oogst van de API Ruimtelijke plannen, met contour; bij C
## is het gebied de vereniging van de gewijzigde locaties uit de renvooi.
##
## Wisseling: de uitspraak over intern salderen (18 december 2024) en die over
## het bestemmingsplan Pasgeld-West (14 januari 2025: ook voor een plan geen
## intern salderen zonder passende beoordeling). Hoofdmaat: na vanaf 2025Q1;
## variant vanaf 2024Q4, het kwartaal van de eerste uitspraak.
##
## 1. Doorstroom van de pijplijn (A): per bestemmingsplan met een ontwerp in
##    2021 tot en met 2023 en per kwartaal vanaf het ontwerp: werd het plan dit
##    kwartaal vastgesteld? Lineair in procentpunten met gemeente x kwartaal,
##    gemeente x nabij en de looptijd sinds het ontwerp als vaste effecten;
##    nabij x na is de verandering van de kans nabij tegenover verderop in
##    dezelfde gemeente en hetzelfde kwartaal. Plannen die de uitspraak nodig
##    hadden (intern salderen bij een plan nabij natuur), zouden na de
##    uitspraak trager moeten doorstromen. Met een event study per halfjaar.
## 2. Dezelfde doorstroom voor de TAM-omgevingsplannen (B), ontwerp 2024 en
##    2025; de voorperiode is kort (167 ontwerpen in 2024).
## 3. Aantal TAM-ontwerpen nabij tegenover verderop, 2024Q1 tot en met
##    2025Q4 (Poisson, gemeente x nabij en gemeente x kwartaal). Verkennend:
##    zonder de wijzigingen in de nieuwe standaard (C) is de telling
##    onvolledig, en gemeenten kozen hun weg zelf.
## 4. Ontwerpen van planwijzigingen tot 10 ha langs alle drie de wegen als een
##    reeks, 2019Q1 tot en met 2026Q2, nabij tegenover ver binnen gemeente en
##    kwartaal, met een eigen term voor het overgangsjaar 2024; en alleen de
##    nieuwe wegen (B en C) vanaf 2024 met 2024 als voorperiode.
## 5. Hetzelfde voor de vaststellingen.
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
sink(file.path(PATHS$output, sprintf("planwijzigingen_natuur_%s.txt", EXTRACT)), split = TRUE)
R <- list()
pct <- function(v) if (!length(v) || all(is.na(v))) NA_real_ else 100 * mean(v, na.rm = TRUE)
ROND <- function(x, d = 3) x[, lapply(.SD, function(v) if (is.numeric(v)) round(v, d) else v)]
KW_START <- 2012L
kw <- function(d) (as.integer(format(d, "%Y")) - KW_START) * 4L + (as.integer(format(d, "%m")) - 1L) %/% 3L + 1L
kw_lab <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)
hj_lab <- function(q) sprintf("%dH%d", KW_START + (q - 1L) %/% 4L, ((q - 1L) %% 4L) %/% 2L + 1L)
Q_SAL <- kw(as.IDate("2024-12-18"))   # 2024Q4
Q_NA <- kw(as.IDate("2025-01-01"))    # 2025Q1, hoofdmaat
Q_EIND <- kw(as.IDate("2026-06-30"))  # 2026Q2: het laatste volle kwartaal voor de oogst van 28 september 2026
ct <- function(m, termen, naam) {
  x <- coeftable(m)
  data.table(groep = naam, term = termen, b = x[termen, 1], se = x[termen, 2], p = x[termen, 4], n = nobs(m))
}

## ---- 0. dossiers --------------------------------------------------------
p <- fread(file.path(PATHS$derived, "plannen_dso.csv"), sep = ";", encoding = "UTF-8",
           colClasses = list(character = c("identificatie", "dossier", "overheidscode", "overheidsnaam", "naam", "planstatus",
                                           "beroep_en_bezwaar", "dossierstatus")))
p[, datum := as.IDate(substr(datum, 1, 10))]
p <- p[type == "bestemmingsplan" & grepl("^[0-9]{4}$", overheidscode) & !startsWith(overheidscode, "99") & overheidscode != "0000" &
       !grepl("^(provincie|ministerie|rijk)", tolower(overheidsnaam)) & !is.na(dossier) & dossier != "" & !is.na(datum)]
## TAM-omgevingsplan: de vlag isTamPlan van de API; in de oogst dragen alle 3.423 versies met die vlag TAM in de naam
p[, tam := grepl("TAM", naam, fixed = TRUE)]
RX_WONEN <- "woning|wonen|woon|appartement|woningbouw|nieuwbouw"
ont <- p[planstatus == "ontwerp"][order(dossier, datum, identificatie)][!duplicated(dossier)]
vst <- p[planstatus == "vastgesteld"][order(dossier, datum, identificatie)][!duplicated(dossier)]
ds <- ont[, .(dossier, tam, gemeente = overheidscode, naam, ontwerp = datum, opp_ha, afstand_m)]
ds[vst, on = "dossier", vaststelling := i.datum]
ds <- ds[!is.na(afstand_m) & (is.na(vaststelling) | vaststelling >= ontwerp)]
ds[, `:=`(D = as.integer(afstand_m <= 2500), klein = !is.na(opp_ha) & opp_ha <= 10, wonen = grepl(RX_WONEN, naam, ignore.case = TRUE),
          q_o = kw(ontwerp), q_v = fifelse(is.na(vaststelling), NA_integer_, kw(vaststelling)))]
kop("Ontwerpdossiers van gemeentelijke bestemmingsplannen en TAM-omgevingsplannen")
R$ontwerpen_per_kwartaal <- ds[q_o >= kw(as.IDate("2022-01-01")) & q_o <= Q_EIND,
                               .(ontwerpen = .N, pct_nabij = pct(D == 1L), pct_klein = pct(klein), pct_vastgesteld = pct(!is.na(vaststelling))),
                               by = .(stroom = fifelse(tam, "TAM-omgevingsplan", "bestemmingsplan"), kwartaal = kw_lab(q_o))][order(stroom, kwartaal)]
print(ROND(R$ontwerpen_per_kwartaal, 1))
R$vaststellingen_per_kwartaal <- p[planstatus == "vastgesteld"][order(dossier, datum)][!duplicated(dossier)][
  datum >= as.IDate("2022-01-01") & kw(datum) <= Q_EIND,
  .(vastgesteld = .N, pct_nabij = pct(!is.na(afstand_m) & afstand_m <= 2500)),
  by = .(stroom = fifelse(tam, "TAM-omgevingsplan", "bestemmingsplan"), kwartaal = kw_lab(kw(datum)))][order(stroom, kwartaal)]
print(ROND(R$vaststellingen_per_kwartaal, 1))

## ---- 1 en 2. doorstroom van ontwerp naar vaststelling ----------------------
risico <- function(d, t_van) {
  ## een rij per dossier en kwartaal, vanaf het kwartaal van het ontwerp tot en met de vaststelling of 2026Q2
  d <- d[is.na(q_v) | q_v >= q_o]
  d[, q_tot := pmin(fifelse(is.na(q_v), Q_EIND, q_v), Q_EIND)]
  d <- d[q_tot >= q_o]
  pa <- d[, .(t = seq(q_o, q_tot)), by = .(dossier, gemeente, D, klein, wonen, q_o, q_v)]
  pa[, `:=`(y = 100 * as.integer(!is.na(q_v) & t == q_v), duur = pmin(t - q_o, 12L))]
  pa <- pa[t >= t_van]
  pa[, `:=`(gem_t = paste(gemeente, t), gem_D = paste(gemeente, D), na = D * as.integer(t >= Q_NA), na4 = D * as.integer(t >= Q_SAL),
            half = hj_lab(t))]
  pa[]
}
doorstroom <- function(pa, naam, ref_half) {
  m1 <- feols(y ~ na | gem_t + gem_D + duur, data = pa, cluster = ~gemeente, notes = FALSE)
  m2 <- feols(y ~ na4 | gem_t + gem_D + duur, data = pa, cluster = ~gemeente, notes = FALSE)
  es <- feols(as.formula(sprintf('y ~ i(half, D, ref = "%s") | gem_t + gem_D + duur', ref_half)), data = pa, cluster = ~gemeente, notes = FALSE)
  x <- coeftable(es)
  list(did = rbind(cbind(ct(m1, "na", naam), na_vanaf = "2025Q1"), cbind(ct(m2, "na4", naam), na_vanaf = "2024Q4")),
       es = data.table(groep = naam, half = sub("^half::([0-9H]+):D$", "\\1", rownames(x)), b = x[, 1], se = x[, 2], p = x[, 4]),
       ruw = pa[, .(plan_kwartalen = .N, dossiers = uniqueN(dossier), kans_pct = mean(y)), by = .(D, periode = fifelse(t >= Q_NA, "na", "voor"))][order(D, periode)])
}
T1 <- kw(as.IDate("2022-01-01"))
A <- ds[tam == FALSE & ontwerp >= as.IDate("2021-01-01") & ontwerp <= as.IDate("2023-12-31")]
B <- ds[tam == TRUE & ontwerp >= as.IDate("2024-01-01") & ontwerp <= as.IDate("2025-12-31")]
kop("1. Pijplijn onder overgangsrecht: %s bestemmingsplannen met een ontwerp in 2021 tot en met 2023 (%s nabij), %s vastgesteld tot en met 2026Q2",
    nfmt(nrow(A)), nfmt(A[, sum(D)]), nfmt(A[!is.na(q_v) & q_v <= Q_EIND, .N]))
## geen groep met woonbestemming: de PDOK-laag kent alleen de vastgestelde versies, en in de naam van het ontwerp staat wonen
## bij 232 van de 7.095 plannen
GROEP <- list(`alle` = quote(TRUE), `tot 10 ha` = quote(klein == TRUE))
uitkomst <- list()
for (g in names(GROEP)) {
  uitkomst[[paste("A", g)]] <- doorstroom(risico(A[eval(GROEP[[g]])], T1), paste("bestemmingsplan onder overgangsrecht,", g), "2024H2")
}
kop("2. TAM-omgevingsplannen: %s ontwerpen in 2024 en 2025 (%s nabij), %s vastgesteld tot en met 2026Q2",
    nfmt(nrow(B)), nfmt(B[, sum(D)]), nfmt(B[!is.na(q_v) & q_v <= Q_EIND, .N]))
for (g in names(GROEP)) {
  uitkomst[[paste("B", g)]] <- doorstroom(risico(B[eval(GROEP[[g]])], kw(as.IDate("2024-01-01"))), paste("TAM-omgevingsplan,", g), "2024H2")
}
R$doorstroom_did <- rbindlist(lapply(uitkomst, `[[`, "did"))
R$doorstroom_es <- rbindlist(lapply(uitkomst, `[[`, "es"))
R$doorstroom_ruw <- rbindlist(lapply(uitkomst, `[[`, "ruw"), idcol = "groep")
cat("\nKans op vaststelling per kwartaal (procentpunt), nabij tegenover ver na de uitspraken, binnen gemeente en kwartaal:\n")
print(ROND(R$doorstroom_did))
cat("\nEvent study per halfjaar tegenover 2024H2:\n"); print(ROND(R$doorstroom_es[startsWith(groep, "bestemmingsplan")], 2))
cat("\nRuw:\n"); print(ROND(R$doorstroom_ruw, 2))

## ---- 3. aantal TAM-ontwerpen nabij en ver ------------------------------------
Qb0 <- kw(as.IDate("2024-01-01")); Qb1 <- kw(as.IDate("2025-12-31"))
tel <- function(sub, naam) {
  cel <- sub[q_o >= Qb0 & q_o <= Qb1, .(y = .N), by = .(gemeente, D, t = q_o)]
  gem <- unique(cel$gemeente)
  pa <- merge(CJ(gemeente = gem, D = 0:1, t = Qb0:Qb1), cel, by = c("gemeente", "D", "t"), all.x = TRUE)
  pa[is.na(y), y := 0L]
  pa[, `:=`(gem_D = paste(gemeente, D), gem_t = paste(gemeente, t), na = D * as.integer(t >= Q_NA), na4 = D * as.integer(t >= Q_SAL))]
  m1 <- fepois(y ~ na | gem_D + gem_t, data = pa, cluster = ~gemeente, notes = FALSE)
  m2 <- fepois(y ~ na4 | gem_D + gem_t, data = pa, cluster = ~gemeente, notes = FALSE)
  list(did = rbind(cbind(ct(m1, "na", naam), na_vanaf = "2025Q1"), cbind(ct(m2, "na4", naam), na_vanaf = "2024Q4")),
       ruw = pa[, .(gemeenten = uniqueN(gemeente), per_kwartaal = sum(y) / uniqueN(t)), by = .(D, periode = fifelse(t >= Q_NA, "na", "voor"))][order(D, periode)])
}
t3 <- list(alle = tel(ds[tam == TRUE], "TAM-ontwerpen"), klein = tel(ds[tam == TRUE & klein == TRUE], "TAM-ontwerpen tot 10 ha"))
R$tam_telling_did <- rbindlist(lapply(t3, `[[`, "did"))[, pct := 100 * (exp(b) - 1)]
R$tam_telling_ruw <- rbindlist(lapply(t3, `[[`, "ruw"), idcol = "groep")
kop("3. Aantal TAM-ontwerpen nabij tegenover ver, 2024Q1 tot en met 2025Q4 (Poisson, gemeente x nabij en gemeente x kwartaal)")
print(ROND(R$tam_telling_did)); print(ROND(R$tam_telling_ruw, 2))

## ---- 4 en 5. planwijzigingen langs de drie wegen, nabij en ver ---------------
## Ontwerpen (4) en vaststellingen (5) van bestemmingsplannen (A), TAM-omgevingsplannen (B) en wijzigingen in de nieuwe
## standaard (C, uit 30a_wijzigingen_lv.py) als een reeks, 2019Q1 tot en met 2026Q2. De wisseling van instrument in 2024
## geldt nabij en ver tegelijk en valt weg in gemeente x kwartaal; 2024 krijgt een eigen term (overgang), zodat na (vanaf
## 2025Q1) het verschil met 2019 tot en met 2023 meet, en na min overgang het verschil met 2024. Alleen wijzigingen tot
## 10 ha (projectwijzigingen); wijzigingen zonder eigen locatie (bruidsschat, ambtsgebied) of met meer dan 200 locaties
## hebben geen oppervlakte en tellen niet mee.
lv_pad <- file.path(PATHS$derived, "wijzigingen_lv.csv")
if (file.exists(lv_pad)) {
  lv <- fread(lv_pad, sep = ";", encoding = "UTF-8", colClasses = list(character = c("soort", "technisch_id", "gemeente", "titel")))
  lv[, `:=`(bekend_op = as.IDate(substr(bekend_op, 1, 10)), vaststelling = as.IDate(substr(vaststelling, 1, 10)))]
  R$lv_dekking <- lv[, .(documenten = .N, met_renvooi = sum(renvooi == 200, na.rm = TRUE), met_afstand = sum(!is.na(afstand_m)),
                         groot = sum(groot == 1), alleen_groep = sum(alleen_groep == 1), tot_10_ha = sum(!is.na(opp_ha) & opp_ha <= 10)), by = soort]
  kop("4 en 5. Planwijzigingen langs de drie wegen; dekking van de wijzigingen in de nieuwe standaard (DSO-LV)")
  print(R$lv_dekking)
  Q19 <- kw(as.IDate("2019-01-01")); Q24 <- kw(as.IDate("2024-01-01"))
  ev_o <- rbind(ds[tam == FALSE, .(gemeente, datum = ontwerp, opp_ha, afstand_m, stroom = "A")],
                ds[tam == TRUE, .(gemeente, datum = ontwerp, opp_ha, afstand_m, stroom = "B")],
                lv[soort == "ontwerp" & !is.na(afstand_m), .(gemeente, datum = bekend_op, opp_ha, afstand_m, stroom = "C")])
  ## vaststellingen: bij A en B de eerste vastgestelde versie per dossier, bij C het besluit (datum van bekendmaking)
  vv <- p[planstatus == "vastgesteld" & !is.na(afstand_m)][order(dossier, datum, identificatie)][!duplicated(dossier)]
  ev_v <- rbind(vv[, .(gemeente = overheidscode, datum, opp_ha, afstand_m, stroom = fifelse(tam, "B", "A"))],
                lv[soort == "besluit" & !is.na(afstand_m), .(gemeente, datum = bekend_op, opp_ha, afstand_m, stroom = "C")])
  reeks <- function(ev, naam, van = Q19, alleen = NULL) {
    e <- ev[!is.na(datum) & !is.na(opp_ha) & opp_ha <= 10]
    if (!is.null(alleen)) e <- e[stroom %chin% alleen]
    e[, `:=`(t = kw(datum), D = as.integer(afstand_m <= 2500))]
    e <- e[t >= van & t <= Q_EIND]
    cel <- e[, .(y = .N), by = .(gemeente, D, t)]
    pa <- merge(CJ(gemeente = unique(e$gemeente), D = 0:1, t = van:Q_EIND), cel, by = c("gemeente", "D", "t"), all.x = TRUE)
    pa[is.na(y), y := 0L]
    pa[, `:=`(gem_D = paste(gemeente, D), gem_t = paste(gemeente, t), D_szn = paste(D, (t - 1L) %% 4L),
              overgang = D * as.integer(t >= Q24 & t < Q_NA), na = D * as.integer(t >= Q_NA), jaar = KW_START + (t - 1L) %/% 4L)]
    ruw <- e[, .(wijzigingen = .N, pct_nabij = pct(D == 1L)), by = .(jaar = KW_START + (t - 1L) %/% 4L, stroom)][order(jaar, stroom)]
    if (van < Q24) {
      m <- fepois(y ~ overgang + na | gem_D + gem_t + D_szn, data = pa, cluster = ~gemeente, notes = FALSE)
      v <- vcov(m)
      d <- data.table(groep = naam, term = "na tegenover 2024", b = coef(m)[["na"]] - coef(m)[["overgang"]],
                      se = sqrt(v["na", "na"] + v["overgang", "overgang"] - 2 * v["na", "overgang"]), p = NA_real_, n = nobs(m))
      d[, p := 2 * pnorm(-abs(b / se))]
      did <- rbind(ct(m, c("overgang", "na"), naam), d)
      mj <- fepois(y ~ i(jaar, D, ref = 2023) | gem_D + gem_t + D_szn, data = pa, cluster = ~gemeente, notes = FALSE)
      x <- coeftable(mj)
      es <- data.table(groep = naam, jaar = as.integer(sub(".*::([0-9]{4}):D", "\\1", rownames(x))), b = x[, 1], se = x[, 2], p = x[, 4])
    } else {
      m <- fepois(y ~ na | gem_D + gem_t + D_szn, data = pa, cluster = ~gemeente, notes = FALSE)
      did <- ct(m, "na", naam)
      es <- NULL
    }
    list(did = did, es = es, ruw = cbind(groep = naam, ruw))
  }
  W <- list(reeks(ev_o, "ontwerpen A+B+C"), reeks(ev_o, "ontwerpen B+C, alleen de nieuwe wegen", van = Q24, alleen = c("B", "C")),
            reeks(ev_v, "vaststellingen A+B+C"), reeks(ev_v, "vaststellingen B+C, alleen de nieuwe wegen", van = Q24, alleen = c("B", "C")))
  R$planwijz_did <- rbindlist(lapply(W, `[[`, "did"))[, pct := 100 * (exp(b) - 1)]
  R$planwijz_jaar <- rbindlist(lapply(W, `[[`, "es"))[, pct := 100 * (exp(b) - 1)]
  R$planwijz_ruw <- rbindlist(lapply(W[c(1, 3)], `[[`, "ruw"))
  cat("\nPlanwijzigingen tot 10 ha nabij tegenover ver (Poisson, gemeente x nabij, gemeente x kwartaal, nabij x seizoen):\n")
  print(ROND(R$planwijz_did))
  cat("\nPer jaar tegenover 2023:\n"); print(dcast(ROND(R$planwijz_jaar, 1), jaar ~ groep, value.var = "pct"))
  cat("\nRuw per jaar en weg:\n"); print(ROND(R$planwijz_ruw, 1))
} else {
  cat("\n4 en 5 overgeslagen: _derived/wijzigingen_lv.csv ontbreekt (30a_wijzigingen_lv.py na de oogst van de DSO-LV)\n")
}

R$meta <- list(bron = "API Ruimtelijke plannen v4, oogst van 28 september 2026", intern_salderen = "2024-12-18", pasgeld_west = "2025-01-14",
               na_vanaf = kw_lab(Q_NA), variant_na_vanaf = kw_lab(Q_SAL), eind = kw_lab(Q_EIND), nabij_m = 2500,
               gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"))
writeLines(to_json(R), file.path(PATHS$output, sprintf("planwijzigingen_natuur_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("planwijzigingen_natuur_%s.json", EXTRACT)))
sink()
