## =====================================================================
## 07_plannen.R -- het plannenspoor: duur en aantal woningbouwplannen
##
## Input : analyses/analysis_stikstof/analysis_files/afstand_n2000_plannen.dta
##         (Huubs analysebestand: provinciale planmonitoring gekoppeld aan
##          BAG, N2000-afstanden en gebiedskenmerken; 1 rij per subplan x jaar)
## Output: _output/plannen_<extract>.json
##
## Port van het blok PLANNEN DUUR / PLANNEN AANTAL uit
## do_analysis_stikstof_jaar.do (r. 546-1040). De opzet is dezelfde:
##
##   (a) zacht -> hard      kans dat een plan in jaar t wordt vastgesteld
##   (b) hard -> realisatie kans dat een vastgesteld plan wordt opgeleverd
##   (c) volledig           van eerste initiatief tot oplevering
##   (d) aantal plannen     nieuwe plannen per provincie x afstandsklasse x jaar
##
## (a)-(c) zijn discrete-tijd duurmodellen (complementair log-log) op
## plan-jaren, zoals Stata's `stset ... ; cloglog _d ...`. (d) is Poisson
## met vaste effecten, zoals ppmlhdfe.
##
## Afwijkingen van de do-file, expliciet:
##  - `plansplitsing` zit niet in het bestand en valt als controle weg.
##  - De event study gebruikt dezelfde controles als de DiD (de do-file
##    laat provincie_cat en woningprijsindex_d in een van de drie ES-blokken
##    weg; dat lijkt onbedoeld).
##  - Standaardfouten: geclusterd op afstandsklasse (zoals de tekst van het
##    conceptartikel), met provincie x afstandsklasse en subplan_id als
##    alternatieven in de tabel.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages({library(fixest); library(haven)})
options(width = 200)

PLANNEN_DTA <- "D:/Radboud Universiteit/Woningbouw RU-VU - General/analyses/analysis_stikstof/analysis_files/afstand_n2000_plannen.dta"
sink(file.path(PATHS$output, sprintf("plannen_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()

## dezelfde mini-JSON-schrijver als in 06_rapport.R
to_json <- function(x) {
  esc <- function(s) { s <- gsub("\\\\", "\\\\\\\\", s); s <- gsub("\"", "\\\\\"", s); s <- gsub("\n", "\\\\n", s); paste0("\"", s, "\"") }
  scalar <- function(v) {
    if (is.na(v)) return("null")
    if (is.logical(v)) return(if (v) "true" else "false")
    if (inherits(v, "Date")) return(esc(format(v)))
    if (is.numeric(v)) { if (!is.finite(v)) return("null"); return(format(round(v, 5), scientific = FALSE, trim = TRUE)) }
    esc(as.character(v))
  }
  if (is.null(x)) return("null")
  if (is.data.frame(x)) { x <- as.data.frame(x)
    return(paste0("[", paste(sapply(seq_len(nrow(x)), function(i) paste0("{", paste(sprintf("%s:%s", esc(names(x)), sapply(names(x), function(n) scalar(x[[n]][i]))), collapse = ","), "}")), collapse = ",\n"), "]")) }
  if (is.list(x)) { if (!is.null(names(x)) && all(names(x) != "")) return(paste0("{", paste(sprintf("%s:%s", esc(names(x)), sapply(x, to_json)), collapse = ",\n"), "}"))
    return(paste0("[", paste(sapply(x, to_json), collapse = ","), "]")) }
  if (length(x) == 1 && is.null(names(x))) return(scalar(x))
  paste0("[", paste(sapply(x, scalar), collapse = ","), "]")
}

## ---------------------------------------------------------------------
## Inlezen
## ---------------------------------------------------------------------
kop("07_plannen.R")
p <- as.data.table(zap_labels(read_dta(PLANNEN_DTA)))
setnames(p, "n2000_afstandsklasse_250", "kl")
cat(sprintf("plan-jaren: %s, subplannen: %s, jaren %d-%d\n", nfmt(nrow(p)), nfmt(uniqueN(p$subplan_id)), min(p$jaar), max(p$jaar)))
p[, contour := !is.na(obsv_id_contour) & obsv_id_contour != ""]
for (dr in PARAMS$drempels) p[, paste0("kl", dr) := as.integer(kl <= dr / PARAMS$klasse_breedte)]
p[, `:=`(prov_afstand = paste(provinciecode, kl), afstand = kl)]

R <- list(meta = list(extract = EXTRACT, bron = basename(PLANNEN_DTA), gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
                      n_planjaren = nrow(p), n_subplannen = uniqueN(p$subplan_id)))

## Descriptief: plannen (eerste waarneming) per provincie en jaar
p[, eerste := seq_len(.N) == 1L, by = .(subplan_id)]
setorder(p, subplan_id, jaar)
p[, eerste := seq_len(.N) == 1L, by = subplan_id]
R$descriptief <- list(
  per_provincie = p[eerste == TRUE, .(plannen = .N, eerste_jaar = min(jaar), laatste_jaar = max(jaar)), by = .(provinciecode, provincienaam)][order(provinciecode)],
  sample = p[eerste == TRUE, .N, by = sample][order(sample)]
)
print(R$descriptief$per_provincie)

## ---------------------------------------------------------------------
## Discrete-tijd risicoset, zoals stset met id() en origin()
## ---------------------------------------------------------------------
## Per subplan: rijen met jaar > origin (t = jaar - origin > 0), oplopend,
## tot en met de eerste rij waarin fail == 1; daarna niets meer.
risicoset <- function(dt, origin, fail) {
  dt <- copy(dt)[!is.na(get(origin))]
  dt[, t := jaar - get(origin)]
  dt <- dt[t > 0]
  setorder(dt, subplan_id, jaar)
  dt[, d := as.integer(get(fail))]
  dt[, na_fail := cumsum(shift(d, fill = 0L)), by = subplan_id]
  dt <- dt[na_fail == 0]
  dt[, na_fail := NULL]
  dt[]
}

## Event-tijd zoals de do-file: -20..-5 -> 1, -4 -> 2, ..., 5 -> 11 (ref 5 = 2018)
event_code <- function(jaar) pmin(pmax(jaar - 2019L + 6L, 1L), 11L)
EVENT_LABELS <- c("<= -5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5")

## Schatters ------------------------------------------------------------
CONTROLES <- "woningprijsindex_d + binnen_kern"
FE_PLAN   <- "afstand + jaar + t + planomvang_cat + inwoners_cat + provincie_cat"

fit_cloglog_did <- function(dt, dr, cl) {
  dt[, tr := get(paste0("kl", dr)) * as.integer(jaar >= 2019)]
  m <- feglm(as.formula(sprintf("d ~ tr + %s | %s", CONTROLES, FE_PLAN)), data = dt,
             family = binomial("cloglog"), cluster = as.formula(paste0("~", cl)), notes = FALSE)
  list(b = coef(m)[["tr"]], se = se(m)[["tr"]], n = m$nobs)
}
fit_cloglog_es <- function(dt, dr) {
  dt[, D := get(paste0("kl", dr))]
  m <- feglm(as.formula(sprintf("d ~ i(event, D, ref = 5) + %s | %s", CONTROLES, FE_PLAN)), data = dt,
             family = binomial("cloglog"), cluster = ~afstand, notes = FALSE)
  cf <- coef(m); s <- se(m); V <- vcov(m)
  ev <- as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  keep <- !is.na(ev)
  tab <- rbind(data.table(event = ev[keep], b = cf[keep], se = s[keep]), data.table(event = 5L, b = 0, se = 0))[order(event)]
  tab[, `:=`(label = EVENT_LABELS[event], hr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
  ## gemiddelde 2019-2022 (event 6..9), zoals lincom
  nm <- names(cf)[keep][ev[keep] %in% 6:9]
  w <- rep(1 / length(nm), length(nm)); est <- sum(w * cf[nm]); sev <- sqrt(as.numeric(t(w) %*% V[nm, nm] %*% w))
  list(tab = tab, gem = list(hr = exp(est), lo = exp(est - 1.96 * sev), hi = exp(est + 1.96 * sev), b = est, se = sev))
}

analyse_duur <- function(naam, dt) {
  kop("%s", naam)
  dt <- dt[!is.na(planomvang_cat) & !is.na(woningprijsindex_d) & jaar < 2023]
  dt[, jaar := pmax(jaar, 2007L)]
  dt[, event := event_code(jaar)]
  n_plannen <- uniqueN(dt[t == 1, subplan_id])
  cat(sprintf("plan-jaren in risicoset: %s, plannen: %s, failures: %s\n", nfmt(nrow(dt)), nfmt(n_plannen), nfmt(sum(dt$d))))
  did <- rbindlist(lapply(PARAMS$drempels, function(dr) rbindlist(lapply(c("afstand", "prov_afstand", "subplan_id"), function(cl) {
    r <- tryCatch(fit_cloglog_did(dt, dr, cl), error = function(e) list(b = NA_real_, se = NA_real_, n = NA_integer_))
    data.table(drempel = dr, cluster = cl, b = r$b, se = r$se, hr = exp(r$b), n = r$n)
  }))))
  es <- lapply(c(500, 1000, 2500, 5000), function(dr) {
    r <- tryCatch(fit_cloglog_es(dt, dr), error = function(e) { message("ES faalt op ", dr, ": ", conditionMessage(e)); NULL })
    if (is.null(r)) return(NULL)
    list(drempel = dr, tab = r$tab, gem = r$gem)
  })
  names(es) <- c("500", "1000", "2500", "5000")
  print(dcast(did[, .(drempel, cluster, v = sprintf("%.3f (%.3f)", hr, se))], drempel ~ cluster, value.var = "v"))
  for (k in names(es)) if (!is.null(es[[k]])) cat(sprintf("  ES %5s m: gem. HR 2019-2022 %.3f [%.3f - %.3f]\n", k, es[[k]]$gem$hr, es[[k]]$gem$lo, es[[k]]$gem$hi))
  list(n_planjaren = nrow(dt), n_plannen = n_plannen, n_failures = sum(dt$d), did = did, es = es,
       per_jaar = dt[, .(planjaren = .N, failures = sum(d)), by = jaar][order(jaar)])
}

## ---------------------------------------------------------------------
## (a) ZACHT -> HARD
## ---------------------------------------------------------------------
a <- p[provinciecode %in% c(26, 27, 31)]   # CBS-provinciecodes: 26 Utrecht, 27 Noord-Holland, 31 Limburg
a[, hard := as.integer(planstatus_binair_impute == 1)]
a[is.na(hard), hard := 0L]
a[, sample_analysis := fifelse(sample == 1, fifelse(contour, 2L, 1L), 0L)]
a <- a[sample_analysis %in% 1:2]
ra <- risicoset(a, "jaar_eerst_plan", "hard")
R$zacht_hard <- analyse_duur("(a) zacht -> hard, NH + Utrecht + Limburg", ra)

## ---------------------------------------------------------------------
## (b) HARD -> REALISATIE
## ---------------------------------------------------------------------
b <- p[provinciecode %in% c(27, 31)]
b[, sample_analysis := 0L]
b[sample == 1 & contour, sample_analysis := 1L]
b[sample_analysis == 0 & sample == 3 & contour, sample_analysis := 2L]
b[sample_analysis == 1 & !is.na(jaar_laatst_plan) & jaar_laatst_plan <= 2020 & is.na(jaar_laatst_bouw), sample_analysis := 3L]
b <- b[sample_analysis %in% 1:3]
b[, rs_min := { v <- realisatiestatus[realisatiestatus != 0 & !is.na(realisatiestatus)]; if (length(v)) min(v) else NA_real_ }, by = subplan_id]
b[, realisatie := as.integer(!is.na(rs_min) & realisatiestatus == rs_min)]
b[, jaar_hard := { v <- jaar[planstatus_binair_impute == 1 & !is.na(planstatus_binair_impute)]; if (length(v)) min(v) else NA_integer_ }, by = subplan_id]
rb <- risicoset(b, "jaar_hard", "realisatie")
rb[, t := pmin(t, 14L)]
R$hard_realisatie <- analyse_duur("(b) hard -> realisatie, NH + Limburg", rb)

## ---------------------------------------------------------------------
## (c) VOLLEDIG: eerste initiatief -> realisatie
## ---------------------------------------------------------------------
cc <- p[provinciecode %in% c(27, 31)]
cc[, sample_analysis := fifelse(sample == 1 & contour, 1L, 0L)]
cc[sample_analysis == 1 & !is.na(jaar_laatst_plan) & jaar_laatst_plan <= 2020 & is.na(jaar_laatst_bouw), sample_analysis := 2L]
cc <- cc[sample_analysis %in% 1:2]
cc[, rs_min := { v <- realisatiestatus[realisatiestatus != 0 & !is.na(realisatiestatus)]; if (length(v)) min(v) else NA_real_ }, by = subplan_id]
cc[, realisatie := as.integer(!is.na(rs_min) & realisatiestatus == rs_min)]
rc <- risicoset(cc, "jaar_eerst_plan", "realisatie")
R$volledig <- analyse_duur("(c) volledig: initiatief -> realisatie, NH + Limburg", rc)

## ---------------------------------------------------------------------
## (d) AANTAL NIEUWE PLANNEN
## ---------------------------------------------------------------------
kop("(d) aantal nieuwe plannen, NH + Limburg, 2013-2022")
dd <- p[!is.na(gemeentenaam_2024) & gemeentenaam_2024 != "" & provinciecode %in% c(27, 31) & sample %in% 1:2 & jaar > 2012 & jaar < 2023]
dd <- dd[eerste == TRUE]
cel <- dd[, .(plannen = .N, omvang = mean(plancap_bruto_valide, na.rm = TRUE)), by = .(provinciecode, kl, jaar)]
vol <- CJ(provinciecode = sort(unique(cel$provinciecode)), kl = 1:41, jaar = 2013:2022)
pd <- merge(vol, cel, by = c("provinciecode", "kl", "jaar"), all.x = TRUE); pd[is.na(plannen), plannen := 0]
for (dr in PARAMS$drempels) pd[, paste0("kl", dr) := as.integer(kl <= dr / PARAMS$klasse_breedte)]
pd[, `:=`(prov_afstand = paste(provinciecode, kl), prov_tijd = paste(provinciecode, jaar), na_2019 = as.integer(jaar >= 2019))]
did_d <- rbindlist(lapply(PARAMS$drempels, function(dr) {
  pd[, tr := get(paste0("kl", dr)) * na_2019]
  m <- fepois(plannen ~ tr | prov_tijd + prov_afstand, data = pd, cluster = ~prov_afstand, notes = FALSE)
  data.table(drempel = dr, b = coef(m)[["tr"]], se = se(m)[["tr"]], irr = exp(coef(m)[["tr"]]), n = m$nobs)
}))
print(did_d)
es_d <- lapply(c(500, 1000, 2500, 5000), function(dr) {
  pd[, D := get(paste0("kl", dr))]
  m <- fepois(plannen ~ i(jaar, D, ref = 2018) | prov_tijd + prov_afstand, data = pd, cluster = ~prov_afstand, notes = FALSE)
  cf <- coef(m); s <- se(m); V <- vcov(m)
  yr <- as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  tab <- rbind(data.table(jaar = yr, b = cf, se = s), data.table(jaar = 2018L, b = 0, se = 0))[order(jaar)]
  tab[, `:=`(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
  nm <- names(cf)[yr %in% 2019:2022]; w <- rep(1 / length(nm), length(nm))
  est <- sum(w * cf[nm]); sev <- sqrt(as.numeric(t(w) %*% V[nm, nm] %*% w))
  list(drempel = dr, tab = tab, gem = list(irr = exp(est), lo = exp(est - 1.96 * sev), hi = exp(est + 1.96 * sev)))
})
names(es_d) <- c("500", "1000", "2500", "5000")
for (k in names(es_d)) cat(sprintf("  ES %5s m: gem. IRR 2019-2022 %.3f [%.3f - %.3f]\n", k, es_d[[k]]$gem$irr, es_d[[k]]$gem$lo, es_d[[k]]$gem$hi))
R$aantal <- list(did = did_d, es = es_d,
                 per_jaar = dd[, .(plannen = .N, woningen = sum(plancap_bruto_valide, na.rm = TRUE)), by = .(jaar, nabij = kl2500)][order(jaar, nabij)],
                 n_plannen = nrow(dd))

## ---------------------------------------------------------------------
## Wegschrijven
## ---------------------------------------------------------------------
writeLines(to_json(R), file.path(PATHS$output, sprintf("plannen_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec", as.numeric(difftime(Sys.time(), t0, units = "secs")))
sink()
