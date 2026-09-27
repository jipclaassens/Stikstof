## =====================================================================
## 06_rapport.R -- alle analyses voor het rapport, in een run
##
## Input : _derived/vbo_analyse.rds
## Output: _output/rapport_<extract>.json   (alle cijfers, tabellen, reeksen)
##         _output/rapport_<extract>.txt    (leesbare log)
##
## Het rapport (een HTML-artifact) leest deze JSON in. Alles wat in het
## rapport staat komt hiervandaan, zodat elk cijfer herleidbaar is naar
## een regel code en een extract.
##
## Opbouw volgt het verhaal van het rapport:
##   1. data        wat er in het objectenbestand zit
##   2. project     documentnummer vs. ruimtelijk-temporeel cluster
##   3. series      de ruwe reeksen, behandeld vs. controle
##   4. es          event study per kwartaal, 2019 en 2024
##   5. regime      regime-DiD met incrementele stappen
##   6. robust      robuustheid: filters, id's, venster, clustering, placebo
##   7. depositie   behandeling op depositie (EIB-matrix) i.p.v. afstand
##   8. uitval      cohortanalyse met vast opvolgvenster
##   9. homes       vertaling naar aantallen woningen
##
## Rekentijd. Blok 7b tot 7d (de duurmodellen) is verreweg het zwaarste en
## zit in een cache: zie met_cache() in 00_config.R. Een wijziging in de
## tekst of in een beschrijvende tabel kost daardoor seconden in plaats van
## minuten. De cache vervalt vanzelf bij een nieuw extract of een nieuwe
## vbo_analyse.rds. Verander je de SPECIFICATIE van een duurmodel, hoog dan
## het versienummer in de met_cache-aanroep op. Alles opnieuw rekenen:
##   Rscript 06_rapport.R --geen-cache
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))
options(width = 200)

sink(file.path(PATHS$output, sprintf("rapport_%s.txt", EXTRACT)), split = TRUE)
t0 <- Sys.time()

R <- list()   # het resultaatobject; weggeschreven met to_json() uit 00_config.R

## ---------------------------------------------------------------------
## Data, tijdindex, gebeurtenissen
## ---------------------------------------------------------------------
d <- read_derived("vbo_analyse")

KW_START <- 2012L
kw_index <- function(datum) {
  y <- as.integer(format(datum, "%Y")); m <- as.integer(format(datum, "%m"))
  (y - KW_START) * 4L + (m - 1L) %/% 3L + 1L
}
kw_label <- function(q) sprintf("%dQ%d", KW_START + (q - 1L) %/% 4L, (q - 1L) %% 4L + 1L)

EVENTS <- data.table(
  naam   = c("PAS-uitspraak", "Logtsebaan", "bouwvrijstelling", "Porthos", "Omgevingswet + Wkb", "intern salderen", "Pasgeld-West"),
  datum  = as.Date(c("2019-05-29", "2021-01-20", "2021-07-01", "2022-11-02", "2024-01-01", "2024-12-18", "2025-01-14")),
  soort  = c("aanscherping", "versoepeling", "versoepeling", "aanscherping", "procedure", "aanscherping", "aanscherping"),
  hoofd  = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
  ecli   = c("ECLI:NL:RVS:2019:1603", "ECLI:NL:RVS:2021:71", "Wsn, Stb. 2021, 140", "ECLI:NL:RVS:2022:3159", "Ow / Wkb", "ECLI:NL:RVS:2024:4923", "ECLI:NL:RVS:2025:87"),
  strekking = c(
    "PAS mag niet meer als toestemmingsbasis; per project passende beoordeling",
    "intern salderen niet vergunningplichtig (terugwerkend tot 1-1-2020)",
    "bouwfase-emissies buiten de toets (Wet stikstofreductie en natuurverbetering)",
    "bouwvrijstelling in strijd met de Habitatrichtlijn",
    "Wnb wordt Ow art. 5.1, aanhaakplicht vervalt; gevolgklasse 1 bouwmelding i.p.v. technische bouwvergunning (geen wegvallende registraties in de BAG, wel een ander registratiemoment; 13_binnenring.R)",
    "intern salderen niet meer in de voortoets; natuurvergunning weer nodig",
    "ook voor bestemmingsplannen geen intern salderen zonder passende beoordeling"))
EVENTS[, q := kw_index(datum)]
EVENTS[, kw := kw_label(q)]
Q_REF <- kw_index(as.Date(c("2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01")))
Q_PAS <- EVENTS[naam == "PAS-uitspraak", q]
Q_POR <- EVENTS[naam == "Porthos", q]
Q_SAL <- EVENTS[naam == "intern salderen", q]
Q_MAX_VERG <- kw_index(as.Date("2026-03-31"))   # 2026Q2 = batchregistratie
Q_MAX_NWB  <- kw_index(as.Date("2026-06-30"))

R$meta <- list(extract = EXTRACT, bag_datum = "20260710", gemaakt = format(Sys.time(), "%Y-%m-%d %H:%M"),
               kw_start = KW_START, q_ref = Q_REF, q_pas = Q_PAS, q_por = Q_POR, q_sal = Q_SAL,
               q_max_verg = Q_MAX_VERG, q_max_nwb = Q_MAX_NWB,
               kw_labels = kw_label(1:Q_MAX_NWB))
R$events <- EVENTS[, .(naam, datum, kwartaal = kw, q, soort, hoofd, ecli, strekking)]

## ---------------------------------------------------------------------
## 1. DATA
## ---------------------------------------------------------------------
kop("1. Data")
w <- d[vbo_gebruiksdoel == "woon"]
s <- d[in_sample == TRUE]

R$data <- list(
  totaal = list(objecten = nrow(d), panden = uniqueN(d$pand_bag_nr), projecten = uniqueN(d$projectid),
                met_vergunning = sum(d$bron_vergunning), in_cbs_nieuwbouw = sum(d$cbs_nieuwbouw),
                alleen_nieuwbouw = sum(d$cbs_nieuwbouw & !d$bron_vergunning),
                woon = nrow(w), in_sample = nrow(s)),
  bronnen = data.table(
    bestand = c("Object_BouwvergunningVerleend", "Object_Nieuwbouw", "Pand_VergunningDuur"),
    eenheid = c("verblijfsobject", "verblijfsobject", "pand"),
    rijen = c(1313420L, 927382L, 620816L),
    inhoud = c("elk object waarvoor een bouwvergunning is verleend, met eindstatus van object en pand",
               "elk object dat CBS als nieuwbouw telt, met de mutatiemaand",
               "tijdlijn per pand: eerste vergunning, bouwstart, oplevering, bouwjaar")),
  projecttype = w[, .N, by = projecttype][order(-N)],
  uitkomstgroep = s[, .(groep = uitkomstgroep, N = .N), by = uitkomstgroep][order(uitkomstgroep)][, .(groep, N, pct = round(100 * N / nrow(s), 2))],
  objectgroep  = { u6 <- d[in_universum == TRUE]
              u6[, .N, by = objectgroep][order(objectgroep)][, .(groep = objectgroep, N, pct = round(100 * N / nrow(u6), 2))] },
  gebruik = { u6 <- d[in_universum == TRUE]
              list(universum = nrow(u6),
                   verg = u6[in_sample_verg == TRUE, .N, by = objectgroep][order(objectgroep)],
                   nwb  = u6[in_sample_nwb  == TRUE, .N, by = objectgroep][order(objectgroep)]) },
  uitkomst = s[, .N, by = uitkomst][order(-N)][, .(uitkomst, N, pct = round(100 * N / nrow(s), 2))],
  vlaggen = data.table(
    vlag = c("pand_vbo_inconsistent", "later_gesloopt", "uitkomst_ambigu", "vlag_duur_negatief", "vlag_delta_grijs", "gebruiksdoel_afgeleid"),
    N = c(sum(s$pand_vbo_inconsistent), sum(s$later_gesloopt), sum(s$uitkomst_ambigu),
          sum(s$vlag_duur_negatief), sum(s$vlag_delta_grijs), sum(s$gebruiksdoel_afgeleid))),
  koppelgraad = d[cbs_nieuwbouw == TRUE & !is.na(mutatiejaar) & mutatiejaar <= 2025,
                  .(nieuwbouw = .N, zonder_vergunning = sum(!bron_vergunning),
                    pct = round(100 * mean(!bron_vergunning), 1)), by = .(jaar = mutatiejaar)][order(jaar)]
)

## Aansluiting tussen de StatLine-vergelijking en de groepstabel. Die twee
## tellen bewust niet hetzelfde: de StatLine-tabel vergelijkt ALLE CBS-nieuwbouw
## over 2012-2025 met de CBS-reeks, de groepstabel beschrijft de ex-ante
## analysesteekproef over alle jaren. Zonder deze brug lijkt het een fout.
buiten <- d[cbs_nieuwbouw == TRUE & in_sample == FALSE]
buiten[, reden := fcase(
  projecttype == "admin_fout",                            "projecttype is administratieve fout",
  projecttype == "verbouwing",                            "projecttype is verbouwing",
  is.na(vbo_gebruiksdoel) | vbo_gebruiksdoel != "woon",   "gebruiksdoel is niet woon",
  is.na(provinciecode),                                   "provincie ontbreekt",
  is.na(dist2n2000_m),                                    "afstand tot N2000 ontbreekt",
  default =                                               "overig")]
R$data$aansluiting <- list(
  cbs_totaal     = sum(d$cbs_nieuwbouw),
  jaar_2026      = sum(d$cbs_nieuwbouw & d$mutatiejaar == 2026, na.rm = TRUE),
  tabel_statline = sum(d$cbs_nieuwbouw & d$mutatiejaar <= 2025, na.rm = TRUE),
  buiten_sample  = nrow(buiten),
  tabel_groepen  = sum(d$cbs_nieuwbouw & d$in_sample),
  beide_filters  = sum(d$cbs_nieuwbouw & d$in_sample & d$mutatiejaar <= 2025, na.rm = TRUE),
  redenen        = buiten[, .N, by = reden][order(-N)])
cat("\nAansluiting CBS-nieuwbouw: totaal", nfmt(R$data$aansluiting$cbs_totaal),
    "| StatLine-tabel", nfmt(R$data$aansluiting$tabel_statline),
    "| groepstabel", nfmt(R$data$aansluiting$tabel_groepen), "\n")

## beslismatrix: pand x object x cbs -> groep
s[, pand := fifelse(is.na(last_pand_status), "(geen vergunningrecord)", last_pand_status)]
s[, vbo  := fifelse(is.na(vbo_status), "(geen vergunningrecord)", vbo_status)]
R$data$matrix <- s[, .(N = .N, ambigu = sum(uitkomst_ambigu), gesloopt = sum(later_gesloopt)),
                    by = .(cbs = cbs_nieuwbouw, pand, vbo, groep = uitkomstgroep)][N >= 200][order(groep, -N)]

## censurering: aandeel pipeline per vergunningjaar x nabij
sv <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= 2025]
sv[, nabij := fifelse(kl2500 == 1, "nabij", "ver")]
R$data$censurering <- dcast(sv[, .(pct = round(100 * mean(uitkomst == "pipeline"), 1)), by = .(jaar = vergunning_jaar, nabij)],
                            jaar ~ nabij, value.var = "pct")
dur <- s[!is.na(duur_tot_oplevering) & duur_tot_oplevering > 0 & vergunning_jaar >= 2012]
R$data$doorlooptijd <- list(mediaan_jr = round(median(dur$duur_tot_oplevering) / 365.25, 2),
                            p25 = round(quantile(dur$duur_tot_oplevering, .25) / 365.25, 2),
                            p75 = round(quantile(dur$duur_tot_oplevering, .75) / 365.25, 2))
print(R$data$uitkomstgroep); print(R$data$koppelgraad)

## ---------------------------------------------------------------------
## 2. PROJECT-ID: documentnummer vs. ruimtelijk-temporeel cluster
## ---------------------------------------------------------------------
kop("2. Project-id")
s[, pid_doc := projectid]
## losse objecten: het rijnummer als sleutel, niet vbo_bag_nr (integer64;
## zonder geladen bit64 vallen zestiencijferige nummers in paste() samen,
## zie 01_build_vbo.R). In versie 1.9 van het rapport voegde dat 11.452
## losse objecten samen tot pseudoclusters.
s[, pid_cl  := fifelse(!is.na(cluster_rel), paste(gemeentecode, cluster_rel), paste0("__los_", .I))]
s[, omv_doc := .N, by = pid_doc]
s[, omv_cl  := .N, by = pid_cl]
s[, dep_doc := omv_doc * mol_woning]
s[, dep_cl  := omv_cl  * mol_woning]
eib_cut <- function(x) as.integer(cut(x, c(-Inf, 0.005, 0.1, 0.5, 1, Inf), labels = 1:5))
s[, depk_doc := eib_cut(dep_doc)]
s[, depk_cl  := eib_cut(dep_cl)]
EIB_LABELS <- c("< 0,005", "0,005 - 0,1", "0,1 - 0,5", "0,5 - 1", "> 1")

sz <- function(v) { n <- s[bron_vergunning == TRUE, .N, by = v]$N
  list(projecten = length(n), mediaan = median(n), p75 = quantile(n, .75)[[1]], p90 = quantile(n, .9)[[1]],
       p99 = quantile(n, .99)[[1]], max = max(n), aandeel_singleton = round(mean(n == 1), 3)) }
x1 <- s[bron_vergunning == TRUE, .(n_doc = uniqueN(pid_doc)), by = pid_cl]
x2 <- s[bron_vergunning == TRUE, .(n_cl = uniqueN(pid_cl)), by = pid_doc]
R$project <- list(
  documentnr = sz("pid_doc"), cluster = sz("pid_cl"),
  nesting = list(clusters_met_1_doc = round(mean(x1$n_doc == 1), 3), clusters_met_meer_doc = round(mean(x1$n_doc > 1), 3),
                 mediaan_docs_bij_meer = median(x1$n_doc[x1$n_doc > 1]),
                 docs_in_1_cluster = round(mean(x2$n_cl == 1), 3)),
  eib_labels = EIB_LABELS,
  eib_crosstab = as.data.table(table(doc = s$depk_doc, cl = s$depk_cl))[, .(doc = as.integer(doc), cl = as.integer(cl), N)],
  eib_verdeling = rbind(s[, .(id = "documentnummer", .N), by = .(klasse = depk_doc)],
                        s[, .(id = "cluster", .N), by = .(klasse = depk_cl)])[order(id, klasse)]
)
cat("projecten doc:", R$project$documentnr$projecten, " cluster:", R$project$cluster$projecten, "\n")

## ---------------------------------------------------------------------
## Paneelbouwer en schatters (gedeeld door 3-8)
## ---------------------------------------------------------------------
## dt: objecten; tijd: kolomnaam (q of jaar); klasse: kolom met klasse-index;
## id: project-id kolom; n_k: aantal klassen. Per project de MINIMUM klasse.
bouw_paneel <- function(dt, tijd, klasse, id, n_k, seizoen = FALSE) {
  pp  <- dt[, .(k = min(get(klasse)), n = .N), by = c(id, "provinciecode", tijd)]
  cel <- pp[, .(y = sum(n)), by = c("provinciecode", "k", tijd)]
  setnames(cel, tijd, "t")
  vol <- CJ(provinciecode = sort(unique(cel$provinciecode)), k = 1:n_k, t = min(cel$t):max(cel$t))
  p <- merge(vol, cel, by = c("provinciecode", "k", "t"), all.x = TRUE)
  p[is.na(y), y := 0]
  p[, `:=`(prov_t = paste(provinciecode, t), prov_k = paste(provinciecode, k),
           k_szn = if (seizoen) paste(k, (t - 1L) %% 4L) else "0")]
  p[]
}
fe_formule <- function(p) if (uniqueN(p$k_szn) > 1) "prov_t + prov_k + k_szn" else "prov_t + prov_k"

fit_es <- function(p, ref, cl = ~prov_k) {
  m <- fepois(as.formula(sprintf("y ~ i(t, D, ref = c(%s)) | %s", paste(ref, collapse = ","), fe_formule(p))),
              data = p, cluster = cl, notes = FALSE)
  cf <- coef(m); se <- se(m)
  tt <- as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  tab <- rbind(data.table(t = tt, b = cf, se = se),
               data.table(t = ref, b = 0, se = 0))[order(t)]
  tab[, `:=`(irr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
  list(model = m, tab = tab)
}
wald_pre <- function(m, ref_min) {
  b <- coef(m); V <- vcov(m)
  tt <- as.integer(sapply(strsplit(names(b), ":"), `[`, 3))
  idx <- which(tt < ref_min)
  W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE]) %*% b[idx])
  list(stat = W, df = length(idx), p = pchisq(W, length(idx), lower.tail = FALSE))
}
gem_regime <- function(m, t_van, t_tot) {
  b <- coef(m); V <- vcov(m)
  tt <- as.integer(sapply(strsplit(names(b), ":"), `[`, 3))
  idx <- which(tt >= t_van & tt <= t_tot); w <- rep(1 / length(idx), length(idx))
  est <- sum(w * b[idx]); se <- sqrt(as.numeric(t(w) %*% V[idx, idx] %*% w))
  list(b = est, se = se, irr = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se), n = length(idx))
}
fit_did <- function(p, post_van, cl = ~prov_k) {
  p[, tr := D * as.integer(t >= post_van)]
  m <- fepois(as.formula(sprintf("y ~ tr | %s", fe_formule(p))), data = p, cluster = cl, notes = FALSE)
  list(b = coef(m)[["tr"]], se = se(m)[["tr"]], n = m$nobs)
}
fit_regime <- function(p, cl = ~prov_k) {
  p[, `:=`(na_pas = D * as.integer(t >= Q_PAS), na_por = D * as.integer(t >= Q_POR), na_sal = D * as.integer(t >= Q_SAL))]
  m2 <- fepois(as.formula(sprintf("y ~ na_pas + na_sal | %s", fe_formule(p))), data = p, cluster = cl, notes = FALSE)
  m3 <- fepois(as.formula(sprintf("y ~ na_pas + na_por + na_sal | %s", fe_formule(p))), data = p, cluster = cl, notes = FALSE)
  list(pas = coef(m2)[["na_pas"]], pas_se = se(m2)[["na_pas"]], sal = coef(m2)[["na_sal"]], sal_se = se(m2)[["na_sal"]],
       pas3 = coef(m3)[["na_pas"]], pas3_se = se(m3)[["na_pas"]], por3 = coef(m3)[["na_por"]], por3_se = se(m3)[["na_por"]],
       sal3 = coef(m3)[["na_sal"]], sal3_se = se(m3)[["na_sal"]])
}
zet_D_afstand <- function(p, dr) { p[, D := as.integer(k <= dr / PARAMS$klasse_breedte)]; invisible(p) }
N_KL <- as.integer(PARAMS$klasse_max_m / PARAMS$klasse_breedte) + 1L

## Steekproeven per kwartaal
verg <- s[!is.na(vergunning_datum) & vergunning_datum >= as.Date("2012-01-01")]
verg[, q := kw_index(vergunning_datum)]; verg <- verg[q >= 1 & q <= Q_MAX_VERG]
nwb <- d[cbs_nieuwbouw == TRUE & projecttype != "admin_fout" & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
nwb[, q := kw_index(mutatiemaand)]; nwb <- nwb[q >= 1 & q <= Q_MAX_NWB]
nwb[, pid_cl := fifelse(!is.na(cluster_rel), paste(gemeentecode, cluster_rel), paste0("__los_", .I))]
nwb[, pid_doc := projectid]
nwb[, jaar := mutatiejaar]; verg[, jaar := vergunning_jaar]
UIT <- list(vergunningen = verg, nieuwbouw = nwb)

## ---------------------------------------------------------------------
## 3. SERIES
## ---------------------------------------------------------------------
kop("3. Reeksen")
R$series <- list()
for (u in names(UIT)) {
  dt <- UIT[[u]]
  kw <- dt[, .(y = .N), by = .(q, D = kl2500)]
  basis <- kw[q %in% Q_REF, .(basis = mean(y)), by = D]
  kw <- merge(kw, basis, by = "D")[, index := y / basis][order(D, q)]
  jr <- dt[jaar <= 2025, .(y = .N), by = .(jaar, D = kl2500)][order(D, jaar)]
  bjr <- jr[jaar == 2018, .(basis = y), by = D]; jr <- merge(jr, bjr, by = "D")[, index := y / basis]
  R$series[[u]] <- list(kwartaal = kw[, .(q, D, y, index = round(index, 3))],
                        jaar = jr[, .(jaar, D, y, index = round(index, 3))],
                        totaal_per_jaar = dt[jaar <= 2025, .N, by = jaar][order(jaar)])
}

## power: hoeveel zit er achter elke drempel
pn0 <- bouw_paneel(nwb, "q", "afstandsklasse", "pid_doc", N_KL, seizoen = TRUE)
R$power <- rbindlist(lapply(PARAMS$drempels, function(dr) {
  zet_D_afstand(pn0, dr)
  data.table(drempel = dr, klassen = uniqueN(pn0$k[pn0$D == 1]), cellen = sum(pn0$D == 1),
             objecten = sum(pn0$y[pn0$D == 1]), aandeel = round(100 * sum(pn0$y[pn0$D == 1]) / sum(pn0$y), 1))
}))
print(R$power)

## ---------------------------------------------------------------------
## 4. EVENT STUDY per kwartaal (hoofdanalyse)
## ---------------------------------------------------------------------
kop("4. Event study per kwartaal")
R$es <- list(); R$regime <- list()
for (u in names(UIT)) {
  p <- bouw_paneel(UIT[[u]], "q", "afstandsklasse", "pid_doc", N_KL, seizoen = TRUE)
  q_max <- max(p$t)
  out <- list(); reg <- list()
  for (dr in c(500, 1000, 2500, 5000)) {
    zet_D_afstand(p, dr)
    es <- fit_es(p, Q_REF)
    w  <- wald_pre(es$model, min(Q_REF))
    g1 <- gem_regime(es$model, Q_PAS, Q_SAL - 1L); g2 <- gem_regime(es$model, Q_SAL, q_max)
    g0 <- gem_regime(es$model, 1L, min(Q_REF) - 1L)
    out[[as.character(dr)]] <- list(drempel = dr, coef = es$tab[, .(t, kw = kw_label(t), b, se, irr, lo, hi)],
                                    pre_wald = w, gem_pre = g0, gem_pas = g1, gem_sal = g2)
    reg[[as.character(dr)]] <- c(list(drempel = dr), fit_regime(p))
    cat(sprintf("%-12s %5dm  pre p=%.3f  pas IRR %.3f [%.3f-%.3f]  sal IRR %.3f [%.3f-%.3f]\n", u, dr, w$p,
                g1$irr, g1$lo, g1$hi, g2$irr, g2$lo, g2$hi))
  }
  R$es[[u]] <- out; R$regime[[u]] <- rbindlist(lapply(reg, as.data.table))
}

## ---------------------------------------------------------------------
## 5. ROBUUSTHEID (enkelvoudige DiD op PAS, coefficient op behandeld x post)
## ---------------------------------------------------------------------
kop("5. Robuustheid")
robuust <- function(u) {
  dt <- UIT[[u]]
  rij <- function(naam, p, post = Q_PAS, cl = ~prov_k, tijd = "kw") {
    rbindlist(lapply(c(1000, 2500), function(dr) {
      zet_D_afstand(p, dr); r <- fit_did(p, post, cl)
      data.table(variant = naam, drempel = dr, b = r$b, se = r$se, n = r$n)
    }))
  }
  P <- function(x, tijd = "q", id = "pid_doc", szn = TRUE) bouw_paneel(x, tijd, "afstandsklasse", id, N_KL, seizoen = szn)
  out <- list(
    rij("basis (kwartaal, documentnr, seizoens-FE)", P(dt)),
    rij("zonder seizoens-FE", P(dt, szn = FALSE)),
    rij("jaar i.p.v. kwartaal", P(dt, tijd = "jaar", szn = FALSE), post = 2019),
    rij("cluster-id i.p.v. documentnr", P(dt, id = "pid_cl")),
    rij("vanaf 2014", P(dt[q >= kw_index(as.Date("2014-01-01"))])),
    rij("t/m 2023 (voor Omgevingswet)", P(dt[q <= kw_index(as.Date("2023-12-31"))])),
    rij("cluster se: afstandsklasse", P(dt), cl = ~k),
    rij("cluster se: provincie", P(dt), cl = ~provinciecode),
    rij("placebo: 2016Q1 op 2012-2018", P(dt[q <= max(Q_REF)]), post = kw_index(as.Date("2016-01-01")))
  )
  if (u == "nieuwbouw") {
    alles <- d[cbs_nieuwbouw == TRUE & !is.na(dist2n2000_m) & !is.na(provinciecode) & !is.na(mutatiemaand)]
    alles[, q := kw_index(mutatiemaand)]; alles <- alles[q >= 1 & q <= Q_MAX_NWB]; alles[, pid_doc := projectid]
    out <- c(out, list(rij("geen statusfilter", P(alles))))
    st <- alles[!grepl("Niet_gerealiseerd_pand|Pand_gesloopt|Sloopvergunning_verleend|Pand_ten_onrechte_opgevoerd", pand_status_mutatie)]
    out <- c(out, list(rij("Stata-filter (pand_status snapshot)", P(st))))
    ## de pooling-bug reproduceren
    pool <- copy(alles); pool[bron_vergunning == FALSE, pid_doc := paste("NA", gemeentecode, q)]
    out <- c(out, list(rij("projectid-NA gepoold (Stata)", P(pool))))
  }
  rbindlist(out)
}
R$robust <- list(nieuwbouw = robuust("nieuwbouw"), vergunningen = robuust("vergunningen"))
print(dcast(R$robust$nieuwbouw[, .(variant, drempel, v = sprintf("%+.3f (%.3f)", b, se))], variant ~ drempel, value.var = "v"))

## ---------------------------------------------------------------------
## 6. DEPOSITIE als behandeling (EIB-matrix), per kwartaal
## ---------------------------------------------------------------------
kop("6. Depositie als behandeling")
nwb <- merge(nwb, s[, .(vbo_bag_nr, depk_doc, depk_cl)], by = "vbo_bag_nr", all.x = TRUE)
nwb[is.na(depk_doc), depk_doc := eib_cut(mol_woning)]   # losse objecten: omvang 1
nwb[is.na(depk_cl),  depk_cl  := eib_cut(mol_woning)]
UIT$nieuwbouw <- nwb
R$depositie <- list(); zet_D_dep <- function(p, k_min) { p[, D := as.integer(k >= k_min)]; invisible(p) }
for (u in names(UIT)) {
  for (id in c("doc", "cl")) {
    p <- bouw_paneel(UIT[[u]], "q", paste0("depk_", id), paste0("pid_", id), 5L, seizoen = TRUE)
    for (k_min in c(3L, 4L)) {
      zet_D_dep(p, k_min)
      es <- fit_es(p, Q_REF); w <- wald_pre(es$model, min(Q_REF))
      g1 <- gem_regime(es$model, Q_PAS, Q_SAL - 1L); g2 <- gem_regime(es$model, Q_SAL, max(p$t))
      dd <- fit_did(p, Q_PAS)
      nm <- sprintf("%s_%s_%s", u, id, if (k_min == 3) "va01" else "va05")
      R$depositie[[nm]] <- list(uitkomst = u, id = id, drempel = if (k_min == 3) ">= 0,1 mol" else ">= 0,5 mol",
                                did_b = dd$b, did_se = dd$se, pre_p = w$p, gem_pas = g1, gem_sal = g2,
                                aandeel_behandeld = round(100 * sum(p$y[p$D == 1]) / sum(p$y), 1),
                                coef = es$tab[, .(t, kw = kw_label(t), irr, lo, hi)])
      cat(sprintf("%-12s %-4s %-10s DiD %+.3f (%.3f)  pre p=%.3f  aandeel %.1f%%\n", u, id,
                  R$depositie[[nm]]$drempel, dd$b, dd$se, w$p, R$depositie[[nm]]$aandeel_behandeld))
    }
  }
}

## ---------------------------------------------------------------------
## 7. UITVAL en PIJPLIJN: cohortanalyse met vast venster
## ---------------------------------------------------------------------
kop("7. Uitval en pijplijn")
co <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= 2021]
co[, `:=`(D = kl2500, post = as.integer(vergunning_jaar >= 2019), y4 = as.integer(gerealiseerd_binnen_4jr),
          prov_jaar = paste(provinciecode, vergunning_jaar), prov_k = paste(provinciecode, afstandsklasse))]
cat("cohorten 2012-2021, NA in gerealiseerd_binnen_4jr:", sum(is.na(co$y4)), "\n")
tab4 <- dcast(co[!is.na(y4), .(pct = round(100 * mean(y4), 1), N = .N), by = .(jaar = vergunning_jaar, nabij = fifelse(D == 1, "nabij", "ver"))],
              jaar ~ nabij, value.var = c("pct", "N"))
m4 <- feols(y4 ~ D:post | prov_jaar + prov_k, data = co[!is.na(y4)], cluster = ~prov_k)
es4 <- feols(y4 ~ i(vergunning_jaar, D, ref = 2018) | prov_jaar + prov_k, data = co[!is.na(y4)], cluster = ~prov_k)
cf <- coef(es4); se4 <- se(es4)
## uitval-eindstand voor oude cohorten (<= 2019, minstens 6 jaar opvolging)
uit <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= 2019]
tabu <- dcast(uit[, .(pct = round(100 * mean(uitkomst == "uitval"), 2)), by = .(jaar = vergunning_jaar, nabij = fifelse(kl2500 == 1, "nabij", "ver"))],
              jaar ~ nabij, value.var = "pct")
R$uitval <- list(
  binnen4jr = tab4,
  did = list(b = coef(m4)[["D:post"]], se = se(m4)[["D:post"]], n = m4$nobs,
             basis_nabij_pre = round(mean(co[D == 1 & post == 0 & !is.na(y4), y4]), 3)),
  es = data.table(jaar = as.integer(sapply(strsplit(names(cf), ":"), `[`, 3)), b = cf, se = se4)[order(jaar)],
  uitval_eind = tabu,
  pipeline = R$data$censurering
)
cat(sprintf("gerealiseerd binnen 4 jaar, DiD (pp): %+.4f (%.4f)\n", coef(m4)[["D:post"]], se(m4)[["D:post"]]))
print(tab4)

## ---------------------------------------------------------------------
## 7b. DUUR: vensters, doorlooptijd, en een hazardmodel met censurering
## ---------------------------------------------------------------------
## Drie manieren om naar dezelfde vraag te kijken (duurt het langer, of
## komt het er niet, dicht bij natuur na 2019):
##  (i)   vaste vensters van 3, 4 en 5 jaar: kans op oplevering binnen n
##        jaar, per cohort volledig waargenomen
##  (ii)  doorlooptijd onder de opgeleverde objecten (mediaan in dagen)
##  (iii) Cox proportional hazards op tijd tot oplevering, met objecten
##        die nog niet zijn opgeleverd als gecensureerd op de extractdatum.
##        Baseline hazard gestratificeerd op provincie x afstandsklasse,
##        cohortdummies, en behandeld x post als de DiD-term.
##        Uitval wordt hier als censurering behandeld (cause-specific hazard).
suppressPackageStartupMessages(library(survival))

## Blok 7b tot en met 7d is het rekenzware deel van het rapport: Cox op ruim
## een miljoen objecten en een Fine-Gray op een steekproef van 150.000. Samen
## goed voor het leeuwendeel van de looptijd. Ze zitten daarom in een cache,
## zodat een tekstuele of beschrijvende wijziging niet de volle rekentijd
## kost. De cache vervalt vanzelf bij een nieuw extract of een nieuwe
## vbo_analyse.rds; hoog het versienummer hieronder op zodra je de
## SPECIFICATIE van een van de drie blokken verandert.
R$duur <- met_cache("duur", "1", {

kop("7b. Duur")
DU <- list()

## (i) vensters
vensters <- rbindlist(lapply(c(3L, 4L, 5L), function(j) {
  v <- paste0("gerealiseerd_binnen_", j, "jr")
  cj <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= (2025L - j) & !is.na(get(v))]
  cj[, `:=`(y = as.integer(get(v)), D = kl2500, post = as.integer(vergunning_jaar >= 2019),
            prov_jaar = paste(provinciecode, vergunning_jaar), prov_k = paste(provinciecode, afstandsklasse))]
  m <- feols(y ~ D:post | prov_jaar + prov_k, data = cj, cluster = ~prov_k)
  data.table(venster_jr = j, cohorten = sprintf("2012-%d", 2025L - j), b = coef(m)[["D:post"]], se = se(m)[["D:post"]], n = m$nobs,
             basis_nabij_pre = round(mean(cj[D == 1 & post == 0, y]), 3), basis_ver_pre = round(mean(cj[D == 0 & post == 0, y]), 3))
}))
DU$vensters <- vensters
print(vensters)

## (ii) doorlooptijd onder opgeleverde objecten, cohorten 2012-2021
dd <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= 2021 &
          !is.na(duur_tot_oplevering) & duur_tot_oplevering > 0 & duur_tot_oplevering <= 4 * 365.25]
dd[, `:=`(nabij = fifelse(kl2500 == 1, "nabij", "ver"),
          duur_bouwstart = as.integer(bouwstart_eerste_datum - vergunning_datum))]
DU$mediaan <- dcast(dd[, .(med = as.double(median(duur_tot_oplevering)), N = .N), by = .(jaar = vergunning_jaar, nabij)],
                        jaar ~ nabij, value.var = c("med", "N"))
DU$mediaan_bouwstart <- dcast(dd[!is.na(duur_bouwstart) & duur_bouwstart >= 0,
                                     .(med = as.double(median(duur_bouwstart))), by = .(jaar = vergunning_jaar, nabij)],
                                  jaar ~ nabij, value.var = "med")
dd[, `:=`(D = kl2500, post = as.integer(vergunning_jaar >= 2019), lnd = log(duur_tot_oplevering),
          prov_jaar = paste(provinciecode, vergunning_jaar), prov_k = paste(provinciecode, afstandsklasse))]
mld <- feols(lnd ~ D:post | prov_jaar + prov_k, data = dd, cluster = ~prov_k)
DU$logduur_did <- list(b = coef(mld)[["D:post"]], se = se(mld)[["D:post"]], n = mld$nobs)
cat(sprintf("log(doorlooptijd) DiD onder opgeleverd binnen 4 jr: %+.4f (%.4f)\n", coef(mld)[["D:post"]], se(mld)[["D:post"]]))
print(DU$mediaan)

## (iii) Cox met censurering, cohorten 2012-2025
cb <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= 2025 & !is.na(opvolgtijd_dagen) & opvolgtijd_dagen > 0]
cb[, event := as.integer(!is.na(duur_tot_oplevering) & duur_tot_oplevering > 0 & duur_tot_oplevering <= opvolgtijd_dagen)]
cb[, tijd  := fifelse(event == 1L, as.numeric(duur_tot_oplevering), as.numeric(opvolgtijd_dagen))]
cb <- cb[tijd > 0]
cb[, `:=`(D = kl2500, post = as.integer(vergunning_jaar >= 2019), prov_k = paste(provinciecode, afstandsklasse),
          jaarf = factor(vergunning_jaar))]
cat(sprintf("Cox: %s objecten, %s opleveringen, %s gecensureerd\n", nfmt(nrow(cb)), nfmt(sum(cb$event)), nfmt(sum(cb$event == 0))))
cox_did <- coxph(Surv(tijd, event) ~ jaarf + D:post + strata(prov_k), data = cb, cluster = prov_k, ties = "efron")
b <- coef(cox_did)[["D:post"]]; se_b <- sqrt(vcov(cox_did)["D:post", "D:post"])
DU$cox_did <- list(b = b, se = se_b, hr = exp(b), lo = exp(b - 1.96 * se_b), hi = exp(b + 1.96 * se_b),
                       n_obj = nrow(cb), n_events = sum(cb$event))
cat(sprintf("Cox DiD: HR %.3f [%.3f - %.3f]\n", exp(b), exp(b - 1.96 * se_b), exp(b + 1.96 * se_b)))
## event study per cohort, referentie 2018
jaren <- setdiff(sort(unique(cb$vergunning_jaar)), 2018L)
## set() i.p.v. := in een for-lus: de LHS-expressie in := gaf hier kolommen
## met een andere naam dan verwacht, waardoor de coefficienten NA werden.
for (y in jaren) set(cb, j = paste0("Dy", y), value = cb$D * as.integer(cb$vergunning_jaar == y))
f_es <- as.formula(paste("Surv(tijd, event) ~ jaarf +", paste(paste0("Dy", jaren), collapse = " + "), "+ strata(prov_k)"))
cox_es <- coxph(f_es, data = cb, cluster = prov_k, ties = "efron")
ce <- coef(cox_es); cv <- sqrt(diag(vcov(cox_es)))
nm <- paste0("Dy", jaren); stopifnot(all(nm %in% names(ce)))
DU$cox_es <- rbind(
  data.table(jaar = jaren, b = ce[nm], se = cv[nm]),
  data.table(jaar = 2018L, b = 0, se = 0))[order(jaar)][, `:=`(hr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se))]
print(DU$cox_es[, .(jaar, hr = round(hr, 3), lo = round(lo, 3), hi = round(hi, 3))])

## ---------------------------------------------------------------------
## 7c. BOUWSTART als tussenstap: vergunning -> bouwstart -> oplevering
## ---------------------------------------------------------------------
## bouwstart_eerste_datum is een pandkenmerk (net als de opleverdatum) en
## bij 78% van de opgeleverde objecten bekend. Twee fasen, elk met een
## mediaan, een DiD op de logaritme en een Cox met censurering.
kop("7c. Bouwstart")
bs <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= 2021]
bs[, `:=`(d1 = as.integer(bouwstart_eerste_datum - vergunning_datum),
          d2 = as.integer(oplevering_eerste_datum - bouwstart_eerste_datum),
          nabij = fifelse(kl2500 == 1, "nabij", "ver"), D = kl2500, post = as.integer(vergunning_jaar >= 2019),
          prov_jaar = paste(provinciecode, vergunning_jaar), prov_k = paste(provinciecode, afstandsklasse))]
f1 <- bs[!is.na(d1) & d1 >= 0 & d1 <= 4 * 365.25]
f2 <- bs[!is.na(d2) & d2 > 0 & !is.na(d1) & d1 >= 0 & d2 <= 4 * 365.25]
m1 <- feols(log1p(d1) ~ D:post | prov_jaar + prov_k, data = f1, cluster = ~prov_k)
m2 <- feols(log(d2) ~ D:post | prov_jaar + prov_k, data = f2, cluster = ~prov_k)
## Cox op tijd tot bouwstart, gecensureerd op de extractdatum
cbs <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= 2025 & !is.na(opvolgtijd_dagen) & opvolgtijd_dagen > 0]
cbs[, d1 := as.integer(bouwstart_eerste_datum - vergunning_datum)]
cbs[, event := as.integer(!is.na(d1) & d1 > 0 & d1 <= opvolgtijd_dagen)]
cbs[, tijd := fifelse(event == 1L, as.numeric(d1), as.numeric(opvolgtijd_dagen))]
cbs <- cbs[tijd > 0]
cbs[, `:=`(D = kl2500, post = as.integer(vergunning_jaar >= 2019), prov_k = paste(provinciecode, afstandsklasse), jaarf = factor(vergunning_jaar))]
cox_bs <- coxph(Surv(tijd, event) ~ jaarf + D:post + strata(prov_k), data = cbs, cluster = prov_k, ties = "efron")
bb <- coef(cox_bs)[["D:post"]]; sb <- sqrt(vcov(cox_bs)["D:post", "D:post"])
DU$bouwstart <- list(
  aandeel_bekend = round(100 * mean(!is.na(bs$bouwstart_eerste_datum)), 1),
  mediaan = dcast(bs[!is.na(d1) & d1 >= 0, .(m1 = as.double(median(d1))), by = .(jaar = vergunning_jaar, nabij)], jaar ~ nabij, value.var = "m1"),
  mediaan_fase2 = dcast(bs[!is.na(d2) & d2 > 0, .(m2 = as.double(median(d2))), by = .(jaar = vergunning_jaar, nabij)], jaar ~ nabij, value.var = "m2"),
  did_fase1 = list(b = coef(m1)[["D:post"]], se = se(m1)[["D:post"]], n = m1$nobs),
  did_fase2 = list(b = coef(m2)[["D:post"]], se = se(m2)[["D:post"]], n = m2$nobs),
  cox = list(b = bb, se = sb, hr = exp(bb), lo = exp(bb - 1.96 * sb), hi = exp(bb + 1.96 * sb), n_obj = nrow(cbs), n_events = sum(cbs$event)))
cat(sprintf("fase 1 (vergunning->bouwstart) DiD log: %+.4f (%.4f); fase 2 (bouwstart->oplevering): %+.4f (%.4f)\n",
            coef(m1)[["D:post"]], se(m1)[["D:post"]], coef(m2)[["D:post"]], se(m2)[["D:post"]]))
cat(sprintf("Cox tijd tot bouwstart: HR %.3f [%.3f - %.3f]\n", exp(bb), exp(bb - 1.96 * sb), exp(bb + 1.96 * sb)))

## ---------------------------------------------------------------------
## 7d. COMPETING RISKS: oplevering tegenover definitieve uitval
## ---------------------------------------------------------------------
## Twee concurrerende eindes: het object wordt opgeleverd, of het strandt
## definitief. De uitvaldatum is de datum waarop het pand de status
## "niet gerealiseerd" of "gesloopt" kreeg (last_pand_status_datum);
## gestrande objecten in een pand dat wel doorging hebben geen eigen
## datum en blijven gecensureerd. Drie maten:
##  - cause-specific Cox voor uitval (spiegel van het model in 7b)
##  - Aalen-Johansen cumulatieve incidentie na 4 jaar, per groep x periode
##  - Fine-Gray subdistribution hazard (survival::finegray), op een
##    steekproef van 150.000 objecten omdat de gewogen dataset anders te
##    groot wordt
kop("7d. Competing risks")
cr <- s[!is.na(vergunning_jaar) & vergunning_jaar >= 2012 & vergunning_jaar <= 2025 & !is.na(opvolgtijd_dagen) & opvolgtijd_dagen > 0]
cr[, t_opl := fifelse(!is.na(duur_tot_oplevering) & duur_tot_oplevering > 0 & duur_tot_oplevering <= opvolgtijd_dagen, as.numeric(duur_tot_oplevering), NA_real_)]
cr[, t_uit := fifelse(uitkomst == "uitval" & last_pand_status %chin% c("Niet_gerealiseerd_pand", PAND_GESLOOPT) & !is.na(last_pand_status_datum),
                      as.numeric(last_pand_status_datum - vergunning_datum), NA_real_)]
cr[!is.na(t_uit) & (t_uit <= 0 | t_uit > opvolgtijd_dagen), t_uit := NA_real_]
cr[, ev := fifelse(!is.na(t_opl), 1L, fifelse(!is.na(t_uit), 2L, 0L))]
cr[, tijd := fifelse(ev == 1L, t_opl, fifelse(ev == 2L, t_uit, as.numeric(opvolgtijd_dagen)))]
cr <- cr[tijd > 0]
cr[, `:=`(D = kl2500, post = as.integer(vergunning_jaar >= 2019), prov_k = paste(provinciecode, afstandsklasse), jaarf = factor(vergunning_jaar),
          evf = factor(ev, levels = 0:2, labels = c("censor", "oplevering", "uitval")))]
cat(sprintf("objecten %s: opgeleverd %s, gestrand met datum %s, gecensureerd %s\n", nfmt(nrow(cr)), nfmt(sum(cr$ev == 1)), nfmt(sum(cr$ev == 2)), nfmt(sum(cr$ev == 0))))

## cause-specific Cox voor uitval
cox_uit <- coxph(Surv(tijd, ev == 2L) ~ jaarf + D:post + strata(prov_k), data = cr, cluster = prov_k, ties = "efron")
bu <- coef(cox_uit)[["D:post"]]; su <- sqrt(vcov(cox_uit)["D:post", "D:post"])
cat(sprintf("cause-specific Cox uitval: HR %.3f [%.3f - %.3f]\n", exp(bu), exp(bu - 1.96 * su), exp(bu + 1.96 * su)))

## Aalen-Johansen: cumulatieve incidentie per groep x periode (cohorten met >= 4 jr opvolging)
aj <- cr[vergunning_jaar <= 2021]
aj[, grp := paste(fifelse(D == 1, "nabij", "ver"), fifelse(post == 1, "post", "pre"), sep = "_")]
sf <- survfit(Surv(tijd, evf) ~ grp, data = aj)
tijden <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4) * 365.25
sm <- summary(sf, times = tijden, extend = TRUE)
cif <- data.table(grp = sub("^grp=", "", as.character(sm$strata)), tijd_jr = round(sm$time / 365.25, 2),
                  oplevering = sm$pstate[, which(sf$states == "oplevering")], uitval = sm$pstate[, which(sf$states == "uitval")])
cif4 <- cif[tijd_jr == 4]
cat("cumulatieve incidentie na 4 jaar:\n"); print(cif4)

## Fine-Gray op een steekproef
set.seed(20260730)
fgs <- cr[sample(.N, min(.N, 150000))]
fg_fit <- function(etype) {
  fgd <- finegray(Surv(tijd, evf) ~ D + post + jaarf + prov_k, data = fgs, etype = etype)
  m <- coxph(Surv(fgstart, fgstop, fgstatus) ~ jaarf + D:post + strata(prov_k), data = fgd, weight = fgwt, cluster = prov_k, ties = "efron")
  b <- coef(m)[["D:post"]]; se <- sqrt(vcov(m)["D:post", "D:post"])
  list(b = b, se = se, shr = exp(b), lo = exp(b - 1.96 * se), hi = exp(b + 1.96 * se), n = nrow(fgs))
}
fg_opl <- tryCatch(fg_fit("oplevering"), error = function(e) { message("Fine-Gray oplevering: ", conditionMessage(e)); NULL })
fg_uit <- tryCatch(fg_fit("uitval"),     error = function(e) { message("Fine-Gray uitval: ", conditionMessage(e)); NULL })
if (!is.null(fg_opl)) cat(sprintf("Fine-Gray oplevering: sHR %.3f [%.3f - %.3f]\n", fg_opl$shr, fg_opl$lo, fg_opl$hi))
if (!is.null(fg_uit)) cat(sprintf("Fine-Gray uitval    : sHR %.3f [%.3f - %.3f]\n", fg_uit$shr, fg_uit$lo, fg_uit$hi))

DU$competing <- list(
  n = list(objecten = nrow(cr), opgeleverd = sum(cr$ev == 1), gestrand = sum(cr$ev == 2), gecensureerd = sum(cr$ev == 0),
           gestrand_zonder_datum = sum(cr$uitkomst == "uitval" & cr$ev != 2)),
  cox_uitval = list(b = bu, se = su, hr = exp(bu), lo = exp(bu - 1.96 * su), hi = exp(bu + 1.96 * su)),
  cif = cif, cif4 = cif4,
  finegray_oplevering = fg_opl, finegray_uitval = fg_uit)

DU
})   ## einde cacheblok 7b tot 7d

## ---------------------------------------------------------------------
## 8. VERTALING NAAR WONINGEN
## ---------------------------------------------------------------------
kop("8. Woningen")
R$homes <- list()
for (u in names(UIT)) {
  p <- bouw_paneel(UIT[[u]], "q", "afstandsklasse", "pid_doc", N_KL, seizoen = TRUE)
  q_max <- max(p$t)
  R$homes[[u]] <- rbindlist(lapply(c(1000, 2500), function(dr) {
    zet_D_afstand(p, dr)
    g <- R$es[[u]][[as.character(dr)]]$gem_pas
    Ypost <- sum(p$y[p$D == 1 & p$t >= Q_PAS & p$t < Q_SAL]); Ytot <- sum(p$y[p$t >= Q_PAS & p$t < Q_SAL])
    ## Aantal woningen dat er meer zou zijn opgeleverd als de behandelde groep
    ## de controle had gevolgd. Let op: deze functie is DALEND in de IRR, dus
    ## de ondergrens van het interval in woningen hoort bij de BOVENgrens van
    ## de IRR en andersom. Een negatieve waarde betekent dat er juist meer is
    ## opgeleverd dan verwacht.
    minder <- function(irr) Ypost / irr - Ypost
    data.table(drempel = dr, behandeld_post = Ypost, totaal_post = Ytot,
               irr = g$irr, irr_lo = g$lo, irr_hi = g$hi,
               minder_punt = round(minder(g$irr)),
               minder_lo   = round(minder(g$hi)),
               minder_hi   = round(minder(g$lo)),
               pct_punt = round(100 * minder(g$irr) / Ytot, 2),
               pct_lo   = round(100 * minder(g$hi)  / Ytot, 2),
               pct_hi   = round(100 * minder(g$lo)  / Ytot, 2))
  }))
  print(R$homes[[u]])
}

## ---------------------------------------------------------------------
## Wegschrijven
## ---------------------------------------------------------------------
json <- to_json(R)
writeLines(json, file.path(PATHS$output, sprintf("rapport_%s.json", EXTRACT)), useBytes = TRUE)
kop("Klaar in %.0f sec -- %s", as.numeric(difftime(Sys.time(), t0, units = "secs")),
    file.path(PATHS$output, sprintf("rapport_%s.json", EXTRACT)))
sink()
