# Het vaste panel doorgetrokken tot 2026: gemeenten die al voor de Wet elektronische publicaties in het Gemeenteblad
# publiceerden, gevolgd van hun eerste volledige jaar tot het tweede kwartaal van 2026.
#
# Gebruik: Rscript analyse_vastpanel.R <analysemap>        (leest zaken.csv en panel.csv uit analyse_export.py)
# Schrijft vastpanel_resultaten.json en een csv per tabel (voorvoegsel vp_).
#
# Drie varianten van het panel: A de gemeenten met minstens 40 aanvragen in elk jaar 2016 tot 2020 (122, vanaf 2016K1),
# B minstens 20 in elk jaar 2017 tot 2020 (vanaf 2017K1), C minstens 20 in elk jaar 2018 tot 2020 (vanaf 2018K1).
# Hoofdspecificatie: Poisson met gemeente maal nabij en gemeente maal kwartaal als vaste effecten, zodat alleen nabij en
# ver binnen dezelfde gemeente en hetzelfde kwartaal worden vergeleken; wat een gemeente in een kwartaal anders deed, zoals
# de overgang naar de nieuwe publicatievorm medio 2021, valt daarmee weg. Ter vergelijking ook met alleen een landelijk
# kwartaaleffect. Gestapelde regimewisselingen: PAS-uitspraak (2019K3), bouwvrijstelling (2021K3), Porthos (2023K1),
# Omgevingswet (2024K1), intern salderen (2025K1). Referentiekwartaal van de event study 2019K1.

suppressMessages({ library(data.table); library(fixest); library(jsonlite) })
args <- commandArgs(trailingOnly = TRUE)
map <- if (length(args)) args[1] else "C:/LocalData/vergunningen/analyse"
zaken <- fread(file.path(map, "zaken.csv"), sep = ";", encoding = "UTF-8")
panel <- fread(file.path(map, "panel.csv"), sep = ";", encoding = "UTF-8")
kw_orde <- function(k) { j <- as.integer(substr(k, 1, 4)); q <- as.integer(substr(k, 6, 6)); j * 4 + q - 1 }
coefs <- function(m, naam) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "schatting", "se", "t", "p"))
  ct[, `:=`(model = naam, n = nobs(m))]
  ct[]
}
veilig <- function(expr, naam) tryCatch(expr, error = function(e) { message(naam, ": ", conditionMessage(e)); NULL })
laatste <- kw_orde("2026K2")
ev <- c(pas = kw_orde("2019K3"), bvr = kw_orde("2021K3"), porthos = kw_orde("2023K1"), ow = kw_orde("2024K1"), is = kw_orde("2025K1"))
zet_post <- function(d) d[, `:=`(post_pas = as.integer(kw_idx >= ev["pas"]), post_bvr = as.integer(kw_idx >= ev["bvr"]),
                                 post_porthos = as.integer(kw_idx >= ev["porthos"]), post_ow = as.integer(kw_idx >= ev["ow"]),
                                 post_is = as.integer(kw_idx >= ev["is"]))]
zaken[, kw_idx := kw_orde(kwartaal)]
panel[, kw_idx := kw_orde(kwartaal)]
per_jaar <- zaken[, .(n = .N), by = .(gemeente, jaar = substr(kwartaal, 1, 4))]
varianten <- list(A = list(jaren = 2016:2020, min = 40, start = "2016K1"),
                  B = list(jaren = 2017:2020, min = 20, start = "2017K1"),
                  C = list(jaren = 2018:2020, min = 20, start = "2018K1"))
res <- list(meta = list(datum = format(Sys.time(), "%Y-%m-%d %H:%M")))
did <- list(); tb <- list(); uk <- list(); besch <- list()
termen <- "nabij:post_pas + nabij:post_bvr + nabij:post_porthos + nabij:post_ow + nabij:post_is"

for (v in names(varianten)) {
  s <- varianten[[v]]
  vast <- per_jaar[jaar %in% as.character(s$jaren) & n >= s$min, .N, by = gemeente][N == length(s$jaren), gemeente]
  start <- kw_orde(s$start)
  pv <- panel[gemeente %in% vast & kw_idx >= start & kw_idx <= laatste]
  zet_post(pv)
  zv <- zaken[gemeente %in% vast & kw_idx >= start & kw_idx <= laatste]
  zet_post(zv)
  res$meta[[v]] <- list(gemeenten = length(vast), start = s$start, aanvragen = sum(pv$aanvragen), nabij = sum(pv[nabij == 1, aanvragen]),
                        drempel = s$min, jaren = paste(range(s$jaren), collapse = " tot "))
  besch[[v]] <- pv[, .(aanvragen = sum(aanvragen), bouw_aanvragen = sum(bouw_aanvragen), gemeenten = uniqueN(gemeente[aanvragen > 0])),
                   by = .(jaar = substr(kwartaal, 1, 4), nabij)][order(jaar, nabij)][, variant := v]
  pv[, kwartaal_f := factor(kwartaal, levels = sort(unique(kwartaal)))]
  # event study binnen gemeente en kwartaal
  m <- veilig(fepois(aanvragen ~ i(kwartaal_f, nabij, ref = "2019K1") | gemeente^nabij + gemeente^kwartaal_f, data = pv, cluster = ~gemeente), paste("es", v))
  if (!is.null(m)) res[[paste0("vp_event_study_", v)]] <- coefs(m, paste0("fepois aanvragen ~ nabij x kwartaal | gemeente^nabij + gemeente^kwartaal, variant ", v))
  for (y in c("aanvragen", "bouw_aanvragen")) {
    m <- veilig(fepois(as.formula(paste(y, "~", termen, "| gemeente^nabij + gemeente^kwartaal")), data = pv, cluster = ~gemeente), paste("did", v, y))
    if (!is.null(m)) did[[paste(v, y, "gk")]] <- coefs(m, paste0("fepois ", y, " ~ nabij x post | gemeente^nabij + gemeente^kwartaal, variant ", v))
    m <- veilig(fepois(as.formula(paste(y, "~", termen, "| gemeente^nabij + kwartaal")), data = pv, cluster = ~gemeente), paste("did k", v, y))
    if (!is.null(m)) did[[paste(v, y, "k")]] <- coefs(m, paste0("fepois ", y, " ~ nabij x post | gemeente^nabij + kwartaal, variant ", v))
  }
  # nieuwbouw van woningen uit de titel, panel uit de zaken
  pn <- zv[soort == "nieuwbouw_woning", .(aanvragen = .N), by = .(gemeente, kwartaal, nabij)]
  pn <- merge(CJ(gemeente = vast, kwartaal = sort(unique(pv$kwartaal)), nabij = 0:1), pn, by = c("gemeente", "kwartaal", "nabij"), all.x = TRUE)
  pn[is.na(aanvragen), aanvragen := 0]
  pn[, kw_idx := kw_orde(kwartaal)]
  zet_post(pn)
  m <- veilig(fepois(as.formula(paste("aanvragen ~", termen, "| gemeente^nabij + gemeente^kwartaal")), data = pn, cluster = ~gemeente), paste("did nieuwbouw", v))
  if (!is.null(m)) did[[paste(v, "nieuwbouw")]] <- coefs(m, paste0("fepois aanvragen nieuwbouw_woning ~ nabij x post | gemeente^nabij + gemeente^kwartaal, variant ", v))
  # trendbreuk bij de uitspraak, met de latere wisselingen als sprongen
  pv[, t := (kw_idx - ev["pas"]) / 4]
  pv[, `:=`(t_voor = nabij * t * as.integer(post_pas == 0), t_na = nabij * t * post_pas, sprong = nabij * post_pas)]
  m <- veilig(fepois(aanvragen ~ t_voor + sprong + t_na + nabij:post_bvr + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + gemeente^kwartaal,
                     data = pv, cluster = ~gemeente), paste("trendbreuk", v))
  if (!is.null(m)) tb[[v]] <- coefs(m, paste0("fepois aanvragen ~ nabij x (trend voor, sprong, trend na) + latere wisselingen | gemeente^nabij + gemeente^kwartaal, variant ", v))
  # uitkomsten per zaak, alleen aanvragen met minstens negen maanden vervolg
  einde <- max(as.IDate(zv$datum_aanvraag))
  z9 <- zv[as.IDate(datum_aanvraag) <= einde - 270]
  z9[, log_dl := ifelse(!is.na(doorlooptijd) & doorlooptijd > 0, log(doorlooptijd), NA_real_)]
  for (y in c("verleend", "geen")) {
    m <- veilig(feols(as.formula(paste(y, "~", termen, "| gemeente^nabij + gemeente^kwartaal")), data = z9, cluster = ~gemeente), paste("uitk", v, y))
    if (!is.null(m)) uk[[paste(v, y)]] <- coefs(m, paste0("feols ", y, " ~ nabij x post | gemeente^nabij + gemeente^kwartaal, variant ", v))
  }
  m <- veilig(feols(as.formula(paste("log_dl ~", termen, "| gemeente^nabij + gemeente^kwartaal")), data = z9[verleend == 1], cluster = ~gemeente), paste("uitk dl", v))
  if (!is.null(m)) uk[[paste(v, "dl")]] <- coefs(m, paste0("feols log(doorlooptijd) ~ nabij x post | gemeente^nabij + gemeente^kwartaal, verleend, variant ", v))
}
# variant L: alle gemeenten in het landelijk dekkende venster (2021K3 tot 2026K2), dezelfde strenge vergelijking binnen
# gemeente en kwartaal, met Porthos, Omgevingswet en intern salderen als wisselingen en 2022K3 als referentie
pl <- panel[kw_idx >= kw_orde("2021K3") & kw_idx <= laatste]
zet_post(pl)
res$meta$L <- list(gemeenten = uniqueN(pl$gemeente), start = "2021K3", aanvragen = sum(pl$aanvragen), nabij = sum(pl[nabij == 1, aanvragen]))
pl[, kwartaal_f := factor(kwartaal, levels = sort(unique(kwartaal)))]
m <- veilig(fepois(aanvragen ~ i(kwartaal_f, nabij, ref = "2022K3") | gemeente^nabij + gemeente^kwartaal_f, data = pl, cluster = ~gemeente), "es L")
if (!is.null(m)) res$vp_event_study_L <- coefs(m, "fepois aanvragen ~ nabij x kwartaal | gemeente^nabij + gemeente^kwartaal, variant L")
termen_l <- "nabij:post_porthos + nabij:post_ow + nabij:post_is"
for (y in c("aanvragen", "bouw_aanvragen")) {
  m <- veilig(fepois(as.formula(paste(y, "~", termen_l, "| gemeente^nabij + gemeente^kwartaal")), data = pl, cluster = ~gemeente), paste("did L", y))
  if (!is.null(m)) did[[paste("L", y, "gk")]] <- coefs(m, paste0("fepois ", y, " ~ nabij x post | gemeente^nabij + gemeente^kwartaal, variant L"))
}
zl <- zaken[kw_idx >= kw_orde("2021K3") & kw_idx <= laatste]
zet_post(zl)
pn <- zl[soort == "nieuwbouw_woning", .(aanvragen = .N), by = .(gemeente, kwartaal, nabij)]
pn <- merge(CJ(gemeente = unique(pl$gemeente), kwartaal = sort(unique(pl$kwartaal)), nabij = 0:1), pn, by = c("gemeente", "kwartaal", "nabij"), all.x = TRUE)
pn[is.na(aanvragen), aanvragen := 0]
pn[, kw_idx := kw_orde(kwartaal)]
zet_post(pn)
m <- veilig(fepois(as.formula(paste("aanvragen ~", termen_l, "| gemeente^nabij + gemeente^kwartaal")), data = pn, cluster = ~gemeente), "did L nieuwbouw")
if (!is.null(m)) did[["L nieuwbouw"]] <- coefs(m, "fepois aanvragen nieuwbouw_woning ~ nabij x post | gemeente^nabij + gemeente^kwartaal, variant L")
einde <- max(as.IDate(zl$datum_aanvraag))
z9 <- zl[as.IDate(datum_aanvraag) <= einde - 270]
z9[, log_dl := ifelse(!is.na(doorlooptijd) & doorlooptijd > 0, log(doorlooptijd), NA_real_)]
for (y in c("verleend", "geen")) {
  m <- veilig(feols(as.formula(paste(y, "~", termen_l, "| gemeente^nabij + gemeente^kwartaal")), data = z9, cluster = ~gemeente), paste("uitk L", y))
  if (!is.null(m)) uk[[paste("L", y)]] <- coefs(m, paste0("feols ", y, " ~ nabij x post | gemeente^nabij + gemeente^kwartaal, variant L"))
}
m <- veilig(feols(as.formula(paste("log_dl ~", termen_l, "| gemeente^nabij + gemeente^kwartaal")), data = z9[verleend == 1], cluster = ~gemeente), "uitk L dl")
if (!is.null(m)) uk[["L dl"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij x post | gemeente^nabij + gemeente^kwartaal, verleend, variant L")
m <- veilig(feols(as.formula(paste("log_dl ~", termen_l, "| gemeente^nabij + gemeente^kwartaal")), data = z9[verleend == 1 & soort == "nieuwbouw_woning"], cluster = ~gemeente), "uitk L dl nieuwbouw")
if (!is.null(m)) uk[["L dl nieuwbouw"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij x post | gemeente^nabij + gemeente^kwartaal, verleend, nieuwbouw_woning, variant L")
m <- veilig(feols(as.formula(paste("log_dl ~", termen_l, "| gemeente^nabij + gemeente^kwartaal")), data = z9[verleend == 1 & soort == "klein_verbouw"], cluster = ~gemeente), "uitk L dl klein")
if (!is.null(m)) uk[["L dl klein"]] <- coefs(m, "feols log(doorlooptijd) ~ nabij x post | gemeente^nabij + gemeente^kwartaal, verleend, klein_verbouw, variant L")
res$vp_beschrijvend <- rbindlist(besch)
res$vp_did <- rbindlist(did)
res$vp_trendbreuk <- rbindlist(tb)
res$vp_uitkomsten <- rbindlist(uk)
for (n in names(res)) if (is.data.frame(res[[n]])) fwrite(res[[n]], file.path(map, paste0(n, ".csv")), sep = ";")
write_json(res, file.path(map, "vastpanel_resultaten.json"), auto_unbox = TRUE, digits = 6, pretty = TRUE, na = "null")
cat("klaar:", paste(names(res), collapse = ", "), "\n")
str(res$meta)
for (v in names(varianten)) {
  e <- res[[paste0("vp_event_study_", v)]]
  if (is.null(e)) next
  e <- e[grepl("::", term)]
  e[, jaar := substr(sub("kwartaal_f::", "", term), 1, 4)]
  cat("== event study", v, "(gemiddeld per jaar, procent)\n")
  print(e[, .(gem = round(100 * mean(schatting), 1), min_p = round(min(p), 3)), by = jaar])
}
for (n in c("vp_did", "vp_trendbreuk", "vp_uitkomsten")) if (nrow(res[[n]])) {
  cat("==", n, "\n")
  for (i in seq_len(nrow(res[[n]]))) with(res[[n]][i], cat(sprintf("  %-95s %-20s %8.2f [%8.2f, %8.2f] p=%.3f\n", substr(model, 1, 95), term,
                                                                   100 * schatting, 100 * (schatting - 1.96 * se), 100 * (schatting + 1.96 * se), p)))
}
