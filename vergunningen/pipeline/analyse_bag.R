# BAG-analyse: van vergunning naar bouwstart en ingebruikname per nieuw pand, landelijk sinds 2012.
#
# Gebruik: Rscript analyse_bag.R <analysemap>        (de map met bag_panden.csv uit bag_export.py)
# Schrijft in dezelfde map: bag_resultaten.json en de tabellen als bag_*.csv.
#
# De BAG kent per pand de eerste datum van Bouwvergunning verleend, Bouw gestart, Pand in gebruik en Niet gerealiseerd,
# voor alle gemeenten vanaf de start van de registratie. Dat maakt drie dingen mogelijk die de bekendmakingen niet kunnen:
# de PAS-uitspraak (29 mei 2019) in het venster, een uitkomst na de vergunning (komt de bouw op gang, wordt het pand
# opgeleverd, of wordt de vergunning nooit benut) en een landelijk panel zonder de publicatieselectie van voor 2021.
# Dezelfde modelfamilie als analyse.R: fepois voor aantallen, feols voor aandelen en log-dagen, vaste effecten voor
# gemeente maal nabij en voor kwartaal, standaardfouten geclusterd op gemeente; nabij is binnen 2.500 meter van de
# AERIUS relevante habitatkartering. Cohorten zijn kwartalen van vergunningverlening; een uitkomst telt alleen voor
# cohorten met genoeg vervolg (een jaar voor bouwstart, twee jaar voor ingebruikname, drie jaar voor niet gerealiseerd).

suppressMessages({ library(data.table); library(fixest); library(jsonlite) })
args <- commandArgs(trailingOnly = TRUE)
map <- if (length(args)) args[1] else "C:/LocalData/vergunningen/analyse"
stand <- as.IDate("2026-09-08")  # stand van het LV BAG-extract
p <- fread(file.path(map, "bag_panden.csv"), encoding = "UTF-8",
           colClasses = list(character = c("identificatie", "gemeentecode", "bouwjaar", "huidige_status", "n2000_klasse")))
res <- list(meta = list(datum = format(Sys.time(), "%Y-%m-%d %H:%M"), stand_extract = as.character(stand), panden_gelezen = nrow(p)))
setnames(p, c("x", "y"), c("x_rd", "y_rd"))  # anders vangt get(y) in een data.table-expressie de coordinaatkolom
p[, gemeente := gemeentecode]  # dezelfde naam als in analyse.R, zodat de formules gelijk zijn

p[, verg := as.IDate(verg_begin)]
p <- p[!is.na(verg) & verg >= as.IDate("2012-01-01") & verg <= stand & gemeentecode != "" & !is.na(dist2n2000_m)]
p[, `:=`(start = as.IDate(start_begin), gebruik = as.IDate(gebruik_begin), nietger = as.IDate(niet_gerealiseerd_begin))]
# bouw op gang: de eerste van Bouw gestart en Pand in gebruik, want een op de dertig panden slaat de startmelding over
p[, bouw := fifelse(is.na(start), gebruik, fifelse(is.na(gebruik), start, pmin(start, gebruik)))]
p[, `:=`(jaar = year(verg), kwartaal = sprintf("%dK%d", year(verg), quarter(verg)))]
p[, nabij := as.integer(dist2n2000_m <= 2500)]
p[, woon := as.integer(n_woon >= 1)]
p[, grootte := fifelse(n_woon == 0, "geen woning", fifelse(n_woon <= 2, "1 tot 2 woningen", fifelse(n_woon <= 9, "3 tot 9 woningen", "10 of meer woningen")))]
p[, dagen_bouw := as.integer(bouw - verg)]
p[, dagen_gebruik := as.integer(gebruik - verg)]
p[, dagen_nietger := as.integer(nietger - verg)]
p[, gestart_365 := fifelse(verg <= stand - 365, as.integer(!is.na(dagen_bouw) & dagen_bouw <= 365), NA_integer_)]
p[, gebruik_730 := fifelse(verg <= stand - 730, as.integer(!is.na(dagen_gebruik) & dagen_gebruik <= 730), NA_integer_)]
p[, nietger_1095 := fifelse(verg <= stand - 1095, as.integer(!is.na(dagen_nietger) & dagen_nietger <= 1095), NA_integer_)]
p[, log_dagen_bouw := fifelse(!is.na(dagen_bouw) & dagen_bouw > 0, log(pmax(dagen_bouw, 1L)), NA_real_)]
laatste_volledig <- "2026K2"  # 2026K3 is bij de stand van 8 september niet af
p <- p[kwartaal <= laatste_volledig]
res$meta$panden <- nrow(p)
res$meta$woonpanden <- sum(p$woon)
res$meta$woningen <- sum(p$n_woon)

kw_orde <- function(k) { j <- as.integer(substr(k, 1, 4)); q <- as.integer(substr(k, 6, 6)); j * 4 + q - 1 }
p[, kw_idx := kw_orde(kwartaal)]
# eerste volledig behandelde kwartaal per gebeurtenis: PAS-uitspraak 29 mei 2019, Porthos 2 november 2022, Omgevingswet 1 januari 2024,
# intern salderen 18 december 2024
ev <- list(pas = kw_orde("2019K3"), porthos = kw_orde("2023K1"), omgevingswet = kw_orde("2024K1"), intern_salderen = kw_orde("2025K1"))
zet_post <- function(d) d[, `:=`(post_pas = as.integer(kw_idx >= ev$pas), post_porthos = as.integer(kw_idx >= ev$porthos),
                                 post_ow = as.integer(kw_idx >= ev$omgevingswet), post_is = as.integer(kw_idx >= ev$intern_salderen))]
zet_post(p)

coefs <- function(m, naam) {
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "schatting", "se", "t", "p"))
  ct[, `:=`(model = naam, n = nobs(m))]
  ct[]
}
veilig <- function(expr, naam) tryCatch(expr, error = function(e) { message(naam, ": ", conditionMessage(e)); NULL })

# 1. beschrijvend: per jaar en nabij, en per kwartaal en nabij (voor de figuur)
res$bag_beschrijvend_jaar_nabij <- p[, .(panden = .N, woonpanden = sum(woon), woningen = sum(n_woon),
                                         gestart_365 = mean(gestart_365, na.rm = TRUE), gebruik_730 = mean(gebruik_730, na.rm = TRUE),
                                         nietger_1095 = mean(nietger_1095, na.rm = TRUE),
                                         mediaan_dagen_bouw = as.numeric(median(dagen_bouw[dagen_bouw >= 0], na.rm = TRUE)),
                                         mediaan_dagen_gebruik = as.numeric(median(dagen_gebruik[dagen_gebruik >= 0], na.rm = TRUE))),
                                     by = .(jaar, nabij)][order(jaar, nabij)]
res$bag_beschrijvend_kwartaal_nabij <- p[, .(panden = .N, woonpanden = sum(woon), woningen = sum(n_woon)), by = .(kwartaal, nabij)][order(kwartaal, nabij)]
res$bag_beschrijvend_klasse <- p[, .(panden = .N, woningen = sum(n_woon), gestart_365 = mean(gestart_365, na.rm = TRUE),
                                     gebruik_730 = mean(gebruik_730, na.rm = TRUE), nietger_1095 = mean(nietger_1095, na.rm = TRUE),
                                     mediaan_dagen_bouw = as.numeric(median(dagen_bouw[dagen_bouw >= 0], na.rm = TRUE))),
                                 by = .(klasse = n2000_klasse)][order(klasse)]
res$bag_beschrijvend_grootte <- p[, .(panden = .N, woningen = sum(n_woon), nabij = mean(nabij), gestart_365 = mean(gestart_365, na.rm = TRUE),
                                      nietger_1095 = mean(nietger_1095, na.rm = TRUE),
                                      mediaan_dagen_bouw = as.numeric(median(dagen_bouw[dagen_bouw >= 0], na.rm = TRUE))),
                                  by = grootte][order(-panden)]

# 2. panel van vergunde panden per gemeente, kwartaal en nabij; event study met de PAS-uitspraak als referentie (2019K1 is het
#    laatste volledige kwartaal ervoor) en gestapelde difference in differences met de vier regimewisselingen
pan <- p[, .(panden = .N, woonpanden = sum(woon), woningen = sum(n_woon)), by = .(gemeente = gemeentecode, kwartaal, nabij)]
vol <- CJ(gemeente = unique(pan$gemeente), kwartaal = sort(unique(p$kwartaal)), nabij = 0:1)
pan <- merge(vol, pan, by = c("gemeente", "kwartaal", "nabij"), all.x = TRUE)
for (v in c("panden", "woonpanden", "woningen")) pan[is.na(get(v)), (v) := 0]
pan[, kw_idx := kw_orde(kwartaal)]
zet_post(pan)
pan[, kwartaal_f := factor(kwartaal, levels = sort(unique(kwartaal)))]
ref_kw <- "2019K1"
m <- veilig(fepois(panden ~ i(kwartaal_f, nabij, ref = ref_kw) | gemeente^nabij + kwartaal_f, data = pan, cluster = ~gemeente), "event study panden")
if (!is.null(m)) res$bag_event_study_panden <- coefs(m, "fepois vergunde panden ~ nabij x kwartaal | gemeente^nabij + kwartaal")
m <- veilig(fepois(woonpanden ~ i(kwartaal_f, nabij, ref = ref_kw) | gemeente^nabij + kwartaal_f, data = pan, cluster = ~gemeente), "event study woonpanden")
if (!is.null(m)) res$bag_event_study_woonpanden <- coefs(m, "fepois vergunde woonpanden ~ nabij x kwartaal | gemeente^nabij + kwartaal")
m <- veilig(fepois(woningen ~ i(kwartaal_f, nabij, ref = ref_kw) | gemeente^nabij + kwartaal_f, data = pan, cluster = ~gemeente), "event study woningen")
if (!is.null(m)) res$bag_event_study_woningen <- coefs(m, "fepois vergunde woningen ~ nabij x kwartaal | gemeente^nabij + kwartaal")
did <- list()
for (y in c("panden", "woonpanden", "woningen")) {
  m <- veilig(fepois(as.formula(paste(y, "~ nabij:post_pas + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal")),
                     data = pan, cluster = ~gemeente), paste("did", y))
  if (!is.null(m)) did[[y]] <- coefs(m, paste("fepois", y, "~ nabij x post | gemeente^nabij + kwartaal"))
  # alleen de PAS-uitspraak, in een venster van drie jaar ervoor tot drie jaar erna (2016K3 tot 2022K2, voor Porthos)
  m <- veilig(fepois(as.formula(paste(y, "~ nabij:post_pas | gemeente^nabij + kwartaal")),
                     data = pan[kw_idx >= kw_orde("2016K3") & kw_idx <= kw_orde("2022K2")], cluster = ~gemeente), paste("did pas", y))
  if (!is.null(m)) did[[paste(y, "pas")]] <- coefs(m, paste("fepois", y, "~ nabij x post_pas | gemeente^nabij + kwartaal, 2016K3 tot 2022K2"))
}
res$bag_did_aantallen <- rbindlist(did)

# 3. uitkomsten na de vergunning op pandniveau: komt de bouw binnen een jaar op gang, is het pand binnen twee jaar in gebruik,
#    wordt de vergunning binnen drie jaar afgeboekt als niet gerealiseerd, en de log-dagen tot bouwstart
uitk <- list()
for (y in c("gestart_365", "gebruik_730", "nietger_1095")) {
  d <- p[!is.na(get(y))]
  m <- veilig(feols(as.formula(paste(y, "~ nabij:post_pas + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal")),
                    data = d, cluster = ~gemeentecode), y)
  if (!is.null(m)) uitk[[y]] <- coefs(m, paste("feols", y, "~ nabij x post | gemeente^nabij + kwartaal"))
  m <- veilig(feols(as.formula(paste(y, "~ nabij:post_pas + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal")),
                    data = d[woon == 1], cluster = ~gemeentecode), paste(y, "woon"))
  if (!is.null(m)) uitk[[paste(y, "woon")]] <- coefs(m, paste("feols", y, "~ nabij x post | gemeente^nabij + kwartaal, woonpanden"))
}
d <- p[!is.na(log_dagen_bouw) & verg <= stand - 365]
m <- veilig(feols(log_dagen_bouw ~ nabij:post_pas + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = d, cluster = ~gemeentecode), "log dagen bouw")
if (!is.null(m)) uitk[["log_dagen_bouw"]] <- coefs(m, "feols log(dagen tot bouwstart) ~ nabij x post | gemeente^nabij + kwartaal, gestart")
m <- veilig(feols(log_dagen_bouw ~ nabij:post_pas + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = d[woon == 1], cluster = ~gemeentecode), "log dagen bouw woon")
if (!is.null(m)) uitk[["log_dagen_bouw woon"]] <- coefs(m, "feols log(dagen tot bouwstart) ~ nabij x post | gemeente^nabij + kwartaal, gestart, woonpanden")
res$bag_did_uitkomsten <- rbindlist(uitk)

# 4. naar projectgrootte: stikstof raakt grote projecten, een enkele woning zelden; woonpanden per grootteklasse
gr <- list()
for (g in c("1 tot 2 woningen", "3 tot 9 woningen", "10 of meer woningen")) {
  d <- p[grootte == g & !is.na(gestart_365)]
  m <- veilig(feols(gestart_365 ~ nabij:post_pas + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = d, cluster = ~gemeentecode), paste("grootte", g))
  if (!is.null(m)) gr[[paste(g, "gestart")]] <- coefs(m, paste("feols gestart_365 ~ nabij x post | gemeente^nabij + kwartaal,", g))
  d <- p[grootte == g & !is.na(log_dagen_bouw) & verg <= stand - 365]
  m <- veilig(feols(log_dagen_bouw ~ nabij:post_pas + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = d, cluster = ~gemeentecode), paste("grootte dagen", g))
  if (!is.null(m)) gr[[paste(g, "dagen")]] <- coefs(m, paste("feols log(dagen tot bouwstart) ~ nabij x post | gemeente^nabij + kwartaal, gestart,", g))
  pg <- p[grootte == g, .(panden = .N), by = .(gemeente = gemeentecode, kwartaal, nabij)]
  pg <- merge(vol, pg, by = c("gemeente", "kwartaal", "nabij"), all.x = TRUE)
  pg[is.na(panden), panden := 0]
  pg[, kw_idx := kw_orde(kwartaal)]
  zet_post(pg)
  m <- veilig(fepois(panden ~ nabij:post_pas + nabij:post_porthos + nabij:post_ow + nabij:post_is | gemeente^nabij + kwartaal, data = pg, cluster = ~gemeente), paste("grootte aantal", g))
  if (!is.null(m)) gr[[paste(g, "aantal")]] <- coefs(m, paste("fepois vergunde panden ~ nabij x post | gemeente^nabij + kwartaal,", g))
}
res$bag_did_grootte <- rbindlist(gr)

# 5. afstandsgradient over de hele periode met gemeente- en kwartaaleffecten (beschrijvend)
p[, klasse_f := factor(n2000_klasse, levels = c("5000_", "2500_5000", "1000_2500", "500_1000", "250_500", "0_250"))]
grad <- list()
for (y in c("gestart_365", "gebruik_730", "nietger_1095")) {
  m <- veilig(feols(as.formula(paste(y, "~ i(klasse_f, ref = '5000_') | gemeentecode + kwartaal")), data = p[!is.na(get(y))], cluster = ~gemeentecode), paste("gradient", y))
  if (!is.null(m)) grad[[y]] <- coefs(m, paste("feols", y, "~ klasse | gemeente + kwartaal"))
}
m <- veilig(feols(log_dagen_bouw ~ i(klasse_f, ref = "5000_") | gemeentecode + kwartaal, data = p[!is.na(log_dagen_bouw) & verg <= stand - 365], cluster = ~gemeentecode), "gradient dagen")
if (!is.null(m)) grad[["log_dagen_bouw"]] <- coefs(m, "feols log(dagen tot bouwstart) ~ klasse | gemeente + kwartaal, gestart")
res$bag_gradient_klasse <- rbindlist(grad)

# 6. de gradient na de PAS-uitspraak tegenover ervoor: verschil per afstandsklasse met 2016K3 tot 2019K2 als voor en 2019K3 tot 2022K2 als na
pv <- p[kw_idx >= kw_orde("2016K3") & kw_idx <= kw_orde("2022K2")]
gp <- list()
for (y in c("gestart_365", "nietger_1095")) {
  m <- veilig(feols(as.formula(paste(y, "~ i(klasse_f, post_pas, ref = '5000_') | gemeentecode^klasse_f + kwartaal")), data = pv[!is.na(get(y))], cluster = ~gemeentecode), paste("gradient pas", y))
  if (!is.null(m)) gp[[y]] <- coefs(m, paste("feols", y, "~ klasse x post_pas | gemeente^klasse + kwartaal, 2016K3 tot 2022K2"))
}
m <- veilig(feols(log_dagen_bouw ~ i(klasse_f, post_pas, ref = "5000_") | gemeentecode^klasse_f + kwartaal, data = pv[!is.na(log_dagen_bouw)], cluster = ~gemeentecode), "gradient pas dagen")
if (!is.null(m)) gp[["log_dagen_bouw"]] <- coefs(m, "feols log(dagen tot bouwstart) ~ klasse x post_pas | gemeente^klasse + kwartaal, gestart, 2016K3 tot 2022K2")
res$bag_gradient_pas <- rbindlist(gp)

for (n in names(res)) if (is.data.frame(res[[n]])) fwrite(res[[n]], file.path(map, paste0(n, ".csv")), sep = ";")
write_json(res, file.path(map, "bag_resultaten.json"), auto_unbox = TRUE, digits = 6, pretty = TRUE, na = "null")
cat("klaar:", paste(names(res), collapse = ", "), "\n")
for (n in c("bag_did_aantallen", "bag_did_uitkomsten", "bag_did_grootte")) if (!is.null(res[[n]])) {
  cat("==", n, "\n")
  for (i in seq_len(nrow(res[[n]]))) with(res[[n]][i], cat(sprintf("  %-95s %-28s %8.2f [%8.2f, %8.2f] p=%.3f\n", substr(model, 1, 95), term,
                                                                     100 * schatting, 100 * (schatting - 1.96 * se), 100 * (schatting + 1.96 * se), p)))
}
