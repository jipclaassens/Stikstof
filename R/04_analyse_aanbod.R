## =====================================================================
## 04_analyse_aanbod.R -- DiD en event study op vergunningen en nieuwbouw
##
## Input : _derived/vbo_analyse.rds
## Output: _output/aanbod_<extract>.txt
##
## Specificatie (gelijk aan de Stata-versie, zodat de uitkomsten
## vergelijkbaar blijven):
##
##   ppmlhdfe y  tr, absorb(provincie#jaar  provincie#afstandsklasse)
##                   vce(cluster provincie#afstandsklasse)
##
## y  = aantal VBO's per provincie x afstandsklasse x jaar
## tr = 1{afstand <= drempel} x 1{jaar >= 2019}
##
## In R: fixest::fepois. Identieke schatter (Poisson pseudo-ML met
## meerdere vaste effecten), zelfde clustering.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
suppressPackageStartupMessages(library(fixest))

sink(file.path(PATHS$output, sprintf("aanbod_%s.txt", EXTRACT)), split = TRUE)

d <- read_derived("vbo_analyse")

## ---------------------------------------------------------------------
## Paneelopbouw
## ---------------------------------------------------------------------
## Twee stappen, net als de `collapse`-regels in Stata:
##   1. per project de MINIMUM afstandsklasse (een project ligt op een
##      plek, ook al varieren de VBO-coordinaten iets)
##   2. tellen per provincie x afstandsklasse x jaar
##
## Kritiek verschil met de Stata-versie: daar is projectid_doc NA voor
## alles zonder vergunningkoppeling, en `collapse ... by(projectid_doc)`
## behandelt NA als een gewone groepswaarde. Alle niet-gekoppelde VBO's
## binnen een gemeente-jaar worden dan een pseudo-project met de kleinste
## afstandsklasse van de hele hoop. In 01_build_vbo.R heeft elk VBO een
## echt project-id, dus dat kan hier niet gebeuren.

bouw_paneel <- function(dt, jaarvar) {
  stopifnot(!any(is.na(dt$projectid)))

  per_project <- dt[, .(kl = min(afstandsklasse), n = .N),
                    by = c("projectid", "provinciecode", jaarvar)]
  cel <- per_project[, .(y = sum(n)), by = c("provinciecode", "kl", jaarvar)]

  ## Lege cellen aanvullen: een provincie x klasse x jaar zonder
  ## waarnemingen is een nul, geen ontbrekende waarneming.
  k_max <- as.integer(PARAMS$klasse_max_m / PARAMS$klasse_breedte) + 1L
  vol <- CJ(provinciecode = sort(unique(cel$provinciecode)),
            kl = 1:k_max,
            J = sort(unique(cel[[jaarvar]])))
  setnames(vol, "J", jaarvar)
  p <- merge(vol, cel, by = c("provinciecode", "kl", jaarvar), all.x = TRUE)
  p[is.na(y), y := 0]

  setnames(p, jaarvar, "jaar")
  for (dr in PARAMS$drempels) {
    p[, paste0("kl", dr) := as.integer(kl <= dr / PARAMS$klasse_breedte)]
  }
  p[, `:=`(na_2019   = as.integer(jaar >= PARAMS$treatment_jaar),
           prov_tijd = paste(provinciecode, jaar),
           prov_kl   = paste(provinciecode, kl))]
  p[]
}

## ---------------------------------------------------------------------
## Schatters
## ---------------------------------------------------------------------

did <- function(p) {
  out <- lapply(PARAMS$drempels, function(dr) {
    p[, tr := get(paste0("kl", dr)) * na_2019]
    m <- fepois(y ~ tr | prov_tijd + prov_kl, data = p,
                cluster = ~prov_kl, notes = FALSE)
    data.table(drempel = paste0(dr, " m"),
               coef = coef(m)[["tr"]], se = se(m)[["tr"]],
               irr = exp(coef(m)[["tr"]]), n = m$nobs)
  })
  r <- rbindlist(out)
  r[, `:=`(coef = round(coef, 4), se = round(se, 4), irr = round(irr, 3),
           ster = fcase(abs(coef / se) > 2.576, "***",
                        abs(coef / se) > 1.960, "**",
                        abs(coef / se) > 1.645, "*",
                        default = ""))]
  r[]
}

event_study <- function(p, drempel) {
  p[, `:=`(D = get(paste0("kl", drempel)),
           rel = jaar - PARAMS$referentie_jaar)]
  m <- fepois(y ~ i(rel, D, ref = 0) | prov_tijd + prov_kl, data = p,
              cluster = ~prov_kl, notes = FALSE)
  cf <- coef(m); s <- se(m)
  jaren <- PARAMS$referentie_jaar +
    as.integer(sapply(strsplit(names(cf), ":"), `[`, 3))
  data.table(jaar = jaren,
             irr = round(exp(cf), 3),
             lo  = round(exp(cf - 1.96 * s), 3),
             hi  = round(exp(cf + 1.96 * s), 3))[order(jaar)]
}

## =====================================================================
## A. VERGUNNINGEN -- telling naar vergunningjaar
## =====================================================================

kop("A. VERGUNNINGEN")

verg <- d[in_sample_verg == TRUE]
cat(sprintf("VBO's in de ex-ante steekproef: %s\n", nfmt(nrow(verg))))
cat(sprintf("vergunningjaren %d t/m %d\n\n", PARAMS$jaar_min, PARAMS$jaar_max))

pv <- bouw_paneel(verg, "vergunning_jaar")
cat("Dif-in-dif:\n"); print(did(pv))

cat("\nEvent study, 1000 m (IRR t.o.v. ", PARAMS$referentie_jaar, "):\n", sep = "")
print(event_study(pv, 1000))
cat("\nEvent study, 2500 m:\n")
print(event_study(pv, 2500))

## Gevoeligheid: reeks afkappen voor de Omgevingswet/Wkb-breuk
cat("\nGevoeligheid -- alleen vergunningjaren t/m ",
    PARAMS$wkb_breuk_jaar - 1, ":\n", sep = "")
print(did(bouw_paneel(verg[vergunning_jaar < PARAMS$wkb_breuk_jaar],
                      "vergunning_jaar")))

## =====================================================================
## B. NIEUWBOUW -- telling naar mutatiejaar (CBS)
## =====================================================================

kop("B. NIEUWBOUW")

nwb <- d[in_sample_nwb == TRUE]
cat(sprintf("VBO's in de nieuwbouwsteekproef: %s\n", nfmt(nrow(nwb))))
cat(sprintf("mutatiejaren %d t/m %d\n\n", PARAMS$jaar_min, PARAMS$jaar_max))

pn <- bouw_paneel(nwb, "mutatiejaar")
cat("Dif-in-dif:\n"); print(did(pn))

cat("\nEvent study, 1000 m:\n"); print(event_study(pn, 1000))
cat("\nEvent study, 2500 m:\n"); print(event_study(pn, 2500))

## =====================================================================
## C. Gevoeligheid voor de filterkeuze
## =====================================================================
## Laat zien dat de keuze tussen statusfilters weinig uitmaakt, en dat
## de projectsleutel wel veel uitmaakt. Wie een filter wil veranderen,
## kan hier zien of het iets doet.

kop("C. GEVOELIGHEID")

varianten <- list(
  "ex ante (in_sample)"          = quote(in_sample_verg == TRUE),
  "  + verbouwingen erbij"       = quote(in_sample_verg == TRUE |
                                          (projecttype == "verbouwing" &
                                           vbo_gebruiksdoel == "woon" &
                                           !is.na(provinciecode) &
                                           !is.na(dist2n2000_m) &
                                           vergunning_jaar >= PARAMS$jaar_min &
                                           vergunning_jaar <= PARAMS$jaar_max)),
  "  alleen gerealiseerd"        = quote(in_sample_verg == TRUE &
                                          uitkomst %chin% c("gerealiseerd_nieuwbouw",
                                                            "gerealiseerd_overig")),
  "  Stata-equivalent (2,4,7)"   = quote(in_sample_verg == TRUE &
                                          uitkomst %chin% c("uitval",
                                                            "gerealiseerd_nieuwbouw",
                                                            "pipeline"))
)

cat("Vergunningen, coefficient op tr per variant:\n\n")
for (nm in names(varianten)) {
  sub <- d[eval(varianten[[nm]])]
  r <- did(bouw_paneel(sub, "vergunning_jaar"))
  cat(sprintf("%-28s N=%9s   %s\n", nm, nfmt(nrow(sub)),
              paste(sprintf("%s: %+.3f%s", r$drempel, r$coef, r$ster),
                    collapse = "  ")))
}

cat("\n\nEffect van de projectsleutel (nieuwbouw, 250 m en 1000 m):\n")
cat("Reproductie van de Stata-situatie waarin niet-gekoppelde VBO's\n")
cat("op een gedeelde NA-sleutel worden gepoold:\n\n")

nwb_pool <- copy(nwb)
nwb_pool[bron_vergunning == FALSE,
         projectid := paste("__NA__", gemeentecode, mutatiejaar)]
r_goed <- did(bouw_paneel(nwb,      "mutatiejaar"))
r_pool <- did(bouw_paneel(nwb_pool, "mutatiejaar"))
print(merge(r_goed[, .(drempel, eigen_sleutel = coef)],
            r_pool[, .(drempel, NA_gepoold = coef)], by = "drempel", sort = FALSE))

sink()
