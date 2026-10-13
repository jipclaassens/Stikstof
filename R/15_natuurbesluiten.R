## =====================================================================
## 15_natuurbesluiten.R -- provinciale natuurbesluiten over woningnieuwbouw,
##                         en welke grote BAG-projecten er een hadden
##
## Input : _derived/vbo_analyse.rds; natuur/natuurvergunningen.sqlite en
##         vergunningen.sqlite in de datamap van het vergunningenspoor
## Output: _output/natuurbesluiten_<extract>.json (door 15a_natuurbesluiten.py)
##         _derived/projecten_20plus.csv (de projecten die gekoppeld worden)
##
## Waarom
## ---------------------------------------------------------------------
## Het vergunningenspoor mat natuurbesluiten als 'een stikstofbesluit binnen
## 500 meter van een project'. Bij nalopen haalt die maat niet meer treffers
## dan toeval (met alle besluitpunten 1 km verschoven evenveel). Dit script
## beperkt zich tot besluiten die volgens hun tekst over woningnieuwbouw gaan
## en een precieze locatie hebben, en koppelt die aan de grote projecten van
## dit rapport, met hetzelfde kansniveau ernaast. Zie de kop van
## 15a_natuurbesluiten.py voor de indeling.
## =====================================================================

.here <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
})
source(file.path(.here, "00_config.R"))
options(width = 200)

d <- read_derived("vbo_analyse")
vpp <- d[, .(n_pand = uniqueN(vbo_bag_nr)), by = pand_bag_nr]
d <- merge(d, vpp, by = "pand_bag_nr", all.x = TRUE)
verg <- d[in_sample == TRUE & !is.na(vergunning_datum) & vergunning_datum >= as.Date("2016-01-01") &
          vergunning_datum <= as.Date("2026-03-31") & !is.na(x) & !is.na(dist2n2000_m)]
proj <- verg[, .(objecten = .N, x = round(mean(x)), y = round(mean(y)), vergund = min(vergunning_datum),
                 afstand = min(dist2n2000_m), meergezins = mean(n_pand >= 3L)), by = projectid][objecten >= 20L]
proj[, `:=`(jaar = as.integer(format(vergund, "%Y")),
            ring = fifelse(afstand <= 1000, "0 tot 1 km", fifelse(afstand <= 2500, "1 tot 2,5 km",
                   fifelse(afstand <= 5000, "2,5 tot 5 km", "verder dan 5 km"))),
            type = fifelse(meergezins >= 0.5, "meergezins", "eengezins"))]
f_proj <- file.path(PATHS$derived, "projecten_20plus.csv")
fwrite(proj[, .(projectid, objecten, x, y, vergund = format(vergund), jaar, ring, type)], f_proj, sep = ";")
cat("projecten van 20 of meer objecten, vergund 2016 tot en met 2026Q1:", nrow(proj), "\n")

f_uit <- file.path(PATHS$output, sprintf("natuurbesluiten_%s.json", EXTRACT))
py <- Sys.getenv("PYTHON", "py")
status <- system2(py, c(if (py == "py") "-3.13", shQuote(file.path(.here, "15a_natuurbesluiten.py")),
                        shQuote(f_proj), shQuote(f_uit), shQuote(PATHS$verg)))
if (status != 0 || !file.exists(f_uit)) stop("15a_natuurbesluiten.py faalde")
cat("geschreven:", f_uit, "\n")
