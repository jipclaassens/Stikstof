## =====================================================================
## 27_regels.R -- de regels voor nieuwbouw, sloop en vervanging uit de
##                titels van een gemeentelijke zaak, op een plek, voor
##                27_pijplijn_sites.R en 27b_econlogic_export.R
##
## zaak_vlaggen(zk) voegt aan een data.table met de kolommen soort (uit
## 09a_titels.py, NA voor zaken zonder aanvraag), titels en t_slopen de
## logische kolommen nieuwbouw, sloop_woning, nieuwbouw_echt en vervanging
## toe (by reference).
##
## Nieuwbouw van woningen: de soort uit 09a_titels.py (analyse_export.
## projectsoort over alle titels van de zaak, met de woordgrens van 28
## september 2026); voor zaken zonder aanvraag, die daar niet in staan,
## dezelfde regel op de bewaarde titels. Daarnaast nieuwbouw_echt: zonder
## realiseren (vaak in een bestaand gebouw), zonder verbouw, splitsing of
## transformatie, en zonder mantelzorgwoning of tijdelijke woning.
## Vervanging: slopen en nieuwbouw_echt in de titels van dezelfde zaak, of
## vervangende nieuwbouw of herbouw. Sloop van een woning: een strengere
## regel dan alleen het woord slopen, want slopen van een schuur of aanbouw
## bij een woning telt niet.
## =====================================================================

RX_NIEUWBOUW <- paste0("\\b(?:her|nieuw|terug|af|wederop)?(?:bouwen|oprichten|realiseren|nieuwbouw) (van )?(een |[0-9]+ |twee |drie |vier |vijf |zes |acht |tien )?",
                       "(nieuwe |vrijstaande |levensloopbestendige |sociale |tijdelijke )?(woning|woningen|woonhuis|appartement|appartementen|",
                       "studentenwoning|wooneenhe|woonzorg|woongebouw|woontoren|woonblok|villa|bungalow|twee-onder-een-kap|rijwoning|",
                       "eengezinswoning|meergezins|flexwoning|mantelzorgwoning)|nieuwbouwwoning|woningbouw|nieuwbouw van|nieuwbouwproject|",
                       "[0-9]+ (woningen|appartementen|studentenwoningen|wooneenheden|studio's)")
RX_SLOOPWONING <- paste0("(slopen|sloop) (van )?(een |de |het |twee |drie |[0-9]+ )?(bestaande |voormalige |vrijstaande |oude )?",
                         "(woning|woningen|woonhuis|woonhuizen|appartement|appartementen|flat|flats|bedrijfswoning|boerderij|",
                         "woonboerderij|woongebouw|woonblok|portiekflat|galerijflat|rijtjeswoning|eengezinswoning|bungalow|villa)")
RX_ECHT <- paste0("(^|[^a-z])(her|nieuw|terug|af|wederop)?(bouwen|oprichten|nieuwbouw) (van )?(een |de |het |[0-9]+ |twee |drie |vier |vijf |zes |acht |tien )?",
                  "(nieuwe |vrijstaande |levensloopbestendige |sociale |grondgebonden )?(woning|woningen|woonhuis|appartement|appartementen|",
                  "woongebouw|woontoren|woonblok|villa|bungalow|twee-onder-een-kap|rijwoning|rijwoningen|eengezinswoning|meergezins)|",
                  "nieuwbouwwoning|nieuwbouw van|vervangende nieuwbouw|herbouw")
RX_NIET <- "verbouw|splits|transformatie|omzetten|mantelzorg|tijdelijk|recreatiewoning|vakantiewoning|chalet|woonwagen|woonark|woonboot"

zaak_vlaggen <- function(zk) {
  zk[, nieuwbouw := fifelse(!is.na(soort), soort == "nieuwbouw_woning", grepl(RX_NIEUWBOUW, titels, ignore.case = TRUE, perl = TRUE))]
  zk[, sloop_woning := grepl(RX_SLOOPWONING, titels, ignore.case = TRUE)]
  zk[, nieuwbouw_echt := grepl(RX_ECHT, titels, ignore.case = TRUE) & !grepl(RX_NIET, titels, ignore.case = TRUE)]
  zk[, vervanging := !grepl(RX_NIET, titels, ignore.case = TRUE) &
       ((t_slopen == 1L & nieuwbouw_echt) | grepl("vervangende nieuwbouw|herbouw", titels, ignore.case = TRUE))]
  invisible(zk)
}
