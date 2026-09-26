"""Fase van een publicatie uit de titel.

De SRU kent geen faseveld. De titel is het eerste signaal (in de meting van 25 september 2026 kende een regelset aan
84 tot 92 procent van de titels een klasse toe, per gemeente 4 tot 100 procent); de frasen in het tekstlichaam zijn het
tweede signaal en komen later, per gemeente. Elke gemeente kiest haar eigen bewoordingen en wisselt die soms, dus de
vangst van deze regels moet per gemeente en jaargang gemeten blijven (tabel publicatie, kolom fase_titel).

Twee stappen. Eerst neutraliseert NEUTRALISEER woorden die een fase noemen zonder dat de publicatie die fase heeft:
"besluit op aanvraag" is een besluit en geen aanvraag, en "wijzigen van de verleende vergunning" is een aanvraag en geen
verlening. Daarna wint de eerste regel uit REGELS die de genormaliseerde titel raakt; de volgorde is de ontwerpkeuze.
Een besluit zonder uitkomst in de titel heet besluit en niet verleend: een klasse te weinig is beter dan een verkeerde,
want wat onbepaald of besluit blijft komt later uit de tekstfrasen, een fout gelabeld verleend-record niet.
"""
import re

KLASSEN = (
    "aanvraag",        # aanvraag ontvangen, ingediend of ingekomen
    "verleend",        # besluit met uitkomst verleend, ook gedeeltelijk, van rechtswege, toestemming, afgegeven
    "geweigerd",       # besluit met uitkomst geweigerd of afgewezen
    "ingetrokken",     # aanvraag of vergunning ingetrokken, buiten behandeling gesteld, niet-ontvankelijk, vervallen
    "verlenging",      # beslistermijn verlengd, verdaagd of opgeschort
    "ontwerp",         # ontwerpbesluit, voornemen, zienswijzen
    "vergunningvrij",  # oordeel dat geen vergunning nodig is
    "melding",         # sloop-, bouw- of gebruiksmelding, geen vergunning
    "rectificatie",    # rectificatie of herpublicatie van een eerdere kennisgeving
    "bezwaar",         # beslissing op bezwaar of beroep tegen een eerder besluit
    "besluit",         # besluit of beschikking zonder uitkomst in de titel
    "onbepaald",       # geen regel raakt de titel
)

# Vervangingen voor het matchen, in volgorde. Groep 1 blijft staan, het fasewoord verdwijnt.
NEUTRALISEER: list[tuple[str, str]] = [
    # "besluit op (de) aanvraag", "beschikking op aanvraag", "besluit aanvraag omgevingsvergunning": een besluit
    (r"\b(besluit|beschikking|beslissing|beslist|beslissen)( op| over| omtrent| inzake| naar aanleiding van)?"
     r"( de| een| uw| het)? (aanvraag|aanvragen|verzoek)\b", r"\1"),
    # "wijzigen van de (eerder) verleende vergunning": de verleende vergunning is het onderwerp, niet de fase
    (r"\b(wijzig\w*|aanpass\w*|verander\w*|afwijk\w*|uitbreid\w*|vervang\w*|aanvull\w*|herzien\w*|actualis\w*|"
     r"omzett\w*|overschrijv\w*|tenaamstell\w*|overdra\w*|intrekk\w*|verleng\w*)"
     r"( van| op| ten opzichte van)?( de| een| het| deze| die| onze| uw)?( eerder| reeds| al| eerdere)? verleende\b",
     r"\1 eerdere"),
    (r"\b(eerder|reeds|al) verleende\b", r"eerdere"),
    # straatnamen als "Verlengde Hoofdstraat" zijn geen verlenging van een beslistermijn
    (r"\bverlengde (\w+(straat|weg|laan|dijk|kade|singel|pad|vaart|gracht|steeg|dreef|hof|plein))\b", r"\1"),
]

# Geordende regels op de genormaliseerde titel; de eerste die raakt wint.
REGELS: list[tuple[str, str]] = [
    ("rectificatie", r"\brectificatie\b|\bgerectificeerd\b|\bherpublicatie\b|\bherplaatsing\b|\bopnieuw gepubliceerd\b"),
    # ontwerp voor de uitkomsten: "ontwerpbesluit ... te weigeren" en "ontwerpbesluit tot intrekking" zijn nog ontwerpen
    ("ontwerp", r"\bontwerp[- ]?(besluit|beschikking|omgevingsvergunning|vergunning)|\bontwerp\b(?! van)|"
                r"\bvoornemen\b|\bvoorgenomen\b|\bzienswijze\w*|\bconcept(besluit|beschikking)\b"),
    ("bezwaar", r"\b(besluit|beslissing|beschikking|uitspraak) op (het |een )?bezwaar\b|\bbezwaarschrift\w*|"
                r"\bop bezwaar\b|\bin beroep\b|\bberoepschrift\w*"),
    ("ingetrokken", r"\bingetrokken\b|\bintrekk\w*|\bintrekt\b|\bbuiten behandeling\b|\bniet in behandeling\b|"
                    r"\bniet[- ]ontvankelijk\b|\bvervallen verklaard\b|\b(vergunning|aanvraag) (is )?vervallen\b|"
                    r"\bgeannuleerd\b"),
    ("verlenging", r"\w*verleng\w*|\bopschort\w*|\bopgeschort\b|\bbeslistermijn\b|\bverdaag\w*|\bverdaging\b"),
    ("geweigerd", r"\bgeweigerd\w*|\bweiger(ing|en|t)?\b|\bafgewezen\b|\bafwijz\w*|\bniet verle(end|nen)\b|"
                  r"\bniet ingewilligd\b|\bnegatie(f|ve)\b"),
    # "vergunningvrije aanbouw" in een aanvraagtitel is een bijvoeglijk naamwoord, geen oordeel; vandaar de smalle vormen
    ("vergunningvrij", r"\bvergunning(s)?vrij\b|\bvergunning(s)?vrije (activiteit|aanvraag|bouw|werkzaamheden)\b|"
                       r"\bgeen (omgevings)?vergunning (nodig|vereist)\b|\bniet vergunning(s)?plichtig\b|"
                       r"\bniet vergunning-? ?of ontheffing(s)?plichtig\b|\bvergunningplichtvrij\b"),
    ("melding", r"^(?!.*vergunning).*\b(\w*melding|gemeld)\b"),
    ("verleend", r"\bverle(end\w*|nen|ent|ning)\b|\btoegekend\b|\btoestemming\b|\bvergund\b|\bafgegeven\b|"
                 r"\bingewilligd\b|\bgehonoreerd\b|\bgoedgekeurd\b"),
    ("aanvraag", r"aanvra(ag|gen)|\baanvr\.|\baangevraagd\w*|\bingediend\w*\b|\bingekomen\b|\bontvangen\b|\bontvangst\b|"
                 r"\bverzoek\b"),
    # "Vergunning voor het bouwen van ..." zonder fasewoord is bij de gemeenten die zo titelen een besluit, met de
    # uitkomst in de tekst; een kale titel "Omgevingsvergunning, adres" blijft onbepaald
    ("besluit", r"\b(wijzigings|herstel|maatwerk|gedoog)?besluit\b|\bbeschikking\b|\bbeslissing\b|\bbeslist\b|"
                r"\bbekendgemaakt\w*|\bafgehandeld\w*|^(?:[\w'.-]+ ){0,4}(omgevings)?vergunning (wateractiviteit )?voor (het|de|een)\b"),
]

_NEUTRALISEER = [(re.compile(rx), vervanging) for rx, vervanging in NEUTRALISEER]
_REGELS = [(klasse, re.compile(rx)) for klasse, rx in REGELS]


def normaliseer(titel: str) -> str:
    t = " ".join(titel.lower().replace("’", "'").split())
    for rx, vervanging in _NEUTRALISEER:
        t = rx.sub(vervanging, t)
    return t


def classificeer_titel(titel: str) -> str:
    t = normaliseer(titel or "")
    for klasse, rx in _REGELS:
        if rx.search(t):
            return klasse
    return "onbepaald"
