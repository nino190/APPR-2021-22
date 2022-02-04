# Analiza podatkov s programom R - 2021/22 #
Avtor: Nino Cajnkar
## Analiza Prometnih nesreč v Sloveniji ##

## Tematika ##

V slopu mojega projekta si bomo pogledali statistiko prometnih nesreč v Sloveniji za leta 2005 do 2020.

Nadalje bom razdelil podatke glede na vzrok nesreč, seštevek nesreč, razporeditev nesreč po policijskih upravah in mesecih.
## Podatki (https://www.policija.si/o-slovenski-policiji/statistika/prometna-varnost, https://pxweb.stat.si/SiStatData/pxweb/sl/Data/Data/2222003S.px/##
1. Statistika nesreč:
* vzrok
* stanje vozišča
* alkoholiziranost
* mesec nesreče
* klasifikacija
* tip nesreče
* vreme
* stanje vozišča
* vrsta vozišča

## Program: ##
Glavni program in poročilo se nahajata v datoteki projekt.Rmd. Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:
* Obdelava, uvoz in čiščenje podatkov: uvoz/uvoz.r
* Analiza in vizualizacija podatkov: vizualizacija/vizualizacija.r
* Napredna analiza podatkov: analiza/analiza.r

Vnaprej pripravljene funkcije se nahajo v mapi lib/. Potrebne knjižnice so v datoteki lib/libraries.r
Podatkovni tipi so v mapi podatki/. Zemljevidi v obliki SHP, ki jih program pobere, se shranijo v mapo ../zemljevidi/ (izven mapre projekta).
Več povezav do podatkov za posamezne znamke in tipe bo dodanih naknadno.
