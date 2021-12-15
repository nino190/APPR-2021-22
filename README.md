# Analiza podatkov s programom R - 2021/22 #
Avtor: Nino Cajnkar
## Analiza Industrije Vodke ##

## Tematika ##

V slopu mojega projekta si bomo pogledali statistiko prodaje alkoholne pijače Vodke za zadnjih nekaj let.
Najprej si bomo ogledali splošno prodajo in konzumacijo izdelka in vpliv na celoten trg alkoholnih pijač.
Nadalje bomo razdelili podatke na različne tipe in znamke vodk ter primerjali vpliv smernikov (na primer kakovost, cena, dostopnost) na prodajo posameznega izdelka.

## Podatki (za [Ameriko](https://www.statista.com/topics/3741/vodka-industry/#dossierKeyfigures) in [Evropo)](https://www.statista.com/outlook/cmo/alcoholic-drinks/spirits/vodka/europe): ##
1. Statistika prodaje:
* Svetovna prodaja
* Velikost trga v Ameriki in Evropi
* Prodaja posameznih znak
* Prodaja posameznih tipov izdelka
* Cenovna razporeditev
2. Statistika izdelka:
* Ime izdelka
* Znamka
* Tip (način izdelave)
* Stopnja alkohola
* Cena
* Dostopnost
3. Statistika večjih znamk:
* Ime znamke
* Prodaja
* Kakovost
* Dostopnost
* Povprečna cena

## Program: ##
Glavni program in poročilo se nahajata v datoteki projekt.Rmd. Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:
* obdelava, uvoz in čiščenje podatkov: uvoz/uvoz.r
* analiza in vizualizacija podatkov: vizualizacija/vizualizacija.r
* napredna analiza podatkov: analiza/analiza.r
Vnaprej pripravljene funkcije se nahajo v mapi lib/. Potrebne knjižnice so v datoteki lib/libraries.r
Podatkovni tipi so v mapi podatki/. Zemljevidi v obliki SHP, ki jih program pobere, se shranijo v mapo ../zemljevidi/ (izven mapre projekta).
