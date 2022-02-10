#2. faza
library(tabulizer)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")


uvoz_nesrec <- function(ime){
    
    return(read.table(as.character(ime), sep = ";", dec = ",", fill = TRUE, as.is = TRUE, header = TRUE, col.names = c("zaporedna_stevilka", "klasifikacija", "postaja", "datum", "ura", "v_naselju", "lokacija", "vrsta_ceste_naselja", 
                    "sifra_ceste_naselja", "tekst_cesta_naselje", "sifra_odseka_ulice", "tekst_odseka_ulice", "stacionaza", "opis", "vzrok", "tip", "vreme",
                    "stanje_prometa", "stanje", "vrsta", "geo_x", "geo_y", "zap_st_osebe", "povzrocitelj", "starost", "spol", "UE_stalnega_prebivalisca",
                    "drzavljanstvo", "poskodba_udelezenca", "vrsta_udelezenca", "varnostni_pas", "vozniski_staz_leta", "vozniski_staz_mesec", "alkotest", "pregled"), fileEncoding = "Windows-1250"))
}


uvoz_nesrec_leta <- function(ime){
    kategorija2 <- c("skupaj", "smrtni_izid", "telesna_poskodba", "materialna_skoda")

    leta <- 2005:2014
    return(read.table("podatki/st_urad_leta.csv", sep =";", as.is = TRUE, skip = 2, header = TRUE, col.names = c("Mesec", as.vector(outer(kategorija2, leta, paste0))), fileEncoding = "Windows-1250", row.names = 1))
}


ekstrat <- extract_tables("https://www.policija.si/images/stories/O_Policiji/Seznam_vseh_PP_01122017.pdf")
seznam_PP <- do.call(rbind, ekstrat)
seznam_PP <- as.data.frame(seznam_PP)
headers <- c("stevilka", "ime", "PU")
names(seznam_PP) <- headers
seznam_PU <- distinct(seznam_PP[c("PU")])
seznam_imen_PP <- seznam_PP[c("ime")]

prvo <- uvoz_nesrec("podatki/pn2005.csv") 
uprava <- data.frame(rep(0, nrow(prvo)))
colnames(uprava) <- "uprava"
prvo <- cbind(prvo, uprava)
prvo <- prvo[!duplicated(prvo$zaporedna_stevilka),]
skupek <- prvo

sestavi <- function() {
    for (i in 2006:2020) {

        zapored <- uvoz_nesrec(paste("podatki/pn", as.character(i), ".csv", sep = ""))
        zapored <- zapored[!duplicated(zapored$zaporedna_stevilka),]

        uprava2 <- data.frame(rep(0, nrow(zapored)))
        colnames(uprava2) <- "uprava"
        zapored <- cbind(zapored, uprava2)
        skupek <<- rbind(skupek, zapored)

        a <- i - 2004
    }
    return()
}

sestavi()
ohrani <- c("zaporedna_stevilka", "datum", "postaja", "stanje", "vrsta", "vzrok", "tip", "vreme", "alkotest", "pregled", "uprava")
skupek <- skupek[ohrani]
skupek <- skupek[c(1, 2, 3, 11, 4, 5, 6, 7, 8, 9, 10)]
skupek$postaja[skupek$postaja == "MARIBOR"] <- "MARIBOR I"
skupek$postaja[skupek$postaja == "LJUBLJANA"] <- "LJUBLJANA CENTER"
skupek$postaja <- paste("POLICIJSKA POSTAJA ", skupek$postaja, sep = "")
skupek$alkotest[skupek$alkotest > 0] <- "POZITIVEN"
skupek$alkotest[skupek$alkotest == 0] <- "NEGATIVEN"
skupek$alkotest[skupek$pregled > 0] <- "POZITIVEN"
skupek$pregled <- NULL
skupek$tip[skupek$tip == "TRCENJE V STOJECE / PARKIRANO VOZILO"] <- "TRCENJE V PARKIRANO VOZILO"
skupek$datum <- skupek$datum %>% as.Date(format = "%d.%m.%Y") %>% format("%Y") %>% as.numeric()
names(skupek)[names(skupek) == "datum"] <- "leto"
skupek$vrsta[skupek$vrsta == "HRAPAV  ASFALT / BETON"] <- "HRAPAV ASFALT"
skupek$vrsta[skupek$vrsta == "NERAVEN ASFALT / BETON"] <- "NERAVEN ASFALT"
skupek$vrsta[skupek$vrsta == "ZGLAJEN ASFALT / BETON"] <- "ZGLAJEN ASFALT"
