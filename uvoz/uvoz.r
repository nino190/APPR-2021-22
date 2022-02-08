#2. faza
library(tabulizer)

sl <- locale("sl", decimal_mark=",", grouping_mark=".")


uvoz_nesrec <- function(ime){
    
    return(read.table(as.character(ime), sep = ";", dec = ",", fill = TRUE, as.is = TRUE, header = TRUE, col.names = c("zaporedna_stevilka", "klasifikacija", "postaja", "datum", "ura", "v_naselju", "lokacija", "vrsta_ceste_naselja", 
                    "sifra_ceste_naselja", "tekst_cesta_naselje", "sifra_odseka_ulice", "tekst_odseka_ulice", "stacionaza", "opis", "vzrok", "tip", "vreme",
                    "stanje_prometa", "stanje_vozisca", "vrsta_vozisca", "geo_x", "geo_y", "zap_st_osebe", "povzrocitelj", "starost", "spol", "UE_stalnega_prebivalisca",
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

# sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# nesrece_uprava <- rep(0, 16)
# nesrece_alkotest <- rep(0, 16)
# nesrece_vzrok <- rep(0, 16)
# sestevek_nesrece <- rep(0, 16)
# nesrece_tip <- rep(0, 16)

# nesrece <- data.frame(x1 = 1:8)
# rownames(nesrece) <- seznam_PU[,"PU"]
# years <- 2005:2020

# seznam_vzrok <- 0
# seznam_tip <- 0
# seznam_vreme <- 0
# seznam_stanje <- 0
# seznam_vrsta <- 0

# vzrok <- data.frame(x1 = 1:11)
# tip <- data.frame(x1 = 1:10)
# vreme <- data.frame(x1 = 1:8)
# stanje <- data.frame(x1 = 1:9)
# vrsta <- data.frame(x1 = 1:5)
prvo <- uvoz_nesrec("podatki/pn2005.csv")
years <- data.frame(rep(2005, nrow(prvo)))
colnames(years) <- "leto"
uprava <- data.frame(rep(0, nrow(prvo)))
colnames(uprava) <- "uprava"
prvo <- cbind(prvo, years, uprava)
prvo <- prvo[!duplicated(prvo$zaporedna_stevilka),]
skupek <- prvo




sestavi <- function() {
    for (i in 2006:2020) {

        zapored <- uvoz_nesrec(paste("podatki/pn", as.character(i), ".csv", sep = ""))
        zapored <- zapored[!duplicated(zapored$zaporedna_stevilka),]

        years2 <- data.frame(rep(i, nrow(zapored)))
        colnames(years2) <- "leto"
        uprava2 <- data.frame(rep(0, nrow(zapored)))
        colnames(uprava2) <- "uprava"
        zapored <- cbind(zapored, years2, uprava2)
        skupek <<- rbind(skupek, zapored)

        a <- i - 2004
        # leto <- uvoz_nesrec(paste("podatki/pn", as.character(i), ".csv", sep = ""))
        # leto <- leto[!duplicated(leto$zaporedna_stevilka),]
        
        # poz_alko <- 0

        # for (j in 1:as.integer(tail(leto["zaporedna_stevilka"], n = 1))) {
        #     if (leto[j, "alkotest"] > 0) {
        #         poz_alko <- poz_alko + 1
        #     }
        #     else if (leto[j, "pregled"] > 0) {
        #         poz_alko <- poz_alko + 1
        #     }
        # }
        # nesrece_alkotest[a] <<- poz_alko

        # sestevek_PP <- table(leto$postaja)
        # names(sestevek_PP) <- c("napaka", names(sestevek_PP[-1]))
        # seznam_nesrec <- rep(0, length(seznam_PU[ ,"PU"]))
        # names(seznam_nesrec) <- seznam_PU[ ,"PU"]
        # for (pp in names(sestevek_PP)){
        #     for (k in 1:as.integer(tail(seznam_PP["stevilka"], n = 1))) {
        #         if (grepl(paste("POLICIJSKA POSTAJA ", pp, sep = ""), seznam_PP[k, "ime"])) {
        #             seznam_nesrec[seznam_PP[k, "PU"]] <- as.integer(seznam_nesrec[seznam_PP[k, "PU"]]) + as.integer(sestevek_PP[pp])
        #             break
        #         }
        #     }
        # }
        
        # nesrece[a] <<- seznam_nesrec
        # sestevek_nesrece[a] <<- sum(seznam_nesrec)

        # seznam_vzrok <<- distinct(leto["vzrok"])
        # sestevek_vzrok <- table(leto["vzrok"])
        # vzrok[a] <<- sestevek_vzrok

        # seznam_tip <<- distinct(leto["tip"])
        # sestevek_tip <- table(leto["tip"])
        # tip[a] <<- sestevek_tip

        # seznam_vreme <<- distinct(leto["vreme"])
        # sestevek_vreme <- table(leto["vreme"])
        # vreme[a] <<- sestevek_vreme

        # seznam_stanje <<- distinct(leto["stanje_vozisca"])
        # sestevek_stanje <- table(leto["stanje_vozisca"])
        # stanje[a] <<- sestevek_stanje

        # seznam_vrsta <<- distinct(leto["vrsta_vozisca"])
        # sestevek_vrsta <- table(leto["vrsta_vozisca"])
        # vrsta[a] <<- sestevek_vrsta

    }
    return()
}

sestavi()
ohrani <- c("zaporedna_stevilka", "postaja", "opis", "vzrok", "tip", "vreme", "alkotest", "pregled", "leto", "uprava")
skupek <- skupek[ohrani]
skupek <- skupek[c(1, 9, 2, 10, 3, 4, 5, 6, 7, 8)]
skupek$postaja[skupek$postaja == "MARIBOR"] <- "MARIBOR I"
skupek$postaja[skupek$postaja == "LJUBLJANA"] <- "LJUBLJANA CENTER"
skupek$postaja <- paste("POLICIJSKA POSTAJA ", skupek$postaja, sep = "")

# sestevek_nesrece <- data.frame(sestevek_nesrece)
# names(nesrece_alkotest) <- as.vector(as.character(years))
# rownames(sestevek_nesrece) <- as.vector(as.character(years))
# colnames(sestevek_nesrece) <- as.vector("NESRECE.SKUPAJ")
# names(nesrece) <- as.vector(as.character(years))
# names(vzrok) <- as.vector(as.character(years))
# names(tip) <- as.vector(as.character(years))
# names(vreme) <- as.vector(as.character(years))
# names(stanje) <- as.vector(as.character(years))
# names(vrsta) <- as.vector(as.character(years))

# rownames(vzrok) <- seznam_vzrok[,"vzrok"]
# rownames(tip) <- seznam_tip[,"tip"]
# rownames(vreme) <- seznam_vreme[,"vreme"]
# rownames(vrsta) <- seznam_vrsta[, "vrsta_vozisca"]
# rownames(stanje) <- seznam_stanje[, "stanje_vozisca"]

# leto <- data.frame(c(2005:2020))
# rownames(leto) <- as.vector(as.character(years))
# colnames(leto) <- as.vector("LETO")

# skupek <- do.call(rbind, list(nesrece, vzrok, tip, vreme, stanje, vrsta, t(sestevek_nesrece), t(leto)))

# tip <- data.frame(t(tip))
# vreme <- data.frame(t(vreme))
# vrsta <- data.frame(t(vrsta))
# nesrece <- data.frame(t(nesrece))
# vzrok <- data.frame(t(vzrok))
# stanje <- data.frame(t(stanje))
# nesrece_alkotest <- data.frame(nesrece_alkotest)
# colnames(nesrece_alkotest) <- as.vector("ALKOHOL")

# skupek <- do.call(cbind, list(nesrece, vzrok, tip, vreme, stanje, vrsta, sestevek_nesrece, nesrece_alkotest, leto))



# colnames(nesrece) <- make.names(colnames(nesrece))
# colnames(vzrok) <- make.names(colnames(vzrok))
# colnames(skupek) <- make.names(colnames(skupek))
# colnames(skupek)[28] <- as.vector("OSTALO2")
# colnames(skupek)[41] <- as.vector("OSTALO3")
# colnames(skupek)[50] <- as.vector("OSTALO4")
# vzrok <- cbind(vzrok, leto)
# tip <- cbind(tip, leto)
# vreme <- cbind(vreme, leto)
# vrsta <- cbind(vrsta, leto)
# nesrece <- cbind(nesrece, leto)
# stanje <- cbind(stanje, leto)
# nesrece_alkotest <- cbind(nesrece_alkotest, leto)

# vzrok1 <- melt(vzrok, "LETO")
# tip1 <- melt(tip, "LETO")
# vreme1 <- melt(vreme, "LETO")
# vrsta1 <- melt(vrsta, "LETO")
# nesrece1 <- melt(nesrece, "LETO")
# stanje1 <- melt(stanje, "LETO")
