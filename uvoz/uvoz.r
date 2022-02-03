#2. faza
library(tabulizer)
uvoz_nesrec <- function(ime){
    # kategorija <- c("zaporedna_stevilka", "klasifikacija", "postaja", "datum", "ura", "v_naselju", "lokacija", "vrsta_ceste_naselja", 
    #                 "sifra_ceste_naselja", "tekst_cesta_naselje", "sifra_odseka_ulice", "tekst_odseka_ulice", "stacionaza", "opis", "vzrok", "tip", "vreme",
    #                 "stanje_prometa", "stanje_vozisca", "vrsta_vozisca", "longitude", "latitude", "zap_st_osebe", "povzrocitelj", "starost", "spol", "UE_stalnega_prebivalisca",
    #                 "drzavljanstvo", "poskodba_udelezenca", "vrsta_udelezenca", "varnostni_pas", "vozniski_staz_leta", "vozniski_staz_mesec", "alkotest", "pregled")
    
    return(read.table(as.character(ime), sep = ";", fill = TRUE, as.is = TRUE, header = TRUE, col.names = c("zaporedna_stevilka", "klasifikacija", "postaja", "datum", "ura", "v_naselju", "lokacija", "vrsta_ceste_naselja", 
                    "sifra_ceste_naselja", "tekst_cesta_naselje", "sifra_odseka_ulice", "tekst_odseka_ulice", "stacionaza", "opis", "vzrok", "tip", "vreme",
                    "stanje_prometa", "stanje_vozisca", "vrsta_vozisca", "geo_x", "geo_y", "zap_st_osebe", "povzrocitelj", "starost", "spol", "UE_stalnega_prebivalisca",
                    "drzavljanstvo", "poskodba_udelezenca", "vrsta_udelezenca", "varnostni_pas", "vozniski_staz_leta", "vozniski_staz_mesec", "alkotest", "pregled"), fileEncoding = "Windows-1250"))
}

cat("Uvažanje podatkov o nesrečah..\n")
nesrece2005 <- uvoz_nesrec("podatki/pn2005.csv")
nesrece2005 <- nesrece2005[!duplicated(nesrece2005$zaporedna_stevilka),]
# nesrece2006 <- uvoz_nesrec("podatki/pn2006.csv")
# nesrece2006 <- nesrece2006[!duplicated(nesrece2006$zaporedna_stevilka),]
# nesrece2007 <- uvoz_nesrec("podatki/pn2007.csv")
# nesrece2007 <- nesrece2007[!duplicated(nesrece2007$zaporedna_stevilka),]
# nesrece2008 <- uvoz_nesrec("podatki/pn2008.csv")
# nesrece2008 <- nesrece2008[!duplicated(nesrece2008$zaporedna_stevilka),]
# nesrece2009 <- uvoz_nesrec("podatki/pn2009.csv")
# nesrece2009 <- nesrece2009[!duplicated(nesrece2009$zaporedna_stevilka),]
# nesrece2010 <- uvoz_nesrec("podatki/pn2010.csv")
# nesrece2010 <- nesrece2010[!duplicated(nesrece2010$zaporedna_stevilka),]
# nesrece2011 <- uvoz_nesrec("podatki/pn2011.csv")
# nesrece2011 <- nesrece2011[!duplicated(nesrece2011$zaporedna_stevilka),]
# nesrece2012 <- uvoz_nesrec("podatki/pn2012.csv")
# nesrece2012 <- nesrece2012[!duplicated(nesrece2012$zaporedna_stevilka),]
# nesrece2013 <- uvoz_nesrec("podatki/pn2013.csv")
# nesrece2013 <- nesrece2013[!duplicated(nesrece2013$zaporedna_stevilka),]
# nesrece2014 <- uvoz_nesrec("podatki/pn2014.csv")
# nesrece2014 <- nesrece2014[!duplicated(nesrece2014$zaporedna_stevilka),]
# nesrece2015 <- uvoz_nesrec("podatki/pn2015.csv")
# nesrece2015 <- nesrece2015[!duplicated(nesrece2015$zaporedna_stevilka),]
# nesrece2016 <- uvoz_nesrec("podatki/pn2016.csv")
# nesrece2016 <- nesrece2016[!duplicated(nesrece2016$zaporedna_stevilka),]
# nesrece2017 <- uvoz_nesrec("podatki/pn2017.csv")
# nesrece2017 <- nesrece2017[!duplicated(nesrece2017$zaporedna_stevilka),]
# nesrece2018 <- uvoz_nesrec("podatki/pn2018.csv")
# nesrece2018 <- nesrece2018[!duplicated(nesrece2018$zaporedna_stevilka),]
# nesrece2019 <- uvoz_nesrec("podatki/pn2019.csv")
# nesrece2019 <- nesrece2019[!duplicated(nesrece2019$zaporedna_stevilka),]
# nesrece2020 <- uvoz_nesrec("podatki/pn2020.csv")
# nesrece2020 <- nesrece2020[!duplicated(nesrece2010$zaporedna_stevilka),]

# sez_nesrec <- list(nesrece2005, nesrece2006, nesrece2007, nesrece2008, nesrece2009, nesrece2010, nesrece2011, nesrece2012, nesrece2013, nesrece2014, nesrece2015, nesrece2016, nesrece2017, nesrece2018, nesrece2019, nesrece2020)

uvoz_nesrec_leta <- function(ime){
    kategorija2 <- c("skupaj", "smrtni_izid", "telesna_poskodba", "materialna_skoda")

    leta <- 2005:2014
    return(read.table("podatki/st_urad_leta.csv", sep =";", as.is = TRUE, skip = 2, header = TRUE, col.names = c("Mesec", as.vector(outer(kategorija2, leta, paste0))), fileEncoding = "Windows-1250", row.names = 1))
}
cat("Uvažanje podatkov o nesrečah po mesecih..\n")

nesrece_leta <- uvoz_nesrec_leta()

ekstrat <- extract_tables("https://www.policija.si/images/stories/O_Policiji/Seznam_vseh_PP_01122017.pdf")
seznam_PP <- do.call(rbind, ekstrat)
seznam_PP <- as.data.frame(seznam_PP)
headers <- c("stevilka", "ime", "PU")
names(seznam_PP) <- headers


seznam_PU <- distinct(seznam_PP[c("PU")])

seznam_imen_PP <- seznam_PP[c("ime")]

sestevek_PP <- table(nesrece2005$postaja)

names(sestevek_PP) <- c("napaka", names(sestevek_PP[-1]))

seznam_nesrec <- rep(0, length(seznam_PU[ ,"PU"]))

names(seznam_nesrec) <- seznam_PU[ ,"PU"]

for (pp in names(sestevek_PP)){
    for (i in 1:as.integer(tail(seznam_PP["stevilka"], n = 1))) {
        if (grepl(paste("POLICIJSKA POSTAJA ", pp, sep = ""), seznam_PP[i, "ime"])) {
            seznam_nesrec[seznam_PP[i, "PU"]] <- as.integer(seznam_nesrec[seznam_PP[i, "PU"]]) + as.integer(sestevek_PP[pp])
            break
        }
    }
}
#sl <- locale("sl", decimal_mark=",", grouping_mark=".")

