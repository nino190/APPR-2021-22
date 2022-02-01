#2. faza
uvoz_nesrec <- function(ime){
    kategorija <- c("zaporedna_stevilka", "klasifikacija", "upravna_enota", "datum", "ura", "v_naselju", "lokacija", "vrsta_ceste_naselja", 
                    "sifra_ceste_naselja", "tekst_cesta_naselje", "sifra_odseka_ulice", "tekst_odseka_ulice", "stacionaza", "vzrok", "tip", "vreme",
                    "stanje_prometa", "stanje_vozisca", "vrsta_vozisca", "geo_x", "geo_y", "zap_st_osebe", "povzrocitelj", "starost", "spol", "UE_stalnega_prebivalisca",
                    "drzavljanstvo", "poskodba_udelezenca", "vrsta_udelezenca", "varnostni_pas", "vozniski_staz_leta", "vozniski_staz_mesec", "alkotest", "pregled")
    
    return(read.table(paste("podatki/", as.character(ime), ".csv", sep = ""), sep = ";", as.is = TRUE, header = TRUE, col.names = kategorija, fileEncoding = "Windows-1250", row.names = 1))
}

cat("Uvažanje podatkov o nesrečah..\n")

nesrece2005 <- uvoz_nesrec(pn2005)
nesrece2005 = nesrece2005[!duplicated(nesrece2005$zaporedna_stevilka),]
nesrece2006 <- uvoz_nesrec(pn2006)
nesrece2006 = nesrece2006[!duplicated(nesrece2006$zaporedna_stevilka),]
nesrece2007 <- uvoz_nesrec(pn2007)
nesrece2007 = nesrece2007[!duplicated(nesrece2007$zaporedna_stevilka),]
nesrece2008 <- uvoz_nesrec(pn2008)
nesrece2008 = nesrece2008[!duplicated(nesrece2008$zaporedna_stevilka),]
nesrece2009 <- uvoz_nesrec(pn2009)
nesrece2009 = nesrece2009[!duplicated(nesrece2009$zaporedna_stevilka),]
nesrece2010 <- uvoz_nesrec(pn2010)
nesrece2010 = nesrece2010[!duplicated(nesrece2010$zaporedna_stevilka),]
nesrece2011 <- uvoz_nesrec(pn2011)
nesrece2011 = nesrece2011[!duplicated(nesrece2011$zaporedna_stevilka),]
nesrece2012 <- uvoz_nesrec(pn2012)
nesrece2012 = nesrece2012[!duplicated(nesrece2012$zaporedna_stevilka),]
nesrece2013 <- uvoz_nesrec(pn2013)
nesrece2013 = nesrece2013[!duplicated(nesrece2013$zaporedna_stevilka),]
nesrece2014 <- uvoz_nesrec(pn2014)
nesrece2014 = nesrece2014[!duplicated(nesrece2014$zaporedna_stevilka),]
nesrece2015 <- uvoz_nesrec(pn2015)
nesrece2015 = nesrece2015[!duplicated(nesrece2015$zaporedna_stevilka),]
nesrece2016 <- uvoz_nesrec(pn2016)
nesrece2016 = nesrece2016[!duplicated(nesrece2016$zaporedna_stevilka),]
nesrece2017 <- uvoz_nesrec(pn2017)
nesrece2017 = nesrece2017[!duplicated(nesrece2017$zaporedna_stevilka),]
nesrece2018 <- uvoz_nesrec(pn2018)
nesrece2018 = nesrece2018[!duplicated(nesrece2018$zaporedna_stevilka),]
nesrece2019 <- uvoz_nesrec(pn2019)
nesrece2019 = nesrece2019[!duplicated(nesrece2019$zaporedna_stevilka),]
nesrece2020 <- uvoz_nesrec(pn2020)
nesrece2020 = nesrece2020[!duplicated(nesrece2010$zaporedna_stevilka),]



uvoz_nesrec_leta <- function(ime){
    kategorija2 <- c("skupaj", "smrtni_izid", "telesna_poskodba", "materialna_skoda")

    leta <- 2005:2014
    return(read.table("podatki/st_urad_leta.csv", sep =";", as.is = TRUE, skip = 2, header = TRUE, col.names = c("Mesec", as.vector(kategorija2, leta, paste0)), fileEncoding = "Windows-1250", row.names = 1))
}
cat("Uvažanje podatkov o nesrečah po mesecih..\n")

nesrece_leta <- uvoz_nesrec_leta()

sl <- locale("sl", decimal_mark=",", grouping_mark=".")
