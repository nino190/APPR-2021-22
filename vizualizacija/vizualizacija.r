#3. faza
source("uvoz/uvoz.r")
source("libraries/uvozi.zemljevid.r")
source("libraries/libraries.r")

nesrece_leta <- uvoz_nesrec_leta()

ekstrat <- extract_tables("https://www.policija.si/images/stories/O_Policiji/Seznam_vseh_PP_01122017.pdf")
seznam_PP <- do.call(rbind, ekstrat)
seznam_PP <- as.data.frame(seznam_PP)
headers <- c("stevilka", "ime", "PU")
names(seznam_PP) <- headers

nesrece2005 <- uvoz_nesrec("podatki/pn2005.csv")
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
zem_slo <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip", "SVN_adm1", encoding = "UTF-8")

nesrece_alkohol <- barplot(nesrece_alkotest)
nesrece_skupaj <- barplot(t(sestevek_nesrece))


graf_po_letih <- ggplot()+
  geom_line (skupek, mapping = aes(x= LETO, y= NESRECE.SKUPAJ)) + labs(title = "Prometne nesreče po letih 2005 - 2020") + xlab("leto") + ylab("Število nesreč")



graf_vzroki <- ggplot(vzrok1, aes(x = LETO, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = "dodge") + 
    theme(legend.position="bottom", legend.key.size = unit(0.2, "cm"))

graf_tipi <- ggplot(tip1, aes(x = LETO, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = "dodge") +
    theme(legend.position="bottom", legend.key.size = unit(0.2, "cm"))

graf_vremena <- ggplot(vreme1, aes(x = LETO, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = "dodge") +
    theme(legend.position="bottom", legend.key.size = unit(0.2, "cm"))

graf_vrste <- ggplot(vrsta1, aes(x = LETO, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = "dodge") +
    theme(legend.position="bottom", legend.key.size = unit(0.2, "cm"))

graf_nesrece <- ggplot(nesrece1, aes(x = LETO, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = "dodge") +
    theme(legend.position="bottom", legend.key.size = unit(0.2, "cm"))

graf_stanja <- ggplot(stanje1, aes(x = LETO, y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = "dodge") +
    theme(legend.position="bottom", legend.key.size = unit(0.2, "cm"))
