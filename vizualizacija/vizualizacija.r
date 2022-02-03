#3. faza
source("uvoz/uvoz.r")

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

#zemljevid <- zemljevid_nesrec()

#lon_2005 <- nesrece2005$longitude
#lat_2005 <- nesrece2006$latitude

#koordinate_2005 <- as.data.frame(cbind(lon_2005, lat_2005))

#zemljevid_2005 <- get_map(location = c(lon = mean(koordinate_2005$lon_2005), lat = mean(koordinate_2005$lat_2005)), zoom = 5, maptype = "satellite", scale = 2)

#ggmap(zemljevid_2005) +
#    geom_point(data = koordinate_2005, aes(x = lon_2005, y = lat_2005, fill = "red", alpha = 0.8), size = 5, shape = 21) +
#    guides(fill = FALSE, alpha = FALSE, size = FALSE)