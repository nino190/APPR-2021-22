#3. faza
ekstrat <- extract_tables("https://www.policija.si/images/stories/O_Policiji/Seznam_vseh_PP_01122017.pdf")
seznam_PP <- do.call(rbind, ekstrat)
seznam_PP <- as.data.frame(seznam_PP)
headers <- c("stevilka", "ime", "uprava")
names(seznam_PP) <- headers

seznam_PU <- distinct(seznam_PP[c("uprava")])

seznam_imen_PP <- seznam_PP[c("ime")]

sestevek_PP <- table(prvo$postaja)

names(sestevek_PP) <- c("napaka", names(sestevek_PP[-1]))

seznam_nesrec <- rep(0, length(seznam_PU[ ,"uprava"]))

names(seznam_nesrec) <- seznam_PU[ ,"uprava"]

for (pp in names(sestevek_PP)){
    for (i in 1:as.integer(tail(seznam_PP["stevilka"], n = 1))) {
        if (grepl(paste("POLICIJSKA POSTAJA ", pp, sep = ""), seznam_PP[i, "ime"])) {
            seznam_nesrec[seznam_PP[i, "uprava"]] <- as.integer(seznam_nesrec[seznam_PP[i, "uprava"]]) + as.integer(sestevek_PP[pp])
            break
        }
    }
}
seznam_PP$uprava <- gsub("PU ", "", as.character(seznam_PP$uprava))

for (i in 81:87) {
    seznam_PP$uprava[i] <- "MURSKA SOBOTA"
}
seznam_PP$ime[108] <- "POLICIJSKA POSTAJA NOVO MESTO"

seznam_PP[seznam_PP == "POLICIJSKA POSTAJA ZA IZRAVNALNE UKREPE MURSKA\rSOBOTA"] <- "POLICIJSKA POSTAJA MURSKA SOBOTA"
skupek[skupek == "POLICIJSKA POSTAJA RIBNICA"] <- "POLICIJSKA POSTAJA LOGATEC"
skupek[skupek == "POLICIJSKA POSTAJA "] <- "nezabelezeno"



skupek$uprava <- seznam_PP[match(skupek$postaja, seznam_PP$ime), 3]
skupek[is.na(skupek)] <- "nezabelezeno"

skupek_nesrece <- skupek %>% group_by(leto, uprava)

po_letih <- skupek_nesrece %>% summarise(sestevek = n())

graf_nesrec <- ggplot(po_letih) +
  aes(x = leto, y = sestevek, fill = uprava) + 
  geom_bar(stat = 'identity', position = "dodge") +
  theme(legend.position="bottom", legend.key.size = unit(0.3, "cm")) +
  labs(title = "Število prometnih nesreč po policijski upravi") + xlab("leto") + ylab("število prometnih nesreč")

vse_nesrece <- po_letih %>%
  group_by(leto) %>%
  summarize(sestevek = sum(sestevek), .groups = "drop_last") %>%
  arrange(leto)

graf_vse <- ggplot(vse_nesrece) + aes(x = leto, y = sestevek) + geom_col() + labs(title = "Število prometnih nesreč po letih") + xlab("leto") + ylab("število prometnih nesreč")

alkohol_vse <- skupek %>% group_by(leto, alkotest) %>% summarize(alkoholiziranost = n())
alkohol_vse <- alkohol_vse[!(alkohol_vse$alkotest == "NEGATIVEN"),]
alkohol_vse <- alkohol_vse %>% group_by(leto, alkoholiziranost)
graf_alkohol <- ggplot(alkohol_vse) + geom_col() + aes(x = leto, y = alkoholiziranost) + labs(title = "Število nesreč z alkoholiziranim voznikom")

vse_vzroki <- skupek %>% group_by(vzrok) %>%
  summarize(povprecje = n())
vse_vzroki$povprecje <- vse_vzroki$povprecje / 16
vse_vzroki$povprecje <- round(100 * vse_vzroki$povprecje / sum(vse_vzroki$povprecje), 2)

graf_vzroki <- ggplot(vse_vzroki, aes(x = "", y = povprecje, fill = vzrok)) +
  geom_col(color = "black") +
  geom_text(aes(label = povprecje),
            position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  coord_polar(theta = "y") + labs(title = "Delež vzrokov nesreč v povprečnem letu") + ylab("odstotek(%)")

vse_vreme <- skupek %>% group_by(vreme) %>%
  summarize(povprecje = n())
vse_vreme$povprecje <- as.integer(vse_vreme$povprecje / 16)
vse_vreme$povprecje <- round(100 * vse_vreme$povprecje / sum(vse_vreme$povprecje), 2)


graf_vreme <- ggplot(vse_vreme, aes(x = "", y = povprecje, fill = vreme)) +
  geom_col(color = "black") +
  geom_text(aes(label = povprecje),
            position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  coord_polar(theta = "y") + labs(title = "Vreme v času nesreč v povprečnem letu") + ylab("odstotek(%)")

skupek_tipi <- skupek %>% group_by(leto, tip)

po_tipih <- skupek_tipi %>% summarise(sestevek = n())

graf_tipi <- ggplot(po_tipih) +
  aes(x = leto, y = sestevek, fill = tip) + 
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Število prometnih nesreč glede na tip nesreče") + xlab("leto") + ylab("število prometnih nesreč")

vrsta1 <- skupek%>% group_by(leto, vrsta) %>% summarise(sestevek = n())

graf_vrste <- ggplot(vrsta1, aes(x = leto, y = sestevek, fill = vrsta)) + 
  geom_bar(stat = 'identity', position = "dodge") +
  theme(legend.position="bottom", legend.key.size = unit(0.2, "cm")) +
  labs(title = "Število prometnih nesreč po vrsti vozišča") + xlab("leto") + ylab("število prometnih nesreč") + 
  coord_flip()

stanje1 <- skupek %>% group_by(leto, stanje)
po_stanju <- stanje1 %>% summarise(sestevek = n())

graf_stanja <- ggplot(po_stanju, aes(x = leto, y = sestevek, fill = stanje)) + 
  geom_bar(stat = 'identity', position = "dodge") +
  labs(title = "Število prometnih nesreč po stanju vozišča") + xlab("leto") + ylab("število prometnih nesreč")

Slovenija <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip",
                             "SVN_adm1", encoding="UTF-8") %>% fortify()
colnames(Slovenija)[12]<-'uprava'

Slovenija$uprava <- gsub('Obalno-kraška', 'KOPER', Slovenija$uprava)
Slovenija$uprava <- gsub('Goriška', 'NOVA GORICA', Slovenija$uprava)
Slovenija$uprava <- gsub('Notranjsko-kraška', 'KOPER', Slovenija$uprava)
Slovenija$uprava <- gsub('Spodnjeposavska', 'NOVO MESTO', Slovenija$uprava)
Slovenija$uprava <- gsub('Jugovzhodna Slovenija', 'NOVO MESTO', Slovenija$uprava)
Slovenija$uprava <- gsub('Gorenjska', 'KRANJ', Slovenija$uprava)
Slovenija$uprava <- gsub('Zasavska', 'LJUBLJANA', Slovenija$uprava)
Slovenija$uprava <- gsub('Osrednjeslovenska', 'LJUBLJANA', Slovenija$uprava)
Slovenija$uprava <- gsub('Savinjska', 'CELJE', Slovenija$uprava)
Slovenija$uprava <- gsub('Podravska', 'MARIBOR', Slovenija$uprava)
Slovenija$uprava <- gsub('Koroška', 'CELJE', Slovenija$uprava)
Slovenija$uprava <- gsub('Pomurska', 'MURSKA SOBOTA', Slovenija$uprava)

po_letih_2020$oznaka <- paste(po_letih_2020$uprava, po_letih_2020$sestevek, sep=",  ")
po_letih_2020 <- po_letih_2020[-7,]

points <- data.frame(oznaka = po_letih_2020$oznaka, long = c(15.263889, 13.729444, 14.355556, 14.5, 15.643889, 16.163056, 13.643333, 15.162778), lat = c(46.228889, 45.546389, 46.243611, 46.0500, 46.5625, 46.66, 45.955833, 45.798056))

po_letih_2020 <- filter(po_letih, leto == 2020)
po_letih_2020 <-data.frame(po_letih_2020)
zemljevid <- ggplot() +
  geom_polygon(data = right_join(po_letih_2020[-1], Slovenija, by = "uprava"), aes(x=long, y=lat, group=group, fill=sestevek)) +
  ggtitle("Število nesreč po policijskih upravah v letu 2020") + 
  theme(axis.title=element_blank(), axis.text=element_blank(), 
        axis.ticks=element_blank(), panel.background = element_blank()) +
  scale_fill_gradient(low = '#FCDADA', high='#970303', limits=c(1,5000)) +
  labs(fill="Število nesreč") +
  geom_point(data = points, aes(x = long, y = lat), color = "black", size = 2, show.legend = FALSE) +
  annotate("text", x = 15.263889, y = 46.328889, label = points$oznaka[1])+
  annotate("text", x = 13.729444, y = 45.646389, label = points$oznaka[2])+
  annotate("text", x = 14.355556, y = 46.343611 , label = points$oznaka[3])+
  annotate("text", x = 14.5, y = 46.15, label = points$oznaka[4])+
  annotate("text", x = 15.643889, y = 46.6625, label = points$oznaka[5])+
  annotate("text", x = 16.163056, y = 46.76, label = points$oznaka[6])+
  annotate("text", x = 13.743333, y = 46.055833 , label = points$oznaka[7])+
  annotate("text", x = 15.162778, y = 45.898056, label = points$oznaka[8])