#3. faza
source("uvoz/uvoz.r")
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

seznam_PP[seznam_PP == "PU MURSKA\rSOBOTA"] <- "PU MURSKA SOBOTA"
seznam_PP[seznam_PP == "POLICIJSKA POSTAJA ZA IZRAVNALNE UKREPE MURSKA\rSOBOTA"] <- "POLICIJSKA POSTAJA MURSKA SOBOTA"
skupek[skupek == "POLICIJSKA POSTAJA RIBNICA"] <- "POLICIJSKA POSTAJA LOGATEC"
skupek[skupek == "POLICIJSKA POSTAJA "] <- "nezabelezeno"



skupek$uprava <- seznam_PP[match(skupek$postaja, seznam_PP$ime), 3]
skupek[is.na(skupek)] <- "nezabeleženo"

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


# nesrece_alkohol <- ggplot() + 
#   geom_line(skupek, mapping = aes(x = LETO, y = ALKOHOL)) + labs(title = "Število prometnih nesreč pod vplivom alkohola") + xlab("leto") + ylab("število nesreč")

# graf_po_letih <- ggplot()+
#   geom_line(skupek, mapping = aes(x= LETO, y= NESRECE.SKUPAJ)) + labs(title = "Prometne nesreče po letih 2005 - 2020") + xlab("leto") + ylab("Število nesreč")

# graf_vzroki <- ggplot(vzrok1, aes(x = LETO, y = value, fill = variable)) + 
#   geom_bar (stat = 'identity', position = "dodge") + 
#   theme(legend.position="bottom", legend.key.size = unit(0.2, "cm")) +
#   labs(title = "Število prometnih nesreč po vzroku") + xlab("leto") + ylab("število prometnih nesreč")

# graf_tipi <- ggplot(tip1, aes(x = LETO, y = value, fill = variable)) + 
#   geom_bar(stat = 'identity', position = "dodge") +
#   theme(legend.position="bottom", legend.key.size = unit(0.2, "cm")) +
#   labs(title = "Število prometnih nesreč po tipu nesreče") + xlab("leto") + ylab("število prometnih nesreč")

# graf_vremena <- ggplot(vreme1, aes(x = LETO, y = value, fill = variable)) + 
#   geom_bar(stat = 'identity', position = "dodge") +
#   theme(legend.position="bottom", legend.key.size = unit(0.2, "cm")) +
#   labs(title = "Število prometnih nesreč po vremenu") + xlab("leto") + ylab("število prometnih nesreč")


# graf_vrste <- ggplot(vrsta1, aes(x = LETO, y = value, fill = variable)) + 
#   geom_bar(stat = 'identity', position = "dodge") +
#   theme(legend.position="bottom", legend.key.size = unit(0.2, "cm")) +
#   labs(title = "Število prometnih nesreč po vrsti vozišča") + xlab("leto") + ylab("število prometnih nesreč")

# graf_nesrece <- ggplot(nesrece1, aes(x = LETO, y = value, fill = variable)) + 
#   geom_bar(stat = 'identity', position = "dodge") +
#   theme(legend.position="bottom", legend.key.size = unit(0.2, "cm")) +
#   labs(title = "Število prometnih nesreč po policijski upravi") + xlab("leto") + ylab("število prometnih nesreč")

# graf_stanja <- ggplot(stanje1, aes(x = LETO, y = value, fill = variable)) + 
#   geom_bar(stat = 'identity', position = "dodge") +
#   theme(legend.position="bottom", legend.key.size = unit(0.2, "cm")) +
#   labs(title = "Število prometnih nesreč po stanju vozišča") + xlab("leto") + ylab("število prometnih nesreč")
