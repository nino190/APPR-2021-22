# 4. faza
nesrece_leta <- uvoz_nesrec_leta()
mes <- data.frame(MESECI = c("januar", "februar", "marec", "april", "maj", "junij", "julij", "avgust", "september", "oktober", "november", "december"))
nesrece_leta <- cbind(mes, nesrece_leta)
nesrece_leta <- adorn_totals(nesrece_leta, "row")
rownames(nesrece_leta) <- as.vector(as.character(1:13))

po_letih$uprava[c(9, 18, 27, 36, 45, 54, 63, 72, 81, 90, 99, 108, 117, 126, 135, 144)] <- "NM"
po_letih$uprava[c(8, 17, 26, 35, 44, 53, 62, 71, 80, 89, 98, 107, 116, 125, 134, 143)] <- "NG"
po_letih$uprava <- substr(po_letih$uprava, 1, 2)

po_letih <- data.frame(leto = po_letih$leto, uprava = po_letih$uprava, sestevek = po_letih$sestevek)

graf1 <- po_letih %>%
  ggplot() +
  geom_point(
    mapping = aes(x = leto, y = sestevek),
    size = 2
  ) +
  geom_label(
    mapping = aes(x = leto, y = sestevek, label = uprava),
  ) +
  xlim(2004, 2021) +
  ylim(0, 10000) +
  theme_classic()

dendrogram = hclust(dist(po_letih[, -2]))


graf2 <- tibble(
  k = 143:1,
  visina = hclust(dist(po_letih[, -2]))$height
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = k, y = visina),
    color = "red"
  ) +
  geom_point(
    mapping = aes(x = k, y = visina),
    color = "red"
  ) +
  xlim(0, 30) +
  scale_x_continuous()(
    breaks = seq(from = 1, to = 30, by = 1),
    limits = c(0, 30)
  ) +
  labs(
    x = "število skupin (k)",
    y = "višina združevanja"
  ) +
  theme_classic()

skupine_2 = dendrogram %>% cutree(k = 2) %>% as.ordered()
skupine_3 = dendrogram %>% cutree(k = 3) %>% as.ordered()
skupine_5 = dendrogram %>% cutree(k = 5) %>% as.ordered()

diagram_skupine <- function(podatki, oznake, skupine, k) {
  podatki <- podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)

  d <- podatki %>%
  ggplot(
    mapping = aes(
      x = x, y = y, color = skupina
    )
  ) +
  geom_point() +
  geom_label(label = oznake, size = 2) +
  scale_color_hue() +
  theme_classic()
  
  for (i in 1:k) {
    d <- d + geom_encircle(
      data <- podatki %>%
        filter(skupina == i)
    )
  }
  d
}
sk2 <- diagram_skupine(po_letih, po_letih$uprava, skupine_2, 2)
sk3 <- diagram_skupine(po_letih, po_letih$uprava, skupine_3, 3)
sk5 <- diagram_skupine(po_letih, po_letih$uprava, skupine_5, 5)

vse_nesrece$leto <- as.character(vse_nesrece$leto)
vse_nesrece$sestevek <- as.character(vse_nesrece$sestevek)

vse_nesrece <- vse_nesrece %>% mutate(leto = parse_integer(leto), sestevek = parse_integer(sestevek))
prileganje <- lm(sestevek ~ leto, data = vse_nesrece)
predict(prileganje, data.frame(leto = seq(2005, 2025, 1)))

graf_predict <- ggplot(vse_nesrece, aes(x = leto, y = sestevek)) + geom_point() + 
  geom_smooth(method = 'lm', formula = y ~ poly(x, 2, raw = TRUE), fullrange=TRUE, color='red') + scale_x_continuous('leto', breaks = seq(2005, 2025, 1), limits = c(2005,2025))+ xlab('Leto') + ylab('Število nesreč')

sestevek_nesrec_2 <- data.frame(years = c(as.character(2005:2014)), sestevek = c(nesrece_leta[13, "skupaj2005"], nesrece_leta[13, "skupaj2006"], nesrece_leta[13, "skupaj2007"], nesrece_leta[13, "skupaj2008"], nesrece_leta[13, "skupaj2009"], nesrece_leta[13, "skupaj2010"], nesrece_leta[13, "skupaj2011"], nesrece_leta[13, "skupaj2012"], nesrece_leta[13, "skupaj2013"], nesrece_leta[13, "skupaj2014"]))
tocke <- data.frame(datum = c(2015:2020), stevilo = c(vse_nesrece$sestevek[11], vse_nesrece$sestevek[12], vse_nesrece$sestevek[13], vse_nesrece$sestevek[14], vse_nesrece$sestevek[15], vse_nesrece$sestevek[16]))

sestevek_nesrec_2$years <- as.character(sestevek_nesrec_2$years)
sestevek_nesrec_2$sestevek <- as.character(sestevek_nesrec_2$sestevek)

sestevek_nesrec_2 <- sestevek_nesrec_2 %>% mutate(years = parse_integer(years), sestevek = parse_double(sestevek))
prileganje2 <- lm(sestevek ~ years, data = sestevek_nesrec_2)
predict(prileganje2, data.frame(years = seq(2005, 2020, 1)))

graf_predict2 <- ggplot(sestevek_nesrec_2, aes(x=years, y=sestevek)) + geom_point() + 
  geom_smooth(method='lm', formula=y ~ poly(x,2,raw=TRUE), fullrange=TRUE, color='blue') + scale_x_continuous('leto', breaks = seq(2005, 2020, 1), limits = c(2005,2020))+ xlab('Leto') + ylab('Število nesreč') + 
  geom_point(data = tocke, mapping = aes(x = datum, y = stevilo), color = "red")


