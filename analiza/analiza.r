# 4. faza
source("vizualizacija/vizualizacija.r")

nesrece_leta <- uvoz_nesrec_leta()
mes <- data.frame(MESECI = c("januar", "februar", "marec", "april", "maj", "junij", "julij", "avgust", "september", "oktober", "november", "december"))
nesrece_leta <- cbind(mes, nesrece_leta)
nesrece_leta <- adorn_totals(nesrece_leta, "row")
rownames(nesrece_leta) <- as.vector(as.character(1:13))


skupek$LETO <- as.character(skupek$LETO)
skupek$NESRECE.SKUPAJ <- as.character(skupek$NESRECE.SKUPAJ)

skupek <- skupek %>% mutate(LETO = parse_integer(LETO), NESRECE.SKUPAJ = parse_integer(NESRECE.SKUPAJ))
prileganje <- lm(NESRECE.SKUPAJ ~ LETO, data = skupek)
predict(prileganje, data.frame(LETO = seq(2005, 2025, 1)))

graf_predict <- ggplot(skupek, aes(x=LETO, y=NESRECE.SKUPAJ)) + geom_point() + 
  geom_smooth(method='lm', formula=y ~ poly(x,2,raw=TRUE), fullrange=TRUE, color='red') + scale_x_continuous('leto', breaks = seq(2005, 2025, 1), limits = c(2005,2025))+ xlab('Leto') + ylab('Število nesreč')

sestevek_nesrec_2 <- data.frame(years = c(as.character(2005:2014)), sestevek = c(nesrece_leta[13, "skupaj2005"], nesrece_leta[13, "skupaj2006"], nesrece_leta[13, "skupaj2007"], nesrece_leta[13, "skupaj2008"], nesrece_leta[13, "skupaj2009"], nesrece_leta[13, "skupaj2010"], nesrece_leta[13, "skupaj2011"], nesrece_leta[13, "skupaj2012"], nesrece_leta[13, "skupaj2013"], nesrece_leta[13, "skupaj2014"]))
tocke <- data.frame(datum = c(2015:2020), stevilo = c(skupek[11, "NESRECE.SKUPAJ"], skupek[12, "NESRECE.SKUPAJ"], skupek[13, "NESRECE.SKUPAJ"], skupek[14, "NESRECE.SKUPAJ"], skupek[15, "NESRECE.SKUPAJ"], skupek[16, "NESRECE.SKUPAJ"]))

sestevek_nesrec_2$years <- as.character(sestevek_nesrec_2$years)
sestevek_nesrec_2$sestevek <- as.character(sestevek_nesrec_2$sestevek)

sestevek_nesrec_2 <- sestevek_nesrec_2 %>% mutate(years = parse_integer(years), sestevek = parse_double(sestevek))
prileganje2 <- lm(sestevek ~ years, data = sestevek_nesrec_2)
predict(prileganje2, data.frame(years = seq(2005, 2020, 1)))

graf_predict2 <- ggplot(sestevek_nesrec_2, aes(x=years, y=sestevek)) + geom_point() + 
  geom_smooth(method='lm', formula=y ~ poly(x,2,raw=TRUE), fullrange=TRUE, color='blue') + scale_x_continuous('leto', breaks = seq(2005, 2020, 1), limits = c(2005,2020))+ xlab('Leto') + ylab('Število nesreč') + 
  geom_point(data = tocke, mapping = aes(x = datum, y = stevilo), color = "red")
