# 4. faza
source("vizualizacija/vizualizacija.r")

prileganje <- lm(NESRECE.SKUPAJ ~ LETO, data = skupek)
predict(prileganje, data.frame(leto = seq(2005, 2020, 1)))

