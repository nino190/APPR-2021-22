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
nesrece_skupaj <- barplot(sestevek_nesrece)

graf_lj_mb <- ggplot() +
  layer(
    data = nesrece,
    mapping = aes(x = PU.LJUBLJANA, y = PU.MARIBOR),
    geom = "point",
    stat = "identity",
    position = "identity"
  ) + 
  scale_y_continuous() +
  scale_x_continuous() +
  coord_cartesian() +
  ggtitle("Primerjava prometnih nesreč na območju PU Ljubljana in PU Maribor") + xlab("število nesreč na območju PU Ljubljana") + ylab("Število nesreč na območju PU Maribor")

graf_tovor_cesta <- ggplot() +
  layer(
    data = vzrok,
    mapping = aes(x = NEUPOSTEVANJE.PRAVIL.O.PREDNOSTI, y = NEPRAVILNOST.NA.CESTI),
    geom = "line",
    stat = "identity",
    position = "identity"
  ) + 
  scale_y_continuous() +
  scale_x_continuous() +
  coord_cartesian() +
  ggtitle("Primerjava prometnih nesreč z vzrokom neupoštevanja pravil o prednosti in nepravilnosti na cesti") + xlab("število nesreč kot posledica neupoštevanja pravil o prednosti") + ylab("število nesreč kot posledica nepravilnosti na cesti")

