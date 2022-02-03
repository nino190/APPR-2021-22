# 4. faza
source("vizualizacija/vizualizacija.r")

vse <- rep(0, 40)

for (i in length(nesrece_leta)){

    vse[ncol(vse) + 1] <<- sum(nesrece_leta[i])

}