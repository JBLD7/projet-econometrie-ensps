###Essai avec la base de données de l'ESS
library(readr)
ESS7e02_2 <- read_csv("~/projet-econometrie-ensps/data_ESS/ESS7e02_2.csv")

data <-  ESS7e02_2
spec(data)

which(is.na(data$hinctnta))



unique(base[which(is.na(base$Taux)), c("An", "Pays")])