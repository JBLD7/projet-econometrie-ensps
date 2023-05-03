###Essai avec la base de données de l'ESS
library(readr)
ESS7e02_2 <- read_csv("./data_ESS/ESS7e02_2.csv")
#Si la ligne ci-dessus ne fonctionne pas, utiliser la commande : 
#setwd("le-chemin-du-fichier-"projet-econometrie-ensps"-sur-votre-ordinateur")
#puis réessayer les deux lignes de code !

data <-  ESS7e02_2
mean(na.omit(data$hinctnta))