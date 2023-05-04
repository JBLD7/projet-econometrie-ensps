setwd("~/projet-econometrie-ensps/")

###Importation des bases de données 
library(readr)
school <- read_delim("./data_LISS/work-and-school.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

background<-read_delim("data_LISS/avars_202207_EN_1.0p.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

###Fusion des bases de données

agregdata <- merge(background, school, by="nomem_encr" )

###Gestion des NA

#détection
sum(is.na(agregdata$cw22o005))
sum(agregdata$cw22o005==28)

agregdata <-  agregdata[!agregdata$cw22o005==28,]#suppression de la catégorie "autre"
#aucun na à priori

###Pré-traitement des données et réécriture des variables

agregdata$cw22o005 <- agregdata$cw22o005*100
agregdata$cw22o005 <- ifelse(agregdata$cw22o005==100|agregdata$cw22o005==200, 0,#pas d'études du tout
                             ifelse(agregdata$cw22o005==300, 8,#elementary de 4 à 12 ans => 8 ans d'études
                                    ifelse(agregdata$cw22o005==400|agregdata$cw22o005==500|agregdata$cw22o005==600|agregdata$cw22o005==700, 10,#1st option : middle school (4 ans) => 8+4=12
                                           ifelse(agregdata$cw22o005==800|agregdata$cw22o005==900|agregdata$cw22o005==1000|agregdata$cw22o005==1100, 12,#2nd option : secondary (5-6 ans) => 8+6=14
                                                  ifelse(agregdata$cw22o005==1200|agregdata$cw22o005==1400|agregdata$cw22o005==1500, 14,#1st option : post-secondary, non tertiary (1-3 ans) => 14+2=16 ans
                                                         ifelse(agregdata$cw22o005==1300, 13,#2nd option = tertiary (4 ans) => 14+4=18 ans
                                                                ifelse(agregdata$cw22o005==1600|agregdata$cw22o005==1700|agregdata$cw22o005==1800, 16, 
                                                                       ifelse(agregdata$cw22o005==1900|agregdata$cw22o005==2000|agregdata$cw22o005==2100|agregdata$cw22o005==2200|agregdata$cw22o005==2300|agregdata$cw22o005==2400|agregdata$cw22o005==2500, 17, 
                                                                              ifelse(agregdata$cw22o005==2600, 19,
                                                                                     ifelse(agregdata$cw22o005==2600, 22,-9))))))))))#post-tertiary (3 ans mais c'est aribitraire) => 18+3=21 ans 


sum(agregdata$cw22o005==400)
hist(agregdata$cw22o008)

###Création d'une table avec les données pertinentes




data<-data.frame(agregdata$nomem_encr,agregdata$leeftijd,agregdata$geslacht,agregdata$netinc,agregdata$cw22o127,agregdata$cw22o134,agregdata$cw22o439, agregdata$cw22o008)
names(data)<-c("identifiant", "happiness", "age", "income", "abovemin", "partner")



