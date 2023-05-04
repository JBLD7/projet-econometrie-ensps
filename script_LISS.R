setwd("~/projet-econometrie-ensps/")

###Importation des bases de données 
library(readr)
income <- read_delim("./data_LISS/ci22o_EN_1.0p.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

background<-read_delim("data_LISS/avars_202207_EN_1.0p.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

###Fusion des bases de données

agregdata <- merge(background, income, by="nomem_encr" )

###Gestion des NA
nrow(income)
nrow(background)
nrow(agregdata)

#Quelques vérifications sur la présence des NA
sum(agregdata$netinc==-13)
sum(is.na(agregdata$ci22o380))
sum(agregdata$ci22o380==-9)
sum(is.na(agregdata$nomem_encr))
sum(is.na(agregdata$ci22o002))
sum(is.na(agregdata$ci22o236))
sum(agregdata$ci22o236<0)
sum(is.na(agregdata$partner))


#Suppression des NA
agregdata <-  agregdata[!agregdata$netinc==-13,]
agregdata <- agregdata[!is.na(agregdata$ci22o380),]
agregdata <-  agregdata[!agregdata$ci22o380==-9,]
agregdata <-  agregdata[!is.na(agregdata$ci22o002),]
agregdata <-  agregdata[!is.na(agregdata$ci22o236),]
agregdata <-  agregdata[!agregdata$ci22o236==-8,,]

#taille du data frame (nbre observations)

### Création de nouvelles variables agrégées

agregdata$abovemin <- ifelse(agregdata$netinc>=agregdata$ci22o236, 1, 0)

data<-data.frame(agregdata$nomem_encr, agregdata$ci22o380,agregdata$ci22o002, agregdata$netinc,agregdata$abovemin, agregdata$partner)
names(data)<-c("identifiant", "happiness", "age", "income", "abovemin", "partner")

lm1 <- lm(data$happiness ~ data$age + data$income + data$abovemin + data$partner)
summary(lm1)

###Tests d'hétéroscédasticité

data$uhat <- lm1$residuals
data$yhat <- lm1$fitted.value

library(ggplot2)
ggplot(data = data, mapping = aes(x = yhat, y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') +
  labs(y = 'Residuals', x = 'Fitted values')

data$res2 <- data$uhat^2






#Pot pourri

assets<-read.csv("./data_LISS/ca22h_EN_1.0p.csv",header = TRUE, sep = ";")
housing<-cd22o_EN_1.0p
familyhouse<-cf22o_EN_1.0p
health<-ch22o_EN_1.0p
income<-ci22o_EN_1.0p

agregdata <- merge(background, )

dt<-merge(background, assets, by="nomem_encr")
dt1<-merge(dt, housing, by="nomem_encr")
dt2<-merge(familyhouse, dt1, by = "nomem_encr")
dt3<-merge(dt2, health, by="nomem_encr")
dt4<-merge(dt3,income, by = "nomem_encr")

data<-data.frame(dt4$nomem_encr,dt4$ci22o380,dt4$cf22o251,dt4$netinc,dt4$ca22h055,dt4$cd22o034,dt4$ch22o264)
View(data)
