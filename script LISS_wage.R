setwd("~/projet-econometrie-ensps/")

###Importation des bases de données 
library(readr)
school <- read_delim("./data_LISS/work-and-school.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

background<-read_delim("data_LISS/avars_202207_EN_1.0p.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

###Fusion des bases de données

agregdata <- merge(background, school, by="nomem_encr" )

###Gestion des NA

#détection
nrow(agregdata)


agregdata <-  agregdata[!agregdata$cw22o005==28,]#suppression de la catégorie "autre"
nrow(agregdata)
sum(is.na(agregdata$cw22o005))
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
nrow(agregdata)
sum(agregdata$cw22o005==-9)
agregdata <-  agregdata[!agregdata$cw22o005==-9,]
hist(agregdata$cw22o005)
nrow(agregdata)
###Création d'une table avec les données pertinentes




data<-data.frame(agregdata$nomem_encr,agregdata$leeftijd,agregdata$geslacht,agregdata$brutoink,agregdata$cw22o127,agregdata$cw22o134,agregdata$cw22o439, agregdata$cw22o005, agregdata$aantalki, agregdata$cw22o139)
names(data)<-c("identite", "age", "genre", "revenu", "heures", "experience", "enfant","education", "nbenfants", "soiree")

nrow(data)
sum(is.na(data$education))
sum(data$soiree==5)


data <-  data[!data$revenu<=0,]
data <- data[!is.na(data$heures),]
data <- data[!is.na(data$experience),]
data <- data[!is.na(data$education),]
#data <- data[!is.na(data$soiree),]
#data <- data[!is.na(data$enfant),]
data <-  data[!data$experience==999,]
data <-  data[!data$education==-9,]
data <-  data[!data$heures==999,]
data <-  data[!data$genre==3,]

###Réecriture des variables : 

data$genre <- ifelse(data$genre==1, 0, 1)
#data$enfant <- ifelse(data$enfant==1, 1, 0)
data$log_revenu <- log(data$revenu)


data$experience <- 2022 - data$experience

#data$soiree <- ifelse(data$soiree>2, 1, 0)




lm1 <- lm(data$log_revenu ~ data$age + data$genre + data$heures + data$experience + data$nbenfants + data$education)
summary(lm1)

#lm7 <- lm(data$log_revenu ~ data$age + data$genre + data$heures + data$experience + data$nbenfants + data$education + data$soiree)
#summary(lm7)#où l'on prend en compte si les enquêtés travaillent régulièrement tard le soir (18-minuit) ou non

nrow(data)
sum(data$education==0)
hist(data$revenu)
sum(data$revenu<0)


#Vérification hétéroscédasticité :

data$uhat <- lm1$residuals
data$yhat <- lm1$fitted.value

library(ggplot2)
ggplot(data = data, mapping = aes(x = yhat, y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') +
  labs(y = 'Residuals', x = 'Fitted values')

data$residuals1<-lm1$residuals
data$residuals1
sum(data$residuals1)

data$ln_resi2<-data$residuals1*data$residuals1
ln_model_BP<-lm(ln_resi2 ~ age + genre + heures + experience + enfant + education, data = data)
summary(ln_model_BP) #la p-value étant très faible, on rejette l'hypothèse nulle d'homoscédasticité, donc on conclut à psce hétéroscédasticité


library(lmtest)
bptest(lm1, studentize=FALSE) #idem, juste pour vérifier


#méthode de white

modele <- lm(data$log_revenu ~ data$age + data$genre + data$heures + data$experience + data$nbenfants + data$education)
smodele <- summary(modele)
library(lmtest)
library(sandwich)

modele_robust <- coeftest(modele, vcov = vcovHC(modele, type = "HC1"))
modele_robust

library(lmtest)
library(car)
modele_robust2 <- coeftest(modele, vcov = hccm(modele, type = "hc1"))
modele_robust2
confint(modele_robust)

#méthode de white (méthode 2, paquet + récent)
library(estimatr)
modele_robust3 <- lm_robust(data=data,log_revenu ~ age + genre + heures + experience + nbenfants + education, se_type = "HC1")
summary(modele_robust3)

#méthode des moindres carrés généralisée
#modele <- lm(data$log_revenu ~ data$age + data$genre + data$heures + data$experience + data$nbenfants + data$education)
#data$logresid2 <- log(residuals(modele)^2) # on ajoute la variable ln(e^2)
#modeleresid <- lm(data=data,logresid2 ~ age + genre + heures + experience + nbenfants + education) # on les régresse sur les x
#data$e2chap <- exp(modeleresid$fitted.values) # on calcule la variance prédite
#modele_mcqg <- lm(data=data,log_revenu ~ age + genre + heures + experience + nbenfants + education,weight=1/e2chap) # on pondère la régression par 1/variance
#summary(modele_mcqg)


###On retient le modèle robuste :

modele <- modele_robust3
summary(modele)

data$uhat <- modele$res_var
data$yhat <- modele$fitted.values

ggplot(data = data, mapping = aes(x = yhat, y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') +
  labs(y = 'Residuals', x = 'Heures')



#Wald
data$enfant <- as.factor(data$nbenfants)
data$enfant <- as.numeric(data$nbenfants)
data$penfant <- ifelse(data$nbenfants>0, 1, 0)
summary(data$enfant)
summary(data$heures)
data$tpsfaible <- ifelse(data$heures<26, 1, 0)
data$tpsenfant <- data$tpsfaible*data$penfant
data$F_tps <- data$genre*data$tpsfaible
data$F_penfant <- data$penfant*data$genre
lm3 <- lm(data$log_revenu ~ data$genre + data$penfant + data$tpsfaible + data$F_tps + data$F_penfant + data$tpsenfant)
summary(lm3)
M <- matrix(c(0, 1, 0, 0, 1, 0, 0,
              0, 1, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE)
wald.test(b = coef(lm3), Sigma = vcov(lm3), L = M)
summary(lm3)

hist(data$enfant)

summary(data$education)

exp(8.22)

mean(data$revenu)


#Chow

library(ggplot2)


ggplot(data, aes(x = data$experience, y = data$log_revenu)) + geom_point(col='steelblue', size=3)

n_exp <- length(data$experience)
hist(data$experience)

library(strucchange)
sctest(data$experience ~ data$log_revenu, type="Chow", point=5)

dt1 <- data[data$age <=25,]
dt2 <- data[data$age>25,]

p_1 <-lm(dt1$log_revenu ~ dt1$experience, data = dt1)
p_1

p_2 <-lm(dt2$log_revenu ~ dt2$experience, data = dt2)
p_2

summary(p_1)
summary(p_2)

p <- lm(data$log_revenu ~ data$experience, data = data)

stat_de_test <- ((sum(p$residuals^2) - ( sum(p_1$residuals^2) + sum(p_2$residuals^2)))/2)/(( sum(p_1$residuals^2) + sum(p_2$residuals^2))/(1860 - 4))
stat_de_test



