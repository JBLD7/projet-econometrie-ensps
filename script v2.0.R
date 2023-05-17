
## Import des bases de données 

setwd("~/projet-econometrie-ensps/")
library(readr)
library(ggplot2)
school <- read_delim("./data_LISS/work-and-school.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
background<-read_delim("data_LISS/avars_202207_EN_1.0p.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Fusion des bases de données

agregdata <- merge(background, school, by="nomem_encr" )

###Gestion des NA

#détection
nrow(agregdata)



agregdata <-  agregdata[!agregdata$oplzon==7,]#suppression de la catégorie "autre"

nrow(agregdata)
sum(is.na(agregdata$oplzon))
sum(agregdata$oplzon==8)
#aucun na à priori

###Pré-traitement des données et réécriture des variables (a.e = années d'études)

#On s'appuie d'abord sur la variable oplzon (niveau d'études "with diploma")
agregdata$educ <- agregdata$oplmet*100
agregdata$educ <- ifelse(agregdata$educ==800|agregdata$educ==900, 0,
                         #pas d'études du tout : "Not yet completed any education" or "Not (yet) started any education"
                         ifelse(agregdata$educ==100, 8,
                                #primary de 4 à 12 ans => 8 ans d'études : primary school
                                ifelse(agregdata$educ==200, 12,
                                       #1st option : VMBO (4 ans, de 12 à 16) => 8+4=12 a.e. : "vmbo (intermediate secondary education, US: junior high school)"
                                       ifelse(agregdata$educ==300, 13.5,
                                              #2nd option : HAVO (5 ans, de 12 à 17) or VWO (6 ans, de 12 à 18) => 8+5,5 = 13.5 a.e. : "havo/vwo (higher secondary education/preparatory university education, US: senior high school)"
                                              ifelse(agregdata$educ==400, 15.25,
                                                     #1st option : MBO (entre 1 et 4 ans, après VMBO, HAVO ou VWO) => 12.75 + 2.5 = 15.25 a.e. : "mbo (intermediate vocational education, US: junior college)"
                                                     ifelse(agregdata$educ==500, 16.75,
                                                            #2nd option : HBO (4 ans, après VMBO, HAVO ou VWO) => 12.75+4=16.75 a.e. : "hbo (higher vocational education, US: college)"
                                                            ifelse(agregdata$educ==600, 17, -9)))))))
#3rd option : WO (3 ans, après 1ere année HBO (13.75 a.e.) ou après VWO (14 a.e.), licence) => 14+3=17 a.e. : "wo (university)"
nrow(agregdata)
sum(agregdata$educ==-9)
agregdata <-  agregdata[!agregdata$educ==-9,]
hist(agregdata$educ)
nrow(agregdata)


#Puis, pour distinguer entre licence, master et doctorat, on s'appuie sur la variable cw22o005 :
sum(agregdata$cw22o005==27) #nombre de titulaires d'un PhD
sum(agregdata$cw22o005==26) #nombre de titulaires d'un master (au maximum)
sum(agregdata$cw22o005==25) #nombre de titulaires d'une licence (au maximum)
sum(agregdata$cw22o005==27)+sum(agregdata$cw22o005==26)+sum(agregdata$cw22o005==25) #nombre de diplômés du supérieur (au moins licence), selon cw22o005
sum(agregdata$educ==18)
sum((agregdata$cw22o005==27|agregdata$cw22o005==26|agregdata$cw22o005==25)&agregdata$educ!=17)#nombre d'observations "bizarres"
#On considère que les réponses à la question cw22o005 sont plus fiables car plus précises.

ggplot(agregdata, aes(x=factor(agregdata$educ)))+
  geom_bar(fill="steelblue")+
  labs(title="Niveau d'éducation (avec diplôme) des individus de l'échantillon", 
       x="Nombre d'années de scolarité et d'études (depuis le début de l'école primaire (4 ans))", y = "Effectifs")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

#On ajoute les études supérieures à educ :

agregdata$educ <- ifelse(agregdata$cw22o005==25, 17,
                         #niveau licence => 18 ans d'études
                         ifelse(agregdata$cw22o005==26, 19,
                                #niveau master, le master dure 1, 2 ou 3 ans => 17 + 2 = 19 a.e.
                                ifelse(agregdata$cw22o005==27, 22.5, agregdata$educ))) #après le master, un doctorat dure 3 à 4 ans => 19 + 3.5 = 22.5 a.e.


ggplot(agregdata, aes(x=factor(agregdata$educ)))+
  geom_bar(fill="steelblue")+
  labs(title="Niveau d'éducation (avec diplôme) des individus de l'échantillon", 
       x="Nombre d'années de scolarité et d'études (depuis le début de l'école primaire (4 ans))", y = "Effectifs")+
  geom_text(stat='count', aes(label=after_stat(count)), vjust=1.6, color="white", size=3.5)+
  theme_minimal()



###Création d'une table avec les données pertinentes

data<-data.frame(agregdata$nomem_encr,agregdata$leeftijd,agregdata$geslacht,agregdata$brutoink,agregdata$cw22o127,agregdata$cw22o134, agregdata$educ, agregdata$aantalki)
names(data)<-c("identite", "age", "genre", "revenu", "heures", "anciennete","education", "nbenfants")

nrow(data)
sum(is.na(data$nbenfants))



data <-  data[!data$revenu<=0,]
data <- data[!is.na(data$heures),]
data <- data[!is.na(data$anciennete),]
data <- data[!is.na(data$education),]
data <-  data[!data$anciennete==999,]
data <-  data[!data$education==-9,]
data <-  data[!data$heures==999,]
data <-  data[!data$genre==3,]

###Réecriture des variables : 

data$genre <- ifelse(data$genre==1, 0, 1)
data$log_revenu <- log(data$revenu)


data$anciennete <- 2022 - data$anciennete


lm1 <- lm(data$log_revenu ~ data$age + data$genre + data$heures + data$anciennete + data$nbenfants + data$education)
summary(lm1)




nrow(data)
sum(data$education==0)
hist(data$revenu)
sum(data$revenu<0)


#Vérification hétéroscédasticité :

data$uhat <- lm1$residuals
data$yhat <- lm1$fitted.values

library(ggplot2)
ggplot(data = data, mapping = aes(x = yhat, y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') +
  labs(y = 'Residuals', x = 'Fitted values')

plot(data$uhat, data$yhat)

library(ggplot2)
ggplot(data = data, mapping = aes(x = education, y = uhat)) +
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') +
  labs(y = 'Residuals', x = 'éducation')
#La variance des résidus semble augmenter avec la variable education, ce qui peut s'expliquer
#par le fait que les individus ayant + d'années d'études ont accès à des postes plus variés
#donc on s'attend à ce que la variation du salaire soit plus forte pour les individus
#ayant un niveau plus important d'éducation

library(ggplot2)
ggplot(data = data, mapping = aes(x = heures, y = uhat)) +
  theme_minimal() +
  geom_point(col='steelblue') +
  geom_hline(yintercept = 0, col = 'red') +
  labs(y = 'Residuals', x = 'heures')
#variance des résidus très élevée pour variable heures faible
#explication ?? Individus travaillant peu peuvent avoir autres
#sources de revenu ?

plot(data$log_revenu,resid(lm1))

plot(lm1)

data$residuals1<-lm1$residuals
data$residuals1
sum(data$residuals1)

#On réalise d'abord le test de Breusch-Pagan en regardant si le modèle est globalement significatif avec un test de Fisher
data$ln_resi2<-lm1$residuals^2
ln_model_BP<-lm(ln_resi2 ~ anciennete + genre + heures + age + nbenfants + education, data = data)
summary(ln_model_BP) #la p-value étant très faible, on rejette l'hypothèse nulle d'homoscédasticité, donc on conclut à psce hétéroscédasticité

#Puis avec un test du rapport de vraisemblance (d'abord méthode auto, puis méthode manuelle)
library(lmtest)
bptest(lm1) #idem, juste pour vérifier, même si les résultats différents (ici le test est basé sur student)

k <-  6
r2 <- summary(ln_model_BP)$r.squared # R-squared
n <- nobs(ln_model_BP) # number of observations
LM_stat <- n * r2 # LM-statistic
LM_pval <- 1 - pchisq(q = LM_stat, df = k) # p-value
LM_pval#mêmes résultats qu'avec méthode auto !!

###Méthode de white

#Méthode manuelle

data$anciennete2 <- data$anciennete*data$anciennete
data$age2 <- data$age*data$age
data$heures2 <- data$heures*data$heures
data$genre2 <- data$genre^2
data$education2 <-  data$education^2
data$nbenfants2 <-  data$nbenfants^2


data$var_crois_1 <- data$anciennete*data$age
data$var_crois_2 <- data$anciennete*data$genre
data$var_crois_3 <- data$anciennete*data$heures
data$var_crois_4 <- data$anciennete*data$nbenfants
data$var_crois_5 <- data$anciennete*data$education
data$var_crois_6 <- data$genre*data$heures
data$var_crois_7 <- data$genre*data$age
data$var_crois_8 <- data$genre*data$nbenfants
data$var_crois_9 <- data$genre*data$education
data$var_crois_10 <- data$heures*data$age
data$var_crois_11 <- data$heures*data$nbenfants
data$var_crois_12 <- data$heures*data$education
data$var_crois_13 <- data$age*data$nbenfants
data$var_crois_14 <- data$age*data$education
data$var_crois_15 <- data$nbenfants*data$education

# Regression pour le test de White 
model_White <- lm(ln_resi2 ~ anciennete + genre + heures + age + nbenfants + education +
                    var_crois_1 + var_crois_2 + var_crois_3 + var_crois_4 + var_crois_5 + var_crois_6 +
                    var_crois_7 + var_crois_8 + var_crois_9 + var_crois_10 + var_crois_11 + var_crois_12+
                    var_crois_13 + var_crois_14 + var_crois_15 + anciennete2 + education2 + nbenfants2 +
                    genre2 + heures2 + age2, 
                  data)
summary(model_White)
#p value très faible, mais la statistique l'est aussi, il y a normalement 28 degrés de liberté donc on ne rejette pas l'hypotèse nulle 

modele <- lm(data$log_revenu ~ data$age + data$genre + data$heures + data$anciennete + data$nbenfants + data$education)
smodele <- summary(modele)
library(lmtest)
library(sandwich)

modele_robust <- coeftest(modele, vcov = vcovHC(modele, type = "HC1"))
modele_robust
summary(modele_robust)

library(lmtest)
library(car)
modele_robust2 <- coeftest(modele, vcov = hccm(modele, type = "hc1"))
modele_robust2
confint(modele_robust)

#méthode de white (méthode 2, paquet + récent)
library(estimatr)
library(dplyr)
modele_robust3 <- lm_robust(data=data,log_revenu ~ age + genre + heures + anciennete + nbenfants + education, se_type = "HC1")
summary(modele_robust3)
model <-  modele_robust3



#méthode des moindres carrés généralisée
#modele <- lm(data$log_revenu ~ data$age + data$genre + data$heures + data$anciennete + data$nbenfants + data$education)
#data$logresid2 <- log(residuals(modele)^2) # on ajoute la variable ln(e^2)
#modeleresid <- lm(data=data,logresid2 ~ age + genre + heures + anciennete + nbenfants + education) # on les régresse sur les x
#data$e2chap <- exp(modeleresid$fitted.values) # on calcule la variance prédite
#modele_mcqg <- lm(data=data,log_revenu ~ age + genre + heures + anciennete + nbenfants + education,weight=1/e2chap) # on pondère la régression par 1/variance
#summary(modele_mcqg)


###On retient le modèle robuste :

modele <- modele_robust3
summary(modele)

data$uhat <- modele$res_var
data$yhat <- modele$fitted.values

ggplot(data = data, mapping = aes(x = yhat, y = uhat)) +
  theme_minimal(base_size = 22) +
  geom_point(col = 'steelblue') +
  geom_hline(yintercept = 0, col = 'steelblue') +
  labs(y = 'Residus', x = 'Logarithme du revenu brut')

sum(modele$residuals) #On a bien une somme des résidus nulle

#Wald - changements
library(lmtest)
library(aod)
data$nbenfants <- as.numeric(data$nbenfants)
data$penfant <- ifelse(data$nbenfants>0, 0, 1)
data$tpsfaible <- ifelse(data$heures<26, 1, 0)
data$tpsenfant <- data$tpsfaible*(1-data$penfant)
data$F_tps <- data$genre*data$tpsfaible
data$F_penfant <- (1-data$penfant)*data$genre
lm3 <- lm(data$log_revenu ~ data$genre + data$penfant + data$tpsfaible + data$F_tps + data$F_penfant + data$tpsenfant)
summary(lm3)
#Sur la régression on observe un effet important des variables croisées (toutes jouant négativement) : renforcement des effets lorsque 
#l'on appartient à plusieurs catégories (ex : femme avec enfants en bas âge ou femme à tps partiel)

#On commence par tester l'hypothèse composite : "au moins une des deux variables croisées liées au genre est significative"
M <- matrix(c(0, 0, 0, 0, 1, 0, 0,
              0, 0, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE)
wald.test(b = coef(lm3), Sigma = vcov(lm3), L = M)
#La p-value étant très faible, on peut en conclure que l'un des variables au moins est significative 

#On veut maintenant tester si l'effet d'avoir un enfant (au moins) est le même pour les hommes et les femmes
M <- matrix(c(0, 0, 1, 0, 1, 1, 0,
              0, 0, 1, 1, 0, 0, 0), nrow = 2, byrow = TRUE)
wald.test(b = coef(lm3), Sigma = vcov(lm3), L = M)
#La p-value étant très faible, il y a une différence significative, donc on rejette H_0 : l'effet n'est pas nul


hist(data$nbenfants)

summary(data$education)

exp(8.22)

mean(data$revenu)

#Chow méthode manuelle

dt1 <- data[data$education <= 16,]
dt2 <- data[data$education>16,]
nrow(dt1)
summary(dt1$education)
summary(dt2$education)

p_1 <-lm(log_revenu ~ age + genre + heures + anciennete + nbenfants + education, data = dt1)
p_1

p_2 <-lm(log_revenu ~ age + genre + heures + anciennete + nbenfants + education, data = dt2)
p_2

summary(p_1)
summary(p_2)

p <- lm(log_revenu ~ age + genre + heures + anciennete + nbenfants + education, data = data)

stat_de_test <- ((sum(p$residuals^2) - ( sum(p_1$residuals^2) + sum(p_2$residuals^2)))/7)/(( sum(p_1$residuals^2) + sum(p_2$residuals^2))/(nrow(data) - 14))
stat_de_test

ggplot(data, aes(x = data$education, y = p$fitted.values)) + geom_point(col='steelblue', size=3)

sum(data$education<16)


#Chow méthode automatique (vérification)


library(strucchange)
sctest(log_revenu ~ age + genre + heures + anciennete + nbenfants + education, type="Chow", point=sum(data$education<16), data=data[order(data$education),])

nrow(data)


##Tests d'endogénéité


#Relation entre l'âge (variable instrumentale) et le revenu 
library(ggplot2)
ggplot(data, aes(x = age, y = log_revenu)) + 
  geom_smooth(method = "lm", col="darkred") +
  geom_point(col="steelblue", size=3)

#Relation entre l'âge (variable instrumentale) et l'expéience  (variable endogène)
library(ggplot2)
ggplot(data, aes(x = age, y = anciennete)) + 
  geom_smooth(method = "lm", col="darkred") +
  geom_point(col="steelblue", size=3)

lm3 <- lm(anciennete ~ age, data = data)

data$pred1 <- lm3$fitted.values

summary(lm3) 
#l'âge a un effet positif (bien qu'assez faiblement significatif) sur l'expérience

lm4 <- lm(anciennete ~ age + genre + heures + nbenfants + education, data = data)

data$pred2 <- lm4$fitted.values

summary(lm4)

#l'âge a un effet positif (bien qu'assez faiblement significatif) sur l'expérience
#La variable instrumentale "age" semble donc pertinente

summary(lm(log_revenu ~ pred1, data = data))


summary(lm(log_revenu ~ pred2 + genre + heures + nbenfants + education, data = data))

# Dans les deux cas, l'effet causal de l'expérience 
#  est plus important que l'effet
# estimé avec une régression naïve.
# Que l'on introduise ou non d'autres variables 
# de contrôle, l'effet de l'augmentation 
# d'un an de l'expérience d'un individu sur
# le log du revenu est réel 
# (+1,3 % environ pour la première spécification,
# +2 % dans la seconde)
#tandis que cet effet était de 0,3% dans la régression initiale (lm1)

library(AER)

summary(ivreg(log_revenu ~ anciennete | age, data = data))

modele_iv <- ivreg(log_revenu ~ education + genre + heures + nbenfants |anciennete|age, data = data) 
summary(modele_iv, diagnostics = TRUE)






library(strucchange)
sctest(log_revenu ~ age + genre + heures + anciennete + nbenfants + education, type="Chow", point=sum(data$education<16), data=data[order(data$education),])

lm1 <-  lm(log_revenu ~ age + genre + heures + anciennete + nbenfants + education, data[data$education<16,])
summary(lm1)
lm2 <-  lm(log_revenu ~ age + genre + heures + anciennete + nbenfants + education, data[data$education>16,])
summary(lm2)
library(xtable)

library(ggplot2)
ggplot(data[data$education<16,], aes(x = nbenfants, y = anciennete)) + 
  geom_smooth(method = "lm", col="darkred") +
  geom_point(col="steelblue", size=3)

library(ggplot2)
ggplot(data[data$education>16,], aes(x = nbenfants, y = anciennete)) + 
  geom_smooth(method = "lm", col="darkred") +
  geom_point(col="steelblue", size=3)

library(ggplot2)
ggplot(data = data, mapping = aes(x = education, y = uhat)) +
  theme_minimal(base_size = 32) +
  geom_point(col='steelblue') +
  geom_hline(yintercept = 0, col = 'red') +
  labs(y = 'Residuals', x = 'éducation')
#La variance des résidus semble augmenter avec la variable education, ce qui peut s'expliquer
#par le fait que les individus ayant + d'années d'études ont accès à des postes plus variés
#donc on s'attend à ce que la variation du salaire soit plus forte pour les individus
#ayant un niveau plus important d'éducation


library(ggplot2)
ggplot(data = data, mapping = aes(x = heures, y = uhat)) +
  theme_minimal(base_size = 32) +
  geom_point(col='steelblue') +
  geom_qq(aes(sample=data$uhat))+
  labs(y = 'Residuals', x = 'heures')
#variance des résidus très élevée pour variable heures faible
#explication ?? Individus travaillant peu peuvent avoir autres
#sources de revenu ?


