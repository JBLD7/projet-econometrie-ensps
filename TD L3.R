############
### TD 1 ###
############

### Ex. 1  (Wald) ###

{
  
  # Q1 

  data(STAR, package = "AER")
  
  # Q2 
  
STAR$female <- ifelse(STAR$gender=="female", 1, 0)
STAR$not_white <- ifelse(STAR$ethnicity=="cauc", 0, 1)
STAR$lower <- ifelse(STAR$lunch1=="free", 1, 0)

# Q3

STAR$lower_female <- STAR$female*STAR$lower
STAR$not_white_female <- STAR$female*STAR$not_white
STAR$not_white_lower <- STAR$female*STAR$not_white

# Q4 

STAR$math1  <- (STAR$math1 - mean(STAR$math1, na.rm = TRUE))/sd(STAR$math1,
                                                                na.rm = TRUE)

# Q5

lm1 <- lm(math1 ~ female + not_white + lower +
            lower_female + not_white_lower + not_white_female,
          data = STAR)

# Q6 

L <- matrix(c(0, 0, 0, 0, 1, 0, 0,
              0, 0, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE)

library(aod)
wald.test(b = coef(lm1), Sigma = vcov(lm1), L = L)

# Q7

# a

L <- matrix(c(0, 1, 0, 0, 1, 0, 0,
              0, 1, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE)

library(aod)
wald.test(b = coef(lm1), Sigma = vcov(lm1), L = L)

# b

L <- matrix(c(0, 1, 0, 0, 0, 0, 0,
              0, 1, 0, 0, 1, 0, 0,
              0, 1, 0, 0, 0, 1, 0), nrow = 3, byrow = TRUE)

library(aod)
wald.test(b = coef(lm1), Sigma = vcov(lm1), L = L)

# c

L <- matrix(c(0, 0, 1, -1, 0, 0, 0), nrow = 1, byrow = TRUE)

library(aod)
wald.test(b = coef(lm1), Sigma = vcov(lm1), L = L)

# d

L <- matrix(c(0, 0, 0, 1, 1, 0, 0), nrow = 1, byrow = TRUE)

library(aod)
wald.test(b = coef(lm1), Sigma = vcov(lm1), L = L)

}


### Ex. 2 (Chow) ###

{
  # Q1 
  
  library(dplyr)
  data<-data.frame(EuStockMarkets)
  
  # Q2
  
  library(ggplot2)
  ggplot(data, aes(x = DAX, y = CAC)) + geom_point(col="steelblue", size=3)

  # Q3
  
  n_obs <- length(data$DAX)
  data$time <- 1991 + c(1:n_obs)/265
  
  ### NB : 265 est le nombre de jours ouvrés. L'on change donc
  ### d'année après 265 observations (en réalité il y a 265 jours ouvrés
  ### seulement en moyenne, mais c'est une approximation 
  ### satisfaisante ici).
  
  # Q4
  
  library(strucchange)
  sctest(data$CAC ~ data$DAX, type = "Chow", point = 1000)
  
  # Q5
  
  data$id <- c(1:n_obs)
  
  dt1 <- data[data$id < 1001, ]
  dt2 <- data[data$id > 1000, ]
  
  
  lmT <- lm(CAC ~ DAX, data = data)
  lm1 <- lm(CAC ~ DAX, data=dt1)
  lm2 <- lm(CAC ~ DAX, data=dt2)
  
  SCR_T <- sum(lmT$residuals^2)
  SCR_1 <- sum(lm1$residuals^2)
  SCR_2 <- sum(lm2$residuals^2)
  
  k <- 2
  N <- n_obs
  
  numerator <- (SCR_T -(SCR_1+SCR_2))/k
  denominator <- (SCR_1 + SCR_2)/(N - 2*k)
  
  F_stat_chow <- numerator/denominator
  
  val_critique <- qf(0.95, k, n_obs - k)
  
  # F >> valeur critique du test.
  
  p_value_chow <- pf(F_stat_chow, k, n_obs - k)
  
  
  
  
  
}

############
### TD 2 ###
############

{

### Partie I : Rappels :

# Question 1 :

library(readr)
hprice1 <- read_csv("hprice1.csv")

# Question 2 :
# Regression model for price
model_0 <- lm(price ~ lotsize + sqrft + bdrms, hprice1)
summary(model_0)

# La taille du logement et la taille du terrain augmente
# significativement le prix du logement.
# L'augmentation d'un squared foot de la surface du logement
# augmente son prix de 120 dollars (unit?s exprim?s en milliers)
# L'augmentation d'un squared foot de la surface du terrain
# augmente son prix de 2 dollars.
# L'effet du nombre de chambres n'est pas signficatif.

### Question 3 :
F_stat <- (0.67/(1 - 0.67))*((88 - 4)/3)
F_pv <- 1 - pf(F_stat, 3, 84)

t_stat <- 0.0002068/0.00006421
t_pv <- 2*(1 - pt(t_stat, 84))

# Question 4
# En passant les unit?s en log-log
# Le log des variables est d?j? pr?sent dans la base 
# sous le nom l + nom de la variable.
model_1 <- lm(lprice ~ llotsize + sqrft + bdrms, hprice1)
summary(model_1)

### interpr?tation en termes d'?l?stacit? :

# Une augmentation d'un pourcent de la taille du
# terrain implique une augmentation de 0.1 % du prix.


### On revient ? la sp?cification intiale. 
### Question 5 :
hprice1$uhat <- model_0$residuals
hprice1$yhat <- model_0$fitted.values

### Question 6 :

library(ggplot2)
ggplot(data = hprice1, mapping = aes(x = yhat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Residuals', x = 'FItted values')

library(ggplot2)
ggplot(data = hprice1, mapping = aes(x = lprice, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Residuals', x = 'FItted values')

### On peut craindre un ph?nom?ne d'h?t?rosc?dasticit? 

### Partie 2 : Tests d'h?t?rosc?dasticit?

# Question 1 : Regression for Breusch-Pagan test
hprice1$resi2 <- hprice1$residuals*hprice1$residuals
model_BP <- lm(resi2 ~ lotsize + sqrft + bdrms, hprice1)
summary(model_BP)

# Question 2 

# On enregistre le nombre de variables ind?pendantes
k <- 3

# Si l'on teste la significativit? globale avec un test de Fisher :
r2 <- summary(model_BP)$r.squared # R-squared
n <- nobs(model_BP) # number of observations

F_stat <- (r2/k) / ((1-r2)/(n-k-1))  # F-statistic
F_pval <- 1 - pf(F_stat, k, n-k-1)  # p-value

### Note : l'on calcule ici ces statistiques pour faire un rappel,
### ces informations sont d?j? dans "summary"

# Question 3
# M?me chose avec un test du rapport de vraisemblance :
LM_stat <- n * r2 # LM-statistic
LM_pval <- 1 - pchisq(q = LM_stat, df = k1) # p-value

### On rejette l'hypoth?se d'homos?dasticit? dans les deux cas.

# Question 4 : test de White 

hprice1$lotsize2 <- hprice1$lotsize*hprice1$lotsize
hprice1$sqrft2 <- hprice1$sqrft*hprice1$sqrft
hprice1$bdrms2 <- hprice1$bdrms*hprice1$bdrms

hprice1$var_crois_1 <- hprice1$lotsize*hprice1$sqrft
hprice1$var_crois_2 <- hprice1$sqrft*hprice1$bdrms
hprice1$var_crois_3 <- hprice1$bdrms*hprice1$lotsize


# Regression pour le test de White 
model_White <- lm(resi2 ~ lotsize + sqrft + bdrms + 
                    lotsize2 + sqrft2 + bdrms2 +
                    var_crois_1 + var_crois_2 + var_crois_3, 
                  hprice1)
summary(model_White)

### L'on a toujours quelque chose de tr?s significatif, ce qui
### n'est pas tr?s ?tonnant puisque l'on a simplement ajout? des variables

# Partie 3 : Question 2

# Estimation du mod?le pour le poids weight=1/sqrft (donn? dans le cours)
model_WLS1 <- lm(formula = price ~ lotsize + sqrft + bdrms, 
                 data = hprice1, weights = 1/sqrft)
summary(model_WLS1)

### Les coefficients sont un peu modifi?s mais nos conclusions ? peu pr?s les m?mes.

# L'on multiplie toutes les variables par 1/sqrt(sqrft), et l'on ins?re une 
# nouvelle variable 1/sqrt(sqrft) (qui "remplace" la constante)

hprice1$new_price <- hprice1$price/sqrt(hprice1$sqrft)
hprice1$new_lotsize <- hprice1$lotsize/sqrt(hprice1$sqrft)
hprice1$new_sqrft <- hprice1$sqrft/sqrt(hprice1$sqrft)
hprice1$new_bdrms <- hprice1$bdrms/sqrt(hprice1$sqrft)
hprice1$new_constant <- 1/sqrt(hprice1$sqrft)

# Question 3 :

model_WLS2 <- lm(new_price ~ 0 + new_constant+ new_bdrms + new_sqrft + new_lotsize,
                 hprice1) # Avec le 0, l'on "force" la constante ? ?tre ?gale ? 0
# Ainsi, l'on remplace la constante par une un coefficient qui d?pend de "sqrft"

summary(model_WLS2)

### Question 4
### L'on mesure l'effet des variables explicatives sur la variance
### Comme sp?cifi? dans l'?nonc? l'on a un mod?le en "log-niveau",
### On doit passer le carr? des r?sidus en log

hprice1$log_residuals2 <- log(hprice1$residuals^2)

### La r?gression
model_g <- lm(log_residuals2 ~ lotsize + sqrft + bdrms, hprice1)
summary(model_g)

### A partir de cette r?gression, l'on obtient un estimateur une pr?diction
### du log de la vaiance pour chaque observation, et donc de sa variance.

hprice1$hhat <-  exp(model_g$fitted.values)

# Avec cette estimation, l'on obtient donc des poids pertinents ?
# utiliser, avec :

model_FGLS1 <- lm(formula = price ~ lotsize + sqrft + bdrms, 
                  data = hprice1, 
                  weights = 1/hhat)
summary(model_FGLS1)

### N.B. Correction avec le mod?le de White
library(lmtest)
library(sandwich)
coeftest(model_0, vcov. = vcovHC(model_0, type = "HC1"))

### Ici, l'on obtient une r?elle diff?rence avec les estimations pr?c?dentes,
### lotsize n'est plus significatif.

}

############
### TD 3 ###
############

{
  
# Q1

library(haven)
dt <- read_dta("/Users/jeanbaptistelagrangedupuis/Downloads/aceetal2001.dta")

# Rapide analyse des données :
# logpgp95 : log du revenu par habitant en 95
# africa : variable muette qui prend 1 pour un pays africain, 0 sinon
# oilres : dotation en pétrole
# landlock : absence d'accès à la mer
# silv : dotation en argent
# goldm : dotation en or
# asia : variable muette qui prend 1 pour un pays africain, 0 sinon.
# avexpr : respect des droits de propriétés (risque d'expropriation), échelle de 0 à 10
# logem4 : log de la mortalité des premiers colons européens

# Q2 

summary(lm(logpgp95 ~ avexpr, data = dt))

# Si l'on adopte naivement une approche causale, l'on a 
# une augmentation d'un point de respect des droits de propriétés
# sur une échelle de 0 à 10 qui génère un niveau de revenu
# par habitant de 50 % en plus (modèle log-niveau).

# Les problèmes d'endogénéités sont expliqués dans le début de l'exercice 1.

# Q3 

summary(lm(logpgp95 ~ avexpr +oilres + silv + goldm + 
             landlock + africa + asia, data = dt))

# Les résultats sont relativement similaires à la régression précédente
# pour le coefficient "avexpr" (0.40 contre 0.50). Les variables
# muettes pour le fait d'avoir un pays africain ou asiatique
# sont négatives et très significatives. 

# L'on règle certains problèmes d'endogénéité (des variables
# omises manifestement importantes et corrélées au respect
# des droits propriétés sont ajoutées) mais pas tous.
# Il subsiste d'autres variables omises (ex: culture),
# et le problème de causalité inverse n'est pas traité.
# Même chose pour l'erreur de mesure potentielle.

# Q4

# D'après le graphique, la mortalité des premiers colons 
# a (indirectement) un effet sur la qualité des institutions
# et constitue un instrument pertinent. D'après le graphique
# la mortalité des premiers colons N'a PAS d'effet sur la croissance
# en dehors de son effet sur les institutions. L'instrument est
# donc exogène (l'instrument et la variable dépendante sont
# indépendants si l'on conditionne de la variable expliquée).


# La mortalité des premiers colons est souvent dûe au hasard
# et l'on a donc une forme "d'expérience naturelle" sur la
# qualité des institutions européennes.

# Q5

library(ggplot2)
ggplot(dt, aes(x = logem4, y = logpgp95)) + 
  geom_smooth(method = "lm", col="darkred") +
  geom_point(col="steelblue", size=3)

# Il semble exister un relation forte entre 
# la mortalité des premiers colons et le
# revenu par habitant aujourd'hui.

# Si l'on accepte le diagramme causal de l'article,
# cette relation n'existe que par la qualité des institutions
# et notre analyse semble donc pertinente.

# On peut également reproduire le même graphique
# entre la variable instrumentale et la variable endogène :

library(ggplot2)
ggplot(dt, aes(x = logem4, y = avexpr)) + 
  geom_smooth(method = "lm", col="darkred") +
  geom_point(col="steelblue", size=3)

# Q6

lm1 <- lm(avexpr ~ logem4, data = dt)

dt$pred1 <- lm1$fitted.values

summary(lm1)

# La mortalité des premiers colons a un effet négatif 
# et fortement significatif  sur la qualité des institutions.

# Q7

lm2 <- lm(avexpr ~ logem4 +oilres + silv + goldm + 
             landlock + africa + asia, data = dt)

dt$pred2 <- lm2$fitted.values

summary(lm2)


# La mortalité des premiers colons a un effet négatif 
# et fortement significatif  sur la qualité des institutions.


# La pertinence de la variable instrumentale est donc vérifiée.

# Q8 

summary(lm(logpgp95 ~ pred1, data = dt))


summary(lm(logpgp95 ~ pred2 +oilres + silv + goldm + 
             landlock + africa + asia, data = dt))

# Dans les deux cas, l'effet causal de la qualité 
# des institutions est plus important que l'effet
# estimé avec une régression naïve.
# Que l'on introduise ou non d'autres variables 
# de contrôle, l'effet de l'augmentation 
# d'un point de la qualité des institution sur
# une échelle de 0 à 10 sur
# le revenu par tête est très important 
# (+87 % environ pour la première spécification,
# +72 % dans la seconde)

# Q 9

library(ivreg)

summary(ivreg(logpgp95 ~ avexpr|logem4, data = dt))


summary(ivreg(logpgp95 ~ oilres + silv + goldm + 
                landlock + africa + asia|avexpr|logem4, data = dt))

# Le seul changement observé est la valeur des écart-types des estimateurs,
# qui étaient (légérement) sous-évalués. En conséquence, la valeur des 
# statistiques de test et des p-values est aussi (légérement) modifiée. 

# L'on a présent l'estimation correct à la fois des estimateurs
# et de la valeur de l'écart-type de l'estimateur.

# A retenir : lorsque les régresseurs ne sont pas observés mais estimés,
# la valeur des écart-type est sous-estimée, sauf si la commande utilisée
# ne corrige pas ce biais par une autre méthode (comme ici ivreg).

}


