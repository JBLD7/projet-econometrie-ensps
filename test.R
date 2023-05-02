library(haven)
data <- read_sas('/Users/jeanbaptistelagrangedupuis/Library/Mobile Documents/com~apple~CloudDocs/documents/ENS - 2022-2023/économétrie/PROJET S2/CRCS14.sas')



install.packages("foreign")

library("foreign")

data <- read.sas("/Users/jeanbaptistelagrangedupuis/Library/Mobile Documents/com~apple~CloudDocs/documents/ENS - 2022-2023/économétrie/PROJET S2/CRCS14.sas", to.data.frame = TRUE)


lookup.xport("/Users/jeanbaptistelagrangedupuis/Desktop/CRCS14.sas")

install.packages('psidR')
library(psidR)

build.psid(datadr = "~/data/PSID", small = TRUE)

build.psid(datadr = "/Users/jeanbaptistelagrangedupuis/Desktop/testR/data/PSID", small = TRUE)

yes


jean-baptiste.lagrange@ens-paris-saclay.fr


NFUpqT8XiQtF

install.packages("data.table")

library(psidR)
library(data.table)
r = system.file(package="psidR")
f = fread(file.path(r,"psid-lists","famvars.txt"))
i = fread(file.path(r,"psid-lists","indvars.txt"))


i

f

cwf <- read.xlsx("http://psidonline.isr.umich.edu/help/xyr/psid.xlsx")


i = dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
f = dcast(f[,list(year,name,variable)],year~name, value.var = "variable")

head(i)

head(f)


setwd("git@github.com:JBLD7/projet-econometrie-ensps.git/")
d = build.panel(datadir="~/projet-econometrie-ensps/data",fam.vars=f,ind.vars=i, heads.only = TRUE,sample="SRC",design="all")

individu <- d[d$year==2017]

je fais un test pour voir si tout s'update'





