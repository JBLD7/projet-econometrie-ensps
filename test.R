#Code - Économétrie

#à exécuter en premier pour installer les packages
install.packages('psidR')
install.packages("data.table")

#Pour créer le data frame
library(psidR)
library(data.table)
r = system.file(package="psidR")
f = fread(file.path(r,"psid-lists","famvars.txt"))
i = fread(file.path(r,"psid-lists","indvars.txt"))
i = dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
f = dcast(f[,list(year,name,variable)],year~name, value.var = "variable")
d = build.panel(datadir="~/projet-econometrie-ensps/data/PSID/",fam.vars=f,ind.vars=i, heads.only = TRUE,sample="SRC",design="all")







