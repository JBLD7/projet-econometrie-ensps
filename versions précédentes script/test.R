#Code - Économétrie

###à exécuter en premier pour installer les packages
install.packages('psidR')
install.packages("data.table")

###Pour créer le data frame
library(psidR)
library(data.table)
r = system.file(package="psidR")
f = fread(file.path(r,"psid-lists","famvars.txt"))
i = fread(file.path(r,"psid-lists","indvars.txt"))
i = dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
f = dcast(f[,list(year,name,variable)],year~name, value.var = "variable")
d = build.panel(datadir="~/projet-econometrie-ensps/data/PSID/",fam.vars=f,ind.vars=i, heads.only = TRUE,sample="SRC",design="all")


###Pour récupérer des variables spécifiques 
library(openxlsx)
library(psidR)
library(data.table)
r = system.file(package="psidR")
f = fread(file.path(r,"psid-lists","famvars.txt"))
i = fread(file.path(r,"psid-lists","indvars.txt"))
cwf <- read.xlsx("http://psidonline.isr.umich.edu/help/xyr/psid.xlsx")
#getNamesPSID("ER75812", cwf, years = 2017) #remplacer par le nom de la variable ! 
getNamesPSID("ER17013", cwf, years = c("2003", "2005", "2007", "2009")) #some years
#getNamesPSID("ER17013", cwf, years = NULL)  # all years
# next, bring into required shape:
i = dcast(i[,list(year,name,variable)],year~name, value.var = "variable")
f = dcast(f[,list(year,name,variable)],year~name, value.var = "variable")
# call the builder function
e = build.panel(datadir="~/projet-econometrie-ensps/data/PSID/",fam.vars=f,ind.vars=i, heads.only = TRUE,sample="SRC",design="all")
# d contains your panel
save(e,file="~/psid.Rds")

cwf = openxlsx::read.xlsx(system.file(package="psidR","psid-lists","psid.xlsx"))
head_age_var_name <- getNamesPSID("ER17013", cwf, years=c(2003))
famvars = data.frame(year=c(2003),age=head_age_var_name)
build.panel(fam.vars=famvars,datadir="~/projet-econometrie-ensps/data/PSID/")
#Je n'ai pas tout à fait bien compris comment ça marchait. Je mets la documentation du package dans les fichiers du projet. Mais peut-être qu'il vaut mieux essayer le liss. 










