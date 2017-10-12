library(FactoClass)
data(BreedsDogs)
#setwd("D:/Profiles/XU/Desktop/breedsdog")
this.dir <- dirname("parent.frame(2)$ofile")
setwd(this.dir)
source("dataprep.R")
source("Kohonenprep.R")
source("Kohonennet.R")

prof=kohonenqualigo(25476,5,5,0.04,0.01,2.99,0.65,base2,dim(base2)[1])
m=kohonenqualiclass(prof,burt.cor,dim(burt.cor)[1])
m2=kohonenqualiclass(prof,disj.cor,dim(disj.cor)[1])

cbind(rownames(burt.cor),m)[order(m),]
cbind(rownames(disj.cor),m2)[order(m2),]
# KACM II utilisant uniquement Burt corrigé
matd=matrix(rep(burt.cor,2500),ncol=dim(burt.cor)[2], byrow=T)
prof=kohonenqualigo(17,5,5,0.1,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1]) # Entrainement sur 20 000
m=kohonenqualiclass(prof,burt.cor,dim(burt.cor)[1])
cbind(rownames(burt.cor),m)[order(m),]
m2=kohonenqualiclass(prof,disj.cor,dim(disj.cor)[1])
cbind(rownames(disj.cor),m2)[order(m2),]
# Plot le résultat de modalité
source("plotcarte.R")