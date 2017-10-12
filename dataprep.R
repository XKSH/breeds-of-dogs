# Pour les variables qualitatives
# construire le tableau disjoint;voir samïl(1992)p122 
disj=as.matrix(acm.disjonctif(BreedsDogs))
#construire le tableau burt
burt=t(disj)%*%disj
# Fonction de la génération du tableau corrigé
# moins de loop
tab.cor=function(tab){
  x=matrix(sqrt(rowSums(tab)),ncol=1)%*%sqrt(colSums(tab))
  tabcor=tab/x
  return(tabcor)
}
# Tableaux corrigés
disj.cor=tab.cor(disj)
burt.cor=tab.cor(burt)
# Pourquoi répéter tableaux corrigés?
base=matrix(rep(burt.cor,2000),ncol=dim(burt.cor)[2], byrow=T)
base2=base[sample(nrow(base)),]