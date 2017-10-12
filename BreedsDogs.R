library(FactoClass)
data(BreedsDogs)
disj=as.matrix(acm.disjonctif(BreedsDogs))
burt=t(disj)%*%disj
ctr=matrix(0,dim(disj)[1],dim(disj)[2])
for (i in 1:dim(disj)[1]){
  for (j in 1:dim(disj)[2]){ctr[i,j]=sqrt(sum(disj[i,]))*sqrt(sum(disj[,j]))}}
disj.cor=disj/ctr

ctr=matrix(0,dim(burt)[1],dim(burt)[2])
for (i in 1:dim(burt)[1]){
  for (j in 1:dim(burt)[2]){ctr[i,j]=sqrt(sum(burt[i,]))*sqrt(sum(burt[,j]))}}
burt.cor=burt/ctr

base=matrix(rep(burt.cor,1000),ncol=dim(burt.cor)[2], byrow=T)
base2=base[sample(nrow(base)),]

vois.constr<-function(nr, nc, radius)
{#un petit problème pour x,y
  x=rep(1,nc);x
  for (i in 2:nr)
  {
    x=c(x, rep(i,nc))
  }
  y=c(rep(c(1:nr),nc))
  adr=cbind(x,y)
  neighboo=matrix(rep(0,(nr*nc)^2), nr*nc,nr*nc)
  for (i in 1:(nr*nc))
  {
    for (j in 1:(nr*nc))
    {
      if (max(abs(x[i]-x[j]),abs(y[i]-y[j]))<=radius){neighboo[i,j]=1}
    }
  }
  return(neighboo)
}

# Fonction de décroissance exponentielle des paramètres
para.expo<-function(beta0,betaT,n){return(beta0*((betaT/beta0)^seq(0,1,len=n)))} # BetaT=0.65 ca a l'air cool

kohonenqualigo<-function(seed,nr,nc,alpha0,alphaT,beta0,betaT,dat,n) 
  # seed, dim carte 1, dim carte 2, taux d'apprentissage initial, final, rayon d'apprentissage initial, final, table des données, nombre d'observations lues, type de pas DTW, contrainte de fenêtre DTW
{
  base.test=as.matrix(dat[1:n,]) # x=colonne PCO
  set.seed(seed)
  beta=para.expo(beta0,betaT,n)
  alpha=para.expo(alpha0,alphaT,n)
  profils=matrix(runif(dim(base.test)[2]*nr*nc,0,1),ncol=dim(base.test)[2])
  neighboo=vois.constr(nr, nc, beta[1]) # Construction de la matrice de voisinage
  b=beta[1]
  for (k in 1:n)
  {
    if (beta[k]%/%1!=b%/%1) {neighboo=vois.constr(nr, nc, beta[k])} # On regarde si la décroissance de béta impacte le nombre de voisins
    b=beta[k]
    dista=apply(profils, 1, function(x, y=base.test) dist(rbind(y[k,],x)))
    m<-which(dista==min(dista)) # Recherche du min de ces distances
    #print(profils)
    nf=exp(-apply(profils, 1, function(x, y=profils) dist(rbind(y[m,], x))/(beta[k]^2)))
    updt=t((base.test[k,]-t(profils)))*alpha[k]*nf*neighboo[,m]
    updt[is.na(updt)==TRUE]=0
    profils=profils+updt
  }
  return(profils)
}

kohonenqualiclass<-function(profils,dat,n)
{
  base.test=as.matrix(dat[1:n,])
  m=rep(0,n)
  for (k in 1:n)
  {
    dista=apply(profils, 1, function(x, y=base.test) dist(rbind(y[k,],x)))
    m[k]<-which(dista==min(dista))
  }
  return(m) # Vecteur des neurones vainqueurs
}


prof=kohonenqualigo(25476,5,5,0.04,0.01,2.99,0.65,base2,dim(base2)[1]) # Entrainement sur 20 000
m=kohonenqualiclass(prof,burt.cor,dim(burt.cor)[1])
m2=kohonenqualiclass(prof,disj.cor,dim(disj.cor)[1])
cbind(rownames(burt.cor),m)[order(m),]
cbind(rownames(disj.cor),m2)[order(m2),]


matd=matrix(rep(burt.cor,1000),ncol=dim(burt.cor)[2], byrow=T)
prof=kohonenqualigo(17,5,5,0.1,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1]) # Entrainement sur 20 000
m=kohonenqualiclass(prof,burt.cor,dim(burt.cor)[1])
cbind(rownames(burt.cor),m)[order(m),]
m2=kohonenqualiclass(prof,disj.cor,dim(disj.cor)[1])
cbind(rownames(disj.cor),m2)[order(m2),]

prof20k=kohonendtwgo(147,5,5,0.01,0.001,3.99,0.65,base2,5700,symmetric2,c0=2) # Entrainement sur 20 000
kohonendtwclass(prof20k,5,5,burt.cor,dim(burt.cor)[1])

