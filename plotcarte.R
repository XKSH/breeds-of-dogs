# Plot la carte Kohonen
# Plot le grid 5x5
adr=expand.grid(1:5,1:5)
par(bg = "thistle",mai=c(0.2,0.2,0.6,0.2))
plot(c(1, 5), c(1, 5), type= "n", xlab = "", ylab = "",axes=FALSE,xlim=c(0.5,5.5),ylim=c(0.5,5.5),main="Distribution de modalité")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="grey")
grid(nx = 5,ny=5)
# Génération des coordonées de modalité
m=kohonenqualiclass(prof,burt.cor,dim(burt.cor)[1])
mcoor=data.frame(rownames(burt.cor),m)[order(m),]
mcoor=data.frame(mcoor,adr[as.numeric(mcoor[,2]),])
mcoor[,4][which(duplicated(data.frame(mcoor[,3],mcoor[,4]))=="TRUE")]=mcoor[,4][which(duplicated(data.frame(mcoor[,3],mcoor[,4]))=="TRUE")]+0.2
text(mcoor[,3],mcoor[,4],mcoor[,1],cex=0.6)

# Ajouter les lignes connectant les modalités
col=palette()
vB=names(BreedsDogs)
for(i in 1:length(vB)){
  x=mcoor[grep(vB[i],mcoor[,1]), c(3,4)]
  x=rbind(x,x[1,])
  lines(x[,1],x[,2],col=col[i])
}
