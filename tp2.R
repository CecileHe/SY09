##########Exercice 1 Visualisation des données############
###########################Iris###########################
data(iris)
iris.acp<-princomp(iris[c(-5,-6)])
iris$color<-"black"
iris$color[iris$Species=="virginica"]<- "red"
iris$color[iris$Species=="versicolor"]<- "blue"
#biplot(iris.acp,cex.axis=1.25,cex.lab=1.25)
png(file = "Exo_1_Iris/1_1_iris_sans_distinction.png")
plot(iris.acp$scores,cex.axis=1.25,cex.lab=1.25)
dev.off()
png(file = "Exo_1_Iris/1_1_iris_couleurs.png")
plot(iris.acp$scores,col=iris$color,cex.axis=1.25,cex.lab=1.25)
legend("bottom",inset = .004,legend=c("Setosa","Virginica","Versicolor"),fill=c("black","red","blue"),cex = 1.25,bty="n")
dev.off()

###########################Crabs##########################
crabs2<-read.csv("crabs2.csv")
crabs.acp<-princomp(crabs2[c(-5,-6)])
#biplot(crabs.acp)
png(file = "Exo_1_Crabs/1_2_crabs_sans_distinction.png")
plot(crabs.acp$scores,cex.axis=1.25,cex.lab=1.25)
dev.off()
crabs2$color[crabs2$sp=="B"&crabs2$sex=="M"]<-"blue"
crabs2$color[crabs2$sp=="O"&crabs2$sex=="M"]<-"darkturquoise"
crabs2$color[crabs2$sp=="O"&crabs2$sex=="F"]<-"orange"
crabs2$color[crabs2$sp=="B"&crabs2$sex=="F"]<-"red"
png(file = "Exo_1_Crabs/1_2_crabs_couleurs.png")
plot(crabs.acp$scores,col=crabs2$color,xlim=c(-0.07,0.12),cex.axis=1.25,cex.lab=1.25)
abline(h=0,v=0,col="gray60")
legend("bottomright",bty="n",legend=c("Femelle bleu","Male bleu","Femelle orange","Male orange"),fill=c("red","blue","orange","darkturquoise"),cex = 1.25)
dev.off()

#########################Mutations########################
#AFTD sur mutations
library(MASS)
mut <- read.csv("mutations2.csv", header=T, row.names=1)
mut <- as.dist(mut, diag=T, upper=T)
#d=2
mut.aftd.2<-cmdscale(mut,k=2)
png(file = "Exo_1_Mutations/1_3_mut_d2.png")
plot(mut.aftd.2,cex.axis=1.25,cex.lab=1.25,xlab="Comp. 1",ylab="Comp. 2")
#text(mut.aftd.2,substring(row.names(mut.aftd.2),1,3))
dev.off()
Shepard(mut,mut.aftd.2)
png(file = "Exo_1_Mutations/1_3_mut_d2_shepard.png")
plot(Shepard(mut,mut.aftd.2),cex.axis=1.25,cex.lab=1.25, xlab="Dissimilarité",ylab="Distance")
abline(a=0,b=1)
dev.off()

#d=3
mut.aftd.3<-cmdscale(mut,k=3)
png(file = "Exo_1_Mutations/1_3_mut_d3_shepard.png")
plot(Shepard(mut,mut.aftd.3),cex.axis=1.3,cex.lab=1.3, xlab="Dissimilarité",ylab="Distance")
abline(a=0,b=1)
dev.off()

#d=4
mut.aftd.4<-cmdscale(mut,k=4)
png(file = "Exo_1_Mutations/1_3_mut_d4_shepard.png")
plot(Shepard(mut,mut.aftd.4),cex.axis=1.3,cex.lab=1.3, xlab="Dissimilarité",ylab="Distance")
abline(a=0,b=1)
dev.off()

#d=5
mut.aftd.5<-cmdscale(mut,k=5)
png(file = "Exo_1_Mutations/1_3_mut_d5_shepard.png")
plot(Shepard(mut,mut.aftd.5),cex.axis=1.3,cex.lab=1.3, xlab="Dissimilarité",ylab="Distance")
abline(a=0,b=1)
dev.off()

#d=10
mut.aftd.10<-cmdscale(mut,k=10)
png(file = "Exo_1_Mutations/1_3_mut_d10_shepard.png")
plot(Shepard(mut,mut.aftd.10), xlab="Dissimilarité",ylab="Distance")
abline(a=0,b=1)
dev.off()

##########Exercice 2 Classification hiérarchique##########
#########################Mutations########################
#classification hiérarchique
mut_cah<-hclust(mut)
plot(mut_cah)

###########################Iris###########################
#construire le tableau de distances avec iris
iris_dist<-dist(iris[c(-5,-6)])
#CAH du tableau (utiliser la méthode de ward)
iris_cah<-hclust(iris_dist,method = "ward.D2")
plot(iris_cah)

#cAH avec la méthode diana
iris_diana<-diana(iris[c(-5,-6)],diss = FALSE)
plot(iris_diana) 

##########Exercice 3 Méthode des centres mobiles##########
###########################Iris###########################

data(iris)
#centrage des données
iris.centre<-scale(iris[-5],center=TRUE,scale=FALSE)
#attention ! regarder quel type de données on doit utiliser pour le k meanskm.iris.2
#kmeans avec iris centré
#k=2
km.iris.2<-kmeans(iris.centre,2)
#png(file = "Exo_3_Iris/3_3_iris_k2.png")
plot(iris.acp$scores,col=km.iris.2$cluster)
points(km.iris.2$centers,col=1:2,pch=19)
#dev.off()

#k=3
km.iris.3<-kmeans(iris.centre,3)
#png(file = "Exo_3_Iris/3_3_iris_k3.png")
plot(iris.centre,col=km.iris.3$cluster)
points(km.iris.3$centers,col=1:3,pch=19)
#dev.off()

data(iris)
iris.acp<-princomp(iris[c(-5,-6)])

#k=2
km.iris.2<-kmeans(iris.acp$scores,2)
table(iris$Species,km.iris.2$cluster)
png(file = "Exo_3_Iris/3_3_iris_k2.png")
plot(iris.acp$scores,col=km.iris.2$cluster)
points(km.iris.2$centers,col=1:2,pch=19)
dev.off()

#k=3
km.iris.3<-kmeans(iris.acp$scores,3)
table(iris$Species,km.iris.3$cluster)
png(file = "Exo_3_Iris/3_3_iris_k3.png")
plot(iris.acp$scores,col=km.iris.3$cluster)
points(km.iris.3$centers,col=1:3,pch=19)
dev.off()

#k=4
km.iris.4<-kmeans(iris.acp$scores,4)
table(iris$Species,km.iris.4$cluster)
png(file = "Exo_3_Iris/3_3_iris_k4.png")
plot(iris.acp$scores,col=km.iris.4$cluster)
points(km.iris.4$centers,col=1:4,pch=19)
dev.off()

#test de la stabilité du résultat de la partition
tab_i <- matrix(0,ncol=1,nrow=10)
cl.iris <- matrix(0,ncol=10,nrow=150)
#cl.iris <- data.frame(1:150)
for(i in 1:10)
{
  km.iris.3 <- kmeans(iris.acp$scores,3)
  tab_i[i,] <- km.iris.3$tot.withinss
  plot(iris.acp$scores,col=km.iris.3$cluster)
  points(km.iris.3$centers,col=1:3,pch=19)
  cl.iris[,i]<-km.iris.3$cluster
}

library(mclust)
rand<-matrix(0,ncol=10,nrow=10)
for(i in 1:10)
{
  for(j in 1:10)
    rand[i,j]<-adjustedRandIndex(cl.iris[,i],cl.iris[,j]) #utilisation de l'indice de Rand
}


#détermination du nombre de classes optimal
for(k in 2:10)
{
  for (n in 1:100)
  {
    km.iris.k.n<-kmeans(iris.acp$scores,k)
  }  
}

#calcul de l'intertie intra-classe minimale sur n répétitions pour k=2...k=10
inertie.min<-NULL

tab_i <- matrix(0,nrow=100,ncol=10)
for(k in 1:10)
{
  for (n in 1:100)
  {
tab_i[n,k] <- kmeans(iris.acp$scores,k)$tot.withinss
  }
}

imin <- apply(tab_i, 2, min)
plot(c(1:10),imin,type="l",ylab="Inertie intra-classe",xlab="Nombre de classes K")

#intertie totale à partir de la variance
#I=trace(n*variance empirique)
#variance empirique : cov.wt

###########################Crabs##########################
crabs2<-read.csv("crabs2.csv")
crabs.acp<-princomp(crabs2[c(-5,-6)])

#classification k=2
tab_i <- matrix(0,ncol=1,nrow=10)
for(i in 1:10)
{
  km.crabs.2 <- kmeans(crabs.acp$scores,2)
  tab_i[i,] <- km.crabs.2$tot.withinss
  x11()
  plot(crabs.acp$scores,col=km.crabs.2$cluster)
  points(km.crabs.2$centers,col=1:2,pch=19)
}

#classification k=4
km.crabs.4 <- kmeans(crabs.acp$scores,4)
x11()
plot(crabs.acp$scores,col=km.crabs.4$cluster)
points(km.crabs.4$centers,col=1:4,pch=19)



#########################Mutations########################
