options(digits=2)
credit<-read.csv("C:/Users/wldbs/Desktop/Credit Card Customer Data.csv",header=T)
credit
head(credit)
attach(credit)
summary(credit)

cred=credit[,2:6]
head(cred)
attach(cred)

#기술통계량&상관분석
cred_cor<-cor(cred)
library(corrplot)
corrplot(cred_cor,method="number")
corrplot(cred_cor,method="circle")
summary(cred)

#산점도 행렬
library(lattice)
splom(cred)

#주성분분석 
attach(cred)
pp_cor=princomp(cred,cor=TRUE)
summary(pp_cor)
attributes(pp_cor)
pp_cor$loadings
library(graphics)
screeplot(pp_cor,npcs=6, type="lines")

biplot(pp_cor)
pp_cor

#인자분석
library(psych)
library(GPArotation)
x_factor=principal(cred, nfactors = 2, rotate = "none",cor = "cor")
x_factor

#초기요인분석
cred.factor = principal(cred,rotate='none')
cred.factor$values
plot(cred.factor$values, type="b")
#고유값이 1이상인 경우가 1,2 임으로 
#2개의 인자수 채택

cred.varimax=principal(cred,nfactor=2,rotate="varimax")
cred.varimax
biplot(cred.varimax)

#인자분석 최종  
cred_fac=factanal(cred, factors=2, rotation="none"); cred_fac
cred_var=factanal(cred, factors=2, rotation="varimax"); cred_var

plot(cred_var$loadings, main="factors pattern")

text(cred_var$loadings[,1], cred_fac$loadings[,2], labels = c("x1","x2","x3","x4","x5"), cex=0.7, pos=3,col = "blue")

#데이터 표준화 
cred_scaled <- data.frame(scale(cred))
head(cred_scaled)

#군집분석
par(mfrow=c(2,2))
hc1=hclust(dist(cred_scaled)^2,method="single")
plot(hc1,labels=id, hang = -1,main="dandrogram:single")

hc2=hclust(dist(cred_scaled)^2,method="complete")
plot(hc2,labels=id, hang = -1,main="complete linkage")

hc3=hclust(dist(cred_scaled)^2,method="average")
plot(hc3,labels=id, hang = -1,main="average linkage")

hc4=hclust(dist(cred_scaled)^2,method="ward.D")
plot(hc4,labels=id, hang = -1,main="ward.D")

library(NbClust)
nc <- NbClust(cred_scaled, min.nc = 2, max.nc = 15, method = "ward.D")

cl.num=3
colnames(cred_scaled)=c("X1","X2","X3","X4","X5")
hc4.result=cutree(hc4,k=cl.num)
plot(cred_scaled, pch=hc4.result)

a=cbind(credit,hc4.result)
a
c1.1=a[(a$hc4.result==1),]  # cluster 1
c1.2=a[(a$hc4.result==2),]  # cluster 2
c1.3=a[(a$hc4.result==3),]  # cluster 3
c1.1 ; c1.2 ; c1.3
table(hc4.result)

c1.1[,1]
c1.2[,1]
c1.3[,1]

#k-means
ccent=function(y,cl){
  f=function(i){colMeans(y[cl==i,])}
  x=sapply(sort(unique(cl)),f)
  colnames(x)=sort(unique(cl))
 return(x)
}

cred_kmeans <- kmeans(cred_scaled, centers = 3)
attributes(cred_kmeans)
cred_kmeans$cluster

#grouping 
clus=cbind(id,cred_scaled,cred_kmeans$cluster)
head(clus)
clus1=clus[(clus[,7]==1),]
clus1
clus2=clus[(clus[,7]==2),]
clus2
clus3=clus[(clus[,7]==3),]
clus3
kc=table(cred_kmeans$cluster)
kc

library(NbClust)
nc <- NbClust(cred_scaled, min.nc = 2, max.nc = 15, method = "kmeans")

plot(cred_scaled, pch=cred_kmeans$cluster, col=cred_kmeans$cluster, main="K-means clustering")
text(cred_scaled,labels=id, adj=0, cex=0.5)
ccent(cred_scaled,cred_kmeans$cluster)

par(mfrow=c(1,3))
boxplot(clus1[,2:6])
boxplot(clus2[,2:6])
boxplot(clus3[,2:6])