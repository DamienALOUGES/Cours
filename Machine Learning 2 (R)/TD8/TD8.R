N     = 100000
dists = runif(N)  # here I'm generating random variates from a uniform

data = vector(length=N)
for(i in 1:N){
  if(dists[i]<probs[1]){
    data[i] = rnorm(1, mean=0, sd=1)
  } else if(dists[i]<probs[2]){
    data[i] = rnorm(1, mean=10, sd=1)
  } else {
    data[i] = rnorm(1, mean=3, sd=.1)
  }
}
probs = c(.3,.8)

plot (density(data))
x = seq(-20, 20, 0.1)




truth = .3*dnorm(x,0,sd=1) + .5*dnorm(x,10,sd=1) + .2*dnorm(x,3,sd=.1)
lines(x,truth,col="red",lwd=2)

legend("topleft",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)

data("USArrests") 

df <- scale(USArrests)
print(head(df))
print(summary(df)) #50 observation, 4 variable
print(df)

km.res<- kmeans(df,4,nstart=25)
print(km.res)
km.res$size
km.res$centers

require(factoextra)

fviz_cluster(km.res, data = df, palette = c("red", "blue", "green","yellow"), # 4 colors since 4 clusters
             ggtheme = theme_minimal(),
             main = "Clustering Plot"
)

pc.kmeans=princomp(df, cor=T) 

fviz_pca_biplot(pc.kmeans, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969"  
)

set.seed(345678)
kme2 <- kmeans(pc.kmeans$scores[, 1:2], centers = 4, nstart = 20)
fviz_cluster(kme2, data = pc.kmeans$scores[, 1:2], 
             palette = c("red", "blue", "green","yellow"), 
             ggtheme = theme_minimal(),
             main = "Clustering Plot On First 2 PCs"
)


data1 <- read.csv("data1.csv")
data2 <- read.csv("data2.csv")

print(data1)
print(summary(data1))
print(dim(data1))

par(mfrow=c(1,2))
plot(data1[,1:2], pch=21, bg=data1$truth, main="Data1")
plot(data2[,1:2], pch=21, bg=data2$truth, main="Data2")
#faire un kmeans à 4 classes sur les deux datasets

km1 <- kmeans(data1[,1:2], 4)
km2 <- kmeans(data2[,1:2], 4)
par(mfrow=c(1,2))
plot(data1[,1:2], pch=21, bg=km1$cluster)
plot(data2[,1:2], pch=21, bg=km2$cluster)

require(mclust)

gmm1 <- Mclust(data1[,1:2])
gmm2 <- Mclust(data2[,1:2])

plot(data1[,1:2], pch=21, bg=gmm1$classification)
plot(data2[,1:2], pch=21, bg=gmm2$classification)