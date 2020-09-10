#TD7

#1)
ligue1 = read.csv("C:/Users/Damien/Downloads/ligue1_17_18.csv", row.names=1, sep=";")

#2)
View(ligue1[1:2])
str(ligue1)

#3)
pointscards = ligue1[c(1,10)]

#4)
set.seed(706974)
km=kmeans(pointscards,2,iter.max = 20)
km2=kmeans(pointscards,3,iter.max = 20)
km3=kmeans(pointscards,4,iter.max = 20)
#5)
summary(km)
summary(km2)
summary(km3)

#6)
km$centers
km2$centers
km3$centers

#7)
plot(pointscards$Points,pointscards$yellow.cards,col=km$cluster,pch=20,cex=2)
points(km$centers,col=1:2,pch=3,cex=3,lwd=3)
plot(pointscards$Points,pointscards$yellow.cards,col=km2$cluster,pch=20,cex=2)
points(km2$centers,col=1:3,pch=3,cex=3,lwd=3)
plot(pointscards$Points,pointscards$yellow.cards,col=km3$cluster,pch=20,cex=2)
points(km3$centers,col=1:4,pch=3,cex=3,lwd=3)

#10)
wss <- (nrow(pointscards)-1)*sum(apply(pointscards,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(pointscards,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#11)

#12)
ligue1_scaled=scale(ligue1)

#13)
km_ligue1=kmeans(ligue1,3,iter.max = 20)
km_ligue1_scaled=kmeans(ligue1_scaled,3,iter.max = 20)

#14)