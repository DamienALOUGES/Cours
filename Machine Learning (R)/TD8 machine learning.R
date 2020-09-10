#TD8
#ALOUGES Damien

#1)
data1 = read.csv("C:/Users/Damien/Downloads/data1.csv")
View(data1)

data2 = read.csv("C:/Users/Damien/Downloads/data2.csv")
View(data2)

par(mfrow=c(1,2))

plot(x = data1$X1, y = data1$X2, pch = 21, bg=data1$truth, main = "Data1",xlab = "X1", ylab = "X2")
plot(x = data2$X1, y = data2$X2, pch = 21, bg=data2$truth, main = "Data2",xlab = "X1", ylab = "X2")

#2)
set.seed(706974)

dataFrame1 = data.frame(data1$X1,data1$X2)
dataFrame2 = data.frame(data1$X2,data2$X2)

k1 = kmeans(dataFrame1, centers = 4, iter.max = 20)
k2 = kmeans(dataFrame2, centers = 4)

par(mfrow=c(1,2))

plot(x = data1$X1, y = data1$X2, pch = 21, bg=k1$cluster, main = "Data1",xlab = "X1", ylab = "X2")
plot(x = data2$X1, y = data2$X2, pch = 21, bg=k2$cluster, main = "Data2",xlab = "X1", ylab = "X2")

#3)
library(mclust)
gmm1 <-Mclust(dataFrame1)
gmm2 <-Mclust(dataFrame2)

par(mfrow=c(2,2))
plot(dataFrame1,pch=21,bg=gmm1$classification)
plot(dataFrame2,pch=21,bg=gmm2$classification)

#4)
summary(gmm2)

#5)
plot(gmm2, what = "classification")
plot(gmm2, what = "uncertainty")

#6)
plot(gmm2, what = "BIC")
# at 4 gaussians curves stop increases so it usless to do more
# so the bestt number of mixtures is 4

#7)
dens2 = densityMclust(dataFrame2)
plot(dens2 , what ="density")

