#1)
library(MASS)
dim(Boston)

#2)
train = 1:400
test = -train
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train,]
testing_data = Boston[test,]
dim(training_data)


#3)
cor(training_data$medv,training_data$age)

#4)
model = lm(medv ~ log(age), data = training_data)
plot(log(training_data$age), training_data$medv)
abline(model)

#5)
model2 =lm(medv ~ (log(age)+log(lstat)),data = training_data)
#plot(log(training_data$age)+log(training_data$lstat),training_data$medv)
#abline(model2)
#need to plot in 3D

#6)
summodel2=summary(model2)
print (summodel2)

#7)
#lstat is more significant because |-26.8| > 6.6

#8)
#the model is not really significant because one of the 2 predictors is not significant enough
#and the R-squared is very low

#9)
model3=lm(medv ~ .,data = training_data)

#10)
model4=lm(medv ~ .-lstat+log(lstat),data = training_data)

#11)
summodel4=summary(model4)
print (summodel4)
#R-squared = 0.785 it's higher than before

#12)
mcor=round(cor(Boston,Boston),2)

#13)
install.packages("corrplot")
library(corrplot)
corrplot(mcor, methode="circle")
corrplot.mixed(mcor, methode="circle")

#14)
#0.91

#15)
model5=lm(medv ~ .-tax-lstat+log(lstat),data = training_data)
summodel5=summary(model5)
print (summodel5)
#R²=0.7775
#Fstatistic=112.7

#16)


#17)
str(Boston$chas)
#35 set bound the charles river

#18)
