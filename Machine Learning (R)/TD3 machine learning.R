#TD3 ALOUGES Damien

#2)
str(Social_Network_Ads)
summary(Social_Network_Ads)

#3)
#install.packages("caTools")
library(caTools) # install it first in the console
set.seed(123)
# we use the function set.seed() with the same seed number
# to randomly generate the same values, you already know that right? 
#and you know why we want to generate the same values, am I wrong? 
split = sample.split(Social_Network_Ads$Purchased, SplitRatio = 0.75)
# here we chose the SplitRatio to 75% of the dataset,
# and 25% for the test set.
training_set = subset(Social_Network_Ads, split == TRUE)
# we use subset to split the dataset
test_set = subset(Social_Network_Ads, split == FALSE)

#4)
#Feature scaling is a method used to normalize the range of independent variables or features of data.
training_set$Age = scale(training_set$Age,center=TRUE,scale=TRUE)
training_set$EstimatedSalary = scale(training_set$EstimatedSalary,center=TRUE,scale=TRUE)
test_set$Age = scale(test_set$Age,center=TRUE,scale=TRUE)
test_set$EstimatedSalary = scale(test_set$EstimatedSalary,center=TRUE,scale=TRUE)

#5)
model=glm(Purchased ~ Age, family="binomial", data=training_set)

#6)
#we must pass in the argument family=binomial in order to tell
#to run a logistic regression rather than some other type of
#generalized linear model

#7)
x = training_set$Age  #because values of purchased are between 18 and 60
y = exp(-(model$coefficient[1]+ model$coefficient[2] * x))
y = 1 / (1 + y)
plot.new()
plot(x,y,col=2,lwd=2)

#8)
summary(model)
#Pr<2e-16 --> its very significant

#9)
#the AIC value of the model is 256.11

#10)
library(ggplot2)
ggplot(training_set, aes(x=Age, y=Purchased)) +
  geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#11)
model2=glm(Purchased ~ Age+EstimatedSalary, family="binomial", data=training_set)

#12)
summary(model2)
#Age :Pr=2.83e-14 
#EstimatedSalary : Pr=2.03e-9
#its very sigificant

#13)
#the AIC is lower so the model is better

#14)
pre = predict(model2,test_set,type="response")
pre2=pre
#15)
for (i in 1:100){
  if (pre2[i] >0.5)
  {
    pre2[i] =1
  }
  else{
    pre2[i]=0
  }
}
pre2

#16)
#install.packages("SDMTools")
library(SDMTools)
confusion.matrix(pre2,test_set$Purchased,threshold = 0.5)

#17)
#accuracy :
accu = (57+26)/(57+26+10+7)
accu
#accuracy = 0.83

#sensitivity : 
sensi=57/67
sensi
#sensitivity = 0.85

#precision :
preci=57/(57+7)
preci
#precision = 0.89


#18)
#install.packages("ROCR")
library(ROCR)

predic=prediction(pre2,test_set$Purchased)
predic
perf=performance(predic,"tpr","fpr")
perf
plot(perf,col=2,lwd=3)
abline(0,1)



#19)
pre3 = predict(model,test_set,type="response")
pre4=pre3
for (i in 1:100){
  if (pre4[i] >0.5)
  {
    pre4[i] =1
  }
  else{
    pre4[i]=0
  }
}
pre4

predic2=prediction(pre4,test_set$Purchased)
predic2
perf2=performance(predic2,"tpr","fpr")
plot(perf,col=2,lwd=3)
plot (perf2 ,col=3,lwd=3, add = TRUE)
abline(0,1)


