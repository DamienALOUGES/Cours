#TD5 Alouges Damien

dataset= read.csv("C:/Users/Damien/Desktop/spotify.csv")
str(dataset)
summary(dataset)
#the data set has 16 columns
#and 2017 rows


library (caTools)
set.seed(706974)

split = sample.split(dataset$target, SplitRatio = 0.75)
#we split the data set in two with the ratio 75% (25% testset,75% trainingset)

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



boxplot(acousticness~target,data=dataset,col="blue",main="acousticness~target")
aov(acousticness~target,data=dataset)
summary(aov(acousticness~target,data=dataset))
#P-value is 5.13e-09 --> very good predictor

boxplot(danceability~target,data=dataset,col="blue",main="danceability~target")
aov(danceability~target,data=dataset)
summary(aov(danceability~target,data=dataset))
#P-value is 1.31e-15 --> very good predictor

boxplot(duration_ms~target,data=dataset,col="blue",main="duration_ms~target")
aov(duration_ms~target,data=dataset)
summary(aov(duration_ms~target,data=dataset))
#P-value is 3.53e-11 --> very good predictor

boxplot(energy~target,data=dataset,col="blue",main="energy~target")
aov(energy~target,data=dataset)
summary(aov(energy~target,data=dataset))
#p-value 0.0747 --> bad predictor

boxplot(instrumentalness~target,data=dataset,col="blue",main="instrumentalness~target")
aov(instrumentalness~target,data=dataset)
summary(aov(instrumentalness~target,data=dataset))
#P-value is 5.61e-12 --> very good predictor

boxplot(key~target,data=dataset,col="blue",main="key~target")
aov(key~target,data=dataset)
summary(aov(key~target,data=dataset))
#Pvalue is 0.132 --> bad predictor

boxplot(liveness~target,data=dataset,col="blue",main="liveness~target")
aov(liveness~target,data=dataset)
summary(aov(liveness~target,data=dataset))
#Pvalue is 0.237 --> bad predictor

boxplot(loudness~target,data=dataset,col="blue",main="loudness~target")
aov(loudness~target,data=dataset)
summary(aov(loudness~target,data=dataset))
#P-value is 0.00121 --> good predictor

boxplot(mode~target,data=dataset,col="blue",main="mode~target")
aov(mode~target,data=dataset)
summary(aov(mode~target,data=dataset))
#P-value is 0.00115 --> good predictor

boxplot(speechiness~target,data=dataset,col="blue",main="speechiness~target")
aov(speechiness~target,data=dataset)
summary(aov(speechiness~target,data=dataset))
#P-value is 3.56e-12 --> very good predictor

boxplot(tempo~target,data=dataset,col="blue",main="tempo~target")
aov(tempo~target,data=dataset)
summary(aov(tempo~target,data=dataset))
#P-value is 0.119 --> bad predictor

boxplot(time_signature~target,data=dataset,col="blue",main="time_signature~target")
aov(time_signature~target,data=dataset)
summary(aov(time_signature~target,data=dataset))
#P-value is 0.0712 --> bad predictor

boxplot(valence~target,data=dataset,col="blue",main="valencce~target")
aov(valence~target,data=dataset)
summary(aov(valence~target,data=dataset))
#P-value is 1.18e-6 --> very good predictor

#we will use the columns valence, speechiness, instrumentalness, duration_ms, danceability, acousticness as predictors

training_set[c(1,2,3,5,10,13)] = scale(training_set[c(1,2,3,5,10,13)])
test_set[c(1,2,3,5,10,13)] = scale(test_set[c(1,2,3,5,10,13)])

model = glm(target ~ acousticness+danceability+duration_ms+instrumentalness+speechiness+valence,family=binomial,data=training_set)
model
summary(model)
#AIC = 1927.5 --> bad model

#prediction
predi=predict(model,test_set,type ="response")

predi_0_1=ifelse(predi>=0.5,1,0)

head(predi)
head(predi_0_1)

#building of the confusion matrix
library(SDMTools)
conf_mat=confusion.matrix(predi_0_1,test_set$target,threshold = 0.5)
conf_mat
mosaicplot(conf_mat,col=sample(1:8,3))

#accuracy = 175+166/175+166+89+74
#accuracy=0.67
#the  accuracy is pretty bad


require(ROCR)
#building of the performance plot
score= prediction(predi,test_set$target)
perf=performance(score,"auc")

plot(performance(score,"tpr","fpr"),col="red")
abline(0,1,lty=8)

