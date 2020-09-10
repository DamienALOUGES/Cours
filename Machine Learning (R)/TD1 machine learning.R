#1.8.2

x=c(1,7,3,4);
y=100:1;
a=x[3]+y[4];
b=cos(x[3])+sin(x[2])* exp(-y[2]);
x[3]= 0;
y[2]= -1;
z=y[x+1];

#1.8.5

qf(0.9, df1=1, df2=5, lower.tail = TRUE, log.p = FALSE);#90% quantile of a F distribution
qf(0.95, df1=1, df2=5, lower.tail = TRUE, log.p = FALSE);#95% quantile of a F distribution
qf(0.99, df1=1, df2=5, lower.tail = TRUE, log.p = FALSE);#99% quantile of a F 

rpois(100,5);

x=seq(-4,4);
y=dt(x,df=1,log=FALSE);#calcul of density of student
plot(x,y);
lines(x,y);#same as the plot before but with lines, not with dot
lines(x,dt(x,df=5,log=FALSE),col="grey");
lines(x,dt(x,df=10,log=FALSE),col="green");
lines(x,dt(x,df=50,log=FALSE),col="blue");
lines(x,dt(x,df=100,log=FALSE),col="red");

#1.9.1

Load("EU.RData","C:\ user\Damien\download",verbose=TRUE);
myModel <- lm(formula = CamCom2011 ~ Population2010, data = EU)
#???3
myModel$residuals
myModel$coefficients
#???4
summaryMyModel <- summary(myModel)
#5
summaryMyModel$sigma
#???1.9.2

library(MASS)
dim(Boston)
train = 1:400
test = -train
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]
dim(training_data)
plot(training_data$lstat, training_data$medv)
model = lm(medv ~ log(lstat), data = training_data)
plot(log(training_data$lstat), training_data$medv)
abline(model)

plot(log(training_data$lstat), training_data$medv,
     xlab = "Log Transform of % of Houshold with Low Socioeconomic Income",
     ylab = "Median House Value",
     col = "green",
     pch = 9, cex=0.5)
abline(model, col = "pink", lwd =3)

predict(model, data.frame(lstat = c(5)))
predict(model, data.frame(lstat = c(5,10,15), interval = "prediction"))
y = testing_data$medv

y_hat = predict(model, data.frame(lstat = testing_data$lstat))
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE
