#TD regression linéaire et ses évolutions

setwd("C:/Users/Damien/Desktop/TD A5 S1/Apprentissage/TD2")
require(data.table) ; 
require(ggplot2) ; require(ggthemes)
require(glmnet)
require(caret)
require(corrplot)
require(GGally)
require(usdm)

#1)
set.seed(222);Nlig<-100;Ncol<-95;X<-matrix(rnorm(Nlig*Ncol),ncol=Ncol);
Y<-rnorm(Nlig);
summary(lm(Y~X))


#2)
DT<-fread("boston.csv")
valid<-sample(1:dim(DT)[1],200)
Y<-log(DT[valid]$medv)


#3)
summary(DT); str(DT)


#4)
RSS<-function(Y, Yhat) sum((Yhat-Y)^2)/length(Y)
R2<-function(Y,Yhat) 1-sum((Yhat-Y)^2)/sum((Y-mean(Y))^2)

#1
mod0<- lm(medv~., DT[-valid]);
summary(mod0) ;
plot(mod0) # interprétez les différents graphes
Yhat<-predict(mod0,DT[valid]) ; # application du modele sur le jeu de validation
Yhat[Yhat<1]<-1 #cette derniere commande est une rustine : pouvoir passer en log et avoir la meme
# métrique d'erreur que dans les modelisions suivantes
RSS.0<-RSS(Y,log(Yhat)) # Residual sum square, mesure residus carres sur jeu de test. log pour
# pouvoir comparer avec les modeles suivants
R2.0<- R2(Y,log(Yhat)) # calcul du R2
Mod.0<-c(summary(mod0)$r.squared, summary(mod0)$adj.r.squared, summary(mod0)$f[1], mod0$rank,RSS.0,R2.0) # sauvegarder le résultats du modele #mod0. Nous sauvegarderons les memes données à chaque fois

DT1<-copy(DT) ; DT1[, medv:=log(medv)] # au lieu de modeliser medv on va modeliser son log qui a une distribition plus "normale". Vérifiez le visuellement
mod1<-lm(medv~.,DT1[-valid]) ;
summary(mod1) ; plot(mod1) # commentez les résultats
Yhat<-predict(mod1,DT1[valid])
RSS.1<-RSS(Y,Yhat) 
R2.1<- R2(Y,Yhat)
Rval1<- 1-sum((Yhat-Y)^2)/sum((Y-mean(Y))^2)
Mod.1<-c(summary(mod1)$r.squared,summary(mod1)$adj.r.squared, summary(mod1)$f[1], mod1$rank,RSS.1,R2.1)


#2
temp<-DT1[-valid] ; temp$medv<-NULL # temp est un extrait de DT sur le jeu de train, sans la target
DT2<-DT1[,c(as.character(vifstep(temp,th=8)@results$Variables),"medv"),with=FALSE] # exécutez le par morceau, comprenez et ajustez le parametre « th »
mod2<- lm(medv~., DT2[-valid])
Yhat<-predict(mod2,DT2[valid])
RSS.2<-RSS(Y,Yhat )
R2.2<-R2(Y,Yhat)
Mod.2<-c(summary(mod2)$r.squared, summary(mod2)$adj.r.squared, summary(mod2)$f[1], mod2$rank,RSS.2,R2.2)


#3
DT3<-copy(DT2) ; DT3[,rm2:=rm^2] ; DT3[,lstat2:=lstat^2]; DT3[,crim2:=crim^2]; DT3[,crimlstat:=crim*lstat]
mod3<-lm(medv~.,DT3[-valid,]) ; #summary(mod3) ;#plot(mod3)
Yhat<-predict(mod3,DT3[valid])
RSS.3<-RSS(Y,Yhat)
R2.3<- R2(Y,Yhat)
Mod.3<-c(summary(mod3)$r.squared, summary(mod3)$adj.r.squared, summary(mod3)$f[1], mod3$rank,RSS.3,R2.3)

#4
train.control <- trainControl(method = "cv", number = 10)
step.model <- train(medv ~., data = DT3,method = "leapForward",tuneGrid = data.frame(nvmax = 1:ncol(DT3)),trControl = train.control)# "leapBackward" "leapForward"
step.model$results ; step.model$bestTune  #observer l'evolution de rmse et mae


mod4<-step.model$finalModel
coef<-coef(mod4,step.model$bestTune[[1]])
DT4<-DT3[,c("medv",names(coef)[-1]),with=FALSE]
mod4<-lm(medv~.,DT4[-valid,])
Yhat<-predict(mod4,DT4[valid])
RSS.4<-RSS(Y,Yhat)
R2.4<- R2(Y,Yhat)
Mod.4<-c(summary(mod4)$r.squared,summary(mod4)$adj.r.squared, summary(mod4)$f[1],mod4$rank,RSS.4,R2.4)

resultats