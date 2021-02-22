# settings ####
setwd("C:/Users/Damien/Desktop/TD A5 S1/Apprentissage/TD6")
require(data.table)
require(ggplot2) ; require(ggthemes) ; tuft<-theme_tufte()
require(pROC) # calcul auc
require(dummies)

# acquisition ####
zz<-fread("challenge_axa_train.csv")
# bibliotheque de fonctions #####
lift      <- function(pred,reel) {  # fonction qui mesure l'information mutuelle de la colonne f de x vis ?  vis du resultat Y
  Tot<-sum(reel) ;nblignes<-length(reel)
  temp<-as.data.table(cbind(reel, pred))
  lift<-sum(temp[order(pred, decreasing = TRUE)][1:floor(nblignes/10)]$reel)/Tot
  return(lift)}
# feature engineering ####
# departement
zz[,departement:=as.character(departement)]
# traitement des NA
chr<-names(which(sapply(zz,class)=="character")) # nom des colonnes qualitatives
num<-setdiff(names(zz),chr) # nom des colonnes numeriques
for (c in chr) zz[is.na(get(c)),(c):="ex.na"]
for (c in num) {zz[,(paste0(c,"bis")):=0]; zz[is.na(get(c)),(paste0(c,"bis")):=1] ; zz[is.na(get(c)),(c):=median(zz[!is.na(get(c))][[c]])]}
# transformation des qualitatifs en factor
for (c in chr) zz[,(c):=as.factor(get(c))]
# version matricielle (sans qualitatif, transformé en dummy)
zz.mat<-copy(zz)
zz.mat[,departement:=NULL] # on ne garde que les 51 meilleurs departement pour ne pas trop augmenter le nb de colonnes
zz.mat=as.matrix(dummy.data.frame(zz.mat))
####### ###### ####### ###### ####### ###### ####### ###### ####### ######
# set trains valid test
train<-sample(nrow(zz),50000)  # jeux d'apprentissage : caler les parametres
testvalid=setdiff(1:nrow(zz),train) 
valid<-sample(testvalid,19000) # jeux de validation : calibrer les hyperparametres
test<-sample(setdiff(testvalid,valid)) # jeux d'évaluation finale de performance

# arbre####
require(rpart) ; require(rpart.plot)
mod.tree<-rpart(as.factor(target)~.,data=zz[train],control=rpart.control(cp=0.00001,minsplit=2,minbucket=1,maxdepth=30))
bestCP= mod.tree$cptable[which.min(mod.tree$cptable[,"xerror"]),"CP"]
mod.tree.test  <- rpart(as.factor(target)~.,data=zz[c(train,valid)],control=rpart.control(cp=bestCP,minsplit=2,minbucket=1,maxdepth=30))
pred.tree.test <- predict(mod.tree,zz[test])[,2] 
roc.tree=roc(zz$target[test],pred.tree.test)
plot(roc.tree)
tree_Test_auc=roc.tree$auc 
tree_Test_lift<-lift(pred.tree.test,zz$target[test])

# clairement l'arbre n'est pas pertinent
# elastic net ####
require(glmnet) 
require(caret)
lambda.grid<-10^seq(0,-5,length.out =50) ; alpha.grid<-15^seq(-3,0,length.out = 10); 
srchGrid<-expand.grid(.alpha=alpha.grid,.lambda=lambda.grid)
trnCtl<-trainControl(method="boot",number=5,classProbs=TRUE, summaryFunction=twoClassSummary) # cadrage coss valisation
my.train<-train(y=as.factor(paste0("a",zz.mat[c(train,valid),"target"])),x=zz.mat[c(train,valid),-1],metric = "ROC",method="glmnet",family="binomial",tuneGrid=srchGrid,trControl=trnCtl,standardize=TRUE)

ggplot(my.train$results,aes(x=log(lambda,10),y=ROC))+geom_line(aes(group=alpha,col=alpha))+theme_tufte()#+ ylim(0.629,0.645)+ xlim(-5,0)
mod.en = my.train$finalModel#my.train$bestTune
pred.en<-predict(mod.en, newx =zz.mat[test,-1] , alpha=my.train$bestTune$alpha ,s = my.train$bestTune$lambda, type = "response")
#save(pred.en0,file="pred.en0.rda") ; load("pred.en0.rda")
# evaluation de performance
EN_test_auc<-roc(zz$target[test],pred.en[,1])$auc
EN_test_lift<-lift(pred.en[,1],zz$target[test])


# randomForest #####
require(randomForest)
zzrf<-copy(zz) ;
# random Forest ne gere que 53 valeurs distinctes pour les donnés catégorielles : il faut ajuster le contenu de la colonne département. par exemple ne garder que les 50 départements les plus fréquents. les autres seront renotées autre
dep_frequent=names(tail(sort(table(zzrf$departement)),52)) # 52 départements les plus fréquents
zzrf[!departement %in% dep_frequent,departement:="autre departement"]
zzrf[,departement:=as.factor(as.character(departement))]
zzrf.y<-as.factor(zzrf$target) ; zzrf.x<-copy(zzrf) ; zzrf.x[,target:=NULL]

res<-data.table()
for (mtry in 1:30) {
  mod.rf<-randomForest(zzrf.x[train],zzrf.y[train],ntree=100,mtry=mtry)
  pred.rf<-predict(mod.rf,zzrf.x[valid],type="prob")[,2]
  aucValid<-roc(zzrf$target[valid],pred.rf)$auc
  liftValid<-lift(pred.rf,zzrf$target[valid])
  temp<-c(mtry,aucValid,liftValid)
  print(temp)
  res<-rbind(res,t(temp))
}
setnames(res,c("mtry","aucValid","liftValid")) 
ggplot(res,aes(x=mtry,y=liftValid))+geom_point()+tuft ;  ggplot

# GBM avec dismo ####
require(dismo)

# xgboost #####



####### ###### ####### ###### ####### ###### ####### ###### ####### ######
# Combinaison de modeles