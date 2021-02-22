# settings ####
setwd("C:/Users/Damien/Desktop/TD A5 S1/Apprentissage/TD4")
#https://www.kaggle.com/bwboerman/r-data-table-glmnet-xgboost-with-caret
require(data.table) ; 
require(ggplot2) ; require(ggthemes) ; 
library(naniar) # complément visualisation NA
library(usdm) # pour vifstep
require(plotly)
R2<-function(Y,Yhat) 1-sum((Y-Yhat)^2)/sum((Y-mean(Y))^2)

# acquisition ####
tt<-fread("data immobilier.csv")
# recherchher les noms de colonnes qui commencent par un chiffre et les renommer
FeatDebNum<-names(tt)[grepl("^[[:digit:]]",names(tt))] ; 
for (f in FeatDebNum) setnames(tt,f,paste0("F_",f)) # faire en sorte qu'aucun nom de variable ne commence par un chiffre _ interdit ..
# feature engennering #####
#target
ggplot(tt,aes(x=SalePrice))+geom_histogram(bins=50,fill="blue")+theme_tufte()
tt[,SalePrice:=log(SalePrice)]
ggplot(tt,aes(x=SalePrice))+geom_histogram(bins=50,fill="blue")+theme_tufte()

#Des exemples de transformation / enrichissement de feature (exemples dispo en ligne)
tt[, ':=' (
  GarageQual=as.numeric(ordered(GarageQual, levels = c("None","Po","Fa","TA","Gd","Ex"))),
  age = YrSold - YearRemodAdd,
  isRemodeled = ifelse(YearRemodAdd == YearBuilt, 1, 0),
  isNew       = ifelse(YrSold       == YearBuilt, 1, 0),
  overallQualGood    = ifelse(as.integer(OverallQual) - 5 < 0, 0, as.integer(OverallQual) - 5),
  overallQualBad     = ifelse(5 - as.integer(OverallQual) < 0, 0, 5 - as.integer(OverallQual)),
  sfTotal     = (TotalBsmtSF + F_1stFlrSF + F_2ndFlrSF),  
  hasUnfinishedLevel= ifelse(HouseStyle %in% c("1.5Unf","2.5Unf"),1,0),
  countBathrooms = FullBath + HalfBath + BsmtHalfBath + BsmtFullBath,
  averageRoomSizeAbvGrd = GrLivArea / TotRmsAbvGrd,
  bathRoomToRoomAbvGrd = (FullBath + HalfBath) / TotRmsAbvGrd,
  yrMoSoldInt = as.numeric(format(as.Date(paste(YrSold, MoSold, "1", sep="-")), '%Y%m')) ,
  sfPorch = EnclosedPorch + F_3SsnPorch + ScreenPorch + OpenPorchSF,
  isNewerDwelling = ifelse(MSSubClass %in% c(20,60,120),1,0),
  isRemodeledRecent = ifelse(YearRemodAdd == YrSold, 1, 0),
  isConditionNearRail    = ifelse(Condition1 %in% c("RRAe","RRAn","RRNe","RRNn") | Condition2 %in% c("RRAe","RRAn","RRNe","RRNn"),1,0),
  isConditionNearArtery  = ifelse(Condition1 == "Artery" | Condition2 == "Artery",1,0),
  isConditionNearFeedr  = ifelse(Condition1 == "Feedr" | Condition2 == "Feedr",1,0),
  isConditionNearPosFeature  = ifelse(Condition1 %in% c("PosA"," PosN") | Condition2 %in% c("PosA"," PosN"),1,0),
  soldHighSeason = ifelse(MoSold %in% c(5,6,7),1,0),
  yearsSinceRemodeled = ifelse(YearRemodAdd == YearBuilt, YrSold - YearRemodAdd, 0)
)]
tt[,c("house_age","garage_age","remod_age"):=list(YrSold-YearBuilt,YrSold-GarageYrBlt,YrSold-YearRemodAdd)]

# type
int<-names(tt)[which(sapply(tt, class)%in% c("integer","numeric"))] 
char<-names(tt)[which(sapply(tt, class)%in% c("logical","character"))] 
level<-sort(sapply(tt[,char,with=FALSE], function(x) length(unique(x))))

# na
isna<-sort(sapply(tt, function(x) sum(is.na(x))/length(x)), decreasing = TRUE)
isna<-isna[isna>0]
isnaDT<-data.table(var=names(isna),txna=isna)
isnaDT[,type:="integer"] ; isnaDT[var %in% char,type:="string"] ; 
isnaDT[,var:=factor(var,levels=names(isna))] # pour ordonner l'affichage
isnaDT[var %in% char,type:="string"] # pour différencier la couleur en fonction du type
ggplot(isnaDT[txna>0],aes(x=var,y=txna))+geom_bar(stat="identity",aes(fill=type))+theme_tufte()

gg_miss_var(tt) # avec package naniar
gg_miss_upset(tt)

#traitement NA # attention : median ou 0, (ou autre)  voir au cas pas cas)
ttfull<-copy(tt)
for (c in intersect(names(isna),char)) ttfull[(is.na(get(c))),(c):="ex.na"]
for (c in setdiff(names(ttfull),char)) ttfull[is.na(get(c)),(c):=median(ttfull[[c]],na.rm=TRUE)]
# regroupement des valeurs quali peu fréquentes
for (c in char) for (v in names(which(table(ttfull[[c]])<10))) ttfull[get(c)==v,(c):="Autre"] # regrouper toutes les valeurs rares dans un même paquet
for (c in char) if(min(table(ttfull[[c]]))<20) {temp<-names(head(sort(table(ttfull[[c]])),2)) ; for (t in temp) ttfull[get(c)==t,(c):=paste(temp,collapse="_")]}
for (c in char) if ( length(table(ttfull[[c]]))<3 & min(table(ttfull[[c]]))<15) ttfull[,(c):=NULL]
for (c in names(which(sapply(ttfull,function(x) length(unique(x)))==1))) ttfull[,(c):=NULL]

# cht en factor (requis par certains algos)
char<-names(ttfull)[which(sapply(ttfull, class)=="character")] 
for (c in char) ttfull[,(c):=as.factor(get(c))]




# modelisation ####
# regression lineaire ####
perfRL<-c() ; for (i in 1:100) {
  valid   <- sample(nrow(ttfull),floor(nrow(ttfull)/3)) 
  RegLin  <- lm(SalePrice~.,ttfull[-valid,])
  predRegLin <- predict(RegLin,ttfull[valid,])
  perfRL  <- c(perfRL,R2(ttfull[valid]$SalePrice,predRegLin))}
ggplot(data.table(perfRL=perfRL),aes(x=perfRL))+geom_histogram()+theme_tufte()

# regLin Pénalisée ####
require(caret) # package qui simplfie le reglage de modeles 
perfRLEN<-c() ;  for (i in 1:100) {
  valid<-sample(nrow(ttMatrice),floor(nrow(ttMatrice)/3)) ;# test<-sample(nrow(tt[-valid]),floor(nrow(tt)/10))
  lambda.grid<-10^seq(-4,3,length=30) ; alpha.grid<-seq(0,1,length=15) ; srchGrid<-expand.grid(.alpha=alpha.grid,.lambda=lambda.grid)
  trnCtl<-trainControl(method="cv",number=10) # cadrage coss valisation
  my.train<-train(SalePrice~.,data=ttMatrice[-valid,],method="glmnet",tuneGrid=srchGrid,trControl=trnCtl)
  varImp(my.train)#plot(my.train) # visialusation des performances pour les differentes valeurs de alpha et beta"
  # ggplot(my.train$results,aes(x=log10(lambda),y=RMSE))+geom_line(aes(group=alpha,col=alpha))+theme_tufte()+xlim(-5,0)+ylim(0.13,0.17)
  # attributes(my.train)
  my.train$bestTune ; my.glmnet.model<-my.train$finalModel ; # coef(my.glmnet.model,s=my.train$bestTune$lambda)
  prediction<-predict(my.glmnet.model, newx = subset(ttMatrice, select=-c(get("SalePrice")))[valid,], s = my.train$bestTune$lambda,alpha=my.train$bestTune$alpha)
  perfRLEN<-c(perfRLEN,R2(ttMatrice[,"SalePrice"][valid],prediction))
}
ggplot(data.table(perfRLEN=perfRLEN),aes(x=perfRLEN))+geom_histogram(binwidth = 0.0015)+theme_tufte()

# arbre ####
require(rpart)
require(rpart.plot)
arbre<-rpart(SalePrice~.,ttfull[-valid])
predAbre<-predict(arbre ,ttfull[valid]) # s’en server pour faire une prédiction
rpart.plot(arbre,cex=0.6)
R2(ttfull[valid]$SalePrice,predAbre)


CP<- arbre$cptable[,"CP"]
bestCP<-arbre$cptable[which.min(arbre$cptable[,"xerror"]),"CP"]
arbrePenalise<-prune(arbre,cp=bestCP)


# randomForest ####
require(randomForest)
RF<- randomForest (SalePrice~.,ttfull[-valid]) #éxécuter l’algorithme
plot(RF)
predRF<-predict(RF,ttfull) # s’en servir pour faire une prédiction
