# settings ####
setwd("C:/Users/Damien/Desktop/TD A5 S1/Apprentissage/TD7")
#https://www.kaggle.com/bwboerman/r-data-table-glmnet-xgboost-with-caret
require(data.table) ; 
require(ggplot2) ; require(ggthemes) ; 
require(plotly)
require(rpart)
R2<-function(Y,Yhat) 1-sum((Y-Yhat)^2)/sum((Y-mean(Y))^2)

# acquisition ####
load("datasets.rda")

# depouillement #####
#save(RegLin,my.glmnet.model,arbrePenalise,rF,mod.gbm,mod.xgb,file="modeles.rda")
load("modeles.rda")
#regression lineaire ####
mod.lm=lm(SalePrice~.,ttfull)
imp.lm<-summary(mod.lm)$coefficients
Imp.lm<-as.data.table(imp.lm) ; Imp.lm[,variable:=row.names(imp.lm)]
setnames(Imp.lm,c("coef","stdERR","tvalue","pvalue","variable"))
Imp.lm<-Imp.lm[pvalue<0.01]
Imp.lm[,variable:=factor(variable,levels=Imp.lm[order(pvalue,decreasing = TRUE)]$variable)]
ggplot(Imp.lm,aes(x=variable,y=abs(tvalue)))+geom_bar(stat="identity",fill="maroon")+theme_tufte()+coord_flip() 

# arbre ####
arbre<-rpart(SalePrice~.,ttfull,control=rpart.control(cp=0.001,minsplit=2,minbucket=1,maxdepth=30)) ;
bestCP= arbre$cptable[which.min(arbre$cptable[,"xerror"]),"CP"]
arbre<-prune(arbre,cp=bestCP)

impArbre<-arbre$variable.importance
impArbre<-data.table(variable=names(impArbre),importance=impArbre)
impArbre[,variable:=factor(variable,levels=impArbre[order(importance,decreasing = TRUE)]$variable)]
ggplot(impArbre[1:20],aes(x=variable,y=importance))+geom_bar(stat="identity")+
  theme_tufte()+coord_flip()

# randomForest ####
require(randomForest)
#rf réglé avec en parametre 'localImp=TRUE'
mod.rF<-randomForest(SalePrice~.,data=ttfull,mtry=20,
                     ntree=250,nodesize=5,importance=TRUE, localImp=TRUE)

imp.RF<-as.data.table(mod.rF$importance) ;setnames(imp.RF,c("IncMSE","IncNodePurity")) ; 
imp.RF[,variable:=rownames(mod.rF$importance)]
ggplotly(ggplot(imp.RF,aes(x=IncMSE,y=IncNodePurity,label=variable))+geom_point()+
           theme_tufte())
# importance locale
localImp<-data.table()
for (i in sample(1:nrow(ttfull),10)) {
  temp<-tail(sort(mod.rF$localImportance[,i]),5)
  temp<-data.table(var=names(temp),imp=temp, observation=i)
  localImp<-rbind(localImp,temp)}
localImp<-localImp[order(observation,imp)]
localImp[,rang:=1:5,by=.(observation)]
ggplot(localImp,aes(x=rang,y=imp,label=var))+geom_bar(stat="identity",aes(fill=var))+geom_text(aes(x=rang,y=imp,label=var))+theme_tufte()+facet_wrap(~observation,scales = "free")+coord_flip()+theme(legend.position = "none")

# GBM ####
require(dismo) ; 
mod.gbm <- gbm.step(data=ttfull, gbm.x = which(names(ttfull)!="SalePrice"), gbm.y = which(names(ttfull)=="SalePrice"), family = "gaussian",step.size=30,tree.complexity = 3,learning.rate = 0.03, bag.fraction = 0.8,n.folds=3,max.trees=5000,verbose=0)
# save(mod.gbm,file="modgbm.rda")
# importance des features
summary(mod.gbm)
impGBM<-as.data.table(summary(mod.gbm))
impGBM[,var:=as.character(var)]
impGBM[,var:=factor(var, levels=impGBM[order(rel.inf)]$var)]
ggplot(impGBM[rel.inf>0.5],aes(x=var,y=rel.inf))+geom_bar(stat="identity",aes(fill=rel.inf))+theme_tufte()+coord_flip() 
#partial dependancy plot
gbm.plot(mod.gbm, n.plots=12, write.title = FALSE)
# interraction de features
impgGBMinter <- gbm.interactions(mod.gbm)# attention calcul un peu long
impCroiseeMelt<-impgGBMinter$rank.list
impCroiseeDcast<-dcast(impCroiseeMelt,var1.names~var2.names,value.var="int.size") 
ggplotly(ggplot(impCroiseeMelt,aes(x=var1.names,y=var2.names))+geom_point(col="red",aes(size=int.size,alpha=int.size))+theme_tufte())
#illustration d'importance croisée détectée
perim<-copy(ttfull)
perim[,sfTotalDiscrete:=cut(sfTotal,breaks=quantile(ttfull$sfTotal,(0:5)/5))]
perim<-perim[,.(SalePrice=median(SalePrice)),by=.(sfTotalDiscrete,OverallQual)]
ggplot(perim[OverallQual%in% 3:8 & !is.na(sfTotalDiscrete)],aes(x=sfTotalDiscrete,y=OverallQual))+geom_point(size=15,aes(col=exp(SalePrice)))+theme_tufte()+scale_color_gradient(low="red",high="green")
# siplification de modele. Attention calcul un peu long
#simpl<-gbm.simplify(mod.gbm, n.folds = 5, n.drops = "auto", alpha = 1, prev.stratify = TRUE,  eval.data = NULL, plot = TRUE)
#save(simpl,file="simpl.rda")
load("simpl.rda") ; 
simplVisu<-as.data.table(t(1:100)) ; setnames(simplVisu,as.character(1:100))
for (m in simpl$pred.list) {x<-as.data.table(t(m)) ; setnames(x,as.character(m)) ; simplVisu<-rbind(simplVisu,x,fill=TRUE)}
variabletriees=names(sort(apply(simplVisu,2,function(x) length(which(!is.na(x))))))
simplVisu[,model:=1:nrow(simplVisu)]
simplVisu<-melt.data.table(simplVisu,id.vars = "model")
simplVisu[,variable:=factor(variable, levels=variabletriees)]

#simplVisu[,variable:=factor(variable,levels=simplVisu[!is.na(value),.(max(model)),by=.(variable)][order(V1)]$variable)]
ggplot(simplVisu,aes(x=model,y=variable))+geom_point(size=1,aes(col=!is.na(value)))+theme_tufte() 
# XGB
load("mod.xgb.rda")
summary(mod.xgb)
# approche agnostique ####
require(iml)
X=as.data.frame(ttfull) ; X$SalePrice=NULL ; Y=ttfull$SalePrice
predict.fun=function(model=model,newdata=newdata) {return(predict(model,n.trees=model$n.trees,newdata))}
predictor <- Predictor$new(mod.gbm, data = X, y = Y,predict.function=predict.fun)

#importance des features
imp <- FeatureImp$new(predictor, loss = "rmse")
plot(imp)
imp$results
#accumulated local effect (alternative aux partial dependency plot)
ale <- FeatureEffect$new(predictor, feature = "sfTotal",method = "ale")
ale$plot()
#ice : ale par observation
ice <- FeatureEffect$new(predictor, feature = "sfTotal",method = "ice")
ice$plot()

# interraction entre features : une contre toute
interact <- Interaction$new(predictor)
plot(interact)
# interraction entre features : une contre chacune
interact <- Interaction$new(predictor, feature = "crim")
plot(interact)
# modele simplifié
tree <- TreeSurrogate$new(predictor, maxdepth = 2)
plot(tree)
# explication locale
lime.explain <- LocalModel$new(predictor, x.interest = X[1300,])
plot(lime.explain)
lime.explain$results
