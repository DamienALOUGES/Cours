setwd("C:/Users/Damien/Desktop/TD A5 S1/Apprentissage/TD1")
require(data.table) ; 
require(ggplot2) ; require(ggthemes) ; # dessin
library(naniar) # complément visualisation NA
# acquisition ####
DT<-fread("train.csv")
# identifier les noms de colonne commençant par un chiffre et ajouter une lettre devant
FeatDebNum<-names(DT)[grepl("^[[:digit:]]",names(DT))] ; 
for (f in FeatDebNum) setnames(DT,f,paste0("F_",f)) # faire en sorte qu'aucun nom de variable ne commence par un chiffre _ interdit ..
# decouverte des données####
str(DT) ; summary(DT)
#GGally::ggpairs(DT[sample(nrow(DT),1000),int[1:10],with=FALSE]) # attention temps de calcul !
library(corrplot) ; corrplot(cor(DT[,names(which(sapply(DT,class)!="character")),with=FALSE]), method = "ellipse")
#target - effet du logarithme
ggplot(DT,aes(x=SalePrice))+geom_histogram(bins=50,fill="blue")+theme_tufte()
ggplot(DT,aes(x=log(SalePrice)))+geom_histogram(bins=50,fill="blue")+theme_tufte() 
DT[,SalePrice:=log(SalePrice)]
# l'approche logarithmique rend comparable des erreurs idépendamment des valeurs absolues
# a aussi pour effet de rapprocher la distribution d'une gaussienne, important pour les approches de régression linéaire

# type des features
int<-names(DT)[which(sapply(DT, class)%in% c("integer","numeric"))]
char<-names(DT)[which(sapply(DT, class)=="character")]
level<-sort(sapply(DT[,char,with=FALSE], function(x) length(unique(x))))# identifier le nombre de valeur différentes pour les colonnes string

# cartographier les NA
isna<-sort(sapply(DT, function(x) sum(is.na(x))/length(x)))
isna<-isna[isna>0]
isnaDT<-data.table(var=names(isna),txna=isna)
isnaDT[,type:="integer"] ; isnaDT[var %in% char,type:="string"] ; 
isnaDT[,var:=factor(var,levels=names(isna))] # pour ordonner l'affichage
isnaDT[var %in% char,type:="string"] # pour différencier la couleur en fonction du type
ggplot(isnaDT[txna>0],aes(x=var,y=txna))+geom_bar(stat="identity",aes(fill=type))+theme_tufte()
# ou avec un package (voir aussi package DataExplorer)
gg_miss_var(DT) # avec package naniar
gg_miss_upset(DT)

# observation des correlations
temp<-copy(DT[,c(char,"SalePrice"),with=FALSE]) 
temp<-melt.data.table(temp,id.vars = "SalePrice")
ggplot(temp,aes(x=value,y=SalePrice))+geom_violin(fill="blue")+facet_wrap(~variable,scales="free_x")+theme_tufte()
temp<-copy(DT[,int,with=FALSE]) 
temp<-melt.data.table(temp,id.vars = "SalePrice")
ggplot(temp,aes(x=value,y=SalePrice))+geom_point(col="blue")+facet_wrap(~variable,scales="free_x")+theme_tufte()

#traitement NA
DTfull<-copy(DT)
for (c in intersect(names(isna),char)) DTfull[(is.na(get(c))),(c):="ex.na"]
for (c in intersect(names(isna),int)) DTfull[is.na(get(c)),(c):=median(DTfull[[c]],na.rm=TRUE)]

#traitement des facteurs peu fréquents - expliquer le traitement suivant
for (c in char) for (v in names(which(table(DTfull[[c]])<15))) DTfull[get(c)==v,(c):="Autre"]
for (c in char) if(min(table(DT[[c]]))<40) {temp<-names(head(sort(table(DTfull[[c]])),2)) ; for (t in temp) DTfull[get(c)==t,(c):=paste(temp,collapse="_")]}

# préparation des bases ####
valid<-sample(nrow(DTfull),floor(nrow(DTfull)/3)) ;# test<-sample(nrow(DT[-valid]),floor(nrow(DT)/10))
DTTrain<-DTfull[-valid] ; DTValid<-DTfull[valid]
supp<-names(which(sapply(DTTrain[,char,with=FALSE],function(x) length(unique(x)))==1)) ; for (c in supp) {DTTrain[,(c):=NULL] ; DTValid[,(c):=NULL]}
char<-names(DTTrain)[which(sapply(DTTrain, class)=="character")]

DTFactor<-rbind(DTTrain,DTValid)
for (c in char) DTFactor[,(c):=as.factor(get(c))] 
DTTrainRF<-DTFactor[1:nrow(DTTrain)] ; DTValidRF<-DTFactor[(nrow(DTTrain)+1):nrow(DTFactor)] ;

require(dummies)
DTMatrice<-rbind(DTTrain,DTValid)
DTMatrice<-dummy.data.frame(DTMatrice)
DTTrainMat<-as.matrix(DTMatrice[1:nrow(DTTrain),]) ; DTValidMat<-as.matrix(DTMatrice[(nrow(DTTrain)+1):nrow(DTMatrice),]) ;



ss<- fread("logistique.csv",encoding="UTF-8")
setnames(ss,nett(name(ss)))
ss<- ss [,.(datedecreationdecommande,provideservice_id,dateexpe)]
ss[,datedecreationdecommande:=as.POSIXct(datedecreationdecommande,"%Y-%m-%d %H;%M:%S",tz="GMT")]