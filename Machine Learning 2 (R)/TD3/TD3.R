setwd("C:/Users/Damien/Desktop/TD A5 S1/Apprentissage/TD3")

require(data.table) ; 
require(ggplot2) ; require(ggthemes) ; # dessin
library(naniar)
library(lubridate)

db<-fread("logistique.csv",encoding="UTF-8")
summary(db)


db<- db[,.(datedecreationdecommande, providerservice_id, dateexpe)]

db[,datedecreationdecommande:=as.POSIXct(datedecreationdecommande, "%Y-%m-%d %H:%M:%S", tz="GMT")]
db[,dateexpe:=as.Date(dateexpe, "%d/%m/%Y", tz="GMT")]

db[,delaiExp:=as.integer(as.Date(dateexpe)-as.Date(datedecreationdecommande))];
names(db)

db[,day:=format(datedecreationdecommande, "%A")]
db[,heure:=as.integer(format(datedecreationdecommande, "%H"))]
db[,comDate:=as.Date(datedecreationdecommande)]

#db[,compt:=seq_len(length.out)]

dates<- seq.Date(from = min(as.Date(db$datedecreationdecommande)),to=max(as.Date(db$datedecreationdecommande)),by="days")
stock<-sapply(dates,function(X) nrow(db[comDate<X & dateexpe>=X]))
#retard de 2j ou 4j
retj2<-sapply(dates,function(X) nrow(db[comDate==X-2 & dateexpe>=X]))
retj4<-sapply(dates,function(X) nrow(db[comDate==X-4 & dateexpe>=X]))

veillepost18h<-sapply(dates,function(X) nrow(db[comDate==X-1 & heure>=18]))
tariteveille<-sapply(dates,function(X) max(nrow(db[dateexpe==X]), nrow(db[dateexpe==X-1])))

temp<-data.table(comDate=dates,stock=stock,retj2=retj2,retj4=retj4,veillepost18h=veillepost18h, tariteveille=tariteveille)

setkey(db,comDate) ; setkey(temp,comDate); db<-temp[db]
