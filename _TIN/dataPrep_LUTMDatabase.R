library(reshape2)
library(limSolve)
library(plotly)
library(rlist)


selectedProv<-"JaBar"
ioPeriod<-2015
LUTMDatabase<-as.data.frame(read.csv("_TIN/data/LUTMDatabaseID.csv"))


# land use transition matrix (LUTM) historis  
LUTMDatabase<-LUTMDatabase[LUTMDatabase$Provinsi==paste0(selectedProv),c("Count",paste0("PL", ioPeriod-1, "RCL"), paste0("PL",ioPeriod,"RCL"))]
colnames(LUTMDatabase)<-c("COUNT","ID_LC1","ID_LC2")
tuplaID<- cbind(as.matrix(cbind(matrix(0, nrow=23^2, ncol=1), as.matrix(expand.grid(1:23, 1:23)))))
colnames(tuplaID)<-c("COUNT","ID_LC1","ID_LC2")
LUTMDatabase<-rbind(LUTMDatabase,tuplaID)
LUTMDatabase<-aggregate(LUTMDatabase, by=list(LUTMDatabase$ID_LC1,LUTMDatabase$ID_LC2), FUN=sum)
LUTMDatabase<-LUTMDatabase[,1:3]
colnames(LUTMDatabase)<-c("ID_LC1","ID_LC2","COUNT")
LUTMDatabase<-LUTMDatabase[LUTMDatabase$ID_LC1>0,]
LUTMDatabase<-LUTMDatabase[LUTMDatabase$ID_LC2>0,]
LUTMDatabase <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
LUTM_his <- dcast(data = LUTMDatabase, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)
LUTM_his<-LUTM_his[,-1]


# land cover historis
landCover_his <- dcast(data = LUTMDatabase, formula = ID_LC2 ~ ., fun.aggregate = sum)
landCover_his<-as.matrix(landCover_his[,2])

# variabel LUTMDatabase, LUTM_his, & landCover_his tolong disimpan sbg RDS & CSV
