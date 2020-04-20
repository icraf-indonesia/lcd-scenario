library(reshape2)
library(limSolve)
library(plotly)
library(rlist)


###BEGIN: initiate all variables & function####
# username <- "alfanugraha"
# password <- "1234"
selectedProv = "JaBar"
datapath <- paste0("data/", selectedProv, "/")
# userFolder <- paste0(datapath, username)
# if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)

ioSector <- readRDS(paste0(datapath, "sector"))
ioIntermediateDemand <- readRDS(paste0(datapath, "indem"))
ioFinalDemand <- readRDS(paste0(datapath, "findem"))
ioFinalDemandComponent <- readRDS(paste0(datapath, "findemcom"))
ioAddedValue <- readRDS(paste0(datapath, "addval"))
ioAddedValueComponent <- readRDS(paste0(datapath, "addvalcom"))
ioLeontif <- readRDS(paste0(datapath, "I_A"))
ioLeontiefInverse <- readRDS(paste0(datapath, "leontief"))
ioPeriod <- readRDS(paste0(datapath, "periodIO"))

satelliteLabour <- readRDS(paste0(datapath, "labour"))
satelliteEnergy <- readRDS(paste0(datapath, "energy"))
satelliteWaste <- readRDS(paste0(datapath, "waste"))

satelliteAgriculture <- read.table("raw/jabar_in_redcluwe/16_satelit_pertanian.csv", header = T, sep = ",")
emissionFactorEnergy <- readRDS(paste0(datapath, "ef_energy"))
emissionFactorWaste <- read.table("raw/jabar_in_redcluwe/11_faktor_emisi_limbah.csv", header = T, sep = ",")
emissionFactorAgriculture <- read.table("raw/jabar_in_redcluwe/17_faktor_emisi_pertanian.csv", header = T, sep = ",")
population <- readRDS(paste0(datapath, "currentPopulation"))
populationProjection <- readRDS(paste0(datapath, "population"))
baselineEmission <- readRDS(paste0(datapath, "otherEm")) # not used in calculation

growthRate <- read.table("_AN/growth5_1630.csv", header = T, sep = ",")

# land
datapathTin<-paste0("_TIN/data/", selectedProv, "/")
LUTMDatabase<-as.data.frame(read.csv("_TIN/data/LUTMDatabaseID.csv"))
LDMProp_his<-read.csv(paste0(datapathTin, "LDMProp.csv"))
LUTMTemplate_his<-read.csv(paste0(datapathTin,"LUTM_template.csv"))
LRCRate_his<-read.csv(paste0(datapathTin,"LRCRate.csv"),header = FALSE)
LRCRate_2<-read.csv(paste0(datapathTin,"LRCRate_2.csv"),header=FALSE)
carbonStock_his<-data.matrix(read.csv(paste0(datapathTin,"carbonStock.csv")))
carbonStock_his<-as.matrix(carbonStock_his[,3])


GDPAll <- readRDS(paste0(datapath, "GDPAll"))
linkagesTable <- readRDS(paste0(datapath, "linkagesTable"))
multiplierAll <- readRDS(paste0(datapath, "multiplierAll"))
rtffile <- readRDS(paste0(datapath, "rtffile"))

matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
matrixIoAddedValue <- as.matrix(ioAddedValue)
nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
ioDimention <- ncol(ioIntermediateDemand)

matrixIoFinalDemand <- as.matrix(ioFinalDemand)
rowSumsMatrixIoFinalDemand <- as.matrix(rowSums(matrixIoFinalDemand))
proportionFinalDemand <- ioFinalDemand/rowSumsMatrixIoFinalDemand
proportionFinalDemand[is.na(proportionFinalDemand)] <- 0

colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput 
ioTotalOutputInverse <- 1/ioTotalOutput
ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
ioTotalOutputInverse <- diag(ioTotalOutputInverse)

rowImport <- 1
rowIncome <- 2
rowProfit <- 3

initialYear <- 2016
finalYear <- 2030
iteration <- finalYear - initialYear

functionSatelliteImpact <- function(type = "energy", 
                                    satellite = data.frame(), 
                                    matrix_output = matrix(), 
                                    emission_factor = data.frame(), 
                                    additional_satellite= NULL, 
                                    additional_emission_factor= NULL) { 
  impact <- list()
  
  # impact$consumption
  impact$consumption <- satellite
  
  # calculate the proportion
  if(type != "labour"){
    proportionConsumption <- impact$consumption[, 4:ncol(impact$consumption)] / impact$consumption[, 3]
    impact$consumption[, 4:ncol(impact$consumption)] <- proportionConsumption
  }
  
  # calculate the coefficient & the new total satellite consumption 
  coefficientConsumption <- as.matrix(impact$consumption[,3]) / ioTotalOutput
  impact$consumption[,3] <- coefficientConsumption * matrix_output
  
  # calculate emission
  if(type != "labour"){
    
    colnames(impact$consumption)[3] <- "Tconsumption"
    
    # get the new satellite consumption for each sector
    # total consumption * proportion
    impact$consumption[,4:ncol(impact$consumption)] <- impact$consumption[,4:ncol(impact$consumption)] * impact$consumption[, 3]
    if(!is.null(additional_satellite)){
      impact$consumption[,4:ncol(impact$consumption)]<-impact$consumption[,4:ncol(impact$consumption)]+additional_satellite[,4:ncol(additional_satellite)]
      impact$consumption[,3]<-rowSums(impact$consumption[,4:ncol(impact$consumption)])
    }
    
    # checking the order of factor emission 
    orderEnergyType <- names(impact$consumption)[4:ncol(impact$consumption)]
    emissionFactor <- numeric()
    if (!is.null(additional_emission_factor)){
      emission_factor<-emission_factor[,2]+additional_emission_factor[,2]
    }
    for(m in 1:length(orderEnergyType)){
      emissionFactor <- c(emissionFactor, emission_factor[which(emission_factor[,1]==orderEnergyType[m]), 2])
    }
    emissionFactor <- diag(emissionFactor, nrow = length(emissionFactor), ncol = length(emissionFactor))
    
    # impact$emission
    impact$emission <- impact$consumption
    impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$consumption[,4:ncol(impact$consumption)]) %*% emissionFactor
    impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
    impact$emission[is.na(impact$emission)] <- 0
    colnames(impact$emission)[3] <- "Temission"
  } 
  
  impact$consumption[is.na(impact$consumption)] <- 0
  return(impact)
}


# Function for calculating Land Requirement Coefficient, Land Requirement, & land Cover

functionSatelliteLand1<-function(type=NULL, 
                                 matrix_output=NULL, 
                                 advanceMode = FALSE,
                                 runNum = NULL, # input for advanceMode = FALSE, runNUm 1 to 2
                                 LRCRate= NULL){   # input for advanceMode = TRUE, LRCRate sebagai reactive value yang by default diisi LRC historis 
  impact<-list()
  
  if(type=="historis"){
    impact$LRC<-analysisLRC
    impact$landReq<-landReq_his
    impact$landCover<-landCover_his
  } else{
    if(advanceMode== TRUE){
      impact$LRC<-analysisLRC*LRCRate^(projectionYear-ioPeriod)
    } else{
      if (runNum == 1 ){
        impact$LRC<-analysisLRC*LRCRate_his^(projectionYear-ioPeriod)
      } else if (runNum ==2 ){
        impact$LRC<-analysisLRC*LRCRate_2^(projectionYear-ioPeriod)
      }
    }
    # Land Requirement
    impact$landReq<-diag(impact$LRC[,1]) %*% rbind(as.matrix(matrix_output[,1]),0)
    impact$landReq[nrow(as.matrix(impact$landReq)),]<-sum(landCover_his[,1])-sum(as.matrix(impact$landReq[1:nrow(as.matrix(impact$landReq))-1,]))
    # Land Cover
    impact$landCover<-LDMProp_sektor %*% as.matrix(impact$landReq)
    rownames(impact$landCover)<-colnames(LDMProp_his)
    
  }
  
  # Rapikan
  impact$landReq <- data.frame(c(rownames(ioSector), nrow(ioSector)+1),
                               c(as.character(ioSector[,1]), "lainnya (tidak menghasilkan output)"),
                               impact$landReq, stringsAsFactors = FALSE)
  
  colnames(impact$landReq)<-c("id.sector", "sector", "land.requirement")
  
  
  impact$landCover <- data.frame(as.character(1:23),
                                 colnames(LDMProp_his),
                                 impact$landCover[,1],stringsAsFactors=FALSE)
  colnames(impact$landCover)<-c("id.land.use", "land.use", "luas.land.use")
  
  
  return(impact)
}


# Function for calculating LUTM
# Land cover yang dihasilkan pada fungsi ini adalah land cover proyeksi + input land cover skenario

functionSatelliteLand2<- function(type=NULL,
                                 landCoverProjection = NULL,  #proyeksi land cover BAU atau skenario
                                 landCoverProjectionMin=NULL,
                                 inputLandCover=NULL,  #perubahan land cover skenario aksi
                                 LUTMTemplate=NULL,
                                 advanceMode=FALSE, 
                                 percentage=NULL, # input parameter jika advanceMode=TRUE 
                                 runNum=NULL, # input parameter jika advanceMode=FALSE
                                 additionalG=NULL, 
                                 additionalH=NULL,
                                 additionalE=NULL, 
                                 additionalF=NULL,
                                 LUTMChange=NULL,
                                 GDP=NULL,
                                 carbonStock=carbonStock_his){
  
  impact<- list()
  
  if (type=="historis"){
    impact$landCover<-landCover_his
    # impact$matrixE<-NULL
    # impact$matrixF<-NULL
    # impact$matrixG<-NULL
    # impact$matrixH<-NULL
    impact$LUTM<-LUTM_his
    
  } else{
    
    # set multiiplier for making matrix H
    
    if(advanceMode==TRUE){
      multiplier <- matrix(percentage, nrow=ncol(TPM), ncol=1)
    } else {
      if(runNum==1){ multiplier = 0.8
      } else if (runNum ==2) {multiplier <- 0.5
      } else if (runNum == 3) {multiplier <- 0.3
      } else if (runNum == 4) {multiplier <- 0.1
      } else if (runNum==5) {multiplier <- 0
      } else if (runNum==6) {
        multiplier <- 0.1
        LUTMTemplate <- matrix(NA, nrow=nrow(LUTMTemplate_his),ncol=ncol(LUTMTemplate_his))
        rownames(LUTMTemplate)<-rownames(LUTMTemplate_his)
        colnames(LUTMTemplate)<-colnames(LUTMTemplate_his)
        for (i in 1:nrow(landCover_his)){
          if (sum(landCover_his[i,])==0){
            LUTMTemplate[i,]<-matrix(0,ncol=ncol(LUTMTemplate_his))    #LUTMTemplate bisa diedit di interface
            LUTMTemplate[,i]<-matrix(0,nrow=ncol(LUTMTemplate_his))
          } else {}
        }
        # LUTMTemplate<-read.csv("_TIN/data/JaBar/LUTMTemplate_his2.csv", header=TRUE)
        LUTMTemplate[is.na(LUTMTemplate)]<-paste0("x",1:length(LUTMTemplate[is.na(LUTMTemplate)]))
      }
    }
    
    # landCover 
    if(!is.null(inputLandCover)){
      impact$landCover<-as.matrix(landCoverProjection)+as.matrix(inputLandCover)
    } else{
      impact$landCover<-as.matrix(landCoverProjection)
    }
    
    # LUTM Template
    jumlahVariabel<-length(LUTMTemplate[LUTMTemplate!=0])
    namaVariabel<-paste0("x",1:length(LUTMTemplate[LUTMTemplate!=0]))
    LUTMTemplate[LUTMTemplate!=0]<-namaVariabel
    
    # matrix E
    impact$matrixE<-matrix(NA,nrow = 46, ncol = jumlahVariabel)
    colnames(impact$matrixE)<-namaVariabel
    variabel_x<-list()
    variabel_y<-list()
    for (a in 1:nrow(LUTMTemplate)){  ## constraint 1
      variabel_x[[a]]<-t(LUTMTemplate[a,])[t(LUTMTemplate[a,])!= 0]
      eval(parse(text=paste0("variabel_x_",a,"<-NULL")))
      eval(parse(text=paste0("variabel_x_",a,"<-variabel_x[[",a,"]]")))
      for (i in 1:length(variabel_x[[a]])){
        if(!identical(variabel_x[[a]],c(numeric(0), character(0),integer(0)))){
          eval(parse(text=paste0("impact$matrixE[",a,",paste0(variabel_x_",a,"[",i,"])]<-1")))
          # impact$matrixE[a,paste0(variabel_n[i])]<-1
        } else {impact$matrixE[a,]<-0}
      }
    }
    for (a in 1:ncol(LUTMTemplate)){  ##constraint 2
      variabel_y[[a]]<-t(LUTMTemplate[,a])[t(LUTMTemplate[,a])!= 0]
      eval(parse(text=paste0("variabel_y_",a,"<-NULL")))
      eval(parse(text=paste0("variabel_y_",a,"<-variabel_y[[",a,"]]")))
      for (i in 1:length(variabel_y[[a]])){
        if(!identical(variabel_y[[a]],c(numeric(0), character(0), integer(0)))){
          eval(parse(text=paste0("impact$matrixE[(23+",a,"),paste0(variabel_y_",a,"[",i,"])]<-1")))
          # impact$matrixE[a,paste0(variabel_n[i])]<-1
        } else {impact$matrixE[(23+a),]<-0}
      }
    }
    impact$matrixE[is.na(impact$matrixE)]<-0
    impact$matrixE<-impact$matrixE[(!(rbind(as.matrix(impact$landCover[,1]),as.matrix(impact$landCover[,1]))) == 0),]  #hapus constraint untuk tupla yg jumlahnya 0 agar compatible saat perhitungan LSEI
    if (is.null(additionalE)){
      impact$matrixE<-impact$matrixE
    } else{
      impact$matrixE<- rbind(impact$matrixE, as.matrix(additionalE))
    }
    
    
    # matrix F
    impact$matrixF<-rbind(landCoverProjectionMin,as.matrix(impact$landCover))
    impact$matrixF<- as.matrix(impact$matrixF[!(rowSums(impact$matrixF) == 0),])
    if (is.null(additionalF)){
      impact$matrixF<-impact$matrixF
    } else{
      impact$matrixF<- rbind(impact$matrixF, as.matrix(additionalF))
    }
    
    # check all diagonal variable names
    diagVariable<-matrix(NA, ncol=1, nrow=ncol(LUTMTemplate))
    for (i in 1:ncol(LUTMTemplate)){
      diagVariable[i,1]<-LUTMTemplate[i,i]
    }
    diagVariable<-diagVariable[!(diagVariable==0),]
    
    # matrix G
    impact$matrixG<-rbind(diag(nrow=(jumlahVariabel)), matrix(0, nrow=length(diagVariable),ncol=jumlahVariabel))  ## buat matrix G constraint 1 & 2
    colnames(impact$matrixG)<-namaVariabel
    for (i in 1:length(diagVariable)){
      impact$matrixG[jumlahVariabel+i,diagVariable[i]]<-1   #assign 1 untuk semua variabel diagonal
    }
    if (is.null(additionalG)){
      impact$matrixG<-impact$matrixG
    } else{
      impact$matrixG<- rbind(impact$matrixG, as.matrix(additionalG))
    }
    
    # get TPM value for each diagonal variable
    
    diagTPM<-matrix(NA, ncol=1, nrow=ncol(TPM))
    for (i in 1:ncol(TPM)){
      diagTPM[i,1]<-TPM[i,i]
    }
    diagTPM<-as.matrix(diagTPM[!(diagTPM==0),])
    
    # matrix H
    diagTPM <- diagTPM*multiplier 
    impact$matrixH<-rbind(matrix(0,nrow=jumlahVariabel,ncol=1),as.matrix(diagTPM*landCoverProjectionMin[landCover_his!=0]))
    
    if (is.null(additionalH)){
      impact$matrixH<-impact$matrixH
    } else{
      impact$matrixH<- rbind(impact$matrixH, as.matrix(additionalH))
    }
    
    
    # LUTM dengan metode LSEI
    variabelLSEI<-lsei(E = impact$matrixE, F = impact$matrixF, G=impact$matrixG, H=impact$matrixH)
    variabelLSEI<-as.matrix(unlist(variabelLSEI[["X"]]))
    variabelLSEI<-as.matrix(as.numeric(variabelLSEI[1:jumlahVariabel,]))
    row.names(variabelLSEI)<-namaVariabel
    impact$LUTM<-as.matrix(LUTMTemplate)
    # impact$LUTM<-matrix(ncol=ncol(LUTMTemplate), nrow=nrow(LUTMTemplate))
    # colnames(impact$LUTM)<-colnames(LUTMTemplate)
    # colnames(impact$LUTM)<-rownames(LUTMTemplate)
    for (a in 1:nrow(impact$LUTM)){
      for(b in 1:ncol(impact$LUTM)){
        if (impact$LUTM[a,b]==0){
          impact$LUTM[a,b]<-as.numeric(0)
        } else {impact$LUTM[a,b]<-as.numeric(variabelLSEI[paste0(LUTMTemplate[a,b]),1])
        }
      }
    }
    class(impact$LUTM)<-"numeric"
    if (!is.null(LUTMChange)){
      impact$LUTM<- as.matrix(impact$LUTM)+as.matrix(LUTMChange)
    }
    
  }
  
  # emission
  impact$emission<-matrix(NA,nrow=nrow(as.matrix(impact$LUTM)), ncol=ncol(as.matrix(impact$LUTM)))
  for (a in 1:nrow(impact$LUTM)){
    for (b in 1:ncol(impact$LUTM)){
      impact$emission[a,b]<-as.numeric(impact$LUTM[a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*(-1)
    }
  }

  impact$emission<-matrix(sum(impact$emission),nrow=nrow(GDP))
  impact$emission<-impact$emission *GDP/sum(GDP)

  
  # rapikan

  impact$landCover <- data.frame(as.character(1:23),
                                 colnames(LDMProp_his),
                                 impact$landCover[,1],stringsAsFactors=FALSE)
  colnames(impact$landCover)<-c("id.land.use", "land.use", "luas.land.use")

  impact$LUTM <- data.frame(as.character(1:23),
                            colnames(LDMProp_his),
                            impact$LUTM,stringsAsFactors=FALSE)
  colnames(impact$LUTM)<-c("id.land.use", "land.use", colnames(LDMProp_his))

  impact$emission <- data.frame(rownames(ioSector),
                                as.character(ioSector[,1]),
                                impact$emission,stringsAsFactors=FALSE)
  colnames(impact$emission)<-c("id.sector", "sector", "emission")
  
  
  return(impact)
}

# function for calculating new LUTMTemplate, additional matrix G, additional matrix H, & delta land Cover (inputLandCover)
functionSatelliteLand3<-function (inputLandScen = NULL,
                                  timeScen = timeStep){
  impact<-list()
  
  if (is.null(inputLandScen)){
    impact$LUTMTemplate<-LUTMTemplate_his
    impact$additionalG<-NULL
    impact$additionalH<-NULL
    impact$inputLandCover<-NULL
  } else{
    # calculate scenario LUTM Template
    impact$LUTMTemplate<-LUTMTemplate_his
    impact$LUTMTemplate[impact$LUTMTemplate!="0"]<-NA
    rownames(impact$LUTMTemplate)<-colnames(impact$LUTMTemplate)
    for (i in 1:nrow(inputLandScen)){
      impact$LUTMTemplate[paste0(inputLandScen[i,1]), paste0(inputLandScen[i,2])]<- NA
    }
    impact$LUTMTemplate[is.na(impact$LUTMTemplate)]<-paste0("x",1:length(impact$LUTMTemplate[is.na(impact$LUTMTemplate)]))
    
    # additional G & additional H
    impact$additionalG<-matrix(0,ncol=length(impact$LUTMTemplate[impact$LUTMTemplate!=0]), nrow=nrow(inputLandScen))
    impact$additionalH<-matrix(ncol=1, nrow=nrow(inputLandScen))
    
    colnames(impact$additionalG)<-as.character(impact$LUTMTemplate[impact$LUTMTemplate!=0])
    
    for (i in 1:nrow(inputLandScen)){
      impact$additionalG[i,impact$LUTMTemplate[paste0(inputLandScen[i,1]), paste0(inputLandScen[i,2])]]<-1
      impact$additionalH[i,1]<-inputLandScen[i,paste0(timeScen)]
    }
    
    # inputLandCover
    impact$inputLandCover<- matrix(0,ncol=1, nrow=23)
    rownames(impact$inputLandCover)<-colnames(impact$LUTMTemplate)
    
    for (landCoverClass in unique(inputLandScen[,2])){
      impact$inputLandCover[paste(landCoverClass),]<-sum(inputLandScen[inputLandScen[,2]==paste(landCoverClass), timeScen]) # pertambahan luas <- positif jumlah total luas kelas tupla yang sama di tahun akhir
    } 
    
    for (landCoverClass in as.character(unique(inputLandScen[,1]))){
      impact$inputLandCover[landCoverClass,]<--sum(inputLandScen[inputLandScen[,1]==paste(landCoverClass), timeScen]) # penurunan luas <- negatif jumlah total luas kelas tupla yang sama tahun akhir
    } 
  }
  return (impact)
}

###END: initiate ####

###BEGIN: regional economic impact analysis & historical emission from satellite account####
# Direct Backward Linkage
analysisDBL <- colSums(ioLeontiefInverse)
analysisBPD <- analysisDBL/(mean(analysisDBL))

# Direct Forward Linkage
analysisDFL <- rowSums(ioLeontiefInverse)
analysisFPD <- analysisDFL/(mean(analysisDFL))

# GDP
analysisGDP <- colSums(matrixIoAddedValue[rowIncome:nrowMatrixIoAddedValue,])
analysisTotalGDP <- sum(analysisGDP)

# Multiplier Output (MO)
analysisMO <- colSums(ioLeontiefInverse)

# Coefficient Income (CI) & Multiplier Income (MI)
analysisCI <- as.matrix(matrixIoAddedValue[rowIncome,]) / ioTotalOutput
analysisMI <- ioLeontiefInverse %*% analysisCI
analysisMI[is.na(analysisMI)] <- 0

# Coefficient Labour (CL) & Multiplier Labour (ML)
analysisCL <- as.matrix(satelliteLabour[,3]) / ioTotalOutput
analysisML <- ioLeontiefInverse %*% analysisCL
analysisML[is.na(analysisML)] <- 0

# Coefficient Energy Used (CE) & Multiplier Energy (ME)
analysisCE <- as.matrix(satelliteEnergy[,3]) / ioTotalOutput
analysisME <- ioLeontiefInverse %*% analysisCE
analysisME[is.na(analysisME)] <- 0

# Coefficient Waste Product (CW) & Multiplier Waste (MW)
analysisCW <- as.matrix(satelliteWaste[,3]) / ioTotalOutput
analysisMW <- ioLeontiefInverse %*% analysisCW
analysisMW[is.na(analysisMW)] <- 0

# Coefficient Agriculture-Fertilizer (CA) & Multiplier Agriculture-Fertilizer (MA)
analysisCA <- as.matrix(satelliteAgriculture[,3]) / ioTotalOutput
analysisMA <- ioLeontiefInverse %*% analysisCA
analysisMA[is.na(analysisMA)] <- 0

# Ratio Wages / Business Surplus
analysisRatioWS <- t(as.matrix(ioAddedValue[2,] / ioAddedValue[3,]))
analysisRatioWS[is.na(analysisRatioWS)] <- 0
analysisRatioWS[analysisRatioWS == Inf] <- 0
colnames(analysisRatioWS) <- "ratio_ws"

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
landCover_his0<- dcast(data = LUTMDatabase, formula = ID_LC1 ~ ., fun.aggregate = sum)
landCover_his0<-as.matrix(landCover_his0[,2])

#TPM 
TPM<-matrix(nrow=nrow(LUTM_his), ncol=ncol(LUTM_his))
for (i in 1:ncol(TPM)){
  TPM[,i]<-LUTM_his[,i]/landCover_his0[i,1]   #proporsi semua elemen LUTM dibagi tupla tahun kedua
}
TPM[is.nan(TPM)]<-0

#land distribution matrix dalam luas (analysisLDMLuas)
analysisLDMLuas<-as.matrix(LDMProp_his)%*%as.matrix(diag(landCover_his[,1]))

#land requirement historis (analysisLR)
landReq_his<-as.matrix(rowSums(analysisLDMLuas))

# Land requirement coefficient (analysisLRC) & land productivity coefficient (analysisLPC)
analysisLPC<-rbind(as.matrix(rowSums(cbind(ioIntermediateDemand, ioFinalDemand))), 0)/landReq_his    #rowSums(cbind(indem, findem))=output
analysisLPC[is.infinite(analysisLPC)]<-0
analysisLPC[is.nan(analysisLPC)]<-0
analysisLRC<-1/analysisLPC
analysisLRC[is.infinite(analysisLRC)]<-0
analysisLRC[is.nan(analysisLPC)]<-0

# land distribution matrix proportion (total sector = 1)
LDMProp_sektor<-matrix(NA, nrow=ncol(analysisLDMLuas), ncol=nrow(analysisLDMLuas))
for (i in 1:ncol(LDMProp_sektor)){
  LDMProp_sektor[,i]<-as.matrix(analysisLDMLuas[i,]/sum(analysisLDMLuas[i,]))
}
LDMProp_sektor[is.na(LDMProp_sektor)]<-0


# LUTM Template
LUTMTemplate_his<-as.matrix(LUTMTemplate_his)
for (i in 1:nrow(landCover_his)){
  if (sum(landCover_his[i,])==0){
    LUTMTemplate_his[i,]<-matrix(0,ncol=ncol(LUTMTemplate_his))    #LUTMTemplate bisa diedit di interface
    LUTMTemplate_his[,i]<-matrix(0,nrow=ncol(LUTMTemplate_his))
  } else {}
}
LUTMTemplate_his[is.na(LUTMTemplate_his)]<-paste0("x",1:length(LUTMTemplate_his[is.na(LUTMTemplate_his)]))


# Satellite account by sectoral GDP
analysisEnergyByGDP <- as.matrix(satelliteEnergy[,3]) / analysisTotalGDP
analysisWasteByGDP <- as.matrix(satelliteWaste[,3]) / analysisTotalGDP
analysisAgricultureByGDP <- as.matrix(satelliteAgriculture[,3]) / analysisTotalGDP

# Emission from energy
emissionFactorEnergyDiagonal <- diag(emissionFactorEnergy[,2], ncol = nrow(emissionFactorEnergy), nrow = nrow(emissionFactorEnergy))
emissionEnergy <- as.matrix(satelliteEnergy[,4:ncol(satelliteEnergy)]) %*% emissionFactorEnergyDiagonal
emissionEnergyTotal <- rowSums(emissionEnergy)

# Emission from waste
emissionFactorWasteDiagonal <- diag(emissionFactorWaste[,2], ncol = nrow(emissionFactorWaste), nrow = nrow(emissionFactorWaste))
emissionWaste <- as.matrix(satelliteWaste[,4:ncol(satelliteWaste)]) %*% emissionFactorWasteDiagonal
emissionWasteTotal <- rowSums(emissionWaste)

# Emission from agriculture-fertilizer
emissionFactorAgricultureDiagonal <- diag(emissionFactorAgriculture[,2], ncol = nrow(emissionFactorAgriculture), nrow = nrow(emissionFactorAgriculture))
emissionAgriculture <- as.matrix(satelliteAgriculture[,4:ncol(satelliteAgriculture)]) %*% emissionFactorAgricultureDiagonal
emissionAgricultureTotal <- rowSums(emissionAgriculture)

# Wages
analysisWages <- as.matrix(t(ioAddedValue[2,]))
colnames(analysisWages) <- "wages"

# Income per capita
analysisIncomePerCapita <- sum(as.matrix(matrixIoAddedValue[rowIncome,])) / population

# Coefficient technology (intermediate demand) or A
analysisCT <- t( t(matrixIoIntermediateDemand) / ioTotalOutput)

# Coefficient primary input
analysisCPI <- t(t(ioAddedValue) / ioTotalOutput)


###END: analysis ####



###BEGIN: BAU projection####

# Series of GPD & Output projection
bauSeriesOfGDP <- data.frame(Sektor = ioSector[,1], stringsAsFactors = FALSE)
bauSeriesOfGDP$y2015 <- analysisGDP
bauSeriesOfFinalDemand <- rowSumsMatrixIoFinalDemand
bauSeriesOfOutput <- ioTotalOutput

# Series of Intervention Point
bauSeriesOfIntermediateDemand <- list()
bauSeriesOfAddedValue <- list()
bauSeriesOfFinalDemandComponent <- list()
bauSeriesOfImpactLabour <- list()
bauSeriesOfImpactEnergy <- list()
bauSeriesOfImpactWaste <- list()
bauSeriesOfImpactAgriculture <- list()
bauSeriesOfImpactLand1<-list()
bauSeriesOfImpactLand2<-list()


# Historical consumption and emission data
eval(parse(text=paste0("bauSeriesOfIntermediateDemand$y",ioPeriod," <- matrixIoIntermediateDemand")))
eval(parse(text=paste0("bauSeriesOfAddedValue$y",ioPeriod," <- matrixIoAddedValue")))
eval(parse(text=paste0("bauSeriesOfFinalDemandComponent$y",ioPeriod," <- matrixIoFinalDemand")))
eval(parse(text=paste0("bauSeriesOfImpactLabour$y",ioPeriod," <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))")))
eval(parse(text=paste0("bauSeriesOfImpactEnergy$y",ioPeriod,"<- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)")))
eval(parse(text=paste0("bauSeriesOfImpactWaste$y",ioPeriod," <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)")))
eval(parse(text=paste0("bauSeriesOfImpactAgriculture$y",ioPeriod,"<- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)")))

# historical LRC, land requirement, & land cover 
eval(parse(text=paste0("bauSeriesOfImpactLand1$y",ioPeriod,"<-functionSatelliteLand1(type= 'historis', matrix_output= as.matrix(ioTotalOutput))")))
# LSEI
eval(parse(text=paste0("bauSeriesOfImpactLand2$y",ioPeriod," <- functionSatelliteLand2(type='historis',carbonStock=carbonStock_his, GDP= as.matrix(bauSeriesOfGDP$y",ioPeriod,") )")))

growthRateSeries <- growthRate
growthRateSeries$Lap_usaha <- NULL
growthRateSeries <- as.matrix(1+growthRateSeries)

projectionYear <- initialYear
listYear <- paste0("y", ioPeriod)

# economic & impact (energy, waste, & agriculture projection 
for(step in 1:(iteration+1)){
  projectionFinalDemand <- growthRateSeries[, step] * bauSeriesOfFinalDemand[, step]
  
  bauSeriesOfFinalDemand <- cbind(bauSeriesOfFinalDemand, projectionFinalDemand)
  projectionOutput <- ioLeontiefInverse %*% projectionFinalDemand 
  bauSeriesOfOutput <- cbind(bauSeriesOfOutput, projectionOutput)
  
  # notes on the year
  timeStep <- paste0("y", projectionYear)
  
  # add additional values to the list
  eval(parse(text=paste0("bauSeriesOfFinalDemandComponent$", timeStep, " <- as.matrix(proportionFinalDemand*projectionFinalDemand)"))) # contains NaN
  eval(parse(text=paste0("bauSeriesOfIntermediateDemand$", timeStep, " <-  analysisCT %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
  eval(parse(text=paste0("bauSeriesOfAddedValue$", timeStep, " <-  analysisCPI %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
  
  # GDP projection 
  eval(parse(text = paste0("bauSeriesOfGDP$", timeStep, "<- colSums(bauSeriesOfAddedValue$", timeStep, "[setdiff(1:nrow(matrixIoAddedValue), rowImport),])")))
  
  # Impact projection
  eval(parse(text= paste0("bauSeriesOfImpactLabour$", timeStep, " <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(projectionOutput))")))
  eval(parse(text= paste0("bauSeriesOfImpactEnergy$", timeStep, " <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorEnergy)")))
  eval(parse(text= paste0("bauSeriesOfImpactWaste$", timeStep, " <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorWaste)")))
  eval(parse(text= paste0("bauSeriesOfImpactAgriculture$", timeStep, " <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorAgriculture)")))
  
  listYear <- c(listYear, timeStep)
  projectionYear <- initialYear+step
  
}

colnames(bauSeriesOfOutput) <- as.character(listYear)
colnames(bauSeriesOfFinalDemand)<- as.character(listYear)

bauSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), bauSeriesOfFinalDemand)
colnames(bauSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear)) 

# land cover projection 

# non-advance mode
for (i in 1:2){
  projectionYear <- initialYear
  listYear <- paste0("y", ioPeriod)
  for(step in 1:(iteration+1)){
    # notes on the year
    timeStep <- paste0("y", projectionYear)
    # projection
    eval(parse(text= paste0("bauSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(bauSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = FALSE,
                                                                                          runNum = ",i,", # input for advanceMode = FALSE
                                                                                          LRCRate= NULL)")))
    listYear <- c(listYear, timeStep)
    projectionYear <- initialYear+step
  } 
  # jika tidak ada value landCover yang negatif, break loop
  if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){  
    if(i==1){
      textDataLRCRate="historis"
    } else {
      textDataLRCRate="historis yang dimodifikasi" 
    }
    print(paste0("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC ", textDataLRCRate)) # use as UI textoutput 
    break
  } else {
    if(i==2){
      print("proyeksi luas tutupan lahan yang dihasilkan bernilai negatif. Silakan masukkan data laju perubahan LRC secara manual")
    }
  }
}

# jika masih ada value landCover yang negatif, force to enter advanceMode pada UI
if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
  repeat{
    # insert UI here to request for new inputLRCRate 
    inputLRCRate<-LRCRate_2  
    projectionYear <- initialYear
    listYear <- paste0("y", ioPeriod)
    for(step in 1:(iteration+1)){
      # notes on the year
      timeStep <- paste0("y", projectionYear)
      eval(parse(text= paste0("bauSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(bauSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = TRUE,
                                                                                          runNum = NULL,
                                                                                          LRCRate= inputLRCRate)")))
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
    }  
    # jika tidak ada value landCover yang negatif, break loop
    if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){ 
      print("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC yang telah Anda modifikasi") # use as UI textoutput 
      break
    } else {
      print("proyeksi tutupan lahan yang dihasilkan memiliki luasan negatif. Silakan menyunting ulang laju perubahan LRC dan atau kembali ke target permintaan akhir") # use as UI textoutput 
    }
  }
}


# LUTM Projection 
projectionYear <- initialYear
listYear <- paste0("y", ioPeriod)

for(step in 1:(iteration+1)){
  for (i in 1:6){   # 5 tipe yg akan dirun otomatis
    timeStep <- paste0("y", projectionYear)
    eval(parse(text=paste0(
      "bauSeriesOfImpactLand2$",timeStep,"<-tryCatch({
    functionSatelliteLand2 (type ='projected',
                            landCoverProjection = as.matrix(bauSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]) ,
                            landCoverProjectionMin=  as.matrix(bauSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                            LUTMTemplate = LUTMTemplate_his, 
                            advanceMode = FALSE,
                            runNum =",i," , 
                            GDP=as.matrix(bauSeriesOfGDP$",timeStep,")
    )
  }, warning = function (a){NA}, error = function(b){NA})"
    )))
    if(any(is.na(bauSeriesOfImpactLand2[[timeStep]]))==FALSE){
      print(paste0("use constraint ", i ," to make LUTM ",timeStep))
      break
    } else {
      if(i==6){
        print(paste0("tidak berhasil menghitung LUTM ",timeStep))
      } 
    }
  }
  listYear <- c(listYear, timeStep)
  projectionYear <- initialYear+step
}

# jika tidak berhasil menghitung LUTM, force to enter advanceMode pada UI (spt pada land cover)

#####END : BAU projection ####

#####BEGIN : BAU projection visualization ####
# 1. GDP (ind. 1)
resultGDP <- data.frame(year = 0, sector = "", category="", GDP = 0, stringsAsFactors = FALSE)
# resultGDP <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
for(c in 2:ncol(bauSeriesOfGDP)){
  add.row <- cbind(ioSector, bauSeriesOfGDP[, c])
  names(add.row) <- c("sector", "category", "GDP")
  add.row$year <- initialYear + (c-3)
  add.row <- add.row[, colnames(resultGDP)]
  resultGDP <- data.frame(rbind(resultGDP, add.row), stringsAsFactors = FALSE)
}
resultGDP <- resultGDP[resultGDP$year != 0, ] # remove initial values

# 2. Income per capita (ind. 9)
resultIncomePerCapita <- data.frame(year = 0, Income.per.capita = 0)
for(t in 0:iteration){
  t_curr <- initialYear + t
  pop_curr <- populationProjection[which(populationProjection[, 1] == t_curr), 2]
  inc_curr <- sum(bauSeriesOfAddedValue[[t+2]][rowIncome,])
  inc_capita <- inc_curr/pop_curr
  add.row <- data.frame(cbind(t_curr, inc_capita))
  names(add.row) <- names(resultIncomePerCapita)
  resultIncomePerCapita <- data.frame(rbind(resultIncomePerCapita, add.row), stringsAsFactors = FALSE)
}
resultIncomePerCapita <- resultIncomePerCapita[resultIncomePerCapita$year != 0, ]

# 3. Wages or Income (ind. 7)
resultIncome <- data.frame(year = 0, sector= "", income = 0, stringsAsFactors = FALSE)
sc.name <- ioSector[,1]
for(t in 0:iteration){
  t_curr <- initialYear + t
  inc_curr <- data.frame(bauSeriesOfAddedValue[[t+2]][rowIncome,])
  add.row <- data.frame(cbind(t_curr, sc.name, inc_curr), stringsAsFactors = FALSE)
  names(add.row) <- names(resultIncome)
  resultIncome <- data.frame(rbind(resultIncome, add.row), stringsAsFactors = FALSE)
}
resultIncome <- resultIncome[resultIncome$year != 0, ]

# 4. Labour (ind. number 10)
resultLabour <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
for(t in 0:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLabour[[t+2]][[1]])
  names(add.row) <- names(resultLabour)[2:4]
  add.row$year <- t_curr
  add.row <- add.row[, names(resultLabour)]
  resultLabour <- data.frame(rbind(resultLabour, add.row), stringsAsFactors = FALSE)
}
resultLabour <- resultLabour[resultLabour$year != 0, ]

# 5. Energy cons (indicator number 2)
resultEnergyConsumption <- bauSeriesOfImpactEnergy[[2]][[1]]
resultEnergyConsumption$year <- initialYear
resultEnergyConsumption <- resultEnergyConsumption[, c("year", names(bauSeriesOfImpactEnergy[[2]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactEnergy[[t+2]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultEnergyConsumption)]
  resultEnergyConsumption <- data.frame(rbind(resultEnergyConsumption, add.row), stringsAsFactors = FALSE)
}
names(resultEnergyConsumption)[2:3] <- c("id.sector", "sector")

# 6. Energy emission (indicator number 3)
resultEnergyEmission <- bauSeriesOfImpactEnergy[[2]][[2]]
resultEnergyEmission$year <- initialYear
resultEnergyEmission <- resultEnergyEmission[, c("year", names(bauSeriesOfImpactEnergy[[2]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactEnergy[[t+2]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultEnergyEmission)]
  resultEnergyEmission <- data.frame(rbind(resultEnergyEmission, add.row), stringsAsFactors = FALSE)
}
names(resultEnergyEmission)[2:3] <- c("id.sector", "sector")

# 7. Waste cons (indicator number 2)
resultWasteDisposal <- bauSeriesOfImpactWaste[[2]][[1]]
resultWasteDisposal$year <- initialYear
resultWasteDisposal <- resultWasteDisposal[, c("year", names(bauSeriesOfImpactWaste[[2]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactWaste[[t+2]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultWasteDisposal)]
  resultWasteDisposal <- data.frame(rbind(resultWasteDisposal, add.row), stringsAsFactors = FALSE)
  
}
names(resultWasteDisposal)[2:3] <- c("id.sector", "sector")

# 8. Waste emission (indicator number 3)
resultWasteEmission <- bauSeriesOfImpactWaste[[2]][[2]]
resultWasteEmission$year <- initialYear
resultWasteEmission <- resultWasteEmission[, c("year", names(bauSeriesOfImpactWaste[[2]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactWaste[[t+2]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultWasteEmission)]
  resultWasteEmission <- data.frame(rbind(resultWasteEmission, add.row), stringsAsFactors = FALSE)
}
names(resultWasteEmission)[2:3] <- c("id.sector", "sector")

# 9. Fertilizer cons (indicator number 2)
resultFertilizerUsed <- bauSeriesOfImpactAgriculture[[2]][[1]]
resultFertilizerUsed$year <- initialYear
resultFertilizerUsed <- resultFertilizerUsed[, c("year", names(bauSeriesOfImpactAgriculture[[2]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactAgriculture[[t+2]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultFertilizerUsed)]
  resultFertilizerUsed <- data.frame(rbind(resultFertilizerUsed, add.row), stringsAsFactors = FALSE)
  
}
names(resultFertilizerUsed)[2:3] <- c("id.sector", "sector")

# 10. Fertilizer emission (indicator number 3)
resultFertilizerEmission <- bauSeriesOfImpactAgriculture[[2]][[2]]
resultFertilizerEmission$year <- initialYear
resultFertilizerEmission <- resultFertilizerEmission[, c("year", names(bauSeriesOfImpactAgriculture[[2]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactAgriculture[[t+2]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultFertilizerEmission)]
  resultFertilizerEmission <- data.frame(rbind(resultFertilizerEmission, add.row), stringsAsFactors = FALSE)
}
names(resultFertilizerEmission)[2:3] <- c("id.sector", "sector")

# 11. Land Requirement 
resultLandReq <- bauSeriesOfImpactLand1[[2]][["landReq"]]
resultLandReq$year <- initialYear
resultLandReq <-resultLandReq[,c("year", names(bauSeriesOfImpactLand1[[2]][["landReq"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLand1[[t+2]][["landReq"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(resultLandReq)]
  resultLandReq <- data.frame(rbind(resultLandReq, add.row), stringsAsFactors = FALSE)
}

# 12. Land Cover
resultLandCover <- bauSeriesOfImpactLand2[[2]][["landCover"]]
resultLandCover$year <- initialYear
resultLandCover <-resultLandCover[,c("year", names(bauSeriesOfImpactLand2[[2]][["landCover"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["landCover"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(resultLandCover)]
  resultLandCover <- data.frame(rbind(resultLandCover, add.row), stringsAsFactors = FALSE)
}

# 13. LUTM
resultLUTM <- bauSeriesOfImpactLand2[[2]][["LUTM"]]
resultLUTM$year <- initialYear
resultLUTM <-resultLUTM[,c("year", names(bauSeriesOfImpactLand2[[2]][["LUTM"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["LUTM"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(resultLUTM)]
  resultLUTM <- data.frame(rbind(resultLUTM, add.row), stringsAsFactors = FALSE)
}

# 14. Land Emission by sector 

resultLandEmission <- bauSeriesOfImpactLand2[[2]][["emission"]]
resultLandEmission$year <- initialYear
resultLandEmission <-resultLandEmission[,c("year", names(bauSeriesOfImpactLand2[[2]][["emission"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["emission"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(resultLandEmission)]
  resultLandEmission <- data.frame(rbind(resultLandEmission, add.row), stringsAsFactors = FALSE)
} 

# 15. Total Emission
# resultTotalEmission <- baselineEmission[which(baselineEmission$Year>=initialYear & baselineEmission$Year<= finalYear),]
resultTotalEmission <- data.frame(Year=initialYear:finalYear)
emissionEnergyConsumption <- numeric()
emissionWasteDisposal <- numeric()
emissionFertilizer <- numeric()
emissionLand <- numeric()
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- sum(resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"])
  add_MWdisp <- sum(resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"])
  add_MF <- sum(resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"])
  add_MLand <-sum(resultLandEmission[resultLandEmission$year==t_curr, "emission"])
  emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
  emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
  emissionFertilizer <- c(emissionFertilizer, add_MF)
  emissionLand<-c(emissionLand, add_MLand)
}
resultTotalEmission$emissionEnergyCons <- emissionEnergyConsumption
resultTotalEmission$emissionWasteDisp <- emissionWasteDisposal
resultTotalEmission$emissionFert <- emissionFertilizer
resultTotalEmission$emissionLand <-emissionLand
resultTotalEmission$TotalEmission <- rowSums(resultTotalEmission[, 2:ncol(resultTotalEmission)])
resultTotalEmission$CummulativeEmission <- cumsum(resultTotalEmission$TotalEmission)

# 16. BAU emission[economic sector, years]
bauSeriesOfEmissionBySector <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"]
  add_MWdisp <- resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"]
  add_MF <- resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"]
  add_MLand <- resultLandEmission[c(resultLandEmission$year==t_curr & resultLandEmission$sector != "lainnya (tidak menghasilkan output"), "emission"]
  eval(parse(text=paste0("bauSeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF + add_MLand")))
}

# resultTotalGDP <- colSums(bauSeriesOfGDP[,2:(ncol(bauSeriesOfGDP)-1)])
bauAllResult <- subset(resultTotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
# bauAllResult <- cbind(bauAllResult, resultTotalGDP)
bauAllResult$resultTotalGDP<-colSums(bauSeriesOfGDP[,2:(ncol(bauSeriesOfGDP)-1)])
bauAllResult$CummulativeGDP <- cumsum(bauAllResult$resultTotalGDP)
bauAllResult$EmissionIntensity <- bauAllResult$TotalEmission / bauAllResult$resultTotalGDP
bauAllResult$CummulativeEmissionIntensity <-cumsum(bauAllResult$EmissionIntensity)


ggplot(data=bauAllResult, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
ggplot(data=bauAllResult, aes(x=Year, y=CummulativeEmission, group=1)) + geom_line() + geom_point()
ggplot(data=bauAllResult, aes(x=Year, y=EmissionIntensity, group=1)) + geom_line() + geom_point()
ggplot(data=bauAllResult, aes(x=Year, y=resultTotalGDP, group=1)) + geom_line() + geom_point()
ggplot(data=bauAllResult, aes(x=Year, y=CummulativeGDP, group=1)) + geom_line() + geom_point()
ggplot(data=bauAllResult, aes(x=Year, y=CummulativeEmissionIntensity, group=1)) + geom_line() + geom_point()


#####END : BAU projection visualization ####

