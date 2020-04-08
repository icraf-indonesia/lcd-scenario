library(reshape2)
library(limSolve)
library(plotly)


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
LUTMTemplate<-read.csv(paste0(datapathTin,"LUTM_template.csv"))
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

functionSatelliteImpact <- function(type = "energy", satellite = data.frame(), matrix_output = matrix(), emission_factor = data.frame()) { 
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
    
    # checking the order of factor emission 
    orderEnergyType <- names(impact$consumption)[4:ncol(impact$consumption)]
    emissionFactor <- numeric()
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


functionSatelliteLand<- function(type=NULL,
                                 LRCRate=LRCRate_2,
                                 matrix_output=NULL,
                                 inputLandCover=NULL, 
                                 LUTMTemplate=NULL, 
                                 additionalE=NULL, 
                                 additionalF=NULL,
                                 additionalG=NULL, 
                                 additionalH=NULL, 
                                 LUTMChange=NULL, 
                                 carbonStock=carbonStock_his){
  
  impact<-list()
  
  # Land Requirement Coefficient and Land Requirement
  if(type=="historis"){
    impact$LRC<-analysisLRC
    impact$landReq<-diag(impact$LRC[,1]) %*% rbind(as.matrix(matrix_output[,1]),0)
    impact$landReq[nrow(as.matrix(impact$landReq)),]<-sum(landCover_his[,1])-sum(as.matrix(impact$landReq[1:nrow(as.matrix(impact$landReq))-1,]))
  } else{
    impact$LRC<-analysisLRC*LRCRate^(projectionYear-ioPeriod)
    impact$landReq<-diag(impact$LRC[,1]) %*% rbind(as.matrix(matrix_output[,1]),0)
    impact$landReq[nrow(as.matrix(impact$landReq)),]<-sum(landCover_his[,1])-sum(as.matrix(impact$landReq[1:nrow(as.matrix(impact$landReq))-1,]))
    # if (length(impact$landReq[impact$landReq<0])>=1){   
    #   impact$LRC<-analysisLRC*as.matrix(LRCRate_2^(projectionYear-ioPeriod))
    #   impact$landReq<-diag(impact$LRC[,1])%*%rbind(as.matrix(matrix_output[,1]),0)
    #   impact$landReq[nrow(impact$landReq),]<-sum(landCover_his[,1])-sum(impact$landReq[1:nrow(impact$landReq)-1,])
    # } else{}
  }
  
  
  # Land Cover
  impact$landCover<-LDMProp_sektor %*% as.matrix(impact$landReq)
  if(!is.null(inputLandCover)){
    impact$landCover<-as.matrix(impact$landCover)+as.matrix(inputLandCover)
  } else{}
  rownames(impact$landCover)<-colnames(LDMProp_his)
  
  if (type=="historis"){
    
    impact$LUTMTemplate<-NULL
    impact$matrixE<-NULL
    impact$matrixF<-NULL
    impact$matrixG<-NULL
    impact$matrixH<-NULL
    impact$LUTM<-LUTM_his

  } else{

    # LUTM Template
    impact$LUTMTemplate<-as.matrix(LUTMTemplate)
    for (i in 1:nrow(impact$landCover)){
      if (sum(impact$landCover[i,])==0){
        impact$LUTMTemplate[i,]<-matrix(0,ncol=ncol(impact$LUTMTemplate))    #LUTMTemplate bisa diedit di interface
        impact$LUTMTemplate[,i]<-matrix(0,nrow=ncol(impact$LUTMTemplate))
      } else {}
    }
    jumlahVariabel<-length(impact$LUTMTemplate[is.na(impact$LUTMTemplate)])
    namaVariabel<-paste0("x",1:length(impact$LUTMTemplate[is.na(impact$LUTMTemplate)]))
    impact$LUTMTemplate[is.na(impact$LUTMTemplate)]<-namaVariabel
    
    # matrix E
    impact$matrixE<-matrix(NA,nrow = 46, ncol = jumlahVariabel)
    colnames(impact$matrixE)<-namaVariabel
    variabel_x<-list()
    variabel_y<-list()
    for (a in 1:nrow(impact$LUTMTemplate)){  ## constraint 1
      variabel_x[[a]]<-t(impact$LUTMTemplate[a,])[t(impact$LUTMTemplate[a,])!= 0]
      eval(parse(text=paste0("variabel_x_",a,"<-NULL")))
      eval(parse(text=paste0("variabel_x_",a,"<-variabel_x[[",a,"]]")))
      for (i in 1:length(variabel_x[[a]])){
        if(!identical(variabel_x[[a]],c(numeric(0), character(0),integer(0)))){
          eval(parse(text=paste0("impact$matrixE[",a,",paste0(variabel_x_",a,"[",i,"])]<-1")))
          # impact$matrixE[a,paste0(variabel_n[i])]<-1
        } else {impact$matrixE[a,]<-0}
      }
    }
    for (a in 1:ncol(impact$LUTMTemplate)){  ##constraint 2
      variabel_y[[a]]<-t(impact$LUTMTemplate[,a])[t(impact$LUTMTemplate[,a])!= 0]
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
      impact$matrixE<- rbind(impact$matrixE, additionalE)
    }


    # matrix F
    impact$matrixF<-rbind(as.matrix(bauSeriesOfImpactLand[[paste0("y", projectionYear-1)]][["landCover"]][["luas.land.use"]]),as.matrix(impact$landCover[,1]))
    impact$matrixF<- as.matrix(impact$matrixF[!(rowSums(impact$matrixF) == 0),])
    if (is.null(additionalF)){
      impact$matrixF<-impact$matrixF
    } else{
      impact$matrixF<- rbind(impact$matrixF, additionalF)
    }

    # matrix G
    diagonalVariabel_pre<-matrix(NA, ncol=1, nrow=ncol(impact$LUTMTemplate))  ## cari nama variabel diagonal LUTM
    for (i in 1:ncol(impact$LUTMTemplate)){
      diagonalVariabel_pre[i,1]<-impact$LUTMTemplate[i,i]
    }
    diagonalVariabel<-diagonalVariabel_pre[!(diagonalVariabel_pre==0),]

    impact$matrixG<-rbind(diag(nrow=(jumlahVariabel)), matrix(0, nrow=length(diagonalVariabel),ncol=jumlahVariabel))  ## buat matrix G constraint 1 & 2
    colnames(impact$matrixG)<-namaVariabel
    for (i in 1:length(diagonalVariabel)){
      impact$matrixG[jumlahVariabel+i,diagonalVariabel[i]]<-1   #assign 1 untuk semua variabel diagonal
    }
    if (is.null(additionalG)){
      impact$matrixG<-impact$matrixG
    } else{
      impact$matrixG<- rbind(impact$matrixG, additionalG)
    }

    # matrix H
    impact$matrixH<-rbind(matrix(0,nrow=jumlahVariabel,ncol=1),as.matrix(impact$landCover[(diagonalVariabel_pre!= 0),1]*0.1))  ## persen variabel diagonal yang akan dipakai
    if (is.null(additionalH)){
      impact$matrixH<-impact$matrixH
    } else{
      impact$matrixH<- rbind(impact$matrixH, additionalH)
    }

    impact$LUTM<-LUTM_his
    
    # LUTM dengan metode LSEI
    variabelLSEI<-lsei(E = impact$matrixE, F = impact$matrixF, G=impact$matrixG, H=impact$matrixH)
    variabelLSEI<-as.matrix(unlist(variabelLSEI))
    variabelLSEI<-as.matrix(as.numeric(variabelLSEI[1:jumlahVariabel,]))
    row.names(variabelLSEI)<-namaVariabel
    impact$LUTM<-as.matrix(impact$LUTMTemplate)
    for (a in 1:nrow(impact$LUTM)){
      for(b in 1:ncol(impact$LUTM)){
        if (impact$LUTM[a,b]==0){
          impact$LUTM[a,b]<-as.numeric(0)
        } else {impact$LUTM[a,b]<-as.numeric(variabelLSEI[paste0(impact$LUTMTemplate[a,b]),1])
        }
      }
    }
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

  impact$emission<-as.matrix(colSums(impact$emission))

  impact$emission<-as.matrix(LDMProp_his) %*% impact$emission

  
  
  # rapikan 
  impact$landReq <- data.frame(c(rownames(ioSector), nrow(ioSector)+1),
                               c(as.character(ioSector[,1]), "lainnya (tidak menghasilkan output)"),
                               impact$landReq, stringsAsFactors = FALSE)
                                        
  colnames(impact$landReq)<-c("id.sector", "sector", "land.requirement")

  impact$landCover <- data.frame(as.character(1:23),
                                 colnames(LDMProp_his),
                                 impact$landCover[,1],stringsAsFactors=FALSE)
  colnames(impact$landCover)<-c("id.land.use", "land.use", "luas.land.use")

  impact$LUTM <- data.frame(as.character(1:23),
                            colnames(LDMProp_his),
                            impact$LUTM,stringsAsFactors=FALSE)
  colnames(impact$LUTM)<-c("id.land.use", "land.use", colnames(LDMProp_his))

  impact$emission <- data.frame(c(rownames(ioSector), nrow(ioSector)+1),
                                c(as.character(ioSector[,1]), "lainnya (tidak menghasilkan output"),
                                impact$emission,stringsAsFactors=FALSE)
  colnames(impact$emission)<-c("id.sector", "sector", "emission")

  
  
  return(impact)
  
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
bauSeriesOfImpactLand<-list()

# Historical consumption and emission data
bauSeriesOfIntermediateDemand$y2015 <- matrixIoIntermediateDemand
bauSeriesOfAddedValue$y2015 <- matrixIoAddedValue
bauSeriesOfFinalDemandComponent$y2015 <- matrixIoFinalDemand
bauSeriesOfImpactLabour$y2015 <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))
bauSeriesOfImpactEnergy$y2015 <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)
bauSeriesOfImpactWaste$y2015 <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)
bauSeriesOfImpactAgriculture$y2015 <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)
bauSeriesOfImpactLand$y2015  <- functionSatelliteLand(type= 'historis', 
                                                      LRCRate=LRCRate_2,
                                                      matrix_output=as.matrix(ioTotalOutput),
                                                      inputLandCover=NULL,
                                                      LUTMTemplate=LUTMTemplate,
                                                      additionalE=NULL,
                                                      additionalF=NULL,
                                                      additionalG=NULL,
                                                      additionalH=NULL,
                                                      LUTMChange=NULL)

growthRateSeries <- growthRate
growthRateSeries$Lap_usaha <- NULL
growthRateSeries <- as.matrix(1+growthRateSeries)

projectionYear <- initialYear
listYear <- paste0("y", ioPeriod)
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
  
  # Impact land projection
  eval(parse(text= paste0("bauSeriesOfImpactLand$", timeStep, " <- functionSatelliteLand(type='projection', 
                                                                                        LRCRate=LRCRate_2,
                                                                                        matrix_output=as.matrix(projectionOutput),
                                                                                        inputLandCover=NULL,
                                                                                        LUTMTemplate=LUTMTemplate,
                                                                                        additionalE=NULL,
                                                                                        additionalF=NULL,
                                                                                        additionalG=NULL,
                                                                                        additionalH=NULL,
                                                                                        LUTMChange=NULL,)")))

  listYear <- c(listYear, timeStep)
  projectionYear <- initialYear+step
  
}

colnames(bauSeriesOfOutput) <- as.character(listYear)

bauSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), bauSeriesOfFinalDemand)
colnames(bauSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear)) 


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
resultLandReq <- bauSeriesOfImpactLand[[2]][["landReq"]]
resultLandReq$year <- initialYear
resultLandReq <-resultLandReq[,c("year", names(bauSeriesOfImpactLand[[2]][["landReq"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLand[[t+2]][["landReq"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(resultLandReq)]
  resultLandReq <- data.frame(rbind(resultLandReq, add.row), stringsAsFactors = FALSE)
}

# 12. Land Cover
resultLandCover <- bauSeriesOfImpactLand[[2]][["landCover"]]
resultLandCover$year <- initialYear
resultLandCover <-resultLandCover[,c("year", names(bauSeriesOfImpactLand[[2]][["landCover"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLand[[t+2]][["landCover"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(resultLandCover)]
  resultLandCover <- data.frame(rbind(resultLandCover, add.row), stringsAsFactors = FALSE)
}

# 13. LUTM
resultLUTM <- bauSeriesOfImpactLand[[2]][["LUTM"]]
resultLUTM$year <- initialYear
resultLUTM <-resultLUTM[,c("year", names(bauSeriesOfImpactLand[[2]][["LUTM"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLand[[t+2]][["LUTM"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(resultLUTM)]
  resultLUTM <- data.frame(rbind(resultLUTM, add.row), stringsAsFactors = FALSE)
}

# 14. Land Emission by sector 

resultLandEmission <- bauSeriesOfImpactLand[[2]][["emission"]]
resultLandEmission$year <- initialYear
resultLandEmission <-resultLandEmission[,c("year", names(bauSeriesOfImpactLand[[2]][["emission"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(bauSeriesOfImpactLand[[t+2]][["emission"]])
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

resultTotalGDP <- colSums(bauSeriesOfGDP[,2:16])
bauAllResult <- subset(resultTotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
bauAllResult <- cbind(bauAllResult, resultTotalGDP)
bauAllResult$EmissionIntensity <- bauAllResult$TotalEmission / bauAllResult$resultTotalGDP

ggplot(data=bauAllResult, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
ggplot(data=bauAllResult, aes(x=Year, y=CummulativeEmission, group=1)) + geom_line() + geom_point()
ggplot(data=bauAllResult, aes(x=Year, y=EmissionIntensity, group=1)) + geom_line() + geom_point()
ggplot(data=bauAllResult, aes(x=Year, y=resultTotalGDP, group=1)) + geom_line() + geom_point()
###END: BAU####


###BEGIN: Scenario energy & transportation simulation####
# ENERGI: PLTM on grid
scenario1EnergyFD <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/17_final_demand_proyeksi_sken1_d.csv", header = T, sep = ",")
scenario1SeriesOfOutput <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
scenario1SeriesOfGDP <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
proportionGDP <- bauSeriesOfGDP$y2015 / ioTotalOutput

# 1. calculate new output and satellite based on intervention on final demand
scenario1SeriesOfImpactEnergy <- list()
for(i in 0:iteration){
  t_curr <- initialYear + i
  eval(parse(text=paste0("scenarioFD <- scenario1EnergyFD$y", t_curr))) 
  
  scenarioOutput <- ioLeontiefInverse %*% scenarioFD
  scenario1SeriesOfOutput <- cbind(scenario1SeriesOfOutput, scenarioOutput)
  
  scenarioGDP <- scenarioOutput * proportionGDP
  scenario1SeriesOfGDP <- cbind(scenario1SeriesOfGDP, scenarioGDP)
  
  eval(parse(text= paste0("scenario1SeriesOfImpactEnergy$y", t_curr, " <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(scenarioOutput), emission_factor = emissionFactorEnergy)")))
}
colnames(scenario1SeriesOfOutput)[3:17] <- as.character(listYear[2:16])
colnames(scenario1SeriesOfGDP)[3:17] <- as.character(listYear[2:16])

# 2. next do intervention on satellite
scenario1EnergySE1 <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/18_persentase_bahan_bakar_sken1_tahap1.csv", header = F, sep = ",")
for(t in 2017:2026){
  scenarioConsumption <- scenario1SeriesOfImpactEnergy[[paste0("y", t)]][["consumption"]]
  scenario1SeriesOfImpactEnergy[[paste0("y", t)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * scenario1EnergySE1
}

scenario1EnergySE2 <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/18_persentase_bahan_bakar_sken1_tahap2.csv", header = F, sep = ",")
for(u in 2027:2030){
  scenarioConsumption <- scenario1SeriesOfImpactEnergy[[paste0("y", u)]][["consumption"]]
  scenario1SeriesOfImpactEnergy[[paste0("y", u)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * scenario1EnergySE2
}

# Energy cons
resultS1EnergyConsumption <- scenario1SeriesOfImpactEnergy[[1]][[1]]
resultS1EnergyConsumption$year <- initialYear
resultS1EnergyConsumption <- resultS1EnergyConsumption[, c("year", names(scenario1SeriesOfImpactEnergy[[1]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario1SeriesOfImpactEnergy[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultS1EnergyConsumption)]
  resultS1EnergyConsumption <- data.frame(rbind(resultS1EnergyConsumption, add.row), stringsAsFactors = FALSE)
}
names(resultS1EnergyConsumption)[2:3] <- c("id.sector", "sector")

# Energy emission 
resultS1EnergyEmission <- scenario1SeriesOfImpactEnergy[[1]][[2]]
resultS1EnergyEmission$year <- initialYear
resultS1EnergyEmission <- resultS1EnergyEmission[, c("year", names(scenario1SeriesOfImpactEnergy[[1]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario1SeriesOfImpactEnergy[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultS1EnergyEmission)]
  resultS1EnergyEmission <- data.frame(rbind(resultS1EnergyEmission, add.row), stringsAsFactors = FALSE)
}
names(resultS1EnergyEmission)[2:3] <- c("id.sector", "sector")

resultS1TotalEmission <- data.frame(Year=initialYear:finalYear)
emissionEnergyConsumption <- numeric()
emissionWasteDisposal <- numeric()
emissionFertilizer <- numeric()
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- sum(resultS1EnergyEmission[resultS1EnergyEmission$year==t_curr, "Temission"])
  add_MWdisp <- sum(resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"])
  add_MF <- sum(resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"])
  emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
  emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
  emissionFertilizer <- c(emissionFertilizer, add_MF)
}
resultS1TotalEmission$emissionEnergyCons <- emissionEnergyConsumption
resultS1TotalEmission$emissionWasteDisp <- emissionWasteDisposal
resultS1TotalEmission$emissionFert <- emissionFertilizer
resultS1TotalEmission$TotalEmission <- rowSums(resultS1TotalEmission[, 2:ncol(resultS1TotalEmission)])
resultS1TotalEmission$CummulativeEmission <- cumsum(resultS1TotalEmission$TotalEmission)

# BAU emission[economic sector, years]
scenario1SeriesOfEmissionBySector <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- resultS1EnergyEmission[resultS1EnergyEmission$year==t_curr, "Temission"]
  add_MWdisp <- resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"]
  add_MF <- resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"]
  eval(parse(text=paste0("scenario1SeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF")))
}

resultS1TotalGDP <- colSums(scenario1SeriesOfGDP[,3:17])
scenario1Result <- subset(resultS1TotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
scenario1Result <- cbind(scenario1Result, resultS1TotalGDP)
scenario1Result$EmissionIntensity <- scenario1Result$TotalEmission / scenario1Result$resultS1TotalGDP

ggplot(data=scenario1Result, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
ggplot(data=scenario1Result, aes(x=Year, y=CummulativeEmission, group=1)) + geom_line() + geom_point()
ggplot(data=scenario1Result, aes(x=Year, y=EmissionIntensity, group=1)) + geom_line() + geom_point()
ggplot(data=scenario1Result, aes(x=Year, y=resultTotalGDP, group=1)) + geom_line() + geom_point()


###END: Energy & transportation####


###BEGIN: Scenario waste simulation####
# Scenario 1

###END: Waste####


###BEGIN: Scenario fertilizer simulation####
# ENERGI: PLTM on grid
scenario1EnergyFD <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/17_final_demand_proyeksi_sken1_d.csv", header = T, sep = ",")
scenario1SeriesOfOutput <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
scenario1SeriesOfGDP <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
proportionGDP <- bauSeriesOfGDP$y2015 / ioTotalOutput

# 1. calculate new output and satellite based on intervention on final demand
scenario1SeriesOfImpactEnergy <- list()
for(i in 0:iteration){
  t_curr <- initialYear + i
  eval(parse(text=paste0("scenarioFD <- scenario1EnergyFD$y", t_curr))) 
  
  scenarioOutput <- ioLeontiefInverse %*% scenarioFD
  scenario1SeriesOfOutput <- cbind(scenarioSeriesOfOutput, scenarioOutput)
  
  scenarioGDP <- scenarioOutput * proportionGDP
  scenario1SeriesOfGDP <- cbind(scenarioSeriesOfGDP, scenarioGDP)
  
  eval(parse(text= paste0("scenario1SeriesOfImpactEnergy$y", t_curr, " <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(scenarioOutput), emission_factor = emissionFactorEnergy)")))
}

# 2. next do intervention on satellite
scenario1EnergySE1 <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/18_persentase_bahan_bakar_sken1_tahap1.csv", header = F, sep = ",")
for(t in 2017:2026){
  scenarioConsumption <- scenario1SeriesOfImpactEnergy[[paste0("y", t)]][["consumption"]]
  scenario1SeriesOfImpactEnergy[[paste0("y", t)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * scenario1EnergySE1
}

scenario1EnergySE2 <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/18_persentase_bahan_bakar_sken1_tahap2.csv", header = F, sep = ",")
for(u in 2027:2030){
  scenarioConsumption <- scenario1SeriesOfImpactEnergy[[paste0("y", u)]][["consumption"]]
  scenario1SeriesOfImpactEnergy[[paste0("y", u)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * scenario1EnergySE2
}





scenarioEnergyFD1 <- read.table("D:/My_Development/RProjects/lcd-scenario/raw/jabar_in_redcluwe/17_final_demand_proyeksi_sken1_d.csv", header = T, sep = ",")

###END: fertilizer####


###BEGIN: Scenario land-based simulation####
# ENERGI: PLTM on grid
scenario1EnergyFD <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/17_final_demand_proyeksi_sken1_d.csv", header = T, sep = ",")
scenario1SeriesOfOutput <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
scenario1SeriesOfGDP <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
proportionGDP <- bauSeriesOfGDP$y2015 / ioTotalOutput

# 1. calculate new output and satellite based on intervention on final demand
scenario1SeriesOfImpactEnergy <- list()
for(i in 0:iteration){
  t_curr <- initialYear + i
  eval(parse(text=paste0("scenarioFD <- scenario1EnergyFD$y", t_curr))) 
  
  scenarioOutput <- ioLeontiefInverse %*% scenarioFD
  scenario1SeriesOfOutput <- cbind(scenarioSeriesOfOutput, scenarioOutput)
  
  scenarioGDP <- scenarioOutput * proportionGDP
  scenario1SeriesOfGDP <- cbind(scenarioSeriesOfGDP, scenarioGDP)
  
  eval(parse(text= paste0("scenario1SeriesOfImpactEnergy$y", t_curr, " <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(scenarioOutput), emission_factor = emissionFactorEnergy)")))
}

# 2. next do intervention on satellite
scenario1EnergySE1 <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/18_persentase_bahan_bakar_sken1_tahap1.csv", header = F, sep = ",")
for(t in 2017:2026){
  scenarioConsumption <- scenario1SeriesOfImpactEnergy[[paste0("y", t)]][["consumption"]]
  scenario1SeriesOfImpactEnergy[[paste0("y", t)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * scenario1EnergySE1
}

scenario1EnergySE2 <- read.table("D:/My_Development/RProjects/lcd-scenario/_DB/18_persentase_bahan_bakar_sken1_tahap2.csv", header = F, sep = ",")
for(u in 2027:2030){
  scenarioConsumption <- scenario1SeriesOfImpactEnergy[[paste0("y", u)]][["consumption"]]
  scenario1SeriesOfImpactEnergy[[paste0("y", u)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * scenario1EnergySE2
}





scenarioEnergyFD1 <- read.table("D:/My_Development/RProjects/lcd-scenario/raw/jabar_in_redcluwe/17_final_demand_proyeksi_sken1_d.csv", header = T, sep = ",")

###END: land-based####



###BEGIN: Scenario waste-fertilizer simulation####
# ENERGI: PLTM on grid
scenario1CombFD <- read.table("D:/My_Development/RProjects/lcd-scenario/_AN/fd_limbah_pertanian.csv", header = T, sep = ",")
ef_fert <- read.table("D:/My_Development/RProjects/lcd-scenario/_AN/EF_Scen2.csv", header = T, sep = ",")
scenario1CombFD <- scenario1CombFD[,2:16] 
colnames(scenario1CombFD) <- as.character(listYear[2:16])
scenario1CombSeriesOfOutput <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
scenario1CombSeriesOfGDP <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
proportionGDP <- bauSeriesOfGDP$y2015 / ioTotalOutput

# 1. calculate new output and satellite based on intervention on final demand
scenario1CombSeriesOfImpactWaste <- list()
scenario1CombSeriesOfImpactFertilizer <- list()
for(i in 0:iteration){
  t_curr <- initialYear + i
  eval(parse(text=paste0("scenarioFD <- scenario1CombFD$y", t_curr))) 
  
  scenarioOutput <- ioLeontiefInverse %*% scenarioFD
  scenario1CombSeriesOfOutput <- cbind(scenario1CombSeriesOfOutput, scenarioOutput)
  
  scenarioGDP <- scenarioOutput * proportionGDP
  scenario1CombSeriesOfGDP <- cbind(scenario1CombSeriesOfGDP, scenarioGDP)
  
  eval(parse(text= paste0("scenario1CombSeriesOfImpactWaste$y", t_curr, " <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(scenarioOutput), emission_factor = emissionFactorWaste)")))
  eval(parse(text= paste0("scenario1CombSeriesOfImpactFertilizer$y", t_curr, " <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(scenarioOutput), emission_factor = ef_fert)")))
}
colnames(scenario1CombSeriesOfOutput)[3:17] <- as.character(listYear[2:16])
colnames(scenario1CombSeriesOfGDP)[3:17] <- as.character(listYear[2:16])

# 2. next do intervention on satellite
for(t in 2016:2030){
  eval(parse(text= paste0("intervensi <- read.table('D:/My_Development/RProjects/lcd-scenario/_YK/raw/input_limbah/intervensi", t, ".csv', header = T, sep = ';')")))
  scenarioConsumption <- scenario1CombSeriesOfImpactWaste[[paste0("y", t)]][["consumption"]]
  scenario1CombSeriesOfImpactWaste[[paste0("y", t)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * intervensi[,3:18]
}

inter_fert <- read.table("D:/My_Development/RProjects/lcd-scenario/_AN/fert_scen.csv", header = T, sep = ",")
for(u in 2016:2030){
  scenarioConsumption <- scenario1CombSeriesOfImpactFertilizer[[paste0("y", u)]][["consumption"]]
  scenario1CombSeriesOfImpactFertilizer[[paste0("y", u)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * inter_fert
}

# Energy cons
resultCombFertConsumption <- scenario1CombSeriesOfImpactFertilizer[[1]][[1]]
resultCombFertConsumption$year <- initialYear
resultCombFertConsumption <- resultCombFertConsumption[, c("year", names(scenario1CombSeriesOfImpactFertilizer[[1]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario1CombSeriesOfImpactFertilizer[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombFertConsumption)]
  resultCombFertConsumption <- data.frame(rbind(resultCombFertConsumption, add.row), stringsAsFactors = FALSE)
}
names(resultCombFertConsumption)[2:3] <- c("id.sector", "sector")

# Energy emission 
resultCombFertEmission <- scenario1CombSeriesOfImpactFertilizer[[1]][[2]]
resultCombFertEmission$year <- initialYear
resultCombFertEmission <- resultCombFertEmission[, c("year", names(scenario1CombSeriesOfImpactFertilizer[[1]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario1CombSeriesOfImpactFertilizer[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombFertEmission)]
  resultCombFertEmission <- data.frame(rbind(resultCombFertEmission, add.row), stringsAsFactors = FALSE)
}
names(resultCombFertEmission)[2:3] <- c("id.sector", "sector")

# Energy cons
resultCombWasteConsumption <- scenario1CombSeriesOfImpactWaste[[1]][[1]]
resultCombWasteConsumption$year <- initialYear
resultCombWasteConsumption <- resultCombWasteConsumption[, c("year", names(scenario1CombSeriesOfImpactWaste[[1]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario1CombSeriesOfImpactWaste[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombWasteConsumption)]
  resultCombWasteConsumption <- data.frame(rbind(resultCombWasteConsumption, add.row), stringsAsFactors = FALSE)
}
names(resultCombWasteConsumption)[2:3] <- c("id.sector", "sector")

# Energy emission 
resultCombWasteEmission <- scenario1CombSeriesOfImpactWaste[[1]][[2]]
resultCombWasteEmission$year <- initialYear
resultCombWasteEmission <- resultCombWasteEmission[, c("year", names(scenario1CombSeriesOfImpactWaste[[1]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario1CombSeriesOfImpactWaste[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombWasteEmission)]
  resultCombWasteEmission <- data.frame(rbind(resultCombWasteEmission, add.row), stringsAsFactors = FALSE)
}
names(resultCombWasteEmission)[2:3] <- c("id.sector", "sector")

resultCombTotalEmission <- data.frame(Year=initialYear:finalYear)
emissionEnergyConsumption <- numeric()
emissionWasteDisposal <- numeric()
emissionFertilizer <- numeric()
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- sum(resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"])
  add_MWdisp <- sum(resultCombWasteEmission[resultCombWasteEmission$year==t_curr, "Temission"])
  add_MF <- sum(resultCombFertEmission[resultCombFertEmission$year==t_curr, "Temission"])
  emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
  emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
  emissionFertilizer <- c(emissionFertilizer, add_MF)
}
resultCombTotalEmission$emissionEnergyCons <- emissionEnergyConsumption
resultCombTotalEmission$emissionWasteDisp <- emissionWasteDisposal
resultCombTotalEmission$emissionFert <- emissionFertilizer
resultCombTotalEmission$TotalEmission <- rowSums(resultCombTotalEmission[, 2:ncol(resultCombTotalEmission)])
resultCombTotalEmission$CummulativeEmission <- cumsum(resultCombTotalEmission$TotalEmission)

# BAU emission[economic sector, years]
scenarioCombSeriesOfEmissionBySector <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"]
  add_MWdisp <- resultCombWasteEmission[resultCombWasteEmission$year==t_curr, "Temission"]
  add_MF <- resultCombFertEmission[resultCombFertEmission$year==t_curr, "Temission"]
  eval(parse(text=paste0("scenarioCombSeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF")))
}

resultCombTotalGDP <- colSums(scenario1CombSeriesOfGDP[,3:17])
scenarioCombResult <- subset(resultCombTotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
scenarioCombResult <- cbind(scenarioCombResult, resultCombTotalGDP)
scenarioCombResult$EmissionIntensity <- scenarioCombResult$TotalEmission / scenarioCombResult$resultCombTotalGDP

write.table(scenarioCombSeriesOfEmissionBySector, "limbah_pertanian_emisi.csv", row.names = F, sep=",")
write.table(scenario1CombSeriesOfGDP, "limbah_pertanian_gdp.csv", row.names = F, sep=",")
write.table(scenarioCombResult, "limbah_pertanian_scen.csv", row.names = F, sep=",")

###END: waste-fertilizer####





###BEGIN: Scenario energy-waste-fertilizer simulation####
scenario2CombFD <- read.table("D:/My_Development/RProjects/lcd-scenario/_AN/fd_limbah_pertanian_energi.csv", header = T, sep = ",")
ef_fert <- read.table("D:/My_Development/RProjects/lcd-scenario/_AN/EF_Scen2.csv", header = T, sep = ",")
scenario2CombFD <- scenario2CombFD[,2:16] 
colnames(scenario2CombFD) <- as.character(listYear[2:16])
scenario2CombSeriesOfOutput <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
scenario2CombSeriesOfGDP <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
proportionGDP <- bauSeriesOfGDP$y2015 / ioTotalOutput

# 1. calculate new output and satellite based on intervention on final demand
scenario2CombSeriesOfImpactWaste <- list()
scenario2CombSeriesOfImpactEnergy <- list()
scenario2CombSeriesOfImpactFertilizer <- list()
for(i in 0:iteration){
  t_curr <- initialYear + i
  eval(parse(text=paste0("scenarioFD <- scenario2CombFD$y", t_curr))) 
  
  scenarioOutput <- ioLeontiefInverse %*% scenarioFD
  scenario2CombSeriesOfOutput <- cbind(scenario2CombSeriesOfOutput, scenarioOutput)
  
  scenarioGDP <- scenarioOutput * proportionGDP
  scenario2CombSeriesOfGDP <- cbind(scenario2CombSeriesOfGDP, scenarioGDP)
  
  eval(parse(text= paste0("scenario2CombSeriesOfImpactWaste$y", t_curr, " <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(scenarioOutput), emission_factor = emissionFactorWaste)")))
  eval(parse(text= paste0("scenario2CombSeriesOfImpactEnergy$y", t_curr, " <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(scenarioOutput), emission_factor = emissionFactorEnergy)")))
  eval(parse(text= paste0("scenario2CombSeriesOfImpactFertilizer$y", t_curr, " <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(scenarioOutput), emission_factor = ef_fert)")))
}
colnames(scenario2CombSeriesOfOutput)[3:17] <- as.character(listYear[2:16])
colnames(scenario2CombSeriesOfGDP)[3:17] <- as.character(listYear[2:16])

# 2. next do intervention on satellite
for(t in 2016:2030){
  eval(parse(text= paste0("intervensi <- read.table('D:/My_Development/RProjects/lcd-scenario/_YK/raw/input_limbah/intervensi", t, ".csv', header = T, sep = ';')")))
  scenarioConsumption <- scenario2CombSeriesOfImpactWaste[[paste0("y", t)]][["consumption"]]
  scenario2CombSeriesOfImpactWaste[[paste0("y", t)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * intervensi[,3:18]
}

inter_fert <- read.table("D:/My_Development/RProjects/lcd-scenario/_AN/fert_scen.csv", header = T, sep = ",")
for(u in 2016:2030){
  scenarioConsumption <- scenario2CombSeriesOfImpactFertilizer[[paste0("y", u)]][["consumption"]]
  scenario2CombSeriesOfImpactFertilizer[[paste0("y", u)]][["consumption"]][4:ncol(scenarioConsumption)] <-  scenarioConsumption[4:ncol(scenarioConsumption)] * inter_fert
}

# Energy cons
resultCombFertConsumption <- scenario2CombSeriesOfImpactFertilizer[[1]][[1]]
resultCombFertConsumption$year <- initialYear
resultCombFertConsumption <- resultCombFertConsumption[, c("year", names(scenario2CombSeriesOfImpactFertilizer[[1]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario2CombSeriesOfImpactFertilizer[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombFertConsumption)]
  resultCombFertConsumption <- data.frame(rbind(resultCombFertConsumption, add.row), stringsAsFactors = FALSE)
}
names(resultCombFertConsumption)[2:3] <- c("id.sector", "sector")

# Energy emission 
resultCombFertEmission <- scenario2CombSeriesOfImpactFertilizer[[1]][[2]]
resultCombFertEmission$year <- initialYear
resultCombFertEmission <- resultCombFertEmission[, c("year", names(scenario2CombSeriesOfImpactFertilizer[[1]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario2CombSeriesOfImpactFertilizer[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombFertEmission)]
  resultCombFertEmission <- data.frame(rbind(resultCombFertEmission, add.row), stringsAsFactors = FALSE)
}
names(resultCombFertEmission)[2:3] <- c("id.sector", "sector")

# Energy cons
resultCombWasteConsumption <- scenario2CombSeriesOfImpactWaste[[1]][[1]]
resultCombWasteConsumption$year <- initialYear
resultCombWasteConsumption <- resultCombWasteConsumption[, c("year", names(scenario2CombSeriesOfImpactWaste[[1]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario2CombSeriesOfImpactWaste[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombWasteConsumption)]
  resultCombWasteConsumption <- data.frame(rbind(resultCombWasteConsumption, add.row), stringsAsFactors = FALSE)
}
names(resultCombWasteConsumption)[2:3] <- c("id.sector", "sector")

# Energy emission 
resultCombWasteEmission <- scenario2CombSeriesOfImpactWaste[[1]][[2]]
resultCombWasteEmission$year <- initialYear
resultCombWasteEmission <- resultCombWasteEmission[, c("year", names(scenario2CombSeriesOfImpactWaste[[1]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario2CombSeriesOfImpactWaste[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombWasteEmission)]
  resultCombWasteEmission <- data.frame(rbind(resultCombWasteEmission, add.row), stringsAsFactors = FALSE)
}
names(resultCombWasteEmission)[2:3] <- c("id.sector", "sector")

# Energy cons
resultCombEnergyConsumption <- scenario2CombSeriesOfImpactEnergy[[1]][[1]]
resultCombEnergyConsumption$year <- initialYear
resultCombEnergyConsumption <- resultCombEnergyConsumption[, c("year", names(scenario2CombSeriesOfImpactEnergy[[1]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario2CombSeriesOfImpactEnergy[[t+1]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombEnergyConsumption)]
  resultCombEnergyConsumption <- data.frame(rbind(resultCombEnergyConsumption, add.row), stringsAsFactors = FALSE)
}
names(resultCombEnergyConsumption)[2:3] <- c("id.sector", "sector")

# Energy emission 
resultCombEnergyEmission <- scenario2CombSeriesOfImpactEnergy[[1]][[2]]
resultCombEnergyEmission$year <- initialYear
resultCombEnergyEmission <- resultCombEnergyEmission[, c("year", names(scenario2CombSeriesOfImpactEnergy[[1]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenario2CombSeriesOfImpactEnergy[[t+1]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(resultCombEnergyEmission)]
  resultCombEnergyEmission <- data.frame(rbind(resultCombEnergyEmission, add.row), stringsAsFactors = FALSE)
}
names(resultCombEnergyEmission)[2:3] <- c("id.sector", "sector")


resultCombTotalEmission <- data.frame(Year=initialYear:finalYear)
emissionEnergyConsumption <- numeric()
emissionWasteDisposal <- numeric()
emissionFertilizer <- numeric()
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- sum(resultCombEnergyEmission[resultCombEnergyEmission$year==t_curr, "Temission"])
  add_MWdisp <- sum(resultCombWasteEmission[resultCombWasteEmission$year==t_curr, "Temission"])
  add_MF <- sum(resultCombFertEmission[resultCombFertEmission$year==t_curr, "Temission"])
  emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
  emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
  emissionFertilizer <- c(emissionFertilizer, add_MF)
}
resultCombTotalEmission$emissionEnergyCons <- emissionEnergyConsumption
resultCombTotalEmission$emissionWasteDisp <- emissionWasteDisposal
resultCombTotalEmission$emissionFert <- emissionFertilizer
resultCombTotalEmission$TotalEmission <- rowSums(resultCombTotalEmission[, 2:ncol(resultCombTotalEmission)])
resultCombTotalEmission$CummulativeEmission <- cumsum(resultCombTotalEmission$TotalEmission)

# BAU emission[economic sector, years]
scenarioCombSeriesOfEmissionBySector <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- resultCombEnergyEmission[resultCombEnergyEmission$year==t_curr, "Temission"]
  add_MWdisp <- resultCombWasteEmission[resultCombWasteEmission$year==t_curr, "Temission"]
  add_MF <- resultCombFertEmission[resultCombFertEmission$year==t_curr, "Temission"]
  eval(parse(text=paste0("scenarioCombSeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF")))
}

resultCombTotalGDP <- colSums(scenario2CombSeriesOfGDP[,3:17])
scenarioCombResult <- subset(resultCombTotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
scenarioCombResult <- cbind(scenarioCombResult, resultCombTotalGDP)
scenarioCombResult$EmissionIntensity <- scenarioCombResult$TotalEmission / scenarioCombResult$resultCombTotalGDP

write.table(scenarioCombSeriesOfEmissionBySector, "limbah_pertanian_energi_emisi.csv", row.names = F, sep=",")
write.table(scenario2CombSeriesOfGDP, "limbah_pertanian_energi_gdp.csv", row.names = F, sep=",")
write.table(scenarioCombResult, "limbah_pertanian_energi_scen.csv", row.names = F, sep=",")

###END: waste-fertilizer####
