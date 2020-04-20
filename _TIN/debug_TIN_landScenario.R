####BEGIN: intervention variables#####
#example: landScenario1: rehabilitasi lahan kritis menjadi hutan lahan kering sekunder
# scenarioFD=read.table("_TIN/landCalc/JaBar/landScen1_findem.csv", header = T, sep = ",") #external input
# scenarioInputLandCover=as.data.frame(read.csv("_TIN/debug/JaBar_landScen1_inputLandCover.csv", header = T, sep = ",")) #external input
# additionalSatelliteEnergy=NULL  
# additionalSatelliteWaste=NULL
# additionalSatelliteAgriculture=NULL
# additionalEmissionFactorEnergy=NULL
# additionalEmissionFactorWaste=NULL
# additionalEmissionFactorAgriculture=NULL

#example: energyScenario: test dari data mba dewi
energyScen= readRDS("_TIN/debug/test_energyScen")
scenarioFD=energyScen[["fdSelisih"]]
scenarioFD=scenarioFD[,2:ncol(scenarioFD)]
scenarioInputLandCover= NULL
additionalSatelliteEnergy=energyScen[["satSelisih"]]  
additionalSatelliteWaste=NULL
additionalSatelliteAgriculture=NULL
additionalEmissionFactorEnergy=NULL
additionalEmissionFactorWaste=NULL
additionalEmissionFactorAgriculture=NULL



####END: intervention variables####
###BEGIN: intervention projection####

# Series of GPD & Output projection
scenarioSeriesOfGDP <- data.frame(Sektor = ioSector[,1], stringsAsFactors = FALSE)
scenarioSeriesOfFinalDemand <- rowSumsMatrixIoFinalDemand
scenarioSeriesOfOutput <- ioTotalOutput

# Series of Intervention Point
scenarioSeriesOfIntermediateDemand <- list()
scenarioSeriesOfAddedValue <- list()
scenarioSeriesOfFinalDemandComponent <- list()
scenarioSeriesOfImpactLabour <- list()
scenarioSeriesOfImpactEnergy <- list()
scenarioSeriesOfImpactWaste <- list()
scenarioSeriesOfImpactAgriculture <- list()
scenarioSeriesOfImpactLand1<-list()
scenarioSeriesOfImpactLand2<-list()
scenarioSeriesOfImpactLand3<-list()


# Historical consumption and emission data
eval(parse(text=paste0("scenarioSeriesOfGDP$y",ioPeriod,"<- analysisGDP")))
eval(parse(text=paste0("scenarioSeriesOfIntermediateDemand$y",ioPeriod," <- matrixIoIntermediateDemand")))
eval(parse(text=paste0("scenarioSeriesOfAddedValue$y",ioPeriod," <- matrixIoAddedValue")))
eval(parse(text=paste0("scenarioSeriesOfFinalDemandComponent$y",ioPeriod," <- matrixIoFinalDemand")))
eval(parse(text=paste0("scenarioSeriesOfImpactLabour$y",ioPeriod," <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))")))
eval(parse(text=paste0("scenarioSeriesOfImpactEnergy$y",ioPeriod," <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)")))
eval(parse(text=paste0("scenarioSeriesOfImpactWaste$y",ioPeriod," <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)")))
eval(parse(text=paste0("scenarioSeriesOfImpactAgriculture$y",ioPeriod," <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)")))


# historical LRC, land requirement, & land cover 
eval(parse(text=paste0("scenarioSeriesOfImpactLand1$y",ioPeriod,"<-functionSatelliteLand1(type= 'historis', matrix_output= as.matrix(ioTotalOutput))")))
# LSEI
eval(parse(text=paste0("scenarioSeriesOfImpactLand2$y",ioPeriod,"<- functionSatelliteLand2(type='historis',carbonStock=carbonStock_his, GDP= as.matrix(bauSeriesOfGDP$y",ioPeriod,"))")))

#projection data
projectionYear <- initialYear
listYear <- paste0("y", ioPeriod)

# economic & impact (energy, waste, & agriculture projection 
for(step in 1:(iteration+1)){
  
  # notes on the year
  timeStep <- paste0("y", projectionYear)
  
  projectionFinalDemand <- bauSeriesOfFinalDemand[,timeStep] + scenarioFD[,timeStep]  # input final demand ditambahkan di sini
  
  scenarioSeriesOfFinalDemand <- cbind(scenarioSeriesOfFinalDemand, projectionFinalDemand)
  projectionOutput <- ioLeontiefInverse %*% projectionFinalDemand 
  scenarioSeriesOfOutput <- cbind(scenarioSeriesOfOutput, projectionOutput)
  
  # add additional values to the list
  eval(parse(text=paste0("scenarioSeriesOfFinalDemandComponent$", timeStep, " <- as.matrix(proportionFinalDemand*projectionFinalDemand)"))) # contains NaN
  eval(parse(text=paste0("scenarioSeriesOfIntermediateDemand$", timeStep, " <-  analysisCT %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
  eval(parse(text=paste0("scenarioSeriesOfAddedValue$", timeStep, " <-  analysisCPI %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
  
  # GDP projection 
  eval(parse(text = paste0("scenarioSeriesOfGDP$", timeStep, "<- colSums(scenarioSeriesOfAddedValue$", timeStep, "[setdiff(1:nrow(matrixIoAddedValue), rowImport),])")))
  
  # Impact projection
  eval(parse(text= paste0("scenarioSeriesOfImpactLabour$", timeStep, " <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(projectionOutput))")))
  eval(parse(text= paste0("scenarioSeriesOfImpactEnergy$", timeStep, " <- functionSatelliteImpact('energy', 
                                                                                                  satellite = satelliteEnergy, 
                                                                                                  matrix_output = as.matrix(projectionOutput), 
                                                                                                  emission_factor = emissionFactorEnergy,
                                                                                                  additional_satellite= additionalSatelliteEnergy[['",timeStep,"']],
                                                                                                  additional_emission_factor=additionalEmissionFactorEnergy[['",timeStep,"']])")))
  eval(parse(text= paste0("scenarioSeriesOfImpactWaste$", timeStep, " <- functionSatelliteImpact('waste', 
                                                                                                 satellite = satelliteWaste, 
                                                                                                 matrix_output = as.matrix(projectionOutput), 
                                                                                                 emission_factor = emissionFactorWaste,
                                                                                                 additional_satellite=additionalSatelliteWaste[['",timeStep,"']], 
                                                                                                 additional_emission_factor=additionalEmissionFactorWaste[['",timeStep,"']])")))
  eval(parse(text= paste0("scenarioSeriesOfImpactAgriculture$", timeStep, " <- functionSatelliteImpact('agriculture', 
                                                                                                        satellite = satelliteAgriculture, 
                                                                                                        matrix_output = as.matrix(projectionOutput), 
                                                                                                        emission_factor = emissionFactorAgriculture, 
                                                                                                        additional_satellite=additionalSatelliteAgriculture[['",timeStep,"']], 
                                                                                                        additional_emission_factor=additionalEmissionFactorAgriculture[['",timeStep,"']])")))
  
  listYear <- c(listYear, timeStep)
  projectionYear <- initialYear+step
  
}

colnames(scenarioSeriesOfOutput) <- as.character(listYear)

scenarioSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), scenarioSeriesOfFinalDemand)
colnames(scenarioSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear)) 

# land cover projection 

# non-advance mode
for (i in 1:2){
  projectionYear <- initialYear
  listYear <- paste0("y", ioPeriod)
  for(step in 1:(iteration+1)){
    # notes on the year
    timeStep <- paste0("y", projectionYear)
    # projection
    eval(parse(text= paste0("scenarioSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                                      matrix_output= as.matrix(scenarioSeriesOfOutput[,'",timeStep,"']), 
                                                                                                      advanceMode = FALSE,
                                                                                                      runNum = ",i,", # input for advanceMode = FALSE
                                                                                                      LRCRate= NULL)")))
    listYear <- c(listYear, timeStep)
    projectionYear <- initialYear+step
  } 
  # jika tidak ada value landCover yang negatif, break loop
  if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){  
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
if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
  repeat{
    # insert UI here to request for new inputLRCRate 
    inputLRCRate<-LRCRate_2  
    projectionYear <- initialYear
    listYear <- paste0("y", ioPeriod)
    for(step in 1:(iteration+1)){
      # notes on the year
      timeStep <- paste0("y", projectionYear)
      eval(parse(text= paste0("scenarioSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(scenarioSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = TRUE,
                                                                                          runNum = NULL,
                                                                                          LRCRate= inputLRCRate)")))
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
    }  
    # jika tidak ada value landCover yang negatif, break loop
    if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){ 
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
  timeStep <- paste0("y", projectionYear)
  for (i in 1:6){   # 5 tipe yg akan dirun otomatis
    eval(parse(text=paste0("scenarioSeriesOfImpactLand3$",timeStep,"<-functionSatelliteLand3(inputLandScen=scenarioInputLandCover,
                                                                                             timeScen='",timeStep,"')")))
    eval(parse(text=paste0("
      scenarioSeriesOfImpactLand2$",timeStep,"<-tryCatch({
      functionSatelliteLand2 (type ='projected',
                              landCoverProjection = as.matrix(scenarioSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]),
                              landCoverProjectionMin = as.matrix(scenarioSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                              inputLandCover = scenarioSeriesOfImpactLand3[['",timeStep,"']][['landCover']], 
                              LUTMTemplate = scenarioSeriesOfImpactLand3[['",timeStep,"']][['LUTMTemplate']], 
                              advanceMode = FALSE,
                              runNum =",i," , 
                              GDP=as.matrix(scenarioSeriesOfGDP$",timeStep,",), 
                              additionalG = scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalG']], 
                              additionalH= scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalH']] 
                              )
      }, warning = function (a){NA}, error = function(b){NA})"
    )))
    if(any(is.na(scenarioSeriesOfImpactLand2[[timeStep]]))==FALSE){  
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

#####END : intervention projection ####
#####BEGIN : intervention visualization ####
# 1. GDP (ind. 1)
scenarioResultGDP <- data.frame(year = 0, sector = "", category="", GDP = 0, stringsAsFactors = FALSE)
# scenarioResultGDP <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
for(c in 2:ncol(scenarioSeriesOfGDP)){
  add.row <- cbind(ioSector, scenarioSeriesOfGDP[, c])
  names(add.row) <- c("sector", "category", "GDP")
  add.row$year <- initialYear + (c-3)
  add.row <- add.row[, colnames(scenarioResultGDP)]
  scenarioResultGDP <- data.frame(rbind(scenarioResultGDP, add.row), stringsAsFactors = FALSE)
}
scenarioResultGDP <- scenarioResultGDP[scenarioResultGDP$year != 0, ] # remove initial values

# 2. Income per capita (ind. 9)
scenarioResultIncomePerCapita <- data.frame(year = 0, Income.per.capita = 0)
for(t in 0:iteration){
  t_curr <- initialYear + t
  pop_curr <- populationProjection[which(populationProjection[, 1] == t_curr), 2]
  inc_curr <- sum(scenarioSeriesOfAddedValue[[t+2]][rowIncome,])
  inc_capita <- inc_curr/pop_curr
  add.row <- data.frame(cbind(t_curr, inc_capita))
  names(add.row) <- names(scenarioResultIncomePerCapita)
  scenarioResultIncomePerCapita <- data.frame(rbind(scenarioResultIncomePerCapita, add.row), stringsAsFactors = FALSE)
}
scenarioResultIncomePerCapita <- scenarioResultIncomePerCapita[scenarioResultIncomePerCapita$year != 0, ]

# 3. Wages or Income (ind. 7)
scenarioResultIncome <- data.frame(year = 0, sector= "", income = 0, stringsAsFactors = FALSE)
sc.name <- ioSector[,1]
for(t in 0:iteration){
  t_curr <- initialYear + t
  inc_curr <- data.frame(scenarioSeriesOfAddedValue[[t+2]][rowIncome,])
  add.row <- data.frame(cbind(t_curr, sc.name, inc_curr), stringsAsFactors = FALSE)
  names(add.row) <- names(scenarioResultIncome)
  scenarioResultIncome <- data.frame(rbind(scenarioResultIncome, add.row), stringsAsFactors = FALSE)
}
scenarioResultIncome <- scenarioResultIncome[scenarioResultIncome$year != 0, ]

# 4. Labour (ind. number 10)
scenarioResultLabour <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
for(t in 0:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactLabour[[t+2]][[1]])
  names(add.row) <- names(scenarioResultLabour)[2:4]
  add.row$year <- t_curr
  add.row <- add.row[, names(scenarioResultLabour)]
  scenarioResultLabour <- data.frame(rbind(scenarioResultLabour, add.row), stringsAsFactors = FALSE)
}
scenarioResultLabour <- scenarioResultLabour[scenarioResultLabour$year != 0, ]

# 5. Energy cons (indicator number 2)
scenarioResultEnergyConsumption <- scenarioSeriesOfImpactEnergy[[2]][[1]]
scenarioResultEnergyConsumption$year <- initialYear
scenarioResultEnergyConsumption <- scenarioResultEnergyConsumption[, c("year", names(scenarioSeriesOfImpactEnergy[[2]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactEnergy[[t+2]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(scenarioResultEnergyConsumption)]
  scenarioResultEnergyConsumption <- data.frame(rbind(scenarioResultEnergyConsumption, add.row), stringsAsFactors = FALSE)
}
names(scenarioResultEnergyConsumption)[2:3] <- c("id.sector", "sector")

# 6. Energy emission (indicator number 3)
scenarioResultEnergyEmission <- scenarioSeriesOfImpactEnergy[[2]][[2]]
scenarioResultEnergyEmission$year <- initialYear
scenarioResultEnergyEmission <- scenarioResultEnergyEmission[, c("year", names(scenarioSeriesOfImpactEnergy[[2]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactEnergy[[t+2]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(scenarioResultEnergyEmission)]
  scenarioResultEnergyEmission <- data.frame(rbind(scenarioResultEnergyEmission, add.row), stringsAsFactors = FALSE)
}
names(scenarioResultEnergyEmission)[2:3] <- c("id.sector", "sector")

# 7. Waste cons (indicator number 2)
scenarioResultWasteDisposal <- scenarioSeriesOfImpactWaste[[2]][[1]]
scenarioResultWasteDisposal$year <- initialYear
scenarioResultWasteDisposal <- scenarioResultWasteDisposal[, c("year", names(scenarioSeriesOfImpactWaste[[2]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactWaste[[t+2]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(scenarioResultWasteDisposal)]
  scenarioResultWasteDisposal <- data.frame(rbind(scenarioResultWasteDisposal, add.row), stringsAsFactors = FALSE)
  
}
names(scenarioResultWasteDisposal)[2:3] <- c("id.sector", "sector")

# 8. Waste emission (indicator number 3)
scenarioResultWasteEmission <- scenarioSeriesOfImpactWaste[[2]][[2]]
scenarioResultWasteEmission$year <- initialYear
scenarioResultWasteEmission <- scenarioResultWasteEmission[, c("year", names(scenarioSeriesOfImpactWaste[[2]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactWaste[[t+2]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(scenarioResultWasteEmission)]
  scenarioResultWasteEmission <- data.frame(rbind(scenarioResultWasteEmission, add.row), stringsAsFactors = FALSE)
}
names(scenarioResultWasteEmission)[2:3] <- c("id.sector", "sector")

# 9. Fertilizer cons (indicator number 2)
scenarioResultFertilizerUsed <- scenarioSeriesOfImpactAgriculture[[2]][[1]]
scenarioResultFertilizerUsed$year <- initialYear
scenarioResultFertilizerUsed <- scenarioResultFertilizerUsed[, c("year", names(scenarioSeriesOfImpactAgriculture[[2]][[1]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactAgriculture[[t+2]][[1]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(scenarioResultFertilizerUsed)]
  scenarioResultFertilizerUsed <- data.frame(rbind(scenarioResultFertilizerUsed, add.row), stringsAsFactors = FALSE)
  
}
names(scenarioResultFertilizerUsed)[2:3] <- c("id.sector", "sector")

# 10. Fertilizer emission (indicator number 3)
scenarioResultFertilizerEmission <- scenarioSeriesOfImpactAgriculture[[2]][[2]]
scenarioResultFertilizerEmission$year <- initialYear
scenarioResultFertilizerEmission <- scenarioResultFertilizerEmission[, c("year", names(scenarioSeriesOfImpactAgriculture[[2]][[2]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactAgriculture[[t+2]][[2]]) # [[2]] for emission
  add.row$year <- t_curr
  add.row <- add.row[, names(scenarioResultFertilizerEmission)]
  scenarioResultFertilizerEmission <- data.frame(rbind(scenarioResultFertilizerEmission, add.row), stringsAsFactors = FALSE)
}
names(scenarioResultFertilizerEmission)[2:3] <- c("id.sector", "sector")

# 11. Land Requirement 
scenarioResultLandReq <- scenarioSeriesOfImpactLand1[[2]][["landReq"]]
scenarioResultLandReq$year <- initialYear
scenarioResultLandReq <-scenarioResultLandReq[,c("year", names(scenarioSeriesOfImpactLand1[[2]][["landReq"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactLand1[[t+2]][["landReq"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(scenarioResultLandReq)]
  scenarioResultLandReq <- data.frame(rbind(scenarioResultLandReq, add.row), stringsAsFactors = FALSE)
}

# 12. Land Cover
scenarioResultLandCover <- scenarioSeriesOfImpactLand2[[2]][["landCover"]]
scenarioResultLandCover$year <- initialYear
scenarioResultLandCover <-scenarioResultLandCover[,c("year", names(scenarioSeriesOfImpactLand2[[2]][["landCover"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactLand2[[t+2]][["landCover"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(scenarioResultLandCover)]
  scenarioResultLandCover <- data.frame(rbind(scenarioResultLandCover, add.row), stringsAsFactors = FALSE)
}

# 13. LUTM
scenarioResultLUTM <- scenarioSeriesOfImpactLand2[[2]][["LUTM"]]
scenarioResultLUTM$year <- initialYear
scenarioResultLUTM <-scenarioResultLUTM[,c("year", names(scenarioSeriesOfImpactLand2[[2]][["LUTM"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactLand2[[t+2]][["LUTM"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(scenarioResultLUTM)]
  scenarioResultLUTM <- data.frame(rbind(scenarioResultLUTM, add.row), stringsAsFactors = FALSE)
}

# 14. Land Emission by sector 

scenarioResultLandEmission <- scenarioSeriesOfImpactLand2[[2]][["emission"]]
scenarioResultLandEmission$year <- initialYear
scenarioResultLandEmission <-scenarioResultLandEmission[,c("year", names(scenarioSeriesOfImpactLand2[[2]][["emission"]]))]
for(t in 1:iteration){
  t_curr <- initialYear + t
  add.row <- data.frame(scenarioSeriesOfImpactLand2[[t+2]][["emission"]])
  add.row$year <- t_curr
  add.row <- add.row[,names(scenarioResultLandEmission)]
  scenarioResultLandEmission <- data.frame(rbind(scenarioResultLandEmission, add.row), stringsAsFactors = FALSE)
} 

# 15. Total Emission
# scenarioResultTotalEmission <- baselineEmission[which(baselineEmission$Year>=initialYear & baselineEmission$Year<= finalYear),]
scenarioResultTotalEmission <- data.frame(Year=initialYear:finalYear)
emissionEnergyConsumption <- numeric()
emissionWasteDisposal <- numeric()
emissionFertilizer <- numeric()
emissionLand <- numeric()
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- sum(scenarioResultEnergyEmission[scenarioResultEnergyEmission$year==t_curr, "Temission"])
  add_MWdisp <- sum(scenarioResultWasteEmission[scenarioResultWasteEmission$year==t_curr, "Temission"])
  add_MF <- sum(scenarioResultFertilizerEmission[scenarioResultFertilizerEmission$year==t_curr, "Temission"])
  add_MLand <-sum(scenarioResultLandEmission[scenarioResultLandEmission$year==t_curr, "emission"])
  emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
  emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
  emissionFertilizer <- c(emissionFertilizer, add_MF)
  emissionLand<-c(emissionLand, add_MLand)
}
scenarioResultTotalEmission$emissionEnergyCons <- emissionEnergyConsumption
scenarioResultTotalEmission$emissionWasteDisp <- emissionWasteDisposal
scenarioResultTotalEmission$emissionFert <- emissionFertilizer
scenarioResultTotalEmission$emissionLand <-emissionLand
scenarioResultTotalEmission$TotalEmission <- rowSums(scenarioResultTotalEmission[, 2:ncol(scenarioResultTotalEmission)])
scenarioResultTotalEmission$CummulativeEmission <- cumsum(scenarioResultTotalEmission$TotalEmission)

# 16. intervention emission[economic sector, years]
scenarioSeriesOfEmissionBySector <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- scenarioResultEnergyEmission[scenarioResultEnergyEmission$year==t_curr, "Temission"]
  add_MWdisp <- scenarioResultWasteEmission[scenarioResultWasteEmission$year==t_curr, "Temission"]
  add_MF <- scenarioResultFertilizerEmission[scenarioResultFertilizerEmission$year==t_curr, "Temission"]
  add_MLand <- scenarioResultLandEmission[c(scenarioResultLandEmission$year==t_curr & scenarioResultLandEmission$sector != "lainnya (tidak menghasilkan output"), "emission"]
  eval(parse(text=paste0("scenarioSeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF + add_MLand")))
}

# scenarioResultTotalGDP <- colSums(scenarioSeriesOfGDP[,2:(ncol(scenarioSeriesOfGDP)-1)])
scenarioAllResult <- subset(scenarioResultTotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
# scenarioAllResult <- cbind(scenarioAllResult, scenarioResultTotalGDP)
scenarioAllResult$resultTotalGDP<-colSums(scenarioSeriesOfGDP[,2:(ncol(scenarioSeriesOfGDP)-1)])
scenarioAllResult$CummulativeGDP <- cumsum(scenarioAllResult$resultTotalGDP)
scenarioAllResult$EmissionIntensity <- scenarioAllResult$TotalEmission / scenarioAllResult$resultTotalGDP
scenarioAllResult$CummulativeEmissionIntensity <-cumsum(scenarioAllResult$EmissionIntensity)

ggplot(data=scenarioAllResult, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
ggplot(data=scenarioAllResult, aes(x=Year, y=CummulativeEmission, group=1)) + geom_line() + geom_point()
ggplot(data=scenarioAllResult, aes(x=Year, y=EmissionIntensity, group=1)) + geom_line() + geom_point()
ggplot(data=scenarioAllResult, aes(x=Year, y=scenarioResultTotalGDP, group=1)) + geom_line() + geom_point()
ggplot(data=scenarioAllResult, aes(x=Year, y=CummulativeGDP, group=1)) + geom_line() + geom_point()
ggplot(data=scenarioAllResult, aes(x=Year, y=CummulativeEmissionIntensity, group=1)) + geom_line() + geom_point()

#####END: intervention visualization####

###### BEGIN: BAU & intervention comparison ####


scenarioAllResult$type <- "SCENARIO"
bauAllResult$type<-"BAU"
comparison<-rbind(scenarioAllResult,bauAllResult)
for (i in colnames(comparisonBAUScenario))
ggplot(comparisonBAUScenario, aes(x=Year, y=CummulativeEmission, group=type))+
  geom_line(aes(color=type))+
  geom_point(aes(color=type))+
  labs(x="Tahun", y="emisi kumulatif")+
  ggtitle("Grafik Kumulatif Emisi")+
  theme(plot.title = element_text(hjust = 0.5))