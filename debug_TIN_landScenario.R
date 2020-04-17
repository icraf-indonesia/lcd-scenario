###BEGIN: BAU projection####

# Series of GPD & Output projection
scenario1LandFD <- read.table("_TIN/landCalc/JaBar/landScen1_findem.csv", header = T, sep = ",")
scenario1LandInputLandCover <-as.data.frame(read.csv("_TIN/debug/JaBar_landScen1_inputLandCover.csv", header = T, sep = ","))
scenario1LandSeriesOfOutput <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
scenario1LandSeriesOfGDP <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
proportionGDP <- bauSeriesOfGDP$y2015 / ioTotalOutput


SeriesOfGDP <- data.frame(Sektor = ioSector[,1], stringsAsFactors = FALSE)
scenario1LandSeriesOfGDP$y2015 <- analysisGDP
scenario1LandSeriesOfFinalDemand <- rowSumsMatrixIoFinalDemand
scenario1LandSeriesOfOutput <- ioTotalOutput

# Series of Intervention Point
scenario1LandSeriesOfIntermediateDemand <- list()
scenario1LandSeriesOfAddedValue <- list()
scenario1LandSeriesOfFinalDemandComponent <- list()
scenario1LandSeriesOfImpactLabour <- list()
scenario1LandSeriesOfImpactEnergy <- list()
scenario1LandSeriesOfImpactWaste <- list()
scenario1LandSeriesOfImpactAgriculture <- list()
scenario1LandSeriesOfImpactLand1<-list()
scenario1LandSeriesOfImpactLand2<-list()
scenario1LandSeriesOfImpactLand3<-list()


# Historical consumption and emission data
scenario1LandSeriesOfIntermediateDemand$y2015 <- matrixIoIntermediateDemand
scenario1LandSeriesOfAddedValue$y2015 <- matrixIoAddedValue
scenario1LandSeriesOfFinalDemandComponent$y2015 <- matrixIoFinalDemand
scenario1LandSeriesOfImpactLabour$y2015 <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))
scenario1LandSeriesOfImpactEnergy$y2015 <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)
scenario1LandSeriesOfImpactWaste$y2015 <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)
scenario1LandSeriesOfImpactAgriculture$y2015 <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)
# historical LRC, land requirement, & land cover 
scenario1LandSeriesOfImpactLand1$y2015<-functionSatelliteLand1(type= 'historis', matrix_output= as.matrix(ioTotalOutput))
# LSEI
scenario1LandSeriesOfImpactLand2$y2015  <- functionSatelliteLand2(type="historis",carbonStock=carbonStock_his, GDP= as.matrix(bauSeriesOfGDP$y2015) )


projectionYear <- initialYear
listYear <- paste0("y", ioPeriod)

# economic & impact (energy, waste, & agriculture projection 
for(step in 1:(iteration+1)){
  
  # notes on the year
  timeStep <- paste0("y", projectionYear)
  
  projectionFinalDemand <- bauSeriesOfFinalDemand[,timeStep] + scenario1LandFD[,timeStep]
  
  scenario1LandSeriesOfFinalDemand <- cbind(scenario1LandSeriesOfFinalDemand, projectionFinalDemand)
  projectionOutput <- ioLeontiefInverse %*% projectionFinalDemand 
  scenario1LandSeriesOfOutput <- cbind(scenario1LandSeriesOfOutput, projectionOutput)
  
  # add additional values to the list
  eval(parse(text=paste0("scenario1LandSeriesOfFinalDemandComponent$", timeStep, " <- as.matrix(proportionFinalDemand*projectionFinalDemand)"))) # contains NaN
  eval(parse(text=paste0("scenario1LandSeriesOfIntermediateDemand$", timeStep, " <-  analysisCT %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
  eval(parse(text=paste0("scenario1LandSeriesOfAddedValue$", timeStep, " <-  analysisCPI %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
  
  # GDP projection 
  eval(parse(text = paste0("scenario1LandSeriesOfGDP$", timeStep, "<- colSums(scenario1LandSeriesOfAddedValue$", timeStep, "[setdiff(1:nrow(matrixIoAddedValue), rowImport),])")))
  
  # Impact projection
  eval(parse(text= paste0("scenario1LandSeriesOfImpactLabour$", timeStep, " <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(projectionOutput))")))
  eval(parse(text= paste0("scenario1LandSeriesOfImpactEnergy$", timeStep, " <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorEnergy)")))
  eval(parse(text= paste0("scenario1LandSeriesOfImpactWaste$", timeStep, " <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorWaste)")))
  eval(parse(text= paste0("scenario1LandSeriesOfImpactAgriculture$", timeStep, " <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorAgriculture)")))
  
  listYear <- c(listYear, timeStep)
  projectionYear <- initialYear+step
  
}

colnames(scenario1LandSeriesOfOutput) <- as.character(listYear)

scenario1LandSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), scenario1LandSeriesOfFinalDemand)
colnames(scenario1LandSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear)) 

# land cover projection 

# non-advance mode
for (i in 1:2){
  projectionYear <- initialYear
  listYear <- paste0("y", ioPeriod)
  for(step in 1:(iteration+1)){
    # notes on the year
    timeStep <- paste0("y", projectionYear)
    # projection
    eval(parse(text= paste0("scenario1LandSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(scenario1LandSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = FALSE,
                                                                                          runNum = ",i,", # input for advanceMode = FALSE
                                                                                          LRCRate= NULL)")))
    listYear <- c(listYear, timeStep)
    projectionYear <- initialYear+step
  } 
  # jika tidak ada value landCover yang negatif, break loop
  if(any(unlist(sapply(scenario1LandSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){  
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
if(any(unlist(sapply(scenario1LandSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
  repeat{
    # insert UI here to request for new inputLRCRate 
    inputLRCRate<-LRCRate_2  
    projectionYear <- initialYear
    listYear <- paste0("y", ioPeriod)
    for(step in 1:(iteration+1)){
      # notes on the year
      timeStep <- paste0("y", projectionYear)
      eval(parse(text= paste0("scenario1LandSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(scenario1LandSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = TRUE,
                                                                                          runNum = NULL,
                                                                                          LRCRate= inputLRCRate)")))
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
    }  
    # jika tidak ada value landCover yang negatif, break loop
    if(any(unlist(sapply(scenario1LandSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){ 
      print("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC yang telah Anda modifikasi") # use as UI textoutput 
      break
    } else {
      print("proyeksi tutupan lahan yang dihasilkan memiliki luasan negatif. Silakan menyunting ulang laju perubahan LRC dan atau kembali ke target permintaan akhir") # use as UI textoutput 
    }
  }
}


# create additional matrix G & H



# LUTM Projection 


for (i in 1:5){   # 5 tipe yg akan dirun otomatis
  
  projectionYear <- initialYear
  listYear <- paste0("y", ioPeriod)
  
  for(step in 1:(iteration+1)){
    timeStep <- paste0("y", projectionYear)
    eval(parse(text=paste0("scenario1LandSeriesOfImpactLand3$",timeStep,"<-functionSatelliteLand3(inputLandScen=as.data.frame(scenario1LandInputLandCover),
                                                                                                  timeScen='",timeStep,"')")))
    eval(parse(text=paste0("
      scenario1LandSeriesOfImpactLand2$",timeStep,"<-tryCatch({
      functionSatelliteLand2 (type ='projected',
                              landCoverProjection = as.matrix(scenario1LandSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]) ,
                              inputLandCover = scenario1LandSeriesOfImpactLand3[['",timeStep,"']][['landCover']], 
                              LUTMTemplate = scenario1LandSeriesOfImpactLand3[['",timeStep,"']][['LUTMTemplate']], 
                              advanceMode = FALSE,
                              runNum =",i," , 
                              GDP=as.matrix(scenario1LandSeriesOfGDP$",timeStep,",), 
                              additionalG = as.matrix(scenario1LandSeriesOfImpactLand3[['",timeStep,"']][['additionalG']]), 
                              additionalH= as.matrix(scenario1LandSeriesOfImpactLand3[['",timeStep,"']][['additionalH']]) 
                              )
      }, warning = function (a){NA}, error = function(b){NA})"
    )))
    listYear <- c(listYear, timeStep)
    projectionYear <- initialYear+step  
  }
  
  if(any(is.na(unlist(scenario1LandSeriesOfImpactLand2)))==FALSE){  
    if(i==1){
      print("use constraint 1 to make LUTM")
    } else if (i==2){
      print("use constraint 2 to make LUTM")
    } else if (i==3){
      print("use constraint 3 to make LUTM")
    } else if (i ==4){
      print("use constraint 4 to make LUTM")
    } else if (i == 5){
      print("use no constraint to make LUTM")
    }
    break
  } else {
    if(i==5){
      print("tidak berhasil menghitung LUTM")
    } 
  }
}

# jika tidak berhasil menghitung LUTM, force to enter advanceMode pada UI (spt pada land cover)

#####END : BAU projection ####