###BEGIN: initiate all variables & function####
# username <- "alfanugraha"
# password <- "1234"
selectedProv = "JaBar"
datapath <- paste0("data/", selectedProv, "/")
# userFolder <- paste0(datapath, username)
# if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)

ioSector <- read.table("D:/My_Development/RProjects/lcd-scenario/raw/jabar_in_redcluwe/01_sektor.csv ", header = F, sep = ",")
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
satelliteAgriculture <- read.table("D:/My_Development/RProjects/lcd-scenario/raw/jabar_in_redcluwe/16_satelit_pertanian.csv", header = T, sep = ",")
emissionFactorEnergy <- readRDS(paste0(datapath, "ef_energy"))
emissionFactorWaste <- read.table("D:/My_Development/RProjects/lcd-scenario/raw/jabar_in_redcluwe/11_faktor_emisi_limbah.csv", header = T, sep = ",")
emissionFactorAgriculture <- read.table("D:/My_Development/RProjects/lcd-scenario/raw/jabar_in_redcluwe/17_faktor_emisi_pertanian.csv", header = T, sep = ",")
population <- readRDS(paste0(datapath, "currentPopulation"))
populationProjection <- readRDS(paste0(datapath, "population"))
baselineEmission <- readRDS(paste0(datapath, "otherEm")) # not used in calculation

growthRate <- read.table("D:/My_Development/RProjects/lcd-scenario/_AN/growth5_1630.csv", header = T, sep = ",")

# to be modified
LU_tahun<-readRDS(paste0(datapath,"LU_tahun"))
LDMProp_his<-readRDS(paste0(datapath,"LDMProp"))
# row.names(LDMProp_his)<-sector[,1]
# landtable <- readRDS(paste0(datapath, "landtable"))
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

# Historical consumption and emission data
bauSeriesOfIntermediateDemand$y2015 <- matrixIoIntermediateDemand
bauSeriesOfAddedValue$y2015 <- matrixIoAddedValue
bauSeriesOfFinalDemandComponent$y2015 <- matrixIoFinalDemand
bauSeriesOfImpactLabour$y2015 <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))
bauSeriesOfImpactEnergy$y2015 <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)
bauSeriesOfImpactWaste$y2015 <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)
bauSeriesOfImpactAgriculture$y2015 <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)

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

# 11. Total Emission
# resultTotalEmission <- baselineEmission[which(baselineEmission$Year>=initialYear & baselineEmission$Year<= finalYear),]
resultTotalEmission <- data.frame(Year=initialYear:finalYear)
emissionEnergyConsumption <- numeric()
emissionWasteDisposal <- numeric()
emissionFertilizer <- numeric()
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- sum(resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"])
  add_MWdisp <- sum(resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"])
  add_MF <- sum(resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"])
  emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
  emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
  emissionFertilizer <- c(emissionFertilizer, add_MF)
}
resultTotalEmission$emissionEnergyCons <- emissionEnergyConsumption
resultTotalEmission$emissionWasteDisp <- emissionWasteDisposal
resultTotalEmission$emissionFert <- emissionFertilizer
resultTotalEmission$TotalEmission <- rowSums(resultTotalEmission[, 2:ncol(resultTotalEmission)])
resultTotalEmission$CummulativeEmission <- cumsum(resultTotalEmission$TotalEmission)

# 12. BAU emission[economic sector, years]
bauSeriesOfEmissionBySector <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
for(t in 0:iteration){
  t_curr <- initialYear + t
  add_MEcons <- resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"]
  add_MWdisp <- resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"]
  add_MF <- resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"]
  eval(parse(text=paste0("bauSeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF")))
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
