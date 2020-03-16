###BEGIN: initiate all variables ####
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
emissionFactorEnergy <- readRDS(paste0(datapath, "ef_energy"))
emissionFactorWaste <- readRDS(paste0(datapath, "ef_waste"))
population <- readRDS(paste0(datapath, "currentPopulation"))
populationProjection <- readRDS(paste0(datapath, "population"))
baselineEmission <- readRDS(paste0(datapath, "otherEm"))

# to be modified
LU_tahun<-readRDS(paste0(datapath,"LU_tahun"))
LDMProp_his<-readRDS(paste0(datapath,"LDMProp"))
# row.names(LDMProp_his)<-sector[,1]
# landtable <- readRDS(paste0(datapath, "landtable"))
GDPAll <- readRDS(paste0(datapath, "GDPAll"))
linkagesTable <- readRDS(paste0(datapath, "linkagesTable"))
multiplierAll <- readRDS(paste0(datapath, "multiplierAll"))
rtffile <- readRDS(paste0(datapath, "rtffile"))

###END: initiate all variables ####


matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
matrixIoAddedValue <- as.matrix(ioAddedValue)
nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
ioDimention <- ncol(ioIntermediateDemand)

colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput 
ioTotalOutputInverse <- 1/ioTotalOutput
ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
ioTotalOutputInverse <- diag(ioTotalOutputInverse)

rowIncome <- 2

## Multiplier Energy Product ##
energyCoef <- ioTotalOutputInverse %*% as.matrix(satelliteEnergy$Total_Kons)
multiplierEnergy <- ioLeontiefInverse %*% energyCoef

## Energy Emission ##
num_ef <- length(emissionFactorEnergy$Em_F)
ef_matrix <- diag(as.vector(emissionFactorEnergy$Em_F), ncol = num_ef, nrow = num_ef)
ef_energy_matrix <- as.matrix(satelliteEnergy[,4:length(satelliteEnergy)]) %*% ef_matrix

### SKENARIO BAU ####

## Tabel GDP Rate ##
sector <- ioSector$V1
inputGDPRate <- 0.05
year <- 2016 : 2030
for (i in 1:length(year)){
  gdpRateTable <- matrix(inputGDPRate,ncol = length(year), nrow = length(sector))
  gdpRateTable <- as.data.frame(gdpRateTable)
  colnames(gdpRateTable) <- year
  gdpRateTable <- cbind(sector, gdpRateTable)
}

## Tabel Proyeksi Final Demand ##

totalFDbau <- data.frame(rowSums(ioFinalDemand))
colnames(totalFDbau) <- "FD_BAU"
# totalFDbau <- cbind(sector, totalFDbau)

gdpRateTable<-data.frame(gdpRateTable[,2:length(gdpRateTable)],
                         stringsAsFactors = FALSE)

proyeksi_FD <- function(gdpRateTable, totalFDbau) {
  for (i in 1:ncol(gdpRateTable)){
    if (i==1){
      gdpRateTable[,i] <-  (1+gdpRateTable[,i]) * totalFDbau
    } else {
      gdpRateTable[,i] <-  gdpRateTable[,i-1] * (1+gdpRateTable[,i])
    }
  }
  return(gdpRateTable)
}

proyeksiFD_t <- proyeksi_FD(gdpRateTable, totalFDbau)
proyeksiFD_t <- data.frame(proyeksiFD_t)
proyeksiFD_t <- cbind(sector, proyeksiFD_t)

## Tabel Proyeksi Output ##

proyeksiFD_matrix <- as.matrix(proyeksiFD_t [,2:length(proyeksiFD_t)])
proyeksiOutput_t <- ioLeontiefInverse %*% proyeksiFD_matrix
sector <- as.data.frame(ioSector$V1)
proyeksiOutput_t <- as.data.frame(cbind(sector,proyeksiOutput_t))

## Tabel Proyeksi PDRB ##

proyeksiPDRB <- GDPAll$P_OUTPUT * (proyeksiOutput_t[,2:length(proyeksiOutput_t)])
colsumPDRBbau<-as.data.frame(colSums(proyeksiPDRB))
colnames(colsumPDRBbau) <- "Total_PDRB"
totalPDRBbau <- as.data.frame(cbind(year,colsumPDRBbau), row.names = 1:length(year))

## Proyeksi BAU Konsumsi Energi ##

# inProyeksiOutput <- "raw/proyeksi_output_jabar.csv"
# proyeksiOutput <- read.table(inProyeksiOutput, header = TRUE, sep = ";")
tablePOutput <- data.frame(proyeksiOutput_t[,2:length(proyeksiOutput_t)],
                           stringsAsFactors = FALSE)
tableKoef <- data.frame(row.names = 1:52,
                        Koef_Energi = energyCoef,
                        stringsAsFactors = FALSE)
tableProyeksi <- function(tablePOutput,tableKoef){
  for (i in 1:ncol(tablePOutput)){
    tablePOutput[,i] <- tableKoef*(tablePOutput[,i])
  }    
  return(tablePOutput)
}
tableProyeksiEnergi<- tableProyeksi(tablePOutput,tableKoef)
year <- 2016:2030
colnames(tableProyeksiEnergi) <- year
sector <- ioSector$V1
proyeksiBAUEnergi <- cbind(sector, tableProyeksiEnergi)

## Proporsi Satelit Energi ##

tablePropEnergi <-satelliteEnergy[,4:length(satelliteEnergy)]/satelliteEnergy$Total_Kons 
tablePropEnergi <- replace(tablePropEnergi, is.na(tablePropEnergi), 0)
sector <- as.data.frame(ioSector$V1)
tablePropEnergi <- cbind(sector, tablePropEnergi)
# tablePropLimbah <- read.table("raw/satelit_limbah.csv", header = TRUE, sep = ";")

## Tabel Proyeksi Konsumsi Energi Tahun X ##

propEnergi <- data.frame(tablePropEnergi[2:length(tablePropEnergi)],
                         stringsAsFactors = FALSE)
#Contoh : Tabel Proyeksi Konsumsi Energi Tahun 2017

## Buat tampilan di SHINY line 151 - 172
totalKonsumsiEnergi_t <- data.frame(proyeksiBAUEnergi$`2017`,
                             stringsAsFactors = FALSE)
tableKonsumsiEnergi <- function(propEnergi, totalKonsumsiEnergi_t){
  for (i in 1:ncol(propEnergi)){
    propEnergi[,i] <- propEnergi[,i] * (totalKonsumsiEnergi_t)
  }
  return(propEnergi)
}
tableKonsumsiEnergi_t <- tableKonsumsiEnergi(propEnergi, totalKonsumsiEnergi_t)
totalKonsumsiEnergi_t <- rowSums(tableKonsumsiEnergi_t)
totalKonsumsiEnergi_t <- sum(totalKonsumsiEnergi_t)

## Tabel Proyeksi Emisi Energi Tahun X ##

#Contoh : Tabel Proyeksi Emisi Energi Tahun 2017

num_input_ef <- length(emissionFactorEnergy$Em_F)
input_ef_matrix <- diag(as.vector(emissionFactorEnergy$Em_F), ncol = num_input_ef, nrow = num_input_ef)
tableEmisiEnergi <- as.matrix(tableKonsumsiEnergi_t) %*% input_ef_matrix
emisiEnergi_t <- as.data.frame(rowSums(tableEmisiEnergi))
colnames(emisiEnergi_t) <- "Total Emisi"
totalEmisiEnergi_t <- sum(emisiEnergi_t)

## Membuat tabel "Total Konsumsi dan Faktor Emisi" tahun awal-akhir ##

eval(parse(text=(paste("tableKonsumsiEnergiAll_",  year, "<-propEnergi * proyeksiBAUEnergi$`", year, "`", sep=""))))
eval(parse(text=(paste("tableEmisiEnergi_", year, "<- as.matrix(tableKonsumsiEnergiAll_", year,")", "%*% input_ef_matrix", sep=""))))
eval(parse(text=(paste("emisiEnergi_", year, "<- as.data.frame(rowSums(tableEmisiEnergi_", year,"))", sep=""))))
eval(parse(text=(paste("totalEmisiEnergi_", year, "<- sum(emisiEnergi_", year, ")", sep=""))))
# Penggabungan total emisi masih manual
## Buat looping Line 182-196
totalEmisiEnergi_All <- rbind(totalEmisiEnergi_2016,
                        totalEmisiEnergi_2017,
                        totalEmisiEnergi_2018,
                        totalEmisiEnergi_2019,
                        totalEmisiEnergi_2020,
                        totalEmisiEnergi_2021,
                        totalEmisiEnergi_2022,
                        totalEmisiEnergi_2023,
                        totalEmisiEnergi_2024,
                        totalEmisiEnergi_2025,
                        totalEmisiEnergi_2026,
                        totalEmisiEnergi_2027,
                        totalEmisiEnergi_2028,
                        totalEmisiEnergi_2029,
                        totalEmisiEnergi_2030)
colnames(totalEmisiEnergi_All) <- "Total_Emisi"
totalEmisiEnergiBAU_All <- as.data.frame(row.names = 1:length(year),
                                   cbind(year,totalEmisiEnergi_All))


### SKENARIO AKSI ####
  ### Aksi 1 : Reformasi sistem transit - BRT System ####
      ## Final Demand ##
      
      findem_BRT <- read.table("_YK/raw/input_transportasi/fd_BRT.csv", header = TRUE, sep =";") #USER INPUT
      findemBRTInt_All <- proyeksiFD_t[2:length(proyeksiFD_t)] + findem_BRT[2:length(findem_BRT)]
      intBRTOutput_All <- ioLeontiefInverse %*% as.matrix(findemBRTInt_All)
      intPDRBBRT_All <- GDPAll$P_OUTPUT * (intBRTOutput_All)
      intColsumPDRB_BRT <- data.frame(colSums(intBRTPDRB_All))
      colnames(intColsumPDRB_BRT) <- "Total_PDRB"
      intTotalPDRB_BRT <- as.data.frame(cbind(year, intColsumPDRB_BRT), row.names = 1:length(year))
      
      ## Tabel Satelit ##
      sector <- ioSector$V1
      eval(parse(text=paste("intervensiEnergi",year, "<-read.table('_YK/raw/input_transportasi/intervensiEnergi",year,".csv', header = TRUE, sep = ';')", sep="")))
      eval(parse(text=(paste("tableKonsumsiEnergi_",  year, "<-propEnergi * proyeksiBAUEnergi$`", year, "`", sep=""))))
      eval(parse(text=(paste("tableKonsumsiEnergiInt_", year, "<- (intervensiEnergi", year,"[,2:length(intervensiEnergi", year,")]) * tableKonsumsiEnergi_", year, sep = ""))))
      eval(parse(text=paste("deltaKonsumsiEnergi_", year, "<- (-1) * (tableKonsumsiEnergiInt_", year, "$GASOLINE - tableKonsumsiEnergi_", year, "$GASOLINE)", sep="")))
      # eval(parse(text=paste("tableBuanganLimbahInt_", year, "<- replace(tableBuanganLimbahInt_", year,"$Padat_3R, c(1:length(sector)) ,(tableBuanganLimbahInt_",year, "$Padat_3R + deltaBuanganLimbahTimbun_", year,"))", sep="")))
      eval(parse(text=paste("tableKonsumsiEnergiInt_", year,"$BIOGASOL <- (tableKonsumsiEnergiInt_",year, "$BIOGASOL + deltaKonsumsiEnergi_", year,")", sep="")))
      eval(parse(text=(paste("emisiEnergi_", year, "<- as.matrix(tableKonsumsiEnergiInt_", year, ") %*% ef_matrix", sep=""))))
      eval(parse(text=(paste("rowTotalEmisi_", year, "<-rowSums(emisiEnergi_", year, ")", sep=""))))
      eval(parse(text=(paste("totalEmisiEnergi_", year, "<-sum(rowTotalEmisi_", year, ")", sep=""))))
      ## BUAT LOOPING LINE 268-282
      totalEmisiEnergiInt_All <- rbind(totalEmisiEnergi_2016,
                                       totalEmisiEnergi_2017,
                                       totalEmisiEnergi_2018,
                                       totalEmisiEnergi_2019,
                                       totalEmisiEnergi_2020,
                                       totalEmisiEnergi_2021,
                                       totalEmisiEnergi_2022,
                                       totalEmisiEnergi_2023,
                                       totalEmisiEnergi_2024,
                                       totalEmisiEnergi_2025,
                                       totalEmisiEnergi_2026,
                                       totalEmisiEnergi_2027,
                                       totalEmisiEnergi_2028,
                                       totalEmisiEnergi_2029,
                                       totalEmisiEnergi_2030)
      colnames(totalEmisiEnergiInt_All) <- "Total_Emisi"
      totalEmisiBRT <- data.frame(cbind(year,totalEmisiEnergiInt_All), row.names = 1:length(year))

      ## Intensitas Emisi ##
      intensistasEmBRT <-totalEmisiBRT$Total_Emisi/intTotalPDRB_BRT$Total_PDRB
      intensistasEmBRT <-as.data.frame(cbind(year,intensistasEmBRT), row.names = 1:length(year))
      colnames(intensistasEmBRT) <- c("year","Intensitas_Emisi")
      

  ### Aksi 2 : Car Free Day ####
    
      ## Final Demand ##
      
      findem_CFD <- read.table("_YK/raw/input_transportasi/fd_CFD.csv", header = TRUE, sep =";")
      findemCFDInt_All <- proyeksiFD_t[2:length(proyeksiFD_t)] + findem_CFD[2:length(findem_CFD)]
      intOutputCFD_All <- ioLeontiefInverse %*% as.matrix(findemCFDInt_All)
      intPDRBCFD_All <- GDPAll$P_OUTPUT * (intOutputCFD_All)
      intColsumPDRB_CFD <- data.frame(colSums(intPDRBCFD_All))
      colnames(intColsumPDRB_CFD) <- "Total_PDRB"
      intTotalPDRB_CFD <- as.data.frame(cbind(year, intColsumPDRB_CFD), row.names = 1:length(year))
      
      ## Tabel Satelit ##
      infaktorEmisi <- emissionFactorEnergy
      F_Type <- as.data.frame(emissionFactorEnergy$F_Type)
      num_ef <- nrow(emissionFactorEnergy)
      year_1 <- 2016:2017
      year_2 <- seq(2019,2030, by=2)
      year_3 <- seq(2018,2030, by=2)
      multiplier <- 3:8
      multiplier2 <- 2:8
      colnames(F_Type) <- "F_Type"

      eval(parse(text=(paste("intFaktorEmisi_", year_1, "<- replace(infaktorEmisi$Em_F,c(6),((1+((", year_1, "-2015) * (-0.3/8))) * infaktorEmisi$Em_F[6]))", sep ="" ))))
      eval(parse(text=(paste("intFaktorEmisi_", year_2, "<- replace(infaktorEmisi$Em_F,c(6),((1+(", multiplier, " * (-0.3/8))) * infaktorEmisi$Em_F[6]))", sep ="" ))))
      eval(parse(text=(paste("intFaktorEmisi_", year_3, "<- replace(infaktorEmisi$Em_F,c(6),((1+(", multiplier2, " * (-0.3/8))) * infaktorEmisi$Em_F[6]))", sep ="" ))))
      eval(parse(text=(paste("efInt_", year, "<-cbind(F_Type,intFaktorEmisi_", year,")", sep="" ))))
      eval(parse(text=(paste("diagEFInt_", year, "<-diag(efInt_", year,"$intFaktorEmisi_", year, ", ncol = num_ef, nrow = num_ef)", sep=""))))
      eval(parse(text=(paste("tableKonsumsiEnergi_",  year, "<-propEnergi * proyeksiBAUEnergi$`", year, "`", sep=""))))
      eval(parse(text=paste("tableIntEmisiEnergi_", year, "<- as.matrix(tableKonsumsiEnergi_", year, ") %*% diagEFInt_", year,sep= "" )))
      eval(parse(text=paste("rowSumIntEmisiEnergi_", year, "<- rowSums(tableIntEmisiEnergi_", year, ")", sep="")))
      eval(parse(text=paste("intTotalEmisiEnergi_", year, "<- sum(rowSumIntEmisiEnergi_", year, ")", sep="")))
      #BUAT LOOPING LINE 379-393
      intTotalEmisiEnergi_All <- rbind(intTotalEmisiEnergi_2016,
                                 intTotalEmisiEnergi_2017,
                                 intTotalEmisiEnergi_2018,
                                 intTotalEmisiEnergi_2019,
                                 intTotalEmisiEnergi_2020,
                                 intTotalEmisiEnergi_2021,
                                 intTotalEmisiEnergi_2022,
                                 intTotalEmisiEnergi_2023,
                                 intTotalEmisiEnergi_2024,
                                 intTotalEmisiEnergi_2025,
                                 intTotalEmisiEnergi_2026,
                                 intTotalEmisiEnergi_2027,
                                 intTotalEmisiEnergi_2028,
                                 intTotalEmisiEnergi_2029,
                                 intTotalEmisiEnergi_2030)
      colnames(intTotalEmisiEnergi_All) <- "Total_Emisi"
      totalEmisiCFD <- data.frame(cbind(year,intTotalEmisiEnergi_All), row.names = 1:length(intTotalEmisiEnergi_All))

      ## Intensitas Emisi ##
      intensistasEmCFD <-as.data.frame(totalEmisiCFD$Total_Emisi/intTotalPDRB_CFD$Total_PDRB)
      intensistasEmCFD <-as.data.frame(cbind(year,intensistasEmCFD), row.names = 1:length(year))
      colnames(intensistasEmCFD) <- c("year","Intensitas_Emisi")
      
      
### OUTPUT DATAF RAME ####
  ## scenario-specific dataframe of sector><simulation years depicting GDP, emission and emission intensity    
      #Aksi 1
      #PDRB
      sector <- ioSector$V1
      colnames(intPDRBBRT_All) <- year
      PDRB_BRT <- as.data.frame(cbind(sector,intPDRBBRT_All))
      #Emisi
      ##BUAT LOOPING LINE 531-546
      rowTotalEmisiBRT <- as.data.frame(cbind(rowTotalEmisi_2016,
                                           rowTotalEmisi_2017,
                                           rowTotalEmisi_2018,
                                           rowTotalEmisi_2019,
                                           rowTotalEmisi_2020,
                                           rowTotalEmisi_2021,
                                           rowTotalEmisi_2022,
                                           rowTotalEmisi_2023,
                                           rowTotalEmisi_2024,
                                           rowTotalEmisi_2025,
                                           rowTotalEmisi_2026,
                                           rowTotalEmisi_2027,
                                           rowTotalEmisi_2028,
                                           rowTotalEmisi_2029,
                                           rowTotalEmisi_2030), row.names = 1:length(sector))
      colnames(rowTotalEmisiBRT) <- year      
      emisi_BRT <- cbind(sector, rowTotalEmisiBRT)
      #Intensitas Emisi
      inEmBRT_temp <- rowTotalEmisiBRT/intBRTPDRB_All
      inEm_BRT <- cbind(sector, inEmBRT_temp)
      
      #Aksi 2
      #PDRB
      sector <- ioSector$V1
      colnames(intPDRBCFD_All) <- year
      PDRB_CFD <- as.data.frame(cbind(sector,intPDRBCFD_All))
      #Emisi
      ##BUAT LOOPING LINE 559-574
      rowSumEmisiCFD <- as.data.frame(cbind(rowSumIntEmisiEnergi_2016,
                                            rowSumIntEmisiEnergi_2017,
                                            rowSumIntEmisiEnergi_2018,
                                            rowSumIntEmisiEnergi_2019,
                                            rowSumIntEmisiEnergi_2020,
                                            rowSumIntEmisiEnergi_2021,
                                            rowSumIntEmisiEnergi_2022,
                                            rowSumIntEmisiEnergi_2023,
                                            rowSumIntEmisiEnergi_2024,
                                            rowSumIntEmisiEnergi_2025,
                                            rowSumIntEmisiEnergi_2026,
                                            rowSumIntEmisiEnergi_2027,
                                            rowSumIntEmisiEnergi_2028,
                                            rowSumIntEmisiEnergi_2029,
                                            rowSumIntEmisiEnergi_2030), row.names = 1:length(sector))
      colnames(rowSumEmisiCFD) <- year      
      emisi_CFD<- cbind(sector, rowSumEmisiCFD)
      #Intensitas Emisi
      inEmCFD_temp <- rowSumEmisiCFD/intPDRBCFD_All
      inEm_CFD <- cbind(sector, inEmCFD_temp)  
      
  ## scenario-specific dataframe of sector><simulation years depicting GDP change against BAU, emission change against BAU and emission intensity change against BAU 
      #Aksi 1
      deltaPDRB <- as.data.frame(cbind(sector,(intPDRBBRT_All - proyeksiPDRB)))
      ## BUAT LOOPING LINE 585-599
      emisiBAU <- cbind(emisiEnergi_2016,
                        emisiEnergi_2017,
                        emisiEnergi_2018,
                        emisiEnergi_2019,
                        emisiEnergi_2020,
                        emisiEnergi_2021,
                        emisiEnergi_2022,
                        emisiEnergi_2023,
                        emisiEnergi_2024,
                        emisiEnergi_2025,
                        emisiEnergi_2026,
                        emisiEnergi_2027,
                        emisiEnergi_2028,
                        emisiEnergi_2029,
                        emisiEnergi_2030)
      deltaEmisi <- as.data.frame(cbind(sector, (rowTotalEmisiBRT  - emisiBAU)))
      intensitasEmBAU <- emisiBAU/proyeksiPDRB
      deltaInEm <- as.data.frame(cbind(sector, (inEmBRT_temp - intensitasEmBAU)))
      
      
      #Aksi 2
      deltaPDRB_cair <- as.data.frame(cbind(sector,(intPDRBCFD_All - proyeksiPDRB)))
      deltaEmisi_cair <- as.data.frame(cbind(sector, (rowSumEmisiCFD  - emisiBAU)))
      intensitasEmBAU <- emisiBAU/proyeksiPDRB
      deltaInEm_cair <- as.data.frame(cbind(sector, (inEmCFD_temp - intensitasEmBAU)))
      
  ## scenario-specific dataframe at t15 depicting cumulative emission reduction, GDP change and emission intensity change
      #Aksi 1
      cumPDRB_BRT <- cumsum(intTotalPDRB_BRT$Total_PDRB)
      cumEmisi_BRT <- cumsum(totalEmisiBRT$Total_Emisi)
      cumInEmisi_BRT <- cumsum(intensistasEmCFD$Intensitas_Emisi)
      cumTable_BRT <- as.data.frame(cbind(year, cumPDRB_BRT, cumEmisi_BRT, cumInEmisi_BRT))
      colnames(cumTable_BRT) <- c("year", "Total_PDRB", "Total_Emisi", "Intensitas_Emisi")
      #cumTable_padat_2030 <- filter(cumTable_padat, year=="2030") 
      
      #Aksi 2
      cumPDRB_CFD <- cumsum(intTotalPDRB_CFD$Total_PDRB)
      cumEmisi_CFD <- cumsum(totalEmisiCFD$Total_Emisi)
      cumInEmisi_CFD <- cumsum(intensistasEmCFD$Intensitas_Emisi)
      cumTable_CFD <- as.data.frame(cbind(year, cumPDRB_CFD, cumEmisi_CFD, cumInEmisi_CFD))
      colnames(cumTable_CFD) <- c("year", "Total_PDRB", "Total_Emisi", "Intensitas_Emisi")
      #cumTable_cair_2030 <- filter(cumTable_cair, year=="2030")
      
### KOMBINASI AKSI ####
  ## scenario-specific dataframe of sector><simulation years depicting GDP, emission and emission intensity        
      #PDRB
      inCombineFD <- read.table("_YK/raw/input_transportasi/fd_kombinasi.csv", header = TRUE, sep =";")
      sector <- ioSector$V1
      combineFD <- as.data.frame(inCombineFD)
      combineFD <- combineFD[,2:length(combineFD)] + proyeksiFD_t[,2:length(proyeksiFD_t)]
      combineOutput <- ioLeontiefInverse %*% as.matrix(combineFD)
      combinePDRB <- GDPAll$P_OUTPUT * combineOutput
      colsum_combinePDRB <- as.data.frame(cbind(year,colSums(combinePDRB)), row.names = 1:length(year))
      colnames(colsum_combinePDRB) <- c("year", "Total_PDRB")
      combinePDRB <- as.data.frame(cbind(sector, combinePDRB))
      
      #Emisi
      
      tableCombineOutput <- data.frame(combineOutput,
                                       stringsAsFactors = FALSE)
      tableCombineKoef <- data.frame(row.names = 1:52,
                                     Koef_Energi = energyCoef,
                                     stringsAsFactors = FALSE)
      tableCombineProyeksi <- function(tableCombineOutput,tableCombineKoef){
        for (i in 1:ncol(tableCombineOutput)){
          tableCombineOutput[,i] <- tableCombineKoef*(tableCombineOutput[,i])
        }    
        return(tableCombineOutput)
      }
      tableKonsumsiEnergi_combine <- tableProyeksi(tableCombineOutput,tableCombineKoef)
      year <- 2016:2030
      colnames(tableKonsumsiEnergi_combine) <- year
      tableKonsumsiEnergi_combine <- cbind(sector, tableKonsumsiEnergi_combine)
      
      eval(parse(text=(paste("tableKonsumsiEnergi_combine",  year, "<-propEnergi * tableKonsumsiEnergi_combine$`", year, "`[2:length(tableKonsumsiEnergi_combine)]", sep=""))))
      eval(parse(text=(paste("tableEmisiCombine_", year, "<- as.matrix(tableKonsumsiEnergi_combine", year,")", "%*% input_ef_matrix", sep=""))))
      eval(parse(text=(paste("emisiCombine_", year, "<- as.data.frame(rowSums(tableEmisiCombine_", year,"))", sep=""))))
      eval(parse(text=paste("TotalEmisiCombine_", year, "<- sum(emisiCombine_", year, ")", sep="")))
      ## BUAT LOOPING LINE 665-679
      combineEmisi <- cbind (emisiCombine_2016,
                             emisiCombine_2017,
                             emisiCombine_2018,
                             emisiCombine_2019,
                             emisiCombine_2020,
                             emisiCombine_2021,
                             emisiCombine_2022,
                             emisiCombine_2023,
                             emisiCombine_2024,
                             emisiCombine_2025,
                             emisiCombine_2026,
                             emisiCombine_2027,
                             emisiCombine_2028,
                             emisiCombine_2029,
                             emisiCombine_2030)
      
      #Intensitas Emisi
      combineIEm <- combineEmisi/combinePDRB[,2:length(combinePDRB)]
      combineIEm <- cbind(sector, combineIEm)
      
  ## scenario-specific dataframe of sector><simulation years depicting GDP change against BAU, emission change against BAU and emission intensity change against BAU 
      #PDRB
      deltaPDRB_combine <- as.data.frame(cbind(sector,(combinePDRB[,2:length(combinePDRB)] - proyeksiPDRB)))
      
      #Emisi
      deltaEmisi_combine <- as.data.frame(cbind(sector, (combineEmisi  - emisiBAU)))
      
      #Intensitas Emisi
      intensitasEmBAU <- emisiBAU/proyeksiPDRB
      deltaInEm_combine <- as.data.frame(cbind(sector, (combineIEm[,2:length(combinePDRB)] - intensitasEmBAU)))
      
  ## scenario-specific dataframe at t15 depicting cumulative emission reduction, GDP change and emission intensity change 
      
      #PDRB
      cumPDRB_combine <- cumsum(colsum_combinePDRB$Total_PDRB)
      
      #Emisi
      ## BUAT LOOPING LINE 702-717
      totalEmisiCombine <- rbind (TotalEmisiCombine_2016,
                                  TotalEmisiCombine_2017,
                                  TotalEmisiCombine_2018,
                                  TotalEmisiCombine_2019,
                                  TotalEmisiCombine_2020,
                                  TotalEmisiCombine_2021,
                                  TotalEmisiCombine_2022,
                                  TotalEmisiCombine_2023,
                                  TotalEmisiCombine_2024,
                                  TotalEmisiCombine_2025,
                                  TotalEmisiCombine_2026,
                                  TotalEmisiCombine_2027,
                                  TotalEmisiCombine_2028,
                                  TotalEmisiCombine_2029,
                                  TotalEmisiCombine_2030)
      totalEmisiCombine <- as.data.frame(cbind(year,totalEmisiCombine), row.names = 1:length(year))
      colnames(totalEmisiCombine) <- c("year", "Total_Emisi")
      cumEmisi_combine <- cumsum(totalEmisiCombine$Total_Emisi)
      
      #Intensitas Emisi
      inEm_Combine <- cumEmisi_combine/cumPDRB_combine
      cumInEmisi_combine <- cumsum(inEm_Combine)
      
      cumTable_combine<- as.data.frame(cbind(year, cumPDRB_combine, cumEmisi_combine, cumInEmisi_combine))
      colnames(cumTable_combine) <- c("year", "Total_PDRB", "Total_Emisi", "Intensitas_Emisi")
      