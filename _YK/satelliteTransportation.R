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

## Proyeksi BAU Buangan Limbah ##

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

## Proporsi Satelit Limbah ##

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
    propEnergi[,i] <- propEnergi[,i] * (totalBuanganEnergi_t)
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

### SKENARIO GABUNGAN ####