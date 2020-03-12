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

## Multiplier Waste Product ##
wasteCoef <- ioTotalOutputInverse %*% as.matrix(satelliteWaste$Total)
multiplierWaste <- ioLeontiefInverse %*% wasteCoef

## Waste Emission ##
inemissionFactorWaste <- "raw/11_faktor_emisi_limbah.csv"
emissionFactorWaste <- read.table(inemissionFactorWaste, header = TRUE, sep = ",")
num_ef <- length(emissionFactorWaste$Em_F)
ef_matrix <- diag(as.vector(emissionFactorWaste$Em_F), ncol = num_ef, nrow = num_ef)
ef_waste_matrix <- as.matrix(satelliteWaste[,4:19]) %*% ef_matrix

### SKENARIO BAU ####

  ## Tabel GDP Rate ##
  
  inputGDPRate <- 0.05
  for (i in year){
    colnames(sector) <- "Lapangan Usaha"
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
  proyeksiPDRB <- as.data.frame(cbind(sector,proyeksiPDRB))
  
  ## Proyeksi BAU Buangan Limbah ##
  
  # inProyeksiOutput <- "raw/proyeksi_output_jabar.csv"
  # proyeksiOutput <- read.table(inProyeksiOutput, header = TRUE, sep = ";")
  tablePOutput <- data.frame(proyeksiOutput_t[,2:length(proyeksiOutput_t)],
                             stringsAsFactors = FALSE)
  tableKoef <- data.frame(row.names = 1:52,
                          Koef_Energi = wasteCoef,
                          stringsAsFactors = FALSE)
  tableProyeksi <- function(tablePOutput,tableKoef){
    for (i in 1:ncol(tablePOutput)){
      tablePOutput[,i] <- tableKoef*(tablePOutput[,i])
    }    
    return(tablePOutput)
  }
  tableProyeksiLimbah <- tableProyeksi(tablePOutput,tableKoef)
  year <- 2016:2030
  colnames(tableProyeksiLimbah) <- year
  sector <- ioSector$V1
  proyeksiBAULimbah <- cbind(sector, tableProyeksiLimbah)
  
  ## Proporsi Satelit Limbah ##
  
  tablePropLimbah <-satelliteWaste[,4:length(satelliteWaste)]/satelliteWaste$Total 
  tablePropLimbah <- replace(tablePropLimbah, is.na(tablePropLimbah), 0)
  sector <- as.data.frame(ioSector$V1)
  tablePropLimbah <- cbind(sector, tablePropLimbah)
  # tablePropLimbah <- read.table("raw/satelit_limbah.csv", header = TRUE, sep = ";")
  
  ## Tabel Proyeksi Buangan Limbah Tahun X ##
  
  propLimbah <- data.frame(tablePropLimbah[2:length(tablePropLimbah)],
                           stringsAsFactors = FALSE)
  #Contoh : Tabel Proyeksi Buangan Limbah Tahun 2017
  totalBuangan_t <- data.frame(proyeksiBAULimbah$`2017`,
                               stringsAsFactors = FALSE)
  tableBuanganLimbah <- function(propLimbah, totalBuangan_t){
    for (i in 1:ncol(propLimbah)){
      propLimbah[,i] <- propLimbah[,i] * (totalBuangan_t)
    }
    return(propLimbah)
  }
  tableBuanganLimbah_t <- tableBuanganLimbah(propLimbah, totalBuangan_t)
  totalBuanganSektor_t <- rowSums(tableBuanganLimbah_t)
  totalBuanganLimbah_t <- sum(totalBuanganSektor_t)

  ## Tabel Proyeksi Emisi Limbah Tahun X ##
  
  #Contoh : Tabel Proyeksi Emisi Limbah Tahun 2017
  infaktorEmisi <- read.table("raw/11_faktor_emisi_limbah.csv", header = TRUE, sep = ";")
  num_input_ef <- length(infaktorEmisi$Em_F)
  input_ef_matrix <- diag(as.vector(infaktorEmisi$Em_F), ncol = num_input_ef, nrow = num_input_ef)
  tableEmisi <- as.matrix(tableBuanganLimbah_t) %*% input_ef_matrix
  emisi_t <- as.data.frame(rowSums(tableEmisi))
  colnames(emisi_t) <- "Total Emisi"
  totalEmisi_t <- sum(emisi_t)
  
  ## Membuat tabel "Total Buangan dan Faktor Emisi" tahun awal-akhir ##
  
  eval(parse(text=(paste("tableBuanganLimbahAll_",  year, "<-propLimbah * proyeksiBAULimbah$`", year, "`", sep=""))))
  eval(parse(text=(paste("tableEmisi_", year, "<- as.matrix(tableBuanganLimbahAll_", year,")", "%*% input_ef_matrix", sep=""))))
  eval(parse(text=(paste("emisi_", year, "<- as.data.frame(rowSums(tableEmisi_", year,"))", sep=""))))
  eval(parse(text=(paste("totalEmisi_", year, "<- sum(emisi_", year, ")", sep=""))))
  # Penggabungan total emisi masih manual
  totalEmisi_All <- rbind(totalEmisi_2016,
                         totalEmisi_2017,
                         totalEmisi_2018,
                         totalEmisi_2019,
                         totalEmisi_2020,
                         totalEmisi_2021,
                         totalEmisi_2022,
                         totalEmisi_2023,
                         totalEmisi_2024,
                         totalEmisi_2025,
                         totalEmisi_2026,
                         totalEmisi_2027,
                         totalEmisi_2028,
                         totalEmisi_2029,
                         totalEmisi_2030)
  colnames(totalEmisi_All) <- "Total_Emisi"
  totalEmisi_All <- as.data.frame(row.names = 1:length(year),
                                  cbind(year,totalEmisi_All))
  
  
  library(ggplot2)
  library(plotly)
  
  ## Grafik Proyeksi Total Buangan Limbah ##
  
  graphWaste <- colSums(proyeksiBAULimbah[,2:length(proyeksiBAULimbah)])
  graphWaste <- as.data.frame(cbind(year,graphWaste), row.names = 1:length(year))
  plotWaste<- ggplot(data=graphWaste, aes(x=year, y=graphWaste, group=1)) + geom_line() + geom_point()
  ggplotly(plotWaste)
  
  ## Grafik Proyeksi Total Emisi Buangan Limbah ##
  
  plotEmission <- ggplot(data=totalEmisi_All, aes(x=year, y=Total_Emisi, group=1)) + geom_line() + geom_point()
  ggplotly(plotEmission)


### SKENARIO AKSI ####  
  
  ## Limbah Padat : Rencana Pembangunan dan Operasional TPS Terpadu 3R/Kompos-ting ##
  

