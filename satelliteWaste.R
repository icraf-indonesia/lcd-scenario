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
waste_matrix <- as.matrix(satelliteWaste[,4:19]) %*% ef_matrix

### SKENARIO BAU ####
## Proyeksi BAU Buangan Limbah ##
inProyeksiOutput <- "raw/proyeksi_output_jabar.csv"
proyeksiOutput <- read.table(inProyeksiOutput, header = TRUE, sep = ";")


tablePOutput <- data.frame(proyeksiOutput[,4:length(proyeksiOutput)],
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


### SKENARIO AKSI ####

## Proporsi Satelit Limbah ##
tablePropLimbah <- read.table("raw/satelit_limbah.csv", header = TRUE, sep = ";")
propLimbah <- data.frame(tablePropLimbah[4:length(tablePropLimbah)],
                         stringsAsFactors = FALSE)

## Contoh : Tabel Buangan Limbah Tahun 2017 ##
totalBuangan_t <- data.frame(proyeksiBAULimbah$`2017`,
                             stringsAsFactors = FALSE)
tableBuanganLimbah_t <- function(propLimbah, totalBuangan_t){
  for (i in 1:ncol(propLimbah)){
    propLimbah[,i] <- propLimbah[,i] * (totalBuangan_t)
  }
  return(propLimbah)
}
tableBuanganLimbah <- tableBuanganLimbah_t(propLimbah, totalBuangan_t)

## Contoh : Tabel Faktor Emisi Limbah Tahun ##
infaktorEmisi <- read.table("raw/faktor_emisi_limbah.csv", header = TRUE, sep = ";")
faktorEmisi <- read.table(inemissionFactorWaste, header = TRUE, sep = ",")
num_input_ef <- length(tableFaktorEmisi$Em_F)
input_ef_matrix <- diag(as.vector(tableFaktorEmisi$Em_F), ncol = num_input_ef, nrow = num_input_ef)
tableFaktorEmisi <- as.matrix(tableBuanganLimbah) %*% input_ef_matrix
totalEmisi <- as.data.frame(rowSums(tableFaktorEmisi))
colnames(totalEmisi) <- "Total Emisi"