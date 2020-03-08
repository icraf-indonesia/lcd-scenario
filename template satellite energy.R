#set wd di laptop dw
setwd("C:/dw/ICRAF/redcluew/syntax/redcluwe")

###BEGIN: initiate all variables ####
# username <- "alfanugraha"
# password <- "1234"
selectedProv = "JaBar"
datapath <- paste0("data/", selectedProv, "/")
datapathCSV <- paste0("raw/jabar_inredcluwe", selectedProv, "/")

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

# data skenario jika dalam csv atau dataset baru
inIntermediateDemand <- paste0(datapathCSV, "/02_input_antara_skenario1.csv")
inEnergy<- paste0(datapathCSV, "/08_satelit_energi_skenario1.csv")
inEmissionFactorEnergiTable<- paste0(datapathCSV, "/10_faktor_emisi_energi.csv") # asumsi faktor emisi tidak berubah

indemScen1 <- read.table(inIntermediateDemand, header=FALSE, sep=",")
energy <- read.table(inEnergy, header=TRUE, sep=",", stringsAsFactors = F)
ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, sep=",", stringsAsFactors = F)


#BAU
ioBau<- ioIntermediateDemand
energyBau <- satelliteEnergy
efBau <- emissionFactorEnergy

# perhitungan table IO
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


# Skenario 1 EBT: PLTM on grid (definisikan skenario) --------------------------------------------------------------
# berpengaruh pada FD
## Asumsi pembuatan 1 PLTM menghabiskan dana $60 juta 
### 60% bertambah di sektor (kode 30) konstruksi
### 20% bertambah di sektor (kode 27) ketenagalistrikan
### 10% bertambah di sektor (kode 34) angkutan darat
### 23% bertambah di sektor (kode 23) industri mesin dan perlengkapan YTDL

# berpengaruh pada satelit energi
## akan mengurangi coal dan diesel gradual tiap tahun hingga 2030 (masih belum ketemu caranya)
## asumsikan dulu sekarang berkurang 10% di HCOAL dan 5% di diesel otomatis bertambah 15% di electr


# Section 2 pilih sektor yang jadi owner ----------------------------------
## belum terbayang caranya karena masih mengasumsikan masukkan tabel baru secara keseluruhan
## baik utk IO ataupun utk satelit energinya
## jadi terbayangnya tinggal mereplace table BAU dg table skenario lalu di run ulang 


# Section 3 masukkan skenario untuk perubahan FD --------------------------

# data scenario
ioScen1 <- indemScen1

### tetap pakai proses diatas hanya saja tabel yang digunakan berbeda
matrixIoIntermediateDemand <- as.matrix(ioScen1) #yang berubah tabel io antara saja krn FD yg nambah
matrixIoAddedValue <- as.matrix(ioAddedValue) #asumsikan dulu tidak ada penambahan import, jd tdk berubah importnya
nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
ioDimention <- ncol(ioScen1) #yang berubah tabel io saja

colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput 
ioTotalOutputInverse <- 1/ioTotalOutput
ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
ioTotalOutputInverse <- diag(ioTotalOutputInverse)


# Section 4: Merubah akun satelit -----------------------------------------

# data scenario
energyScen1 <- energy
efScen1 <- ef_energy




# Section 5: Running ------------------------------------------------------

#import_row <- 1
#income_row <- 2
#profit_row <- 3
#gdpRate <- as.numeric(input$gdpRate)/100
#startT <- as.numeric(input$dateFrom)
#endT <- as.numeric(input$dateTo)

## tabel IO dari excehl redcluwe.jabar
indem_matrix <- as.matrix(ioBau)
addval_matrix <- as.matrix(ioAddedValue)
dimensi <- ncol(indem_matrix)

indem_colsum <- colSums(indem_matrix)
addval_colsum <- colSums(addval_matrix)
fin_con <- 1/(indem_colsum+addval_colsum)
fin_con[is.infinite(fin_con)] <- 0
tinput_invers <- diag(fin_con)
A <- indem_matrix %*% tinput_invers
I <- as.matrix(diag(dimensi))
I_A <- I-A
leontief <- solve(I_A)


## tabel energi dari excel redcluwe.jabar
tbl_sat = energyBau
emission_lookup = efBau


## fungsi untuk sheet tabel energi --> hasil ekstrak dari satellliteimpact di apps shiny 
satelliteImpactEnergy <- function(sat_type = "energy", tbl_sat = data.frame(), 
                            emission_lookup = data.frame()){ 
  if(sat_type == "energy" | sat_type == "waste"){
    impact <- list() # impact$cons; impact$emission
    
    impact$cons <- tbl_sat
    
    order_cname <- names(impact$cons)[4:ncol(impact$cons)]
    em_f <- numeric()
    for(m in 1:length(order_cname)){
      em_f <- c(em_f, emission_lookup[which(emission_lookup[,1]==order_cname[m]), 2])
    }
    em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
    
    
    #perkalian matriks
    impact$emission <- impact$cons
    impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)]) %*% em_f
    impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
    colnames(impact$emission)[3] <- "Temission"
  } 
  impact$cons[is.na(impact$cons)] <- 0
  impact$emission[is.na(impact$emission)] <- 0
  return(impact)
}

#BAU: baseline
tabelEmisiEnergiBAU <- satelliteImpactEnergy('energy', tbl_sat = energyBau, emission_lookup = efBau)
tabelEmisiEnergiBAU$emission[,3] #total tabel emisi energi BAU

#Skenario 1
tabelEmisiEnergiSken <- satelliteImpactEnergy('energy', tbl_sat = energyScen1, emission_lookup = efScen1)
tabelEmisiEnergiSken$emission[,3] #total tabel emisi energi skenario 1


# Tabel proyeksi ----------------------------------------------------------

gdpRate <- 5/100
yearFrom <- 2016 
yearTo <- 2030

## bagian FD
fdCalculate <- function(tbl1, tbl2){
  for(i in 1:ncol(tbl1)){
    if(i == 1){
      tbl1[,i] <- tbl2[,i]*(tbl1[,i] + 1)
    } else {
      tbl1[,i] <- tbl1[,i-1]*(tbl1[,i] + 1)
    }
  }
  return(tbl1)
}
lengthYear <- (yearTo - yearFrom)+1
column_year <- paste0("y", yearFrom:yearTo )
lengthSector <- nrow(sector)
proyPertumEkonomi <- matrix(gdpRate,nrow = lengthSector,ncol = lengthYear)
#rownames(proyPertumEkonomi) <- ioSector[,1]
colnames(proyPertumEkonomi) <- column_year

findem <- ioFinalDemand
findemcom <- ioFinalDemandComponent
colnames(findem) <- c(t(findemcom))
findem$`Total Permintaan Akhir` <- rowSums(findem)
fdSelectYear <- findem$`Total Permintaan Akhir`

fdAllYear <- fdCalculate(tbl1 = proyPertumEkonomi,tbl2 = as.data.frame(fdSelectYear))


## bagian Output: belum selesai yang muncul masih kolom terakhir saja
outputCalculate <- function(tbl1,tbl2){
  for(i in 1:ncol(tbl2)){
    tblOutput <- tbl1 %*% tbl2[,i]
  }
  return(tblOutput)
}

outputSelectYear <- leontief %*% fdSelectYear

outputAllYear <- outputCalculate(tbl1 = leontief, tbl2 = fdAllYear)



