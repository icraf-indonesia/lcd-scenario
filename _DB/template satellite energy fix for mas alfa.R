#set wd di laptop dw
#setwd("C:/dw/ICRAF/redcluew/syntax/lcd-scenario")
pathcsv <- ("C:/dw/ICRAF/redcluew/syntax/lcd-scenario/_DB")

###BEGIN: initiate all variables ####
# username <- "alfanugraha"
# password <- "1234"
selectedProv = "JaBar"
datapath <- paste0("data/", selectedProv, "/")
datapathCSV <- pathcsv


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
inFD <- paste0(datapathCSV, "/17_final_demand_proyeksi_sken1_d.csv")
inFD2 <- paste0(datapathCSV, "/17_final_demand_proyeksi_sken2_f.csv")
inEmissionFactorEnergiTable<- paste0(datapathCSV, "/10_faktor_emisi_energi.csv") # asumsi faktor emisi tidak berubah
inProporsiPDRB <- paste0(datapathCSV, "/16_proporsi_pdrb.csv")
inPersentaseBahanBAkar1 <- paste0(datapathCSV, "/18_persentase_bahan_bakar_sken1_tahap1.csv")
inPersentaseBahanBAkar2 <- paste0(datapathCSV, "/18_persentase_bahan_bakar_sken1_tahap2.csv")
inPersentaseBahanBAkarSken2 <- paste0(datapathCSV, "/18_persentase_bahan_bakar_sken2_f.csv")
inDiesel2016 <- paste0(datapathCSV, "/19_konsumsi_diesel_2016_sken2_f.csv")

#gabungan skenario 1 dan 2
inFDS1S2 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/20_s1s2_FD.csv")
inKonsS1S22016 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2016.csv")
inKonsS1S22017 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2017.csv")
inKonsS1S22018 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2018.csv")
inKonsS1S22019 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2019.csv")
inKonsS1S22020 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2020.csv")
inKonsS1S22021 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2021.csv")
inKonsS1S22022 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2022.csv")
inKonsS1S22023 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2023.csv")
inKonsS1S22024 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2024.csv")
inKonsS1S22025 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2025.csv")
inKonsS1S22026 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2026.csv")
inKonsS1S22027 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2027.csv")
inKonsS1S22028 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2028.csv")
inKonsS1S22029 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2029.csv")
inKonsS1S22030 <- paste0(datapathCSV, "/csv gabungan sken 1 dan sken 2/21_s1s2_konsumsi_energi_2030.csv")


#indemScen1 <- read.table(inIntermediateDemand, header=F, sep=",")
fdSken1 <- read.table(inFD, header=TRUE, sep=",", stringsAsFactors = F)
fdSken2 <- read.table(inFD2, header=TRUE, sep=",", stringsAsFactors = F)
ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, sep=",", stringsAsFactors = F)
proporsiPDRB <- read.table(inProporsiPDRB, header=TRUE, sep=",", stringsAsFactors = F)
persenBahanBakar1 <- read.table(inPersentaseBahanBAkar1, header=F, sep=",", stringsAsFactors = F)
persenBahanBakar2 <- read.table(inPersentaseBahanBAkar2, header=F, sep=",", stringsAsFactors = F)
persenBahanBakarSken2 <- read.table(inPersentaseBahanBAkarSken2, header=F, sep=",", stringsAsFactors = F)
dieselSken2 <- read.table(inDiesel2016, header=T, sep=",", stringsAsFactors = F)

fsS1S2 <- read.table(inFDS1S2, header=T, sep=",", stringsAsFactors = F)
KonsS1S22016 <- read.table(inKonsS1S22016, header=T, sep=",", stringsAsFactors = F)
KonsS1S22017 <- read.table(inKonsS1S22017, header=T, sep=",", stringsAsFactors = F)
KonsS1S22018 <- read.table(inKonsS1S22018, header=T, sep=",", stringsAsFactors = F)
KonsS1S22019 <- read.table(inKonsS1S22019, header=T, sep=",", stringsAsFactors = F)
KonsS1S22020 <- read.table(inKonsS1S22020, header=T, sep=",", stringsAsFactors = F)
KonsS1S22021 <- read.table(inKonsS1S22021, header=T, sep=",", stringsAsFactors = F)
KonsS1S22022 <- read.table(inKonsS1S22022, header=T, sep=",", stringsAsFactors = F)
KonsS1S22023 <- read.table(inKonsS1S22023, header=T, sep=",", stringsAsFactors = F)
KonsS1S22024 <- read.table(inKonsS1S22024, header=T, sep=",", stringsAsFactors = F)
KonsS1S22025 <- read.table(inKonsS1S22025, header=T, sep=",", stringsAsFactors = F)
KonsS1S22026 <- read.table(inKonsS1S22026, header=T, sep=",", stringsAsFactors = F)
KonsS1S22027 <- read.table(inKonsS1S22027, header=T, sep=",", stringsAsFactors = F)
KonsS1S22028 <- read.table(inKonsS1S22028, header=T, sep=",", stringsAsFactors = F)
KonsS1S22029 <- read.table(inKonsS1S22029, header=T, sep=",", stringsAsFactors = F)
KonsS1S22030 <- read.table(inKonsS1S22030, header=T, sep=",", stringsAsFactors = F)

################################################################################
#                                                                              #
#                                    INPUT                                     #
#                                                                              #
################################################################################

gdpRate <- 5/100 #user input
yearFrom <- 2016 #user input
yearTo <- 2030 #user input



# BAU ---------------------------------------------------------------------
ioIntermediateDemand
energyBau <- satelliteEnergy
efBau <- emissionFactorEnergy

################################################################################
#                                                                              #
#                                   SHEET IO                                   #
#                                                                              #
################################################################################
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

indem_matrix <- as.matrix(ioIntermediateDemand)
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

matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand) 
matrixIoAddedValue <- as.matrix(ioAddedValue) #asumsikan dulu tidak ada penambahan import, jd tdk berubah importnya
nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
ioDimention <- ncol(ioIntermediateDemand) 

colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput 
ioTotalOutputInverse <- 1/ioTotalOutput
ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
ioTotalOutputInverse <- diag(ioTotalOutputInverse)



################################################################################
#                                                                              #
#                       beberapa komponen SHEET ANALISIS                       #
#                                                                              #
################################################################################
findem <- ioFinalDemand
findemcom <- ioFinalDemandComponent
colnames(findem) <- c(t(findemcom))
findem$`Total Permintaan Akhir` <- rowSums(findem)
fdSelectYear <- findem$`Total Permintaan Akhir`

#permintaan akhir
fdSelectYear <- findem$`Total Permintaan Akhir`

#output = leontif * permintaan akhir
outputSelectYear <- leontief %*% fdSelectYear

#Total output dari tabel IO -- > yang harusnya rowsum
matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
matrixIoFinalDemand <- as.matrix(ioFinalDemand)

rowSumsMatrixIoIntermediateDemand <- rowSums(matrixIoIntermediateDemand)
rowSumsMatrixIoAddedValue <- rowSums(matrixIoFinalDemand)
ioTotalOutputRow <- rowSumsMatrixIoIntermediateDemand + rowSumsMatrixIoAddedValue # ioTotaloutput 

#cek
cek <- outputSelectYear - ioTotalOutputRow

#PDRB AWAL
pdrbAwal <- outputSelectYear * proporsiPDRB
#colSums(pdrbAwal)



################################################################################
#                                                                              #
#                                 SHEET ENERGI                                 #
#                                                                              #
################################################################################
#beberapa bagian 
#Total konsumsi energi per Row
totalKonsumsiEnergi <- energyBau[,3]
#Total output
totalOutput <- ioTotalOutputRow
## koefisien energi
koefEnergi <- totalKonsumsiEnergi/ioTotalOutputRow

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
#tabelEmisiEnergiBAU$emission[,3] #total tabel emisi energi BAU


################################################################################
#                                                                              #
#                                SHEET PROYEKSI                                #
#                                                                              #
################################################################################
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
sector <- ioSector
lengthSector <- nrow(sector)
proyPertumEkonomi <- matrix(gdpRate,nrow = lengthSector,ncol = lengthYear)
#rownames(proyPertumEkonomi) <- ioSector[,1]
colnames(proyPertumEkonomi) <- column_year


fdSelectYear <- findem$`Total Permintaan Akhir` #ini untuk BAU
fdAllYear <- fdCalculate(tbl1 = proyPertumEkonomi,tbl2 = as.data.frame(fdSelectYear))

## bagian Output
outputAllYear <- leontief %*% fdAllYear


## bagian PDRB
proyPdrb <- outputAllYear*proporsiPDRB[,1]
colProyPdrb <- colSums(proyPdrb)
plot(yearFrom : yearTo,colProyPdrb) #plot pdrb

## proyeksi konsumsi energi
#koefisien energi dari sheet energi
#tabel konsumsi energi proyeksi
proyKonsumsiEnergi <- outputAllYear*koefEnergi

# tabel proporsi energi yang diambil dari tahun 2015
tabelKonsumsiEnergi <-  energyBau[,-(1:3)]
propEnergi <- tabelKonsumsiEnergi/totalKonsumsiEnergi
proyKonsumsiEnergi <- outputAllYear*koefEnergi

#terbentuk 15 tabel konsumsi energi
proyTabelKonsEnergi<-list()
for (i in 1:ncol(proyKonsumsiEnergi)) {
  proyTabelKonsEnergi[[i]]<-proyKonsumsiEnergi[,i]*propEnergi
}
names(proyTabelKonsEnergi)<-paste0("y",yearFrom:yearTo)

#matriks faktor emisi
matEfBau <- numeric()
order_energi_name <- names(energyBau)[4:ncol(energyBau)]
for(m in 1:length(order_energi_name)){
  matEfBau <- c(matEfBau, efBau[which(efBau[,1]==order_energi_name[m]), 2])
}
matEfBau <- diag(matEfBau, nrow = length(matEfBau), ncol = length(matEfBau))

# terbentuk 15 tabel proyeksi emisi
proyEmisi <- list()
for (i in 1:lengthYear) {
  proyEmisi[[i]]<-as.matrix(proyTabelKonsEnergi[[i]]) %*% matEfBau
}
names(proyEmisi)<-paste0("y",yearFrom:yearTo)

for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisi <- rowSums(proyEmisi[[i]])
  }else{
    rowsumProyEmisi<- cbind(rowsumProyEmisi,rowSums(proyEmisi[[i]]))
  }
}
colnames(rowsumProyEmisi) <- paste0("y",yearFrom:yearTo)

#COLSUM proyeksi energi
colsumProyEmisi <- colSums(rowsumProyEmisi)

################################################################################
#                                                                              #
#                           Skenario 1: PLTM on grip                           #
#                                                                              #
################################################################################

# Step 1: definisikan skenario --------------------------------------------------------------
# berpengaruh pada FD
## Asumsi pembuatan 1 PLTM menghabiskan dana $30 juta = 450 miliar lalu yang dibuat adalah 2 PLTM
## pembangunan PLTM akan berlangsung selama 1 tahun
## kurs 15000 maka total 9e11 menyesuaikan satuan tabel io dibagi juta rupiah
## 9e11/1e6 = 900.000 bertambah di tahun 2016 dan tahun 2026
### 60% bertambah di sektor (kode 30) konstruksi
### 20% bertambah di sektor (kode 27) ketenagalistrikan
### 10% bertambah di sektor (kode 34) angkutan darat
### 23% bertambah di sektor (kode 23) industri mesin dan perlengkapan YTDL

# berpengaruh pada satelit energi
## akan mengurangi konsumsi coal dan diesel setelah PLTM beroprasi (setelah tahun 2016 dan th 2026)
## asumsikan dulu sekarang berkurang 10% di HCOAL dan 5% di diesel 
## berkurang 10% di HCOAL dan 5% di dieseldi tahun 2017:2026
## berkurang 20% di HCOAL dan 10% di diesel di tahun 2027:2030

# Step 2: pilih sektor yang jadi owner ----------------------------------
## belum terbayang caranya karena masih mengasumsikan masukkan tabel baru secara keseluruhan
## baik utk FD ataupun utk satelit energinya
## jadi terbayangnya tinggal mereplace table BAU dg table skenario lalu di run ulang 


# Step 3: masukkan skenario untuk perubahan FD --------------------------
# data scenario


################################################################################
#                                                                              #
#                                SHEET PROYEKSI Sken1d                         #
#                                                                              #
################################################################################

# Step 4: Merubah akun satelit -----------------------------------------

## bagian FD
### JIKA PAKAI SKENARIO 1A --> file fdnya pakai yang skenario
fdAllYearSken1 <- as.matrix(fdSken1)
persenBahanBakarSken1Tahap1 <- as.matrix(persenBahanBakar1) #skenario 2017 sd 2026 #user input
persenBahanBakarSken1Tahap2 <- as.matrix(persenBahanBakar2) #skenario 2027:2030 #user input

## bagian Output
outputAllYearsken1 <- leontief %*% fdAllYearSken1

## bagian PDRB
proyPdrbSken1 <- outputAllYearsken1*proporsiPDRB[,1]
colProyPdrbSken1 <- colSums(proyPdrbSken1) ## 3 angka terakhirnya beda, tp nanti sampe emisinya sama dengan excel
plot(yearFrom : yearTo,colProyPdrbSken1) #plot pdrb

## proyeksi konsumsi energi
#koefisien energi dari sheet energi
#tabel proyeksi konsumsi energi
proyKonsumsiEnergiSken1 <- outputAllYearsken1*koefEnergi

#terbentuk 15 tabel konsumsi energi
### intervensi satelit energi 
### dilakukan di 2017:2026 berkurang 10% hcoal dan 5%diesel 
### sdgkan di 2027:2030 berkurang 20% hcoal dan 10% diesel karena pembangunan PLTM 2 kali lipatnya
### tabel proporsi energi yang diambil dari tahun 2015
proyTabelKonsEnergiSken1<-list()
for (i in 1:ncol(proyKonsumsiEnergiSken1)) { 
  if(i==1){
    proyTabelKonsEnergiSken1[[i]]<-proyKonsumsiEnergiSken1[,i]*propEnergi # tahun 2016 tidak ada pengurangan krn PLTM baru beroprasi 2017
  }else if(i==12 || i == 13 || i == 14 || i==15){
    proyTabelKonsEnergiSken1[[i]]<-proyKonsumsiEnergiSken1[,i]*propEnergi*persenBahanBakarSken1Tahap2
  }
  else{
    proyTabelKonsEnergiSken1[[i]]<-proyKonsumsiEnergiSken1[,i]*propEnergi*persenBahanBakarSken1Tahap1
  }
}
names(proyTabelKonsEnergiSken1)<-paste0("y",yearFrom:yearTo)

# terbentuk 15 tabel proyeksi emisi
proyEmisiSken1 <- list()
for (i in 1:lengthYear) {
  proyEmisiSken1[[i]]<-as.matrix(proyTabelKonsEnergiSken1[[i]]) %*% matEfBau #mat faktor emisi dari bau
}
names(proyEmisiSken1)<-paste0("y",yearFrom:yearTo)

for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisiSken1 <- rowSums(proyEmisiSken1[[i]])
  }else{
    rowsumProyEmisiSken1<- cbind(rowsumProyEmisiSken1,rowSums(proyEmisiSken1[[i]]))
  }
}
colnames(rowsumProyEmisiSken1) <- paste0("y",yearFrom:yearTo)

#COLSUM proyekski energi
colsumProyEmisiSken1 <- colSums(rowsumProyEmisiSken1)



################################################################################
#                                                                              #
#           Skenario 2f: subtitusi bahan bakar fosil dengan biodiesel           #
#                                                                              #
################################################################################
# Step 1 dan 2: definisikan skenario & pilih sektor yang jadi owner --------------------------------------------------------------
# berpengaruh pada FD
## Asumsi meningkatkan FD sebesar 900 M
## 50% bertambah di sektor (kode 11) Industri Batubara dan Pengilangan Migas: sector yang produksi biodiesel
## 30% bertambah di sektor (kode 3) perkebunan: permintaan CPO yang bertambah
## 20% bertambah di sektor (kode 32) perdagangan: pom bensin yang jual biodiesel


# berpengaruh pada satelit energi
## Semua kolom di bahan bakar diesel berkurang 30% di tahun 2030, persentase pengurangan gradual nya tiap tahun dipindahkan ke bahan bakar biodiesel 


# Step 3: masukkan skenario untuk perubahan FD --------------------------
################################################################################
#                                                                              #
#                                SHEET PROYEKSI sken2                          #
#                                                                              #
################################################################################

# Step 4: Merubah akun satelit -----------------------------------------

## bagian FD
### JIKA PAKAI SKENARIO 1A --> file fdnya pakai yang skenario
fdAllYearSken2 <- as.matrix(fdSken2)
persenBahanBakarSken2 <- as.matrix(persenBahanBakarSken2) 

## bagian Output
outputAllYearSken2 <- leontief %*% fdAllYearSken2


## bagian PDRB
proyPdrbSken2 <- outputAllYearSken2*proporsiPDRB[,1]
colProyPdrbSken2 <- colSums(proyPdrbSken2) ## 3 angka terakhirnya beda, tp nanti sampe emisinya sama dengan excel
plot(yearFrom : yearTo,colProyPdrbSken2) #plot pdrb

## proyeksi konsumsi energi
#koefisien energi dari sheet energi


#tabel proyeksi konsumsi energi
proyKonsumsiEnergiSken2 <- outputAllYearSken2*koefEnergi

#terbentuk 15 tabel konsumsi energi
### intervensi satelit energi 
### dimulai tahun 2017
### Semua kolom di bahan bakar diesel berkurang 30% di tahun 2030, persentase pengurangan gradual nya tiap tahun dipindahkan ke bahan bakar biodiesel 
## tabel pengali bahan bakar untuk kolom diesel (15 tabel) 
persenDiesel<- list()
gradual <- rep(0.7,52)
for (i in 1:(lengthYear-1)) {
  persenDiesel[[i]] <- as.matrix(dieselSken2-dieselSken2*gradual^(i/lengthYear))*(-1)
}

persenBiogas<- list()
for (i in 1:(lengthYear-1)) {
  persenBiogas[[i]] <- as.matrix(persenDiesel[[i]]*(-1))
}

matNol <- list()
for (i in 1:lengthYear) {
  matNol[[i]] <- as.matrix(matrix(0,nrow = 52,ncol = 26))
}

for (i in 1:lengthYear) {
  if(i==1){
    matNol[[i]] <- matNol[[i]]
  }
  else{
    matNol[[i]][,5] <- as.vector(persenDiesel[[i-1]])
    matNol[[i]][,17] <- as.vector(persenBiogas[[i-1]]) #mulai dari 2017, GANTI DI KOLOM DIESEL
    
  }
}

proyTabelKonsEnergiSken2<-list()
for (i in 1:ncol(proyKonsumsiEnergiSken2)) { 
  if(i==1){
    proyTabelKonsEnergiSken2[[i]]<-proyKonsumsiEnergiSken2[,i]*propEnergi # tahun 2016 tidak ada pengurangan krn belum diterapkan
  }
  else{
    proyTabelKonsEnergiSken2[[i]]<-proyKonsumsiEnergiSken2[,i]*propEnergi+matNol[[i]]
  }
}
names(proyTabelKonsEnergiSken2)<-paste0("y",yearFrom:yearTo)

# terbentuk 15 tabel proyeksi emisi
proyEmisiSken2 <- list()
for (i in 1:lengthYear) {
  proyEmisiSken2[[i]]<-as.matrix(proyTabelKonsEnergiSken2[[i]]) %*% matEfBau
}
names(proyEmisiSken2)<-paste0("y",yearFrom:yearTo)


for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisiSken2 <- rowSums(proyEmisiSken2[[i]])
  }else{
    rowsumProyEmisiSken2<- cbind(rowsumProyEmisiSken2,rowSums(proyEmisiSken2[[i]]))
  }
}
colnames(rowsumProyEmisiSken2) <- paste0("y",yearFrom:yearTo)


#COLSUM proyekski energi
colsumProyEmisiSken2 <- colSums(rowsumProyEmisiSken2)


 


################################################################################
#                                                                              #
#                          GABUNGAN SKENARIO 1 DAN 2                           #
#                                                                              #
################################################################################
fdAllYearS1S2 <- as.matrix(fsS1S2)

## bagian Output
outputAllYearS1S2 <- leontief %*% fdAllYearS1S2

## bagian PDRB
proyPdrbS1S2 <- outputAllYearS1S2*proporsiPDRB[,1]
colProyPdrbS1S2 <- colSums(outputAllYearS1S2) ## 3 angka terakhirnya beda, tp nanti sampe emisinya sama dengan excel
plot(yearFrom : yearTo,colProyPdrbS1S2) #plot pdrb

##konsumsi energi
KonsS1S22016 <- KonsS1S22016[,-27]
KonsS1S22017 <- KonsS1S22017[,-27]
KonsS1S22018 <- KonsS1S22018[,-27]
KonsS1S22019 <- KonsS1S22019[,-27]
KonsS1S22020 <- KonsS1S22020[,-27]
KonsS1S22021 <- KonsS1S22021[,-27]
KonsS1S22022 <- KonsS1S22022[,-27]
KonsS1S22023 <- KonsS1S22023[,-27]
KonsS1S22024 <- KonsS1S22024[,-27]
KonsS1S22025 <- KonsS1S22025[,-27]
KonsS1S22026 <- KonsS1S22026[,-27]
KonsS1S22027 <- KonsS1S22027[,-27]
KonsS1S22028 <- KonsS1S22028[,-27]
KonsS1S22029 <- KonsS1S22029[,-27]
KonsS1S22030 <- KonsS1S22030[,-27]

proyTabelKonsEnergiS1S2<-list()
for (i in 1:lengthYear) { 
  if(i==1){
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22016)
  }else if (i==2) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22017) 
  }else if (i==3) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22018) 
  }else if (i==4) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22019) 
  }else if (i==5) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22020) 
  }else if (i==6) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22021) 
  }else if (i==7) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22022) 
  }else if (i==8) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22023) 
  }else if (i==9) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22024) 
  }else if (i==10) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22025) 
  }else if (i==11) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22026) 
  }else if (i==12) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22027) 
  }else if (i==13) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22028) 
  }else if (i==14) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22029) 
  }else if (i==15) {
    proyTabelKonsEnergiS1S2[[i]]<-as.matrix(KonsS1S22030) 
  }
}
names(proyTabelKonsEnergiS1S2)<-paste0("y",yearFrom:yearTo)

# terbentuk 15 tabel proyeksi emisi
proyEmisiS1S2 <- list()
for (i in 1:lengthYear) {
  proyEmisiS1S2[[i]]<-as.matrix(proyTabelKonsEnergiS1S2[[i]]) %*% matEfBau
}
names(proyEmisiS1S2)<-paste0("y",yearFrom:yearTo)


for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisiS1S2 <- rowSums(proyEmisiS1S2[[i]])
  }else{
    rowsumProyEmisiS1S2<- cbind(rowsumProyEmisiS1S2,rowSums(proyEmisiS1S2[[i]]))
  }
}
colnames(rowsumProyEmisiS1S2) <- paste0("y",yearFrom:yearTo)


#COLSUM proyekski energi
colsumProyEmisiS1S2 <- colSums(rowsumProyEmisiS1S2)
plot(yearFrom : yearTo,colsumProyEmisiS1S2)

################################################################################
#                                                                              #
#          table delta PDRB DAN EMISI: Skenario - BAU                          #
#                                                                              #
################################################################################
# delta PDRB
deltaPDRBSken1 <- as.data.frame(colSums(proyPdrbSken1) - colSums(proyPdrb))  
deltaPDRBSken2 <- as.data.frame(colSums(proyPdrbSken1) - colSums(proyPdrb)) 
deltaPDRBS1S2 <- as.data.frame(colSums(proyPdrbS1S2) - colSums(proyPdrb)) 

# delta Emisi
deltaEmisiSken1 <- as.data.frame(colsumProyEmisiSken1 - colsumProyEmisi)  
deltaEmisiSken2 <- as.data.frame(colsumProyEmisiSken2 - colsumProyEmisi) 
deltaEmisiS1S2 <- as.data.frame(colsumProyEmisiS1S2 - colsumProyEmisi)

################################################################################
#                                                                              #
#                            save data to rds file                             #
#                                                                              #
################################################################################
#SKENARIO 1
saveRDS(fdAllYearSken1,"_DB/save rds file/fdAllYearSken1.rds") #FD
saveRDS(proyPdrbSken1,"_DB/save rds file/proyPdrbSken1.rds") #PDRB
saveRDS(proyTabelKonsEnergiSken1,"_DB/save rds file/proyTabelKonsEnergiSken1.rds") #PROYEKSI  KONSUMSI 
saveRDS(proyEmisiSken1,"_DB/save rds file/proyEmisiSken1.rds") #PROYEKSI EMISIsaveRDS(rowsumProyEmisiSken2,"_DB/save rds file/rowsumProyEmisiSken2.rds") #rowsum emisi
saveRDS(rowsumProyEmisiSken1,"_DB/save rds file/rowsumProyEmisiSken1.rds") #rowsum emisi
saveRDS(colsumProyEmisiSken1,"_DB/save rds file/colsumProyEmisiSken1.rds") #colsum emisi
saveRDS(deltaPDRBSken1,"_DB/save rds file/deltaPDRBSken1.rds")
saveRDS(deltaEmisiSken1,"_DB/save rds file/deltaEmisiSken1.rds")



# SKENARIO 2
saveRDS(fdAllYearSken2,"_DB/save rds file/fdAllYearSken1.rds") #FD
saveRDS(proyPdrbSken2,"_DB/save rds file/proyPdrbSken1.rds") #PDRB
saveRDS(proyTabelKonsEnergiSken2,"_DB/save rds file/proyTabelKonsEnergiSken1.rds") #PROYEKSI  KONSUMSI 
saveRDS(proyEmisiSken2,"_DB/save rds file/proyEmisiSken1.rds") #PROYEKSI EMISI
saveRDS(rowsumProyEmisiSken2,"_DB/save rds file/rowsumProyEmisiSken2.rds") #rowsum emisi
saveRDS(colsumProyEmisiSken2,"_DB/save rds file/colsumProyEmisiSken2.rds") #colsum emisi
saveRDS(deltaPDRBSken2,"_DB/save rds file/deltaPDRBSken2.rds")
saveRDS(deltaEmisiSken2,"_DB/save rds file/deltaEmisiSken2.rds")

# SKENARIO 1 dan 2
saveRDS(fdAllYearS1S2,"_DB/save rds file/fdAllYearS1S2.rds") #FD
saveRDS(proyPdrbS1S2,"_DB/save rds file/proyPdrbS1S2.rds") #PDRB
saveRDS(proyTabelKonsEnergiS1S2,"_DB/save rds file/proyTabelKonsEnergiS1S2.rds") #PROYEKSI  KONSUMSI 
saveRDS(proyEmisiS1S2,"_DB/save rds file/proyEmisiS1S2.rds") #PROYEKSI EMISI
saveRDS(rowsumProyEmisiS1S2,"_DB/save rds file/rowsumProyEmisiS1S2.rds") #rowsum emisi
saveRDS(colsumProyEmisiS1S2,"_DB/save rds file/colsumProyEmisiS1S2.rds") #colsum emisi
saveRDS(deltaPDRBS1S2,"_DB/save rds file/deltaPDRBS1S2.rds")
saveRDS(deltaEmisiS1S2,"_DB/save rds file/deltaEmisiS1S2.rds")


write.csv(colProyPdrbS1S2,"_DB/csv gabungan sken 1 dan sken 2/colProyPdrbS1S2.csv")
write.csv(colsumProyEmisiS1S2,"_DB/csv gabungan sken 1 dan sken 2/colsumProyEmisiS1S2.csv")
