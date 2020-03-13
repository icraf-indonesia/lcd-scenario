#set wd di laptop dw
setwd("C:/dw/ICRAF/redcluew/syntax/lcd-scenario")
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
#inIntermediateDemand <- paste0(datapathCSV, "/02_input_antara_skenario1.csv")
inFD <- paste0(datapathCSV, "/17_final_demand_proyeksi_skenario1_b.csv")
inFD2 <- paste0(datapathCSV, "/17_final_demand_proyeksi_skenario2.csv")
inEmissionFactorEnergiTable<- paste0(datapathCSV, "/10_faktor_emisi_energi.csv") # asumsi faktor emisi tidak berubah
inProporsiPDRB <- paste0(datapathCSV, "/16_proporsi_pdrb.csv")
inPersentaseBahanBAkar1 <- paste0(datapathCSV, "/18_persentase_bahan_bakar.csv")
inPersentaseBahanBAkar2 <- paste0(datapathCSV, "/18_persentase_bahan_bakar_2.csv")
inPersentaseBahanBAkarSken2 <- paste0(datapathCSV, "/18_persentase_bahan_bakar_sken2.csv")

#indemScen1 <- read.table(inIntermediateDemand, header=F, sep=",")
fdSken1 <- read.table(inFD, header=TRUE, sep=",", stringsAsFactors = F)
fdSken2 <- read.table(inFD2, header=TRUE, sep=",", stringsAsFactors = F)
ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, sep=",", stringsAsFactors = F)
proporsiPDRB <- read.table(inProporsiPDRB, header=TRUE, sep=",", stringsAsFactors = F)
persenBahanBakar1 <- read.table(inPersentaseBahanBAkar1, header=F, sep=",", stringsAsFactors = F)
persenBahanBakar2 <- read.table(inPersentaseBahanBAkar2, header=F, sep=",", stringsAsFactors = F)
persenBahanBakarSken2 <- read.table(inPersentaseBahanBAkarSken2, header=F, sep=",", stringsAsFactors = F)

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
#                                SHEET PROYEKSI Sken1b                         #
#                                                                              #
################################################################################

# Step 4: Merubah akun satelit -----------------------------------------

## bagian FD
### JIKA PAKAI SKENARIO 1A --> file fdnya pakai yang skenario
fdAllYear <- as.matrix(fdSken1)
persenBahanBakar1 <- as.matrix(persenBahanBakar1) #skenario 2017 sd 2026 #user input
persenBahanBakar2 <- as.matrix(persenBahanBakar2) #skenario 2027:2030 #user input

## bagian Output
outputAllYear <- leontief %*% fdAllYear

## bagian PDRB
proyPdrb <- outputAllYear*proporsiPDRB[,1]
colProyPdrb <- colSums(proyPdrb) ## 3 angka terakhirnya beda, tp nanti sampe emisinya sama dengan excel
plot(yearFrom : yearTo,colProyPdrb) #plot pdrb

## proyeksi konsumsi energi
#koefisien energi dari sheet energi
#tabel konsumsi energi proyeksi
proyKonsumsiEnergi <- outputAllYear*koefEnergi

#tabel proyeksi konsumsi energi
proyKonsumsiEnergi <- outputAllYear*koefEnergi

#terbentuk 15 tabel konsumsi energi
### intervensi satelit energi 
### dilakukan di 2017:2026 berkurang 10% hcoal dan 5%diesel 
### sdgkan di 2027:2030 berkurang 20% hcoal dan 10% diesel karena pembangunan PLTM 2 kali lipatnya
### tabel proporsi energi yang diambil dari tahun 2015
proyTabelKonsEnergi<-list()
for (i in 1:ncol(proyKonsumsiEnergi)) { 
  if(i==1){
    proyTabelKonsEnergi[[i]]<-proyKonsumsiEnergi[,i]*propEnergi # tahun 2016 tidak ada pengurangan krn PLTM baru beroprasi 2017
  }else if(i==12 || i == 13 || i == 14 || i==15){
    proyTabelKonsEnergi[[i]]<-proyKonsumsiEnergi[,i]*propEnergi*persenBahanBakar2
  }
  else{
    proyTabelKonsEnergi[[i]]<-proyKonsumsiEnergi[,i]*propEnergi*persenBahanBakar1
  }
}
names(proyTabelKonsEnergi)<-paste0("y",yearFrom:yearTo)

# terbentuk 15 tabel proyeksi emisi
proyEmisi <- list()
for (i in 1:lengthYear) {
  proyEmisi[[i]]<-as.matrix(proyTabelKonsEnergi[[i]]) %*% matEfBau #mat faktor emisi dari bau
}
names(proyEmisi)<-paste0("y",yearFrom:yearTo)

for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisi <- rowSums(proyEmisi[[i]])
  }else{
    rowsumProyEmisi<- cbind(rowsumProyEmisi,rowSums(proyEmisi[[i]]))
  }
}


#COLSUM proyekski energi
colsumProyEmisiSken <- colSums(rowsumProyEmisi)



################################################################################
#                                                                              #
#           Skenario 2: subtitusi bahan bakar fosil dengan biodiesel           #
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
fdAllYear <- as.matrix(fdSken2)
persenBahanBakarSken2 <- as.matrix(persenBahanBakarSken2) 

## bagian Output
outputAllYear <- leontief %*% fdAllYear


## bagian PDRB
proyPdrb <- outputAllYear*proporsiPDRB[,1]
colProyPdrb <- colSums(proyPdrb) ## 3 angka terakhirnya beda, tp nanti sampe emisinya sama dengan excel
plot(yearFrom : yearTo,colProyPdrb) #plot pdrb

## proyeksi konsumsi energi
#koefisien energi dari sheet energi
#tabel konsumsi energi proyeksi
proyKonsumsiEnergi <- outputAllYear*koefEnergi

#tabel proyeksi konsumsi energi
proyKonsumsiEnergi <- outputAllYear*koefEnergi

#terbentuk 15 tabel konsumsi energi
### intervensi satelit energi 
### dimulai tahun 2017
### Semua kolom di bahan bakar diesel berkurang 30% di tahun 2030, persentase pengurangan gradual nya tiap tahun dipindahkan ke bahan bakar biodiesel 
proyTabelKonsEnergi<-list()
for (i in 1:ncol(proyKonsumsiEnergi)) { 
  if(i==1){
    proyTabelKonsEnergi[[i]]<-proyKonsumsiEnergi[,i]*propEnergi # tahun 2016 tidak ada pengurangan krn belum diterapkan
  }
  else{
    proyTabelKonsEnergi[[i]]<-proyKonsumsiEnergi[,i]*propEnergi*persenBahanBakarSken2
  }
}
names(proyTabelKonsEnergi)<-paste0("y",yearFrom:yearTo)

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


#COLSUM proyekski energi
colsumProyEmisiSken2 <- colSums(rowsumProyEmisi)




