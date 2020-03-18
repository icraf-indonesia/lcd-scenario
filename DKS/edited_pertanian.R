library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)



###BEGIN: initiate all variables ####
# username <- "alfanugraha"
# password <- "1234"
selectedProv = "JaBar"
datapath <- paste0("data/", selectedProv, "/")


datapathCSV <- pathcsv
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



##### Read csv files from my own folder in laptop #####

year <- read.table("DKS/datapathCSV/year.csv", header = TRUE, sep = ",")
proporsiPDRB <- read.table("DKS/datapathCSV/proporsi_pdrb.csv", header = TRUE, sep = ",")
satellite_pertanian <- read.table("DKS/datapathCSV/satellite_pertanian.csv", header = TRUE, sep = ",")
emissionFactorPertanian <- read.table("DKS/datapathCSV/emissionFaktorPertanian.csv", header = TRUE, sep = ",")
fdScen1 <- read.table("DKS/datapathCSV/fdScen1.csv", header = TRUE, sep = ",")
fdScen1 <- read.table("DKS/datapathCSV/fdScen1.csv", header = TRUE, sep = ",")
fdScen2<- read.table("DKS/datapathCSV/fdScen2.csv", header = TRUE, sep = ",")
fdComb<- read.table("DKS/datapathCSV/fdCombination.csv", header = TRUE, sep = ",")



#### USER INPUT ###

gdpRate <- 5/100
yearFrom <- 2016
yearTo <- 2030


### BAU SCENARIO ###
ioIntermediateDemand
pertanian_Bau <- satellite_pertanian
ef_Bau <- emissionFactorPertanian


### IO CALCULATION FOR YEAR 2015 ###

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



#FINAL DEMAND, OUTPUT AND GGP CALCULATION FOR YEAR 2015

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



####                            ###
### SATELLITE ACCOUNT PERTANIAN ###
###                             ###

#Total konsumsi pupuk pertanian per Row
totalKonsumsiPertanian <- pertanian_Bau [,3]

#Total output
totalOutput <- ioTotalOutputRow

## koefisien pertanian
koefPertanian <- totalKonsumsiPertanian/totalOutput




###### PROYEKSI 2016-2030 #######

#### 1. Proyeksi Final Demand ####

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
colnames(proyPertumEkonomi) <- column_year


fdSelectYear <- findem$`Total Permintaan Akhir` #ini untuk BAU
fdAllYear <- fdCalculate(tbl1 = proyPertumEkonomi,tbl2 = as.data.frame(fdSelectYear))


#### 2. Proyeksi Output ####
outputAllYear <- leontief %*% fdAllYear


## 3. Proyeksi PDRB ####
proyPdrb <- outputAllYear*proporsiPDRB[,1]
colProyPdrb <- colSums(proyPdrb)
plot(yearFrom : yearTo,colProyPdrb) #plot pdrb

#Graph of PDRB Projection (2016-2030)
ProyPDRB_table<- cbind.data.frame(year,colProyPdrb)
pdrb_graph<-ggplot(data=ProyPDRB_table,aes(x=Year, y=colProyPdrb))+
  geom_line(color="red", stat = "identity")+ylab("PDRB (juta Rupiah")


## 4. Proyeksi Konsumsi Pertanian ##
tabelKonsumsiPertanian <-  pertanian_Bau[,-(1:3)]
propPertanian <- tabelKonsumsiPertanian/totalKonsumsiPertanian
#Replace NaN value with 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
propPertanian[is.nan(propPertanian)] <- 0

proyKonsumsiPertanian<- outputAllYear*koefPertanian


#terbentuk 15 tabel konsumsi pertanian
proyTabelKonsPertanian<-list()
for (i in 1:ncol(proyKonsumsiPertanian)) {
  proyTabelKonsPertanian[[i]]<-proyKonsumsiPertanian [,i]*propPertanian  
}
names(proyTabelKonsPertanian)<-paste0("y",yearFrom:yearTo)
        

##### 5.Proyeksi Emisi  ######
#matriks faktor emisi
matEfBau <- numeric()
order_Pertanian_name <- names(pertanian_Bau)[4:ncol(pertanian_Bau)]
for(m in 1:length(order_Pertanian_name)){
  matEfBau <- c(matEfBau, ef_Bau[which(ef_Bau[,1]==order_Pertanian_name[m]), 2])
}
matEfBau <- diag(matEfBau, nrow = length(matEfBau), ncol = length(matEfBau))



# terbentuk 15 tabel proyeksi emisi
proyEmisi <- list()
for (i in 1:lengthYear) {
  proyEmisi[[i]]<-as.matrix(proyTabelKonsPertanian[[i]]) %*% matEfBau
}
names(proyEmisi)<-paste0("y",yearFrom:yearTo)

for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisi <- rowSums(proyEmisi[[i]])
  }else{
    rowsumProyEmisi<- cbind(rowsumProyEmisi,rowSums(proyEmisi[[i]]))
  }
}

#COLSUM proyeksi emisi pertanian
colsumProyEmisi <- colSums(rowsumProyEmisi)



#######                                          #######
#######    SKENARIO 1: PENGGUNAAN PUPUK ORGANIK  ####### 
#######                                          #######


#Merubah Final demand untuk beberapa sektor terpengaruh thd skenario 1
# Sektor terpengaruh tsb adalah:
# 1. Jasa pertanian dan perburuan (60%)
# 2. Jasa lainnya (40%)


####### __________PERUBAHAN FINAL DEMAND ______________ #######

#Final demand and Output (2015)
FD_Scenario1 <- as.matrix(fdScen1) 
outputScen1 <- leontief %*% FD_Scenario1

#Proyeksi PDRB (2016-2030)
proyPdrb_scen1 <- outputScen1*proporsiPDRB[,1]
colProyPdrb_scen1 <- colSums(proyPdrb_scen1) ## 3 angka terakhirnya beda, tp nanti sampe emisinya sama dengan excel
plot(yearFrom : yearTo,colProyPdrb_scen1) #plot pdrb

#Graph of PDRB Projection using Scenario 1 (2016-2030)
ProyPDRB_table1<- cbind.data.frame(year,colProyPdrb_scen1)
pdrb_graph1<-ggplot(data=ProyPDRB_table1,aes(x=Year, y=colProyPdrb_scen1))+
  geom_line(color="red", stat = "identity")+ylab("PDRB (juta Rupiah")


##Proyeksi konsumsi pupuk pertanian Skenario 1
proyKonsPertanian_Scen1 <- outputScen1*koefPertanian
colnames(proyKonsPertanian_Scen1)<-paste0("Kons",yearFrom:yearTo)

#tabel konsumsi pertanian per 5 jenis pupuk berbeda menggunakan Skenario 1
proyTabelKonsPertanian_s1<-list()
for (i in 1:15) {
  proyTabelKonsPertanian_s1[[i]]<-proyKonsPertanian_Scen1 [,i]*propPertanian  
}
names(proyTabelKonsPertanian_s1)<-paste0("Kons1_",yearFrom:yearTo)



######## _____________ PERUBAHAN JUMLAH KONSUMSI PUPUK KIMIA DAN ORGANIK _________ ########

#Perhitungan pengurangan konsumsi pupuk kimia sebesar 0.02 tiap tahunnya(0.005% tiap jenis pupuk yg berbeda)
proyKonsumsiPertanian_ferS1 <- list()
for (i in 1:15) {
  proyKonsumsiPertanian_ferS1[[i]]<- proyTabelKonsPertanian_s1[[i]]*0.995
}
names(proyKonsumsiPertanian_ferS1)<-paste0("Kons2_",yearFrom:yearTo)


#Mengeluarkan list proyeksi konsumsi menjadi dataframe untuk kemudian dihitung selisih konsumsinya
#Terbentuk 15 dataframe konsumsi pupuk awal untuk skenario 2
year <- 2016:2030
i <- 1:length(year)
eval(parse(text=paste("konsPupuk_", year, "<- proyTabelKonsPertanian_s1[[", i, "]]", sep="")))


#Terbentuk 15 dataframe konsumsi pupuk setelah dikurangi 0.02 tiap tahunnya
eval(parse(text=paste("konsPupukBaru_", i , "<- proyKonsumsiPertanian_ferS1[[", i, "]]", sep="")))


#Menghitung selisih konsumsi pupuk kimia yang berkurang 0.02 tiap tahunnya (0.005 tiap jenis pupuk yg berbeda)
eval(parse(text=paste("deltaKonsPupukKimia_", year, "<- 
                      rowSums(konsPupuk_", year, "[,1:4] - konsPupukBaru_", i, "[,1:4])", sep="")))

eval(parse(text=paste("konsPupukBaru_", i ," $ORGANIK <- 
                      (konsPupuk_",year, "$ORGANIK + deltaKonsPupukKimia_", year,")", sep="")))


#creating list from Konsumsi 
konsFinal_<- list()
for(i in 1:length(year)){
  eval(parse(text=paste("konsFinal_[[",i,"]] <- konsPupukBaru_",i, sep="")))
}


#Proyeksi emisi (2016-2030) dengan menggunakan tabel konsumsi pupuk baru
# 15 tabel proyeksi emisi
proyEmisi_s1 <- list()
for (i in 1:lengthYear) {
  proyEmisi_s1[[i]]<-as.matrix(konsFinal_[[i]]) %*% matEfBau
}
names(proyEmisi_s1)<-paste0("y",yearFrom:yearTo)

for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisi_s1 <- rowSums(proyEmisi_s1[[i]])
  }else{
    rowsumProyEmisi_s1<- cbind(rowsumProyEmisi_s1,rowSums(proyEmisi_s1[[i]]))
  }
}

#COLSUM proyeksi pertanian
colsumProyEmisi_s1 <- colSums(rowsumProyEmisi_s1)
tabelProyEmisi_s1 <- cbind.data.frame(year,colsumProyEmisi_s1)


### Proyeksi Intensitas Emisi Skenario 2 ###
proyIntensitas_s1 <- colsumProyEmisi_s1/colProyPdrb_scen1
tabelIntesitas_S1 <- cbind.data.frame(year,proyIntensitas_s1)

proyEmisi <- NULL
initial <- 2015
for(i in 1:lengthYear){
  eval(parse(text=paste0("proyEmisi <- cbind(proyEmisi, rowSums(proyEmisi_s1$y", initial + i,"))")))
  # proyEmisi <- cbind(proyEmisi, rowSums(proyEmisi_s1$y2016))
}
proyEmisi<-cbind(as.character(sector[,1]), proyEmisi)
colnames(proyEmisi)<-c("sector", column_year)



#######                                                #######
#######    SKENARIO 2: PEMBANGUNAN SISTEM IRIGASI SRI  ####### 
#######                                                ######


####### __________PERUBAHAN FINAL DEMAND ______________ #######

#Merubah Final demand untuk beberapa sektor terpengaruh thd skenario 2
# Sektor terpengaruh tsb adalah:
# 1. Konstruksi (70%)
# 2. Jasa lainnya (30%)

#Final demand and Output (2015)
FD_Scenario2 <- as.matrix(fdScen2) 
outputScen2 <- leontief %*% FD_Scenario2


#Proyeksi PDRB (2016-2030)
proyPdrb_scen2 <- outputScen2*proporsiPDRB[,1]
colnames(proyPdrb_scen2)<-paste0("PDRB_",yearFrom:yearTo)
colProyPdrb_scen2 <- colSums(proyPdrb_scen2) ## 3 angka terakhirnya beda, tp nanti sampe emisinya sama dengan excel
plot(yearFrom : yearTo,colProyPdrb_scen2) #plot pdrb

#Graph of PDRB Projection using Scenario 1 (2016-2030)
ProyPDRB_table2<- cbind.data.frame(year,colProyPdrb_scen2)
pdrb_graph2<-ggplot(data=ProyPDRB_table2,aes(x=Year, y=colProyPdrb_scen2))+
  geom_line(color="red", stat = "identity")+ylab("PDRB (juta Rupiah")


##Proyeksi konsumsi pupuk pertanian Skenario 2
proyKonsPertanian_Scen2 <- outputScen2*koefPertanian
colnames(proyKonsPertanian_Scen2)<-paste0("Kons",yearFrom:yearTo)

#tabel konsumsi pertanian per 5 jenis pupuk berbeda menggunakan Skenario 2
proyTabelKonsPertanian_s2<-list()
for (i in 1:ncol(proyKonsPertanian_Scen2)) {
  proyTabelKonsPertanian_s2[[i]]<-proyKonsPertanian_Scen2 [,i]*propPertanian  
}
names(proyTabelKonsPertanian_s2)<-paste0("Kons_s1",yearFrom:yearTo)



######## _____________ PERUBAHAN FAKTOR EMISI JENIS PUPUK _________ ########

# Terjadi pengurangan nilai faktor emisi utk pupuk kimia sebesar 30% per jenis pupuk
#Calculate the changes of Emission Factor for each fertilizer types using gradual rate (0.02% per year)

initial_EF<-as.data.frame(ef_Bau$Em_F)
grad_rate <- 0.005
EF_next.year<-initial_EF-(initial_EF*grad_rate)
EF_grad<-EF_next.year
for (i in 1:14) {
  EF_next.year<-EF_next.year-(EF_next.year*grad_rate)
  EF_grad<-cbind(EF_grad,EF_next.year)
}
names(EF_grad)<-paste0("EF_s2",yearFrom:yearTo)

efOrg<-(c(0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03))
efOrganik<- as.data.frame(t(efOrg))
names(efOrganik)<-paste0("EF_s2",yearFrom:yearTo)


#Tabel Emission Factor Baru
ef_table_s2<- rbind(EF_grad[1:4,],efOrganik)

#Perkalian diagonal tabel faktor emisi baru
ef_scen2_diag <- list()
for (i in 1:ncol(ef_table_s2)) {
  ef_scen2_diag[[i]]<-diag(ef_table_s2[[i]])
}


#Proyeksi emisi (2016-2030) dengan menggunakan EF faktor baru
# 15 tabel proyeksi emisi
proyEmisi_s2 <- list()
for (i in 1:lengthYear) {
  proyEmisi_s2[[i]]<-as.matrix(proyTabelKonsPertanian_s2[[i]]) %*% ef_scen2_diag[[i]]
}
names(proyEmisi_s2)<-paste0("y",yearFrom:yearTo)

for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisi_s2 <- rowSums(proyEmisi_s2[[i]])
  }else{
    rowsumProyEmisi_s2<- cbind(rowsumProyEmisi_s2,rowSums(proyEmisi_s2[[i]]))
  }
}


proyEmisi <- NULL
initial <- 2015
for(i in 1:lengthYear){
  eval(parse(text=paste0("proyEmisi <- cbind(proyEmisi, rowSums(proyEmisi_s2$y", initial + i,"))")))
  # proyEmisi <- cbind(proyEmisi, rowSums(proyEmisi_s1$y2016))
}
proyEmisi<-cbind(as.character(sector[,1]), proyEmisi)
colnames(proyEmisi)<-c("sector", column_year)




#COLSUM proyeksi emisi pertanian
colsumProyEmisi_s2 <- colSums(rowsumProyEmisi_s2)


#Proyeksi Intensitas Emisi Skenario 2
proyIntensitas_s2 <- colsumProyEmisi_s2/colProyPdrb_scen2
tabelIntesitas_S2 <- cbind.data.frame(year,proyIntensitas_s2)




#######                                                #######
#######         KOMBINASI AKSI SKENARIO 1&2            ####### 
#######                                                ######


#######  __________ PROYEKSI PDRB _____________ #######

#### 1. Final Demand kombinasi 2 skenario: fdComb
fdComb_m <- as.matrix(fdComb)

#### 2. Proyeksi Output ####
outputComb <- leontief %*% fdComb_m


## 3. Proyeksi PDRB ####
proyPdrb_Comb <- outputComb*proporsiPDRB[,1]
colProyPdrb_Comb <- colSums(proyPdrb_Comb)

#Graph of PDRB Projection using Scenario 1 (2016-2030)
TablePDRB_Comb<- cbind.data.frame(year,colProyPdrb_Comb)
pdrbGraph_Comb<-ggplot(data=TablePDRB_Comb,aes(x=year, y=colProyPdrb_Comb))+
  geom_line(color="red", stat = "identity")+ylab("PDRB (juta Rupiah")



######## __________ PROYEKSI KONSUMSI PUPUK ____________ #########

##Proyeksi konsumsi pupuk pertanian kombinasi aksi
proyKonsPertanian_Comb <- outputComb*koefPertanian
colnames(proyKonsPertanian_Comb)<-paste0("Kons",yearFrom:yearTo)

#tabel konsumsi pertanian per 5 jenis pupuk berbeda menggunakan kombinasi aksi
proyTabelKonsPupuk_Comb<-list()
for (i in 1:15) {
  proyTabelKonsPupuk_Comb[[i]]<-proyKonsPertanian_Comb [,i]*propPertanian  
}
names(proyTabelKonsPupuk_Comb)<-paste0("Kons1_",yearFrom:yearTo)



######## _____________ PERUBAHAN JUMLAH KONSUMSI PUPUK KIMIA DAN ORGANIK _________ ########

#Perhitungan pengurangan konsumsi pupuk kimia sebesar 0.02 tiap tahunnya(0.005% tiap jenis pupuk yg berbeda)
proyKonsumsiPupuk_ferComb <- list()
for (i in 1:15) {
  proyKonsumsiPupuk_ferComb[[i]]<- proyTabelKonsPupuk_Comb [[i]]*0.995
}
names(proyKonsumsiPupuk_ferComb)<-paste0("Kons2_",yearFrom:yearTo)


#Mengeluarkan list proyeksi konsumsi menjadi dataframe untuk kemudian dihitung selisih konsumsinya

#Terbentuk 15 dataframe konsumsi pupuk awal untuk kombinasi aksi
i=1:length(year)
eval(parse(text=paste("konsPupuk_Comb_", i, "<- proyTabelKonsPupuk_Comb[[", i, "]]", sep="")))


#Terbentuk 15 dataframe konsumsi pupuk setelah dikurangi 0.02 tiap tahunnya
eval(parse(text=paste("konsBaru_Comb_", i , "<- proyKonsumsiPupuk_ferComb[[", i, "]]", sep="")))


#Menghitung selisih konsumsi pupuk kimia yang berkurang 0.02 tiap tahunnya (0.005 tiap jenis pupuk yg berbeda)
eval(parse(text=paste("deltaKonsPupuk_Comb_", year, "<- 
                      rowSums(konsPupuk_Comb_", i, "[,1:4] - konsBaru_Comb_", i, "[,1:4])", sep="")))

eval(parse(text=paste("konsPupuk_Comb_", i ," $ORGANIK <- 
                      (konsPupuk_Comb_",i, "$ORGANIK + deltaKonsPupuk_Comb_", year,")", sep="")))


#creating list from Konsumsi 
konsFinal_Comb_<- list()
  for(i in 1:length(year)){
  eval(parse(text=paste("konsFinal_Comb_[[",i,"]] <- konsPupuk_Comb_",i, sep="")))
}



######## __________ PROYEKSI EMISI ____________ #########


# Terjadi pengurangan nilai faktor emisi utk pupuk kimia sebesar 30% per jenis pupuk
#Perhitungan proyeksi emisi menggunakan tabel emission factor yang baru: ef_scen2_diag (class:list)


#Proyeksi emisi (2016-2030) dengan menggunakan EF faktor baru
# 15 tabel proyeksi emisi
proyEmisi_Comb <- list()
for (i in 1:lengthYear) {
  proyEmisi_Comb[[i]]<-as.matrix(konsFinal_Comb_[[i]]) %*% ef_scen2_diag[[i]]
}
names(proyEmisi_Comb)<-paste0("y",yearFrom:yearTo)




for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisi_Comb <- rowSums(proyEmisi_Comb[[i]])
  }else{
    rowsumProyEmisi_Comb<- cbind(rowsumProyEmisi_Comb,rowSums(proyEmisi_Comb[[i]]))
  }
}


proyEmisi <- NULL
initial <- 2015
for(i in 1:lengthYear){
  eval(parse(text=paste0("proyEmisi <- cbind(proyEmisi, rowSums(proyEmisi_Comb$y", initial + i,"))")))
  # proyEmisi <- cbind(proyEmisi, rowSums(proyEmisi_s1$y2016))
}
proyEmisi<-cbind(as.character(sector[,1]), proyEmisi)
colnames(proyEmisi)<-c("sector", column_year)


#COLSUM proyeksi emisi pertanian
colsumProyEmisi_Comb <- colSums(rowsumProyEmisi_Comb)


#Proyeksi Intensitas Emisi Kombinasi Aksi
proyIntensitas_Comb <- colsumProyEmisi_Comb/colProyPdrb_Comb
tabelIntesitas_Comb <- cbind.data.frame(year,proyIntensitas_Comb)



#######  ______________________________________________    #######
#######            PDRB, EMISI DAN INTENSITAS EMISI        ####### 
#######                    PER SEKTOR                      ######
#######  ______________________________________________    ######


#### Skenario 1 Per Sektor #####

#Proyeksi PDRB per sektor Skenario 1
proyPdrb_scen1 <- outputScen1*proporsiPDRB[,1]
colnames(proyPdrb_scen1)<-paste0("PDRB_",yearFrom:yearTo)
ProyPDRB_Sector1<- cbind.data.frame(sector,proyPdrb_scen1)

#Proyeksi Emisi per sektor Skenario 1
rowsumProyEmisi_s1 <- list()
for (i in 1:length(year)) {
  rowsumProyEmisi_s1 [[i]]<- rowSums(proyEmisi_s1[[i]])
}

tabelProyEms_sektor1 <- as.data.frame(rowsumProyEmisi_s1)
names(tabelProyEms_sektor1)<-paste0("Emisi_",yearFrom:yearTo)  


#Proyeksi Intensitas Emisi per Sektor Skenario 1
IE_sektor1<- tabelProyEms_sektor1/ProyPDRB_Sector1[,3:17]
colnames(IE_sektor1)<-paste0("IE_",yearFrom:yearTo)
IETable_sektor1<- cbind.data.frame(sector,IE_sektor1)




###### Skenario 2 per sektor ########

#Proyeksi PDRB per sektor Skenario 2
proyPdrb_scen2 <- outputScen2*proporsiPDRB[,1]
colnames(proyPdrb_scen2)<-paste0("PDRB_",yearFrom:yearTo)
ProyPDRB_Sector2<- cbind.data.frame(sector,proyPdrb_scen2)

#Proyeksi Emisi per sektor Skenario 2
rowsumProyEmisi_s2 <- list()
for (i in 1:length(year)) {
  rowsumProyEmisi_s2 [[i]]<- rowSums(proyEmisi_s2[[i]])
}

tabelProyEms_sektor2 <- as.data.frame(rowsumProyEmisi_s2)
names(tabelProyEms_sektor2)<-paste0("Emisi_",yearFrom:yearTo) 

#Proyeksi Intensitas Emisi per Sektor Skenario 2
IE_sektor2<- tabelProyEms_sektor2/ProyPDRB_Sector2[,3:17]
colnames(IE_sektor2)<-paste0("IE_",yearFrom:yearTo)
IETable_sektor2<- cbind.data.frame(sector,IE_sektor2)
                                   
 
                                  
                                   
######## Skenario Kombinasi Aksi per Sektor ########
#Proyeksi PDRB per sektor Aksi Kombinasi
proyPdrb_Comb <- outputComb*proporsiPDRB[,1]
colnames(proyPdrb_Comb)<-paste0("PDRB_",yearFrom:yearTo)
ProyPDRB_Combi<- cbind.data.frame(sector,proyPdrb_Comb)


#Proyeksi Emisi per sektor Kombinasi Aksi
rowProyEmisi_Comb <- list()
for (i in 1:length(year)) {
  rowProyEmisi_Comb [[i]]<- rowSums(proyEmisi_Comb[[i]])
}

tabelProyEms_Comb <- as.data.frame(rowProyEmisi_Comb)
names(tabelProyEms_Comb)<-paste0("Emisi_",yearFrom:yearTo)

#Proyeksi Intensitas Emisi per Sektor Kombinasi Aksi
IE_Comb<- tabelProyEms_Comb/ProyPDRB_Combi[,3:17]
colnames(IE_Comb)<-paste0("IE_",yearFrom:yearTo)
IETable_Comb<- cbind.data.frame(sector,IE_Comb)



#######  ______________________________________________    #######
#######            PDRB, EMISI DAN INTENSITAS EMISI        ####### 
#######              BAU VS SKENARIO PER SEKTOR            ######
#######  ______________________________________________    ######


#########            BAU VS Skenario 1       #############


######## Delta PDRB BAU VS SKenario 1 #######
deltaPDRB_S1 <- ProyPDRB_Sector1[,3:17]-proyPdrb
colnames(deltaPDRB_S1)<-paste0("DeltaPDRB_",yearFrom:yearTo)
TabledeltaPDRB_S1 <- cbind.data.frame(sector,deltaPDRB_S1)

######## Emisi ##########
#Proyeksi Emisi per sektor Skenario BAU
rowsumProyEmisi_BAU <- list()
for (i in 1:length(year)) {
  rowsumProyEmisi_BAU [[i]]<- rowSums(proyEmisi[[i]])
}
tabelProyEms_BAU <- as.data.frame(rowsumProyEmisi_BAU)
names(tabelProyEms_BAU)<-paste0("EmisiBAU_",yearFrom:yearTo)

######### Delta Emisi BAU VS SKenario 1 #########
deltaEmisi_S1 <- tabelProyEms_sektor1-tabelProyEms_BAU
colnames(deltaEmisi_S1)<-paste0("DeltaEmisi_",yearFrom:yearTo)
TabledeltaEmisi_S1 <- cbind.data.frame(sector,deltaEmisi_S1)


######## Intensitas Emisi ##########
#Proyeksi Intensitas Emisi per sektor Skenario BAU
IE_BAU <- tabelProyEms_BAU/proyPdrb

#Delta IE BAU VS Skenario 1 #######
deltaIE_S1 <- IE_sektor1-IE_BAU
colnames(deltaIE_S1)<-paste0("DeltaIE_",yearFrom:yearTo)
TabledeltaIE_S1 <- cbind.data.frame(sector,deltaIE_S1)




#########            BAU VS Skenario 2       #############

######## Delta PDRB BAU VS SKenario 2 #######
deltaPDRB_S2 <- ProyPDRB_Sector2[,3:17]-proyPdrb
colnames(deltaPDRB_S2)<-paste0("DeltaPDRB_",yearFrom:yearTo)
TabledeltaPDRB_S2 <- cbind.data.frame(sector,deltaPDRB_S2)


######### Delta Emisi BAU VS SKenario 2 #########
deltaEmisi_S2 <- tabelProyEms_sektor2-tabelProyEms_BAU
colnames(deltaEmisi_S2)<-paste0("DeltaEmisi_",yearFrom:yearTo)
TabledeltaEmisi_S2 <- cbind.data.frame(sector,deltaEmisi_S2)


#Delta IE BAU VS Skenario 2 #######
deltaIE_S2 <- IE_sektor2-IE_BAU
colnames(deltaIE_S2)<-paste0("DeltaIE_",yearFrom:yearTo)
TabledeltaIE_S2 <- cbind.data.frame(sector,deltaIE_S2)




#########            BAU VS Kombinasi Aksi       #############

######## Delta PDRB BAU VS Kombinasi Aksi #######
deltaPDRB_Comb <- proyPdrb_Comb - proyPdrb
colnames(deltaPDRB_Comb)<-paste0("DeltaPDRB_",yearFrom:yearTo)
TabledeltaPDRB_Comb <- cbind.data.frame(sector,deltaPDRB_Comb)


######### Delta Emisi BAU VS Kombinasi Aksi #########
deltaEmisi_Comb <- tabelProyEms_Comb - tabelProyEms_BAU
colnames(deltaEmisi_Comb)<-paste0("DeltaEmisi_",yearFrom:yearTo)
TabledeltaEmisi_Comb <- cbind.data.frame(sector,deltaEmisi_Comb)


#Delta IE BAU VS Kombinasi Aksi #######
deltaIE_Comb <- IE_Comb-IE_BAU
colnames(deltaIE_Comb)<-paste0("DeltaIE_",yearFrom:yearTo)
TabledeltaIE_Comb <- cbind.data.frame(sector,deltaIE_Comb)

