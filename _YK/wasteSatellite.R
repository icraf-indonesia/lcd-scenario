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
inemissionFactorWaste <- "_YK/raw/11_faktor_emisi_limbah.csv"
emissionFactorWaste <- read.table(inemissionFactorWaste, header = TRUE, sep = ",")
num_ef <- length(emissionFactorWaste$Em_F)
ef_matrix <- diag(as.vector(emissionFactorWaste$Em_F), ncol = num_ef, nrow = num_ef)
ef_waste_matrix <- as.matrix(satelliteWaste[,4:19]) %*% ef_matrix

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
                              Koef_Limbah = wasteCoef,
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
      #Buat tampilan di SHINY line 151 - 172
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
      infaktorEmisi <- read.table("_YK/raw/11_faktor_emisi_limbah.csv", header = TRUE, sep = ",")
      num_input_ef <- length(infaktorEmisi$Em_F)
      input_ef_matrix <- diag(as.vector(infaktorEmisi$Em_F), ncol = num_input_ef, nrow = num_input_ef)
      tableEmisi <- as.matrix(tableBuanganLimbah_t) %*% input_ef_matrix
      emisi_t <- as.data.frame(rowSums(tableEmisi))
      colnames(emisi_t) <- "Total Emisi"
      totalEmisi_t <- sum(emisi_t)
      
    ## Membuat tabel "Total Buangan dan Faktor Emisi" tahun awal-akhir ##
      
      i <- 1:length(year)
      eval(parse(text=(paste("tableBuanganLimbahAll_",  year, "<-propLimbah * proyeksiBAULimbah$`", year, "`", sep=""))))
      eval(parse(text=(paste("tableEmisi_", year, "<- as.matrix(tableBuanganLimbahAll_", year,")", "%*% input_ef_matrix", sep=""))))
      eval(parse(text=(paste("emisi_", i, "<- as.data.frame(rowSums(tableEmisi_", year,"))", sep=""))))
      eval(parse(text=(paste("totalEmisi_", i, "<- sum(emisi_", i, ")", sep=""))))
      
      totalEmisiLimbahTemp_ <- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("totalEmisiLimbahTemp_[[",i,"]] <- totalEmisi_", i, "", sep="")))
      }
      totalEmisiBAU_All <- as.matrix(totalEmisiLimbahTemp_)
      colnames(totalEmisiBAU_All) <- "Total_Emisi"
      totalEmisiBAU_All <- as.data.frame(cbind(year, totalEmisiBAU_All), row.names = 1:length(year))
      totalEmisiBAU_All$Total_Emisi <- as.numeric(totalEmisiBAU_All$Total_Emisi)
      totalEmisiBAU_All$year <- as.numeric(totalEmisiBAU_All$year)
      
      library(ggplot2)
      library(plotly)
      
    ## Grafik Proyeksi Total Buangan Limbah ##
      
      graphWaste <- colSums(proyeksiBAULimbah[,2:length(proyeksiBAULimbah)])
      graphWaste <- as.data.frame(cbind(year,graphWaste), row.names = 1:length(year))
      plotWaste<- ggplot(data=graphWaste, aes(x=year, y=graphWaste, group=1)) + geom_line() + geom_point()
      ggplotly(plotWaste)
      
    ## Grafik Proyeksi Total Emisi Buangan Limbah ##
      
      plotEmission <- ggplot(data=totalEmisiBAU_All, aes(x=year, y=Total_Emisi, group=1)) + geom_line() + geom_point()
      ggplotly(plotEmission)


### SKENARIO AKSI ####  
  
  # Intensitas Emisi BAU
    # intensistasEmBAU <- totalEmisiBAU_All$Total_Emisi/totalPDRBbau$Total_PDRB
    # intensistasEmBAU <- data.frame(cbind(year,intensistasEmBAU), row.names = 1:length(year))
    # colnames(intensistasEmBAU) <- c("year","Intensitas_Emisi")
    
  ## Aksi 1 (Limbah Padat) : Rencana Pembangunan dan Operasional TPS Terpadu 3R/Kompos-ting ####
  
    ## Final Demand ##
  
      FD_padat <- read.table("_YK/raw/fd_limbah_padat.csv", header = TRUE, sep =";") #USER INPUT
      FDint_All <- proyeksiFD_t[2:length(proyeksiFD_t)] + FD_padat[2:length(FD_padat)]
      intOutput_All <- ioLeontiefInverse %*% as.matrix(FDint_All)
      intPDRB_All <- GDPAll$P_OUTPUT * (intOutput_All)
      int_colsumPDRB <- data.frame(colSums(intPDRB_All))
      colnames(int_colsumPDRB) <- "Total_PDRB"
      int_totalPDRBpadat <- as.data.frame(cbind(year, int_colsumPDRB), row.names = 1:length(year))
    
      #Grafik perbandingan intervensi dan BAU PDRB
      
      gdpBAU <- totalPDRBbau
      gdpInv <- int_totalPDRBpadat
      
      cumSumBAU_gdp <- subset(gdpBAU, select=c(year, Total_PDRB))
      cumSumInv_gdp <- subset(gdpInv, select=c(year, Total_PDRB))
      
      cumSumBAU_gdp$Scenario<-"BAU"
      cumSumInv_gdp$Scenario<-"Aksi Mitigasi Limbah Padat"
      
      tblCumSumScenario_gdp <- rbind(cumSumBAU_gdp, cumSumInv_gdp)
      
      plotAll_gdp<-ggplot(tblCumSumScenario_gdp, aes(x=year, y=Total_PDRB, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "PDRB")+
        ggtitle("Grafik Proyeksi PDRB")
      ggplotly(plotAll_gdp)
    
    ## Tabel Satelit ##
      
      i <- 1:length(year)
      sector <- ioSector$V1
      eval(parse(text=paste("intervensi",year, "<-read.table('_YK/raw/input_limbah/intervensi",year,".csv', header = TRUE, sep = ';')", sep="")))
      eval(parse(text=(paste("tableBuanganLimbah_",  year, "<-propLimbah * proyeksiBAULimbah$`", year, "`", sep=""))))
      eval(parse(text=(paste("tableBuanganLimbahInt_", year, "<- (intervensi", year,"[,3:length(intervensi", year,")]) * tableBuanganLimbah_", year, sep = ""))))
      eval(parse(text=paste("deltaBuanganLimbahTimbun_", year, "<- (-1) * rowSums(tableBuanganLimbahInt_", year, "[,1:2] - tableBuanganLimbah_", year, "[,1:2])", sep="")))
      # eval(parse(text=paste("tableBuanganLimbahInt_", year, "<- replace(tableBuanganLimbahInt_", year,"$Padat_3R, c(1:length(sector)) ,(tableBuanganLimbahInt_",year, "$Padat_3R + deltaBuanganLimbahTimbun_", year,"))", sep="")))
      eval(parse(text=paste("tableBuanganLimbahInt_", year,"$Padat_3R <- (tableBuanganLimbahInt_",year, "$Padat_3R + deltaBuanganLimbahTimbun_", year,")", sep="")))
      eval(parse(text=(paste("emisiLimbah_", year, "<- as.matrix(tableBuanganLimbahInt_", year, ") %*% ef_matrix", sep=""))))
      eval(parse(text=(paste("rowTotalEmisi_", i, "<-rowSums(emisiLimbah_", year, ")", sep=""))))
      eval(parse(text=(paste("totalEmisiLimbah_", i, "<-sum(rowTotalEmisi_", i, ")", sep=""))))
      
      totalEmisiLimbahInt_ <- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("totalEmisiLimbahInt_[[",i,"]] <- totalEmisiLimbah_", i, "", sep="")))
      }
      totalEmisiLimbahInt_All <- as.matrix(totalEmisiLimbahInt_)
      colnames(totalEmisiLimbahInt_All) <- "Total_Emisi"
      totalEmPadatInt_All <- as.data.frame(cbind(year, totalEmisiLimbahInt_All),row.names = 1:length(year))
      totalEmPadatInt_All$year <- as.numeric(totalEmPadatInt_All$year)
      totalEmPadatInt_All$Total_Emisi <- as.numeric(totalEmPadatInt_All$Total_Emisi)
      
      plotIntEmission <- ggplot(data=totalEmPadatInt_All, aes(x=year, y=Total_Emisi, group=1)) + geom_line() + geom_point()
      ggplotly(plotIntEmission)
    
      #Grafik perbandingan intervensi dan BAU Emisi
      
      emissionBAU <- totalEmisiBAU_All
      emissionInv <- totalEmPadatInt_All
      
      cumSumBAU <- subset(emissionBAU, select=c(year, Total_Emisi))
      cumSumInv <- subset(emissionInv, select=c(year, Total_Emisi))
      
      cumSumBAU$Scenario<-"BAU"
      cumSumInv$Scenario<-"Aksi Mitigasi Limbah Padat"
      
      tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
      
      plotAll_emisi<-ggplot(tblCumSumScenario, aes(x=year, y=Total_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Emisi")+
        ggtitle("Grafik Proyeksi Emisi")
      ggplotly(plotAll_emisi)
      
    ## Intensitas Emisi ##
      
       intensistasEmPadat <-totalEmPadatInt_All$Total_Emisi/int_totalPDRBpadat$Total_PDRB
       intensistasEmPadat <-as.data.frame(cbind(year,intensistasEmPadat), row.names = 1:length(year))
       colnames(intensistasEmPadat) <- c("year","Intensitas_Emisi")
       
       #Grafik perbandingan intervensi dan BAU Intesitas Emisi
       
       # iEmisiBAU <- intensistasEmBAU
       iEmisiInv <- intensistasEmPadat
       
       # cumSumBAU_IEm <- subset(iEmisiBAU, select=c(year, Intensitas_Emisi))
       cumSumInv_IEm <- subset(iEmisiInv, select=c(year, Intensitas_Emisi))
       
       # cumSumBAU_IEm$Scenario<-"BAU"
       cumSumInv_IEm$Scenario<-"Aksi Mitigasi Limbah Padat"
       
       # tblCumSumScenario_IEm <- rbind(cumSumBAU_IEm, cumSumInv_IEm)
       
       plotAll_IEm<-ggplot(cumSumInv_IEm, aes(x=year, y=Intensitas_Emisi, group=Scenario)) +
         geom_line(aes(color=Scenario))+
         geom_point(aes(color=Scenario))+
         labs(x = "Tahun", y = "Intensitas Emisi")+
         ggtitle("Grafik Intensitas Emisi")
       ggplotly(plotAll_IEm)
    
  ## Aksi 2 (Limbah Cair) : Pengolahan Air Limbah Secara Terpusat aerobik  ####
      
    ## Final Demand ##
      
      FD_cair <- read.table("_YK/raw/fd_limbah_cair.csv", header = TRUE, sep =";")
      FDintCair_All <- proyeksiFD_t[2:length(proyeksiFD_t)] + FD_cair[2:length(FD_cair)]
      intOutputCair_All <- ioLeontiefInverse %*% as.matrix(FDintCair_All)
      intPDRBCair_All <- GDPAll$P_OUTPUT * (intOutputCair_All)
      int_colsumPDRBCair <- data.frame(colSums(intPDRBCair_All))
      colnames(int_colsumPDRBCair) <- "Total_PDRB"
      int_totalPDRBCair <- as.data.frame(cbind(year, int_colsumPDRBCair), row.names = 1:length(year))
      
      #Grafik perbandingan intervensi dan BAU PDRB
      
      gdpBAU_2 <- totalPDRBbau
      gdpInv_2 <- int_totalPDRBCair
      
      cumSumBAU_gdp2 <- subset(gdpBAU, select=c(year, Total_PDRB))
      cumSumInv_gdp2 <- subset(gdpInv, select=c(year, Total_PDRB))
      
      cumSumBAU_gdp2$Scenario<-"BAU"
      cumSumInv_gdp2$Scenario<-"Aksi Mitigasi Limbah Cair"
      
      tblCumSumScenario_gdp2 <- rbind(cumSumBAU_gdp2, cumSumInv_gdp2)
      
      plotAll_gdp2<-ggplot(tblCumSumScenario_gdp2, aes(x=year, y=Total_PDRB, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "PDRB")+
        ggtitle("Grafik Proyeksi PDRB")
      ggplotly(plotAll_gdp2)
    
    ## Tabel Satelit ##
      
      infaktorEmisi <- read.table("_YK/raw/faktor_emisi_limbah.csv", header = TRUE, sep = ";") #USER INPUT
      F_Type <- as.data.frame(infaktorEmisi$F_Type)
      num_ef <- nrow(infaktorEmisi)
      i <- 1:length(year)
      year_1 <- 2016:2020
      year_2 <- 2021:2030
      colnames(F_Type) <- "F_Type"
      
      eval(parse(text=(paste("intFaktorEmisi_", year_1, "<- replace(infaktorEmisi$Em_F,c(9,10),infaktorEmisi$Em_F[9:10])", sep ="" ))))
      eval(parse(text=(paste("intFaktorEmisi_", year_2, "<- replace(infaktorEmisi$Em_F,c(9,10),((1+((", year_2, "-2020) * -0.03)) * infaktorEmisi$Em_F[9:10]))", sep ="" ))))
      eval(parse(text=(paste("efInt_", year, "<-cbind(F_Type,intFaktorEmisi_", year,")", sep="" ))))
      eval(parse(text=(paste("diagEFInt_", year, "<-diag(efInt_", year,"$intFaktorEmisi_", year, ", ncol = num_ef, nrow = num_ef)", sep=""))))
      eval(parse(text=(paste("tableBuanganLimbah_",  year, "<-propLimbah * proyeksiBAULimbah$`", year, "`", sep=""))))
      eval(parse(text=paste("tableIntEmisi_", year, "<- as.matrix(tableBuanganLimbah_", year, ") %*% diagEFInt_", year,sep= "" )))
      eval(parse(text=paste("rowSumIntEmisi_", i, "<- rowSums(tableIntEmisi_", year, ")", sep="")))
      eval(parse(text=paste("intTotalEmisi_", i, "<- sum(rowSumIntEmisi_", i, ")", sep="")))
      
      totalEmisiLimbahInt2_ <- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("totalEmisiLimbahInt2_[[",i,"]] <- intTotalEmisi_", i, "", sep="")))
      }
      totalEmisiLimbahInt2_All <- as.matrix(totalEmisiLimbahInt2_)
      colnames(totalEmisiLimbahInt2_All) <- "Total_Emisi"
      totalEmCairInt_All <- as.data.frame(cbind(year, totalEmisiLimbahInt2_All))
      totalEmCairInt_All$year <- as.numeric(totalEmCairInt_All$year)
      totalEmCairInt_All$Total_Emisi <- as.numeric(totalEmCairInt_All$Total_Emisi)
      
      plotIntEmission_2 <- ggplot(data=totalEmCairInt_All, aes(x=year, y=Total_Emisi, group=1)) + geom_line() + geom_point()
      ggplotly(plotIntEmission_2)
      
      #Grafik perbandingan intervensi dan BAU Emisi
      
      emissionBAU_2 <- totalEmisiBAU_All
      emissionInv_2 <- totalEmCairInt_All
      
      cumSumBAU_2 <- subset(emissionBAU_2, select=c(year, Total_Emisi))
      cumSumInv_2 <- subset(emissionInv_2, select=c(year, Total_Emisi))
      
      cumSumBAU_2$Scenario<-"BAU"
      cumSumInv_2$Scenario<-"Aksi Mitigasi Limbah Cair"
      
      tblCumSumScenario_2 <- rbind(cumSumBAU_2, cumSumInv_2)
      
      plotAll_2<-ggplot(tblCumSumScenario_2, aes(x=year, y=Total_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Emisi")+
        ggtitle("Grafik Proyeksi Emisi")
      ggplotly(plotAll_2)
    
    ## Intensitas Emisi ##
      
      intensistasEmCair <-as.data.frame(totalEmCairInt_All$Total_Emisi/int_totalPDRBCair$Total_PDRB)
      intensistasEmCair <-as.data.frame(cbind(year,intensistasEmCair), row.names = 1:length(year))
      colnames(intensistasEmCair) <- c("year","Intensitas_Emisi")
      
      #Grafik perbandingan intervensi dan BAU Intesitas Emisi
      
      # iEmisiBAU <- intensistasEmBAU
      iEmisiInv_2 <- intensistasEmCair
      
      # cumSumBAU_IEm <- subset(iEmisiBAU, select=c(year, Intensitas_Emisi))
      cumSumInv_IEm2 <- subset(iEmisiInv, select=c(year, Intensitas_Emisi))
      
      # cumSumBAU_IEm$Scenario<-"BAU"
      cumSumInv_IEm2$Scenario<-"Aksi Mitigasi Limbah Cair"
      
      # tblCumSumScenario_IEm <- rbind(cumSumBAU_IEm, cumSumInv_IEm)
      
      plotAll_IEm2<-ggplot(cumSumInv_IEm2, aes(x=year, y=Intensitas_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Intensitas Emisi")+
        ggtitle("Grafik Intensitas Emisi")
      ggplotly(plotAll_IEm2)

      
### OUTPUT GRAFIK ####
    ## Grafik PDRB ##
      
      pdrbBAU <- totalPDRBbau
      pdrbAksi1 <- int_totalPDRBpadat
      pdrbAksi2 <- int_totalPDRBCair
      
      subset_pdrbBAU <- subset(pdrbBAU, select=c(year, Total_PDRB))
      subset_pdrbAksi1 <- subset(pdrbAksi1, select=c(year, Total_PDRB))
      subset_pdrbAksi2 <- subset(pdrbAksi2, select=c(year, Total_PDRB))
      
      subset_pdrbBAU$Scenario<-"BAU"
      subset_pdrbAksi1$Scenario<-"Aksi Mitigasi Limbah Padat"
      subset_pdrbAksi2$Scenario<-"Aksi Mitigasi Limbah Cair"
      
      tblPDRB_All <- rbind(subset_pdrbBAU, subset_pdrbAksi1, subset_pdrbAksi2)
      
      plotPDRB_All<-ggplot(tblPDRB_All, aes(x=year, y=Total_PDRB, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Emisi")+
        ggtitle("Grafik Proyeksi PDRB")
      ggplotly(plotPDRB_All)
      
    ## Grafik Emisi ##
      
      emBAU <- totalEmisiBAU_All
      emAksi1 <- totalEmPadatInt_All
      emAksi2 <- totalEmCairInt_All
      
      subset_emBAU <- subset(emBAU, select=c(year, Total_Emisi))
      subset_emAksi1 <- subset(emAksi1, select=c(year, Total_Emisi))
      subset_emAksi2 <- subset(emAksi2, select=c(year, Total_Emisi))
      
      subset_emBAU$Scenario<-"BAU"
      subset_emAksi1$Scenario<-"Aksi Mitigasi Limbah Padat"
      subset_emAksi2$Scenario<-"Aksi Mitigasi Limbah Cair"
      
      tblEmisi_All <- rbind(subset_emBAU, subset_emAksi1, subset_emAksi2)
      
      plotEmisi_All<-ggplot(tblEmisi_All, aes(x=year, y=Total_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Emisi")+
        ggtitle("Grafik Proyeksi Emisi")
      ggplotly(plotEmisi_All)
      
    ## Grafik Intensitas Emisi ##
      
      iEmAksi1 <- intensistasEmPadat
      iEmAksi2 <- intensistasEmCair
      
      subset_iEmAksi1 <- subset(iEmAksi1, select=c(year, Intensitas_Emisi))
      subset_iEmAksi2 <- subset(iEmAksi2, select=c(year, Intensitas_Emisi))
      
      subset_iEmAksi1$Scenario<-"Aksi Mitigasi Limbah Padat"
      subset_iEmAksi2$Scenario<-"Aksi Mitigasi Limbah Cair"
      
      tblIntensitasEm_All <- rbind(subset_iEmAksi1, subset_iEmAksi2)
      
      plotIntensitasEm_All<-ggplot(tblIntensitasEm_All, aes(x=year, y=Intensitas_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Emisi")+
        ggtitle("Grafik Intensitas Emisi")
      ggplotly(plotIntensitasEm_All)
      
    ## Bar Chart di Tahun 2030 ##
      
      tblPDRB_t <- filter(tblPDRB_All, year=="2030")
      barPDRB <- ggplot(tblPDRB_t, (aes(x=Scenario, y=Total_PDRB))) + geom_bar(stat="identity")
      ggplotly(barPDRB)
      
      tblEmisi_t <- filter(tblEmisi_All, year=="2030")
      barEmisi <- ggplot(tblEmisi_t, (aes(x=Scenario, y=Total_Emisi))) + geom_bar(stat="identity")
      ggplotly(barEmisi)
      

      tblIntensitasEm_t <- filter(tblIntensitasEm_All, year=="2030")
      barInEmisi <- ggplot(tblIntensitasEm_t, (aes(x=Scenario, y=Intensitas_Emisi))) + geom_bar(stat="identity")
      ggplotly(barInEmisi)
      
      
### OUTPUT DATA FRAME ####
  ## scenario-specific dataframe of sector><simulation years depicting GDP, emission and emission intensity    
    #Aksi 1
      #PDRB
      sector <- ioSector$V1
      colnames(intPDRB_All) <- year
      PDRB_padat <- as.data.frame(cbind(sector,intPDRB_All))
      
      #Emisi
      rowTotalEmisi1_<- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("rowTotalEmisi1_[[",i,"]] <- rowTotalEmisi_", i, "", sep="")))
      }
      rowTotalEmisi <- as.data.frame(rowTotalEmisi1_)
      colnames(rowTotalEmisi) <- year      
      emisi_padat <- cbind(sector, rowTotalEmisi)
      
      #Intensitas Emisi
      inEm_padatTemp <- rowTotalEmisi/intPDRB_All
      inEm_padat <- cbind(sector, inEm_padatTemp)
    
    #Aksi 2
      #PDRB
      sector <- ioSector$V1
      colnames(intPDRBCair_All) <- year
      PDRB_cair <- as.data.frame(cbind(sector,intPDRBCair_All))
      
      #Emisi
      rowTotalEmisi2_<- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("rowTotalEmisi2_[[",i,"]] <- rowSumIntEmisi_", i, "", sep="")))
      }
      rowSumIntEmisi <- as.data.frame(rowTotalEmisi2_)
      colnames(rowSumIntEmisi) <- year      
      emisi_cair <- cbind(sector, rowSumIntEmisi)
      
      #Intensitas Emisi
      inEm_cairTemp <- rowSumIntEmisi/intPDRBCair_All
      inEm_cair <- cbind(sector, inEm_cairTemp)  
  
  ## scenario-specific dataframe of sector><simulation years depicting GDP change against BAU, emission change against BAU and emission intensity change against BAU 
    #Aksi 1
      deltaPDRB <- as.data.frame(cbind(sector,(intPDRB_All - proyeksiPDRB)))
      
      emisiBAU_<- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("emisiBAU_[[",i,"]] <- emisi_", i, "", sep="")))
      }
      emisiBAU <- as.data.frame(emisiBAU_)
      deltaEmisi <- as.data.frame(cbind(sector, (rowTotalEmisi  - emisiBAU)))
      
      intensitasEmBAU <- emisiBAU/proyeksiPDRB
      deltaInEm <- as.data.frame(cbind(sector, (inEm_padatTemp - intensitasEmBAU)))
    
      
    #Aksi 2
      deltaPDRB_cair <- as.data.frame(cbind(sector,(intPDRBCair_All - proyeksiPDRB)))
      deltaEmisi_cair <- as.data.frame(cbind(sector, (rowSumIntEmisi  - emisiBAU)))
      intensitasEmBAU <- emisiBAU/proyeksiPDRB
      deltaInEm_cair <- as.data.frame(cbind(sector, (inEm_cairTemp - intensitasEmBAU)))
      
  ## scenario-specific dataframe at t15 depicting cumulative emission reduction, GDP change and emission intensity change
    #Aksi 1
      cumPDRB <- cumsum(int_totalPDRBpadat$Total_PDRB)
      cumEmisi <- cumsum(totalEmPadatInt_All$Total_Emisi)
      cumInEmisi <- cumsum(intensistasEmPadat$Intensitas_Emisi)
      cumTable_padat <- as.data.frame(cbind(year, cumPDRB, cumEmisi, cumInEmisi))
      colnames(cumTable_padat) <- c("year", "Total_PDRB", "Total_Emisi", "Intensitas_Emisi")
      #cumTable_padat_2030 <- filter(cumTable_padat, year=="2030") 
      
    #Aksi 2
      cumPDRB_cair <- cumsum(int_totalPDRBCair$Total_PDRB)
      cumEmisi_cair <- cumsum(totalEmCairInt_All$Total_Emisi)
      cumInEmisi_cair <- cumsum(intensistasEmCair$Intensitas_Emisi)
      cumTable_cair <- as.data.frame(cbind(year, cumPDRB_cair, cumEmisi_cair, cumInEmisi_cair))
      colnames(cumTable_cair) <- c("year", "Total_PDRB", "Total_Emisi", "Intensitas_Emisi")
      #cumTable_cair_2030 <- filter(cumTable_cair, year=="2030")
      
      
### KOMBINASI AKSI ####
  ## scenario-specific dataframe of sector><simulation years depicting GDP, emission and emission intensity        
    #PDRB
      inCombineFD <- read.table("_YK/raw/fd_aksi_padat_cair.csv", header = TRUE, sep =";")
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
                              Koef_Limbah = wasteCoef,
                              stringsAsFactors = FALSE)
      tableCombineProyeksi <- function(tableCombineOutput,tableCombineKoef){
        for (i in 1:ncol(tableCombineOutput)){
          tableCombineOutput[,i] <- tableCombineKoef*(tableCombineOutput[,i])
        }    
        return(tableCombineOutput)
      }
      tableBuanganLimbah_combine <- tableProyeksi(tableCombineOutput,tableCombineKoef)
      year <- 2016:2030
      colnames(tableBuanganLimbah_combine) <- year
      tableBuanganLimbah_combine <- cbind(sector, tableBuanganLimbah_combine)
      
      i <- 1:length(year)
      eval(parse(text=(paste("tableBuanganLimbah_combine",  year, "<-propLimbah * tableBuanganLimbah_combine$`", year, "`", sep=""))))
      eval(parse(text=(paste("tableEmisiCombine_", year, "<- as.matrix(tableBuanganLimbah_combine", year,")", "%*% input_ef_matrix", sep=""))))
      eval(parse(text=(paste("emisiCombine_", i, "<- as.data.frame(rowSums(tableEmisiCombine_", year,"))", sep=""))))
      eval(parse(text=paste("TotalEmisiCombine_", i, "<- sum(emisiCombine_", i, ")", sep="")))
      
      combineEmisi_<- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("combineEmisi_[[",i,"]] <- emisiCombine_", i, "", sep="")))
      }
      combineEmisi <- as.data.frame(combineEmisi_)
      colnames(combineEmisi) <- year
      
    #Intensitas Emisi
      combineIEm <- combineEmisi/combinePDRB[,2:length(combinePDRB)]
      combineIEm <- cbind(sector, combineIEm)
      
  ## scenario-specific dataframe of sector><simulation years depicting GDP change against BAU, emission change against BAU and emission intensity change against BAU 
    #PDRB
      deltaPDRB_combine <- as.data.frame(cbind(sector,(combinePDRB - proyeksiPDRB)))
      
    #Emisi
      deltaEmisi_combine <- as.data.frame(cbind(sector, (combineEmisi  - emisiBAU)))
      
    #Intensitas Emisi
      intensitasEmBAU <- emisiBAU/proyeksiPDRB
      deltaInEm_combine <- as.data.frame(cbind(sector, (combineIEm - intensitasEmBAU)))
      
  ## scenario-specific dataframe at t15 depicting cumulative emission reduction, GDP change and emission intensity change 
    #PDRB
      cumPDRB_combine <- cumsum(colsum_combinePDRB$Total_PDRB)
  
    #Emisi
      totalEmisiCombine_<- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("totalEmisiCombine_[[",i,"]] <- TotalEmisiCombine_", i, "", sep="")))
      }
      totalEmisiCombine <- as.matrix(totalEmisiCombine_)
      colnames(totalEmisiCombine) <- "Total_Emisi"
      totalEmisiCombine <- as.data.frame(cbind(year, totalEmisiCombine))
      cumEmisi_combine <- cumsum(totalEmisiCombine$Total_Emisi)
      
    #Intensitas Emisi
      inEm_Combine <- cumEmisi_combine/cumPDRB_combine
      cumInEmisi_combine <- cumsum(inEm_Combine)
      
      cumTable_combine<- as.data.frame(cbind(year, cumPDRB_combine, cumEmisi_combine, cumInEmisi_combine))
      colnames(cumTable_combine) <- c("year", "Total_PDRB", "Total_Emisi", "Intensitas_Emisi")
      