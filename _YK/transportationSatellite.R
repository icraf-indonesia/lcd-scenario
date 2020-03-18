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
      #Buat tampilan di SHINY line 151 - 172
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
      
      i <- 1:length(year)
      eval(parse(text=(paste("tableKonsumsiEnergiAll_",  year, "<-propEnergi * proyeksiBAUEnergi$`", year, "`", sep=""))))
      eval(parse(text=(paste("tableEmisiEnergi_", year, "<- as.matrix(tableKonsumsiEnergiAll_", year,")", "%*% input_ef_matrix", sep=""))))
      eval(parse(text=(paste("emisiEnergi_", i, "<- as.data.frame(rowSums(tableEmisiEnergi_", year,"))", sep=""))))
      eval(parse(text=(paste("totalEmisiEnergiBAU_", i , "<- sum(emisiEnergi_", i, ")", sep=""))))
      
      totalEmisiEnergitemp_ <- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("totalEmisiEnergitemp_[[",i,"]] <- totalEmisiEnergiBAU_", i, "", sep="")))
      }
      totalEmisiEnergi_All <- as.matrix(totalEmisiEnergitemp_)
      colnames(totalEmisiEnergi_All) <- "Total_Emisi"
      totalEmisiEnergiBAU_All <- as.data.frame(cbind(year, totalEmisiEnergi_All))
      totalEmisiEnergiBAU_All$year <- as.numeric(totalEmisiEnergiBAU_All$year)
      totalEmisiEnergiBAU_All$Total_Emisi <- as.numeric(totalEmisiEnergiBAU_All$Total_Emisi)
      
      
      library(ggplot2)
      library(plotly)
      
    ## Grafik Proyeksi Total Konsumsi Energi ##
      
      graphEnergy <- colSums(proyeksiBAUEnergi[,2:length(proyeksiBAUEnergi)])
      graphEnergy <- as.data.frame(cbind(year,graphEnergy), row.names = 1:length(year))
      plotEnergy <- ggplot(data=graphEnergy, aes(x=year, y=graphEnergy, group=1)) + geom_line() + geom_point()
      ggplotly(plotEnergy)
      
    ## Grafik Proyeksi Total Emisi Energi ##
      
      plotEmissionEnergy <- ggplot(data=totalEmisiEnergiBAU_All, aes(x=year, y=Total_Emisi, group=1)) + geom_line() + geom_point()
      ggplotly(plotEmissionEnergy)


### SKENARIO AKSI ####
  ### Aksi 1 : Reformasi sistem transit - BRT System ####
    ## Final Demand ##
      
      findem_BRT <- read.table("_YK/raw/input_transportasi/fd_BRT.csv", header = TRUE, sep =";") #USER INPUT
      findemBRTInt_All <- proyeksiFD_t[2:length(proyeksiFD_t)] + findem_BRT[2:length(findem_BRT)]
      intBRTOutput_All <- ioLeontiefInverse %*% as.matrix(findemBRTInt_All)
      intPDRBBRT_All <- GDPAll$P_OUTPUT * (intBRTOutput_All)
      intColsumPDRB_BRT <- data.frame(colSums(intPDRBBRT_All))
      colnames(intColsumPDRB_BRT) <- "Total_PDRB"
      intTotalPDRB_BRT <- as.data.frame(cbind(year, intColsumPDRB_BRT), row.names = 1:length(year))
      
      #Grafik perbandingan intervensi dan BAU PDRB
      
      gdpBAU <- totalPDRBbau
      gdpInv <- intTotalPDRB_BRT
      
      cumSumBAU_gdp <- subset(gdpBAU, select=c(year, Total_PDRB))
      cumSumInv_gdp <- subset(gdpInv, select=c(year, Total_PDRB))
      
      cumSumBAU_gdp$Scenario<-"BAU"
      cumSumInv_gdp$Scenario<-"Aksi Mitigasi BRT"
      
      tblCumSumScenario_gdp <- rbind(cumSumBAU_gdp, cumSumInv_gdp)
      
      plotAll_gdp<-ggplot(tblCumSumScenario_gdp, aes(x=year, y=Total_PDRB, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "PDRB")+
        ggtitle("Grafik Proyeksi PDRB")
      ggplotly(plotAll_gdp)
      
    ## Tabel Satelit ##
      
      sector <- ioSector$V1
      i <- 1:length(year)
      eval(parse(text=paste("intervensiEnergi",year, "<-read.table('_YK/raw/input_transportasi/intervensiEnergi",year,".csv', header = TRUE, sep = ';')", sep="")))
      eval(parse(text=(paste("tableKonsumsiEnergi_",  year, "<-propEnergi * proyeksiBAUEnergi$`", year, "`", sep=""))))
      eval(parse(text=(paste("tableKonsumsiEnergiInt_", year, "<- (intervensiEnergi", year,"[,2:length(intervensiEnergi", year,")]) * tableKonsumsiEnergi_", year, sep = ""))))
      eval(parse(text=paste("deltaKonsumsiEnergi_", year, "<- (-1) * (tableKonsumsiEnergiInt_", year, "$GASOLINE - tableKonsumsiEnergi_", year, "$GASOLINE)", sep="")))
      # eval(parse(text=paste("tableBuanganLimbahInt_", year, "<- replace(tableBuanganLimbahInt_", year,"$Padat_3R, c(1:length(sector)) ,(tableBuanganLimbahInt_",year, "$Padat_3R + deltaBuanganLimbahTimbun_", year,"))", sep="")))
      eval(parse(text=paste("tableKonsumsiEnergiInt_", year,"$BIOGASOL <- (tableKonsumsiEnergiInt_",year, "$BIOGASOL + deltaKonsumsiEnergi_", year,")", sep="")))
      eval(parse(text=(paste("emisiEnergi_", year, "<- as.matrix(tableKonsumsiEnergiInt_", year, ") %*% ef_matrix", sep=""))))
      eval(parse(text=(paste("rowTotalEmisi_", i, "<-rowSums(emisiEnergi_", year, ")", sep=""))))
      eval(parse(text=(paste("totalEmisiEnergiInt_", i, "<-sum(rowTotalEmisi_", i, ")", sep=""))))
      
      totalEmEnergiIntTemp_ <- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("totalEmEnergiIntTemp_[[",i,"]] <- totalEmisiEnergiInt_", i, "", sep="")))
      }
      totalEmisiEnergiInt_All <- as.matrix(totalEmEnergiIntTemp_)
      colnames(totalEmisiEnergiInt_All) <- "Total_Emisi"
      totalEmisiBRT <- as.data.frame(cbind(year, totalEmisiEnergiInt_All))
      totalEmisiBRT$year <- as.numeric(totalEmisiBRT$year)
      totalEmisiBRT$Total_Emisi <- as.numeric(totalEmisiBRT$Total_Emisi)
      
      #Grafik perbandingan intervensi dan BAU Emisi
      
      emissionBAU <- totalEmisiEnergiBAU_All
      emissionInv <- totalEmisiBRT
      
      cumSumBAU <- subset(emissionBAU, select=c(year, Total_Emisi))
      cumSumInv <- subset(emissionInv, select=c(year, Total_Emisi))
      
      cumSumBAU$Scenario<-"BAU"
      cumSumInv$Scenario<-"Aksi Mitigasi BRT"
      
      tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
      
      plotAll_emisi<-ggplot(tblCumSumScenario, aes(x=year, y=Total_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Emisi")+
        ggtitle("Grafik Proyeksi Emisi")
      ggplotly(plotAll_emisi)

    ## Intensitas Emisi ##
      
      intensistasEmBRT <-totalEmisiBRT$Total_Emisi/intTotalPDRB_BRT$Total_PDRB
      intensistasEmBRT <-as.data.frame(cbind(year,intensistasEmBRT), row.names = 1:length(year))
      colnames(intensistasEmBRT) <- c("year","Intensitas_Emisi")
      
      #Grafik perbandingan intervensi dan BAU Intesitas Emisi
      
      # iEmisiBAU <- intensistasEmBAU
      iEmisiInv <- intensistasEmBRT
      
      # cumSumBAU_IEm <- subset(iEmisiBAU, select=c(year, Intensitas_Emisi))
      cumSumInv_IEm <- subset(iEmisiInv, select=c(year, Intensitas_Emisi))
      
      # cumSumBAU_IEm$Scenario<-"BAU"
      cumSumInv_IEm$Scenario<-"Aksi Mitigasi BRT"
      
      # tblCumSumScenario_IEm <- rbind(cumSumBAU_IEm, cumSumInv_IEm)
      
      plotAll_IEm<-ggplot(cumSumInv_IEm, aes(x=year, y=Intensitas_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Intensitas Emisi")+
        ggtitle("Grafik Intensitas Emisi")
      ggplotly(plotAll_IEm)

  ### Aksi 2 : Car Free Day ####
    ## Final Demand ##
      
      findem_CFD <- read.table("_YK/raw/input_transportasi/fd_CFD.csv", header = TRUE, sep =";")
      findemCFDInt_All <- proyeksiFD_t[2:length(proyeksiFD_t)] + findem_CFD[2:length(findem_CFD)]
      intOutputCFD_All <- ioLeontiefInverse %*% as.matrix(findemCFDInt_All)
      intPDRBCFD_All <- GDPAll$P_OUTPUT * (intOutputCFD_All)
      intColsumPDRB_CFD <- data.frame(colSums(intPDRBCFD_All))
      colnames(intColsumPDRB_CFD) <- "Total_PDRB"
      intTotalPDRB_CFD <- as.data.frame(cbind(year, intColsumPDRB_CFD), row.names = 1:length(year))
      
      #Grafik perbandingan intervensi dan BAU PDRB
      
      gdpBAU_2 <- totalPDRBbau
      gdpInv_2 <- intTotalPDRB_CFD
      
      cumSumBAU_gdp2 <- subset(gdpBAU, select=c(year, Total_PDRB))
      cumSumInv_gdp2 <- subset(gdpInv, select=c(year, Total_PDRB))
      
      cumSumBAU_gdp2$Scenario<-"BAU"
      cumSumInv_gdp2$Scenario<-"Aksi Mitigasi CFD"
      
      tblCumSumScenario_gdp2 <- rbind(cumSumBAU_gdp2, cumSumInv_gdp2)
      
      plotAll_gdp2<-ggplot(tblCumSumScenario_gdp2, aes(x=year, y=Total_PDRB, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "PDRB")+
        ggtitle("Grafik Proyeksi PDRB")
      ggplotly(plotAll_gdp2)
      
    ## Tabel Satelit ##
      
      infaktorEmisi <- emissionFactorEnergy
      F_Type <- as.data.frame(emissionFactorEnergy$F_Type)
      num_ef <- nrow(emissionFactorEnergy)
      year_1 <- 2016:2017
      year_2 <- seq(2019,2030, by=2)
      year_3 <- seq(2018,2030, by=2)
      multiplier <- 3:8
      multiplier2 <- 2:8
      i <- 1:length(year)
      colnames(F_Type) <- "F_Type"

      eval(parse(text=(paste("intFaktorEmisi_", year_1, "<- replace(infaktorEmisi$Em_F,c(6),((1+((", year_1, "-2015) * (-0.3/8))) * infaktorEmisi$Em_F[6]))", sep ="" ))))
      eval(parse(text=(paste("intFaktorEmisi_", year_2, "<- replace(infaktorEmisi$Em_F,c(6),((1+(", multiplier, " * (-0.3/8))) * infaktorEmisi$Em_F[6]))", sep ="" ))))
      eval(parse(text=(paste("intFaktorEmisi_", year_3, "<- replace(infaktorEmisi$Em_F,c(6),((1+(", multiplier2, " * (-0.3/8))) * infaktorEmisi$Em_F[6]))", sep ="" ))))
      eval(parse(text=(paste("efInt_", year, "<-cbind(F_Type,intFaktorEmisi_", year,")", sep="" ))))
      eval(parse(text=(paste("diagEFInt_", year, "<-diag(efInt_", year,"$intFaktorEmisi_", year, ", ncol = num_ef, nrow = num_ef)", sep=""))))
      eval(parse(text=(paste("tableKonsumsiEnergi_",  year, "<-propEnergi * proyeksiBAUEnergi$`", year, "`", sep=""))))
      eval(parse(text=paste("tableIntEmisiEnergi_", year, "<- as.matrix(tableKonsumsiEnergi_", year, ") %*% diagEFInt_", year,sep= "" )))
      eval(parse(text=paste("rowSumIntEmisiEnergi_", i, "<- rowSums(tableIntEmisiEnergi_", year, ")", sep="")))
      eval(parse(text=paste("totalEmisiEnergiInt2_", i, "<- sum(rowSumIntEmisiEnergi_", i, ")", sep="")))
      
      totalEmEnergiIntTemp2_ <- list()
      for (i in 1:length(year)){
        eval(parse(text=paste("totalEmEnergiIntTemp2_[[",i,"]] <- totalEmisiEnergiInt2_", i, "", sep="")))
      }
      totalEmisiEnergiInt2_All <- as.matrix(totalEmEnergiIntTemp2_)
      colnames(totalEmisiEnergiInt2_All) <- "Total_Emisi"
      totalEmisiCFD <- as.data.frame(cbind(year, totalEmisiEnergiInt2_All))
      totalEmisiCFD$year <- as.numeric(totalEmisiCFD$year)
      totalEmisiCFD$Total_Emisi <- as.numeric(totalEmisiCFD$Total_Emisi)
      
      #Grafik perbandingan intervensi dan BAU Emisi
      
      emissionBAU_2 <- totalEmisiEnergiBAU_All
      emissionInv_2 <- totalEmisiCFD
      
      cumSumBAU_2 <- subset(emissionBAU_2, select=c(year, Total_Emisi))
      cumSumInv_2 <- subset(emissionInv_2, select=c(year, Total_Emisi))
      
      cumSumBAU_2$Scenario<-"BAU"
      cumSumInv_2$Scenario<-"Aksi Mitigasi CFD"
      
      tblCumSumScenario_2 <- rbind(cumSumBAU_2, cumSumInv_2)
      
      plotAll_2<-ggplot(tblCumSumScenario_2, aes(x=year, y=Total_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Emisi")+
        ggtitle("Grafik Proyeksi Emisi")
      ggplotly(plotAll_2)

    ## Intensitas Emisi ##
      
      intensistasEmCFD <-as.data.frame(totalEmisiCFD$Total_Emisi/intTotalPDRB_CFD$Total_PDRB)
      intensistasEmCFD <-as.data.frame(cbind(year,intensistasEmCFD), row.names = 1:length(year))
      colnames(intensistasEmCFD) <- c("year","Intensitas_Emisi")
      
      #Grafik perbandingan intervensi dan BAU Intesitas Emisi
      
      # iEmisiBAU <- intensistasEmBAU
      iEmisiInv_2 <- intensistasEmCFD
      
      # cumSumBAU_IEm <- subset(iEmisiBAU, select=c(year, Intensitas_Emisi))
      cumSumInv_IEm2 <- subset(iEmisiInv, select=c(year, Intensitas_Emisi))
      
      # cumSumBAU_IEm$Scenario<-"BAU"
      cumSumInv_IEm2$Scenario<-"Aksi Mitigasi CFD"
      
      # tblCumSumScenario_IEm <- rbind(cumSumBAU_IEm, cumSumInv_IEm)
      
      plotAll_IEm2<-ggplot(cumSumInv_IEm2, aes(x=year, y=Intensitas_Emisi, group=Scenario)) +
        geom_line(aes(color=Scenario))+
        geom_point(aes(color=Scenario))+
        labs(x = "Tahun", y = "Intensitas Emisi")+
        ggtitle("Grafik Intensitas Emisi")
      ggplotly(plotAll_IEm2)
      
      
### OUTPUT DATAF RAME ####
  ## scenario-specific dataframe of sector><simulation years depicting GDP, emission and emission intensity    
      #Aksi 1
        #PDRB
        sector <- ioSector$V1
        colnames(intPDRBBRT_All) <- year
        PDRB_BRT <- as.data.frame(cbind(sector,intPDRBBRT_All))
        
        #Emisi
        rowTotalEmisiBRT_<- list()
        for (i in 1:length(year)){
          eval(parse(text=paste("rowTotalEmisiBRT_[[",i,"]] <- rowTotalEmisi_", i, "", sep="")))
        }
        rowTotalEmisiBRT <- as.data.frame(rowTotalEmisiBRT_)
        colnames(rowTotalEmisiBRT) <- year
        emisi_BRT <- as.data.frame(cbind(sector, rowTotalEmisiBRT))
        
        #Intensitas Emisi
        inEmBRT_temp <- rowTotalEmisiBRT/intBRTPDRB_All
        inEm_BRT <- cbind(sector, inEmBRT_temp)
      
      #Aksi 2
        #PDRB
        sector <- ioSector$V1
        colnames(intPDRBCFD_All) <- year
        PDRB_CFD <- as.data.frame(cbind(sector,intPDRBCFD_All))
                                  
        #Emisi
        
        rowTotalEmisiCFD_<- list()
        for (i in 1:length(year)){
          eval(parse(text=paste("rowTotalEmisiCFD_[[",i,"]] <- rowSumIntEmisiEnergi_", i, "", sep="")))
        }
        rowTotalEmisiCFD <- as.data.frame(rowTotalEmisiCFD_)
        colnames(rowTotalEmisiCFD) <- year
        emisi_CFD <- as.data.frame(cbind(sector, rowTotalEmisiCFD))
  
        #Intensitas Emisi
        inEmCFD_temp <- rowSumEmisiCFD/intPDRBCFD_All
        inEm_CFD <- cbind(sector, inEmCFD_temp)  
      
  ## scenario-specific dataframe of sector><simulation years depicting GDP change against BAU, emission change against BAU and emission intensity change against BAU 
      #Aksi 1
        deltaPDRB <- as.data.frame(cbind(sector,(intPDRBBRT_All - proyeksiPDRB)))
        
        emisiBAU_<- list()
        for (i in 1:length(year)){
          eval(parse(text=paste("emisiBAU_[[",i,"]] <- emisiEnergi_", i, "", sep="")))
        }
        emisiBAU <- as.data.frame(emisiBAU_)
        colnames(emisiBAU) <- year
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
      
      i <- 1:length(year)
      eval(parse(text=(paste("tableKonsumsiEnergi_combine",  year, "<-propEnergi * tableKonsumsiEnergi_combine$`", year, "`[2:length(tableKonsumsiEnergi_combine)]", sep=""))))
      eval(parse(text=(paste("tableEmisiCombine_", year, "<- as.matrix(tableKonsumsiEnergi_combine", year,")", "%*% input_ef_matrix", sep=""))))
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
      