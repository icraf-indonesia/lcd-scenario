library(limSolve)
library(quadprog)
library(stringr)
library(dplyr)
library(plotly)
library(reshape2)




###BEGIN: initiate all variables ####

# username <- "alfanugraha"
# password <- "1234"
selectedProv = "SumSel"
# scenNumber = 1
datapath <- paste0("_TIN/data/", selectedProv, "/")
# resultpath<-paste0("_TIN/result/", selectedProv, "/landScen", scenNumber,"_")

LUTMDatabase<-as.data.frame(read.csv("_TIN/data/LUTMDatabaseID.csv"))

periodIO<-readRDS(paste0(datapath, "periodIO"))
tOutputSeries<-read.csv(paste0(datapath,"tOutputSeries.csv"))
# landCover_his<-read.csv(paste0(datapath,"landCover_his.csv"), header=FALSE)
LDMProp_his<-read.csv(paste0(datapath, "LDMProp.csv")) # if CSV
# LDMProp_his<-readRDS(paste0(datapath,"LDMProp"))   # if RDS
GDPAll<-readRDS(paste0(datapath,"GDPAll"))
sector<-readRDS(paste0(datapath,"sector"))
LUTM_template<-read.csv(paste0(datapath,"LUTM_template.csv"))
# LUTM_his<-read.csv(paste0(datapath,"LUTM_his.csv"))
LRCRate_his<-read.csv(paste0(datapath,"LRCRate.csv"),header = FALSE)
LRCRate_2<-read.csv(paste0(datapath,"LRCRate_2.csv"),header=FALSE)
carbonStock_his<-data.matrix(read.csv(paste0(datapath,"carbonStock.csv")))
carbonStock_his<-as.matrix(carbonStock_his[,3])
leontief <- readRDS(paste0(datapath,"leontief"))
findem_series<-read.csv(paste0(datapath,"findem_series.csv"),header=FALSE)
indem<-readRDS(paste0(datapath, "indem"))
findem<-readRDS(paste0(datapath,"findem"))

################################# (i) PREPARE DATA  #############################################################

##### buat LUTM_his dan landCover_his#####


LUTMDatabase<-LUTMDatabase[LUTMDatabase$Provinsi==paste0(selectedProv),c("Count",paste0("PL", periodIO-1, "RCL"), paste0("PL",periodIO,"RCL"))]
# LUTMDatabase<-LUTMDatabase[LUTMDatabase$Provinsi==paste0(selectedProv),c("Count",paste0("PL", 2010, "RCL"), paste0("PL",2011,"RCL"))]

colnames(LUTMDatabase)<-c("COUNT","ID_LC1","ID_LC2")
tuplaID<- cbind(as.matrix(cbind(matrix(0, nrow=23^2, ncol=1), as.matrix(expand.grid(1:23, 1:23)))))
colnames(tuplaID)<-c("COUNT","ID_LC1","ID_LC2")
LUTMDatabase<-rbind(LUTMDatabase,tuplaID)


LUTMDatabase<-aggregate(LUTMDatabase, by=list(LUTMDatabase$ID_LC1,LUTMDatabase$ID_LC2), FUN=sum)
LUTMDatabase<-LUTMDatabase[,1:3]
colnames(LUTMDatabase)<-c("ID_LC1","ID_LC2","COUNT")
LUTMDatabase<-LUTMDatabase[LUTMDatabase$ID_LC1>0,]
LUTMDatabase<-LUTMDatabase[LUTMDatabase$ID_LC2>0,]

sum(LUTMDatabase$COUNT) #cek

LUTMDatabase <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))

areaLandCover1 <- dcast(data = LUTMDatabase, formula = ID_LC1 ~ ., fun.aggregate = sum)   
colnames(areaLandCover1)[2] ="COUNT"

areaLandCover2 <- dcast(data = LUTMDatabase, formula = ID_LC2 ~ ., fun.aggregate = sum)
colnames(areaLandCover2)[2] ="COUNT"

landCover_his<-areaLandCover2[,2]

LUTM_his <- dcast(data = LUTMDatabase, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)
LUTM_his<-LUTM_his[,-1]


###########################  hitung landTable, LPC, LRC historis #####################

landCover_his<-as.matrix(landCover_his)
LDMProp_his<-as.matrix(LDMProp_his)
GDPAll<-as.data.frame(GDPAll)

diagLU_his<-as.matrix(diag(landCover_his[,1]))
landTable_his<-LDMProp_his%*%diagLU_his
landReq_his<-as.matrix(rowSums(landTable_his))


LPC_his<-rbind(as.matrix(rowSums(cbind(indem, findem))), 0)/landReq_his    #rowSums(cbind(indem, findem))=output
LPC_his[is.infinite(LPC_his)]<-0
LPC_his[is.nan(LPC_his)]<-0
LRC_his<-1/LPC_his
LRC_his[is.infinite(LRC_his)]<-0
LRC_his[is.nan(LPC_his)]<-0
landTable_his<-as.data.frame(cbind(rbind(as.matrix(sector[,1]), "sektor lainnya"), rbind(as.matrix(sector[,2]), "sektor lainnya"), landTable_his, landReq_his, LPC_his, LRC_his))
colnames(landTable_his)<-c("Sektor", "Kategori", colnames(LDMProp_his),"Total_Kebutuhan_Lahan", "LPC", "LRC")
tahun<-as.vector(str_extract_all(colnames(landCover_his), '[0-9]+'))
tahun<-as.data.frame(tahun)
tahun<-t(tahun)


################################# (I) PERHITUNGAN BAU LAHAN ######################################################


# hitung proyeksi GPD BAU lahan
GDP_BAU<-matrix(NA, nrow=nrow(findem_series), ncol=ncol(findem_series))
for (i in 1:ncol(findem_series)){
  GDP_BAU[,i]<-tOutputSeries[,i]*GDPAll$P_OUTPUT
}


#### cari LRC tiap tahun proyeksi####
LRC_0<-matrix(NA, nrow=nrow(landTable_his),ncol=ncol(tOutputSeries))
for (i in 1:ncol(LRC_0)){
  if(i==1){
    LRC_0[,i]<-as.matrix(LRC_his)
  } else{
    LRC_0[,i]<-as.matrix(LRC_his)*as.matrix(LRCRate_his^(i-1))   #LRCRate_his nanti bisa dimodif di interface
  }
}


LRC_2<-matrix(NA, nrow=nrow(landTable_his),ncol=ncol(tOutputSeries))
for (i in 1:ncol(LRC_2)){
  LRC_2[,i]<-as.matrix(LRC_his)*as.matrix(LRCRate_2^(i-1))    #LRCRate_2 nanti bisa dimodif di interface
}


#### cari landReq dari proyeksi output #### 
landReq<-matrix(NA, nrow=nrow(landTable_his), ncol=ncol(tOutputSeries))
colnames(landReq)<-colnames(tOutputSeries)

for (i in 1:ncol(tOutputSeries)){
  landReq[,i]<-diag(LRC_0[,i])%*%rbind(as.matrix(tOutputSeries[,i]),0)
  landReq[nrow(landReq),i]<-sum(landCover_his[,1])-sum(landReq[1:nrow(landReq)-1,i])
}


if (length(landReq[landReq<0])>=1){
  for (i in 1:ncol(tOutputSeries)){
    landReq[,i]<-diag(LRC_2[,i])%*%rbind(as.matrix(tOutputSeries[,i]),0)
landReq[nrow(landReq),i]<-sum(landCover_his[,1])-sum(landReq[1:nrow(landReq)-1,i])
  }
  print("LRC_2 is used")
}else{}



rownames(landReq)<-c(as.matrix(sector[,1]),"sektor lainnya")

#### buat LDM prop (proporsi terhadap sektor) ####
LDMLuas<- as.matrix(landTable_his[,3:25])
class(LDMLuas)<-"numeric"
# LDMProp_sektor<-t(LDMLuas)%*%solve(diag(rowSums(LDMLuas)))
LDMProp_sektor<-matrix(NA, nrow=ncol(LDMLuas), ncol=nrow(LDMLuas))
for (i in 1:ncol(LDMProp_sektor)){
  LDMProp_sektor[,i]<-as.matrix(LDMLuas[i,]/sum(LDMLuas[i,]))
}
LDMProp_sektor[is.na(LDMProp_sektor)]<-0


################## cari land cover dari landReq yang diketahui: Land Cover<-LDMProp_sektor * LandReq ####################
landCover<-matrix(NA, nrow=nrow(LDMProp_sektor), ncol=ncol(tOutputSeries))
for (i in 1:ncol(tOutputSeries)){
  landCover[,i]<-LDMProp_sektor %*%landReq[,i]
}
rownames(landCover)<-colnames(LDMProp_his)




########### SUSUN TEMPLATE PERHITUNGAN LUTM ############################

## masukkan 0 pada matriks transisi jika total landcover = 0
for (i in 1:nrow(LUTM_template)){
  if (sum(landCover[i,])==0){
    LUTM_template[i,]<-t(as.matrix(rep(0,time=ncol(LUTM_template))))     #LUTM_template bisa diedit di interface
  } else {}
}
for (i in 1:ncol(LUTM_template)){
  if (sum(landCover[i,])==0){
    LUTM_template[,i]<-as.matrix(rep(0,time=ncol(LUTM_template)))
  } else {}
}


################################# PERHITUNGAN LUTM ################################################

jumlahvariabel<-length(LUTM_template[is.na(LUTM_template)])
namavariabel<-paste0("x",1:length(LUTM_template[is.na(LUTM_template)]))
LUTM_template[is.na(LUTM_template)]<-namavariabel


# isi matriks transisi dengan menganggap matriks sbg system of linear equations

# solve system of linear equations dgn matriks, Ax=B

  ################################### making equality constraints Ex = F ###############################################
  ## di shiny, reactive function thd line 151
  ## constraint 1: jumlah variabel LUTM di tiap row = tupla tahun pertama, 
  ##                maka matrix E matriks berisi 0 dan 1, di mana 1 diberikan untuk variabel yang berada pada baris yang sama
  ##                matrix F berisi tupla tahun pertama
  ## constraint 2: jumlah variabel LUTM di tiap kolom = tupla tahun kedua
  ##                maka matrix E matriks berisi 0 dan 1, di mana 1 diberikan untuk variabel yang berada pada kolom yang sama
  ##                matrix F berisi tupla tahun kedua
  
  ##### buat matrix E
  matrix_E<-matrix(NA,nrow = 46, ncol = jumlahvariabel)
  colnames(matrix_E)<-namavariabel
  variabel_x<-list()
  variabel_y<-list()
  for (a in 1:nrow(LUTM_template)){  ## constraint 1
    variabel_x[[a]]<-t(LUTM_template[a,])[t(LUTM_template[a,])!= 0]
    eval(parse(text=paste0("variabel_x_",a,"<-NULL")))
    eval(parse(text=paste0("variabel_x_",a,"<-variabel_x[[",a,"]]")))
    for (i in 1:length(variabel_x[[a]])){
      if(!identical(variabel_x[[a]],c(character(0),integer(0)))){
        eval(parse(text=paste0("matrix_E[",a,",paste0(variabel_x_",a,"[",i,"])]<-1")))
        # matrix_E[a,paste0(variabel_n[i])]<-1
      } else {matrix_E[a,]<-0}
    }
  }
  for (a in 1:ncol(LUTM_template)){  ##constraint 2
    variabel_y[[a]]<-t(LUTM_template[,a])[t(LUTM_template[,a])!= 0]
    eval(parse(text=paste0("variabel_y_",a,"<-NULL")))
    eval(parse(text=paste0("variabel_y_",a,"<-variabel_y[[",a,"]]")))
    for (i in 1:length(variabel_y[[a]])){
      if(!identical(variabel_y[[a]],numeric(0))){
        eval(parse(text=paste0("matrix_E[(23+",a,"),paste0(variabel_y_",a,"[",i,"])]<-1")))
        # matrix_E[a,paste0(variabel_n[i])]<-1
      } else {matrix_E[(23+a),]<-0}
    }
  }
  matrix_E[is.na(matrix_E)]<-0 
  matrix_E<-matrix_E[(!(rbind(as.matrix(landCover[,1]),as.matrix(landCover[,1]))) == 0),]  #hapus constraint untuk tupla yg jumlahnya 0 agar compatible saat perhitungan LSEI
  
  ## buat matrix F
  matrix_F<-list()
  subset_F<-list()
  for (i in 1:ncol(landCover)){
    if (i==1){
      matrix_F[[i]]<-NULL
    }else{
      matrix_F[[i]]<-rbind(as.matrix(landCover[,i-1]), as.matrix(landCover[,i]))
      matrix_F[[i]]<- as.matrix(matrix_F[[i]][!(rowSums(matrix_F[[i]]) == 0),]) 
    }
  }
    
  
  
  ################################### making inequality constraints Gx >= H ########################################
  
      ######################## asumsi 1 ######################## 
      
      # constraint 1: semua variabel >= 0, maka matrix G adalah matrik identitas, matrix H adalah vektor 0 
      # constraint 2: semua variabel yang mengisi diagonal LUTM (tupla yg tidak berubah) >= sekian persen tupla di tahun berikutnya, 
      #               maka matriks G adalah matriks berisi 0 dan 1 (di mana 1 diberikan untuk variabel diagonal), 
      #               matriks H adalah tupla2 * persentase minimum
      
      
      ## cari nama variabel diagonal LUTM
      diagonalVariabel_pre<-matrix(NA, ncol=1, nrow=ncol(LUTM_template))
      for (i in 1:ncol(LUTM_template)){
        diagonalVariabel_pre[i,1]<-LUTM_template[i,i]
      }
      diagonalVariabel<-diagonalVariabel_pre[!(diagonalVariabel_pre==0),]

      ## buat matrix G constraint 1 & 2
      matrix_G<-rbind(diag(nrow=(jumlahvariabel)), matrix(0, nrow=length(diagonalVariabel),ncol=jumlahvariabel))
      colnames(matrix_G)<-namavariabel
      for (i in 1:length(diagonalVariabel)){
        matrix_G[jumlahvariabel+i,diagonalVariabel[i]]<-1   #assign 1 untuk semua variabel diagonal
      }


      ## buat matrix H constraint 1 & 2
      matrix_H<-list()
      for (i in 1:ncol(landCover)){
        if (i==1){
          matrix_H[[i]]<-NULL
        }else{
          matrix_H[[i]]<-rbind(matrix(0,nrow=jumlahvariabel,ncol=1),as.matrix(landCover[(diagonalVariabel_pre!= 0),i])*0)
        }
      }
      
      ########################  asumsi 2 ###########################  
      
      # constraint 1: semua variabel >= proporsi LUTM historis, maka matrix G adalah matrik identitas, matrix H adalah vektor proporsi LUTM (proporsi = variabel / jumlah kelas tupla tsb di tahun kedua) 
      
      
      # analyze historical LUTM to decide which assumption to use for the inequality constraints
      # LUTMProp_his<-matrix(nrow=nrow(LUTM_his), ncol=ncol(LUTM_his))
      # for (i in 1:ncol(LUTMProp_his)){
      #   LUTMProp_his[,i]<-LUTM_his[,i]/landCover_his[i,1]   #proporsi semua elemen LUTM dibagi tupla tahun kedua
      # }
      # LUTMProp_his[is.nan(LUTMProp_his)]<-0
      # LUTMProp_his<-LUTMProp_his*0.3  #0.9 dari proporsi
      # 
      # matrix_G<-diag(1, nrow=jumlahvariabel)
      # 
      # pre_matrix_H<-list()
      # matrix_H<-list()
      # for (i in 1:ncol(landCover)){
      #   pre_matrix_H[[i]]<-matrix(ncol=ncol(LUTMProp_his), nrow=nrow(LUTMProp_his))
      #   for (a in 1:ncol(LUTMProp_his)){
      #     pre_matrix_H[[i]][,a]<-LUTMProp_his[,a]*landCover[a,i]
      #   }
      #   matrix_H[[i]]<-matrix(pre_matrix_H[[i]][LUTM_template!=0], ncol=1)
      # }
      
      
      ######################## asumsi 3 #############################
      # constraint 1: semua variabel >= proporsi LUTM historis, maka matrix G adalah matrik identitas, matrix H adalah vektor proporsi LUTM (proporsi = variabel / jumlah kelas tupla tsb di tahun kedua) 
      # constraint 2: semua variabel yang mengisi diagonal LUTM (tupla yg tidak berubah) >= sekian persen tupla di tahun berikutnya, 
      #               maka matriks G adalah matriks berisi 0 dan 1 (di mana 1 diberikan untuk variabel diagonal) dan matriks H adalah tupla2 * persentase minimum
      
      # analyze historical LUTM to decide which assumption to use for the inequality constraints
      # LUTMProp_his<-matrix(nrow=nrow(LUTM_his), ncol=ncol(LUTM_his))
      # for (i in 1:ncol(LUTMProp_his)){
      #   LUTMProp_his[,i]<-LUTM_his[,i]/landCover_his[i,1]   #proporsi semua elemen LUTM dibagi tupla tahun kedua
      # }
      # LUTMProp_his[is.nan(LUTMProp_his)]<-0
      # LUTMProp_his<-LUTMProp_his*0.3  #0.9 dari proporsi
      # 
      # ## cari nama variabel diagonal LUTM
      # diagonalVariabel_pre<-matrix(NA, ncol=1, nrow=ncol(LUTM_template))
      # for (i in 1:ncol(LUTM_template)){
      #   diagonalVariabel_pre[i,1]<-LUTM_template[i,i]
      # }
      # diagonalVariabel<-diagonalVariabel_pre[!(diagonalVariabel_pre==0),]
      # 
      # ## buat matrix G constraint 1 & 2
      # matrix_G<-rbind(diag(nrow=(jumlahvariabel)), matrix(0, nrow=length(diagonalVariabel),ncol=jumlahvariabel))
      # colnames(matrix_G)<-namavariabel
      # for (i in 1:length(diagonalVariabel)){
      #   matrix_G[jumlahvariabel+i,diagonalVariabel[i]]<-1   #assign 1 untuk semua variabel diagonal
      # }
      # 
      # pre_matrix_H<-list()
      # matrix_H<-list()
      # for (i in 1:ncol(landCover)){
      #   pre_matrix_H[[i]]<-matrix(ncol=ncol(LUTMProp_his), nrow=nrow(LUTMProp_his))
      #   for (a in 1:ncol(LUTMProp_his)){
      #     pre_matrix_H[[i]][,a]<-LUTMProp_his[,a]*landCover[a,i]
      #   }
      #   matrix_H[[i]]<-rbind(matrix(pre_matrix_H[[i]][LUTM_template!=0], ncol=1),as.matrix(landCover[(diagonalVariabel_pre != 0),i])*0.8)
      # }
      
      
  ######################################  solve dengan metode LSEI #######################################################
  
  
  lseiResult_list<-list()
  lseiResult<-list()
  variabel_lsei<-list()
  LUTM<-list()
  
  for (i in 1:ncol(landCover)){
    if (i==1){
      LUTM[[i]]<-as.matrix(LUTM_his)
    } else{
      lseiResult_list[[i]]<-lsei(E = matrix_E, F = matrix_F[[i]], G=matrix_G, H=matrix_H[[i]])
      lseiResult[[i]]<-as.matrix(unlist(lseiResult_list[[i]]))
      variabel_lsei[[i]]<-as.matrix(as.numeric(lseiResult[[i]][1:jumlahvariabel,]))
      row.names(variabel_lsei[[i]])<-namavariabel
      LUTM[[i]]<-as.matrix(LUTM_template)
      for (a in 1:nrow(LUTM[[i]])){
        for(b in 1:ncol(LUTM[[i]])){
          if (LUTM[[i]][a,b]==0){
            LUTM[[i]][a,b]<-as.numeric(0)
          } else {LUTM[[i]][a,b]<-as.numeric(variabel_lsei[[i]][paste0(LUTM_template[a,b]),1])
          }
        }
      }
      LUTM[[i]]<-as.matrix(LUTM[[i]])
      class(LUTM[[i]])<-"numeric"
    }
  }
  
  

  
  
  
  
##################################### HITUNG EMISI & INTENSITAS EMISI ################################
  
  emissionTable<-list()
  emissionYear_BAU<-matrix(NA,nrow=nrow(landCover), ncol=ncol(landCover))
  
  for (i in 1:length(LUTM)){
    emissionTable[[i]]<-matrix(NA,nrow=nrow(LUTM[[i]]), ncol=ncol(LUTM[[i]]))
    for (a in 1:nrow(LUTM[[i]])){
      for (b in 1:ncol(LUTM[[i]])){
        emissionTable[[i]][a,b]<-as.numeric(LUTM[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*(-1)
        emissionYear_BAU[,i]<-as.matrix(rowSums(emissionTable[[i]]))
      }
    }
  }
  
  
emissionBAU<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(emissionYear_BAU)))
colnames(emissionBAU)<-c("year", "emission.BAU")

findemTotYear<-colSums(findem_series)
findemSectorProp<-matrix(nrow=nrow(findem_series),ncol=ncol(findem_series))
emissionBAU_sector<-matrix(nrow=nrow(findem_series),ncol=ncol(findem_series))
emIntensityBAU<-matrix(nrow=nrow(findem_series),ncol=ncol(findem_series))
for (i in 1:nrow(emissionBAU)){
  for(a in 1:nrow(findemSectorProp)){
    findemSectorProp[a,i]<-findem_series[a,i]/findemTotYear[i]
    emissionBAU_sector[a,i]<-findemSectorProp[a,i]*as.numeric(emissionBAU[i,2])
    emIntensityBAU[,i]<-emissionBAU_sector[,i]/GDP_BAU[,i]
  }
}



################################## (II) PERHITUNGAN SKENARIO INTERVENSI #########################################



scenNumber=3
resultpath<-paste0("_TIN/result/", selectedProv, "/landScen", scenNumber,"_")




## masukkan final demand skenario 
#landScen_findem<-landScen1_findem
eval(parse(text=paste0("landScen_findem<-landScen",scenNumber,"_findem")))  #landScen_findem = user input

landScen_findem_series<-as.matrix(landScen_findem+findem_series)
landScen_tOutputSeries<-leontief%*%landScen_findem_series

## hitung proyeksi GDP skenario
landScen_GDP<-matrix(NA, nrow=nrow(landScen_findem_series), ncol=ncol(landScen_findem_series))
for (i in 1:ncol(landScen_findem_series)){
  landScen_GDP[,i]<-landScen_tOutputSeries[,i]*GDPAll$P_OUTPUT
}

## hitung proyeksi land Requirement skenario 
landScen_landReq<-matrix(NA, nrow=nrow(landTable_his), ncol=ncol(landScen_tOutputSeries))
colnames(landScen_landReq)<-colnames(landScen_tOutputSeries)


for (i in 1:ncol(landScen_tOutputSeries)){
  landScen_landReq[,i]<-diag(LRC_0[,i])%*%rbind(as.matrix(landScen_tOutputSeries[,i]),0)
  landScen_landReq[nrow(landScen_landReq),i]<-sum(landCover_his[,1])-sum(landScen_landReq[1:nrow(landScen_landReq)-1,i])
}

## jika hasil landReq ada yang 0, gunakan LRC yang modified
if (length(landScen_landReq[landScen_landReq<0])>=1){
  for (i in 1:ncol(landScen_tOutputSeries)){
    landScen_landReq[,i]<-diag(LRC_2[,i])%*%rbind(as.matrix(landScen_tOutputSeries[,i]),0)
    landScen_landReq[nrow(landScen_landReq),i]<-sum(landCover_his[,1])-sum(landScen_landReq[1:nrow(landScen_landReq)-1,i])
  }
}else{}


rownames(landScen_landReq)<-c(as.matrix(sector[,1]),"sektor lainnya")



# #### buat LDM prop (proporsi terhadap sektor) ####
# LDMLuas<- as.matrix(landTable_his[,3:25])
# class(LDMLuas)<-"numeric"
# LDMProp_sektor<-t(LDMLuas)%*%solve(diag(rowSums(LDMLuas)))

################## cari land cover dari proyeksi landReq skenario: land cover <-LDMProp_sektor * LandReq ####################
landScen_landCover_proj<-matrix(NA, nrow=nrow(LDMProp_sektor), ncol=ncol(landScen_tOutputSeries))

for (i in 1:ncol(landScen_tOutputSeries)){
  landScen_landCover_proj[,i]<-LDMProp_sektor %*% landScen_landReq[,i]
}
rownames(landScen_landCover_proj)<-colnames(LDMProp_his)


#################### Masukkan land cover intervensi ###############################
# dari landCover baru yang diproyeksi berdasarkan final demand, edit sesuai dengan intervensi 
write.csv(landScen_landCover_proj,paste0(resultpath,"landCover_proj.csv"))     # sebagai output rhandsontable di interface

#edit landScen_landCover_result, dan masukkan kembali sebagai landScen_landCover
#landScen_inputLandCover<-as.matrix(landScen_inputLandCover)   # sebagai user input
eval(parse(text=paste0("landScen_inputLandCover<-as.matrix(landScen",scenNumber,"_inputLandCover)")))  # user input
landScen_inputLandCover[is.na(landScen_inputLandCover)]<-0
landScen_landCover<-as.matrix(as.matrix(landScen_landCover_proj) + as.matrix(landScen_inputLandCover))


############################### SUSUN TEMPLATE PERHITUNGAN LUTM ############################

# masukkan input LUTM_template skenario

# landScen_LUTM_template<-as.matrix(landScen_LUTM_template) # user input
eval(parse(text=paste0("landScen_LUTM_template<-as.matrix(landScen",scenNumber,"_LUTM_template)")))

#masukkan 0 pada matriks transisi jika total landcover = 0 

for (i in 1:nrow(landScen_LUTM_template)){
  if (sum(landScen_landCover[i,])==0){
    landScen_LUTM_template[i,]<-t(as.matrix(rep(0,time=ncol(landScen_LUTM_template))))     #LUTM_template bisa diedit di interface
  } else {}
}
for (i in 1:ncol(landScen_LUTM_template)){
  if (sum(landScen_landCover[i,])==0){
    landScen_LUTM_template[,i]<-as.matrix(rep(0,time=ncol(landScen_LUTM_template)))
  } else {}
}


################################# PERHITUNGAN LUTM ################################################

landScen_jumlahvariabel<-length(landScen_LUTM_template[is.na(landScen_LUTM_template)])
landScen_namavariabel<-paste0("x",1:length(landScen_LUTM_template[is.na(landScen_LUTM_template)]))
landScen_LUTM_template[is.na(landScen_LUTM_template)]<-landScen_namavariabel
rownames(landScen_LUTM_template) <-colnames(LUTM_template)
colnames(landScen_LUTM_template)<-colnames(LUTM_template)

# isi matriks transisi dengan menganggap matriks sbg system of linear equations

# solve system of linear equations dgn matriks, Ax=B

################################### making equality constraints Ex = F ###############################################
## di shiny, reactive function thd line 151
## constraint 1: jumlah variabel LUTM di tiap row = tupla tahun pertama, 
##                maka matrix E matriks berisi 0 dan 1, di mana 1 diberikan untuk variabel yang berada pada baris yang sama
##                matrix F berisi tupla tahun pertama
## constraint 2: jumlah variabel LUTM di tiap kolom = tupla tahun kedua
##                maka matrix E matriks berisi 0 dan 1, di mana 1 diberikan untuk variabel yang berada pada kolom yang sama
##                matrix F berisi tupla tahun kedua

####### buat matrix E #########
landScen_matrix_E<-matrix(NA,nrow = 46, ncol = landScen_jumlahvariabel)
colnames(landScen_matrix_E)<-landScen_namavariabel
landScen_variabel_x<-list()
landScen_variabel_y<-list()
for (a in 1:nrow(landScen_LUTM_template)){  ## constraint 1
  landScen_variabel_x[[a]]<-t(landScen_LUTM_template[a,])[t(landScen_LUTM_template[a,])!= 0]
  eval(parse(text=paste0("landScen_variabel_x_",a,"<-NULL")))
  eval(parse(text=paste0("landScen_variabel_x_",a,"<-landScen_variabel_x[[",a,"]]")))
  for (i in 1:length(landScen_variabel_x[[a]])){
    if(!identical(landScen_variabel_x[[a]],c(character(0),integer(0)))){
      eval(parse(text=paste0("landScen_matrix_E[",a,",paste0(landScen_variabel_x_",a,"[",i,"])]<-1")))
      # landScen_matrix_E[a,paste0(landScen_variabel_n[i])]<-1
    } else {landScen_matrix_E[a,]<-0}
  }
}
for (a in 1:ncol(landScen_LUTM_template)){  ##constraint 2
  landScen_variabel_y[[a]]<-t(landScen_LUTM_template[,a])[t(landScen_LUTM_template[,a])!= 0]
  eval(parse(text=paste0("landScen_variabel_y_",a,"<-NULL")))
  eval(parse(text=paste0("landScen_variabel_y_",a,"<-landScen_variabel_y[[",a,"]]")))
  for (i in 1:length(landScen_variabel_y[[a]])){
    if(!identical(landScen_variabel_y[[a]],c(numeric(0),character(0), integer(0)))){
      eval(parse(text=paste0("landScen_matrix_E[(23+",a,"),paste0(landScen_variabel_y_",a,"[",i,"])]<-1")))
      # landScen_matrix_E[a,paste0(landScen_variabel_n[i])]<-1
    } else {landScen_matrix_E[(23+a),]<-0}
  }
}
landScen_matrix_E[is.na(landScen_matrix_E)]<-0 
landScen_matrix_E<-landScen_matrix_E[(!(rbind(as.matrix(landScen_landCover[,1]),as.matrix(landScen_landCover[,1]))) == 0),]  #hapus constraint untuk tupla yg jumlahnya 0 agar compatible saat perhitungan LSEI



######  buat matrix F ######
landScen_matrix_F<-list()
for (i in 1:ncol(landScen_landCover)){
  if (i==1){
    landScen_matrix_F[[i]]<-NULL
  }else{
    landScen_matrix_F[[i]]<-rbind(as.matrix(landScen_landCover[,i-1]), as.matrix(landScen_landCover[,i]))
    landScen_matrix_F[[i]]<- as.matrix(landScen_matrix_F[[i]][!(rowSums(landScen_matrix_F[[i]]) == 0),]) 
  }
}



################################### making inequality constraints Gx >= H ########################################

######################## asumsi 1 ######################## 

# constraint 1: semua variabel >= 0, maka matrix G adalah matrik identitas, matrix H adalah vektor 0 
# constraint 2: semua variabel yang mengisi diagonal LUTM (tupla yg tidak berubah) >= sekian persen tupla di tahun berikutnya, 
#               maka matriks G adalah matriks berisi 0 dan 1 (di mana 1 diberikan untuk variabel diagonal), 
#               matriks H adalah tupla2 * persentase minimum


## cari nama variabel diagonal LUTM
landScen_diagonalVariabel_pre<-matrix(NA, ncol=1, nrow=ncol(landScen_LUTM_template))
for (i in 1:ncol(landScen_LUTM_template)){
  landScen_diagonalVariabel_pre[i,1]<-landScen_LUTM_template[i,i]
}
landScen_diagonalVariabel<-landScen_diagonalVariabel_pre[!(landScen_diagonalVariabel_pre==0),]

## buat matrix G constraint 1 & 2
landScen_matrix_G<-rbind(diag(nrow=(landScen_jumlahvariabel)), matrix(0, nrow=length(landScen_diagonalVariabel),ncol=landScen_jumlahvariabel))
colnames(landScen_matrix_G)<-landScen_namavariabel
for (i in 1:length(landScen_diagonalVariabel)){
  landScen_matrix_G[landScen_jumlahvariabel+i,landScen_diagonalVariabel[i]]<-1   #assign 1 untuk semua variabel diagonal
}


## buat matrix H constraint 1 & 2
landScen_matrix_H<-list()
for (i in 1:ncol(landScen_landCover)){
  if (i==1){
    landScen_matrix_H[[i]]<-NULL
  }else{
    landScen_matrix_H[[i]]<-rbind(matrix(0,nrow=landScen_jumlahvariabel,ncol=1),as.matrix(landScen_landCover[(diagonalVariabel_pre != 0),i])*0)
  }
}

########################  asumsi 2 ###########################  

# constraint 1: semua variabel >= proporsi LUTM historis, maka matrix G adalah matrik identitas, matrix H adalah vektor proporsi LUTM (proporsi = variabel / jumlah kelas tupla tsb di tahun kedua) 


# analyze historical LUTM to decide which assumption to use for the inequality constraints
# LUTMProp_his<-matrix(nrow=nrow(LUTM_his), ncol=ncol(LUTM_his))
# for (i in 1:ncol(LUTMProp_his)){
#   LUTMProp_his[,i]<-LUTM_his[,i]/landCover_his[i,1]   #proporsi semua elemen LUTM dibagi tupla tahun kedua
# }
# LUTMProp_his[is.nan(LUTMProp_his)]<-0
# LUTMProp_his<-LUTMProp_his*0.3  #0.9 dari proporsi
# 
# landScen_matrix_G<-diag(1, nrow=landScen_jumlahvariabel)
# 
# landScen_pre_matrix_H<-list()
# landScen_matrix_H<-list()
# for (i in 1:ncol(landScen_landCover)){
#   landScen_pre_matrix_H[[i]]<-matrix(ncol=ncol(LUTMProp_his), nrow=nrow(LUTMProp_his))
#   for (a in 1:ncol(LUTMProp_his)){
#     landScen_pre_matrix_H[[i]][,a]<-LUTMProp_his[,a]*landScen_landCover[a,i]
#   }
#   landScen_matrix_H[[i]]<-matrix(landScen_pre_matrix_H[[i]][landScen_LUTM_template!=0], ncol=1)
# }


######################## asumsi 3 #############################
# constraint 1: semua variabel >= proporsi LUTM historis, maka matrix G adalah matrik identitas, matrix H adalah vektor proporsi LUTM (proporsi = variabel / jumlah kelas tupla tsb di tahun kedua) 
# constraint 2: semua variabel yang mengisi diagonal LUTM (tupla yg tidak berubah) >= sekian persen tupla di tahun berikutnya, 
#               maka matriks G adalah matriks berisi 0 dan 1 (di mana 1 diberikan untuk variabel diagonal) dan matriks H adalah tupla2 * persentase minimum

# # analyze historical LUTM to decide which assumption to use for the inequality constraints
# LUTMProp_his<-matrix(nrow=nrow(LUTM_his), ncol=ncol(LUTM_his))
# for (i in 1:ncol(LUTMProp_his)){
#   LUTMProp_his[,i]<-LUTM_his[,i]/landCover_his[i,1]   #proporsi semua elemen LUTM dibagi tupla tahun kedua
# }
# LUTMProp_his[is.nan(LUTMProp_his)]<-0
# LUTMProp_his<-LUTMProp_his*0.3  #0.9 dari proporsi
# 
# ## cari nama variabel diagonal LUTM
# landScen_diagonalVariabel_pre<-matrix(NA, ncol=1, nrow=ncol(landScen_LUTM_template))
# for (i in 1:ncol(landScen_LUTM_template)){
#   landScen_diagonalVariabe_prel[i,1]<-landScen_LUTM_template[i,i]
# }
# landScen_diagonalVariabel<-landScen_diagonalVariabel_pre[!(landScen_diagonalVariabel_pre==0),]
# 
# ## buat matrix G constraint 1 & 2
# landScen_matrix_G<-rbind(diag(nrow=(landScen_jumlahvariabel)), matrix(0, nrow=length(landScen_diagonalVariabel),ncol=landScen_jumlahvariabel))
# colnames(landScen_matrix_G)<-landScen_namavariabel
# for (i in 1:length(landScen_diagonalVariabel)){
#   landScen_matrix_G[landScen_jumlahvariabel+i,landScen_diagonalVariabel[i]]<-1   #assign 1 untuk semua variabel diagonal
# }
# 
# ## buat matrix H constraint 1 & 2
# landScen_pre_matrix_H<-list()
# landScen_matrix_H<-list()
# for (i in 1:ncol(landScen_landCover)){
#   landScen_pre_matrix_H[[i]]<-matrix(ncol=ncol(LUTMProp_his), nrow=nrow(LUTMProp_his))
#   for (a in 1:ncol(LUTMProp_his)){
#     landScen_pre_matrix_H[[i]][,a]<-LUTMProp_his[,a]*landScen_landCover[a,i]
#   }
#   landScen_matrix_H[[i]]<-rbind(matrix(landScen_pre_matrix_H[[i]][landScen_LUTM_template!=0], ncol=1),as.matrix(landScen_landCover[(landScen_diagonalVariabel_pre[,i] != 0),i])*0.8)
# }

################################ masukkan input additional constraint #########################

        ###### input constraint matrix E ########

        # landScen_additionalE<-landScen1_additionalE  # user input
        eval(parse(text=paste0("landScen_additionalE<-landScen",scenNumber,"_additionalE")))
        # masukkan tambahan constraint
        if (is.null(landScen_additionalE)){
          landScen_matrix_E<-landScen_matrix_E
        } else{
          landScen_matrix_E<- rbind(landScen_matrix_E, landScen_additionalE)
        }

        ###### input constraint matrix F #######
        
        # landScen_additionalF<-landScen1_additionalF  # user input
        eval(parse(text=paste0("landScen_additionalF<-landScen",scenNumber,"_additionalF")))
        # masukkan tambahan constraint
        
        if (is.null(landScen_additionalF)){
          landScen_matrix_F<-landScen_matrix_F
          print("landScen_matrix_F=null")
        } else{
          for (i in 1:length(landScen_matrix_F)){
          landScen_matrix_F[[i]]<- rbind(landScen_matrix_F[[i]], landScen_additionalF[[i]])
          }
        }
        
        
        ####### input constraint matrix G #######
        
          # landScen_additionalG<-landScen1_additionalG  # user input
          eval(parse(text=paste0("landScen_additionalG<-landScen",scenNumber,"_additionalG")))
          # masukkan tambahan constraint
          if (is.null(landScen_additionalG)){
            landScen_matrix_G<-landScen_matrix_G
          } else{
            landScen_matrix_G<- rbind(landScen_matrix_G, landScen_additionalG)
          }
        
        
        
        ####### input constraint matrix H #######
          # landScen_additionalF<-landScen1_additionalH  # user input
          eval(parse(text=paste0("landScen_additionalH<-landScen",scenNumber,"_additionalH")))
          # masukkan tambahan constraint
          if (is.null(landScen_additionalH[[i]])){
              landScen_matrix_H<-landScen_matrix_H
          } else{
            for (i in 1:length(landScen_matrix_H)){
            landScen_matrix_H[[i]]<- rbind(landScen_matrix_H[[i]], landScen_additionalH[[i]])
            }  
          }          


######################################  solve dengan metode LSEI #######################################################


landScen_lseiResult_list<-list()
landScen_lseiResult<-list()
landScen_variabel_lsei<-list()
landScen_LUTM<-list()

for (i in 1:ncol(landScen_landCover)){
  if (i==1){
    landScen_LUTM[[i]]<-as.matrix(LUTM_his)
  } else{
    landScen_lseiResult_list[[i]]<-lsei(E = landScen_matrix_E, F = landScen_matrix_F[[i]], G=landScen_matrix_G, H=landScen_matrix_H[[i]])
    landScen_lseiResult[[i]]<-as.matrix(unlist(landScen_lseiResult_list[[i]]))
    landScen_variabel_lsei[[i]]<-as.matrix(as.numeric(landScen_lseiResult[[i]][1:landScen_jumlahvariabel,]))
    row.names(landScen_variabel_lsei[[i]])<-landScen_namavariabel
    landScen_LUTM[[i]]<-as.matrix(landScen_LUTM_template)
    for (a in 1:nrow(landScen_LUTM[[i]])){
      for(b in 1:ncol(landScen_LUTM[[i]])){
        if (landScen_LUTM[[i]][a,b]==0){
          landScen_LUTM[[i]][a,b]<-as.numeric(0)
        } else {landScen_LUTM[[i]][a,b]<-as.numeric(landScen_variabel_lsei[[i]][paste0(landScen_LUTM_template[a,b]),1])
        }
      }
    }
    landScen_LUTM[[i]]<-as.matrix(landScen_LUTM[[i]])
    class(landScen_LUTM[[i]])<-"numeric"
  }
}


####### untuk intervensi di LUTM #########
eval(parse(text = paste0("landScen_LUTMChange<-landScen",scenNumber,"_LUTMChange")))

if (is.null(landScen_LUTMChange)){
  landScen_LUTM<-landScen_LUTM
} else {
  for (i in 1:length(landScen_LUTMChange)){
    if(i ==1){
      landScen_LUTM[[i]]<-LUTM_his
    }else{
      landScen_LUTM[[i]]<-as.matrix(landScen_LUTM[[i]]+landScen_LUTMChange[[i]])
    }
  }
}

######### hitung emisi skenario ##############

landScen_emissionTable<-list()
landScen_emissionYear<-matrix(NA,nrow=nrow(landCover), ncol=ncol(landCover))

for (i in 1:length(landScen_LUTM)){
  landScen_emissionTable[[i]]<-matrix(NA,nrow=nrow(landScen_LUTM[[i]]), ncol=ncol(landScen_LUTM[[i]]))
  for (a in 1:nrow(landScen_LUTM[[i]])){
    for (b in 1:ncol(landScen_LUTM[[i]])){
      landScen_emissionTable[[i]][a,b]<-as.numeric(landScen_LUTM[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*(-1)
    }
  }
  landScen_emissionYear[,i]<-as.matrix(rowSums(landScen_emissionTable[[i]]))
}
landScen_emission<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen_emissionYear)))



## hitung emIntensity

landScen_findem_totalYear<-colSums(landScen_findem_series)
landScen_findemSectorProp<-matrix(nrow=nrow(landScen_findem_series),ncol=ncol(landScen_findem_series))
landScen_emission_sector<-matrix(nrow=nrow(landScen_findem_series),ncol=ncol(landScen_findem_series))
landScen_emIntensity<-matrix(nrow=nrow(landScen_findem_series),ncol=ncol(landScen_findem_series))
for (i in 1:nrow(landScen_emission)){
  for(a in 1:nrow(landScen_findemSectorProp)){
    landScen_findemSectorProp[a,i]<-landScen_findem_series[a,i]/landScen_findem_totalYear[i]
    landScen_emission_sector[a,i]<-landScen_findemSectorProp[a,i]*as.numeric(landScen_emission[i,2])
    landScen_emIntensity[,i]<-landScen_emission_sector[,i]/landScen_GDP[,i]
  }
}




######################## write data ############################


##### hapus tahun pertama. Perlu nggak yah?
GDP_BAU<-GDP_BAU[,-c(1)]
emissionBAU_sector<-emissionBAU_sector[,-c(1)]
emissionBAU<-emissionBAU[-c(1),]
emIntensityBAU<-emIntensityBAU[,-c(1)]
landScen_GDP<-landScen_GDP[,-c(1)]
landScen_emission_sector<-landScen_emission_sector[,-c(1)]
landScen_emIntensity<-landScen_emIntensity[,-c(1)]
tOutputSeries<-tOutputSeries[,-c(1)]
landScen_tOutputSeries<-landScen_tOutputSeries[,-c(1)]

# #### buat tabel sesuai start & end year
# colnames(GDP_BAU)<-colnames(tOutputSeries)
# colnames(emissionBAU_sector)<-colnames(tOutputSeries)
# rownames(emissionBAU)<-colnames(tOutputSeries)
# colnames(emIntensityBAU)<-colnames(tOutputSeries)
# colnames(landScen_GDP)<-colnames(tOutputSeries)
# colnames(landScen_emission_sector)<-colnames(tOutputSeries)
# colnames(landScen_emIntensity)<-colnames(tOutputSeries)
# colnames(landScen_tOutputSeries)<-colnames(tOutputSeries)
# 
# startYear= "y2017"
# endYear= "y2020"
# 
# GDP_BAU<-GDP_BAU[,startYear:endYear]
# emissionBAU_sector<-emissionBAU_sector[,-c(1)]
# emissionBAU<-emissionBAU[-c(1),]
# emIntensityBAU<-emIntensityBAU[,-c(1)]
# landScen_GDP<-landScen_GDP[,-c(1)]
# landScen_emission_sector<-landScen_emission_sector[,-c(1)]
# landScen_emIntensity<-landScen_emIntensity[,-c(1)]
# tOutputSeries<-tOutputSeries[,-c(1)]
# landScen_tOutputSeries<-landScen_tOutputSeries[,-c(1)]


######### (1) scenario-specific dataframe of sector><simulation years depicting GDP, emission and emission intensity ########

#### for BAU
write.csv(GDP_BAU, paste0(resultpath,"landBAU_GDP.csv"))
write.csv(emissionBAU_sector,paste0(resultpath,"landBAU_emission.csv"))
write.csv(emIntensityBAU,paste0(resultpath,"landBAU_emIntensity.csv"))
     
#### for scenario   
write.csv(landScen_GDP,paste0(resultpath,"GDP.csv"))
write.csv(landScen_emission_sector,paste0(resultpath,"emission.csv"))
write.csv(landScen_emIntensity,paste0(resultpath,"emIntensity.csv"))


######### (2) scenario-specific dataframe of sector><simulation years depicting GDP change against BAU, emission change against BAU and emission intensity change against BAU#####

landScen_deltaGDP<-as.matrix(landScen_GDP-GDP_BAU)
landScen_deltaEmission<-as.matrix(landScen_emission_sector-emissionBAU_sector)
landScen_deltaEmIntensity<-as.matrix(landScen_emIntensity-emIntensityBAU)

write.csv(landScen_deltaGDP, paste0(resultpath,"deltaGDP.csv"))
write.csv(landScen_deltaEmission, paste0(resultpath,"deltaEmission.csv"))
write.csv(landScen_deltaEmIntensity, paste0(resultpath,"deltaEmIntensity.csv"))

      
########## (3) scenario-specific dataframe from t0 - t15 depicting cumulative emission reduction, GDP change and emission intensity change #######
##### for BAU 
###GDP
BAU_totGDPYear<-cbind(colnames(tOutputSeries), colSums(GDP_BAU))
colnames(BAU_totGDPYear)<-c("year", "GDP.BAU")
BAU_cumulativeGDP <- as.matrix(BAU_totGDPYear)
for (i in 1:nrow(BAU_totGDPYear)){
  if (i==1){
    BAU_cumulativeGDP[i,2]<-as.numeric(BAU_totGDPYear[i,2])
  } else{
    BAU_cumulativeGDP[i,2]<-as.numeric(BAU_totGDPYear[i,2])+as.numeric(BAU_cumulativeGDP[i-1,2])
  }
}


###emission
BAU_totEmissionYear<-emissionBAU
colnames(BAU_totEmissionYear)<-c("year", "emission.BAU")
BAU_cumulativeEmission <- as.matrix(BAU_totEmissionYear)
for (i in 1:nrow(BAU_totEmissionYear)){
  if (i==1){
    BAU_cumulativeEmission[i,2]<-as.numeric(BAU_totEmissionYear[i,2])
  } else{
    BAU_cumulativeEmission[i,2]<-as.numeric(BAU_totEmissionYear[i,2])+as.numeric(BAU_cumulativeEmission[i-1,2])
  }
}

##emIntensity 
BAU_totEmIntensityYear<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(as.numeric(BAU_totEmissionYear[,2])/as.numeric(BAU_totGDPYear[,2])))
colnames(BAU_totEmIntensityYear)<-c("year", "emIntensity.BAU")
BAU_cumulativeEmIntensity <- as.matrix(BAU_totEmIntensityYear)
for (i in 1:nrow(BAU_totEmIntensityYear)){
  if (i==1){
    BAU_cumulativeEmIntensity[i,2]<-as.numeric(BAU_totEmIntensityYear[i,2])
  } else{
    BAU_cumulativeEmIntensity[i,2]<-as.numeric(BAU_totEmIntensityYear[i,2])+as.numeric(BAU_cumulativeEmIntensity[i-1,2])
  }
}

###### for Scenario
### cumulative GDP
landScen_totGDPYear<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen_GDP)))
colnames(landScen_totGDPYear)<-c("year", "GDP")
landScen_cumulativeGDP <- as.matrix(landScen_totGDPYear)
for (i in 1:nrow(landScen_totGDPYear)){
  if (i==1){
    landScen_cumulativeGDP[i,2]<-as.numeric(landScen_totGDPYear[i,2])
  } else{
    landScen_cumulativeGDP[i,2]<-as.numeric(landScen_totGDPYear[i,2])+as.numeric(landScen_cumulativeGDP[i-1,2])
  }
}


### cumulative emission
landScen_totEmissionYear<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen_emission_sector)))
colnames(landScen_totEmissionYear)<-c("year", "emission")
landScen_cumulativeEmission <- as.matrix(landScen_totEmissionYear)
  for (i in 1:nrow(landScen_cumulativeEmission)){
    if (i==1){
      landScen_cumulativeEmission[i,2]<-as.numeric(landScen_totEmissionYear[i,2])
    } else{
      landScen_cumulativeEmission[i,2]<-as.numeric(landScen_totEmissionYear[i,2])+as.numeric(landScen_cumulativeEmission[i-1,2])
    }
  }

### cumulative emission intensity

landScen_totEmIntensityYear<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(as.numeric(landScen_totEmissionYear[,2])/as.numeric(landScen_totGDPYear[,2])))
colnames(landScen_totEmIntensityYear)<-c("year", "emIntensity")
landScen_cumulativeEmIntensity <- as.matrix(landScen_totEmIntensityYear)
for (i in 1:nrow(landScen_totEmIntensityYear)){
  if (i==1){
    landScen_cumulativeEmIntensity[i,2]<-as.numeric(landScen_totEmIntensityYear[i,2])
  } else{
    landScen_cumulativeEmIntensity[i,2]<-as.numeric(landScen_totEmIntensityYear[i,2])+as.numeric(landScen_cumulativeEmIntensity[i-1,2])
  }
}


#### write csv
write.csv(landScen_cumulativeGDP, paste0(resultpath,"cumulativeGDP.csv"))
write.csv(landScen_cumulativeEmission, paste0(resultpath,"cumulativeEmission.csv"))
write.csv(landScen_cumulativeEmIntensity, paste0(resultpath,"cumulativeEmIntensity.csv"))

#### graph
compare_cumulativeGDP<-merge(BAU_cumulativeGDP, landScen_cumulativeGDP,by="year")
    compare_cumulativeGDP[,2]<-as.numeric(as.character(compare_cumulativeGDP[,2]))
    compare_cumulativeGDP[,3]<-as.numeric(as.character(compare_cumulativeGDP[,3]))
compare_cumulativeEmission<-merge(BAU_cumulativeEmission, landScen_cumulativeEmission,by="year")
    compare_cumulativeEmission[,2]<-as.numeric(as.character(compare_cumulativeEmission[,2]))
    compare_cumulativeEmission[,3]<-as.numeric(as.character(compare_cumulativeEmission[,3]))
compare_cumulativeEmIntensity<-merge(BAU_cumulativeEmIntensity, landScen_cumulativeEmIntensity,by="year")
    compare_cumulativeEmIntensity[,2]<-as.numeric(as.character(compare_cumulativeEmIntensity[,2]))
    compare_cumulativeEmIntensity[,3]<-as.numeric(as.character(compare_cumulativeEmIntensity[,3]))

plot_compare_cumulativeGDP<- ggplot(data=as.data.frame(compare_cumulativeGDP), aes(year)) + geom_line(aes(y=GDP.BAU, group=1, colour = "GDP.BAU")) + geom_line(aes(y=GDP, group=1,colour = "GDP"))
plot_compare_cumulativeEmission<-ggplot(data=as.data.frame(compare_cumulativeEmission), aes(year)) + geom_line(aes(y=emission.BAU, group=1, colour = "emission.BAU")) + geom_line(aes(y=emission,group=1, colour = "emission"))
plot_compare_cumulativeEmIntensity<-ggplot(data=as.data.frame(compare_cumulativeEmIntensity), aes(year)) + geom_line(aes(y=emIntensity.BAU, group=1, colour = "emIntensity.BAU")) + geom_line(aes(y=emIntensity,group=1, colour = "emIntensity"))

persenPeningkatanGDP<-(compare_cumulativeGDP[nrow(compare_cumulativeGDP),3]/compare_cumulativeGDP[nrow(compare_cumulativeGDP),2]-1)*100
persenPenurunanEmisi<-(compare_cumulativeEmission[nrow(compare_cumulativeEmission),3]/compare_cumulativeEmission[nrow(compare_cumulativeEmission),2]-1)*-100
persenPenurunanIntensitasEmisi<-(compare_cumulativeEmIntensity[nrow(compare_cumulativeEmIntensity),3]/compare_cumulativeEmIntensity[nrow(compare_cumulativeEmIntensity),2]-1)*-100

####### (4) scenario-specific line chart of emission, GDP and emission intensity ########
landScen_totGDPYear<-as.data.frame(landScen_totGDPYear)
    landScen_totGDPYear[,2]<-as.numeric(as.character(landScen_totGDPYear[,2]))
landScen_totEmissionYear<-as.data.frame(landScen_totEmissionYear)
    landScen_totEmissionYear[,2]<-as.numeric(as.character(landScen_totEmissionYear[,2]))
landScen_totEmIntensityYear<-as.data.frame(landScen_totEmIntensityYear)
    landScen_totEmIntensityYear[,2]<-as.numeric(as.character(landScen_totEmIntensityYear[,2]))


        
plot_landScen_totGDPYear<- ggplot(data=as.data.frame(landScen_totGDPYear), aes(x=year, y=GDP, group=1)) + geom_line() + geom_point()
plot_landScen_totEmissionYear<-ggplot(data=as.data.frame(landScen_totEmissionYear), aes(x=year, y=emission, group=1)) + geom_line() + geom_point()
plot_landScen_totEmIntensityYear<-ggplot(data=as.data.frame(landScen_totEmIntensityYear), aes(x=year, y=emIntensity, group=1)) + geom_line() + geom_point()



compare_totGDPYear<-merge(BAU_totGDPYear,landScen_totGDPYear, by="year")
    compare_totGDPYear[,2]<-as.numeric(as.character(compare_totGDPYear[,2]))
    compare_totGDPYear[,3]<-as.numeric(as.character(compare_totGDPYear[,3]))
compare_totEmissionYear<-merge(BAU_totEmissionYear,landScen_totEmissionYear, by="year")
    compare_totEmissionYear[,2]<-as.numeric(as.character(compare_totEmissionYear[,2]))
    compare_totEmissionYear[,3]<-as.numeric(as.character(compare_totEmissionYear[,3]))
compare_totEmIntensityYear<-merge(BAU_totEmIntensityYear,landScen_totEmIntensityYear,by="year")
    compare_totEmIntensityYear[,2]<-as.numeric(as.character(compare_totEmIntensityYear[,2]))
    compare_totEmIntensityYear[,3]<-as.numeric(as.character(compare_totEmIntensityYear[,3]))

    
plot_compare_totGDPYear<-ggplot(data=as.data.frame(compare_totGDPYear), aes(year)) + geom_line(aes(y=GDP.BAU, group=1, colour = "GDP.BAU")) + geom_line(aes(y=GDP, group=2,colour = "GDP"))
plot_compare_totEmissionYear<-ggplot(data=as.data.frame(compare_totEmissionYear), aes(year)) + geom_line(aes(y=emission.BAU, group=1, colour = "emission.BAU")) + geom_line(aes(y=emission,group=2, colour = "emission"))
plot_compare_totEmIntensityYear<-ggplot(data=as.data.frame(compare_totEmIntensityYear), aes(year)) + geom_line(aes(y=emIntensity.BAU, group=1, colour = "emIntensity.BAU")) + geom_line(aes(y=emIntensity,group=2, colour = "emIntensity"))

ggsave(paste0(resultpath, "plot_compare_cumulativeEmIntensity.jpg"), plot_compare_cumulativeEmIntensity)
ggsave(paste0(resultpath, "plot_compare_cumulativeEmission.jpg"), plot_compare_cumulativeEmission)
ggsave(paste0(resultpath, "plot_compare_cumulativeGDP.jpg"), plot_compare_cumulativeGDP)
ggsave(paste0(resultpath, "plot_compare_totEmIntensityYear.jpg"), plot_compare_totEmIntensityYear)
ggsave(paste0(resultpath, "plot_compare_totEmissionYear.jpg"), plot_compare_totEmissionYear)
ggsave(paste0(resultpath, "plot_compare_totGDPYear.jpg"), plot_compare_totGDPYear)




# (5) bar chart of performance comparison across scenario at t15 on emission, GDP and emission intensity

compare_landScenPerformance<-matrix(NA, nrow=7, ncol=4)
for (i in 1:7){
  GDP<-as.matrix(read.csv(paste0(resultpath,"cumulativeGDP.csv"), header=TRUE))
  emission<-as.matrix(read.csv(paste0(resultpath,"cumulativeEmission.csv"), header=TRUE))
  emIntensity<-as.matrix(read.csv(paste0(resultpath,"cumulativeEmIntensity.csv"), header=TRUE))
  compare_landScenPerformance[i,]<-cbind(paste0("landScen",i), 
                                         as.numeric(GDP[nrow(GDP),ncol(GDP)]),
                                         as.numeric(emission[nrow(emission),ncol(emission)]),
                                         as.numeric(emIntensity[nrow(emIntensity),ncol(emIntensity)]))
}

write.csv(compare_landScenPerformance,paste0(resultpath,"compare_landScenPerformance.csv"))
