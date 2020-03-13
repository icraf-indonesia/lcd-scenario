library(limSolve)
library(quadprog)
library(stringr)
library(dplyr)

tOutputSeries<-read.csv("landCalc/tOutputSeries.csv")
LU_tahun<-read.csv("data/JaBar/LU_tahun.csv")
LDMProp_his<-read.csv("landCalc/LDMProp.csv")
GDPAll<-readRDS("data/JaBar/GDPAll")
sector<-readRDS("data/JaBar/sector")
tpm_template<-read.csv("landCalc/tpm_template.csv")
tpm_his<-read.csv("landCalc/tpm_his.csv")
LRCRate_his<-read.csv("landCalc/LRCRate.csv",header = FALSE)
LRCRate_2<-read.csv("landCalc/LRCRate_2.csv",header=FALSE)
carbonStock_his<-data.matrix(read.csv("landCalc/carbonStock.csv"))
carbonStock_his<-as.matrix(carbonStock_his[,3])
leontief <- readRDS("data/JaBar/leontief")
findem_series<-read.csv("landCalc/findem_series.csv",header=FALSE)



############################### BAU lahan ##################################################

GDP_BAU<-matrix(NA, nrow=nrow(findem_series), ncol=ncol(findem_series))
for (i in 1:ncol(findem_series)){
  GDP_BAU[,i]<-tOutputSeries[,i]*GDPAll$P_OUTPUT
}


# untuk hitung landTable, LPC, LRC historis 

# LU_tahun<-as.data.frame(LU_tahun)
LU_tahun<-as.matrix(LU_tahun)
# LDMdimcol<-ncol(LDMProp)
# LDMdimrow<-nrow(LDMProp)
LDMProp_his<-as.matrix(LDMProp_his)
GDPAll<-as.data.frame(GDPAll)

diagLU_his<-as.matrix(diag(LU_tahun[,1]))
landTable_his<-LDMProp_his%*%diagLU_his
landReq_his<-as.matrix(rowSums(landTable_his))

LPC_his<-rbind(as.matrix(GDPAll[,4]), 0)/landReq_his
LPC_his[is.infinite(LPC_his)]<-0
LRC_his<-1/LPC_his
LRC_his[is.infinite(LRC_his)]<-0
landTable_his<-as.data.frame(cbind(rbind(as.matrix(sector[,1]), "sektor lainnya"), rbind(as.matrix(sector[,2]), "sektor lainnya"), landTable_his, landReq_his, LPC_his, LRC_his))
colnames(landTable_his)<-c("Sektor", "Kategori", colnames(LDMProp_his),"Total_Kebutuhan_Lahan", "LPC", "LRC")
tahun<-as.vector(str_extract_all(colnames(LU_tahun), '[0-9]+'))
tahun<-as.data.frame(tahun)
tahun<-t(tahun)

#### cari LRC tiap tahun proyeksi####
LRC_0<-matrix(NA, nrow=nrow(landTable_his),ncol=ncol(tOutputSeries))
for (i in 1:ncol(LRC_0)){
  if(i==1){
    LRC_0[,i]<-as.matrix(LRC_his)
  } else{
    LRC_0[,i]<-as.matrix(LRC_his)*as.matrix(LRCRate_his^(i-1))
  }
}

# LRCRate_1<-LRCRate_his
# for (i in 1:nrow(LRCRate_1)){
#   if (LRCRate_1[i,]>=1){
#     LRCRate_1[i,]<-1
#   }
# }
# 
# LRC_1<-matrix(NA, nrow=nrow(landTable_his),ncol=ncol(tOutputSeries))
# for (i in 1:ncol(LRC_1)){
#   if(i==1){
#     LRC_1[,i]<-as.matrix(LRC_his)
#   } else{
#     LRC_1[,i]<-as.matrix(LRC_his)*as.matrix(LRCRate_1^(i-1))
#   }
# }

# LRCRate_2<-LRCRate_his
# for (i in 1:nrow(LRCRate_2)){
#   if (LRCRate_2[i,]>=1){
#     # LRCRate_2[i,]<-1.001
#     LRCRate_2[i,]<-LRCRate_his[i,]*0.84
#   } else{
#     LRCRate_2[i,]<-LRCRate_his[i,]*1
#   }
# }

LRC_2<-matrix(NA, nrow=nrow(landTable_his),ncol=ncol(tOutputSeries))
for (i in 1:ncol(LRC_2)){
    LRC_2[,i]<-as.matrix(LRC_his)*as.matrix(LRCRate_2^(i-1))
}

# LRC_2<-matrix(NA, nrow=nrow(landTable_his),ncol=ncol(tOutputSeries))
# for (i in 1:ncol(LRC_2)){
#   if(i==1){
#     LRC_2[,i]<-as.matrix(LRC_his)
#   } else{
#     LRC_2[,i]<-as.matrix(LRC_his)*as.matrix(LRCRate_2^(i-1))
#   }
# }

#### cari landReq dari proyeksi output ### 
landReq<-matrix(NA, nrow=nrow(landTable_his), ncol=ncol(tOutputSeries))
colnames(landReq)<-colnames(tOutputSeries)

for (i in 1:ncol(tOutputSeries)){
  landReq[,i]<-diag(LRC_0[,i])%*%rbind(as.matrix(tOutputSeries[,i]),0)
  landReq[53,i]<-sum(LU_tahun[,1])-sum(landReq[1:52,i])
}

# if (length(landReq[landReq<0])>=1){
#   for (i in 1:ncol(tOutputSeries)){
#     landReq[,i]<-diag(LRC_1[,i])%*%rbind(as.matrix(tOutputSeries[,i]),0)
#     landReq[53,i]<-sum(LU_tahun[,1])-sum(landReq[1:52,i])
#   }
# }else{}
# 
if (length(landReq[landReq<0])>=1){
  for (i in 1:ncol(tOutputSeries)){
    landReq[,i]<-diag(LRC_2[,i])%*%rbind(as.matrix(tOutputSeries[,i]),0)
    landReq[53,i]<-sum(LU_tahun[,1])-sum(landReq[1:52,i])
  }
}else{}



rownames(landReq)<-c(as.matrix(sector[,1]),"sektor lainnya")

#### buat LDM prop (proporsi terhadap sektor) ####
LDMLuas<- as.matrix(landTable_his[,3:25])
class(LDMLuas)<-"numeric"
LDMProp_sektor<-t(LDMLuas)%*%solve(diag(rowSums(LDMLuas)))

##### cari land cover dari landReq yang diketahui: Land Cover<-LDMProp_sektor * LandReq
landCover<-matrix(NA, nrow=nrow(LDMProp_sektor), ncol=ncol(tOutputSeries))
for (i in 1:ncol(tOutputSeries)){
  landCover[,i]<-LDMProp_sektor %*%landReq[,i]
}
rownames(landCover)<-colnames(LDMProp_his)
#### LDM baru ####




####### hitung matriks transisi

#masukkan 0 pada matriks transisi jika total landcover = 0 
for (i in 1:nrow(tpm_template)){
  if (sum(landCover[i,])==0){
  tpm_template[i,]<-t(as.matrix(rep(0,time=ncol(tpm_template))))
  } else {}
}
for (i in 1:ncol(tpm_template)){
  if (sum(landCover[i,])==0){
    tpm_template[,i]<-as.matrix(rep(0,time=ncol(tpm_template)))
  } else {}
}




jumlahvariabel<-length(tpm_template[is.na(tpm_template)])
namavariabel<-paste0("x",1:length(tpm_template[is.na(tpm_template)]))
tpm_template[is.na(tpm_template)]<-namavariabel


# isi matriks transisi dengan menganggap matriks sbg system of linear equations

# solve system of linear equations dgn matriks, Ax=B

##### buat matriks koefisien (matrix_coba)

matrix_coba<-matrix(NA,nrow = 46, ncol = jumlahvariabel)
colnames(matrix_coba)<-namavariabel
variabel_x<-list()
variabel_y<-list()
for (a in 1:nrow(tpm_template)){
  variabel_x[[a]]<-t(tpm_template[a,])[t(tpm_template[a,])!= 0]
  eval(parse(text=paste0("variabel_x_",a,"<-NULL")))
  eval(parse(text=paste0("variabel_x_",a,"<-variabel_x[[",a,"]]")))
     for (i in 1:length(variabel_x[[a]])){
       if(!identical(variabel_x[[a]],c(character(0),integer(0)))){
       eval(parse(text=paste0("matrix_coba[",a,",paste0(variabel_x_",a,"[",i,"])]<-1")))
       # matrix_coba[a,paste0(variabel_n[i])]<-1
       } else {matrix_coba[a,]<-0}
     }
}
for (a in 1:ncol(tpm_template)){
  variabel_y[[a]]<-t(tpm_template[,a])[t(tpm_template[,a])!= 0]
  eval(parse(text=paste0("variabel_y_",a,"<-NULL")))
  eval(parse(text=paste0("variabel_y_",a,"<-variabel_y[[",a,"]]")))
  for (i in 1:length(variabel_y[[a]])){
    if(!identical(variabel_y[[a]],numeric(0))){
      eval(parse(text=paste0("matrix_coba[(23+",a,"),paste0(variabel_y_",a,"[",i,"])]<-1")))
      # matrix_coba[a,paste0(variabel_n[i])]<-1
    } else {matrix_coba[(23+a),]<-0}
  }
}

matrix_coba[is.na(matrix_coba)]<-0

#####  solve dengan metode LSEI


matrix_E<-matrix_coba
subset_E<- matrix_E[(!(rbind(as.matrix(landCover[,1]),as.matrix(landCover[,1]))) == 0),]
matrix_G=diag(nrow=jumlahvariabel)
matrix_H=matrix(rep(0, time=jumlahvariabel))
matrix_F<-list()
subset_F<-list()
lseiResult_list<-list()
lseiResult<-list()
variabel_lsei<-list()
tpm<-list()

for (i in 1:ncol(landCover)){
  if (i==1){
    tpm[[i]]<-as.matrix(tpm_his)
  } else{
    matrix_F[[i]]<-rbind(as.matrix(landCover[,i-1]), as.matrix(landCover[,i]))
    subset_F[[i]]<- as.matrix(matrix_F[[i]][!(rowSums(matrix_F[[i]]) == 0),])
    lseiResult_list[[i]]<-lsei(E = subset_E, F = subset_F[[i]], G=matrix_G, H=matrix_H)
    lseiResult[[i]]<-as.matrix(unlist(lseiResult_list[[i]]))
    variabel_lsei[[i]]<-as.matrix(as.numeric(lseiResult[[i]][1:jumlahvariabel,]))
    row.names(variabel_lsei[[i]])<-namavariabel
    tpm[[i]]<-as.matrix(tpm_template)
    for (a in 1:nrow(tpm[[i]])){
      for(b in 1:ncol(tpm[[i]])){
        if (tpm[[i]][a,b]==0){
          tpm[[i]][a,b]<-as.numeric(0)
        } else {tpm[[i]][a,b]<-as.numeric(variabel_lsei[[i]][paste0(tpm_template[a,b]),1])
        }
      }
    }
    tpm[[i]]<-as.matrix(tpm[[i]])
  }
}



emissionTable<-list()
emissionYear_BAU<-matrix(NA,nrow=nrow(landCover), ncol=ncol(landCover))

for (i in 1:length(tpm)){
  emissionTable[[i]]<-matrix(NA,nrow=nrow(tpm[[i]]), ncol=ncol(tpm[[i]]))
  for (a in 1:nrow(tpm[[i]])){
    for (b in 1:ncol(tpm[[i]])){
      emissionTable[[i]][a,b]<-as.numeric(tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*(-1)
      emissionYear_BAU[,i]<-as.matrix(rowSums(emissionTable[[i]]))
    }
  }
  # if (i==1){
  #   emissionTable[[i]]<-NULL
  #   emissionYear_BAU[,i]<-as.matrix(rowSums(emissionTable[[i]]))
  # }else{
  #   for (a in 1:nrow(tpm[[i]])){
  #     for (b in 1:ncol(tpm[[i]])){
  #       emissionTable[[i]][a,b]<-as.numeric(tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*(-1)
  #       emissionYear_BAU[,i]<-as.matrix(rowSums(emissionTable[[i]]))
  #      }
  #   }
  # }
}

emissionBAU<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(emissionYear_BAU)))




####################################### SKENARIO 1 ##############################################################

# Rehabilitasi area konservasi & lindung 
# lahan kritis (semak, savana, & lahan terbuka) di Hutan Lindung -> hutan sekunder lahan kering bekas tebangan
# findem: sektor 6 (), sektor 34 (), sektor 51 ()
# intervensi di TPM, perubahan lahan kritis -> hutan sekunder sebesar 2344 Ha disebar merata tiap tahun
# TPM berubah -> landCover berubah sesuai TPM -> LDM baru -> land Req baru -> 

landScen1_findem<-read.csv("landCalc/landScen1_findem.csv", header = FALSE)
landScen1_findemTot<-as.matrix(landScen1_findem+findem_series)
landScen1_output<-leontief%*%landScen1_findemTot

landScen1_GDP<-matrix(NA, nrow=nrow(landScen1_findemTot), ncol=ncol(landScen1_findemTot))
for (i in 1:ncol(landScen1_findemTot)){
  landScen1_GDP[,i]<-landScen1_output[,i]*GDPAll$P_OUTPUT
}

landScen1_landReq<-matrix(NA, nrow=nrow(landTable_his), ncol=ncol(landScen1_output))
colnames(landScen1_landReq)<-colnames(landScen1_output)

for (i in 1:ncol(landScen1_output)){
  landScen1_landReq[,i]<-diag(LRC_0[,i])%*%rbind(as.matrix(landScen1_output[,i]),0)
  landScen1_landReq[53,i]<-sum(LU_tahun[,1])-sum(landScen1_landReq[1:52,i])
}

# if (length(landReq[landReq<0])>=1){
#   for (i in 1:ncol(tOutputSeries)){
#     landReq[,i]<-diag(LRC_1[,i])%*%rbind(as.matrix(tOutputSeries[,i]),0)
#     landReq[53,i]<-sum(LU_tahun[,1])-sum(landReq[1:52,i])
#   }
# }else{}
# 
if (length(landScen1_landReq[landScen1_landReq<0])>=1){
  for (i in 1:ncol(landScen1_output)){
    landScen1_landReq[,i]<-diag(LRC_2[,i])%*%rbind(as.matrix(landScen1_output[,i]),0)
    landScen1_landReq[53,i]<-sum(LU_tahun[,1])-sum(landScen1_landReq[1:52,i])
  }
}else{}



rownames(landScen1_landReq)<-c(as.matrix(sector[,1]),"sektor lainnya")

######  buat input intervensi 1 ######

    ## Rule ##
    landScen1_tpmRule<-matrix(0, nrow=nrow(tpm_his), ncol=ncol(tpm_his))
    landScen1_tpmRule[9,2]<- landScen1_tpmRule[2,9]+(0.23148209*2344/15)
    landScen1_tpmRule[18,2]<- landScen1_tpmRule[2,18]+(0.76851791*2344/15)
    landScen1_tpmRule[12,18]<-landScen1_tpmRule[2,18]-(2344/15/2)
    landScen1_tpmRule[12,9]<-landScen1_tpmRule[2,18]-(2344/15/2)
    
    #### tpm skenario 1 ####
    
    landScen1_tpm<-list()
    for (i in 1:length(tpm)){
      if (i==1){
        landScen1_tpm[[i]]<-tpm[[i]]
      }else{
        tpm[[i]]<-matrix(as.numeric(unlist(tpm[[i]])),nrow=nrow(tpm[[i]]))
        landScen1_tpm[[i]]<-as.matrix(tpm[[i]]+landScen1_tpmRule)
      }
    }



######### hitung emisi skenario 1 ##############

landScen1_emissionTable<-list()
landScen1_emissionYear<-matrix(NA,nrow=nrow(landCover), ncol=ncol(landCover))

for (i in 1:length(landScen1_tpm)){
  landScen1_emissionTable[[i]]<-matrix(NA,nrow=nrow(landScen1_tpm[[i]]), ncol=ncol(landScen1_tpm[[i]]))
  for (a in 1:nrow(landScen1_tpm[[i]])){
    for (b in 1:ncol(landScen1_tpm[[i]])){
      landScen1_emissionTable[[i]][a,b]<-as.numeric(landScen1_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
    }
  }
  landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # if (i==1){
  #   landScen1_emissionTable[[i]]<-emissionTable[[i]]
  #   landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # }else{
  #   for (a in 1:nrow(landScen1_tpm[[i]])){
  #     for (b in 1:ncol(landScen1_tpm[[i]])){
  #       landScen1_emissionTable[[i]][a,b]<-as.numeric(landScen1_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
  #     }
  #   }
  #   landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # }
}
landScen1_emission<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen1_emissionYear)))




####################################### SKENARIO 2 ##############################################################
# Rehabilitasi lahan dengan skema hutan tanaman masyarakat 
# lahan kritis (semak, savana, & lahan terbuka) di Hutan Produksi -> agroforest ()
# 
# intervensi di TPM, perubahan lahan kritis -> kebun campur sebesar 5713 Ha disebar merata tiap tahun

landScen2_findem<-read.csv("landCalc/landScen1_findem.csv", header = FALSE)
landScen2_findemTot<-as.matrix(landScen2_findem+findem_series)
landScen2_output<-leontief%*%landScen2_findemTot

landScen2_GDP<-matrix(NA, nrow=nrow(landScen2_findemTot), ncol=ncol(landScen2_findemTot))
for (i in 1:ncol(landScen2_findemTot)){
  landScen2_GDP[,i]<-landScen2_output[,i]*GDPAll$P_OUTPUT
}

landScen2_landReq<-matrix(NA, nrow=nrow(landTable_his), ncol=ncol(landScen2_output))
colnames(landScen2_landReq)<-colnames(landScen2_output)

###### buat input intervensi 2 #######

## Rule ##
landScen2_tpmRule<-matrix(0, nrow=nrow(tpm_his), ncol=ncol(tpm_his))
landScen2_tpmRule[9,13]<- landScen2_tpmRule[13,9]+(0.23148209*5713/15)
landScen2_tpmRule[18,13]<- landScen2_tpmRule[13,18]+(0.76851791*5713/15)
landScen2_tpmRule[12,18]<-landScen2_tpmRule[2,18]-(5713/15/2)
landScen2_tpmRule[12,9]<-landScen2_tpmRule[2,18]-(5713/15/2)

#### tpm skenario 2 ####

landScen2_tpm<-list()
for (i in 1:length(tpm)){
  if (i==1){
    landScen2_tpm[[i]]<-tpm[[i]]
  }else{
    tpm[[i]]<-matrix(as.numeric(unlist(tpm[[i]])),nrow=nrow(tpm[[i]]))
    landScen2_tpm[[i]]<-as.matrix(tpm[[i]]+landScen2_tpmRule)
  }
}



######### hitung emisi skenario 2 ##########

landScen2_emissionTable<-list()
landScen2_emissionYear<-matrix(NA,nrow=nrow(landCover), ncol=ncol(landCover))

for (i in 1:length(landScen2_tpm)){
  landScen2_emissionTable[[i]]<-matrix(NA,nrow=nrow(landScen2_tpm[[i]]), ncol=ncol(landScen2_tpm[[i]]))
  for (a in 1:nrow(landScen2_tpm[[i]])){
    for (b in 1:ncol(landScen2_tpm[[i]])){
      landScen2_emissionTable[[i]][a,b]<-as.numeric(landScen2_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
    }
  }
  landScen2_emissionYear[,i]<-as.matrix(rowSums(landScen2_emissionTable[[i]]))
  # if (i==1){
  #   landScen1_emissionTable[[i]]<-emissionTable[[i]]
  #   landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # }else{
  #   for (a in 1:nrow(landScen1_tpm[[i]])){
  #     for (b in 1:ncol(landScen1_tpm[[i]])){
  #       landScen1_emissionTable[[i]][a,b]<-as.numeric(landScen1_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
  #     }
  #   }
  #   landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # }
}
landScen2_emission<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen2_emissionYear)))



####################################### SKENARIO 3 ##############################################################
# Mengurangi perubahan lahan hutan menjadi lahan kritis

landScen3_findem<-read.csv("landCalc/landScen3_findem.csv", header = FALSE)
landScen3_findemTot<-as.matrix(landScen3_findem+findem_series)
landScen3_output<-leontief%*%landScen3_findemTot

landScen3_GDP<-matrix(NA, nrow=nrow(landScen3_findemTot), ncol=ncol(landScen3_findemTot))
for (i in 1:ncol(landScen3_findemTot)){
  landScen3_GDP[,i]<-landScen3_output[,i]*GDPAll$P_OUTPUT
}

landScen3_landReq<-matrix(NA, nrow=nrow(landTable_his), ncol=ncol(landScen3_output))
colnames(landScen3_landReq)<-colnames(landScen3_output)

#### buat input intervensi 3 #####

## Rule ##
landScen3_tpmChange<-matrix(1, nrow=nrow(tpm_his), ncol=ncol(tpm_his))
landScen3_tpmChange[1,2:23]<- 0.03
landScen3_tpmChange[2,3:23]<- 0.03

landScen3_tpmRule<-list()
for (i in 1:length(tpm)){
  landScen3_tpmRule[[i]]<-matrix(0, nrow=nrow(tpm_his), ncol=ncol(tpm_his))
  if(i==1){
    landScen3_tpmRule[[i]]<-matrix(0, nrow=nrow(tpm_his), ncol=ncol(tpm_his))
  } else{
    for (a in 1:2){
      for (b in 1:ncol(landScen3_tpmChange)){
        landScen3_tpmRule[[i]][a,b]<-tpm[[i]][a,b]*landScen3_tpmChange[a,b]*(-1)
      }
    }
  }
}
for (i in 1:length(landScen3_tpmRule)){
  if(i!=1){
    if(sum(tpm[[i]][1,2:23])!=0 & sum(tpm[[i]][2,3:23])!=0){
      landScen3_tpmRule[[i]][1,1]<-sum(landScen3_tpmRule[[i]][1,2:23])*(-1)
      landScen3_tpmRule[[i]][2,2]<-sum(landScen3_tpmRule[[i]][2,3:23])*(-1)
    } else if (sum(tpm[[i]][1,2:23])==0 & sum(tpm[[i]][2,3:23])!=0){
      landScen3_tpmRule[[i]][1,1]<-0
      landScen3_tpmRule[[i]][2,2]<-sum(landScen3_tpmRule[[i]][2,3:23])*(-1)
    } else if (sum(tpm[[i]][1,2:23])!=0 & sum(tpm[[i]][2,3:23])==0){
      landScen3_tpmRule[[i]][1,1]<-sum(landScen3_tpmRule[[i]][1,2:23])*(-1)
      landScen3_tpmRule[[i]][2,2]<-0
    } else{}
  }
  else{}
}





#### tpm skenario 3 ####

landScen3_tpm<-list()
for (i in 1:length(tpm)){
  if (i==1){
    landScen3_tpm[[i]]<-tpm[[i]]
  }else{
    tpm[[i]]<-matrix(as.numeric(unlist(tpm[[i]])),nrow=nrow(tpm[[i]]))
    landScen3_tpm[[i]]<-as.matrix(tpm[[i]]+landScen3_tpmRule[[i]])
  }
}



######### hitung emisi skenario 3 ##############

landScen3_emissionTable<-list()
landScen3_emissionYear<-matrix(NA,nrow=nrow(landCover), ncol=ncol(landCover))

for (i in 1:length(landScen3_tpm)){
  landScen3_emissionTable[[i]]<-matrix(NA,nrow=nrow(landScen3_tpm[[i]]), ncol=ncol(landScen3_tpm[[i]]))
  for (a in 1:nrow(landScen3_tpm[[i]])){
    for (b in 1:ncol(landScen3_tpm[[i]])){
      landScen3_emissionTable[[i]][a,b]<-as.numeric(landScen3_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
    }
  }
  landScen3_emissionYear[,i]<-as.matrix(rowSums(landScen3_emissionTable[[i]]))
  # if (i==1){
  #   landScen1_emissionTable[[i]]<-emissionTable[[i]]
  #   landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # }else{
  #   for (a in 1:nrow(landScen1_tpm[[i]])){
  #     for (b in 1:ncol(landScen1_tpm[[i]])){
  #       landScen1_emissionTable[[i]][a,b]<-as.numeric(landScen1_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
  #     }
  #   }
  #   landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # }
}
landScen3_emission<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen3_emissionYear)))






################### combination of scenarios: 1, 2, & 3 #######################

landScen123_findem<-as.matrix(landScen1_findem+landScen2_findem+landScen3_findem)
landScen123_findemTot<-as.matrix(landScen123_findem+findem_series)
landScen123_output<-leontief%*%landScen2_findemTot

landScen123_GDP<-matrix(NA, nrow=nrow(landScen123_findemTot), ncol=ncol(landScen123_findemTot))
for (i in 1:ncol(landScen123_findemTot)){
  landScen123_GDP[,i]<-landScen123_output[,i]*GDPAll$P_OUTPUT
}

landScen123_landReq<-matrix(NA, nrow=nrow(landTable_his), ncol=ncol(landScen123_output))
colnames(landScen123_landReq)<-colnames(landScen123_output)

##### Buat rule ###

landScen123_tpmRule<-list()
for (i in 1:length(tpm)){
  landScen123_tpmRule[[i]]<-matrix(0, nrow=nrow(tpm_his),ncol=ncol(tpm_his))
  if(i==1){}
  else{
    landScen123_tpmRule[[i]]<-as.matrix(landScen1_tpmRule+landScen2_tpmRule+landScen3_tpmRule[[i]])
  }
}

#### tpm kombinasi skenario 1,2,3 ####

landScen123_tpm<-list()
for (i in 1:length(tpm)){
  if (i==1){
    landScen123_tpm[[i]]<-tpm[[i]]
  }else{
    tpm[[i]]<-matrix(as.numeric(unlist(tpm[[i]])),nrow=nrow(tpm[[i]]))
    landScen123_tpm[[i]]<-as.matrix(tpm[[i]]+landScen123_tpmRule[[i]])
  }
}

######### hitung emisi kombinasi skenario 1,2,3 ##############

landScen123_emissionTable<-list()
landScen123_emissionYear<-matrix(NA,nrow=nrow(landCover), ncol=ncol(landCover))

for (i in 1:length(landScen123_tpm)){
  landScen123_emissionTable[[i]]<-matrix(NA,nrow=nrow(landScen123_tpm[[i]]), ncol=ncol(landScen123_tpm[[i]]))
  for (a in 1:nrow(landScen123_tpm[[i]])){
    for (b in 1:ncol(landScen123_tpm[[i]])){
      landScen123_emissionTable[[i]][a,b]<-as.numeric(landScen123_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
    }
  }
  landScen123_emissionYear[,i]<-as.matrix(rowSums(landScen123_emissionTable[[i]]))
  # if (i==1){
  #   landScen1_emissionTable[[i]]<-emissionTable[[i]]
  #   landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # }else{
  #   for (a in 1:nrow(landScen1_tpm[[i]])){
  #     for (b in 1:ncol(landScen1_tpm[[i]])){
  #       landScen1_emissionTable[[i]][a,b]<-as.numeric(landScen1_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
  #     }
  #   }
  #   landScen1_emissionYear[,i]<-as.matrix(rowSums(landScen1_emissionTable[[i]]))
  # }
}
landScen123_emission<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen123_emissionYear)))



############################# write data #################################################


###### tabel emisi tiap tahun u/ tiap tutupan laha n######
write.csv(emissionYear_BAU,"landCalc/landCalc_Data/landCalc.csv")
write.csv(landScen1_emissionYear,"landCalc/landCalc_Data/landScen1_emissionYear.csv")
write.csv(landScen2_emissionYear,"landCalc/landCalc_Data/landScen2_emissionYear.csv")
write.csv(landScen3_emissionYear,"landCalc/landCalc_Data/landScen3_emissionYear.csv")
write.csv(landScen123_emissionYear,"landCalc/landCalc_Data/landScen123_emissionYear.csv")


###### tabel GDP tiap tahun per sektor ######
write.csv(GDP_BAU,"landCalc/landCalc_Data/GDP_BAU.csv")
write.csv(landScen1_GDP,"landCalc/landCalc_Data/landScen1_GDP.csv")
write.csv(landScen2_GDP,"landCalc/landCalc_Data/landScen2_GDP.csv")
write.csv(landScen3_GDP,"landCalc/landCalc_Data/landScen3_GDP.csv")
write.csv(landScen123_GDP,"landCalc/landCalc_Data/landScen123_GDP.csv")


###### tabel total emisi sektor lahan per tahun ######
write.csv(emissionBAU,"landCalc/landCalc_Data/emissionBAU.csv")
write.csv(landScen1_emission,"landCalc/landCalc_Data/landScen1_emission.csv")
write.csv(landScen2_emission,"landCalc/landCalc_Data/landScen2_emission.csv")
write.csv(landScen3_emission,"landCalc/landCalc_Data/landScen3_emission.csv")
write.csv(landScen123_emission,"landCalc/landCalc_Data/landScen123_emission.csv")

###### tabel total GDP per tahun #######
GDP_BAU_tot <- cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(GDP_BAU)))
landScen1_GDP_tot<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen1_GDP)))
landScen2_GDP_tot<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen1_GDP)))
landScen3_GDP_tot<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen1_GDP)))
landScen123_GDP_tot<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(landScen1_GDP)))
write.csv(GDP_BAU_tot,"landCalc/landCalc_Data/GDP_BAU_tot.csv")
write.csv(landScen1_GDP_tot,"landCalc/landCalc_Data/landScen1_GDP_tot.csv")
write.csv(landScen2_GDP_tot,"landCalc/landCalc_Data/landScen2_GDP_tot.csv")
write.csv(landScen3_GDP_tot,"landCalc/landCalc_Data/landScen3_GDP_tot.csv")
write.csv(landScen123_GDP_tot,"landCalc/landCalc_Data/landScen123_GDP_tot.csv")

###### tabel total emission intensity per tahun ########
emIntensity_BAU<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(as.numeric(emissionBAU[,2])/as.numeric(GDP_BAU_tot[,2])))
landScen1_emIntensity<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(as.numeric(landScen1_emission[,2])/as.numeric(landScen1_GDP_tot[,2])))
landScen2_emIntensity<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(as.numeric(landScen2_emission[,2])/as.numeric(landScen2_GDP_tot[,2])))
landScen3_emIntensity<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(as.numeric(landScen3_emission[,2])/as.numeric(landScen3_GDP_tot[,2])))
landScen123_emIntensity<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(as.numeric(landScen123_emission[,2])/as.numeric(landScen123_GDP_tot[,2])))
write.csv(emIntensity_BAU,"landCalc/landCalc_Data/emIntensity_BAU.csv")
write.csv(landScen1_emIntensity,"landCalc/landCalc_Data/landScen1_emIntensity.csv")
write.csv(landScen2_emIntensity,"landCalc/landCalc_Data/landScen2_emIntensity.csv")
write.csv(landScen3_emIntensity,"landCalc/landCalc_Data/landScen3_emIntensity.csv")
write.csv(landScen123_emIntensity,"landCalc/landCalc_Data/landScen123_emIntensity.csv")
