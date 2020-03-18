library(limSolve)
library(quadprog)
library(stringr)
library(dplyr)
library(plotly)

tOutputSeries<-read.csv("_TIN/landCalc/tOutputSeries.csv")
LU_tahun<-read.csv("_TIN/data/JaBar/LU_tahun.csv")
LDMProp_his<-read.csv("_TIN/landCalc/LDMProp.csv")
GDPAll<-readRDS("_TIN/data/JaBar/GDPAll")
sector<-readRDS("_TIN/data/JaBar/sector")
tpm_template<-read.csv("_TIN/landCalc/tpm_template.csv")
tpm_his<-read.csv("_TIN/landCalc/tpm_his.csv")
LRCRate_his<-read.csv("_TIN/landCalc/LRCRate.csv",header = FALSE)
LRCRate_2<-read.csv("_TIN/landCalc/LRCRate_2.csv",header=FALSE)
carbonStock_his<-data.matrix(read.csv("_TIN/landCalc/carbonStock.csv"))
carbonStock_his<-as.matrix(carbonStock_his[,3])
leontief <- readRDS("_TIN/data/JaBar/leontief")
findem_series<-read.csv("_TIN/landCalc/findem_series.csv",header=FALSE)



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
    LRC_0[,i]<-as.matrix(LRC_his)*as.matrix(LRCRate_his^(i-1))   #LRCRate_his nanti bisa dimodif di interface
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
  LRC_2[,i]<-as.matrix(LRC_his)*as.matrix(LRCRate_2^(i-1))    #LRCRate_2 nanti bisa dimodif di interface
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

####### hitung matriks transisi

#masukkan 0 pada matriks transisi jika total landcover = 0 
for (i in 1:nrow(tpm_template)){
  if (sum(landCover[i,])==0){
    tpm_template[i,]<-t(as.matrix(rep(0,time=ncol(tpm_template))))     #tpm_template bisa diedit di interface
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
## di shiny, reactive function thd line 151

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
xsampleResult_list<-list()

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



################################## Perhitungan Skenario intervensi #########################################


scenNumber=3


#landScen_findem<-landScen1_findem
eval(parse(text=paste0("landScen_findem<-landScen",scenNumber,"_findem")))  #landScen_findem = user input

landScen_findem_series<-as.matrix(landScen_findem+findem_series)
landScen_tOutputSeries<-leontief%*%landScen_findem_series

landScen_GDP<-matrix(NA, nrow=nrow(landScen_findem_series), ncol=ncol(landScen_findem_series))
for (i in 1:ncol(landScen_findem_series)){
  landScen_GDP[,i]<-landScen_tOutputSeries[,i]*GDPAll$P_OUTPUT
}

landScen_landReq<-matrix(NA, nrow=nrow(landTable_his), ncol=ncol(landScen_tOutputSeries))
colnames(landScen_landReq)<-colnames(landScen_tOutputSeries)

for (i in 1:ncol(landScen_tOutputSeries)){
  landScen_landReq[,i]<-diag(LRC_0[,i])%*%rbind(as.matrix(landScen_tOutputSeries[,i]),0)
  landScen_landReq[53,i]<-sum(LU_tahun[,1])-sum(landScen_landReq[1:52,i])
}

# if (length(landReq[landReq<0])>=1){
#   for (i in 1:ncol(tOutputSeries)){
#     landReq[,i]<-diag(LRC_1[,i])%*%rbind(as.matrix(tOutputSeries[,i]),0)
#     landReq[53,i]<-sum(LU_tahun[,1])-sum(landReq[1:52,i])
#   }
# }else{}
# 
if (length(landScen_landReq[landScen_landReq<0])>=1){
  for (i in 1:ncol(landScen_tOutputSeries)){
    landScen_landReq[,i]<-diag(LRC_2[,i])%*%rbind(as.matrix(landScen_tOutputSeries[,i]),0)
    landScen_landReq[53,i]<-sum(LU_tahun[,1])-sum(landScen_landReq[1:52,i])
  }
}else{}



rownames(landScen_landReq)<-c(as.matrix(sector[,1]),"sektor lainnya")



# #### buat LDM prop (proporsi terhadap sektor) ####
# LDMLuas<- as.matrix(landTable_his[,3:25])
# class(LDMLuas)<-"numeric"
# LDMProp_sektor<-t(LDMLuas)%*%solve(diag(rowSums(LDMLuas)))

##### cari land cover dari landReq yang diketahui: Land Cover<-LDMProp_sektor * LandReq
landScen_landCover_proj<-matrix(NA, nrow=nrow(LDMProp_sektor), ncol=ncol(landScen_tOutputSeries))
for (i in 1:ncol(landScen_tOutputSeries)){
  landScen_landCover_proj[,i]<-LDMProp_sektor %*%landScen_landReq[,i]
}
rownames(landScen_landCover_proj)<-colnames(LDMProp_his)


##### Masukkan land cover intervensi ######## 
# dari landCover baru yang diproyeksi berdasarkan final demand, edit sesuai dengan intervensi 
write.csv(landScen_landCover_proj,paste0("_TIN/result/landScen",scenNumber,"_landCover_proj.csv"))     # sebagai output rhandsontable di interface

#edit landScen_landCover_result, dan masukkan kembali sebagai landScen_landCover
#landScen_inputLandCover<-as.matrix(landScen_inputLandCover)   # sebagai user input
eval(parse(text=paste0("landScen_inputLandCover<-as.matrix(landScen",scenNumber,"_inputLandCover)")))  # user input
landScen_inputLandCover[is.na(landScen_inputLandCover)]<-0
landScen_landCover<-as.matrix(as.matrix(landScen_landCover_proj) + as.matrix(landScen_inputLandCover))


####### hitung matriks transisi


# masukkan input tpm_template skenario

# landScen_tpm_template<-as.matrix(landScen_tpm_template) # user input
eval(parse(text=paste0("landScen_tpm_template<-as.matrix(landScen",scenNumber,"_tpm_template)")))

#masukkan 0 pada matriks transisi jika total landcover = 0 


for (i in 1:nrow(landScen_tpm_template)){
  if (sum(landScen_landCover[i,])==0){
    landScen_tpm_template[i,]<-t(as.matrix(rep(0,time=ncol(landScen_tpm_template))))     #tpm_template bisa diedit di interface
  } else {}
}
for (i in 1:ncol(landScen_tpm_template)){
  if (sum(landScen_landCover[i,])==0){
    landScen_tpm_template[,i]<-as.matrix(rep(0,time=ncol(landScen_tpm_template)))
  } else {}
}




landScen_jumlahvariabel<-length(landScen_tpm_template[is.na(landScen_tpm_template)])
landScen_namavariabel<-paste0("x",1:length(landScen_tpm_template[is.na(landScen_tpm_template)]))
landScen_tpm_template[is.na(landScen_tpm_template)]<-landScen_namavariabel


# isi matriks transisi dengan menganggap matriks sbg system of linear equations

# solve system of linear equations dgn matriks, Ax=B

##### buat matriks koefisien (matrix_coba)
## di shiny, reactive function thd line 151

landScen_matrix_coba<-matrix(NA,nrow = 46, ncol = landScen_jumlahvariabel)
colnames(landScen_matrix_coba)<-landScen_namavariabel
landScen_variabel_x<-list()
landScen_variabel_y<-list()
for (a in 1:nrow(landScen_tpm_template)){
  landScen_variabel_x[[a]]<-t(landScen_tpm_template[a,])[t(landScen_tpm_template[a,])!= 0]
  eval(parse(text=paste0("landScen_variabel_x_",a,"<-NULL")))
  eval(parse(text=paste0("landScen_variabel_x_",a,"<-landScen_variabel_x[[",a,"]]")))
  for (i in 1:length(landScen_variabel_x[[a]])){
    if(!identical(landScen_variabel_x[[a]],c(character(0),integer(0), numeric(0)))){
      eval(parse(text=paste0("landScen_matrix_coba[",a,",paste0(landScen_variabel_x_",a,"[",i,"])]<-1")))
      # landScen_matrix_coba[a,paste0(landScen_variabel_n[i])]<-1
    } else {landScen_matrix_coba[a,]<-0}
  }
}
for (a in 1:ncol(landScen_tpm_template)){
  landScen_variabel_y[[a]]<-t(landScen_tpm_template[,a])[t(landScen_tpm_template[,a])!= 0]
  eval(parse(text=paste0("landScen_variabel_y_",a,"<-NULL")))
  eval(parse(text=paste0("landScen_variabel_y_",a,"<-landScen_variabel_y[[",a,"]]")))
  for (i in 1:length(landScen_variabel_y[[a]])){
    if(!identical(landScen_variabel_y[[a]],c(character(0),integer(0), numeric(0)))){
      eval(parse(text=paste0("landScen_matrix_coba[(23+",a,"),paste0(landScen_variabel_y_",a,"[",i,"])]<-1")))
      # landScen_matrix_coba[a,paste0(variabel_n[i])]<-1
    } else {landScen_matrix_coba[(23+a),]<-0}
  }
}

landScen_matrix_coba[is.na(landScen_matrix_coba)]<-0

#####  solve dengan metode LSEI

#input constraint
# landScen_additionalE<-landScen1_additionalE  # user input
# landScen_additionalF<-landScen1_additionalF  # user input
eval(parse(text=paste0("landScen_additionalE<-landScen",scenNumber,"_additionalE")))
eval(parse(text=paste0("landScen_additionalF<-landScen",scenNumber,"_additionalF")))


landScen_matrix_E<-landScen_matrix_coba
landScen_subset_E<- landScen_matrix_E[(!(rbind(as.matrix(landScen_landCover[,1]),as.matrix(landScen_landCover[,1]))) == 0),]

# masukkan tambahan constraint
if (is.null(landScen_subset_E)){
  landScen_subset_E<-landScen_subset_E
} else{
  landScen_subset_E<- rbind(landScen_subset_E, landScen_additionalE)
}
landScen_matrix_G=diag(nrow=landScen_jumlahvariabel)
landScen_matrix_H=matrix(rep(0, time=landScen_jumlahvariabel))
landScen_matrix_F<-list()
landScen_subset_F<-list()
landScen_lseiResult_list<-list()
landScen_lseiResult<-list()
landScen_variabel_lsei<-list()
landScen_tpm<-list()

for (i in 1:ncol(landScen_landCover)){
  if (i==1){
    landScen_tpm[[i]]<-as.matrix(tpm_his)
  } else{
    landScen_matrix_F[[i]]<-rbind(as.matrix(landScen_landCover[,i-1]), as.matrix(landScen_landCover[,i]))
    landScen_subset_F[[i]]<- as.matrix(landScen_matrix_F[[i]][!(rowSums(landScen_matrix_F[[i]]) == 0),])
    #tambahkan constraint
    if(is.null(landScen_additionalF)){
      landScen_subset_F[[i]]<-landScen_subset_F[[i]]
    }else{
      landScen_subset_F[[i]]<-rbind(landScen_subset_F[[i]],landScen_additionalF[[i]])
    }
    landScen_lseiResult_list[[i]]<-lsei(E = landScen_subset_E, F = landScen_subset_F[[i]], G=landScen_matrix_G, H=landScen_matrix_H)
    landScen_lseiResult[[i]]<-as.matrix(unlist(landScen_lseiResult_list[[i]]))
    landScen_variabel_lsei[[i]]<-as.matrix(as.numeric(landScen_lseiResult[[i]][1:landScen_jumlahvariabel,]))
    row.names(landScen_variabel_lsei[[i]])<-landScen_namavariabel
    landScen_tpm[[i]]<-as.matrix(landScen_tpm_template)
    for (a in 1:nrow(landScen_tpm[[i]])){
      for(b in 1:ncol(landScen_tpm[[i]])){
        if (landScen_tpm[[i]][a,b]==0){
          landScen_tpm[[i]][a,b]<-as.numeric(0)
        } else {landScen_tpm[[i]][a,b]<-as.numeric(landScen_variabel_lsei[[i]][paste0(landScen_tpm_template[a,b]),1])
        }
      }
    }
    landScen_tpm[[i]]<-as.matrix(landScen_tpm[[i]])
    rownames(landScen_tpm[[i]])<-colnames(LDMProp_his)
    class(landScen_tpm[[i]])<-"numeric"
  }
}

####### untuk intervensi di LUTM #########
eval(parse(text = paste0("landScen_tpmChange<-landScen",scenNumber,"_tpmChange")))

if (is.null(landScen_tpmChange)){
  landScen_tpm<-landScen_tpm
} else {
  for (i in 1:length(landScen_tpmChange)){
    landScen_tpm[[i]]<-as.matrix(landScen_tpm[[i]]+landScen_tpmChange[[i]])
  }
}

######### hitung emisi skenario ##############

landScen_emissionTable<-list()
landScen_emissionYear<-matrix(NA,nrow=nrow(landCover), ncol=ncol(landCover))

for (i in 1:length(landScen_tpm)){
  landScen_emissionTable[[i]]<-matrix(NA,nrow=nrow(landScen_tpm[[i]]), ncol=ncol(landScen_tpm[[i]]))
  for (a in 1:nrow(landScen_tpm[[i]])){
    for (b in 1:ncol(landScen_tpm[[i]])){
      landScen_emissionTable[[i]][a,b]<-as.numeric(landScen_tpm[[i]][a,b])*(carbonStock_his[b,]-carbonStock_his[a,])*3.67*-1
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

######### (1) scenario-specific dataframe of sector><simulation years depicting GDP, emission and emission intensity ########
#### for BAU
write.csv(GDP_BAU, "_TIN/result/landBAU_GDP.csv")
write.csv(emissionBAU_sector,"_TIN/result/landBAU_emission.csv")
write.csv(emIntensityBAU,"_TIN/result/landBAU_emIntensity.csv")
     
#### for scenario   
write.csv(landScen_GDP,paste0("_TIN/result/landScen",scenNumber,"_GDP.csv"))
write.csv(landScen_emission_sector,paste0("_TIN/result/landScen",scenNumber,"_emission.csv"))
write.csv(landScen_emIntensity,paste0("_TIN/result/landScen",scenNumber,"_emIntensity.csv"))


######### (2) scenario-specific dataframe of sector><simulation years depicting GDP change against BAU, emission change against BAU and emission intensity change against BAU#####

landScen_deltaGDP<-as.matrix(landScen_GDP-GDP_BAU)
landScen_deltaEmission<-as.matrix(landScen_emission_sector-emissionBAU_sector)
landScen_deltaEmIntensity<-as.matrix(landScen_emIntensity-emIntensityBAU)

write.csv(landScen_deltaGDP, paste0("_TIN/result/landScen",scenNumber,"_deltaGDP.csv"))
write.csv(landScen_deltaEmission, paste0("_TIN/result/landScen",scenNumber,"_deltaEmission.csv"))
write.csv(landScen_deltaEmIntensity, paste0("_TIN/result/landScen",scenNumber,"_deltaEmIntensity.csv"))

      
########## (3) scenario-specific dataframe from t0 - t15 depicting cumulative emission reduction, GDP change and emission intensity change #######
##### for BAU 
###GDP
BAU_totGDPYear<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(GDP_BAU)))
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
BAU_totEmIntensityYear<-cbind(as.matrix(colnames(tOutputSeries)), as.matrix(colSums(emIntensityBAU)))
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
write.csv(landScen_cumulativeGDP, paste0("_TIN/result/landScen",scenNumber,"_cumulativeGDP.csv"))
write.csv(landScen_cumulativeEmission, paste0("_TIN/result/landScen",scenNumber,"_cumulativeEmission.csv"))
write.csv(landScen_cumulativeEmIntensity, paste0("_TIN/result/landScen",scenNumber,"_cumulativeEmIntensity.csv"))

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

plot_compare_cumulativeGDP<- ggplot(data=compare_cumulativeGDP, aes(year)) + geom_line(aes(y=GDP.BAU, group=1, colour = "GDP.BAU")) + geom_line(aes(y=GDP, group=1,colour = "GDP"))
plot_compare_cumulativeEmission<-ggplot(data=compare_cumulativeEmission, aes(year)) + geom_line(aes(y=emission.BAU, group=1, colour = "emission.BAU")) + geom_line(aes(y=emission,group=2, colour = "emission"))
plot_compare_cumulativeEmIntensity<-ggplot(data=compare_cumulativeEmIntensity, aes(year)) + geom_line(aes(y=emIntensity.BAU, group=1, colour = "emIntensity.BAU")) + geom_line(aes(y=emIntensity,group=2, colour = "emIntensity"))

orca(plot_compare_cumulativeGDP, paste0("_TIN/result/landScen",scenNumber,"plot_compare_cumulativeGDP.png"))


####### (4) scenario-specific line chart of emission, GDP and emission intensity ########
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

    
plot_compare_totGDPYear<-ggplot(data=compare_totGDPYear, aes(year)) + geom_line(aes(y=GDP.BAU, group=1, colour = "GDP.BAU")) + geom_line(aes(y=GDP, group=1,colour = "GDP"))
plot_compare_totEmissionYear<-ggplot(data=compare_totEmissionYear, aes(year)) + geom_line(aes(y=emission.BAU, group=1, colour = "emission.BAU")) + geom_line(aes(y=emission,group=2, colour = "emission"))
plot_compare_totEmIntensityYear<-ggplot(data=compare_totEmIntensityYear, aes(year)) + geom_line(aes(y=emIntensity.BAU, group=1, colour = "emIntensity.BAU")) + geom_line(aes(y=emIntensity,group=2, colour = "emIntensity"))


# (5) bar chart of performance comparison across scenario at t15 on emission, GDP and emission intensity

compare_landScenPerformance<-matrix(NA, nrow=7, ncol=4)
for (i in 1:7){
  GDP<-as.matrix(read.csv(paste0("_TIN/result/landScen",i,"_cumulativeGDP.csv"), header=TRUE))
  emission<-as.matrix(read.csv(paste0("_TIN/result/landScen",i,"_cumulativeEmission.csv"), header=TRUE))
  emIntensity<-as.matrix(read.csv(paste0("_TIN/result/landScen",i,"_cumulativeEmIntensity.csv"), header=TRUE))
  compare_landScenPerformance[i,]<-cbind(paste0("landScen",i), 
                                         as.numeric(GDP[nrow(GDP),ncol(GDP)]),
                                         as.numeric(emission[nrow(emission),ncol(emission)]),
                                         as.numeric(emIntensity[nrow(emIntensity),ncol(emIntensity)]))
}

