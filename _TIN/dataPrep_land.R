###################################################################################################################
#
#                                              JABAR
#
#################################################################################################################


selectedProv="SumSel"
scenpath <- paste0("_TIN/landCalc/", selectedProv, "/landScen", scenNumber,"_")

########################## skenario intervensi 1 ###########################
########################## SKENARIO 1 ######################################

# Rehabilitasi area konservasi & lindung 
# lahan kritis (semak, savana, & lahan terbuka) di Hutan Lindung -> hutan sekunder lahan kering bekas tebangan
# findem: sektor 6 (), sektor 34 (), sektor 51 ()
# intervensi di TPM, perubahan lahan kritis -> hutan sekunder sebesar 2344 Ha disebar merata tiap tahun
# TPM berubah -> landCover berubah sesuai TPM -> LDM baru -> land Req baru -> 

landScen1_findem <-read.csv(paste0(scenpath,"findem.csv"), header = FALSE)
landScen1_inputLandCover<-read.csv(paste0(scenpath,"inputLandCover.csv"), header=FALSE)
landScen1_LUTM_template<-read.csv(paste0(scenpath,"LUTM_template.csv"), header=TRUE)

### make additional constraint for matrix E & matrix F
# perubahan semak menjadi hutan sekunder sebanyak 0.23148209*2344/15= X5 = 0.23148209*2344/15
# perubahan lahan terbuka menjadi hutan sekunder 0.76851791*2344/15 = X7 = 0.76851791*2344/15
landScen1_additionalE<-matrix(0, nrow=2,ncol=131)   #131= jumlahvariabel
landScen1_additionalE[1,5]<-1  #for x5
landScen1_additionalE[2,7]<-1  #for x7
landScen1_additionalF<-list()
for (i in 1:ncol(landScen1_findem)){
  if (i==1){
    landScen1_additionalF[[i]]<-matrix(c(0,0), nrow=2, ncol=1)
  }else{
    landScen1_additionalF[[i]]<-matrix(c(0.23148209*2344/15, 0.76851791*2344/15), nrow=2, ncol=1)
  }
}

landScen3_additionalG<-NULL
landScen3_additionalH<-NULL
landScen1_LUTMChange<-NULL



######################## skenario 2 ###########################
# Rehabilitasi lahan dengan skema hutan tanaman masyarakat 
# lahan kritis (semak, savana, & lahan terbuka) di Hutan Produksi -> agroforest ()
# intervensi di TPM, perubahan lahan kritis -> kebun campur sebesar 5713 Ha disebar merata tiap tahun
selectedProv="JaBar"
scenNumber<-2
scenpath <- paste0("_TIN/landCalc/", selectedProv, "/landScen", scenNumber,"_")

landScen2_findem <-read.csv(paste0(scenpath,"findem.csv"), header = FALSE)
landScen2_inputLandCover<-read.csv(paste0(scenpath,"inputLandCover.csv"), header=FALSE)
landScen2_LUTM_template<-read.csv(paste0(datapath,"LUTM_template.csv"), header=TRUE)

### make additional constraint for matrix E & matrix F
# perubahan semak menjadi pertanian lahan kering campur sebanyak 0.23148209*5713/15= X56 = 0.23148209*5713/15
# perubahan lahan terbuka menjadi pertanian lahan kering campur 0.76851791*5713/15 = X61 = 0.76851791*5713/15
landScen2_additionalG<-matrix(0,nrow=2, ncol=130)   #130 = jumlahvariabel
landScen2_additionalG[1,56]<-1  #for x56
landScen2_additionalG[2,61]<-1  #for X61
landScen2_additionalH<-list()
for(i in 1:ncol(landScen2_findem)){
  if(i==1){
    landScen2_additionalH[[i]]<-matrix(c(0,0), nrow=2, ncol=1)  
  }else{
    landScen2_additionalH[[i]]<-matrix(c((0.23148209*5713/15)+variabel_lsei[[i]][56,1], (0.76851791*5713/15)+variabel_lsei[[i]][61,1]), nrow=2, ncol=1)
  }
}

landScen3_additionalE<-NULL
landScen3_additionalF<-NULL
landScen2_LUTMChange<-NULL

####################################### SKENARIO 3 ##############################################################
# Mengurangi perubahan lahan hutan menjadi lahan kritis
landScen3_findem<-read.csv(paste0(scenpath,"findem.csv"), header = FALSE)
landScen3_inputLandCover<-matrix(0,nrow=23, ncol=16)
landScen3_LUTM_template<-read.csv(paste0(scenpath,"LUTM_template.csv"), header=TRUE)

### change landScen_LUTM

landScen3_additionalE<-NULL
landScen3_additionalF<-NULL
landScen3_additionalG<-NULL
landScen3_additionalH<-NULL

landScen3_LUTMRule<-read.csv(paste0(scenpath,"landScen3_tpmRule.csv"), header = TRUE)
landScen3_LUTMChange<-list()

for (i in 1:length(LUTM)){
  class(LUTM[[i]])<-"numeric"
  if (i==1){
    landScen3_LUTMChange[[i]]<-matrix(0, nrow=nrow(LUTM[[i]]), ncol=ncol(LUTM[[i]]) )
  } else{
    landScen3_LUTMChange[[i]]<-as.matrix(landScen3_LUTMRule * LUTM[[i]]*-1)
    landScen3_LUTMChange[[i]][1,1]<--sum(landScen3_LUTMChange[[i]][1,])
    landScen3_LUTMChange[[i]][2,2]<--sum(landScen3_LUTMChange[[i]][2,])
    landScen3_LUTMChange[[i]][5,5]<--sum(landScen3_LUTMChange[[i]][5,])
    landScen3_LUTMChange[[i]][6,6]<--sum(landScen3_LUTMChange[[i]][6,])
  }
}

######################### combination of scenario 1 & 2 ##############################
landScen4_findem <-landScen1_findem+landScen2_findem
landScen4_inputLandCover<-landScen1_inputLandCover+landScen2_inputLandCover
landScen4_LUTM_template<-landScen1_LUTM_template

### make additional constraint for matrix E & matrix F
# perubahan semak menjadi hutan sekunder sebanyak 0.23148209*2344/15= X5 = 0.23148209*2344/15
# perubahan lahan terbuka menjadi hutan sekunder 0.76851791*2344/15 = X7 = 0.76851791*2344/15
landScen4_additionalE<-matrix(0, nrow=4,ncol=131)   #131= jumlahvariabel
landScen4_additionalE[1,5]<-1  #for x5
landScen4_additionalE[2,7]<-1  #for x7
landScen4_additionalE[3,57]<-1  #for x57
landScen4_additionalE[4,62]<-1  #for X62

landScen4_additionalF<-list()
for (i in 1:ncol(landScen1_findem)){
  if (i==1){
    landScen4_additionalF[[i]]<-matrix(0, nrow=4, ncol=1)
  }else{
    landScen4_additionalF[[i]]<-rbind(landScen1_additionalF[[i]],landScen2_additionalF[[i]])
  }
}

landScen4_LUTMChange<-NULL

######################### combination of scenario 1 & 3 ##############################
landScen5_findem <-landScen1_findem+landScen3_findem
landScen5_inputLandCover<-landScen1_inputLandCover+landScen3_inputLandCover
landScen5_LUTM_template<-landScen1_LUTM_template

### make additional constraint for matrix E & matrix F
# perubahan semak menjadi hutan sekunder sebanyak 0.23148209*2344/15= X5 = 0.23148209*2344/15
# perubahan lahan terbuka menjadi hutan sekunder 0.76851791*2344/15 = X7 = 0.76851791*2344/15
landScen5_additionalE<-landScen1_additionalE

landScen5_additionalF<-list()
for (i in 1:ncol(landScen5_findem)){
  if (i==1){
    landScen5_additionalF[[i]]<-matrix(0, nrow=2, ncol=1)
  }else{
    landScen5_additionalF[[i]]<-landScen1_additionalF[[i]]
  }
}

landScen5_LUTMChange<-landScen3_LUTMChange

######################### combination of scenario 2 & 3 ##############################
landScen6_findem <-landScen2_findem+landScen3_findem
landScen6_inputLandCover<-landScen2_inputLandCover+landScen3_inputLandCover
landScen6_LUTM_template<-landScen2_LUTM_template

### make additional constraint for matrix E & matrix F
# perubahan semak menjadi hutan sekunder sebanyak 0.23148209*2344/15= X5 = 0.23148209*2344/15
# perubahan lahan terbuka menjadi hutan sekunder 0.76851791*2344/15 = X7 = 0.76851791*2344/15
landScen6_additionalE<-landScen2_additionalE

landScen6_additionalF<-list()
for (i in 1:ncol(landScen6_findem)){
  if (i==1){
    landScen6_additionalF[[i]]<-matrix(0, nrow=2, ncol=1)
  }else{
    landScen6_additionalF[[i]]<-landScen2_additionalF[[i]]
  }
}

landScen6_LUTMChange<-landScen3_LUTMChange


######################### combination of scenario 1, 2 & 3 ##############################
landScen7_findem <-landScen1_findem+landScen2_findem+landScen3_findem
landScen7_inputLandCover<-landScen1_inputLandCover+landScen2_inputLandCover+landScen3_inputLandCover
landScen7_LUTM_template<-landScen1_LUTM_template

### make additional constraint for matrix E & matrix F
# perubahan semak menjadi hutan sekunder sebanyak 0.23148209*2344/15= X5 = 0.23148209*2344/15
# perubahan lahan terbuka menjadi hutan sekunder 0.76851791*2344/15 = X7 = 0.76851791*2344/15
landScen7_additionalE<-matrix(0, nrow=4,ncol=131)   #131= jumlahvariabel
landScen7_additionalE[1,5]<-1  #for x5
landScen7_additionalE[2,7]<-1  #for x7
landScen7_additionalE[3,57]<-1  #for x57
landScen7_additionalE[4,62]<-1  #for X62

landScen7_additionalF<-list()
for (i in 1:ncol(landScen7_findem)){
  if (i==1){
    landScen7_additionalF[[i]]<-matrix(0, nrow=4, ncol=1)
  }else{
    landScen7_additionalF[[i]]<-rbind(landScen1_additionalF[[i]],landScen2_additionalF[[i]])
  }
}

landScen7_LUTMChange<-landScen3_LUTMChange



################################ skenario CPO mba Dewi ############################################
landScen8_findem <-landScen1_findem+landScen2_findem+landScen3_findem+as.matrix(read.csv("_TIN/landCalc/landScen8_findem.csv", header = TRUE))
landScen8_inputLandCover<-landScen1_inputLandCover+landScen2_inputLandCover+landScen3_inputLandCover
landScen8_LUTM_template<-landScen1_LUTM_template

### make additional constraint for matrix E & matrix F
# perubahan semak menjadi hutan sekunder sebanyak 0.23148209*2344/15= X5 = 0.23148209*2344/15
# perubahan lahan terbuka menjadi hutan sekunder 0.76851791*2344/15 = X7 = 0.76851791*2344/15
landScen8_additionalE<-matrix(0, nrow=4,ncol=131)   #131= jumlahvariabel
landScen8_additionalE[1,5]<-1  #for x5
landScen8_additionalE[2,7]<-1  #for x7
landScen8_additionalE[3,57]<-1  #for x57
landScen8_additionalE[4,62]<-1  #for X62

landScen8_additionalF<-list()
for (i in 1:ncol(landScen8_findem)){
  if (i==1){
    landScen8_additionalF[[i]]<-matrix(0, nrow=4, ncol=1)
  }else{
    landScen8_additionalF[[i]]<-rbind(landScen1_additionalF[[i]],landScen2_additionalF[[i]])
  }
}

landScen8_LUTMChange<-landScen3_LUTMChange


###################################################################################################################
#
#                                              SUMSEL
#
#################################################################################################################

selectedProv="SumSel"
scenNumber=1
scenpath <- paste0("_TIN/landCalc/", selectedProv, "/landScen", scenNumber,"_")
sector<-readRDS(paste0(datapath, "sector"))

########################## skenario intervensi 1 ###########################
########################## SKENARIO 1 ######################################
# aksi mitigasi: rehabilitasi hutan dan lahan
# Rehabilitasi area konservasi & lindung 
# lahan kritis (semak, savana, & lahan terbuka) di Hutan Lindung -> hutan sekunder lahan kering bekas tebangan
# findem: sektor kehutanan, angkutan darat, kegiatan sosial
# intervensi di TPM, perubahan lahan kritis -> hutan sekunder sebesar 2344 Ha disebar merata tiap tahun
# TPM berubah -> landCover berubah sesuai TPM -> LDM baru -> land Req baru -> 

landScen1_findem <-read.csv(paste0(scenpath,"findem.csv"), header = FALSE)
landScen1_inputLandCover<-read.csv(paste0(scenpath,"inputLandCover.csv"), header=FALSE)
landScen1_LUTM_template<-read.csv(paste0(scenpath,"LUTM_template.csv"), header=TRUE)


landScen1_additionalE<-NULL
landScen1_additionalF<-NULL
### make additional constraint for matrix E & matrix F
# perubahan semak menjadi hutan sekunder sebanyak 1971.4 = X5
# perubahan savanna menjadi hutan sekunder 23.95 = X7
# perubahan lahan terbuka menjadi hutan sekunder 956.8 = x9
landScen1_additionalG<-matrix(0, nrow=3,ncol=163)   #161= jumlahvariabel
landScen1_additionalG[1,5]<-1  #for x5
landScen1_additionalG[2,7]<-1  #for x7
landScen1_additionalG[3,9]<-1  #for x9
landScen1_additionalH<-list()
for (i in 1:ncol(landScen1_findem)){
  if (i==1){
    landScen1_additionalH[[i]]<-matrix(c(0,0,0), nrow=3, ncol=1)
  }else{
    landScen1_additionalH[[i]]<-matrix(c(1971.4, 23.95,956.8), nrow=3, ncol=1)
  }
}

landScen1_LUTMChange<-NULL


########################## SKENARIO 2 ######################################
# pengembangan hutan rakyat
# Rehabilitasi lahan dengan skema hutan tanaman masyarakat 
# lahan kritis (semak, savana, & lahan terbuka) di Hutan Produksi -> agroforest ()
# intervensi di TPM, perubahan lahan kritis -> kebun campur sebesar 5713 Ha disebar merata tiap tahun
landScen2_findem <-read.csv(paste0(scenpath,"findem.csv"), header = FALSE)
landScen2_inputLandCover<-read.csv(paste0(scenpath,"inputLandCover.csv"), header=FALSE)
landScen2_LUTM_template<-read.csv(paste0(scenpath,"LUTM_template.csv"), header=TRUE)

### make additional constraint for matrix G & matrix H
# perubahan semak menjadi pertanian lahan kering campur sebanyak 8584 ha = X75
# perubahan savanna menjadi pertanian lahan kering campur sebanyak 585.1 = X78
# perubahan lahan terbuka menjadi pertanian lahan kering campur 22020.05 = X82

landScen2_additionalG<-matrix(0,nrow=3, ncol=162)   #130 = jumlahvariabel
landScen2_additionalG[1,75]<-1  #for x75
landScen2_additionalG[2,78]<-1  #for X78
landScen2_additionalG[3,82]<-1  #for X82

landScen2_additionalH<-list()
for(i in 1:ncol(landScen2_findem)){
  if(i==1){
    landScen2_additionalH[[i]]<-matrix(c(0,0,0), nrow=3, ncol=1)  
  }else{
    landScen2_additionalH[[i]]<-matrix(c(8584, 585.1, 22020.05),
                                       # (8584)+variabel_lsei[[i]][75,1], 
                                       #   (585.1)+variabel_lsei[[i]][78,1],
                                       #   (22020.05)+variabel_lsei[[i]][82,1]),
                                       nrow=3, ncol=1)
  }
}

landScen2_additionalE<-NULL
landScen2_additionalF<-NULL
landScen2_LUTMChange<-NULL



###################################################################################################################
#
#                                              SULAWESI SELATAN
#
#################################################################################################################


########################## skenario intervensi 1 ###########################
########################## SKENARIO 1 ######################################
# aksi mitigasi: rehabilitasi hutan dan lahan
# Rehabilitasi area konservasi & lindung 
# lahan kritis (semak, savana, & lahan terbuka) di Hutan Lindung -> hutan sekunder lahan kering bekas tebangan
# findem: sektor perkebunan (19), angkutan darat (96), Jasa Kemasyarakatan Lainnya  (109)
# intervensi di TPM, perubahan lahan kritis -> hutan sekunder sebesar 2344 Ha disebar merata tiap tahun
# TPM berubah -> landCover berubah sesuai TPM -> LDM baru -> land Req baru -> 

selectedProv="Sulawesi_Selatan"
scenNumber=1
scenpath <- paste0("_TIN/landCalc/", selectedProv, "/landScen", scenNumber,"_")

landScen1_findem <-read.csv(paste0(scenpath,"findem.csv"), header = FALSE)
landScen1_inputLandCover<-read.csv(paste0(scenpath,"inputLandCover.csv"), header=FALSE)
landScen1_LUTM_template<-read.csv(paste0(scenpath,"LUTM_template.csv"), header=TRUE)


landScen1_additionalE<-NULL
landScen1_additionalF<-NULL
### make additional constraint for matrix E & matrix F
# perubahan semak menjadi hutan sekunder sebanyak 7177.714286 = X4
# perubahan savanna menjadi hutan sekunder 2091.52381 = X6
# perubahan lahan terbuka menjadi hutan sekunder 133.1904762 = x8
landScen1_additionalG<-matrix(0, nrow=3,ncol=154)   #156= jumlahvariabel
landScen1_additionalG[1,4]<-1  #for x4
landScen1_additionalG[2,6]<-1  #for x6
landScen1_additionalG[3,8]<-1  #for x8
landScen1_additionalH<-list()
for (i in 1:ncol(landScen1_findem)){
  if (i==1){
    landScen1_additionalH[[i]]<-matrix(c(0,0,0), nrow=3, ncol=1)
  }else{
    landScen1_additionalH[[i]]<-matrix(c(7177.714286, 2091.52381,133.1904762), nrow=3, ncol=1)
  }
}

landScen1_LUTMChange<-NULL


########################## SKENARIO 2 ######################################
# pengembangan hutan rakyat
# Rehabilitasi lahan dengan skema hutan tanaman masyarakat 
# lahan kritis (semak, savana, & lahan terbuka) di Hutan Produksi -> agroforest ()
# intervensi di TPM, perubahan lahan kritis -> kebun campur sebesar 5713 Ha disebar merata tiap tahun

selectedProv="Sulawesi_Selatan"
scenNumber=2
scenpath <- paste0("_TIN/landCalc/", selectedProv, "/landScen", scenNumber,"_")

landScen2_findem <-read.csv(paste0(scenpath,"findem.csv"), header = FALSE)
landScen2_inputLandCover<-read.csv(paste0(scenpath,"inputLandCover.csv"), header=FALSE)
landScen2_LUTM_template<-read.csv(paste0(scenpath,"LUTM_template.csv"), header=TRUE)

### make additional constraint for matrix G & matrix H
# perubahan semak menjadi pertanian lahan kering campur sebanyak 8584 ha = X75
# perubahan savanna menjadi pertanian lahan kering campur sebanyak 585.1 = X78
# perubahan lahan terbuka menjadi pertanian lahan kering campur 22020.05 = X82

landScen2_additionalG<-matrix(0,nrow=3, ncol=154)   #130 = jumlahvariabel
landScen2_additionalG[1,75]<-1  #for x75
landScen2_additionalG[2,78]<-1  #for X78
landScen2_additionalG[3,82]<-1  #for X82

landScen2_additionalH<-list()
for(i in 1:ncol(landScen2_findem)){
  if(i==1){
    landScen2_additionalH[[i]]<-matrix(c(0,0,0), nrow=3, ncol=1)  
  }else{
    landScen2_additionalH[[i]]<-matrix(c(8584, 585.1, 22020.05),
                                       # (8584)+variabel_lsei[[i]][75,1], 
                                       #   (585.1)+variabel_lsei[[i]][78,1],
                                       #   (22020.05)+variabel_lsei[[i]][82,1]),
                                       nrow=3, ncol=1)
  }
}

landScen2_additionalE<-NULL
landScen2_additionalF<-NULL
landScen2_LUTMChange<-NULL