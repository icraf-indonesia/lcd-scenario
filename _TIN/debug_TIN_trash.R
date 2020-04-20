robustLog = function(x) {
    tryCatch(log(x),
            warning = function(w) {print(paste("negative argument", x)); 
                 log(-x)},
             error = function(e) {print(paste("non-numeric argument", x)); 
                 NaN}) 
 }
inputs = list(1, 2, 4, -5, 'oops', 0, 10)
 for(input in inputs) {
   print(paste("robust log of", input, "=", robustLog(input)))
 }






inputs = list(1, 2, 4, -5, 'oops', 0, 10)
robustLog=function(number){
  log(number)
}
  
listX<-list()

for(input in inputs){
  tryCatch({
    eval(parse(text=paste0("listX$x",input,"<-robustLog(",input,")")))
    print(paste("robust log of", input, "=", robustLog(input)))
  },
    warning=function(a){
      eval(parse(text=paste0("listX$x",input,"<-'warning')")))
      # print(paste("negative argument", input))
    }, 
    error = function(b){
      eval(parse(text=paste0("listX$x",input,"<-'error')")))
      # print(paste("non-numeric argument", input))
    }
  )
}


projectionYear <- initialYear
projectionYear<-2030
listYear <- paste0("y", ioPeriod)
i<-3
for(step in 1:(iteration+1)){
  timeStep <- paste0("y", projectionYear)
eval(parse(text=paste0(
  "bauSeriesOfImpactLand2$",timeStep,"<-
      functionSatelliteLand2 (type ='projected',
                              landCoverProjection = as.matrix(bauSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]),
                              LUTMTemplate = LUTMTemplate_his, 
                              advanceMode = FALSE,
                              runNum =",i,", 
                              GDP=as.matrix(bauSeriesOfGDP$",timeStep,")
      )")))
listYear <- c(listYear, timeStep)
projectionYear <- initialYear+step  
}

-------------------------------------------
projectionYear <- initialYear
projectionYear<-2016
timeStep<-"y2016"

listYear <- paste0("y", ioPeriod)
eval(parse(text=paste0("scenario1LandSeriesOfImpactLand3$",timeStep,"<-functionSatelliteLand3(inputLandScen=as.data.frame(scenario1LandInputLandCover),
                                                                                                  timeScen='",timeStep,"')")))



eval(parse(text=paste0("
      scenario1LandSeriesOfImpactLand2$",timeStep,"<-
      functionSatelliteLand2 (type ='projected',
                              landCoverProjection = as.matrix(scenario1LandSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]) ,
                              inputLandCover = scenario1LandSeriesOfImpactLand3[['",timeStep,"']][['landCover']], 
                              LUTMTemplate = scenario1LandSeriesOfImpactLand3[['",timeStep,"']][['LUTMTemplate']], 
                              advanceMode = FALSE,
                              runNum =",i," , 
                              GDP=as.matrix(scenario1LandSeriesOfGDP$",timeStep,",), 
                              additionalG = as.matrix(scenario1LandSeriesOfImpactLand3[['",timeStep,"']][['additionalG']]), 
                              additionalH= as.matrix(scenario1LandSeriesOfImpactLand3[['",timeStep,"']][['additionalH']]) 
                              
      )")))

type ='projected'
landCoverProjection = as.matrix(scenario1LandSeriesOfImpactLand1[[timeStep]][['landCover']][['luas.land.use']])
inputLandCover = scenario1LandSeriesOfImpactLand3[[timeStep]][['landCover']]
LUTMTemplate = scenario1LandSeriesOfImpactLand3[[timeStep]][['LUTMTemplate']] 
advanceMode = FALSE
runNum =4 
GDP=as.matrix(paste0(scenario1LandSeriesOfGDP[[timeStep]]))
additionalG= as.matrix(scenario1LandSeriesOfImpactLand3[[timeStep]][['additionalG']]) 
additionalH= as.matrix(scenario1LandSeriesOfImpactLand3[[timeStep]][['additionalH']])
additionalE=NULL
additionalF=NULL


------------------------------------------
  
# matrix G
impact$matrixG<-rbind(diag(nrow=(jumlahVariabel)), matrix(0, nrow=length(diagVariable),ncol=jumlahVariabel))  ## buat matrix G constraint 1 & 2
colnames(impact$matrixG)<-namaVariabel
for (i in 1:length(diagVariable)){
  impact$matrixG[jumlahVariabel+i,diagVariable[i]]<-1   #assign 1 untuk semua variabel diagonal
}
if (is.null(additionalG)){
  impact$matrixG<-impact$matrixG
} else{
  impact$matrixG<- rbind(impact$matrixG, additionalG)
}


# matrix H
impact$matrixH<-rbind(matrix(0,nrow=jumlahVariabel,ncol=1),as.matrix(diagTPM*as.matrix(bauSeriesOfImpactLand1[[paste0("y", projectionYear-1)]][["landCover"]][landCover_his!=0,])))

if (is.null(additionalH)){
  impact$matrixH<-impact$matrixH
} else{
  impact$matrixH<- rbind(impact$matrixH, additionalH)
}


##############################
# tes energi
test_energyScen<-readRDS("_TIN/debug/test_energyScen")
changefd<-read.csv("_DB/input csv/20_delta_FD_sken1.csv", header=TRUE)
test_energyScen[["fdSelisih"]][,2:ncol(test_energyScen[["fdSelisih"]])]<-changefd
for(i in names(test_energyScen[["satSelisih"]])){
  matrixTes<-as.matrix(test_energyScen[["satSelisih"]][[paste0(i)]])[,4:length(test_energyScen[["satSelisih"]][[paste0(i)]])]
  class(matrixTes)<-"numeric"
  test_energyScen[["satSelisih"]][[paste0(i)]][["Tconsumption"]]<-rowSums(matrixTes)
}
saveRDS(test_energyScen,"_TIN/debug/test_energyScen" )
yep<-readRDS("_TIN/debug/test_energyScen")



##################
i<-6
projectionYear <- initialYear
projectionYear<-2017
timeStep<-"y2017"
eval(parse(text=paste0("
      scenarioSeriesOfImpactLand2$",timeStep,"<-
      functionSatelliteLand2 (type ='projected',
                              landCoverProjection = as.matrix(scenarioSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]),
                              landCoverProjectionMin = as.matrix(scenarioSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                              inputLandCover = scenarioSeriesOfImpactLand3[['",timeStep,"']][['landCover']], 
                              LUTMTemplate = scenarioSeriesOfImpactLand3[['",timeStep,"']][['LUTMTemplate']], 
                              advanceMode = FALSE,
                              runNum =",i," , 
                              GDP=as.matrix(scenarioSeriesOfGDP$",timeStep,",), 
                              additionalG = scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalG']], 
                              additionalH = scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalH']] 
                              )"
)))


type ='projected'
landCoverProjection = as.matrix(scenarioSeriesOfImpactLand1[[timeStep]][['landCover']][['luas.land.use']])
inputLandCover = scenarioSeriesOfImpactLand3[[timeStep]][['landCover']]
LUTMTemplate = scenarioSeriesOfImpactLand3[[timeStep]][['LUTMTemplate']] 
advanceMode = FALSE
runNum =6
GDP=as.matrix(paste0(scenarioSeriesOfGDP[[timeStep]]))
additionalG= scenarioSeriesOfImpactLand3[[timeStep]][['additionalG']]
additionalH= scenarioSeriesOfImpactLand3[[timeStep]][['additionalH']]
additionalE=NULL
additionalF=NULL

####
landCoverSeries<-matrix(nrow=nrow(scenarioSeriesOfImpactLand1[["y2016"]][["landCover"]]), ncol=length(scenarioSeriesOfImpactLand1))
colnames(landCoverSeries)<-names(scenarioSeriesOfImpactLand1)
for (i in names(scenarioSeriesOfImpactLand1)){
  eval(parse(text=paste0('landCoverSeries[,"',i,'"]<-as.matrix(scenarioSeriesOfImpactLand1[["',i,'"]][["landCover"]][["luas.land.use"]])')))
}
bauLandCoverSeries<-matrix(nrow=nrow(scenarioSeriesOfImpactLand1[["y2016"]][["landCover"]]), ncol=length(scenarioSeriesOfImpactLand1))
colnames(bauLandCoverSeries)<-names(scenarioSeriesOfImpactLand1)
for (i in names(scenarioSeriesOfImpactLand1)){
  eval(parse(text=paste0('bauLandCoverSeries[,"',i,'"]<-as.matrix(bauSeriesOfImpactLand1[["',i,'"]][["landCover"]][["luas.land.use"]])')))
}


######################################################

  
projectionYear <- initialYear
listYear <- paste0("y", ioPeriod)
  
for(step in 1:(iteration+1)){
  timeStep <- paste0("y", projectionYear)
    for (i in 1:6){   # 5 tipe yg akan dirun otomatis
    eval(parse(text=paste0("scenarioSeriesOfImpactLand3$",timeStep,"<-functionSatelliteLand3(inputLandScen=scenarioInputLandCover,
                                                                                             timeScen='",timeStep,"')")))
    eval(parse(text=paste0("
      scenarioSeriesOfImpactLand2$",timeStep,"<-tryCatch({
      functionSatelliteLand2 (type ='projected',
                              landCoverProjection = as.matrix(scenarioSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]),
                              landCoverProjectionMin = as.matrix(scenarioSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                              inputLandCover = scenarioSeriesOfImpactLand3[['",timeStep,"']][['landCover']], 
                              LUTMTemplate = scenarioSeriesOfImpactLand3[['",timeStep,"']][['LUTMTemplate']], 
                              advanceMode = FALSE,
                              runNum =",i," , 
                              GDP=as.matrix(scenarioSeriesOfGDP$",timeStep,",), 
                              additionalG = scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalG']], 
                              additionalH= scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalH']] 
                              )
      }, warning = function (a){NA}, error = function(b){NA})"
    )))
    if(any(is.na(scenarioSeriesOfImpactLand2[[timeStep]]))==FALSE){  
      print(paste0("use constraint ", i ," to make LUTM ",timeStep))
      break
    } else {
      if(i==6){
        print(paste0("tidak berhasil menghitung LUTM ",timeStep))
      } 
    }
    }    
  listYear <- c(listYear, timeStep)
  projectionYear <- initialYear+step
}
  
  
###############################
  
  
projectionYear <- initialYear
listYear <- paste0("y", ioPeriod)
  
for(step in 1:(iteration+1)){
  for (i in 1:6){   # 5 tipe yg akan dirun otomatis
    timeStep <- paste0("y", projectionYear)
    eval(parse(text=paste0(
    "bauSeriesOfImpactLand2$",timeStep,"<-tryCatch({
    functionSatelliteLand2 (type ='projected',
                            landCoverProjection = as.matrix(bauSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]) ,
                            landCoverProjectionMin=  as.matrix(bauSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                            LUTMTemplate = LUTMTemplate_his, 
                            advanceMode = FALSE,
                            runNum =",i," , 
                            GDP=as.matrix(bauSeriesOfGDP$",timeStep,")
    )
  }, warning = function (a){NA}, error = function(b){NA})"
    )))
    if(any(is.na(bauSeriesOfImpactLand2[[timeStep]]))==FALSE){
      print(paste0("use constraint ", i ," to make LUTM ",timeStep))
      break
    } else {
      if(i==6){
        print(paste0("tidak berhasil menghitung LUTM ",timeStep))
      } 
    }
  }
listYear <- c(listYear, timeStep)
projectionYear <- initialYear+step
}



#####################################################################################################
####### LUTM projection scenario, jika run type untuk semua tahun disamakan

for (i in 1:6){   # 5 tipe yg akan dirun otomatis
  
  projectionYear <- initialYear
  listYear <- paste0("y", ioPeriod)
  
  for(step in 1:(iteration+1)){
    timeStep <- paste0("y", projectionYear)
    eval(parse(text=paste0("scenarioSeriesOfImpactLand3$",timeStep,"<-functionSatelliteLand3(inputLandScen=scenarioInputLandCover,
                                                                                             timeScen='",timeStep,"')")))
    eval(parse(text=paste0("
      scenarioSeriesOfImpactLand2$",timeStep,"<-tryCatch({
      functionSatelliteLand2 (type ='projected',
                              landCoverProjection = as.matrix(scenarioSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]),
                              landCoverProjectionMin = as.matrix(scenarioSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                              inputLandCover = scenarioSeriesOfImpactLand3[['",timeStep,"']][['landCover']], 
                              LUTMTemplate = scenarioSeriesOfImpactLand3[['",timeStep,"']][['LUTMTemplate']], 
                              advanceMode = FALSE,
                              runNum =",i," , 
                              GDP=as.matrix(scenarioSeriesOfGDP$",timeStep,",), 
                              additionalG = scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalG']], 
                              additionalH= scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalH']] 
                              )
      }, warning = function (a){NA}, error = function(b){NA})"
    )))
    listYear <- c(listYear, timeStep)
    projectionYear <- initialYear+step  
  }
  
  if(any(is.na(unlist(scenarioSeriesOfImpactLand2)))==FALSE){  
    if(i==1){
      print("use constraint 1 to make LUTM")
    } else if (i==2){
      print("use constraint 2 to make LUTM")
    } else if (i==3){
      print("use constraint 3 to make LUTM")
    } else if (i ==4){
      print("use constraint 4 to make LUTM")
    } else if (i == 5){
      print("use no constraint 5 to make LUTM")
    } else if (i== 6){
      print("use constraint 6 to make LUTM")
    }
    break
  } else {
    if(i==6){
      print("tidak berhasil menghitung LUTM")
    } 
  }
}