setwd("D:/MRV/Aksara/Pertanian")

### Calculate agriculture satellite table ###

#Read input
int_dem<-read.csv("int_dem.csv", header = FALSE)
add_val<-read.csv("add_val.csv", header = FALSE)
fin_dem<-read.csv("fin_dem.csv", header = FALSE)
cons_fer<- read.csv("konsumsi_pupuk.csv",header = FALSE)  #tabel konsumsi penggunaan pupuk
ef_fer<-read.csv("EF_fer.csv", header = TRUE)  #tabel faktor emisi pupuk
ef_diag<-read.csv("ef_diag.csv", header = FALSE)
ef_diag.m<-as.matrix(ef_diag)


new_output<- read.csv("new_output.csv", header = TRUE)

#Tabel satelit pertanian per sektor 
tot_fer<-rowSums(cons_fer)
CO2_fer<-cons_fer*ef_fer



#Calculate multiplier of fertilizer consumption

#Output
int_dem.tot<- colSums(int_dem)
fin_dem.tot<- rowSums(fin_dem)
output<- int_dem.tot+fin_dem.tot

#Koef. pertanian
cons_fer.tot<- rowSums(cons_fer)
coef_ag<- cons_fer.tot/output

#Multiplier
cons_fer.m<- as.matrix(cons_fer)
coef_ag.m<-as.matrix(coef_ag)
ag_multiplier<- coef_ag* cons_fer.tot


#Calculate the projection of fertilizer consumption (2015-2030)

#Proporsi jenis pupuk
prop_fer<- cons_fer/rowSums(cons_fer)
prop_fer.m<-as.matrix(prop_fer)
prop_fer.m[is.nan(prop_fer.m)]<-0
prop_fer.dat<-as.data.frame(prop_fer.m)


#Konsumsi pupuk baru
new_cons<- coef_ag*new_output
new_cons.m<- as.matrix(new_cons)


#projection of fertilizer consumption
tab_konsum <- list()
for (i in 1:ncol(new_cons.m)) {
  tab_konsum[[i]]<-matrix(NA, nrow=nrow(prop_fer.m), ncol=ncol(prop_fer.m))
  for (a in 1:ncol(prop_fer.m)){
    tab_konsum[[i]][,a] <- as.matrix(new_cons.m[,i])* as.matrix(prop_fer.m [,a])
  }}


#Calculate CO2-eq emission from the projection of fertilizer consumption (2015-2030)

emission_tot<-list()
for (j in 1:length(tab_konsum)) {
  emission_tot[[j]]<- tab_konsum[[j]]%*%ef_diag.m
  
}

