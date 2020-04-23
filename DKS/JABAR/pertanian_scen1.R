#Skenario Sektor Pertanian
#Skenario 1: Penggunaan pupuk organik

setwd("D:/MRV/Aksara/Pertanian/Skenario")

#Read Input:
sektor<- read.csv("01_sektor.csv", header = FALSE)
int_dem<- read.csv("02_input_antara.csv", header = FALSE)
fin_dem_str<- read.csv("03_komponen_permintaan_akhir.csv", header = FALSE)
fin_dem<- read.csv("04_permintaan_akhir.csv", header = FALSE)
add_val_str<- read.csv("05_komponen_input_primer.csv", header = FALSE)
add_val<- read.csv("06_input_primer.csv", header = FALSE)

gdp_rate<- read.csv("08_gdp_rate.csv", header = FALSE)
additional_FD<- read.csv("07_additional_FD.csv", header = FALSE)

ef_diag<-read.csv("ef_diag.csv", header = FALSE)
ef_diag.m<-as.matrix(ef_diag)


#Intervention point:1. FINAL DEMAND

#Final demand projection from 2016-2030
FD_BAU.tot <- data.frame(rowSums(fin_dem))
colnames(FD_BAU.tot) <- "FD_BAU"

GDP_rate.tab<-data.frame(gdp_rate[,1:length(gdp_rate)],
                         stringsAsFactors = FALSE)

proyeksi_FD <- function(GDP_rate.tab, FD_BAU.tot) {
  for (i in 1:ncol(GDP_rate.tab)){
    if (i==1){
      GDP_rate.tab[,i] <-  (1+GDP_rate.tab[,i]) * FD_BAU.tot
    } else {
      GDP_rate.tab[,i] <-  GDP_rate.tab[,i-1] * (1+GDP_rate.tab[,i])
    }
  }
  return(GDP_rate.tab)
}

FD_proj <- proyeksi_FD(GDP_rate.tab, FD_BAU.tot)
FD_proj <- data.frame(FD_proj)


#CALCULATE CHANGES OF FINAL DEMAND IN CERTAIN SECTORS

#Adding the value of BAU FD and additional FD
FD_new<- FD_proj+additional_FD
FD_new[is.na(FD_new)]<- 0



#Intervention Point 2: Satellite account


year<- 2016:2030
for(i in 2016:2030){
  eval(parse(text=paste("pupuk",year, "<-as.matrix(read.table('D:/MRV/Aksara/Pertanian/Skenario/pupuk",year,".csv', header = TRUE, sep=','))", sep="")))
  eval(parse(text=paste("intervensi",year, "<-as.matrix(read.table('D:/MRV/Aksara/Pertanian/Skenario/intervensi_",year,".csv', header = FALSE, sep =','))", sep="")))
}


#Perkalian presentase dan proyeksi konsumsi pupuk
for(i in 2016:2030){
eval(parse(text=paste("table_perkalian_",year,"<- intervensi", year, " * pupuk", year, sep = "")))
}


#colsums
for(i in 2016:2030){
  eval(parse(text=paste("table_colsums_",year,"<- colSums(table_perkalian_", year,")", sep = "")))
}


#sum
for(i in 2016:2030){
  eval(parse(text=paste("table_sum_",year,"<- sum(table_colsums_", year,")", sep = "")))
}

sum_consumption<- rbind(table_sum_2016,table_sum_2017,table_sum_2018,table_sum_2019,table_sum_2020,table_sum_2021,table_sum_2022,table_sum_2023, table_sum_2024,
                     table_sum_2025,table_sum_2026,table_sum_2027, table_sum_2028, table_sum_2029, table_sum_2030)
                     

#creating fertilizer consumption graph

cons_table<- cbind.data.frame(year,sum_consumption)
cons_fer.graph<-ggplot(data=cons_table,aes(x=year, y=sum_consumption))+geom_line(color="red", stat = "identity")+ylab("Fertilizer consumption (ton)")


#Emission calculation (table perkalian*faktor emisi)
#Perkalian matriks table perkalian * faktor emisi

for(i in 2016:2030){
  eval(parse(text=paste("table_emisi_",year,"<- table_perkalian_", year, " %*% ef_diag.m", sep = "")))
}

#colsums
for(i in 2016:2030){
  eval(parse(text=paste("colsums_emisi_",year,"<- colSums(table_emisi_", year,")", sep = "")))
}

#sum
for(i in 2016:2030){
  eval(parse(text=paste("sum_emisi_",year,"<- sum(colsums_emisi_", year,")", sep = "")))
}

sum_emission<- rbind(sum_emisi_2016,sum_emisi_2017,sum_emisi_2018,sum_emisi_2019,sum_emisi_2020,sum_emisi_2021,sum_emisi_2022,sum_emisi_2023,
                     sum_emisi_2024,sum_emisi_2025,sum_emisi_2026,sum_emisi_2027,sum_emisi_2028,sum_emisi_2029,sum_emisi_2030)


#Creating fertilizer emission graph

ems_table<- cbind.data.frame(year,sum_emission)
ems_fer.graph<-ggplot(data=ems_table,aes(x=year, y=sum_emission))+geom_line(color="red", stat = "identity")+ylab("CO2-eq")
