# inPadatTPA <- read.table("_YK/raw/input_limbah/01_padat_ditimbun_di_TPA.csv", header = TRUE, sep = ";")
# inPadatLubang <- read.table("_YK/raw/input_limbah/02_padat_ditimbun_lubang.csv", header = TRUE, sep = ";")
# inPadatKompos <- read.table("_YK/raw/input_limbah/03_padat_kompos.csv", header = TRUE, sep = ";")
# inPadat3R <- read.table("_YK/raw/input_limbah/04_padat_3R.csv", header = TRUE, sep = ";")
# inPadatDomestik <- read.table("_YK/raw/input_limbah/05_padat_ins_sampah_domestik.csv", header = TRUE, sep = ";")
# inPadatIndustri <- read.table("_YK/raw/input_limbah/06_padat_ins_sampah_industri.csv", header = TRUE, sep = ";")
# inPadatDibakar <- read.table("_YK/raw/input_limbah/07_padat_dibakar_terbuka.csv", header = TRUE, sep = ";")
# inPadatTakTerkelola <- read.table("_YK/raw/input_limbah/08_padat_tidak_terkelola.csv", header = TRUE, sep = ";")
# inCairSeptictank <- read.table("_YK/raw/input_limbah/09_cair_septic_tank.csv", header = TRUE, sep = ";")
# inCairMCK <- read.table("_YK/raw/input_limbah/10_cair_mck_plus.csv", header = TRUE, sep = ";")
# inIPALaerob <- read.table("_YK/raw/input_limbah/11_cair_ipal_aerob.csv", header = TRUE, sep = ";")
# inIPALanaerob <- read.table("_YK/raw/input_limbah/12_cair_ipal_anaerob.csv", header = TRUE, sep = ";")
# inCairTakTerkelola <- read.table("_YK/raw/input_limbah/13_cair_tidak_terkelola.csv", header = TRUE, sep = ";")
# inCairBiogas <- read.table("_YK/raw/input_limbah/14_cair_biogas.csv", header = TRUE, sep = ";")
# inCairKimia <- read.table("_YK/raw/input_limbah/15_cair_pengolahan_kimia.csv", header = TRUE, sep = ";")
# inCair3R <- read.table("_YK/raw/input_limbah/16_cair_3R.csv", header = TRUE, sep = ";")

# proyeksiBAULimbah <- data.frame(proyeksiBAULimbah[,2:length(proyeksiBAULimbah)],
#                                 stringsAsFactors = FALSE)
# padatTPA <- data.frame(inPadatTPA[,2:length(inPadatTPA)],
#                        stringsAsFactors = FALSE)
# tablePadatTPA <- proyeksiBAULimbah * (1+padatTPA)
# deltaPadatTPA <- proyeksiBAULimbah - tablePadatTPA
# tableEmisi_padatTPA <- as.matrix(tablePadatTPA) %*% input_ef_matrix