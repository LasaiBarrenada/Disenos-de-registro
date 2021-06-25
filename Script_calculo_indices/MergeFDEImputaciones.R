setwd("M:/EIPI3/Lasai/")
library(data.table)
library(StQ)
library(RepoReadWrite)
library(haven)
library(stringr)
datos <- readRDS("Datos/dataindices_envio1.rds")
fde <-as.data.table(read_sas("M:/EIPI3/Lasai/fdes_19-20/fde1120.sas7bdat"))

fde_depurado <- fde[cn01 != 0 & !is.na(cn01)]

DD_ICN <- RepoXLSToDD("M:/EIPI3/Lasai/FF/E30052.NombresVariables_V1.xlsx")


datos <- datos[,(list(numidest = numidest,CN01 = CN01,imputar= imputar))]

datosmerge <- datos[imputar == "Si"]
datpsmerge <- datosmerge[,list(numidest,CN01)]
fde_envio1 <- merge.data.table(fde_depurado,datpsmerge, by = 'numidest', all = T)





fde_envio1[!is.na(CN01)]$cn01 <- fde_envio1[!is.na(CN01)]$CN01
names(fde_envio1)[61] = "norden"
fde10 <-as.data.table(read_sas("M:/EIPI3/Lasai/fdes_19-20/fde1020.sas7bdat"))
names(fde10)[61] = 'norden'
fde_envio1

fde_envio1_FF <- fde_envio1[,!c("CN01")]
FF_StQ_envio1 <- melt_StQ(fde_envio1_FF, DD_ICN)

FF_StQ_Octubre <- melt_StQ(fde10, DD_ICN)
WriteRepoFile(FF_StQ_envio1, "FFNov1")
WriteRepoFile(FF_StQ_Octubre, "FFOct")

####ENVIO 2


fde <-as.data.table(read_sas("M:/EIPI3/Lasai/fdes_19-20/fde1120.sas7bdat"))

fde_depurado <- fde[cn01 != 0 & !is.na(cn01)]

DD_ICN <- RepoXLSToDD("M:/EIPI3/Lasai/FF/E30052.NombresVariables_V1.xlsx")

datos2 <- readRDS("Datos/dataindices_envio2.rds")


datosmerge <- datos2[imputar == "Si"]
datpsmerge <- datosmerge[,list(numidest,CN01)]
fde_envio2 <- merge.data.table(fde_depurado,datpsmerge, by = 'numidest', all = T)





fde_envio2[!is.na(CN01)]$cn01 <- fde_envio2[!is.na(CN01)]$CN01
names(fde_envio2)[61] = "norden"
fde10 <-as.data.table(read_sas("M:/EIPI3/Lasai/fdes_19-20/fde1020.sas7bdat"))
names(fde10)[61] = 'norden'
fde_envio2

fde_envio2_FF <- fde_envio2[,!c("CN01")]
FF_StQ_envio2 <- melt_StQ(fde_envio2_FF, DD_ICN)

FF_StQ_Octubre <- melt_StQ(fde10, DD_ICN)
WriteRepoFile(FF_StQ_envio2, "FFNov2")
WriteRepoFile(FF_StQ_Octubre, "FFOct")


####ENVIO 3


fde <-as.data.table(read_sas("M:/EIPI3/Lasai/fdes_19-20/fde1120.sas7bdat"))

fde_depurado <- fde[cn01 != 0 & !is.na(cn01)]

DD_ICN <- RepoXLSToDD("M:/EIPI3/Lasai/FF/E30052.NombresVariables_V1.xlsx")

datos3 <- readRDS("Datos/dataindices_envio3.rds")


datosmerge <- datos3[imputar == "Si"]
datpsmerge <- datosmerge[,list(numidest,CN01)]
fde_envio3 <- merge.data.table(fde_depurado,datpsmerge, by = 'numidest', all = T)





fde_envio3[!is.na(CN01)]$cn01 <- fde_envio3[!is.na(CN01)]$CN01
names(fde_envio3)[61] = "norden"
fde10 <-as.data.table(read_sas("M:/EIPI3/Lasai/fdes_19-20/fde1020.sas7bdat"))
names(fde10)[61] = 'norden'
fde_envio3

fde_envio3_FF <- fde_envio3[,!c("CN01")]
FF_StQ_envio3 <- melt_StQ(fde_envio3_FF, DD_ICN)

FF_StQ_Octubre <- melt_StQ(fde10, DD_ICN)
WriteRepoFile(FF_StQ_envio3, "FFNov3")
WriteRepoFile(FF_StQ_Octubre, "FFOct")

