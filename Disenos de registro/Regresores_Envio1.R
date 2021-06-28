# REGRESORES ENVIO 1

setwd("M:/EIPI3/Lasai")
library("openxlsx") #openxlsx 
library(VIM)
library(tidyverse)
source("Disenos de registro/Read.R") #lectura Datos
source("Disenos de registro/CodeManagement.R") #CNAE 

regresores.fde.anterior <- FDE_anteriores[,list(numidest,periodo,acti,ccaa,cn01,cn01a,cn01v,cn01e,cn02,cn02a,cn02v,cn02e,cn03,cn03a,cn03v,cn03e,cn04,cn04a,cn04v,cn04e,cn05, cn05a,cn05v,cn05e, month, year)]
regresores.fde.anterior$month = month(regresores.fde.anterior$periodo)
regresores.fde.anterior$year = year(regresores.fde.anterior$periodo)
# regresores.fde <- regresores.fde.anterior[periodo == "2020-10-01", list(numidest,periodo,acti,ccaa,cn01,cn01a,cn01v,cn01e,cn02,cn02a,cn02v,cn02e,cn03,cn03a,cn03v,cn03e,cn04,cn04a,cn04v,cn04e,cn05, cn05a,cn05v,cn05e)]
for (i in 1:length(names(regresores.fde.anterior))){
  names(regresores.fde.anterior)[i] = paste(names(regresores.fde.anterior)[i],'anterior', sep = ".")
}

# INTERMENSUAL E INTERANUAL
CNCNAE <- FDE_anteriores[, .(sum(cn01)), by = .(periodo,acti)]
regresores.fde.anterior <- regresores.fde.anterior[, sumacn01:=.(sum(cn01.anterior)), by = .(periodo.anterior,acti.anterior)]
regresores.fde.anterior <- regresores.fde.anterior[,c('intermensualCNAE','interanualCNAE'):=list((sumacn01-shift(sumacn01))/shift(sumacn01),
                                                                                                 (sumacn01-shift(sumacn01,n = 12))/shift(sumacn01, n = 12))]
# CNAEs <- unique(CNCNAE$acti)
# newtable = data.table()
# for (CNAE in CNAEs)
# {
#   tabla <- CNCNAE[acti == CNAE]
#   tabla <- tabla[order(tabla$periodo)]
#   tabla[, intermensualCNAE := (V1-shift(V1))/shift(V1)]
#   tabla[, interanualCNAE := (V1-shift(V1,n = 12))/shift(V1, n = 12)]
#   newtable <- rbind(newtable,tabla)
# }


# regresores.fde.anterior <- merge.data.table(regresores.fde.anterior,newtable, by.x = c("periodo.anterior","acti.anterior"), by.y =c("periodo","acti"))

regresores.fde.anterior <- regresores.fde.anterior[order(regresores.fde.anterior$periodo.anterior)]
regresores.fde.anterior[, c("intermensual.cn01","intermensual.cn02",
                            "intermensual.cn03", "intermensual.cn04",
                            "intermensual.cn05" , "interanual.cn01",
                            "interanual.cn02","interanual.cn03", 
                            "interanual.cn04", "interanual.cn05") := list((cn01.anterior-shift(cn01.anterior))/shift(cn01.anterior),
                                                                          (cn02.anterior-shift(cn02.anterior))/shift(cn02.anterior),
                                                                          (cn03.anterior-shift(cn03.anterior))/shift(cn03.anterior),
                                                                          (cn04.anterior-shift(cn04.anterior))/shift(cn04.anterior),
                                                                          (cn05.anterior-shift(cn05.anterior))/shift(cn05.anterior),
                                                                          (cn01.anterior-shift(cn01.anterior, n = 12))/shift(cn01.anterior, n = 12),   
                                                                          (cn02.anterior-shift(cn02.anterior, n = 12))/shift(cn02.anterior, n = 12),
                                                                          (cn03.anterior-shift(cn03.anterior, n = 12))/shift(cn03.anterior, n = 12),
                                                                          (cn04.anterior-shift(cn04.anterior, n = 12))/shift(cn04.anterior, n = 12),
                                                                          (cn05.anterior-shift(cn05.anterior, n = 12))/shift(cn05.anterior, n = 12)),
                        by = numidest.anterior]


# Asignar a cada numidest su intermensual e interanual
month(regresores.fde.anterior$periodo.anterior[1])

i = 1

intermensual = data.table()
mes = 11
Ano = 2020
intermensual = regresores.fde.anterior[ year.anterior == Ano & month.anterior == mes - 1]
intermensual.merge <- intermensual[,list(numidest.anterior,ccaa.anterior,acti.anterior,cn01.anterior, cn01a.anterior,cn01v.anterior,cn01e.anterior, 
                                         cn02.anterior, cn02a.anterior,cn02v.anterior,cn02e.anterior,
                                         cn03.anterior, cn03a.anterior,cn03v.anterior,cn03e.anterior,
                                         cn04.anterior, cn04a.anterior,cn04v.anterior,cn04e.anterior,
                                         cn05.anterior, cn05a.anterior,cn05v.anterior,cn05e.anterior,
                                         intermensual.cn01, 
                                         intermensual.cn02,intermensual.cn03,
                                         intermensual.cn04,intermensual.cn05,
                                         interanual.cn01, 
                                         interanual.cn02,interanual.cn03,
                                         interanual.cn04,interanual.cn05,
                                         intermensualCNAE,interanualCNAE)]


regresores.PGR <- dataenvio1[,list(NUMIDEST,CN01,CN02,CN03,CN04,CN05)]

# regresores.fde <- FDE[,list(numidest,acti,ccaa,cn01a,cn01v,cn01e)]
regresores.PID <- dataenvio1PID[,list(numidest,cnaeest,`cnaeemp `,provem,proves,codddpp,codtame,actual,obsanual1,obsanual2,prioridp,resulta, envioPID)]



# REPES

r <- regresores.PID[duplicated(regresores.PID)] #334 registros duplicados
r2 <- regresores.PID[duplicated(regresores.PID$numidest)] #2668 numidest repetidos

#REGRESORES PID + FDE
# regresores <- merge.data.table(regresores.PID,regresores.fde,by = "numidest", all.x = T)
regresores <- merge.data.table(regresores.PID,intermensual.merge,by.x = 'numidest', by.y = 'numidest.anterior', all.x = T)
regresores <- merge.data.table(regresores,regresores.PGR, by.x = "numidest",by.y = "NUMIDEST",all.x = T)

regresores[is.na(acti.anterior)]$acti.anterior = regresores[is.na(acti.anterior)]$cnaeest
regresores[is.na(ccaa.anterior)]$ccaa.anterior = provinciaToCCAA(regresores[is.na(ccaa.anterior)]$proves)
names(regresores)[3] = 'cnaeemp'

#Regresores derivados

# Resulta
regresores$imputar <- resultacode(regresores$resulta)



# CNAE emp a cnae acti

regresores[cnaeemp == "NNCC"]$cnaeemp = regresores[cnaeemp == "NNCC"]$acti
# CNAE EMPRESA
regresores$cnaeemp3 <- substr(regresores$cnaeemp,0,3)

regresores$cnaeemp2 <- substr(regresores$cnaeemp3,0,2)

regresores$cnaeemp1 <- CNAE3toCNAE1(regresores$cnaeemp3)

# CNAE Establecimiento
regresores$cnaeest3 <- substr(regresores$acti,0,3)

regresores$cnaeest2 <- substr(regresores$cnaeest3,0,2)

regresores$cnaeest1 <- CNAE3toCNAE1(regresores$cnaeest3)

regresores$match.cnae4 <- regresores$cnaeemp == regresores$acti
regresores$match.cnae3 <- regresores$cnaeemp3 == regresores$cnaeest3
regresores$match.cnae2 <- regresores$cnaeemp2 == regresores$cnaeest2
regresores$match.cnae1 <- regresores$cnaeemp1 == regresores$cnaeest1

# CNAE MIG


regresores$cnaeempMIG <- CNAE3toCNAEMIG(regresores$cnaeemp3)
regresores$cnaeempSub <- CNAE3toCNAESub(regresores$cnaeemp3)

regresores$cnaeestMIG <- CNAE3toCNAEMIG(regresores$cnaeest3)
regresores$cnaeestSub <- CNAE3toCNAESub(regresores$cnaeest3)

regresores$match.cnaeMIG <- regresores$cnaeempMIG == regresores$cnaeestMIG
regresores$match.cnaeSub <- regresores$cnaeempSub == regresores$cnaeestSub
# Coincide provincia 

regresores$match.prov <- regresores$provem == regresores$proves

# CCAA Sacar de FDE
regresores$CAemp <- provinciaToCCAA(regresores$provem)
# Match CCAA emp y est 

regresores$match.CCAA <- regresores$ccaa == regresores$CAemp

# Actual
regresores$actual[substr(regresores$actual,0,1)=="A"] <- "Entra"
regresores$actual[substr(regresores$actual,0,1)=="B"] <- "Sale"
regresores$actual[substr(regresores$actual,0,1)==""] <- "*"

# Obs 

obs <- paste(regresores$obsanual1, regresores$obsanual2)
regresores$obs <- ifelse(obs != " ",TRUE, FALSE)



regresores$obs.nchar <- nchar(obs, allowNA = T) 

regresores[,quartile:=.(quantile(cn01.anterior,probs = c(0.95),na.rm = T)), by = .(cnaeest2,ccaa.anterior)]
regresores$treshold <- regresores$cn01.anterior > regresores$quartile


regresoresFinal <- unique(regresores)
# regresoresFinal[regresoresFinal[is.na(acti)]]$acti = regresoresFinal[regresoresFinal[is.na(acti)]]$cnaeest

unicosRegresores<- unique(regresoresFinal)

unicosRegresores[,c("cnaeest","cnaeemp","provem","actual","proves","codddpp","prioridp","codtame","acti.anterior","ccaa.anterior","imputar","cnaeemp3","cnaeemp2","cnaeemp1","cnaeest3","cnaeest2","cnaeest1","cnaeempMIG","cnaeempSub","cnaeestMIG","cnaeestSub","CAemp")] <- lapply(unicosRegresores[,c("cnaeest","cnaeemp","provem","actual","proves","codddpp","prioridp","codtame","acti.anterior","ccaa.anterior","imputar","cnaeemp3","cnaeemp2","cnaeemp1","cnaeest3","cnaeest2","cnaeest1","cnaeempMIG","cnaeempSub","cnaeestMIG","cnaeestSub","CAemp")],as.factor) 
unicosRegresores[,c("CN01",'CN02','CN03','CN04','CN05',"cn01.anterior",'cn02.anterior','cn03.anterior','cn04.anterior','cn05.anterior')] <- lapply(unicosRegresores[,c("CN01",'CN02','CN03','CN04','CN05',"cn01.anterior",'cn02.anterior','cn03.anterior','cn04.anterior','cn05.anterior')], as.numeric)

unicosRegresoresNONA <- unicosRegresores

unicosRegresoresNONA <- unicosRegresoresNONA %>% drop_na(interanual.cn01)

unicosRegresoresNONA[envioPID =="01"]


pdf(file = "Imagenes/Envio1/PatronNoRespuestaENVIO1.pdf")

aggr_plot <- aggr(unicosRegresoresNONA, 
                  col=c('navyblue','red'), 
                  numbers=TRUE, 
                  sortVars=TRUE,
                  combined = T,
                  only.miss =F,
                  varheight = F,
                  labels=names(data),
                  bars =F, 
                  cex.axis=.55,
                  gap=1,
                  axes = T,
                  cex.numbers = 0.8,
                  cex.lab = 0.7,
                  ylab=c("Patron de falta de respuesta"),)
aggr_plot$missings
dev.off()

NAImputar <- unicosRegresores[is.na(unicosRegresores$imputar)] #SC e IN
NA14actual <-unicosRegresores[is.na(ccaa.anterior)] #Nuevas entradas pueden ser 
NAmatch <- unicosRegresores[is.na(unicosRegresores$match.cnae4)]
#Missing nchar porque son multybite
NAintermensual <- unicosRegresores[is.na(unicosRegresores$intermensual.cn01)] #Hay muchos NANs pq seria 0 el periodo anterior

NAInteranual <- unicosRegresores[is.na(unicosRegresores$interanual.cn01)]


saveRDS(unicosRegresoresNONA, "Datos/regresoresenvio1BIEN.rds")
