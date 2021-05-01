# REGRESORES

setwd("M:/EIPI3/Lasai")
library("xlsx") #openxlsx 
library(VIM)

source("Disenos de registro/Read.R") #lectura Datos
source("Disenos de registro/CodeManagement.R") #CNAE 
source('Disenos de registro/CNAEMIG.R') #Formateos YO

regresores.fde.anterior <- FDE_anteriores[,list(numidest,periodo,acti,ccaa,cn01,cn02,cn03,cn04,cn05, month, year)]
regresores.fde.anterior$month = month(regresores.fde.anterior$periodo)
regresores.fde.anterior$year = year(regresores.fde.anterior$periodo)
for (i in 1:length(names(regresores.fde.anterior))){
  names(regresores.fde.anterior)[i] = paste(names(regresores.fde.anterior)[i],'anterior', sep = ".")
}

# INTERMENSUAL E INTERANUAL
CNCNAE <- FDE_anteriores[, .(sum(cn01)), by = .(periodo,acti)]
CNAEs <- unique(CNCNAE$acti)
newtable = data.table()
for (CNAE in CNAEs)
{
  tabla <- CNCNAE[acti == CNAE]
  tabla <- tabla[order(tabla$periodo)]
  tabla[, intermensualCNAE := (V1-shift(V1))/shift(V1)]
  tabla[, interanualCNAE := (V1-shift(V1,n = 12))/shift(V1, n = 12)]
  newtable <- rbind(newtable,tabla)
}


regresores.fde.anterior <- merge.data.table(regresores.fde.anterior,newtable, by.x = c("periodo.anterior","acti.anterior"), by.y =c("periodo","acti"))


regresores.PGR <- data_PGR_total[,list(NUMIDEST,CN01,CN02,CN03,CN04,CN05)]
regresores.fde <- FDE[,list(numidest,acti,ccaa)]
regresores.PID <- data_PID_total[,list(numidest,cnaeest,`cnaeemp `,provem,proves,codddpp,codtame,actual,obsanual1,obsanual2,prioridp,resulta, envioPID)]



regresores <- merge.data.table(regresores.PID,regresores.fde,by = "numidest", all.x = T)

regresores[is.na(acti)]$acti = regresores[is.na(acti)]$cnaeest
regresores[is.na(ccaa)]$ccaa = provinciaToCCAA(regresores[is.na(ccaa)]$proves)
# REPES
names(regresores)[3] = 'cnaeemp'
repes <- as.data.frame(regresores[duplicated(regresores$numidest)]) #Varios numidest repetidos
numirepe <- unique(repes$numidest)

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


regresoresFinal <- merge.data.table(regresores,regresores.PGR, by.x = "numidest", by.y = "NUMIDEST", all.x = T)

regresoresFinal <- unique(regresoresFinal)

regresoresFinal[regresoresFinal[is.na(acti)]]$acti = regresoresFinal[regresoresFinal[is.na(acti)]]$cnaeest

regresores.fde.anterior <- regresores.fde.anterior[order(regresores.fde.anterior$periodo.anterior)]
regresores.fde.anterior[, c("intermensual.cn01","intermensual.cn02","intermensual.cn03", "intermensual.cn04", "intermensual.cn05" , "interanual.cn01","interanual.cn02","interanual.cn03", "interanual.cn04", "interanual.cn05") := list((cn01.anterior-shift(cn01.anterior))/shift(cn01.anterior),
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
# regresoresFinal <- unique(regresoresFinal, by = 'numidest')
i = 1
intermensual = data.table()
# for (numidest2 in regresoresFinal$numidest)
# {
#   
Ano = 2020
mes = 11
#   
#   estable = regresores.fde.anterior[numidest.anterior == numidest2 & year.anterior == Ano & month.anterior == mes - 1] 
#   intermensual = rbind(intermensual,estable)
#   anual = regresores.fde.anterior[numidest.anterior == numidest2 & year.anterior == Ano & month.anterior == mes-1]
#   interanual = rbind(interanual,anual)
#   i = i +1 
# }
intermensual = regresores.fde.anterior[ year.anterior == Ano & month.anterior == mes - 1]
intermensual.merge <- intermensual[,list(numidest.anterior,cn01.anterior, intermensual.cn01, 
                                         intermensual.cn02,intermensual.cn03,
                                         intermensual.cn04,intermensual.cn05,
                                         interanual.cn01, 
                                         interanual.cn02,interanual.cn03,
                                         interanual.cn04,interanual.cn05,
                                         intermensualCNAE,interanualCNAE)]
# interanual.merge <- interanual[,list(numidest.anterior,interanual.cn01, 
                                     # interanual.cn02,interanual.cn03,
                                     # interanual.cn04,interanual.cn05)]
regresoresFinal <- merge.data.table(regresoresFinal,intermensual.merge, by.x = 'numidest', by.y = 'numidest.anterior', all.x = T)
# regresoresFinal <- merge.data.table(regresoresFinal,interanual.merge, by.x = 'numidest', by.y = 'numidest.anterior')

unicosRegresores<- unique(regresoresFinal)

unicosRegresores[,c("cnaeemp","provem","proves","codddpp","prioridp","codtame","acti","ccaa","imputar","cnaeemp3","cnaeemp2","cnaeemp1","cnaeest3","cnaeest2","cnaeest1","cnaeempMIG","cnaeempSub","cnaeestMIG","cnaeestSub","CAemp")] <- lapply(unicosRegresores[,c("cnaeemp","provem","proves","codddpp","prioridp","codtame","acti","ccaa","imputar","cnaeemp3","cnaeemp2","cnaeemp1","cnaeest3","cnaeest2","cnaeest1","cnaeempMIG","cnaeempSub","cnaeestMIG","cnaeestSub","CAemp")],as.factor) 
unicosRegresores[,c("cn01.anterior",'CN01','CN02','CN03','CN04','CN05')] <- lapply(unicosRegresores[,c('cn01.anterior','CN01','CN02','CN03','CN04','CN05')], as.numeric)


regresores1 <- unicosRegresores[envioPID == "01"]
regresores2 <- unicosRegresores[envioPID == "02"]
regresores3 <- unicosRegresores[envioPID == "03"]


aggr_plot <- aggr(unicosRegresores, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
aggr_plot$missings

NAImputar <- unicosRegresores[is.na(unicosRegresores$imputar)] #SC e IN
NA14actual <-unicosRegresores[is.na(ccaa)] #Nuevas entradas pueden ser 
NAmatch <- unicosRegresores[is.na(unicosRegresores$match.cnae4)]
 #Missing nchar porque son multybite
NAintermensual <- unicosRegresores[is.na(unicosRegresores$intermensual.cn01)] #Hay muchos NANs pq seria 0 el periodo anterior

r <- unicosRegresores[is.na(unicosRegresores$interanual.cn01)]



