setwd("M:/EIPI3/Lasai")
source('Disenos de registro/CNAEMIG.R') #Formateos YO

library(devtools)
library(fastReadfwf)
library(haven)
library(data.table)
library(GGally)
#LECTURA DISEÃ‘O DE REGISTRO

FDE <- as.data.table(read_sas("Nov20/fde1120.sas7bdat"))
FDE <- codigovalidez(FDE)
FDE <- FDE[validez == 'SI']
#READ FDE

files = list.files("fdes_19-20", full.names = T)
FDE_anteriores = data.table()
for (filename in files)
{
  FDEdata = read_sas(filename)
  FDE_anteriores = rbind(FDE_anteriores,FDEdata)
}
FDE_anteriores$periodo <- as.Date(paste(FDE_anteriores$periodo,'01'), format = '%Y%m%d')
FDE_anteriores$cn01<- ifelse(FDE_anteriores$cn01 == 0,1,FDE_anteriores$cn01)
FDE_anteriores$cn02 <-ifelse(FDE_anteriores$cn02 == 0,1,FDE_anteriores$cn02)
FDE_anteriores$cn03 <- ifelse(FDE_anteriores$cn03 == 0,1,FDE_anteriores$cn03)
FDE_anteriores$cn04 <- ifelse(FDE_anteriores$cn04 == 0,1,FDE_anteriores$cn04)
FDE_anteriores$cn05 <-ifelse(FDE_anteriores$cn05 == 0,1,FDE_anteriores$cn05)
# 
# FDE_anteriores <- codigovalidez(FDE_anteriores)
# FDE_anteriores <- FDE_anteriores[validez == 'SI']
# FDEs = lapply(files, read_sas)




filenamePID <- "Disenos de registro/ICNPID_Schema.xlsx"
dataFile_PID1 <- "Nov20/pid112001.txt"
dataFile_PID2 <- "Nov20/pid112002.txt"
dataFile_PID3 <- "Nov20/pid112003.txt"


stSchema_PID <- fastReadfwf::xlsxToSchema(filenamePID, sheetname = 'Schema', lang = 'en')
data_PID_st1 <- fastReadfwf::fread_fwf(dataFile_PID1, stSchema_PID, outFormat = 'data.table', convert = FALSE, perl = F, encoding = "Latin-1")
data_PID_st1$envioPID <- "01"
data_PID_st1 <- unique(data_PID_st1)
data_PID_st2 <- fastReadfwf::fread_fwf(dataFile_PID2, stSchema_PID, outFormat = 'data.table', convert = FALSE, perl = F, encoding = "Latin-1")
data_PID_st2$envioPID <- "02"
data_PID_st2 <- unique(data_PID_st2)
data_PID_st3 <- fastReadfwf::fread_fwf(dataFile_PID3, stSchema_PID, outFormat = 'data.table', convert = FALSE, perl = F, encoding = "Latin-1")
data_PID_st3$envioPID <- "03"
data_PID_st3 <- unique(data_PID_st3)
data_PID_total <- rbind(data_PID_st1,data_PID_st2, data_PID_st3)


fastReadfwf::validateValues(data_PID_total, stSchema_PID)

filenamePGR <- "Disenos de registro/ICNPGR_Schema.xlsx"
dataFile_PGR1 <- "Nov20/pgr112001.txt"
dataFile_PGR2 <- "Nov20/pgr112002.txt"
dataFile_PGR3 <- "Nov20/pgr112003.txt"

stSchema_PGR <- fastReadfwf::xlsxToSchema(filenamePGR, sheetname =  'Schema', lang = 'en')
data_PGR_st1 <- fastReadfwf::fread_fwf(dataFile_PGR1, stSchema_PGR, outFormat = 'data.table', convert = FALSE,  encoding = "Latin-1")
data_PGR_st1$envioPGR <- "01"
data_PGR_st1 <- unique(data_PGR_st1)

data_PGR_st2 <- fastReadfwf::fread_fwf(dataFile_PGR2, stSchema_PGR, outFormat = 'data.table', convert = FALSE,  encoding = "Latin-1")
data_PGR_st2$envioPGR <- "02"
data_PGR_st2 <- unique(data_PGR_st2)

data_PGR_st3 <- fastReadfwf::fread_fwf(dataFile_PGR3, stSchema_PGR, outFormat = 'data.table', convert = FALSE,  encoding = "Latin-1")
data_PGR_st3$envioPGR <- "03"
data_PGR_st3 <- unique(data_PGR_st3)

data_PGR_total <- rbind(data_PGR_st1,data_PGR_st2, data_PGR_st3)

fastReadfwf::validateValues(data_PGR_total, stSchema_PGR)

dataenvio2 <- unique(data_PGR_total[envioPGR != "03"])
dataenvio2PID <-  unique(data_PID_total[envioPID != "03"])

duplicadas = duplicated(dataenvio2$NUMIDEST)
dataenvio2duplicadas <- dataenvio2[duplicadas]
envio2añadir <- dataenvio2duplicadas[ envioPGR== "02"]
dataenvio2unicas <- dataenvio2[!duplicadas]
envio2finalPGR <- rbind(dataenvio2unicas,envio2añadir)

#1522 en el pGR observaciones en el envio 2


duplicadasPID = duplicated(dataenvio2PID$NUMIDEST)
dataenvio2duplicadasPID <- dataenvio2PID[duplicadasPID]
envio2añadirPID <- dataenvio2duplicadasPID[ envioPID== "02"]
dataenvio2PIDunicas <- dataenvio2PID[!duplicadasPID]
envio2finalPID <- dataenvio2PID
envio2finalPID[envioPID == "02"]

#1678 en pid ENVIO 2

dataenvio3PID <- data_PID_total 
dataenvio3PGR <-data_PGR_total
duplicadasPGR = duplicated(dataenvio3PGR$NUMIDEST)
dataenvio3duplicadasPGR <- dataenvio3PGR[duplicadasPGR]
envio3añadir <- dataenvio3duplicadasPGR[ envioPGR== "03"]
dataenvio3unicas <- dataenvio3PGR[!duplicadasPGR]
envio3finalPGR <- rbind(dataenvio3unicas,envio3añadir)
envio3finalPID <- dataenvio3PID
