setwd("M:/EIPI3/Lasai")

library(devtools)
library(fastReadfwf)
library(haven)
library(data.table)
library(GGally)
#LECTURA DISEÃ‘O DE REGISTRO

# Año <- readline(prompt = "Enter year: 20,21  ")
# Mes <- readline(prompt = "Enter month and year e.g Sep, Oct, Nov:  ")
# 
# Mesnumero <- formatC(match(Mes,month.abb),width = 2,format = "d",flag = "0")
# 
# Envio <-  readline(prompt = "Numero de envio 01,02 or 03: ")


FDE <- as.data.table(read_sas("Nov20/fde1120.sas7bdat"))

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


# FDEs = lapply(files, read_sas)



# Mesanterior <- month.abb[as.integer(Mesnumero)-1]
# Mesnumeroanterior <- formatC(as.integer(Mesnumero)-1,width = 2,format = "d",flag = "0")
# 
# FDEanterior <- as.data.table(read_sas(paste(Mesanterior ,Año,"/fde",Mesnumeroanterior,Año,".sas7bdat", sep ="" )))
# regresores.fde <- FDE[,list(numidest,acti,cn01,cn02,cn03,cn04,cn05,observa,ccaa)]



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



# fastReadfwf::validateValues(data_PID_total, stSchema_PID)

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





# data_PGR_st[,c(seq(from = 5, to = 35, by = 2))]<- lapply(data_PGR_st[,c(seq(from = 5, to = 35, by = 2))], as.numeric)

# ggpairs(data_PGR_st, columns =  c(5,33,35)) #plot CNO and VE01 02
# ggcorr(data_PGR_st)
