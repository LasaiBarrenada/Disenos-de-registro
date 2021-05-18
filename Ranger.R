library(ranger)
library(ggplot2)
setwd("M:/EIPI3/Lasai/")
source("Disenos de registro/Regresores.R")

FDEpreds <- FDE[,list(numidest,cn01)]
names(FDEpreds)[2]="cn01.fde"
regresoresRF <- merge.data.table(regresores1,FDEpreds,by = "numidest",all.x = T)
drop2 <- c("cn02.anterior" ,"cn02a.anterior","cn02v.anterior","cn02e.anterior",
           "cn03.anterior" ,"cn03a.anterior", "cn03v.anterior", "cn03e.anterior",
           "cn04.anterior", "cn04a.anterior", "cn04v.anterior","cn04e.anterior","cn05.anterior","cn05a.anterior", "cn05v.anterior","cn05e.anterior")

dropvariables = c('numidest','CN02','CN03','CN04','CN05','cnaeest','obsanual1','obsanual2', 'actual',"envioPID")
regresoresRF1 <- regresoresRF[,(dropvariables):=NULL]
regresoresRF1 <- regresoresRF[,(drop2):=NULL]
regresoresRF1 <- regresoresRF1[!is.na(intermensual.cn01)]
regresoresRF1 <- regresoresRF1[!is.na(imputar)]
regresoresRF1 <- regresoresRF1[!is.na(cn01.anterior)]
regresoresRF1 <- regresoresRF1[!is.na(interanual.cn01)]
regresoresRF1 <- regresoresRF1[!is.na(cn01a.anterior)]
regresoresRF1 <- regresoresRF1[!is.na(cn01v.anterior)]
regresoresRF1 <- regresoresRF1[!is.na(cn01e.anterior)]
regresoresRF1 <- regresoresRF1[!is.na(cn01.fde)]
missingsRF <- aggr(regresoresRF1,  col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 
missingsRF$missings


data.train <- regresoresRF1[!is.na(regresoresRF1$CN01)]
data.train$imputado <- "No"
data.control <- regresoresRF1[is.na(regresoresRF1$CN01)]
data.control$imputado <- 'Si'

ranger <- ranger(CN01 ~ . -cn01.fde  , data = na.omit(data.train), mtry = sqrt(ncol(data.train)) ,  importance = "impurity")
ranger
summary(ranger)
varimportance <- as.data.frame(ranger$variable.importance)
ranger$prediction.error
ranger$r.squared

ggplot(data = varimportance, aes(x = rownames(varimportance) ,y = `ranger$variable.importance`))+
  geom_point()+
  theme(axis.text.x = element_text(size = 12,angle = 90))

grafico1 <- data.frame(x = data.train$CN01, y = ranger$predictions, cn01a = data.train$cn01a.anterior, resulta = data.train$resulta, imputar = data.train$imputar  )
ggplot(data = grafico1[grafico1$imputar != "Baja",], aes(x = x,y = y, col = imputar)) +
  geom_point()+
  facet_grid(cn01a~imputar,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

preds = predict(ranger, data = data.control)
grafico2 <- data.table(x = data.control$cn01.fde, y = preds$predictions, cn01a = data.control$cn01a.anterior, resulta = data.control$resulta, imputar = data.control$imputar )


ggplot(data = grafico2, aes(x = x,y = y, col = imputar)) +
  geom_point()+
  facet_grid(cn01a~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

