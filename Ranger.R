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

dropvariables = c('CN02','CN03','CN04','CN05','cnaeest','obsanual1','obsanual2', 'actual',"envioPID")
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
data.control$imputado <- '1'

ranger <- ranger(CN01 ~ . -cn01.fde  , data = na.omit(data.train), mtry = sqrt(ncol(data.train)) ,  importance = "impurity")
ranger
summary(ranger)
varimportance <- as.data.frame(ranger$variable.importance)
ranger$prediction.error
ranger$r.squared

ggplot(data = varimportance, aes(x = rownames(varimportance) ,y = `ranger$variable.importance`))+
  geom_point()+
  theme(axis.text.x = element_text(size = 12,angle = 90,vjust =  0.5))

grafico1 <- data.frame(x = data.train$CN01, y = ranger$predictions, cn01a = data.train$cn01a.anterior, resulta = data.train$resulta, imputar = data.train$imputar  )
ggplot(data = grafico1[grafico1$imputar != "Baja",], aes(x = x,y = y, col = imputar)) +
  geom_point()+
  facet_grid(cn01a~imputar,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

preds = predict(ranger, data = data.control)
grafico2 <- data.table(x = data.control$cn01.fde, y = preds$predictions, cn01a = data.control$cn01a.anterior, resulta = data.control$resulta, imputar = data.control$imputar, cnaest = data.control$cnaeestMIG, CCAA = data.control$ccaa.anterior )


ggplot(data = grafico2[grafico2$imputar != 'No'], aes(x = x,y = y, col = imputar)) +
  geom_point()+
  facet_grid(cnaest~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

#Sobreestima las pequeñas e infraestima las grandes

data.control$CN01 = preds$predictions

datacomplete1 <- rbind(data.train,data.control)


#ENVIO 2
regresoresRF <- merge.data.table(regresores2,FDEpreds,by = "numidest",all.x = T)
regresoresRF2 <- regresoresRF[,(dropvariables):=NULL]
regresoresRF2 <- regresoresRF[,(drop2):=NULL]
regresoresRF2 <- regresoresRF2[!is.na(intermensual.cn01)]
regresoresRF2 <- regresoresRF2[!is.na(imputar)]
regresoresRF2 <- regresoresRF2[!is.na(cn01.anterior)]
regresoresRF2 <- regresoresRF2[!is.na(interanual.cn01)]
regresoresRF2 <- regresoresRF2[!is.na(cn01a.anterior)]
regresoresRF2 <- regresoresRF2[!is.na(cn01v.anterior)]
regresoresRF2 <- regresoresRF2[!is.na(cn01e.anterior)]
regresoresRF2 <- regresoresRF2[!is.na(cn01.fde)]
missingsRF <- aggr(regresoresRF2,  col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 
missingsRF$missings

regresoresRF2 <- datacomplete1[regresoresRF2, on=.(numidest), CN01 := .(i.CN01)][]
data.train2 <- regresoresRF2[!is.na(regresoresRF2$CN01)]
data.control2 <- regresoresRF2[is.na(regresoresRF2$CN01)]
data.control2$imputado <- '2'



ranger2 <- ranger(CN01 ~ . -cn01.fde  , data = na.omit(data.train2), mtry = sqrt(ncol(data.train2)) ,  importance = "impurity")
ranger2
summary(ranger2)
varimportance2 <- as.data.frame(ranger2$variable.importance)
ranger2$prediction.error
ranger2$r.squared

ggplot(data = varimportance2, aes(x = rownames(varimportance2) ,y = `ranger2$variable.importance`))+
  geom_point()+
  theme(axis.text.x = element_text(size = 12,angle = 90,vjust =  0.5))

grafico1 <- data.frame(x = data.train2$CN01, y = ranger2$predictions, cn01a = data.train2$cn01a.anterior, resulta = data.train2$resulta, imputar = data.train2$imputar  )
ggplot(data = grafico1[grafico1$imputar != "Baja",], aes(x = x,y = y, col = imputar)) +
  geom_point()+
  facet_grid(cn01a~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

preds2 = predict(ranger2, data = data.control2)
grafico2 <- data.table(x = data.control2$cn01.fde, y = preds2$predictions, cn01a = data.control2$cn01a.anterior, resulta = data.control2$resulta, imputar = data.control2$imputar, cnaest = data.control2$cnaeestMIG, CCAA = data.control2$ccaa.anterior )


ggplot(data = grafico2[grafico2$imputar != 'No'], aes(x = x,y = y, col = imputar)) +
  geom_point()+
  facet_grid(cnaest~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

data.control2$CN01 = preds2$predictions

datacomplete2 <- rbind(data.train2,data.control2)

#ENVIO 3

regresoresRF <- merge.data.table(regresores3,FDEpreds,by = "numidest",all.x = T)
regresoresRF3 <- regresoresRF[,(dropvariables):=NULL]
regresoresRF3 <- regresoresRF[,(drop2):=NULL]
regresoresRF3 <- regresoresRF3[!is.na(intermensual.cn01)]
regresoresRF3 <- regresoresRF3[!is.na(imputar)]
regresoresRF3 <- regresoresRF3[!is.na(cn01.anterior)]
regresoresRF3 <- regresoresRF3[!is.na(interanual.cn01)]
regresoresRF3 <- regresoresRF3[!is.na(cn01a.anterior)]
regresoresRF3 <- regresoresRF3[!is.na(cn01v.anterior)]
regresoresRF3 <- regresoresRF3[!is.na(cn01e.anterior)]
regresoresRF3 <- regresoresRF3[!is.na(cn01.fde)]
missingsRF <- aggr(regresoresRF3,  col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 
missingsRF$missings

regresoresRF3 <- datacomplete2[regresoresRF3, on=.(numidest), CN01 := .(i.CN01)][]
data.train3 <- regresoresRF3[!is.na(regresoresRF3$CN01)]
data.control3 <- regresoresRF3[is.na(regresoresRF3$CN01)]
data.control3$imputado <- '3'



ranger3 <- ranger(CN01 ~ . -cn01.fde  , data = na.omit(data.train3), mtry = sqrt(ncol(data.train3)) ,  importance = "impurity")
ranger3
summary(ranger3)
varimportance3 <- as.data.frame(ranger2$variable.importance)
ranger3$prediction.error
ranger3$r.squared

ggplot(data = varimportance3, aes(x = rownames(varimportance3) ,y = `ranger2$variable.importance`))+
  geom_point()+
  theme(axis.text.x = element_text(size = 12,angle = 90,vjust =  0.5))

grafico1 <- data.frame(x = data.train3$CN01, y = ranger3$predictions, cn01a = data.train3$cn01a.anterior, resulta = data.train3$resulta, imputar = data.train3$imputar  )
ggplot(data = grafico1[grafico1$imputar != "Baja",], aes(x = x,y = y, col = imputar)) +
  geom_point()+
  facet_grid(cn01a~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

preds3 = predict(ranger3, data = data.control3)
grafico2 <- data.table(x = data.control3$cn01.fde, y = preds3$predictions, cn01a = data.control3$cn01a.anterior, resulta = data.control3$resulta, imputar = data.control3$imputar, cnaest = data.control3$cnaeestMIG, CCAA = data.control3$ccaa.anterior )


ggplot(data = grafico2[grafico2$imputar != 'No'], aes(x = x,y = y, col = imputar)) +
  geom_point()+
  facet_grid(cnaest~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

data.control3$CN01 = preds3$predictions

datacomplete3 <- rbind(data.train3,data.control3)
