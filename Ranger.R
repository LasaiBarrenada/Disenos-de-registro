library(ranger)
setwd("M:/EIPI3/Lasai/")
source("Disenos de registro/Regresores.R")
#Prueba
dropvariables = c('numidest','CN02','CN03','CN04','CN05','cnaeest','obsanual1','obsanual2')
regresoresRF1 <- regresores1[,(dropvariables):=NULL]

regresoresRF1 <- regresoresRF1[!is.na(intermensual.cn01)]
regresoresRF1 <- regresoresRF1[!is.na(imputar)]
regresoresRF1 <- regresoresRF1[!is.na(cn01.anterior)]
regresoresRF1 <- regresoresRF1[!is.na(interanual.cn01)]

data.train <- regresoresRF1[!is.na(regresoresRF1$CN01)]


missingsRF <- aggr(regresoresRF1,  col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 
missingsRF$missings

ranger <- ranger(CN01 ~ .  , data = na.omit(data.train), importance = "impurity")

summary(ranger)
ranger$variable.importance
ranger$prediction.error
ranger$r.squared


preds = predict(ranger, data = data.train)
preds$predictions
ranger$predictions

tabla = data.table()
tabla$pred = preds$predictions
tabla$real = data.train$CN01

summary(tabla)
