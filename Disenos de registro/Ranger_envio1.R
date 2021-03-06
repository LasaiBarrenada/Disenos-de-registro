library(ranger)
library(ggplot2)
library(VIM)
setwd("M:/EIPI3/Lasai/")

regresores1 <- readRDS("Datos/regresoresenvio1BIEN.rds")
FDE <- readRDS("Datos/FDE.rds")
FDEpreds <- FDE[,list(numidest,cn01)]
names(FDEpreds)[2]="cn01.fde"
regresoresRF <- merge.data.table(regresores1,FDEpreds,by = "numidest",all.x = T)
drop2 <- c("cn02.anterior" ,"cn02a.anterior","cn02v.anterior","cn02e.anterior",
           "cn03.anterior" ,"cn03a.anterior", "cn03v.anterior", "cn03e.anterior",
           "cn04.anterior", "cn04a.anterior", "cn04v.anterior","cn04e.anterior",
           "cn05.anterior","cn05a.anterior", "cn05v.anterior","cn05e.anterior")

dropvariables = c('CN02','CN03','CN04','CN05','obsanual1','obsanual2',"resulta", "envioPID")

regresores1 <- regresoresRF[,(dropvariables):=NULL]
regresores1 <- regresoresRF[,(drop2):=NULL]
regresores1 <- regresores1[!is.na(cn01.fde)] #243 salen
regresores1$cn01a.anterior <- as.factor(regresores1$cn01a.anterior)
regresores1$cn01e.anterior <- as.factor(regresores1$cn01e.anterior)
regresores1$cn01v.anterior <- as.factor(regresores1$cn01v.anterior)
missingsRF <- aggr(regresores1,  col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
missingsRF$missings #1400 imputaciones




library(ggpubr)
library(ranger)
library(ggplot2)
library(data.table)
regresoresRF1 <- regresores1

regresoresRF1[imputar == 'Cero']$CN01 = 0
data.train <- regresoresRF1[!is.na(regresoresRF1$CN01) & imputar != "Si"]
data.train$imputado <- "No"

data.control <- regresoresRF1[is.na(regresoresRF1$CN01) | imputar == "Si"]
data.control$imputado <- "Si"
starttime1 <- Sys.time()
ranger <- ranger(CN01 ~ . -cn01.fde  , 
                 data = data.train[,!c("numidest","quartile")],
                 mtry = (ncol(data.train[,!c("numidest","quartile")]))/3 , 
                 importance = "permutation",
                 min.node.size = 5,
                 seed = 123)

endtime1 <- Sys.time()
starttime1-endtime1

sqrt(ranger$prediction.error)
varimportance <- data.frame(importancia=sort(ranger$variable.importance, decreasing=TRUE))
ranger$r.squared
preds <- predict(ranger, data = data.train)
MAEdefault = sum(abs(data.train$CN01 - preds$predictions)/nrow(data.train))
test_rmse <-  sqrt(mean((preds$predictions - data.train$CN01)^2))
paste("Error de train (rmse) del modelo:", round(test_rmse,2))

list_features=data.frame("feature"= names(ranger$variable.importance),"value"=unname(ranger$variable.importance))

pdf(file = "Imagenes/TopVariables.pdf", width = 4.72441,height = 2.75591)
list_features %>%
  dplyr::arrange(desc(value)) %>%
  dplyr::top_n(10) %>%
  ggplot(aes(reorder(feature, value), value)) +
  geom_col() +
  coord_flip() +
  ggtitle("10 variables mas importantes")+
  xlab("Variable")+
  ylab("Importancia (permutacion)")+
  theme_bw()
dev.off()
pdf(file = "Imagenes/BotVariables.pdf",width = 4.72441,height = 2.75591)
list_features %>%
  dplyr::arrange(desc(value)) %>%
  dplyr::top_n(-10) %>%
  ggplot(aes(reorder(feature, -value), value)) +
  geom_col() +
  coord_flip() +
  ggtitle("10 variables menos importantes")+
   xlab("Variable")+
   ylab("Importancia (permutacion)")+
  theme_bw()
dev.off()

 worstfeatures <- list_features %>%
   dplyr::arrange(desc(value)) %>%
   dplyr::top_n(-10) 

variablesdrop <- worstfeatures[,"feature"] 
variablesdrop<- append(variablesdrop,c("numidest","quartile"))
data.train.dropped <- data.train
data.train.dropped <- data.train[,!c("numidest","quartile")]

# Validación empleando el Out-of-Bag error (root mean squared error)
# ==============================================================================

# Valores evaluados
num_trees_range <- seq(1, 1000, 20)

# Bucle para entrenar un modelo con cada valor de num_trees y extraer su error
# de entrenamiento y de Out-of-Bag.

train_errors <- rep(NA, times = length(num_trees_range))
oob_errors   <- rep(NA, times = length(num_trees_range))

for (i in seq_along(num_trees_range)){
  modelo  <- ranger(CN01 ~ . -cn01.fde, 
                    data = data.train.dropped,
                    mtry = (ncol(data.train.dropped))/3 , 
                    importance = "none",
                    min.node.size = 5,
                    num.trees = num_trees_range[i],
                    oob.error = T,
                    seed = 123)
  
  predicciones_train <- predict(
    modelo,
    data = data.train
  )
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - data.train$CN01)^2)
  oob_error   <- modelo$prediction.error
  

  train_errors[i] <- sqrt(train_error)
  oob_errors[i]   <- sqrt(oob_error)
  
}
df_resulados <- data.frame(n_arboles = num_trees_range, train_errors, oob_errors )


pdf(file = "Imagenes/ErrorVSnumtree.pdf",width = 4.72441,height = 2.75591)
ggplot(data = df_resulados) +
  geom_line(aes(x = num_trees_range, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_trees_range, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept = 221,
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolucion del error vs n�mero �rboles",
    x     = "numero de �boles",
    y     = "RMSE",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


dev.off()

paste("Valor �ptimo de num.trees:", 221)



hyper_grid <- expand.grid(
  mtry       = seq(5, 50, by = 2),
  node_size  = seq(1, 6, by = 1),
  OOB_RMSE   = 0,
  R2 = 0
)
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = CN01 ~ . -cn01.fde , 
    data            = data.train.dropped, 
    num.trees       = 221,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    seed            = 123 ,# Notese el seteo de la semilla,
    importance = "none"
  )
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
  
  predicciones_train <- predict(
    model,
    data = data.train
  )
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - data.train$CN01)^2)
  hyper_grid$RMSE_Train[i] = train_error
  hyper_grid$R2[i] = model$r.squared 
}
grid <- data.table(mtry = hyper_grid$mtry, node.size = as.factor(hyper_grid$node_size), OOB_RMSE = hyper_grid$OOB_RMSE, RMSE_Train = hyper_grid$RMSE_Train, R2 = hyper_grid$R2)

pdf(file = "Imagenes/OOBRMSEvsMTRY.pdf",width = 4.72441,height = 2.75591)

ggplot(data = grid, aes(x = mtry, y = OOB_RMSE, colour = node.size))+
  geom_line()+
  geom_point(aes(shape = node.size))+
  geom_vline(xintercept = grid[which.min(OOB_RMSE)]$mtry,color = "firebrick",
             linetype = "dashed")+
  theme_bw()+
  ggtitle(label = "Error OOB vs Mtry by Node.size ",
          subtitle = paste(221," Arboles"))

dev.off()
pdf(file = "Imagenes/RMSETrainvsMTRY.pdf",width = 4.72441,height = 2.75591)

ggplot(data = grid, aes(x = mtry, y = RMSE_Train, colour = node.size))+
  geom_line()+
  geom_point(aes(shape = node.size))+
  geom_vline(xintercept = grid[which.min(RMSE_Train)]$mtry,color = "firebrick",
             linetype = "dashed")+
  theme_bw()+
  ggtitle(label = "Error Entrenamiento vs Mtry by Node.size ",
          subtitle = paste(221," Arboles"))

dev.off()

pdf(file = "Imagenes/R2vsMTRY.pdf",width = 4.72441,height = 2.75591)

ggplot(data = grid, aes(x = mtry, y = R2, colour = node.size))+
  geom_line()+
  geom_point(aes(shape = node.size))+
  geom_vline(xintercept = grid[which.max(R2)]$mtry,color = "firebrick",
             linetype = "dashed")+
  theme_bw()+
  ggtitle(label = "R2 vs Mtry by Node.size ",
          subtitle = paste(221," Arboles"))


dev.off()
optimumtree <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]

starttime <- Sys.time()
optimumranger <- ranger(
    formula         = CN01 ~ . -cn01.fde , 
    data            = data.train.dropped, 
    num.trees       = 221,
    mtry            = optimumtree$mtry,
    min.node.size   = optimumtree$node_size,
    seed            = 123,
    importance = "permutation"# Notese el seteo de la semilla
  )
endtime <- Sys.time()
starttime-endtime
optimumranger
optimumranger
sqrt(optimumranger$prediction.error)

predicciones_optim <- predict(
  optimumranger,
  data = data.train
)
predicciones_train <- predicciones_optim$predictions

error = data.train$CN01 - predicciones_train
MAE = sum(abs(error))/nrow(data.train)
##SCATTER PLOT
data.train$preds = predicciones_train

scatterplot <- data.table(Real = data.train$CN01, 
                          Prediccion = optimumranger$predictions,
                          CNAE1 = data.train$cnaeest1 ,
                          CNAE2 = data.train$cnaeest2,
                          CNAE = data.train$acti.anterior,
                          CCAA = data.train$ccaa.anterior,
                          imputar = data.train$imputar,
                          CNAEMIG = data.train$cnaeestMIG,
                          CNAESUB = data.train$cnaeestSub,
                          Umbral = data.train$treshold)

scatterplotGrandes <- scatterplot[Real > 100000000]
scatterplotPeque�as <- scatterplot[Real < 100000000]

pdf(file = "Imagenes/GrandesScatter.pdf",width = 4.72441,height = 2.75591)
ggplot(data = scatterplotGrandes, aes(x = Real,y = Prediccion)) +
  geom_point()+
  # facet_grid(CNAE1~CNAE2,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  ggtitle("Real vs Predicciones",
         "Unidades  grandes CN > 1e7")
  # facet_grid(CCAA ~., scales = "free", switch = "y")
dev.off()
pdf(file = "Imagenes/Peque�asScatter.pdf",width = 4.72441,height = 2.75591)
ggplot(data = scatterplotPeque�as , aes(x = Real,y = Prediccion)) +
  geom_point()+
  # facet_grid(CNAE1~CNAE2,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
    ggtitle("Real vs Predicciones",
          "Unidades peque�as  CN < 1e7")
dev.off()
pdf(file = "Imagenes/TotalScatterUmbral.pdf",width = 4.72441,height = 2.75591)
ggplot(data = scatterplot, aes(x = Real,y = Prediccion, colour = Umbral)) +
  geom_point()+
  # facet_grid(CNAE1~CNAE2,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  ggtitle("Real vs Predicciones")
dev.off()
ggarrange(grandes,peque�as)+
  ggtitle(" Valor Real vs Predicciones")






predictions <-predict(optimumranger,
                      data = data.control)
data.control$CN01 <- predictions$predictions

options(scipen = 100)

data.control = data.control[imputar == 'Si']
data.control$error = error 
RMSE <- sqrt(sum((data.control$CN01 - data.control$cn01.fde)^2)/nrow(data.control))
MAE <- sum(abs(data.control$CN01 - data.control$cn01.fde))/nrow(data.control)

error = data.control$CN01 - data.control$cn01.fde
error <- data.table(error = error, MIG = data.control$cnaeestMIG)
ggplot(error, aes(x = error)) + 
  geom_histogram(binwidth = 1000000, color = "black", fill = "darkgrey" )+
  facet_grid(MIG~., scales = "free")
  theme_bw()
  pdf(file = "Imagenes/ErrorImputaciones.pdf",width = 4.72441,height = 2.75591)
  
  ggplot(error, aes(x = error)) + 
    geom_histogram(binwidth = 1000000, color = "black", fill = "darkgrey" )+
  
  theme_bw()
  dev.off()

preddata <- data.table(Imputacion.INE = data.control$cn01.fde, Imputacion.RF = data.control$CN01, Umbral = data.control$treshold, imputar =data.control$imputar, cnaeMIG = data.control$cnaeestMIG, umbral = data.control$treshold)


pdf(file = "Imagenes/ImputacioRFvsINE.pdf",width = 4.72441,height = 2.75591)

ggplot(data = preddata, aes(x = Imputacion.INE,y = Imputacion.RF)) +
  geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  ggtitle("Imputacion INE vs Imputacion RF",
  )
dev.off()
pdf(file = "Imagenes/ImputacioRFvsINEUmbral.pdf",width = 4.72441,height = 2.75591)

ggplot(data = preddata, aes(x = Imputacion.INE,y = Imputacion.RF, colour = cnaeMIG )) +
  geom_point()+
  facet_grid(umbral~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  ggtitle("Imputacion INE vs Imputacion RF",
          )
dev.off()




dataindices <- rbind(data.control, data.train[,!c("preds")])

indices <- readRDS("Datos/Indice_envio1nuevo.rds")
indices$general
indicesMIG <- indices$MIG

names(indices$MIG2)[1] = "MIG"
indicesMIG <- rbind(indicesMIG, indices$MIG2[4])

general.indice <-as.data.table(read_sas("Nov20/INDICN1120201/in0.sas7bdat"))
ccaa.indice <- as.data.table(read_sas("Nov20/INDICN1120201/in2ca.sas7bdat"))
divisiones.indice <- as.data.table(read_sas("Nov20/INDICN1120201/in1.sas7bdat"))
MIG.indice <-as.data.table(read_sas("Nov20/INDICN1120201/in2m.sas7bdat"))

general.indice <- general.indice[ano == 2020 & mes == 11]
ccaa.indice <- ccaa.indice[ano == 2020 & mes == 11]
MIG.indice <- MIG.indice[ano == 2020 & mes == 11]
divisiones.indice <- divisiones.indice[ano == 2020 & mes == 11]


pdf(file = "Imagenes/IndiceMIG.pdf", width = 4.72441,height = 2.75591)

color = indicesMIG$index - MIG.indice$IT
color <- ifelse(color >0, "blue3","red3")
ggplot(data = MIG.indice, aes(x = CODIGO, y = IT ))+
  theme_bw()+
  geom_segment( aes(x=CODIGO, xend=CODIGO, y=indicesMIG$index, yend=MIG.indice$IT),colour = color,linetype = "longdash", show.legend =  F,lineend = "round")+
  ggtitle("Indice MIGs",
          "Envio 1")+
  geom_point(aes(colour = "INE"))+
  geom_point(data = indicesMIG, aes(x = MIG, y = index, colour = 'RF'))

dev.off()
pdf(file = "Imagenes/IndiceCCAA.pdf", width = 4.72441,height = 2.75591)


color = indices$ccaa$index - ccaa.indice$IT
color <- ifelse(color >0, "blue3","red3")

ggplot(data = ccaa.indice, aes(x = CODIGO, y = IT ))+
  geom_segment( aes(x=CODIGO, xend=CODIGO, y=indices$ccaa$index , yend=ccaa.indice$IT),colour = color,linetype = "longdash", show.legend =  F,lineend = "round")+
  geom_point(aes(colour = "INE"))+
  geom_point(data = indices$ccaa, aes(x = ccaa, y = index, colour = 'RF'))+
  theme_bw()+
  ggtitle("Indice CCAA",
          "Envio 1")
dev.off()

pdf(file = "Imagenes/IndiceGeneral.pdf", width = 4.72441,height = 2.75591)
color = indices$general$index - general.indice$INDICE0
color <- ifelse(color >0, "blue3","red3")
ggplot(data = general.indice, aes(x = "11/1", y = INDICE0))+
  geom_segment( aes(x="11/1", xend="11/1", y=indices$general$index , yend=general.indice$INDICE0),colour = color,linetype = "longdash", show.legend =  F,lineend = "round")+
  geom_point(aes(colour = "INE"))+
  geom_point(data = indices$general, aes(x = "11/1", y = index, colour = "RF" ))+
  theme_bw()+
  ggtitle("Indice General",
          "Envio 1")
dev.off()

pdf(file = "Imagenes/IndiceSeccion.pdf", width = 4.72441,height = 2.75591)

color = indices$seccion$index - divisiones.indice$it
color <- ifelse(color >0, "blue3","red3")

ggplot(data = divisiones.indice, aes(x = CODIGO, y = it ))+
  geom_segment( aes(x=CODIGO, xend=CODIGO, y=indices$seccion$index , yend=divisiones.indice$it),colour = color,linetype = "longdash", show.legend =  F,lineend = "round")+
  geom_point(aes(colour = "INE"))+
  geom_point(data = indices$seccion, aes(x = seccion, y = index, colour = 'RF'))+
  theme_bw()+
  ggtitle("Indice Seccion",
          "Envio 1")

dev.off()


mean(indices$ccaa$index / ccaa.indice$IT)
