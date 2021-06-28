#### ENViO 2
library(haven)
library(ggpubr)
library(ranger)
library(ggplot2)
library(data.table)
setwd("M:/EIPI3/Lasai/")
data.train <- readRDS("Datos/Train2.rds")
data.control <- readRDS("Datos/Control2.rds")
data.train.dropped <- readRDS("Datos/data.train.dropped2.rds")
df_resulados <- as.data.table(readRDS("Datos/gridTrees2.rds"))
hyper_grid <- readRDS("Datos/hypergrid_envio2.rds")



pdf(file = "Imagenes/Envio2/ErrorVSnumtree2.pdf")
ggplot(data = df_resulados) +
  geom_line(aes(x = n_arboles, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = n_arboles, y = oob_errors, color = "oob rmse")) +
  geom_vline(xintercept = df_resulados[which.min(oob_errors)]$n_arboles,
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolucion del error vs numero de arboles",
    x     = "numero de arboles",
    y     = "RMSE",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


dev.off()

grid = as.data.table(readRDS("Datos/hypergrid_envio2.rds"))
grid$node_size <-  as.factor(grid$node_size)

pdf(file = "Imagenes/Envio2/OOBRMSEvsMTRY2.pdf")

ggplot(data = grid, aes(x = mtry, y = OOB_RMSE, colour = node_size))+
  geom_line()+
  geom_point(aes(shape = node_size))+
  geom_vline(xintercept = grid[which.min(OOB_RMSE)]$mtry,color = "firebrick",
             linetype = "dashed")+
  theme_bw()+
  ggtitle(label = "Error OOB vs Mtry by Node.size ",
          subtitle = paste(df_resulados[which.min(oob_errors)]$n_arboles," Arboles"))

dev.off()
pdf(file = "Imagenes/Envio2/RMSETrainvsMTRY2.pdf")

ggplot(data = grid, aes(x = mtry, y = RMSE_Train, colour = node_size))+
  geom_line()+
  geom_point(aes(shape = node_size))+
  geom_vline(xintercept = grid[which.min(RMSE_Train)]$mtry,color = "firebrick",
             linetype = "dashed")+
  theme_bw()+
  ggtitle(label = "Error Entrenamiento vs Mtry by Node.size ",
          subtitle = paste(df_resulados[which.min(oob_errors)]$n_arboles," Arboles"))

dev.off()

pdf(file = "Imagenes/Envio2/R2vsMTRY2.pdf")

ggplot(data = grid, aes(x = mtry, y = R2, colour = node_size))+
  geom_line()+
  geom_point(aes(shape = node_size))+
  geom_vline(xintercept = grid[which.max(R2)]$mtry,color = "firebrick",
             linetype = "dashed")+
  theme_bw()+
  ggtitle(label = "R2 vs Mtry by Node.size ",
          subtitle = paste(df_resulados[which.min(oob_errors)]$n_arboles," Arboles"))


dev.off()

optimumtree <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]
start <-Sys.time()
optimumranger <- ranger(
  formula         = CN01 ~ . -cn01.fde , 
  data            = data.train.dropped, 
  num.trees       = df_resulados[which.min(oob_errors)]$n_arboles,
  mtry            = optimumtree$mtry,
  min.node.size   = optimumtree$node_size,
  seed            = 123)
endtime <- Sys.time()
predicciones_optim <- predict(
  optimumranger,
  data = data.train
)
predicciones_train <- predicciones_optim$predictions
data.train$preds = predicciones_train


#ERROR
#RMSE
error <- (data.train$CN01-data.train$preds)^2

sqrt(sum(error)/length(error))

#MAE
sum(sqrt(error))/length(error)

##SCATTER PLOT

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
scatterplotPequeñas <- scatterplot[Real < 100000000]

pdf(file = "Imagenes/Envio2/GrandesScatter2.pdf")
ggplot(data = scatterplotGrandes, aes(x = Real,y = Prediccion)) +
  geom_point()+
  # facet_grid(CNAE1~CNAE2,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  ggtitle("Real vs Predicciones",
          "Unidades  grandes CN > 1e7")
# facet_grid(CCAA ~., scales = "free", switch = "y")
dev.off()
pdf(file = "Imagenes/Envio2/PequeñasScatter2.pdf")
ggplot(data = scatterplotPequeñas , aes(x = Real,y = Prediccion)) +
  geom_point()+
  # facet_grid(CNAE1~CNAE2,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  ggtitle("Real vs Predicciones",
          "Unidades pequeñas  CN < 1e7")
dev.off()
pdf(file = "Imagenes/Envio2/TotalScatterUmbral2.pdf")
ggplot(data = scatterplot, aes(x = Real,y = Prediccion, colour = Umbral)) +
  geom_point()+
  # facet_grid(CNAE1~CNAE2,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  ggtitle("Real vs Predicciones")
dev.off()



predictions <-predict(optimumranger,
                      data = data.control)
data.control$CN01 <- predictions$predictions

data.control$error <- abs(data.control$CN01 - data.control$cn01.fde)

datasubdivision <- data.control[,.(ErrorDivision=sum(error),CN01DivisionRF = sum(CN01), CN01DivisionINE = sum(cn01.fde)), by = cnaeestSub]
datasubdivision[,relativaRF := (CN01DivisionRF/sum(CN01DivisionRF)*100)]
datasubdivision[,relativaINE := (CN01DivisionINE/sum(CN01DivisionINE)*100)]
datasubdivision[,ErrorAbsRelativo := (ErrorDivision/sum(ErrorDivision)*100)]

tablalatex <- datasubdivision[,list(cnaeestSub,relativaRF,relativaINE,ErrorAbsRelativo)]
tablalatex <- tablalatex[order(ErrorAbsRelativo, decreasing =  T),]



preddata <- data.table(Imputacion.INE = data.control$cn01.fde,
                       Imputacion.RF = data.control$CN01,
                       Umbral = data.control$treshold, 
                       imputar =data.control$imputar,
                       cnaeMIG = data.control$cnaeestMIG, 
                       umbral = data.control$treshold,
                       CNAE3 = data.control$cnaeest3,
                       CNAESUB = data.control$cnaeestSub,
                       error = data.control$error)


pdf(file = "Imagenes/Envio2/ImputacioRFvsINE.pdf")

ggplot(data = preddata, aes(x = Imputacion.INE,y = Imputacion.RF)) +
  geom_point()+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))+
  
  ggtitle("Imputacion INE vs Imputacion RF",
  )
dev.off()
pdf(file = "Imagenes/Envio2/ImputacioRFvsINEUmbral.pdf")

ggplot(data = preddata, aes(x = Imputacion.INE,y = Imputacion.RF, colour = cnaeMIG )) +
  geom_point()+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))+
  
  facet_grid(umbral~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  ggtitle("Imputacion INE vs Imputacion RF",
  )
dev.off()
pdf(file = "Imagenes/Envio2/ImputacioRFvsINEMIG.pdf")

ggplot(data = preddata, aes(x = Imputacion.INE,y = Imputacion.RF, colour = cnaeMIG )) +
  geom_point()+
  # facet_grid(umbral~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme_bw()+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))+
  
  ggtitle("Imputacion INE vs Imputacion RF",
  )
dev.off()


pdf(file = "Imagenes/Envio2/ErrorINERF.pdf")

ggplot(data = preddata[error > sum(error)*0.01], aes(x = Imputacion.INE,y = Imputacion.RF, colour = CNAESUB)) +
  geom_point()+
  facet_grid(cnaeMIG~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))+
  
  theme_bw()+
  ggtitle("Real vs Predicciones")

dev.off()

##INDICES
# 
# indices <- readRDS("Datos/Indice_envio2.rds")
# indices$general
# indicesMIG <- indices$MIG
# 
# names(indices$MIG2)[1] = "MIG"
# indicesMIG <- rbind(indicesMIG, indices$MIG2[4])
# 
# general.indice <-as.data.table(read_sas("Nov20/INDICN1120201/in0.sas7bdat"))
# ccaa.indice <- as.data.table(read_sas("Nov20/INDICN1120201/in2ca.sas7bdat"))
# divisiones.indice <- as.data.table(read_sas("Nov20/INDICN1120201/in1.sas7bdat"))
# MIG.indice <-as.data.table(read_sas("Nov20/INDICN1120201/in2m.sas7bdat"))
# 
# general.indice <- general.indice[ano == 2020 & mes == 11]
# ccaa.indice <- ccaa.indice[ano == 2020 & mes == 11]
# MIG.indice <- MIG.indice[ano == 2020 & mes == 11]
# divisiones.indice <- divisiones.indice[ano == 2020 & mes == 11]
# 
# 
# pdf(file = "Imagenes/Envio2/IndiceMIG2.pdf", width = 4.72441,height = 2.75591)
# 
# color = indicesMIG$index - MIG.indice$IT
# color <- ifelse(color >0, "blue3","red3")
# ggplot(data = MIG.indice, aes(x = CODIGO, y = IT ))+
#   theme_bw()+
#   geom_segment( aes(x=CODIGO, xend=CODIGO, y=indicesMIG$index, yend=MIG.indice$IT),colour = color,linetype = "longdash", show.legend =  F,lineend = "round")+
#   ggtitle("Indice MIGs",
#           "Envio 2")+
#   geom_point(aes(colour = "INE"))+
#   geom_point(data = indicesMIG, aes(x = MIG, y = index, colour = 'RF'))
# 
# dev.off()
# pdf(file = "Imagenes/Envio2/IndiceCCAA2.pdf", width = 4.72441,height = 2.75591)
# 
# 
# color = indices$ccaa$index - ccaa.indice$IT
# color <- ifelse(color >0, "blue3","red3")
# 
# ggplot(data = ccaa.indice, aes(x = CODIGO, y = IT ))+
#   geom_segment( aes(x=CODIGO, xend=CODIGO, y=indices$ccaa$index , yend=ccaa.indice$IT),colour = color,linetype = "longdash", show.legend =  F,lineend = "round")+
#   geom_point(aes(colour = "INE"))+
#   geom_point(data = indices$ccaa, aes(x = ccaa, y = index, colour = 'RF'))+
#   theme_bw()+
#   ggtitle("Indice CCAA",
#           "Envio 2")
# dev.off()
# 
# pdf(file = "Imagenes/Envio2/IndiceGeneral2.pdf", width = 4.72441,height = 2.75591)
# color = indices$general$index - general.indice$INDICE0
# color <- ifelse(color >0, "blue3","red3")
# ggplot(data = general.indice, aes(x = "11/2", y = INDICE0))+
#   geom_segment( aes(x="11/2", xend="11/2", y=indices$general$index , yend=general.indice$INDICE0),colour = color,linetype = "longdash", show.legend =  F,lineend = "round")+
#   geom_point(aes(colour = "INE"))+
#   geom_point(data = indices$general, aes(x = "11/2", y = index, colour = "RF" ))+
#   theme_bw()+
#   ggtitle("Indice General",
#           "Envio 2")
# dev.off()
# 
# pdf(file = "Imagenes/Envio2/IndiceSeccion2.pdf", width = 4.72441,height = 2.75591)
# 
# color = indices$seccion$index - divisiones.indice$it
# color <- ifelse(color >0, "blue3","red3")
# 
# ggplot(data = divisiones.indice, aes(x = CODIGO, y = it ))+
#   geom_segment( aes(x=CODIGO, xend=CODIGO, y=indices$seccion$index , yend=divisiones.indice$it),colour = color,linetype = "longdash", show.legend =  F,lineend = "round")+
#   geom_point(aes(colour = "INE"))+
#   geom_point(data = indices$seccion, aes(x = seccion, y = index, colour = 'RF'))+
#   theme_bw()+
#   ggtitle("Indice Seccion",
#           "Envio 2")
# 
# dev.off()
