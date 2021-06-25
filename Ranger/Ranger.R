library(ranger)
library(ggplot2)
setwd("M:/EIPI3/Lasai/")

regresores1 <- readRDS("Datos/regresores1.rds")
regresores2 <- readRDS("Datos/regresores2.rds")
regresores3 <- readRDS("Datos/regresores3.rds")
FDE <- readRDS("Datos/FDE.rds")
FDEpreds <- FDE[,list(numidest,cn01)]
names(FDEpreds)[2]="cn01.fde"
regresoresRF <- merge.data.table(regresores1,FDEpreds,by = "numidest",all.x = T)
drop2 <- c("cn02.anterior" ,"cn02a.anterior","cn02v.anterior","cn02e.anterior",
           "cn03.anterior" ,"cn03a.anterior", "cn03v.anterior", "cn03e.anterior",
           "cn04.anterior", "cn04a.anterior", "cn04v.anterior","cn04e.anterior",
           "cn05.anterior","cn05a.anterior", "cn05v.anterior","cn05e.anterior")

dropvariables = c('CN02','CN03','CN04','CN05',"cnaeest",'obsanual1','obsanual2',"resulta", "envioPID")

regresoresRF1 <- regresoresRF[,(dropvariables):=NULL]
regresoresRF1 <- regresoresRF[,(drop2):=NULL]
regresoresRF1 <- regresoresRF1[!is.na(cn01.fde)] #243 salen
regresoresRF1$cn01a.anterior <- as.factor(regresoresRF1$cn01a.anterior)
regresoresRF1$cn01e.anterior <- as.factor(regresoresRF1$cn01e.anterior)
regresoresRF1$cn01v.anterior <- as.factor(regresoresRF1$cn01v.anterior)
missingsRF <- aggr(regresoresRF1,  col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
missingsRF$missings #748 imputaciones

saveRDS(regresoresRF1, "Datos/RegresoresRF_envio1.rds")

data.train <- regresoresRF1[!is.na(regresoresRF1$CN01)]
data.train$imputado <- "No"

data.control <- regresoresRF1[is.na(regresoresRF1$CN01)]
data.control$imputado <- '1'

ranger <- ranger(CN01 ~ . -cn01.fde+-cn01e.anterior   , 
                 data = datos_train_train[,!c("numidest")],
                 mtry = (ncol(datos_train_train))/3 , 
                 importance = "permutation",
                 min.node.size = 5,
                 seed = 123)
# ranger <- ranger(CN01 ~ . -cn01.fde   , 
#                  data = datos_train_train[,!c("numidest","interanualCNAE", 'intermensual.cn03', 'intermensual.cn05','intermensual.cn04', 'interanual.cn04',"provem","imputado","cn01e.anterior", "actual")],
#                  mtry = (ncol(datos_train_train))/3 , 
#                  importance = "permutation",
#                  min.node.size = 5,
#                  seed = 123)

ranger
summary(ranger)
ranger$prediction.error
varimportance <- data.frame(importancia=sort(ranger$variable.importance, decreasing=TRUE))
ranger$r.squared
preds <- predict(ranger, data = datos_train_control)
test_rmse <-  sqrt(mean((preds$predictions - datos_train_control$CN01)^2))
paste("Error de test (rmse) del modelo:", round(test_rmse,2))




# Validación empleando el Out-of-Bag error (root mean squared error)
# ==============================================================================

# Valores evaluados
num_trees_range <- seq(1, 1000, 20)

# Bucle para entrenar un modelo con cada valor de num_trees y extraer su error
# de entrenamiento y de Out-of-Bag.

train_errors <- rep(NA, times = length(num_trees_range))
test_errors <- rep(NA, times = length(num_trees_range))
oob_errors   <- rep(NA, times = length(num_trees_range))

for (i in seq_along(num_trees_range)){
  modelo  <- ranger(CN01 ~ . -cn01.fde+-cn01e.anterior  , 
                    data = datos_train_train[,!c("numidest")],
                    mtry = (ncol(datos_train_train))/3 , 
                    importance = "none",
                    min.node.size = 5,
                    num.trees = num_trees_range[i],
                    oob.error = T,
                    seed = 123)
  
  predicciones_train <- predict(
    modelo,
    data = datos_train_train
  )
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - datos_train_train$CN01)^2)
  oob_error   <- modelo$prediction.error
  
  preds <- predict(modelo, data = datos_train_control)
  test_rmse <-  mean((preds$predictions - datos_train_control$CN01)^2)
  
  test_errors[i] <- sqrt(test_rmse)
  train_errors[i] <- sqrt(train_error)
  oob_errors[i]   <- sqrt(oob_error)
  
}
df_resulados <- data.frame(n_arboles = num_trees_range, train_errors, oob_errors, test_errors )
ggplot(data = df_resulados) +
  geom_line(aes(x = num_trees_range, y = train_errors, color = "train rmse")) + 
  geom_line(aes(x = num_trees_range, y = oob_errors, color = "oob rmse")) +
  geom_line(aes(x = num_trees_range, y = test_errors, color = "test error")) +
  geom_vline(xintercept = num_trees_range[which.min(oob_errors)],
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolución del error vs número árboles",
    x     = "número de árboles",
    y     = "Errores",
    color = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

paste("Valor óptimo de num.trees:", num_trees_range[which.min(oob_errors)])



hyper_grid <- expand.grid(
  mtry       = seq(15, 50, by = 2),
  node_size  = seq(1, 6, by = 1),
  OOB_RMSE   = 0
)
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = CN01 ~ . -cn01.fde+-cn01e.anterior , 
    data            = datos_train_train[,!c("numidest")], 
    num.trees       = 141,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    seed            = 123 # Notese el seteo de la semilla
  )
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
  
  predicciones_train <- predict(
    model,
    data = datos_train_train
  )
  predicciones_train <- predicciones_train$predictions
  
  train_error <- mean((predicciones_train - datos_train_train$CN01)^2)
  predicciones_test <- predict(
    model,
    data = datos_train_control
  )
  predicciones_test <- predicciones_test$predictions
  
  test_error <- mean((predicciones_test - datos_train_control$CN01)^2)
  hyper_grid$RMSE_Train[i] = train_error
  hyper_grid$RMSE_Test[i] =  test_error
}
grid <- data.table(mtry = hyper_grid$mtry, node.size = hyper_grid$node_size, OOB_RMSE = hyper_grid$OOB_RMSE, RMSE_Train = hyper_grid$RMSE_Train, RMSE_Test = hyper_grid$RMSE_Test)
ggplot(data = grid, aes(x = mtry, y = OOB_RMSE, colour = as.factor(node.size)))+
  geom_line()+
  geom_point(aes(shape = as.factor(node.size)))+
  theme_bw()

ggplot(data = grid, aes(x = mtry, y = RMSE_Train, colour = as.factor(node.size)))+
  geom_line()+
  geom_point(aes(shape = as.factor(node.size)))
ggplot(data = grid, aes(x = mtry, y = RMSE_Test, colour = as.factor(node.size)))+
  geom_line()+
  geom_point(aes(shape = as.factor(node.size)))


ggplot(data = varimportance, aes(x =factor(Variable, ordered = T) ,y = importancia))+
  geom_point()+
  theme(axis.text.x = element_text(size = 12,angle = 90,vjust =  0.5))


grafico1 <- data.table(x = data.train$CN01, y = ranger$predictions, cn01a = data.train$cn01a.anterior, resulta = data.train$resulta, imputar = data.train$imputar  )
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
regresoresRF2 <- regresoresRF2[!is.na(cn01.fde)]
missingsRF <- aggr(regresoresRF2,  col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 
missingsRF$missings

regresoresRF2 <- datacomplete1[regresoresRF2, on=.(numidest), CN01 := .(i.CN01)][]
data.train2 <- regresoresRF2[!is.na(regresoresRF2$CN01)]
data.control2 <- regresoresRF2[is.na(regresoresRF2$CN01)]
data.control2$imputado <- '2'



ranger2 <- ranger(CN01 ~ . -cn01.fde + -numidest +-cn01e.anterior , data = na.omit(data.train2), mtry = sqrt(ncol(data.train2)) ,  importance = "impurity")
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



ranger3 <- ranger(CN01 ~ . -cn01.fde +-numidest+-cn01e.anterior  , data = na.omit(data.train3), mtry = sqrt(ncol(data.train3)) ,  importance = "impurity")
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
saveRDS(datacomplete3, file = "datosimputados.rds" ) 
#COMPROBAR imputaciones
grafico4 <- data.table(x = datacomplete3$CN01, y = datacomplete3$cn01.fde, imputar = datacomplete3$imputado, canemig = datacomplete3$cnaeestMIG, resulta = datacomplete3$imputar ) 


ggplot(data = grafico4[grafico4$imputar != 'No'], aes(x = x,y = y, col = resulta)) +
  geom_point()+
  facet_grid(imputar~.,scales = "free")+
  geom_abline(intercept = 0,slope = 1)

rsquare <- matrix(c(ranger$prediction.error,ranger2$prediction.error,ranger3$prediction.error, 
                    ranger$r.squared,ranger2$r.squared,ranger3$r.squared),ncol = 3, byrow = T)
colnames(rsquare) <- c("Envio 1","Envio 2","Envio 3")
rownames(rsquare) <- c("Error de prediccion OOB", "R2")

#Calcular RDS con datacomplete

saveRDS(datacomplete1, file = "Nov20/datosimputados1.rds" ) 
saveRDS(datacomplete2, file = "Nov20/datosimputados2.rds" ) 
saveRDS(datacomplete3, file = "Nov20/datosimputados3.rds" ) 




r<- readRDS("datosimputados1.rds")
