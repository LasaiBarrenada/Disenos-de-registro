
indices1 <- readRDS("Datos/Indice_envio1.rds")

indices2 <- readRDS("Datos/Indice_envio2.rds")

indices3 <- readRDS("Datos/Indice_envio3.rds")


indicesMIG <- indices$MIG

names(indices$MIG2)[1] = "MIG"
indicesMIG <- rbind(indicesMIG, indices$MIG2[4])
general.indice <-as.data.table(read_sas("Nov20/INDICN1120201/in0.sas7bdat"))
general.indice <- general.indice[ano == 2020 & mes == 11]


index <- data.table(envio = c("1","2","3"), RF = c(indices1$general$index,indices2$general$index,indices3$general$index))

ggplot(data = index, aes(x = envio, y = RF) )+
  geom_point()+
  geom_hline(yintercept = general.indice$INDICE0)+
  theme_bw()
