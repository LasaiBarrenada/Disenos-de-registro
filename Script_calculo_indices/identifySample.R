identifySample <- function(data_running, data_ref, idVar){
  
  # A partir de los conjuntos de datos data_running (mes en curso) y data_ref 
  # (mes anterior) se determina la muestra para el cómputo de los índices 
  # elementales utilizando los códigos de validez, de actualización, de 
  # actividad y comunidad autónoma
  data_running.dt <- dcast_StQ(data_running, UnitNames = TRUE)
  data_ref.dt <- dcast_StQ(data_ref, UnitNames = TRUE)
  
  for (idv in idVar){
    
    data_running.dt <- data_running.dt[get(idv) != '']
    data_ref.dt <- data_ref.dt[get(idv) != '']
  }
  
  # Condición 1: mismo numidest, ccaa, cnae a 3 dígitos
  temp_running.dt <- data_running.dt[
    , .(numidest, ccaa, acti)][
    , acti3 := substr(acti, 1, 3)][
    , acti := NULL]
  temp_ref.dt <- data_ref.dt[
    , .(numidest, ccaa, acti)][
    , acti3 := substr(acti, 1, 3)][
    , acti := NULL]
  sample.dt <- merge(temp_running.dt, temp_ref.dt, by = c('numidest', 'ccaa', 'acti3'))
  data_running.dt <- data_running.dt[sample.dt[, .(numidest)], on = 'numidest']
  data_ref.dt <- data_ref.dt[sample.dt[, .(numidest)], on = 'numidest']
  
  # Condicion2: running y ref: cn01v == 1 
  data_running_cond2.dt <- data_running.dt[cn01v == 1]
  data_ref_cond2.dt <- data_ref.dt[cn01v == 1]
  numidest_cond2.dt <- merge(
    data_running_cond2.dt[, ..idVar], data_ref_cond2.dt[, ..idVar], by = idVar)
  
  # Condicion3: running: cn01v == 1 y ref: cn01v == 2
  data_running_cond3.dt <- data_running.dt[cn01v == 1]
  data_ref_cond3.dt <- data_ref.dt[cn01v == 2]
  numidest_cond3.dt <- merge(
    data_running_cond3.dt[, ..idVar], data_ref_cond3.dt[, ..idVar], by = idVar)
  
  # Condicion4: running: cn01v = 1 y ref: cn01a = 3
  data_running_cond4.dt <- data_running.dt[cn01v == 1]
  data_ref_cond4.dt <- data_ref.dt[cn01a == 3]
  numidest_cond4.dt <- merge(
    data_running_cond4.dt[, ..idVar], data_ref_cond4.dt[, ..idVar], by = idVar)
  
  numidest_cond.dt <- rbindlist(list(numidest_cond2.dt, numidest_cond3.dt, numidest_cond4.dt))
  numidest_cond.dt <- numidest_cond.dt[!duplicated(numidest_cond.dt, by = idVar)]
  
  data_running.dt <- data_running.dt[numidest_cond.dt, on = idVar]
  data_ref.dt <- data_ref.dt[numidest_cond.dt, on = idVar]
  
  sample <- data_running.dt[, idVar, with = FALSE]
  
  return(sample)
}