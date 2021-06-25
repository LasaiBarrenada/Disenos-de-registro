computeElementaryIndices <- function(total_running, total_ref, indices_ref){
  
  setnames(total_running, 'total', 'total_running')
  setnames(total_ref, 'total', 'total_ref')
  temp_data.dt <- merge(total_running, total_ref, 
                        by = intersect(names(total_running), names(total_ref)))[
    , ratio := ifelse(abs(total_ref) > 1e-3, total_running / total_ref, 1e-8)][
    , c('total_running', 'total_ref') := NULL]
  index.dt <- merge(temp_data.dt, indices_ref, 
                        by = intersect(names(temp_data.dt), names(indices_ref)))[
    , index := index * ratio][
    , ratio := NULL]
  
  return(index.dt)
}