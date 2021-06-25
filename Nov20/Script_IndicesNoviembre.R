
path_root   <- 'M:/EIPI3/Lasai'
path_data   <- file.path(path_root, 'Nov20')
path_source <- file.path(path_root, 'Script_calculo_indices')

source(file.path(path_source, 'identifySample.R'))
source(file.path(path_source, 'computeTotal.R'))
source(file.path(path_source, 'computeElementaryIndices.R'))
source(file.path(path_source, 'computeComposedIndices.R'))
source(file.path(path_source, 'computeRates.R'))
source(file.path(path_source, 'actiToSubdivi.R'))


envio <- readline("Select envio ")

if (envio == "01")
{
  data = "FFNov1"
}
if (envio == "02")
{
  data = "FFNov2"
}
if (envio == "03")
{
  data = "FFNov3"
}
data
DD_ICN <- RepoXLSToDD("M:/EIPI3/Lasai/FF/E30052.NombresVariables_V1.xlsx")

varNames  <- c('cn01', 'cn02', 'cn03', 'cn04', 'cn05')
cellNames <- c('ccaa', 'GRUPO')
GRUPO <- new(Class = 'Variable',
             UnitName = 'GRUPO',
             IDDD = 'ActivEcono',
             QualsValues = list(NOrden = '', TipoMicrodato = '13.', TareaProceso = '6.', RolProceso = '6.', Clasificacion = '2.6.4.1.', EsModif ='0'),
             Length = '3',
             ClassVar = 'character',
             ValueRegExp = '[.]+',
             Formula = as.call(list('actiToSubdivi(ActivEcono_35._6._2.1.2._2.1.4._)')),
             SlotName = 'MicroData',
             Literal = '',
             DDversion = '1')


FF_StQ_Octubre <- ReadRepoFile(
  'FFOct', DD_ICN, perl = TRUE)[
    NOrden != '']

FF_StQ_envio1 <- ReadRepoFile(
  data, DD_ICN, perl = TRUE)[
    NOrden != ''] #Microdatos StQ

sample_ICN <- identifySample(FF_StQ_Octubre, FF_StQ_envio1, idVar = 'norden')

FF_StQ_envio1 <- setVariable(FF_StQ_envio1, GRUPO)
FF_StQ_Octubre <- setVariable(FF_StQ_Octubre, GRUPO)

total_running <- computeTotal(FF_StQ_envio1, sample_ICN, varNames, cellNames)
setnames(total_running, 'GRUPO', 'codigo')
total_ref     <- computeTotal(FF_StQ_Octubre, sample_ICN, varNames, cellNames)
setnames(total_ref, 'GRUPO', 'codigo')

indices_ref.dt <- as.data.table(read_sas(file.path(path_source, 'indices_icn.sas7bdat')))[
  mes == '01' & ano == '2021'][
    , c('mes', 'ano') := NULL] #indices mes anterior

ind_names <- paste0('cn0', 1:5)
names(ind_names) <- c( "INDICE0", "IMI0", "IZE0", "IZNE0", "IRM0")
indices_ref.dt <- melt(
  indices_ref.dt, id.vars = c('ccaa', 'codigo'), 
  measure.vars = c( "INDICE0", "IMI0", "IZE0", "IZNE0", "IRM0"),
  value.name = 'index')[
    , variable := ind_names[variable]]


elemIndices.dt <- computeElementaryIndices(total_running, total_ref, indices_ref.dt)


pond_fund.dt <- as.data.table(read_sas(file.path(path_source, 'pond_divi_ccaa.sas7bdat')))
pond_fund_names <- c('Total', 'Int', 'Euro', 'NoEuro', 'RM', 'Ext', 'ExtNoEuro')
names(pond_fund_names) <- c( "pont", "PONMI", "PONZE", "PONZNE", "PONRM", "PONME", "PONMENE")
pond_fund.dt <- melt(
  pond_fund.dt, id.vars = c('ccaa', 'codigo'),
  measure.vars = names(pond_fund_names),
  value.name = 'pond')[
    , mercado := pond_fund_names[variable]][
      , variable := NULL]
setcolorder(pond_fund.dt, c('ccaa', 'codigo', 'mercado', 'pond'))

codigo_pond_fund_comp <- c('10', '13', '20', '25', '26', '27', '30', '32')


pond_elem_ccaa_codigo.dt <- pond_fund.dt[
  mercado == 'Total'][
    !codigo %chin% codigo_pond_fund_comp][
      , variable := 'cn01'][
        , mercado := NULL]
setcolorder(pond_elem_ccaa_codigo.dt, c('ccaa', 'codigo', 'variable', 'pond'))
pond_positiva.dt <- pond_elem_ccaa_codigo.dt[pond > 0, .(ccaa, codigo)]
elemIndices.dt <- merge(elemIndices.dt, pond_positiva.dt, by = c('ccaa', 'codigo'), all.y = TRUE)
tempList <- vector('list', length(varNames))
names(tempList) <- varNames
for (var in varNames){
  
  tempList[[var]] <- elemIndices.dt[
    is.na(index)][
      , c('variable', 'index') := NULL][
        , variable := var]
  
}
pond_positiva.dt <- rbindlist(tempList)[
  , index := 1e-10]
elemIndices.dt <- elemIndices.dt[!is.na(variable)]
elemIndices.dt <- rbindlist(list(elemIndices.dt, pond_positiva.dt))[
  order(ccaa, codigo)]




ICN_dissemPlan <- list(
  ccaa       = data.table(ccaa = str_pad(1:17, 2, 'left', '0')),
  div_subdiv = data.table(codigo = c("05",  "06",  "07",  "08",  "10A", "10B", "11",  "12", "13A", "13B", "14",  "15",  "16", 
                                     "17",  "18",  "19" , "20A", "20B", "21",  "22",  "23", "24",  "25A", "25B", "26A", "26B",
                                     "26C", "27A", "27B", "28",  "29",  "30A", "30B", "31", "32A", "32B", "32C", "33")),
  division   = data.table(codigo = c("05",  "06",  "07",  "08",  "10A", "10B", "11",  "12", "13A", "13B", "14",  "15",  "16", 
                                     "17",  "18",  "19" , "20A", "20B", "21",  "22",  "23", "24",  "25A", "25B", "26A", "26B",
                                     "26C", "27A", "27B", "28",  "29",  "30A", "30B", "31", "32A", "32B", "32C", "33"),
                          division = c("05", "06", "07", "08", "10", "10", "11", "12", "13", "13", "14", "15", "16", 
                                       "17", "18", "19" , "20", "20", "21", "22", "23", "24", "25", "25", "26", "26",
                                       "26", "27", "27", "28", "29", "30", "30", "31", "32", "32", "32", "33")),
  seccion     = data.table(division = c("05", "06", "07", "08", "10", "11", "12", "13", "14", "15", "16", 
                                        "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27",
                                        "28", "29", "30", "31", "32", "33"),
                           seccion  = c(rep('B', 4), rep('C', 24))),
  general    = data.table(seccion = c('B', 'C'), general = c('general', 'general')),
  MIG        = data.table(codigo = c("05",  "06",  "07",  "08",  "10A", "10B", "11",  "12", "13A", "13B", "14",  "15",  "16", 
                                     "17",  "18",  "19" , "20A", "20B", "21",  "22",  "23", "24",  "25A", "25B", "26A", "26B",
                                     "26C", "27A", "27B", "28",  "29",  "30A", "30B", "31", "32A", "32B", "32C", "33"),
                          MIG    = c('EN', 'EN', 'BI', 'BI', 'CN',  'BI', 'CN', 'CN',  'BI',  'CN', 'CN', 'CN', 'BI',
                                     'BI',  'CN',  'EN', 'CN', 'BI',  'CN',  'BI', 'BI', 'BI', 'BC', 'BI', 'BI', 'BC',
                                     'CD', 'CD', 'BI', 'BC', 'BC', 'BC', 'CD', 'CD', 'CD', 'CN', 'BC', 'BC')),
  MIG2       = data.table(MIG = c('EN', 'BI', 'CD', 'CN', 'BC'), MIG2 = c('EN', 'BI', 'XC', 'XC', 'BC')),
  Total      = data.table(MIG2 = c('EN', 'BI', 'XC', 'BC'), total = c('total', 'total', 'total', 'total')))


indices <- computeComposedIndices(elemIndices.dt, weights = pond_elem_ccaa_codigo.dt, cells = ICN_dissemPlan)

indices$ccaa
indices$Total
indices$div_subdiv

saveRDS(indices,"Datos/Indice_envio3.rds")
