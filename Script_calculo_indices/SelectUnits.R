#' Selección de unidades en objetos StQ y StQList.
#'
#' \code{SelectUnits} Selecciona las unidades en objetos de clase StQ y StQLsit
#' que verifican un conjunto de condiciones.
#'
#' @param object Objeto de clase \linkS4class{StQ} o \linkS4class{StQList} con
#' el conjunto de datos del que se quiere realizar la selección de unidades.
#'
#' @param Condition \code{vector} de tipo \code{character} con la expresión que
#' define la condición o condiciones que deben cumplir las unidades que se
#' desea seleccionar.
#'
#' @param UnitNames \code{Vector} de tipo \code{logical} y longitud 1 indicando
#' si los nombres que se utilizan son los de la columna UnitNames del diccionario
#' de datos de la operación estadística (TRUE) o su nombre en notación del
#' repositorio (FALSE).
#'
#' @return \linkS4class{data.table} con las unidades que cumplen las conodiciones
#' especificadas.
#'
#' @examples
#' \dontrun{
#' SelectUnits(object, Conditions, UnitNames = TRUE)
#' }
#'
#' @export
setGeneric("SelectUnits", function(object, Conditions, UnitNames = TRUE){standardGeneric("SelectUnits")})
#' @rdname SelectUnits
#'
#' @import data.table StQ
#'
#'
#' @export
setMethod(
  f = "SelectUnits",
  signature = c("StQ", "character", "logical"),
  function(object, Conditions, UnitNames = TRUE){

    DD <- getDD(object)
    IDQuals <- getIDQual(object, 'MicroData')
    object.dt <- dcast_StQ(object)

    if (UnitNames) setnames(object.dt, names(object.dt), UnitToIDDDNames(names(object.dt)))

    out <- object.dt[eval(parse(text = Conditions)), (IDQuals) , with = FALSE]
    # out <-lapply(Conditions, function(Cond){
    #   object.dt[eval(parse(text = Cond)), (IDQuals) , with = FALSE]
    # })
    return(out)
})
