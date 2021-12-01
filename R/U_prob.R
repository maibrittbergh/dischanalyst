#' Probability to to have a smaller Discharge Value than U
#'
#' @param U numeric; Limit Value. Function returns possibility to receive Value smaller than U.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{ U_prob(88, "TRIER UP", mosel)}
#'
U_prob=function(U, station, data){
  nbr=which(names(data)==station)
  Values=sort(data[[nbr]][,2])
  l=length(Values)
  under=which(Values< U)
  lu=length(under)
  p=round((lu/l)*100,2)
  return(paste("The propability to have a Discharge value smaller than", U, "is", p, "%. At", station))
}
