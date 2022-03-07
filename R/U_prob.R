#' Probability to fall below U
#'
#' @description Function returns probability to to receive a smaller Discharge Value than U
#'
#' @param U numeric; Limit Value. Function returns possibility to receive Value smaller than U.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#'
#' @return Probability
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
  return(paste("The probability to have a Discharge value smaller than", U, "is", p, "%. At", station))
}

