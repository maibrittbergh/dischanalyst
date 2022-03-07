
#' Sum of Days under U within all measurements
#'
#'@description Function returns sum of  days  under value U within all measurements.
#'
#' @param U numeric; Limit Value.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; Contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#'
#' @return numeric; sum of days.
#' @export
#'
#' @examples
#' \dontrun{ Usum_period(88, "TRIER UP", mosel)}

Usum_period=function(U, station, data){
  nbr=which(names(data)==station)
  Val=data[[nbr]][,2]
  uU=which(Val<U)
  l=length(uU)
  if (all(Val>U)) {return(paste("No days"))}
  else {return(paste(l, "days were under given Value"))}
}
