
#' Sum of Days under U within all measurements
#'
#'
#' @param U numeric; Limit Value. Function returns sum of  days  Value U within all measurements.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#'
#' @return
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
