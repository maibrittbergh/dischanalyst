#' Calculates minimum Value for every year since the begin of the measurements. Returns Linear Model Coefficients for the Minimum Values over the years. Minimum Values ~ Years.
#'
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param Name character; Name of the Dataset. e.g. "Mosel"
#'
#' @return
#' @export
#'@import stats
#' @examples
#' \dontrun{ min_value_lm(mosel, "COCHEM", "Mosel")}

min_lm=function(data, station,Name) {
  nbr=which(names(data)==station)
  val=data[[nbr]]
  abs_min=min(data[[nbr]][,2])
  #Minima der Jahre
  year_one=as.numeric(substring(as.character(data[[nbr]][1,1]),1,4))
  length=length(data[[nbr]][,1])
  last_year=as.numeric(substring(as.character(data[[nbr]][length,1]),1,4))
  years=c(year_one:last_year)
  l=length(years)
  q_min=rep(0, l)
  for ( i in 1:l){
    year=as.character(years[i])
    j=grep(year, data[[nbr]][,1])
    Val=data[[nbr]][,2][j]
    q_min[i]=min(Val)
  }
  results=data.frame(years, q_min)
  model=lm(q_min~years, results)
  return(model$coefficients)
}
