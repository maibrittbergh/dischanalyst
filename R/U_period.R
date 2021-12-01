#' Max undercut length of value U within all measurements
#'
#' @description Function indicates how long the longest period existed that a certain value was undercut (at a given measuring point of a given river),
#' within the time period of the measurements.
#'
#' @param U as numeric, Limit Value. Function returns longest time measured under Value U.
#' @param station as character. Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#'
#' @return
#' @export
#'
#'
#' @examples
#' \dontrun{ U_period(88, "TRIER UP", mosel)}

U_period=function(U,station, data){
  nbr=which(names(data)==station)
  Val=data[[nbr]][,2]
  uU=which(Val<U)
  l=length(uU)
  if (all(Val>U)) {paste("No days")}
  else {
    c=rep(0,l)
    for ( i in 1:l){
      c[i]=(uU[i+1]-uU[i])
    }
    index=cbind(c, uU)
    G=which(c>1)
    c[G]=0
    c[is.na(c)] = 0


    d=c
    l=length(c)
    e=rep(0,l)


    for (i in 2:l){
      e[1]=d[1]
      if ((e[i-1]+d[i]) > e[i-1]){e[i]=e[i-1]+d[i]}
      else {e[i]=0}
    }

    ic_max=which(e == (max(e)))
    id_End_period=index[,2][ic_max]
    id_Start_period=id_End_period-max(e)

    Startdate=data[[nbr]][,1][id_Start_period]
    Enddate=data[[nbr]][,1][id_End_period]

    paste("Length of maximum time period  under given Value of", U, "is", max(e), "days. Timeperiod from:",Startdate, "to:",Enddate)
  }

}
