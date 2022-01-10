#' Discharge Plot of the entire time series of measurements
#'
#' @description Desciptive Statistics. Time Series of Discharge at specific station. Including all measurements in list.
#'
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param Name character; Name of the River. e.g. "Mosel"
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data
#'
#' @return Graphic showing Discharge time series.
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{ Qplot(mosel, "COCHEM")}
#'
Qplot=function(data,  station ){

  nbr=which(names(data)==station)
  minDate= data[[nbr]][1,1]
  l=length(data[[nbr]][,1])
  maxDate=data[[nbr]][l,1]


min=as.numeric(substr(minDate, 1, 4))
max=as.numeric(substr(maxDate, 1, 4))
y=data[[nbr]]$Value
x=seq(from=min, to= max, length=l)


  titl=paste("Discharge time series: ", station , "start", minDate,"end", maxDate)




  plot= ggplot()+geom_line(data[[nbr]], mapping=aes(x=YYYY.MM.DD,y=Value, group=1, col="1"))+scale_x_date(name="Date")+
    labs(title=titl, subtitle="Datasource: GRDC- Dataset ")

print(plot)

  return(plot)
}



