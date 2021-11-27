#' Discharge time series plot
#'
#' @description Desciptive Statistics. Time Series of Discharge at specific station.
#'
#' @param data River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations
#' @param Name as character. Name of the River. e.g. "Mosel"
#' @param station as character. Name of the Station e.g. "COCHEM" - must be named the same like list entry in data.
#'
#' @return Graphic showing Discharge time series.
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{ Q_plot(mosel, "Mosel", "COCHEM)}
#'
Q_plot=function(data, Name, station ){


  nbr=which(names(data)==station)
  minDate= data[[nbr]][1,1]
  l=length(data[[nbr]][,1])
  maxDate=data[[nbr]][l,1]
  titl=paste("Discharge time series: ",Name, station , "start", minDate,"end", maxDate)

  plot= ggplot()+geom_line(data[[nbr]], mapping=aes(x=YYYY.MM.DD,y=Value, group=1, col="red"))+scale_x_date(name="Date")+labs(title=titl, subtitle="Datasource: GRDC- Dataset ")+theme(legend.position="none")
  return(plot)
}
