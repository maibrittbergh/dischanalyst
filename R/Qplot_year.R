#' Discharge Plot in specific year
#'
#' @param data River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param Name as character. Name of the River. e.g. "Mosel"
#' @param station as character. Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param year as numeric. a certain year within the time series since begin of measurements
#'
#' @return graphic of discharge time series in given year
#' @export
#'@import ggplot2


Qplot_year=function(data, Name, station, year){



  nbr=which(names(data)==station)
  j=grep(year, data[[nbr]][,1])
  new_data=data[[nbr]][j,]

  titl=paste("Discharge time series: ",year, Name,",",station)

  plot= ggplot()+geom_line(new_data, mapping=aes(x=YYYY.MM.DD,y=Value, group=1, col="red"))+scale_x_date(name="Date")+labs(title=titl, subtitle="Aus GRDC-Datensatz des BfG")+theme(legend.position="none")
  return(plot)
}
