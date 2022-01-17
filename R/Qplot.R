#' Discharge Plot of the entire time series of measurements
#'
#' @description Desciptive Statistics. Time Series of Discharge at specific station. Including all measurements in list.
#'
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
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


  titl=paste("Discharge Time Series at: ", station)

subtitl=paste("from", format(data[[station]][,1], "%Y"),"to", format(data[[station]][(nrow(data[[station]])),1] , "%Y") )


  plot= ggplot()+geom_line(data[[nbr]], mapping=aes(x=YYYY.MM.DD,y=Value, group=1, col="1"))+scale_x_date(name="Year")+
    labs(title=titl, subtitle=subtitl, y="Discharge Value")+theme(legend.position="none")


  return(plot)
}



