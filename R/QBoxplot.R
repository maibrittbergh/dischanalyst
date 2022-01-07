#' Discharge Boxplot of measured Values within entire time series of measurements
#'
#'@description  Desciptive Statistics. Boxplot of Time Series of Discharge at specific station. Including all measurements in list.
#'
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param Name character; Name of the River. e.g. "Mosel"
#'
#' @return Boxplot Graphic of Discharge time series. Using \link[ggplot2]{geom_boxplot}
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{ QBoxplot(mosel, "COCHEM")}

QBoxplot=function(data,  station){
  titl=paste("Boxplot of",  station )
  plot=ggplot(data[[station]])+geom_boxplot(aes(y=data[[station]][,2], color="red"))+labs(title=titl, subtitle="GRDC-Dataset by the BfG")+theme(legend.position="none")
  return(plot)
}
