#' Boxplot of Discharge Data
#'
#'@description  Desciptive Statistics. Boxplot of Time Series of Discharge at specific station. Including all measurements in list.
#'
#' @param data River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param station as character. Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param Name as character. Name of the River. e.g. "Mosel"
#'
#' @return Boxplot Graphic of Discharge time series.
#' @export
#'
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{QBoxplot(mosel, "Mosel", "COCHEM")}

QBoxplot=function(data, Name, station){
  titl=paste("Boxplot of", Name,",", station )
  plot=ggplot(data[[station]])+geom_boxplot(aes(y=Value, color="red"))+labs(title=titl, subtitle="From GRDC-Datensatz des BfG")+ theme(legend.position="none")
  return(plot)
}
