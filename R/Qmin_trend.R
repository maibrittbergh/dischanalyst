#' Trend of Annual Discharge Minima since begin of Measurements
#'
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param Name character; Name of the River. e.g. "Mosel"
#'
#' @return
#' @export
#' @import ggplot2
#' @importFrom zyp zyp.trend.vector
#'@import Kendall
#'
#' @examples
#' \dontrun{ Qmin_trend(mosel, "COCHEM", "MOSEL")}
#'
Qmin_trend=function(data, station,Name) {
  nbr=which(names(data)==station)
  val=data[[nbr]]
  abs_min=min(data[[nbr]][,2])

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

model=zyp.trend.vector(y=results$q_min, x=results$years, method="yuepilon")
  titl=paste("Minimum Values measured at",station,",",Name,",", "from", year_one, "to", last_year)
  subtitl=paste("Smallest Value being measurd is: ", abs_min)
  plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="red"))+labs(title=titl, subtitle=subtitl, x="years" , y="min.discharge")+theme(legend.position="none")+geom_abline(intercept = model[11], slope= model[2])
  print(plot)
}
