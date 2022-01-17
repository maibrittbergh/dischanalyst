#' Trend of Annual Discharge Minima since begin of Measurements
#'
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#'
#' @return
#' @export
#' @import ggplot2
#' @importFrom zyp zyp.trend.vector
#' @import stats
#'@import Kendall
#'
#' @examples
#' \dontrun{ Qmin_trend(mosel, "COCHEM")}
#'
Qmin_trend=function(data, station) {

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
model= min_trend(data, station)
  titl=paste("Trend of Minimum Values at",station)
 cap=paste("Smallest Value being measured at",station, "is: ", abs_min)
  plot=ggplot(results)+geom_line(mapping=aes(x=years,y=q_min, group=1, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=paste("from", year_one, "to", last_year), x="Years" , y="Minimum Discharge Value", caption=cap)+
   geom_abline(aes(intercept = model$intercept_zyp, slope= model$slope_zyp,  col="b"), show.legend=TRUE)+
    geom_abline(aes(intercept= model$intercept_lm, slope=model$slope_lm,col="c"), show.legend=TRUE)+  scale_color_manual(name = "Legend:   ",
                       labels=c("Minimum values", "Trend Line - Sens Sloap",
                                "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "#00BDD0", "c"="darkblue"), guide="legend")+ theme(legend.position = "bottom" )

return(plot)


}


