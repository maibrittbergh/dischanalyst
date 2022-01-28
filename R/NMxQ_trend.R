
#' NMxQ_trend
#'@description Function calculates for every year within measurements NMxQ Value (Description: \link[dischanalyst]{NMxQ}). It returns a trend: Sens Sloap and Linear Model.
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param x length of period (days). With decreasing Values for x , the influence of short-term anthropogenic influences increases. E.g. x=7, x=14, x=30
#' @param graphic logical; default=T. For graphic=T, function returns a graph and visualizes the trend.   For graphic=F, function returns the model coefficients. For the Sens Sloap Approach (using: \link[zyp]{zyp.trend.vector}) and for the linear model approach (using: \link[stats]{lm}).
#'
#'
#'@import zyp
#'@import ggplot2
#'
#' @return Graph/ List.
#' @export
#'
#' @examples
#'  \dontrun{
#' list=NMxQ_trend(data, "COCHEM", 4, graphic=F)}
#'
NMxQ_trend=function(data, station, x, graphic=T){


datan=data[[station]]
l=nrow(datan)
startyear=as.numeric(substr(datan[1,1],1,4))
endyear=as.numeric(substr(datan[l,1],1,4))-1
years=c(startyear:endyear)
le=length(years)
nmxq=rep(0,le)
for ( i in 1: le){
  nmxq[i]=NMxQ(x,data, station, years[i] )

}

results=data.frame(cbind(nmxq, years[-l]))

titl=paste("NMxQ- Trend. For x=", x, "at", station)
subtitl=paste("Timeseries rom", startyear, "to", endyear)
model=zyp.trend.vector(results$nmxq, results$V2 , "yuepilon")

linmod=lm(nmxq~V2, results)
slop=as.numeric(model[2])
sig=as.numeric(model[6])
int=as.numeric(model[11])

if (graphic==T){

  res=ggplot(results)+geom_line(mapping=aes(x=years,y=nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl)+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

    geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
    geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
    scale_color_manual(name = "Legend:   ",
                       labels=c("NMxQ", "Trend Line - Sens Sloap",
                                "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
    theme(legend.position = "bottom" )

}else{


res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2] )

  names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm")
}






return(res)
}


