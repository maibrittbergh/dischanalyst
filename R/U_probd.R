
#' Probability to receive Discharge Values under U within the decades since begin of measurements
#'
#' @param U numeric; Limit Value. Function returns possibility to receive Value smaller than U for every decade since begin of measurements.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param Name character; Name of the Dataset. e.g. "Mosel"
#'
#' @return
#' @export
#'@import ggplot2
#'@importFrom zyp zyp.trend.vector
#'@import Kendall
#' @examples
#' \dontrun{ U_probd(150,"COCHEM", mosel)}
#'
U_probd=function(U, station, data){
  nbr=which(names(data)==station)
  decade_one=(as.numeric(substring(as.character(data[[nbr]][1,1]),1,3)))
  length=length(data[[nbr]][,1])
  last_decade=(as.numeric(substring(as.character(data[[nbr]][length,1]),1,3)))
  decades=c(decade_one:last_decade)
  l=length(decades)
  U_prob=rep(0, l)
  for ( i in 1:l){
    dec=as.character(decades[i])
    j=grep(dec, data[[nbr]][,1])
    Val=sort(data[[nbr]][,2][j])
    under=which(Val<U)
    U_prob[i]=100*(length(under)/length(Val))
  }
  decades=decades*10
  results= data.frame(decades, U_prob)

  model=zyp.trend.vector(y=results$U_prob, x=results$decades, method="yuepilon")
  modellm=lm(U_prob~decades, results)
  titl=paste("Probability of falling below Value:",U,". At:",station, "from", decade_one*10, "to", last_decade*10, "[in decades]")

  plot=ggplot(results)+geom_line(mapping=aes(x=decades,y=U_prob, group=1, col="1"))+labs(title=titl,  x="decades" , y="Probabilty [%]")+
    geom_abline(aes(intercept = model[11], slope= model[2], col="2"))+
    geom_abline(aes(intercept=modellm$coefficients[1], slope=modellm$coefficients[2], col="3"))+
    scale_color_manual(name = "Legend",
                       labels=c("Probability [%]", "Trend Line - Sens Sloap",
                                "Trend Line-Least Squares"), values=c("a"="#F8766D", "#00BDD0", "darkblue"), guide="legend")+ theme(legend.position = "right" )

  return(plot)
}



