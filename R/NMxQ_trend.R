
#' NMxQ_trend
#'
#'@description Function calculates for every year within measurements NMxQ Value (Description: \link[dischanalyst]{NMxQ}). It returns a trend: Sens Sloap Trend and Linear Trend. The Trend can be calculated for a specific season or for a whole year.
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param x length of period (days). With decreasing Values for x , the influence of short-term anthropogenic influences increases. E.g. x=7, x=14, x=30
#' @param graphic logical; default=T. For graphic=T, function returns a graph and visualizes the trend.   For graphic=F, function returns the model coefficients as list. For the Sens Sloap Approach (using: \link[zyp]{zyp.trend.vector}) and for the linear model approach (using: \link[stats]{lm}).
#'@param seasonal character; default= "Y". Possible Inputs:  "Y"( MQ Trend of (hydrological) Years); "WI"(MQ Trend of Winters during years); "SP"(MQ Trend of Springs during (hydrological) years); "SU"(MQ Trend of Summers during (hydrological) years); "AU"(MQ Trend of Autums during (hydrological) years)
#'
#'@import zyp
#'@import ggplot2
#'
#' @return Graphic/ list:
#' \describe{
#'   \item{intercept_zyp}{intercept created by \link[zyp]{zyp.trend.vector}}
#'   \item{slope_zyp}{slope created by \link[zyp]{zyp.trend.vector}}
#'   \item{sig_zyp}{significance (Kendall's P-Value) for the final detrended time-series}
#'   \item{intercept_ls}{intercept created by \link[stats]{lm}}
#'   \item{slope_ls}{slope created by \link[stats]{lm}}
#' }
#' @export
#'
#' @examples
#'  \dontrun{
#' list=NMxQ_trend(data, "COCHEM", 4, graphic=F)
#' list_SUMMER=NMxQ_trend(mata, "COCHEM",4,  seasonal="SU", graphic=F)}
#'
#'

NMxQ_trend(data, "COCHEM", 4, "Y")

NMxQ_trend=function(data, station, x, seasonal="Y", graphic=T){

datan=data[[station]]
l=nrow(datan)
j=l-x
NMXQ=rep(0,j)
for ( i in 1:j){
  NMXQ[i]=round(mean(datan[i:(i+x), 2]),0)

}

mNMXQ=mean(NMXQ)


if (seasonal=="Y"){
startyear=as.numeric(substr(datan[1,1],1,4))
endyear=as.numeric(substr(datan[l,1],1,4))-1
years=c(startyear:endyear)
le=length(years)
l=le-1
nmxq=rep(0,l)
for ( i in 1: l){
  nmxq[i]=NMxQ(x,data, station, years[i] )

}

results=data.frame(cbind(nmxq, years[-le]))

titl=paste("NMxQ- Trend for x=", x, "at", station)
subtitl=paste("Timeseries from", startyear, "to", endyear)
model=zyp.trend.vector(results$nmxq, results$V2 , "yuepilon")

linmod=lm(nmxq~V2, results)
slop=as.numeric(model[2])
sig=as.numeric(model[6])
int=as.numeric(model[11])

if (graphic==T){

  res=ggplot(results)+geom_line(mapping=aes(x=V2,y=nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("slope: Trend Line- Sens Sloap:", slop,"slope: Trend Line- Least Squares:", linmod$coefficients[2] ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

    geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
    geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
    scale_color_manual(name = "Legend:   ",
                       labels=c("NMxQ", "Trend Line - Sens Sloap",
                                "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
    theme(legend.position = "bottom" )


}else{


res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

  names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
}

return(res)

}else if (seasonal=="WI"){



  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  le=length(years)

la=le-1
  Nmxq=rep(0,la)

  for (i in 1:la){
    yearmin=years[i]
    yearmax=years[i+1]

    min=paste(yearmin,"-", "11-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "01-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    datam=datan[start:end, ]


    l=nrow(datam)

    le=l-x

    for ( k in 1:le){
      Nmxq[i]=round(mean(datam[k:(k+x), 2]),0)


    }
  }

  results=data.frame(cbind(Nmxq, years[-le]))
  results




  titl=paste("NMxQ- Trend. For x=", x, "at", station, "in Winter")
  subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Dec, Jan, Feb")
  model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

  linmod=lm(Nmxq~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("slope: Trend Line- Sens Sloap:", slop,"slope: Trend Line- Least Squares:", linmod$coefficients[2] ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
      theme(legend.position = "bottom" )









  }else{


    res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

    names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
  }

  return(res)
}else if (seasonal=="SP"){



  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  le=length(years)

  la=le-1
  Nmxq=rep(0,la)

  for (i in 1:la){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "02-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "04-30")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    datam=datan[start:end, ]


    l=nrow(datam)

    le=l-x

    for ( k in 1:le){
      Nmxq[i]=round(mean(datam[k:(k+x), 2]),0)


    }
  }

  results=data.frame(cbind(Nmxq, years[-le]))
  results




  titl=paste("NMxQ- Trend. For x=", x, "at", station, "in Spring")
  subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Feb, Mar, Apr")
  model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

  linmod=lm(Nmxq~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("slope: Trend Line- Sens Sloap:", slop,"slope: Trend Line- Least Squares:", linmod$coefficients[2] ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
      theme(legend.position = "bottom" )






}else{

  res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

  names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
}

return(res)
}else if (seasonal=="SU"){



  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  le=length(years)

  la=le-1
  Nmxq=rep(0,la)

  for (i in 1:la){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "05-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "07-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    datam=datan[start:end, ]


    l=nrow(datam)

    le=l-x

    for ( k in 1:le){
      Nmxq[i]=round(mean(datam[k:(k+x), 2]),0)


    }
  }

  results=data.frame(cbind(Nmxq, years[-le]))
  results




  titl=paste("NMxQ- Trend. For x=", x, "at", station, "in Summer")
  subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: May, June, July")
  model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

  linmod=lm(Nmxq~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("slope: Trend Line- Sens Sloap:", slop,"slope: Trend Line- Least Squares:", linmod$coefficients[2] ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
      theme(legend.position = "bottom" )






}else{

  res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

  names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
}

return(res)
}else if (seasonal=="AU"){



  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  le=length(years)

  la=le-1
  Nmxq=rep(0,la)

  for (i in 1:la){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "08-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "10-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    datam=datan[start:end, ]


    l=nrow(datam)

    le=l-x

    for ( k in 1:le){
      Nmxq[i]=round(mean(datam[k:(k+x), 2]),0)


    }
  }

  results=data.frame(cbind(Nmxq, years[-le]))
  results




  titl=paste("NMxQ- Trend. For x=", x, "at", station, "in Autumn")
  subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Aug, Sep, Oct")
  model=zyp.trend.vector(results$Nmxq, results$V2 , "yuepilon")

  linmod=lm(Nmxq~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=Nmxq, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl, caption=paste("slope: Trend Line- Sens Sloap:", slop,"slope: Trend Line- Least Squares:", linmod$coefficients[2] ))+ylab("NMxQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightblue"), guide="legend")+
      theme(legend.position = "bottom" )






}else{

  res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/mNMXQ)*100 )

  names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend")
}

return(res)
}



}

