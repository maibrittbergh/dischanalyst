
#' MQ_trend
#'
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param seasonal character; Possible Inputs:  "Y"( MQ Trend of (hydrological) Years); "WI"(MQ Trend of Winters during years); "SP"(MQ Trend of Springs during (hydrological) years); "SU"(MQ Trend of Summers during (hydrological) years); "AU"(MQ Trend of Autums during (hydrological) years)
#' @param graphic logical; default=T. For graphic=T, function returns a graph and visualizes the trend.   For graphic=F, function returns the model coefficients as list. For the Sens Sloap Approach (using: \link[zyp]{zyp.trend.vector}) and for the linear model approach (using: \link[stats]{lm}).
#'
#' @return graphic / list
#' @export
#'@import zyp
#'@import ggplot2
#'
#' @examples
#'  \dontrun{
#' summerplot=MQ_trend(data=mata, "COCHEM", seasonal="SU", graphic=T)}


MQ_trend=function(data, station, seasonal=Y, graphic=T){


datan=data[[station]]

MEAN=mean(datan$Value)

if (seasonal=="Y"){

  l=nrow(datan)
startyear=as.numeric(substr(datan[1,1],1,4))
endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){
    yearmin=years[i]
    yearmax=years[i+1]

    min=paste(yearmin,"-", "11-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "10-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    data=datan[start:end, ]
     MQ[i]=round(mean(data[,2],0))

  }

results=data.frame(cbind(MQ, years[-l]))


  titl=paste("MQ- Trend within Hydrological Years at", station)
  subtitl=paste("Timeseries from", startyear, "to", endyear)
  model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl)+ylab("MQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
      theme(legend.position = "bottom" )

  }else{


    res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

    names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
  }
  return(res)


}else if (seasonal=="WI"){


  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){
    yearmin=years[i]
    yearmax=years[i+1]

    min=paste(yearmin,"-", "11-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "01-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    data=datan[start:end, ]
    MQ[i]=round(mean(data[,2],0))

  }



  results=data.frame(cbind(MQ, years[-l]))



  titl=paste("MQ- Trend within Winter at", station)
  subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Nov, Dec, Jan")
  model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl)+ylab("MQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
      theme(legend.position = "bottom" )


  }else{


    res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

    names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
  }

  return(res)


}else if (seasonal=="SP"){



  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "02-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "04-30")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    data=datan[start:end, ]
    MQ[i]=round(mean(data[,2],0))

  }



  results=data.frame(cbind(MQ, years[-l]))



  titl=paste("MQ- Trend within Spring at", station)
  subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Feb, Mar, Apr")
  model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl)+ylab("MQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
      theme(legend.position = "bottom" )


  }else{


    res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

    names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
  }

  return(res)


}else if (seasonal=="SU"){



  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "05-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "07-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    data=datan[start:end, ]
    MQ[i]=round(mean(data[,2],0))

  }



  results=data.frame(cbind(MQ, years[-l]))



  titl=paste("MQ- Trend within Summer at", station)
  subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: May, June, July")
  model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl)+ylab("MQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
      theme(legend.position = "bottom" )


  }else{


    res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

    names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
  }

  return(res)


}else if (seasonal=="AU"){



  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "08-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "10-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    data=datan[start:end, ]
    MQ[i]=round(mean(data[,2],0))

  }



  results=data.frame(cbind(MQ, years[-l]))



  titl=paste("MQ- Trend within Autumn at", station)
  subtitl=paste("Timeseries from", startyear, "to", endyear, "Months: Aug, Sep, Oct")
  model=zyp.trend.vector(results$MQ, results$V2 , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  if (graphic==T){

    res=ggplot(results)+geom_line(mapping=aes(x=V2,y=MQ, col="a"), show.legend  =TRUE)+labs(title=titl, subtitle=subtitl)+ylab("MQ-Value")+xlab("Year [hydrological Year]")+

      geom_abline(aes(intercept = int, slope=slop,  col="b"), show.legend=TRUE)+
      geom_abline(aes(intercept= linmod$coefficients[1], slope=linmod$coefficients[2],col="c"), show.legend=TRUE)+
      scale_color_manual(name = "Legend:   ",
                         labels=c("NMxQ", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D","b"= "darkblue", "c"="lightgreen"), guide="legend")+
      theme(legend.position = "bottom" )

  }else{


    res= list(int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100 )

    names(res)=c("intercept_zyp", "slope_zyp","sig_zyp",  "intercept_lm", "slope_lm", "normalized Trend" )
  }

  return(res)


}

}



