#' Probabilty to fall under U for every year since begin of the measurements
#'
#' @param U numeric; Limit Value. Function calculates probability to fall below U for every year since the begin of the measurements. Returns a Graphic and a Trend over the years.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param Name character; Name of the dataset. e.g. "Mosel"
#' @param h logical;  hydrological year. If h=TRUE; hydrological year November - October (given year/given year +1). If h=FALSE: calendrical year: January- December.
#'
#' @return
#' @export
#'@import ggplot2
#'@importFrom zyp zyp.trend.vector
#'@import Kendall
#' @examples
#' \dontrun{ U_proby(150,"COCHEM", mosel,h=T)}
#'
U_proby=function(U, station, data,h){



  nbr=which(names(data)==station)

  if (h==T){
    Novem=grep("11-01" ,mosel[[nbr]][,1]) #Problem : January counted as well.
    Oct=grep("10-31" ,mosel[[nbr]][,1])
    l=length(Novem)
    a=length(Novem)-1
    U_prob=rep(0,l)

    for ( i in 1:a){
      Val=sort(c(mosel[[nbr]][Novem[i]:Novem[i+1],2]))
      leng=length(Val)
      Val=Val[-leng]
      under=which(Val<U)
      U_prob[i]=100*(length(under)/length(Val))
    }


    years=data[[nbr]][Novem,1]

    l=length(years)
    years.numeric=rep(0,l)
    for ( i in 1: l){
      years.numeric[i]= as.numeric(substr(years[i],1,4))
    }

  #eliminate january
    if(any(years.numeric==1911)){
      vec=which(years.numeric==1911)
     l= length(which(years.numeric==1911))

      january= l-1
     jan=vec[1:january]

      years.numeric= years.numeric[-jan]
    years=years[-jan]
    U_prob=U_prob[-jan]

    }
    if(any(years.numeric==2011)){
      vec=which(years.numeric==2011)
      l= length(which(years.numeric==2011))

      january= l-1
      jan=vec[1:january]

      years.numeric= years.numeric[-jan]
      years=years[-jan]
      U_prob=U_prob[-jan]

    }







    results= data.frame(years, U_prob, years.numeric)



    titl=paste("Probability of falling below Value:",U,". At:",station, "from", years[1], "to", years[length(years)], "[hydrological years]")



    model= zyp.trend.vector(y=results$U_prob, x=results$years.numeric, method="yuepilon")
    modellm=lm(U_prob~years.numeric, results)




    plot=ggplot(results)+geom_line(mapping=aes(x=years.numeric,y=U_prob, group=1, col="1"))+labs(title=titl,  x="years" , y="Probabilty [%]")+
      geom_abline(aes(intercept = model[11], slope= model[2], col="2"))+
      geom_abline(aes(intercept=modellm$coefficients[1], slope=modellm$coefficients[2], col="3"))+
      scale_color_manual(name = "Legend",
                         labels=c("Probability [%]", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D", "#00BDD0", "darkblue"), guide="legend")+ theme(legend.position = "right" )



    return(plot)



  }else{


    year_one=as.numeric(substring(as.character(data[[nbr]][1,1]),1,4))
    length=length(data[[nbr]][,1])
    last_year=as.numeric(substring(as.character(data[[nbr]][length,1]),1,4))
    years=c(year_one:last_year)
    l=length(years)
    U_prob=rep(0, l)
    for ( i in 1:l){
      year=as.character(years[i])
      j=grep(year, data[[nbr]][,1])
      Val=sort(data[[nbr]][,2][j])
      under=which(Val<U)
      U_prob[i]=100*(length(under)/length(Val))
    }

years.numeric=as.numeric(years)




    results= data.frame(years, U_prob, years.numeric)



    model= zyp.trend.vector(y=results$U_prob, x=results$years.numeric, method="yuepilon")
    modellm=lm(U_prob~years.numeric, results)


    title=paste("Probability of falling below Value:",U,". At: ",station, "from", year_one, "to", last_year)

    plot=ggplot(results)+geom_line(mapping=aes(x=years.numeric,y=U_prob, group=1, col="1"))+labs(title=title,  x="years" , y="Probabilty [%]")+
      geom_abline(aes(intercept = model[11], slope= model[2], col="2"))+
      geom_abline(aes(intercept=modellm$coefficients[1], slope=modellm$coefficients[2], col="3"))+
      scale_color_manual(name = "Legend",
                         labels=c("Probability [%]", "Trend Line - Sens Sloap",
                                  "Trend Line-Least Squares"), values=c("a"="#F8766D", "#00BDD0", "darkblue"), guide="legend")+ theme(legend.position = "right" )

  return(plot)
  }


}





