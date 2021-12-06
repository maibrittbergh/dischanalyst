#' Probabilty to fall under U for every year during since begin of the measurements
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
#' @examples
#' \dontrun{ U_prob_years(150,"COCHEM", mosel, "Mosel",h=T)}
#'
U_proby=function(U, station, data,Name,h){
  nbr=which(names(data)==station)

  if (h==T){
    Novem=grep("11-01" ,mosel[[nbr]][,1])
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

    results= data.frame(years, U_prob)
    model=lm(U_prob~years, results)
    titl=paste("Probability of falling below Value:",U,". At",Name,",",station, "from", years[1], "to", years[length(years)])

    plot=ggplot(results)+geom_line(mapping=aes(x=years,y=U_prob, group=1, col="red"))+labs(title=titl,  x="years" , y="Probabilty [%]")+theme(legend.position="none")+geom_abline(intercept = model$coefficients[1], slope= model$coefficients[2])
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
    results= data.frame(years, U_prob)
    model=lm(U_prob~years, results)
    title=paste("Probability of falling below Value:",U,". At",Name,",",station, "from", year_one, "to", last_year)

    plot=ggplot(results)+geom_line(mapping=aes(x=years,y=U_prob, group=1, col="red"))+labs(title=title,  x="years" , y="Probabilty [%]")+theme(legend.position="none")+geom_abline(intercept = model$coefficients[1], slope= model$coefficients[2])
    return(plot)
  }


}
