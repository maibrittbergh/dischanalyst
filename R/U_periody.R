#' Max. amount of days under Value U within a specific (calendrical/hydrological) year
#'
#' @description Function indicates how long the longest period existed that a certain value was undercut (at a given measuring point of a given river within a given year),
#' within the time period of the measurements.
#'
#' @param U numeric; Limit Value.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param h logical;  hydrological year. If h=TRUE; hydrological year November - October (given year/given year +1). If h=FALSE: calendrical year: January- December.
#'
#' @return Function returns longest time measured under Value U in specific year.
#' @export
#'
#' @examples
#' \dontrun{ U_periody(80, "COCHEM", data, "2003", h=T)}
#'
U_periody=function(U, station, data, year,h){

  nbr=which(names(data)==station)



  if (h==T){


    one=rep("0",2)
    two=rep("0", 10)

    for (i in 1:2){
      hydroyear1=c("11", "12" )
      one[i]=paste(year,"-",hydroyear1[i])
      one=sub(" - ", "-", one)

    }
    for (i in 1:10){
      hydroyear2=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10")
      y=as.numeric(year)
      y2=as.character(y+1)
      two[i]=paste(y2,"-",hydroyear2[i])
      two=sub(" - ", "-", two)
    }
    year_=c(one,two)

    Nov=grep(year_[1],data[[nbr]][,1] )
    Dec=grep(year_[2],data[[nbr]][,1] )
    Jan=grep(year_[3],data[[nbr]][,1] )
    Feb=grep(year_[4],data[[nbr]][,1] )
    Mar=grep(year_[5],data[[nbr]][,1] )
    April=grep(year_[6],data[[nbr]][,1] )
    May=grep(year_[7],data[[nbr]][,1] )
    June=grep(year_[8],data[[nbr]][,1] )
    July=grep(year_[9],data[[nbr]][,1] )
    August=grep(year_[10],data[[nbr]][,1] )
    Sep=grep(year_[11],data[[nbr]][,1] )
    Oct=grep(year_[12],data[[nbr]][,1] )


    j=c(Nov,Dec,Jan, Feb,Mar, April, May, June, July, August, Sep, Oct)


  }else{


    j=grep(year, data[[nbr]][,1])

  }
  Val=data[[nbr]][,2][j]
  uU=which(Val<U)
  l=length(uU)
  if (all(Val>U)) {paste("No days")
  }else {
    c=rep(0,l)#oder l-1
    for ( i in 1:l){
      c[i]=(uU[i+1]-uU[i])
    }
    index=cbind(c, uU)
    G=which(c>1)
    c[G]=0  #Vektor c in 0 und 1
    c[is.na(c)] = 0


    d=c
    l=length(c)
    e=rep(0,l)


    for (i in 2:l){
      e[1]=d[1]
      if ((e[i-1]+d[i]) > e[i-1]){e[i]=e[i-1]+d[i]}
      else {e[i]=0}
    }

    ic_max=which(e == (max(e))) #indexin(c)vektor
    id_End_period=min(j)+index[,2][ic_max] #index inxGesamtdatensatz
    id_Start_period=id_End_period-max(e)

    #Welche Tage
    Startdate=data[[nbr]][,1][id_Start_period]
    Enddate=data[[nbr]][,1][id_End_period]

    paste("Length of maximum time period in", year, "under given Value of", U, "is", max(e), "days. Timeperiod from:",Startdate, "to:",Enddate)
  }

}

