#' Sum of Days under Threshold Value within a specific (calendrical/hydrological) Year

#'
#' @description Function returns sum of days under a certain value (at a given measuring point of a given river)
#' within one year.
#'
#' @param U numeric; Limit Value. Function returns sum of days under given Value U in a year.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; Contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param h logical; If h=TRUE; Hydrological year November - October (given year/given year +1). If h=FALSE: calendrical year: January- December.
#'@param year numeric; Calendrical year /hydrological year. Timespan of Analysis.
#'
#' @return Sum of days under given threshold within a specific year.
#' @export
#'
#'
#' @examples
#' \dontrun{ Usum_periody(88, "TRIER UP", data, 2003, h=T)}
#'
Usum_periody=function(U, station, data, year,h){
  year=as.character(year)

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
    Val=data[[nbr]][,2][j]

    uU=which(Val<U)
    l=length(uU)
    if (all(Val>U)) {paste("No days")}
    else {paste(l, "days were under given Value in the hydrological year", y,"/",y2)}


  }else{


    j=grep(year, data[[nbr]][,1])
    Val=data[[nbr]][,2][j]
    uU=which(Val<U)
    l=length(uU)
    if (all(Val>U)) {paste("No days")}
    else {paste(l, "days were under given Value in the calendrical year", year)}

  }



}
