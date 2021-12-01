#' Sum of Days under U within a specific (calendrical/hydrological) yeaf

#'
#' @description Function returns sum of days under a certain value (at a given measuring point of a given river),
#' within one year.
#'
#' @param U numeric; Limit Value. Function returns sum of days under given Value U in a year.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param h logical;  hydrological year. If h=TRUE; hydrological year November - October (given year/given year +1). If h=FALSE: calendrical year: January- December.
#'
#'
#' @return
#' @export
#'
#'
#' @examples
#' \dontrun{ Usum_periody(88, "TRIER UP", mosel, "2003", h=T)}
#'
Usum_periody=function(U, station, data, year,h){
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

    Nov=grep(year_[1],mosel[[nbr]][,1] )
    Dec=grep(year_[2],mosel[[nbr]][,1] )
    Jan=grep(year_[3],mosel[[nbr]][,1] )
    Feb=grep(year_[4],mosel[[nbr]][,1] )
    Mar=grep(year_[5],mosel[[nbr]][,1] )
    April=grep(year_[6],mosel[[nbr]][,1] )
    May=grep(year_[7],mosel[[nbr]][,1] )
    June=grep(year_[8],mosel[[nbr]][,1] )
    July=grep(year_[9],mosel[[nbr]][,1] )
    August=grep(year_[10],mosel[[nbr]][,1] )
    Sep=grep(year_[11],mosel[[nbr]][,1] )
    Oct=grep(year_[12],mosel[[nbr]][,1] )


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
