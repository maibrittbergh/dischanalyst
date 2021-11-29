#' Boxplot of Discharge Measurements in a specific year
#'
#' @param data River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param Name as character. Name of the River. e.g. "Mosel"
#' @param station as character. Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param year as numeric. a certain year within the time series since begin of measurements
#' @param h hydrological year: TRUE or FALSE. If h=T, output is boxplot of hydrological year (given year/given year+1). If h=F output is Boxplot of Values of calendric year.
#'
#' @return Boxplot Graphic of Discharge Measurements of specific (hydrological/calendrical) year
#' @export
#' @import ggplot2
#'
#' @examples
#' \dontrun{Boxplot_year(mosel, "MOSELLE RIVER", "COCHEM", 2000,h=T)}
#'
#'
QBoxplot_year=function(data, Name, station, year, h){



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
      two[i]=paste(year+1,"-",hydroyear2[i])
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
    new_data=mosel[[nbr]][j,]
    titl=paste("Boxplot of", Name, ",", station, "in the hydrological year", year, "and", year+1)
    plot=ggplot(new_data)+geom_boxplot(aes(y=new_data[,2], color="red"))+labs(title=titl, subtitle=" GRDC-Data by the  BfG", xlab="Discharge Value")+theme(legend.position="none")
    return(plot)
  }else{
    year_=year
    j=grep(year_, mosel[[nbr]][,1])
    new_data=mosel[[nbr]][j,]
    titl=paste("Boxplot of", Name, ",", station, "in", year)
    plot=ggplot(new_data)+geom_boxplot(aes(y=new_data[,2], color="red"))+labs(title=titl, subtitle=" GRDC-Data by the  BfG", xlab="Discharge Value")+theme(legend.position="none")
    return(plot)
  }


}
