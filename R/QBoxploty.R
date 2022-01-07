#' Discharge Boxplot of measured Values during specific (calendrical/hydrological) year
#'
#' @param data list; River from GRDC - Dataset. Output of grdc-readr function. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param Name character; Name of the River. e.g. "Mosel".
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param year numeric; year within time series of measurements.
#' @param h logical; hydrological year. If h=TRUE (default); hydrological year November - October (given year/given year +1). If h=FALSE: calendrical year: January- December.
#'
#' @return Boxplot graphic of Discharge Measurements in a specific (calendrical/hydrological) year. Using \link[ggplot2]{geom_boxplot}.
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{ QBoxploty(mosel, "COCHEM", 2000, h=T)}
#'


QBoxploty=function(data,  station, year, h=T){



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
    plot=ggplot(new_data)+geom_boxplot(aes(y=new_data[,2], color="red"))+labs(title=titl, subtitle=" GRDC-Data by the  BfG")+theme(legend.position="none")+ylab("Discharge Value")
    return(plot)
  }else{
    year_=year
    j=grep(year_, mosel[[nbr]][,1])
    new_data=mosel[[nbr]][j,]
    titl=paste("Boxplot of",station, "in", year)
    plot=ggplot(new_data)+geom_boxplot(aes(y=new_data[,2], color="red"))+labs(title=titl, subtitle=" GRDC-Data by the  BfG")+theme(legend.position="none")+ylab("Discharge Value")
    return(plot)
  }


}
