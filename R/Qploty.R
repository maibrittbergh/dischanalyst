#' Discharge Plot in specific (calendrical/hydrological) year
#'
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param year numeric; a certain year within the time series since begin of measurements.
#' @param h logical; hydrological year. If h=TRUE; hydrological year November - October (given year/given year +1). If h=FALSE: calendrical year: January- December.
#'
#' @return graphic of discharge time series in given year
#' @export
#'@import ggplot2
#'
#'@examples
#'\dontrun{
#'Qploty(mosel, "COCHEM", 2000, h=T)
#'}
#'
#'


Qploty=function(data, station, year,h=T){



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
    new_data=data[[nbr]][j,]
    titl=paste("Discharge Time Series","at",station)
    subtitl=paste("In Hydrological Year:", year, "/", year+1)
    plot= ggplot()+geom_line(new_data, mapping=aes(x=new_data[,1],y=new_data[,2], group=1, col="red"))+scale_x_date(name="Date")+labs(title=titl, subtitle=subtitl)+theme(legend.position="none")+ylab("Discharge Value")
    return(plot)

  }else{
    year_=year
    j=grep(year_, data[[nbr]][,1])
    new_data=data[[nbr]][j,]
    titl=paste("Discharge Time Series","at",station)
    subtitl=paste("In  Year:", year)
    plot= ggplot()+geom_line(new_data, mapping=aes(x=new_data[,1],y=new_data[,2], group=1, col="red"))+scale_x_date(name="Date")+labs(title=titl, subtitle=subtitl)+theme(legend.position="none")+ylab("Discharge Value")
    return(plot)

  }




}



