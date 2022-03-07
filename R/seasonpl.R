






#' Seasonal Plot
#'
#'
#' @description Function returns discharge plot of a given season at a given station, possible to choose timeframe of multiple years.
#' It allows the user to compare the same season/timespan within different hydrological years. Minimum startmonth: november. Maximum endmonth: october.
#'
#' @param data list; River from GRDC - dataset. Output of \link[dischanalyst]{grdc_readr}. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param station character; Must equal name of station in data.
#' @param Startyear numeric; Startyear.
#' @param Endyear numeric; Endyear.
#' @param month_start numeric; Begin of the season. E.g. Febuary=2
#' @param month_end numeric; End of the seasoan. E.g. August=8
#'
#' @return Seasonal plot. The x-axis is scaled by the choosen season.
#' @export
#'
#' @examples
#' \dontrun{
#' seasonpl(mosel, "COCHEM", 2013, 2017, 2,3)
#' }
#'
seasonpl=function(data, station, Startyear, Endyear, month_start, month_end){



# convert month -----------------------------------------------------------




  if (month_start==1){
    month_start="01"
  }
  if(month_end==1){
    month_end="01"
  }

  if (month_start==2){
    month_start="02"
  }
  if(month_end==2){
    month_end="02"
  }


  if (month_start==3){
    month_start="03"
  }
  if(month_end==3){
    month_end="03"
  }


  if (month_start==4){
    month_start="04"
  }
  if(month_end==4){
    month_end="04"
  }

  if (month_start==5){
    month_start="05"
  }
  if(month_end==5){
    month_end="05"
  }


  if (month_start==6){
    month_start="06"
  }
  if(month_end==6){
    month_end="06"
  }



  if (month_start==7){
    month_start="07"
  }
  if(month_end==7){
    month_end="07"
  }


  if (month_start==8){
    month_start="08"
  }
  if(month_end==8){
    month_end="08"
  }


  if (month_start==9){
    month_start="09"
  }
  if(month_end==9){
    month_end="09"
  }

month_start=as.character(month_start)

month_end=as.character(month_end)

########






years=Startyear:Endyear
years
l=length(years)

list2 =vector(mode = "list", length = l)

nbr=which(names(data)==station)
for ( t in 1:l){



  year=years[t]



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

datan=data[[nbr]][j,]
datan$HydroYear=paste(year,"/", year+1)


list2[[t]]= datan



}


list3 =vector(mode = "list", length = l)
for ( b in 1:l){

  list2[[b]]$Month=format(list2[[b]][,1], "%m/%d")
  list2[[b]]$Year=substr(list2[[b]][,1],1,4)

start=min(which(format(list2[[b]][,1], "%m")==month_start))
end=max(which(format(list2[[b]][,1], "%m")==month_end))
list3[[b]]=list2[[b]][start:end,]
list3[[b]]$Number=1:length(start:end)

}




Result=bind_rows(list3, .id="HydroYear")

for ( i in 1:l){

  Result$HydroYear[which(Result$HydroYear==i)]=paste(years[i],"/", years[i]+1)
}




graph= ggplot(Result, aes(x=Number, y=Value, group=HydroYear, col=HydroYear ))+geom_line()+
  labs(title= paste("Seasonal plot within the timespan of month" ,month_start, "-", month_end ),
                           subtitle = paste ("During the years", Startyear, "to", Endyear, "at", station) ,
                   colour="Year" )+ xlab("Day (within given timespan)")




         return(graph)






}













