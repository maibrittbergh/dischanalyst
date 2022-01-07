





#' Plot timeseries of measurements
#'
#' @param metadata "matrix" "array" ;  metadata of grdc dataset. Can be created by metadata_grdc function
#' @param path character; pathway to grdc_discharge folder on computer

#' @param startyear integer
#' @param endyear integer
#' @param startyear integer; year- when did time series begin
#' @param endyear integer; year- when did time series end
#' @param type type of visualization in ggplot. default: geom_line() (e.g. geom_point(), geom_path())
#'
#' @return
#' @export
#' @import dplyr
#'@import ggplot2
#' @examples
#' \dontrun{
#' timeseries(metadata, "/Users/username/Desktop/folderone/datafolder/grdc_03_2021/grdc_disc/" , 1990,2020)
#' }
#'

#'
#'
#'
timeseries=function(metadata, path, startyear, endyear,type=geom_smooth(method="auto")){




  l=nrow(metadata) #all stations included in measurements
  stations_s=rep(F,l)
  stations_e=rep(F,l)
  for ( i in 1:l){
    stations_s[i]=metadata$startyear[i]<=startyear   #measurements at least as long as given timeseries
    stations_e[i]=metadata$endyear[i]>=endyear
  }

  start=which(stations_s == TRUE)
  end=which(stations_e == TRUE)
  l=length(start)
  vec=rep(F,l)
  for ( i in 1:l){
    if (identical(which(end==start[i]), integer(0))){
      vec[i]=F
    }else{ vec[i]=T}

  }
  timeseries=start[which(vec==T)]      #filtered. only stations with measurements during whole time included

  l=length(timeseries)
  stations=metadata$station[timeseries]
  rivernames=metadata$river[timeseries]

  ts=cbind(stations, rivernames)     #concluded in table
  lts=nrow(ts)


  list2 =vector(mode = "list", length = lts)    # measurements added to table, new format: list
  for ( i in 1:lts){
    data=grdc_readr(metadata, ts[i,2], path)
    station=as.character(ts[i,1])
    nbr=which(names(data)==station)
    datast=data[[nbr]]

    list2[[i]]=datast
  }


  vec = rep(0, lts)      #possible failures?
  vec=as.logical(vec)
  for ( i in 1:lts){
    vec[i] =identical(names(list2[[i]]), names(list2[[1]]))
  }




  success=which(vec==T)
  #if (length(success)==lts){
    hh=na.omit(bind_rows(list2, .id="station"))
    for ( i in 1:lts){
      number=which(hh$station== i)
      hh$station[number]=ts[i,1]
    }

    title=paste("Timeseries of Discharge Values from", startyear, "to", endyear)
    graph= ggplot(hh, aes(x=YYYY.MM.DD, y=Value, colour=station))+type+ xlim(startyear,endyear)+
      theme(legend.position="right", legend.box = "vertical")+ylab("Discharge Value")+xlab("Time [years]")+
      labs(title=title)







    #}else{


    #fail=which(vec==F)

    #lf=length(fail)

    #for ( i in 1: lf){

     # data= grdc_readr(metadata, ts[fail[i],2], path)
     ## station=as.character(ts[fail[i],1])
      #nbr=which(names(data)==station)[1]
#      list2[[fail[i]]]=data[[nbr]]
 #   }
#
#
 #   hh=na.omit(bind_rows(list2, .id="station"))
  #  for ( i in 1:lts){
   #   number=which(hh$station== i)
    #  hh$station[number]=ts[i,1]
    #}

#title=paste("Timeseries of Discharge Values from", startyear, "to", endyear)

 #   graph= ggplot(hh, aes(x=YYYY.MM.DD, y=Value, colour=station))+type+ xlim(startyear,endyear)+ scale_x_date(name="Date")+
   #  ylim(0,4000)+ylab("Discharge Value")+xlab("Time [years]")+ theme(legend.position="bottom", legend.box = "horizontal")+
  #    labs(title=title)
#
#
 #   graph= ggplot(hh, aes(x=YYYY.MM.DD, y=Value, colour=station))+type+ xlim(startyear,endyear)+scale_x_date(name="Date")+
  #   ylim(0,4000)+ylab("Discharge Value")+xlab("Time [years]")+ theme(legend.position="bottom", legend.box = "horizontal")
#

 #   print(graph)
  #}
print(graph)
}


