#' Plot: timeseries of mulitple measurements
#'
#'@description Function plots timeseries within a certain timeframe.
#'To provide clarity the function reduces the amount of displayed timeseries if the number of stations with measurements >= 20.
#'The plotted timeseries then are reduced to the most representative timeseries within the timeframe. As the representative levels within \link[dischanalyst] were determined only for Germany,
#'this function only produces a for stations in germany as it was developed for the WebApp: Dischanalyst.
#'
#' @param metadata "matrix" "array" ;  Metadata of grdc dataset. Can be created by \link[dischanalyst]{metadata_grdc} function.
#' @param data list; Contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param startyear integer; Last possible startyear of timeseries. Minimum of xlim for plot.
#' @param endyear integer; Minimum endyear of timeseries. maximum of xlim for plot.
#'@param frame1 minimum of Y-axis -resolution
#'@param frame2 maximum of X-axis - resolution
#
#' @return geom_line plot. Every line represents one station.
#' @export
#' @import dplyr
#' @import ggplot2
#' @examples
#' \dontrun{
#' tiseger(metadata, "/Users/username/Desktop/folderone/datafolder/grdc_03_2021/grdc_disc/" , 1990,2020, metadata_repg= metadata_repg)
#' }
#'



tiseger=function(metadata, data, startyear, endyear,frame1, frame2){
  metadata_repg=metadata_repg(metadata)
  library(dplyr)
  l=nrow(metadata) #all stations, included in measurements
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

  if (lts > 20){
    nr= nrow(metadata_repg)
    g= rep(0,nr)

    for ( i in 1:nr){
      g[i]=is.element(metadata_repg$station[i], ts[,1])
    }
    true=which(g==1)
    ts_=metadata_repg$station[true]
    tr=metadata_repg$river[true]
    tsn=cbind(ts_, tr)
    ltsn=nrow(tsn)




    list2 =vector(mode = "list", length = ltsn)    # measurements added to table, new format: list
    names(list2)=ts_
    for ( i in 1:ltsn){

      val=data[[ts_[i]]]



      list2[[i]]=val




      hh=na.omit(bind_rows(list2, .id="station"))
      for ( i in 1:ltsn){
        number=which(hh$station== i)
        hh$station[number]=tsn[i,1]
      }


    }



  }else{


    list2 =vector(mode = "list", length = lts)    # measurements added to table, new format: list
    for ( i in 1:lts){


      val=data[[stations[i]]]



      list2[[i]]=val

      names(list2)=stations





      hh=na.omit(bind_rows(list2, .id="station"))
      for ( i in 1:lts){
        number=which(hh$station== i)
        hh$station[number]=ts[i,1]
      }

    }

  }




  title=paste("Timeseries of Discharge Values from", startyear, "to", endyear)
  graph= ggplot(hh, aes(x=YYYY.MM.DD, y=Value, colour=station))+geom_line()+ xlim(startyear,endyear)+ylim(frame1,frame2)+
    theme(legend.position="right", legend.box = "vertical")+ylab("Discharge Value")+xlab("Time [years]")+
    labs(title=title)







  return(graph)
}


