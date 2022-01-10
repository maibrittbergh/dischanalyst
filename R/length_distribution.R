#' Distribution of Length of Discharge Measurements Timeseries
#'
#' @param metadata "matrix" "array" ;  metadata of grdc dataset. Can be created by \link[dischanalyst]{metadata_grdc} function
#' @param type output; as character: "map" or "dens", default="map"- returns interactive tmap. if type"dens"- fucntion returns densityplot.
#'
#' @return tmap or density plot
#' @export
#'
#' @import sf
#' @import ggplot2
#' @import tmap
#'
#' @examples
#' \dontrun{
#' length_distribution(metadata)}
#'
length_distribution=function(metadata, type="map"){

nr=nrow(metadata)
  vec=rep("0", nr)
  for ( i in 1: nr){
    vec[i]=sub("  - ", "-", paste(metadata$river[i], "-", metadata$station[i]))

  }
  metadata$river_station=vec

  st_meta=st_as_sf(metadata, coords=c("longitude","latitude"), crs=4326 )




  length_timeseries=st_meta$d_years

  metadata$length_timeseries=length_timeseries
  st_meta$length_timeseries=length_timeseries





  st_meta$startyear=as.character(st_meta$startyear)
  st_meta$endyear=as.character(st_meta$endyear)


  tmap_mode("view") #view einfügen, damit Hintergrundkarte funktioniert


  tm=tm_shape(st_meta)+ tm_dots("length_timeseries", id="river_station",interactive=T, popup.vars=c(
    "Length of Timeseries"="length_timeseries",
    "Startyear" = "startyear",
    "Endyear"= "endyear"
  ) , palette="YlOrBr")+ tm_scale_bar()+ tm_basemap(c("OpenStreetMap","Esri.WorldImagery"))+
    tm_layout("Length of Timeseries [years]")




  pl=ggplot(metadata)+geom_density(aes(y=length_timeseries, col="red"))+coord_flip()+
    theme(legend.position = "none")+ labs(x = "Length of timeseries[years]", y = "density",
            title ="Density Distribution of Length of Discharge Time Series", subtitle="Source: GRDC-Dataset")

  if (type=="dens"){
    return(pl)
  }else {
    return(tm)
  }

}


