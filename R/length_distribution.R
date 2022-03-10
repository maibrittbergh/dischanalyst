#' Distribution of Length of Discharge Measurements of all Stations within Metadata
#'
#' @param metadata "matrix" "array" ;  Metadata of grdc dataset. Can be created by \link[dischanalyst]{metadata_grdc} function
#'
#' @return  density plot
#' @export
#'
#' @import sf
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' length_distribution(metadata)}
#'
<<<<<<< HEAD
<<<<<<< HEAD
length_distribution=function(metadata){
=======
length_distribution=function(metadata, type="dens"){
>>>>>>> ffc5ade01805f6d97cd5bc4bbf8c7168d89c11ec
=======
length_distribution=function(metadata, type="dens"){
>>>>>>> ffc5ade01805f6d97cd5bc4bbf8c7168d89c11ec

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


<<<<<<< HEAD
<<<<<<< HEAD


=======
 # tmap_mode("view") #view einfügen, damit Hintergrundkarte funktioniert
=======
 # tmap_mode("view") #view einfügen, damit Hintergrundkarte funktioniert


  #tm=tm_shape(st_meta)+ tm_dots("length_timeseries", title="Length of Timeseries [years]", id="river_station",interactive=T, popup.vars=c(
#    "Length of Timeseries"="length_timeseries",
 #   "Startyear" = "startyear",
  #  "Endyear"= "endyear"
  #) , palette="YlOrBr")+ tm_scale_bar()+ tm_basemap(c("OpenStreetMap","Esri.WorldImagery"))+
   # tm_layout("Length of Timeseries [years]")
#if (type=="map"){
#  return(tm)
#}else{
>>>>>>> ffc5ade01805f6d97cd5bc4bbf8c7168d89c11ec


  #tm=tm_shape(st_meta)+ tm_dots("length_timeseries", title="Length of Timeseries [years]", id="river_station",interactive=T, popup.vars=c(
#    "Length of Timeseries"="length_timeseries",
 #   "Startyear" = "startyear",
  #  "Endyear"= "endyear"
  #) , palette="YlOrBr")+ tm_scale_bar()+ tm_basemap(c("OpenStreetMap","Esri.WorldImagery"))+
   # tm_layout("Length of Timeseries [years]")
#if (type=="map"){
#  return(tm)
#}else{

>>>>>>> ffc5ade01805f6d97cd5bc4bbf8c7168d89c11ec
  pl=ggplot(metadata)+geom_density(aes(y=length_timeseries, col="red"))+coord_flip()+
    theme(legend.position = "none")+ labs(y = "Length of timeseries[years]", x = "density",
                                                                             title ="Density Distribution of Length of Discharge Time Series", subtitle="Source: GRDC-Dataset")

  return(pl)
<<<<<<< HEAD
<<<<<<< HEAD

=======
#}
>>>>>>> ffc5ade01805f6d97cd5bc4bbf8c7168d89c11ec
=======
#}
>>>>>>> ffc5ade01805f6d97cd5bc4bbf8c7168d89c11ec





}


