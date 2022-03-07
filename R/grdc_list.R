#'GRDC List
#'
#'@description Function returns List. It may take some time to load the GRDC-List, depending on the amount of entries in metadata.
#'
#'
#' @param metadata Data Frame. Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function
#' @param path character; Pathway to grdc_discharge folder on computer
#'
#' @return Grdc List. Each entry contains Discharge Measurements as well as the corresponding date of a specific station. The Number of the stations depends on the metadata.
#' @export
#'
#' @examples
#'\dontrun{
#' grdc_list(metadata_germany, "/Users/username/Desktop/folderone/datafolder/grdc_03_2021/grdc_disc/")
#'}
#'
#'
grdc_list=function(metadata, path){

  length=nrow(metadata)
  grdc_list=vector(mode = "list", length = length)



  for ( i in 1:length){

    data=grdc_readr(metadata,   metadata$river[i]    , path)
    station=metadata$station[i]
    nbr=which(names(data)== station)
    val=data[[nbr]]



    grdc_list[[i]]=val
    names(grdc_list)=metadata$station

  }


  return(grdc_list)
}
