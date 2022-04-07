#' Area Distribution
#' @description descriptive statistics. Function helps to gain overview over the distribution of the catchment area - sizes within grdc-dataset.
#'
#' @param metadata "matrix" "array" ;  metadata of grdc dataset. Can be created by \link[dischanalyst]{metadata_grdc} function
#'
#' @return histogram , Distribution of Catchment Areas.
#' @imort ggplot2
#' @export
#'
#' @examples
#' \dontrun{ area_dist(metadata)}


area_dist=function(metadata){



  graph=ggplot(data)+geom_histogram(aes( x=catch_area/1000),bins=20, fill="darkcyan", col="grey")+ylab("Count")+labs(title="Distribution of Size of Catchmen-Areas")+xlab(expression('Size of Catchemnt Area[km'^2*']  x10'^3))

  return(graph)


}


