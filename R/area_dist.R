#' Area Distribution
#'
#' @param metadata "matrix" "array" ;  metadata of grdc dataset. Can be created by \link[dischanalyst]{metadata_grdc} function
#'
#' @return dotplot, Distribution of Catchment Areas.
#' @export
#'
#' @examples
#' \dontrun{ area_dist(metadata)}
area_dist=function(metadata){
  metadata=metadata_repg(metadata, mark=T)


  graph=ggplot(metadata)+geom_dotplot(aes( x=catch_area, fill=rep_stat))+scale_fill_discrete(name="Representative Station")+xlab(expression('Catchment Area [km'^2*']'))
return(graph)


}

