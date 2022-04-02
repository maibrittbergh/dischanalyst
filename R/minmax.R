#' minmax
#'@description function returns minimum and maximum trend within list.
#' @param data list, results of trend calculation. Can be created by  \link[dischanalyst]{metaMQ}, \link[dischanalyst]{mintrendmeta}, \link[dischanalyst]{nmxqmeta}.
#'
#' @return
#' @export
#'
#' @examples
maxmin=function(data){
  d=data
  l=length(d)
  colnams=c(6,9,11,14,16,19,21,24,26,29)
  p=length(colnams)
  res=data.frame(matrix(ncol=2, nrow=l))
  for(i in 1:l){

    no=d[[i]]
    vals=no[,colnams]

    res[i,1]=min(vals)
    res[i,2]=max(vals)
  }
  results=c(min(res), max(res))
  return(results)
}

