#'Normalized Trends
#'
#'@description Function normalizes the calculated trends. The normalization of the trends is done with the help of the catchment area size. The newly determined value is the "specific discharge". For this, the trend is divided by the catchment area size and multiplied by 1000.  The trends must be available in a list.
#'
#' @param data list, results of trend calculation. Can be created by  \link[dischanalyst]{metaMQ}, \link[dischanalyst]{mintrendmeta}, \link[dischanalyst]{nmxqmeta}.
#' @param metadata Data Frame. Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function
#'
#' @return list, results of normalized trend calculation.
#' @export
#'

normalized_trends=function(data,metadata){


  meta=metadata
  d=data
  l=length(d)

  result=d

  for( i in 1:l){

    nd=d[[i]]#hier i einfügen
    stations=meta$station
    stat=nd$station
    k=length(stat)
    numb=rep(0,k)
    for ( o in 1:k){

      numb[o]=which(stations==stat[o])
    }
    numb
    area=meta$catch_area[numb]

    colnams=c(6,9,11,14,16,19,21,24,26,29)
    p=length(colnams)


    res=data.frame(matrix(ncol=p, nrow=k))

    for ( j in 1:k){


      res[j,]=(nd[j,colnams]/area[j])*1000

    }





    for(h in 1:p){
      nd[,colnams[h]]=res[h]
    }
    result[[i]]=nd

  }
  return(result)
}
