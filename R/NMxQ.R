
#' NMxQ
#'
#'@Description arithmetic mean of x consecutive days within a period (here: hydrological year) (Source: (DVWK 1983))
#'
#' @param x length of period (days). With decreasing Values for x , the influence of short-term anthropogenic influences increases. E.g. x=7, x=14, x=30
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the station. Must equal one entry of the data list.
#' @param year hydrological year. If you enter: 2007, output will be the Result for the NMxQ of the hydrological year 2007/2008.
#'
#' @return NMxQ-Value
#' @export
#'
#' @examples
#'  \dontrun{ hyear09=NMxQ(4, data_n, "COCHEM", 2009)}

#'
NMxQ= function(x, data, station, year){
  data=data[[station]]

yearmax=as.character(year+1)
yearmin=as.character(year)


min=paste(yearmin,"-", "11-01")
min=sub(" - ", "-",min)
max= paste(yearmax,"-", "10-31")
max=sub(" - ", "-",max)
start=grep(min, data[,1])
end=grep(max, data[,1])


data=data[start:end, ]
l=nrow(data)

le=l-x
le
Nmxq=rep(0, le)
for ( i in 1:le){
  Nmxq[i]=round(mean(data[i:(i+x), 2]),0)


}
return(min(Nmxq))
}



