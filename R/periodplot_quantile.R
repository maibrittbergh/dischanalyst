
#' periodplot_quantile
#'
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param station character; Name of the Station e.g. "COCHEM" - must be named equally like list entry in data.
#' @param quantile numeric; If Input is 0.1; 90% of all Values are bigger than this Value.
#' @param year numeric;  hydrological year November - October (given year/given year +1)
#' @param graph logical; default=T. If graph=T, function returns graph, visualizing LowFlowPeriod. If graph=F, function returns List containing characteristic Values of LowFlow Period.
#'
#' @return Graph/List. Visualizing/ characterizing Low Flow Period. Threshold based.
#' @export
#'@import ggplot2
#' @examples
#' \dontrun{ plot=periodplot_quantile (data, "COCHEM" , 0.3, 2006)}
periodplot_quantile= function (data, station , quantile, year, graph=T){


data=data[[station]]
min=paste(as.character(year), "-11-01")
min=sub(" -", "-",min)
max=paste(as.character(year+1), "-10-31")
max=sub(" -", "-",max)
mindate=grep(min,data[,1])
maxdate=grep(max,data[,1])
Val=data[,2]
U=quantile(Val, probs=quantile)
datayear=data[mindate:maxdate,]
valyear=datayear[,2]
le=length(mindate:maxdate)







#calculate deficite
if(any(valyear<U)==F){return(paste("No results.Please select a value higher than",U, "or change the year"))}else{
g=which(valyear<U)
vals=valyear[g]
suml=length(g)
sum=rep(0, suml)
for(i in 1:suml){
  sum[i]=U-vals[i]
}
deficite=sum(sum)

uU=g
l=length(uU)
c=rep(0,l)#oder l-1
for ( i in 1:l){
  c[i]=(uU[i+1]-uU[i])
}



G=which(c>1)
c[G]=0  #Vektor c in 0 und 1
c[is.na(c)] = 0


d=c
l=length(c)
e=rep(0,l)

for (i in 2:l){
  e[1]=d[1]
  if ((e[i-1]+d[i]) > e[i-1]){e[i]=e[i-1]+d[i]}
  else {e[i]=0}
}



if (graph==T){
plot=ggplot()+labs(title=paste("Low Flow Period at", station, "in", year, "/",year+1), subtitle = paste("Threshold:",U, "the", quantile*100, "% quantile (of complete timeseries at the station). Mean Value:", mean(data[,2])), caption=paste("Volume of deficite: ",deficite, ". Amount of days under Threshold:", suml, ".Longest Low Flow period is", max(e), "days."))+
  ylab("Discharge Value")+xlab("Days")+
  geom_polygon(aes(c(datayear$YYYY.MM.DD[1],datayear$YYYY.MM.DD[1],  datayear$YYYY.MM.DD[le], datayear$YYYY.MM.DD[le] ),c(0,U,U,0 ), col="i"), colour="red", fill="brown3")+
  geom_polygon(aes(c(datayear$YYYY.MM.DD[1], datayear$YYYY.MM.D, datayear$YYYY.MM.DD[le] ), c(0, valyear, 0)), colour="cornflowerblue", fill="cornflowerblue")+
  geom_hline(yintercept = U, linetype=2, colour="black")

plot
return(plot)}

else{
  lb= list(paste(year, year+1),U,quantile*100 , deficite, suml, max(e))

  names(lb)=c("Hydrological Year", "Threshold", "Quantile [%]","Deficite",  "Sum of days under Threshold", "Longest Period in Hydrological Year [days] ")


  return(lb)
}}

}


