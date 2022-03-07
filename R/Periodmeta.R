
#' Area-wide Low Flow Period Trend
#'
#' @param metadata data.frame; Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function.
#' @param data list; Contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param Startyear numeric; Startyear of timerange.
#' @param Endyear numeric; Endyear of timerange.
#'@import stats
#'@import zyp
#'
#' @return dataframe. Including the stationname, the river, the spatial information of the station, the trend (linear model and zyp/"yuepilon"approach (with PreWhitening and Autocorrelation)) trend within every year/winter/summer/spring/autumn in timeframe.
#' \describe{
#'   \item{intercept_zyp}{intercept created by \link[zyp]{zyp.trend.vector}}
#'   \item{slope_zyp}{slope created by \link[zyp]{zyp.trend.vector}}
#'   \item{sig_zyp}{significance (Kendall's P-Value) for the final detrended time-series}
#'   \item{intercept_ls}{intercept created by \link[stats]{lm}}
#'   \item{slope_ls}{slope created by \link[stats]{lm}}
#' }
#' @export
#'
#' @examples
periodmeta=function(metadata, data, Startyear, Endyear){




  U_periody=function(U, station, data, year,h=T){

    nbr=which(names(data)==station)



    if (h==T){


      one=rep("0",2)
      two=rep("0", 10)

      for (i in 1:2){
        hydroyear1=c("11", "12" )
        one[i]=paste(year,"-",hydroyear1[i])
        one=sub(" - ", "-", one)

      }
      for (i in 1:10){
        hydroyear2=c("01","02", "03", "04", "05", "06", "07", "08", "09", "10")
        y=as.numeric(year)
        y2=as.character(y+1)
        two[i]=paste(y2,"-",hydroyear2[i])
        two=sub(" - ", "-", two)
      }
      year_=c(one,two)

      Nov=grep(year_[1],data[[nbr]][,1] )
      Dec=grep(year_[2],data[[nbr]][,1] )
      Jan=grep(year_[3],data[[nbr]][,1] )
      Feb=grep(year_[4],data[[nbr]][,1] )
      Mar=grep(year_[5],data[[nbr]][,1] )
      April=grep(year_[6],data[[nbr]][,1] )
      May=grep(year_[7],data[[nbr]][,1] )
      June=grep(year_[8],data[[nbr]][,1] )
      July=grep(year_[9],data[[nbr]][,1] )
      August=grep(year_[10],data[[nbr]][,1] )
      Sep=grep(year_[11],data[[nbr]][,1] )
      Oct=grep(year_[12],data[[nbr]][,1] )


      j=c(Nov,Dec,Jan, Feb,Mar, April, May, June, July, August, Sep, Oct)


    }else{


      j=grep(year, data[[nbr]][,1])

    }
    Val=data[[nbr]][,2][j]
    uU=which(Val<U)
    l=length(uU)
    if (isTRUE(all(Val>U))) {


      a=0
      return(a)

    }else {
      c=rep(0,l)#oder l-1
      for ( i in 1:l){
        c[i]=(uU[i+1]-uU[i])
      }
      index=cbind(c, uU)
      G=which(c>1)
      c[G]=0  #Vektor c in 0 und 1
      c[is.na(c)] = 0


      d=c
      l=length(c)
      e=rep(0,l)


      for (i in 2:l){
        e[1]=d[1]
        if (isTRUE((e[i-1]+d[i]) > e[i-1])){e[i]=e[i-1]+d[i]}
        else {e[i]=0}
      }


      vector=c(max(e), length(uU))
      return(vector)

    }

  }


  # Stations within timeframe -----------------------------------------------



dataset=which(metadata$startyear<=Startyear)

#stationnames that fit in startyear
ld=length(dataset)
starthydro=rep(0,ld )
for ( i in 1:ld){

  starthydro[i]=as.numeric(substr(metadata$startday[dataset[i]], 6,7))
  start=which(starthydro<12)

}
dataset=dataset[start]
datasetnames=metadata$station[dataset]

#Stations ending at least with October within Endyear

datasetend=which(metadata$endyear>=Endyear)
lde=length( datasetend)
endhydro=rep(0,lde )
for ( i in 1:lde){

  endhydro[i]=as.numeric(substr(metadata$endday[datasetend[i]], 6,7))
  end=which(endhydro>9)

}

datasetend=datasetend[end]

datasetendnames=metadata$station[datasetend]

#stationnames that fit in endyear




vec=rep(F,length(dataset))
for ( i in 1:length(dataset) ){
  vec[i]=is.element(dataset[i],datasetend)

}

stations=dataset[which(vec==T)]
names=metadata$station[stations]







lst=length(stations)





mat=matrix(nrow=lst, ncol=40 )







mat[,1]=metadata$station[stations]

mat[,2]=metadata$river[stations]

mat[,3]=metadata$longitude[stations]


mat[,4]=metadata$latitude[stations]



colnames(mat)=c("station", "river", "longitude", "latitude", "Q70_tmax_zyp", "Q70_ld_zyp","Q70_tmax_lm", "Q70_ld_lm", "Q75_tmax_zyp",
                "Q75_ld_zyp", "Q75_tmax_lm", "Q75_ld_lm", "Q80_tmax_zyp", "Q80_ld_zyp", "Q80_tmax_lm", "Q80_ld_lm","Q85_tmax_zyp",
                "Q85_ld_zyp", "Q85_tmax_lm", "Q85_ld_lm", "Q90_tmax_zyp", "Q90_ld_zyp"," Q90_tmax_lm", "Q90_ld_lm", "Q95_tmax_zyp",
                "Q95_ld_zyp", "Q95_tmax_lm", "Q95_ld_lm", "Q70sigtmax", "Q75sigtmax", "Q80sigtmax", "Q85sigtmax", "Q90sigtmax", "Q95sigtmax", "Q70sigld",
                "Q75sigld", "Q80sigld", "Q85sigld", "Q90sigld", "Q95sigld")


for ( i in 1:lst){



datan=data[[stations[i]]]
datan

min=paste(Startyear, "-11") #calc min of dataset
min=sub(" -", "-",  min)
min
datan
min=min(grep(min, datan[,1]))

max=paste(Endyear, "-10")
max=sub(" -", "-",  max)

max=max(grep(max, datan[,1]))

datak=datan[min:max,]



VAL=datak[,2]
quantiles=quantile(VAL, probs=c(0.05, 0.1,0.25,0.2,0.35,0.3), na.rm=T)

years=Startyear:Endyear
ly=length(years)

ly=length(years)
ly1=ly-1


# Q95 ---------------------------------------------------------------------


tmax=rep(0, ly)
ld=rep(0,ly)
resQ95=data.frame(cbind(years, tmax, ld))

for ( t in 1:ly){



period=U_periody(quantiles[1],names[i] , data, year=years[t],h=T)
  resQ95[t,2]=period[1]
  resQ95[t,3]=period[2]

}
resQ95



# Q90 ---------------------------------------------------------------------



tmax=rep(0, ly)
ld=rep(0,ly)
resQ90=data.frame(cbind(years, tmax, ld))


for ( t in 1:ly){



  period=U_periody(quantiles[2],names[i] , data, year=years[t],h=T)
  resQ90[t,2]=period[1]
  resQ90[t,3]=period[2]

}
resQ90



# Q85 ---------------------------------------------------------------------



tmax=rep(0, ly)
ld=rep(0,ly)
resQ85=data.frame(cbind(years, tmax, ld))


for ( t in 1:ly){



  period=U_periody(quantiles[3],names[i] , data, year=years[t],h=T)
  resQ85[t,2]=period[1]
  resQ85[t,3]=period[2]

}
resQ85



# Q80 ---------------------------------------------------------------------



tmax=rep(0, ly)
ld=rep(0,ly)
resQ80=data.frame(cbind(years, tmax, ld))


for ( t in 1:ly){



  period=U_periody(quantiles[4],names[i] , data, year=years[t],h=T)
  resQ80[t,2]=period[1]
  resQ80[t,3]=period[2]

}
resQ80



# Q75 ---------------------------------------------------------------------




tmax=rep(0, ly)
ld=rep(0,ly)
resQ75=data.frame(cbind(years, tmax, ld))


for ( t in 1:ly){



  period=U_periody(quantiles[5],names[i] , data, year=years[t],h=T)
  resQ75[t,2]=period[1]
  resQ75[t,3]=period[2]

}
resQ75

# Q70 ---------------------------------------------------------------------




tmax=rep(0, ly)
ld=rep(0,ly)
resQ70=data.frame(cbind(years, tmax, ld))


for ( t in 1:ly){



  period=U_periody(quantiles[6],names[i] , data, year=years[t],h=T)
  resQ70[t,2]=period[1]
  resQ70[t,3]=period[2]

}

resQ70




Q70zyp=zyp.trend.vector(resQ70$tmax, method="yuepilon")
mat[i,5]=Q70zyp[2]
Q70zyp2=zyp.trend.vector(resQ70$ld,resQ70$years,method="yuepilon")
mat[i,6]=Q70zyp2[2]
Q70lm=lm(resQ70$tmax~resQ70$years)
mat[i,7]=Q70lm$coefficients[2]
Q70lm2=lm(resQ70$ld~resQ70$years)
mat[i,8]=Q70lm2$coefficients[2]

Q75zyp=zyp.trend.vector(resQ75$tmax,resQ75$years, method="yuepilon")
mat[i,9]=Q75zyp[2]
Q75zyp2=zyp.trend.vector(resQ75$ld, resQ75$years,method="yuepilon")
mat[i,10]=Q75zyp2[2]
Q75lm=lm(resQ75$tmax~resQ75$years)
mat[i,11]=Q75lm$coefficients[2]
Q75lm2=lm(resQ75$ld~resQ75$years)
mat[i,12]=Q75lm2$coefficients[2]


Q80zyp=zyp.trend.vector(resQ80$tmax,resQ80$years, method="yuepilon")
mat[i,13]=Q80zyp[2]
Q80zyp2=zyp.trend.vector(resQ80$ld, resQ80$years,method="yuepilon")
mat[i,14]=Q80zyp2[2]
Q80lm=lm(resQ80$tmax~resQ80$years)
mat[i,15]=Q80lm$coefficients[2]
Q80lm2=lm(resQ80$ld~resQ80$years)
mat[i,16]=Q80lm2$coefficients[2]


Q85zyp=zyp.trend.vector(resQ85$tmax,resQ85$years, method="yuepilon")
mat[i,17]=Q85zyp[2]
Q85zyp2=zyp.trend.vector(resQ85$ld, resQ85$years,method="yuepilon")
mat[i,18]=Q85zyp2[2]
Q85lm=lm(resQ85$tmax~resQ85$years)
mat[i,19]=Q85lm$coefficients[2]
Q85lm2=lm(resQ85$ld~resQ85$years)
mat[i,20]=Q85lm2$coefficients[2]


Q90zyp=zyp.trend.vector(resQ90$tmax,resQ90$years, method="yuepilon")
mat[i,21]=Q90zyp[2]
Q90zyp2=zyp.trend.vector(resQ90$ld, resQ90$years, method="yuepilon")
mat[i,22]=Q90zyp2[2]
Q90lm=lm(resQ90$tmax~resQ90$years)
mat[i,23]=Q90lm$coefficients[2]
Q90lm2=lm(resQ90$ld~resQ90$years)
mat[i,24]=Q90lm2$coefficients[2]




Q95zyp=zyp.trend.vector(resQ95$tmax,resQ95$years, method="yuepilon")
mat[i,25]=Q95zyp[2]
Q95zyp2=zyp.trend.vector(resQ95$ld, resQ95$years,method="yuepilon")
mat[i,26]=Q95zyp2[2]
Q95lm=lm(resQ95$tmax~resQ95$years)
mat[i,27]=Q95lm$coefficients[2]
Q95lm2=lm(resQ95$ld~resQ95$years)
mat[i,28]=Q95lm2$coefficients[2]


mat[i, 29]=Q70zyp[6]
mat[i, 30]=Q75zyp[6]
mat[i, 31]=Q80zyp[6]
mat[i, 32]=Q85zyp[6]
mat[i, 33]=Q90zyp[6]
mat[i, 34]=Q95zyp[6]

mat[i, 35]=Q70zyp2[6]
mat[i, 36]=Q75zyp2[6]
mat[i, 37]=Q80zyp2[6]
mat[i, 38]=Q85zyp2[6]
mat[i, 39]=Q90zyp2[6]
mat[i, 40]=Q95zyp2[6]


}
mat

return(mat)
}







