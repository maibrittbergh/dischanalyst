




#' Area-wide Minimum-Values Trend
#' @description Calculating Trend for annual minimum values. It is  possible to calculate the trend for the  minimum values of a season or for the whole hydrological year.
#'
#' @param metadata data.frame; Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function.
#' @param data list; Contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param Startyear numeric; Startyear of timerange.
#' @param Endyear numeric; Endyear of timerange.
#'
#'
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
mintrendmeta=function(metadata, data,  Startyear, Endyear){



  # Filter Stations within Timeframe ----------------------------------------



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
  l=length(stations)

  abs_min=rep(0, l)

  #Calculate the year



  Yslopezyp=rep(0, l)         #Calculate Vals for every Station
  Yinterceptzyp=rep(0, l)
  Ysigzyp=rep(0, l)
  Yslopelm=rep(0, l)
  Yintlm=rep(0, l)


  Wslopezyp=rep(0, l)
  Winterceptzyp=rep(0, l)
  Wsigzyp=rep(0, l)
  Wslopelm=rep(0, l)
  Wintlm=rep(0, l)



  Spslopezyp=rep(0, l)
  Spinterceptzyp=rep(0, l)
  Spsigzyp=rep(0, l)
  Spslopelm=rep(0, l)
  Spintlm=rep(0, l)







  Sslopezyp=rep(0, l)
  Sinterceptzyp=rep(0, l)
  Ssigzyp=rep(0, l)
  Sslopelm=rep(0, l)
  Sintlm=rep(0, l)



  Aslopezyp=rep(0, l)
  Ainterceptzyp=rep(0, l)
  Asigzyp=rep(0, l)
  Aslopelm=rep(0, l)
  Aintlm=rep(0, l)




  for( i in 1:l) {

    datan=data[[stations[i]]]

    #Calculate the Absolute Min at Station




    min=paste(Startyear, "-11") #calc min of dataset
    min=sub(" -", "-",  min)

    min=min(grep(min, datan[,1]))

    max=paste(Endyear, "-10")
    max=sub(" -", "-",  max)

    max=max(grep(max, datan[,1]))

    datak=datan[min:max,]    #Stational Dataframe

    abs_min[i]=min(datak[,2])

    years=(Startyear):(Endyear)
    ls=length(years)
    lm=ls-1



    # ANNUAL TRENDS -----------------------------------------------------------



    annualminy=rep(0, lm)

    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]

      min=paste(yearmin,"-11")
      min=sub(" -", "-",min)
      min
      max= paste(yearmax,"-10")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))

      h=datak[start:end, ]
      annualminy[t]=min(h[,2])


    }


    hyears=years[1:lm]
    yf=data.frame(hyears, annualminy)

    zyp=zyp.trend.vector(yf$annualminy, yf$hyears, "yuepilon")
    linmod=lm(yf$annualminy~yf$hyears)


    Yslopezyp[i]=zyp[2]
    Yinterceptzyp[i]=zyp[11]
    Ysigzyp[i]=zyp[6]

    Yslopelm[i]=as.numeric(linmod$coefficients[2])
    Yintlm[i]=as.numeric(linmod$coefficients[1])


    # WINTER ------------------------------------------------------------------



    annualminw=rep(0, lm)


    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]

      min=paste(yearmin,"-11")
      min=sub(" -", "-",min)
      min
      max= paste(yearmax,"-01")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))

      h=datak[start:end, ]
      annualminw[t]=min(h[,2])


    }


    hyears=years[1:lm]
    wf=data.frame(hyears, annualminw )

    zyp=zyp.trend.vector(wf$annualminw, wf$hyears, "yuepilon")
    linmod=lm(wf$annualminw~wf$hyears)


    Wslopezyp[i]=zyp[2]
    Winterceptzyp[i]=zyp[11]
    Wsigzyp[i]=zyp[6]

    Wslopelm[i]=as.numeric(linmod$coefficients[2])
    Wintlm[i]=as.numeric(linmod$coefficients[1])

    # SPRING ------------------------------------------------------------------





    annualminf=rep(0, ls)



    for (t in 2:ls){
      yearmin=years[t]


      min=paste(yearmin,"-02")
      min=sub(" -", "-",min)
      min
      max= paste(yearmax,"-04")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))

      h=datak[start:end, ]
      annualminf[t]=min(h[,2])


    }

    annualminf=annualminf[-1]



    hyears=years[-1]

    spf=data.frame(hyears, annualminf )


    zyp=zyp.trend.vector(spf$annualminf, spf$hyears, "yuepilon")
    linmod=lm(spf$annualminf~spf$hyears)


    Spslopezyp[i]=zyp[2]
    Spinterceptzyp[i]=zyp[11]
    Spsigzyp[i]=zyp[6]

    Spslopelm[i]=as.numeric(linmod$coefficients[2])
    Spintlm[i]=as.numeric(linmod$coefficients[1])



    # SUMMER ------------------------------------------------------------------

    annualmins=rep(0, ls)



    for (t in 2:ls){
      yearmin=years[t]


      min=paste(yearmin,"-05")
      min=sub(" -", "-",min)
      min
      max= paste(yearmax,"-07")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))

      h=datak[start:end, ]
      annualmins[t]=min(h[,2])


    }

    annualmins=annualmins[-1]



    hyears=years[-1]

    sf=data.frame(hyears, annualmins)


    zyp=zyp.trend.vector(sf$annualmins, sf$hyears, "yuepilon")
    linmod=lm(sf$annualmins~sf$hyears)


    Sslopezyp[i]=zyp[2]
    Sinterceptzyp[i]=zyp[11]
    Ssigzyp[i]=zyp[6]

    Sslopelm[i]=as.numeric(linmod$coefficients[2])
    Sintlm[i]=as.numeric(linmod$coefficients[1])



    # AUTUMN  -----------------------------------------------------------------


    annualmina=rep(0, ls)



    for (t in 2:ls){
      yearmin=years[t]


      min=paste(yearmin,"-08")
      min=sub(" -", "-",min)
      min
      max= paste(yearmax,"-10")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))

      h=datak[start:end, ]
      annualmina[t]=min(h[,2])


    }

    annualmina=annualmina[-1]



    hyears=years[-1]

    af=data.frame(hyears, annualmina )


    zyp=zyp.trend.vector(af$annualmina, af$hyears, "yuepilon")
    linmod=lm(af$annualmina~af$hyears)

    Aslopezyp[i]=zyp[2]
    Ainterceptzyp[i]=zyp[11]
    Asigzyp[i]=zyp[6]

    Aslopelm[i]=as.numeric(linmod$coefficients[2])
    Aintlm[i]=as.numeric(linmod$coefficients[1])




  }

  #metadata


  meta=data.frame( metadata$station[stations] ,  metadata$river[stations]  , metadata$longitude[stations],
                   metadata$latitude[stations],
                   Yslopezyp,   #Year
                   Yinterceptzyp,
                   Ysigzyp,

                   Yslopelm,
                   Yintlm,

                   Wslopezyp,#Winter
                   Winterceptzyp,
                   Wsigzyp,

                   Wslopelm,
                   Wintlm,
                   Spslopezyp,  #Spring
                   Spinterceptzyp,
                   Spsigzyp,

                   Spslopelm,
                   Spintlm,

                   Sslopezyp,  #Summer
                   Sinterceptzyp,
                   Ssigzyp,

                   Sslopelm,
                   Sintlm,

                   Aslopezyp,   #Autumn
                   Ainterceptzyp,
                   Asigzyp,

                   Aslopelm,
                   Aintlm




  )

  colnames(meta)[c(1,2,3,4)]=c("station","river", "longitude","latitude" )


  return(meta)


}
