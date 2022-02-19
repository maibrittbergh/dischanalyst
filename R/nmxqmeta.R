





#View(new_nmxq_1820_2019)
#write.csv(new_nmxq_1820_2019, "7_new_nmxq_19_2019.csv")
#new_nmxq_1820_2019=  NMxQmeta(7, data, metadata, 1820, 2019)



#' NMxQmeta
#'
#' @description Function creates dataframe of stations, containing Trend (every year/within a specific season) of their  NMxQ Value within the given timeframe. Therefore it filters all stations within the metadataset depending on the start- and endyear of their Measurement Series.
#'
#' @param x length of period (days). With decreasing Values for x , the influence of short-term anthropogenic influences increases. E.g. x=7, x=14, x=30
#' @param data  list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param metadata Data Frame. Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function.
#' @param Startyear  numeric; startyear of timerange.
#' @param Endyear  numeric; endyear of timerange.
#'
#' @return dataframe. Including the stationname, the river, the spatial information of the station, the trend (linear model and zyp/"yuepilon"approach (with PreWhitening and Autocorrelation)) trend within every year/winter/summer/spring/autumn in timeframe.
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' NMxQmeta(7 , data, metadata, 1820, 2019)}
#'






NMxQmeta=function(x, data, metadata, Startyear, Endyear){



  dataset=which(metadata$startyear<=Startyear)

  datasetnames=metadata$station[dataset]  #stationnames that fit in startyear


  datasetend=which(metadata$endyear>=Endyear)

  datasetendnames=metadata$station[datasetend]  #stationnames that fit in endyear




  vec=rep(F,length(dataset))
  for ( i in 1:length(dataset) ){
    vec[i]=is.element(dataset[i],datasetend)

  }

  stations=dataset[which(vec==T)]


  #stations that fit in start as well as endyear


  ######





  l=length(stations)








  # calculate for Year ------------------------------------------------------


  Yslopezyp=rep(0, l)
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



  for ( i in 1:l){

    datan=data[[stations[i]]]


    min=paste((Startyear+1), "-11") #calc min of dataset
    min=sub(" -", "-",  min)

    min=min(grep(min, datan[,1]))

    max=paste((Endyear-1), "-10")
    max=sub(" -", "-",  max)

    max=max(grep(max, datan[,1]))

    datak=datan[min:max,]


    years=(Startyear+1):(Endyear-1)

    lm=length(years)-1

    ls=length(years)

    minxqy=rep(0,lm)


    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]


      min=paste(yearmin,"-11")
      min=sub(" -", "-",min)
      max= paste(yearmax,"-10")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))


      datal=datak[start:end, ]
      l=nrow(datal)

      le=l-x
      le
      Nmxq=rep(0, le)
      for ( k in 1:le){
        Nmxq[k]=mean(datal[k:(k+x), 2])


      }
      minxqy[t]=(min(Nmxq))


    }

    hyears=years[-length(years)]

    df=data.frame(hyears, minxqy)


    zyp=zyp.trend.vector(df$minxqy, df$hyears, "yuepilon")
    linmod=lm(df$minxqy~df$hyears)


    Yslopezyp[i]=zyp[2]
    Yinterceptzyp[i]=zyp[11]
    Ysigzyp[i]=zyp[6]

    Yslopelm[i]=as.numeric(linmod$coefficients[2])
    Yintlm[i]=as.numeric(linmod$coefficients[1])






    minxqw=rep(0,lm)


    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]


      min=paste(yearmin,"-11")
      min=sub(" -", "-",min)
      max= paste(yearmax,"-01")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))


      datal=datak[start:end, ]
      l=nrow(datal)

      le=l-x
      le
      Nmxq=rep(0, le)
      for ( k in 1:le){
        Nmxq[k]=mean(datal[k:(k+x), 2])


      }
      minxqw[t]=(min(Nmxq))


    }

    hyears=years[-length(years)]

    df=data.frame(hyears, minxqw)


    zyp=zyp.trend.vector(df$minxqw, df$hyears, "yuepilon")
    linmod=lm(df$minxqw~df$hyears)


    Wslopezyp[i]=zyp[2]
    Winterceptzyp[i]=zyp[11]
    Wsigzyp[i]=zyp[6]

    Wslopelm[i]=as.numeric(linmod$coefficients[2])
    Wintlm[i]=as.numeric(linmod$coefficients[1])









  minxqf=rep(0,ls)


    for (t in 2:ls){
      yearmin=years[t]



      min=paste(yearmin,"-02")
      min=sub(" -", "-",min)
      max= paste(yearmin,"-04")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))


      datal=datak[start:end, ]
      p=nrow(datal)

      le=p-x

      Nmxq=rep(0, le)
      for ( k in 1:le){
        Nmxq[k]=mean(datal[k:(k+x), 2])


      }
      minxqf[t]=(min(Nmxq))


    }

    hyears=years[-1]
    minxqf=minxqf[-1]

    df=data.frame(hyears, minxqf)


    zyp=zyp.trend.vector(df$minxqf, df$hyears, "yuepilon")
    linmod=lm(df$minxqf~df$hyears)

    Spslopezyp[i]=zyp[2]
    Spinterceptzyp[i]=zyp[11]
    Spsigzyp[i]=zyp[6]

    Spslopelm[i]=as.numeric(linmod$coefficients[2])
    Spintlm[i]=as.numeric(linmod$coefficients[1])








    minxqs=rep(0,ls)

    for (t in 2:ls){
      yearmin=years[t]



      min=paste(yearmin,"-05")
      min=sub(" -", "-",min)
      max= paste(yearmin,"-07")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))


      datal=datak[start:end, ]
      p=nrow(datal)

      le=p-x

      Nmxq=rep(0, le)
      for ( k in 1:le){
        Nmxq[k]=mean(datal[k:(k+x), 2])


      }
      minxqs[t]=(min(Nmxq))


    }

    hyears=years[-1]
    minxqs=minxqs[-1]

    df=data.frame(hyears, minxqs)


    zyp=zyp.trend.vector(df$minxqs, df$hyears, "yuepilon")
    linmod=lm(df$minxqs~df$hyears)

    Sslopezyp[i]=zyp[2]
    Sinterceptzyp[i]=zyp[11]
    Ssigzyp[i]=zyp[6]

    Sslopelm[i]=as.numeric(linmod$coefficients[2])
    Sintlm[i]=as.numeric(linmod$coefficients[1])





  # Autumn ------------------------------------------------------------------







  minxqa=rep(0,ls)


    for (t in 2:ls){
      yearmin=years[t]



      min=paste(yearmin,"-08")
      min=sub(" -", "-",min)
      max= paste(yearmin,"-10")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))


      datal=datak[start:end, ]
      p=nrow(datal)

      le=p-x

      Nmxq=rep(0, le)
      for ( k in 1:le){
        Nmxq[k]=mean(datal[k:(k+x), 2])


      }
      minxqa[t]=(min(Nmxq))


    }

    hyears=years[-1]
    minxqa=minxqa[-1]

    df=data.frame(hyears, minxqa)


    zyp=zyp.trend.vector(df$minxqa, df$hyears, "yuepilon")
    linmod=lm(df$minxqa~df$hyears)


    Aslopezyp[i]=zyp[2]
    Ainterceptzyp[i]=zyp[11]
    Asigzyp[i]=zyp[6]

    Aslopelm[i]=as.numeric(linmod$coefficients[2])
    Aintlm[i]=as.numeric(linmod$coefficients[1])





  }



  # metadata ----------------------------------------------------------------









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


