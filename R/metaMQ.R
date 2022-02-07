



#' metaMQ
#'
#'
#'@description Function creates metadataset. Measurements of stations within metadataset are at least as long as the given timeframe. To guarantee comparability between stations the measurement series of the stations are adapted to the time frame and shortened to the same length.
#'
#'
#' @param Startyear numeric; Startyear of timerange
#' @param Endyear numeric; Endyear of Timerange
#' @param metadata  Data Frame. Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#'
#'@import zyp
#'@import stats
#'
#' @return dataframe; metadata of stations whose measurement series are at least as long as the given time frame. The Dataframe includes different approaches to calculate the trend of the annual Normalized MQ (within the whole Year/the Spring/the Summer/the Autumn/the Winter)
#' @export
#'
#' @examples
#' \dontrun{
#' MQtf1820_2019=dataset(1820, 2019, metadata, data)

#'}
#'
#'
#'
#'
metaMQ=function(Startyear, Endyear, metadata, data){
  Startyear=1820
  Endyear=2019
  metadata=metadata_repg(metadata_germany)
  data=grdc_list(metadata, path)

  dataset=which(metadata$startyear<=Startyear)

  dataset=metadata$station[dataset]
  dataset

  datasetend=which(metadata$endyear>=Endyear)

  datasetend=metadata$station[datasetend]




  vec=rep(F,length(dataset))
  for ( i in 1:length(dataset) ){
    vec[i]=is.element(dataset[i],datasetend)

  }

  stations=which(vec==T)

  stations=dataset[stations]


  ######


  datanew=data[stations]
  l=length(datanew)
  index=rep(0, l)

  list=vector(mode="list", length=l)





  # calculate for Year ------------------------------------------------------


  Yslopezyp=rep(0, l)
  Yinterceptzyp=rep(0, l)
  Ysigzyp=rep(0, l)
  Yslopelm=rep(0, l)
  Yintlm=rep(0, l)



  for ( i in 1:l){

    datan=datanew[[i]]
    ld=nrow(datan)

    startyear=as.numeric(substr(datan[1,1],1,4))+1
    endyear=as.numeric(substr(datan[ld,1],1,4))
    years=startyear:endyear

    lm=length(years)-1
    MQ=rep(0,lm)
    NMQ=rep(0,lm)
    MEAN=round(mean(datan[,2],0))

    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]

      min=paste(yearmin,"-", "11-01")
      min=sub(" - ", "-",min)
      min
      max= paste(yearmax,"-", "10-31")
      max=sub(" - ", "-",max)
      start=grep(min, datan[,1])
      end=grep(max, datan[,1])

      h=datan[start:end, ]
      mean=round(mean(h[,2],2))
      MQ[t]=mean
      NMQ[t]=round(mean/MEAN,2)



    }

    hyears=years[-lm]
    df=data.frame(hyears, MQ, NMQ)



    zyp=zyp.trend.vector(df$NMQ, df$hyears, "yuepilon")
    linmod=lm(df$NMQ~df$hyears)


    Yslopezyp[i]=zyp[2]
    Yinterceptzyp[i]=zyp[11]
    Ysigzyp[i]=zyp[6]

    Yslopelm[i]=as.numeric(linmod$coefficients[2])
    Yintlm[i]=as.numeric(linmod$coefficients[1])
    index[i]=which(metadata$station==stations[i])


  }






  # Winter ------------------------------------------------------------------






  Wslopezyp=rep(0, l)
  Winterceptzyp=rep(0, l)
  Wsigzyp=rep(0, l)
  Wslopelm=rep(0, l)
  Wintlm=rep(0, l)



  for ( i in 1:l){

    datan=datanew[[i]]
    ld=nrow(datan)

    startyear=as.numeric(substr(datan[1,1],1,4))+1
    endyear=as.numeric(substr(datan[ld,1],1,4))
    years=startyear:endyear

    lm=length(years)-1
    MQ=rep(0,lm)
    NMQ=rep(0,lm)
    MEAN=round(mean(datan[,2],0))

    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]

      min=paste(yearmin,"-", "11-01")
      min=sub(" - ", "-",min)
      min
      max= paste(yearmax,"-", "01-31")
      max=sub(" - ", "-",max)
      start=grep(min, datan[,1])
      end=grep(max, datan[,1])

      h=datan[start:end, ]
      mean=round(mean(h[,2],2))
      MQ[t]=mean
      NMQ[t]=round(mean/MEAN,2)



    }

    hyears=years[-lm]
    df=data.frame(hyears, MQ, NMQ)



    zyp=zyp.trend.vector(df$NMQ, df$hyears, "yuepilon")
    linmod=lm(df$NMQ~df$hyears)


    Wslopezyp[i]=zyp[2]
    Winterceptzyp[i]=zyp[11]
    Wsigzyp[i]=zyp[6]

    Wslopelm[i]=as.numeric(linmod$coefficients[2])
    Wintlm[i]=as.numeric(linmod$coefficients[1])


  }



  # Spring ------------------------------------------------------------------

  Spslopezyp=rep(0, l)
  Spinterceptzyp=rep(0, l)
  Spsigzyp=rep(0, l)
  Spslopelm=rep(0, l)
  Spintlm=rep(0, l)



  for ( i in 1:l){

    datan=datanew[[i]]
    ld=nrow(datan)

    startyear=as.numeric(substr(datan[1,1],1,4))+1
    endyear=as.numeric(substr(datan[ld,1],1,4))
    years=startyear:endyear

    lm=length(years)-1
    MQ=rep(0,lm)
    NMQ=rep(0,lm)
    MEAN=round(mean(datan[,2],0))

    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]

      min=paste(yearmin,"-", "02-01")
      min=sub(" - ", "-",min)
      min
      max= paste(yearmax,"-", "04-30")
      max=sub(" - ", "-",max)
      start=grep(min, datan[,1])
      end=grep(max, datan[,1])

      h=datan[start:end, ]
      mean=round(mean(h[,2],2))
      MQ[t]=mean
      NMQ[t]=round(mean/MEAN,2)



    }

    hyears=years[-lm]
    df=data.frame(hyears, MQ, NMQ)



    zyp=zyp.trend.vector(df$NMQ, df$hyears, "yuepilon")
    linmod=lm(df$NMQ~df$hyears)


    Spslopezyp[i]=zyp[2]
    Spinterceptzyp[i]=zyp[11]
    Spsigzyp[i]=zyp[6]

    Spslopelm[i]=as.numeric(linmod$coefficients[2])
    Spintlm[i]=as.numeric(linmod$coefficients[1])


  }


  # Summer ------------------------------------------------------------------


  Sslopezyp=rep(0, l)
  Sinterceptzyp=rep(0, l)
  Ssigzyp=rep(0, l)
  Sslopelm=rep(0, l)
  Sintlm=rep(0, l)



  for ( i in 1:l){

    datan=datanew[[i]]
    ld=nrow(datan)

    startyear=as.numeric(substr(datan[1,1],1,4))+1
    endyear=as.numeric(substr(datan[ld,1],1,4))
    years=startyear:endyear

    lm=length(years)-1
    MQ=rep(0,lm)
    NMQ=rep(0,lm)
    MEAN=round(mean(datan[,2],0))

    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]

      min=paste(yearmin,"-", "05-01")
      min=sub(" - ", "-",min)
      min
      max= paste(yearmax,"-", "07-31")
      max=sub(" - ", "-",max)
      start=grep(min, datan[,1])
      end=grep(max, datan[,1])

      h=datan[start:end, ]
      mean=round(mean(h[,2],2))
      MQ[t]=mean
      NMQ[t]=round(mean/MEAN,2)



    }

    hyears=years[-lm]
    df=data.frame(hyears, MQ, NMQ)



    zyp=zyp.trend.vector(df$NMQ, df$hyears, "yuepilon")
    linmod=lm(df$NMQ~df$hyears)


    Sslopezyp[i]=zyp[2]
    Sinterceptzyp[i]=zyp[11]
    Ssigzyp[i]=zyp[6]

    Sslopelm[i]=as.numeric(linmod$coefficients[2])
    Sintlm[i]=as.numeric(linmod$coefficients[1])


  }







  # Autumn ------------------------------------------------------------------

  Aslopezyp=rep(0, l)
  Ainterceptzyp=rep(0, l)
  Asigzyp=rep(0, l)
  Aslopelm=rep(0, l)
  Aintlm=rep(0, l)



  for ( i in 1:l){

    datan=datanew[[i]]
    ld=nrow(datan)

    startyear=as.numeric(substr(datan[1,1],1,4))+1
    endyear=as.numeric(substr(datan[ld,1],1,4))
    years=startyear:endyear

    lm=length(years)-1
    MQ=rep(0,lm)
    NMQ=rep(0,lm)
    MEAN=round(mean(datan[,2],0))

    for (t in 1:lm){
      yearmin=years[t]
      yearmax=years[t+1]

      min=paste(yearmin,"-", "08-01")
      min=sub(" - ", "-",min)
      min
      max= paste(yearmax,"-", "10-31")
      max=sub(" - ", "-",max)
      start=grep(min, datan[,1])
      end=grep(max, datan[,1])

      h=datan[start:end, ]
      mean=round(mean(h[,2],2))
      MQ[t]=mean
      NMQ[t]=round(mean/MEAN,2)



    }

    hyears=years[-lm]
    df=data.frame(hyears, MQ, NMQ)



    zyp=zyp.trend.vector(df$NMQ, df$hyears, "yuepilon")
    linmod=lm(df$NMQ~df$hyears)


    Aslopezyp[i]=zyp[2]
    Ainterceptzyp[i]=zyp[11]
    Asigzyp[i]=zyp[6]

    Aslopelm[i]=as.numeric(linmod$coefficients[2])
    Aintlm[i]=as.numeric(linmod$coefficients[1])


  }

















  meta=data.frame( metadata$station[index] ,  metadata$river[index]  , metadata$longitude[index],
                  metadata$latitude[index],
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
