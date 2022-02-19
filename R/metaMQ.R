

#nor_metaMQ_1820_2019=metaMQ(1820, 2019, metadata, data)
#write.csv(nor_metaMQ_1820_2019, "f.nor_metaMQ_1820_2019.csv")
#nor_metaMQ_1840_2019=metaMQ(1840, 2019, metadata, data)
#write.csv(nor_metaMQ_1840_2019, "f.nor_metaMQ_1840_2019.csv")
 #write.csv(nor_metaMQ_1940_2019, "nor_metaMQ_1940_2019.csv")
#nor_metaMQ_1860_2019=metaMQ(1860, 2019, metadata, data)
#write.csv(nor_metaMQ_1860_2019, "f.nor_metaMQ_1860_2019.csv")
#nor_metaMQ_1900_2019=metaMQ(1900, 2019, metadata, data)
#write.csv(nor_metaMQ_1900_2019, "f.nor_metaMQ_1900_2019.csv")
#View(nor_metaMQ_1900_2019)
nor_metaMQ_1940_2019=metaMQ(1940, 2019, metadata, data=data_)
write.csv(nor_metaMQ_1940_2019, "f.nor_metaMQ_1940_2019.csv")

View(data_)
Startyear=1940
Endyear=2019

data=data2
#' metaMQ
#'
#'
#'@description Function creates metadataset. Measurements of stations within metadataset are at least as long as the given timeframe. To guarantee comparability between stations the measurement series of the stations are adapted to the time frame and shortened to the same length.
#'
#'
#' @param Startyear numeric; startyear of timerange.
#' @param Endyear numeric; endyear of timerange.
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




  for ( i in 1:l ){

    datan=data[[stations[i]]]


    min=paste((Startyear+1), "-11") #calc min of dataset
    min=sub(" -", "-",  min)

    min=min(grep(min, datan[,1]))

    max=paste((Endyear-1), "-10")
    max=sub(" -", "-",  max)

    max=max(grep(max, datan[,1]))

    datak=datan[min:max,]

    MEAN=(mean(datak[,2]))



    years=(Startyear+1):(Endyear-1)
    ls=length(years)
    lm=length(years)-1
    MQ=rep(0,lm)
    NMQ=rep(0,lm)




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
      mean=(mean(h[,2]))
      MQ[t]=mean
      NMQ[t]=(mean/MEAN)



    }




    hyears=years[-length(years)]
    df=data.frame(hyears, MQ, NMQ)


    zyp=zyp.trend.vector(df$NMQ, df$hyears, "yuepilon")
    linmod=lm(df$NMQ~df$hyears)


    Yslopezyp[i]=zyp[2]
    Yinterceptzyp[i]=zyp[11]
    Ysigzyp[i]=zyp[6]

    Yslopelm[i]=as.numeric(linmod$coefficients[2])
    Yintlm[i]=as.numeric(linmod$coefficients[1])





  # Winter ------------------------------------------------------------------






    MQ=rep(0,lm)
    NMQ=rep(0,lm)




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
      mean=(mean(h[,2]))
      MQ[t]=mean
      NMQ[t]=(mean/MEAN)



    }




    hyears=years[-length(years)]
    wdf=data.frame(hyears, MQ, NMQ)


    zyp=zyp.trend.vector(wdf$NMQ, wdf$hyears, "yuepilon")
    linmod=lm(wdf$NMQ~wdf$hyears)


    Wslopezyp[i]=zyp[2]
    Winterceptzyp[i]=zyp[11]
    Wsigzyp[i]=zyp[6]

    Wslopelm[i]=as.numeric(linmod$coefficients[2])
    Wintlm[i]=as.numeric(linmod$coefficients[1])








# Spring ------------------------------------------------------------------



    MQ=rep(0,lm)
    NMQ=rep(0,lm)

    for (t in 2:ls){
      yearmin=years[t]



      min=paste(yearmin,"-02")
      min=sub(" -", "-",min)
      min
      max= paste(yearmin,"-04")


      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))

      h=datak[start:end, ]
      mean=(mean(h[,2]))
      MQ[t]=mean
      NMQ[t]=(mean/MEAN)



    }






    MQ=MQ[-1]
    NMQ=NMQ[-1]

    hyears=years[-1]
    spdf=data.frame(hyears, MQ, NMQ)


    zyp=zyp.trend.vector(spdf$NMQ, spdf$hyears, "yuepilon")
    linmod=lm(spdf$NMQ~spdf$hyears)





    Spslopezyp[i]=zyp[2]
    Spinterceptzyp[i]=zyp[11]
    Spsigzyp[i]=zyp[6]

    Spslopelm[i]=as.numeric(linmod$coefficients[2])
    Spintlm[i]=as.numeric(linmod$coefficients[1])






  # Summer renew ------------------------------------------------------------

    MQ=rep(0,lm)
    NMQ=rep(0,lm)




    for (t in 2:ls){
      yearmin=years[t]






      min=paste(yearmin,"-05")
      min=sub(" -", "-",min)

      max= paste(yearmin,"-07")



      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))

      h=datak[start:end, ]
      mean=(mean(h[,2]))
      MQ[t]=mean
      NMQ[t]=(mean/MEAN)



    }

    MQ=MQ[-1]
    NMQ=NMQ[-1]

    hyears=years[-1]

sdf=data.frame(hyears, MQ, NMQ)




    zyp=zyp.trend.vector(sdf$NMQ, sdf$hyears, "yuepilon")
    linmod=lm(sdf$NMQ~sdf$hyears)






    Sslopezyp[i]=zyp[2]
    Sinterceptzyp[i]=zyp[11]
    Ssigzyp[i]=zyp[6]

    Sslopelm[i]=as.numeric(linmod$coefficients[2])
    Sintlm[i]=as.numeric(linmod$coefficients[1])






  # Autumn ------------------------------------------------------------------




    MQ=rep(0,ls)
    NMQ=rep(0,ls)




    for (t in 2:ls){
      yearmin=years[t]



      min=paste(yearmin,"-08")
      min=sub(" -", "-",min)
      min
      max= paste(yearmin,"-10")
      max=sub(" -", "-",max)
      start=min(grep(min, datak[,1]))
      end=max(grep(max, datak[,1]))

      h=datak[start:end, ]
      mean=(mean(h[,2]))
      MQ[t]=mean
      NMQ[t]=(mean/MEAN)



    }


    MQ=MQ[-1]
    NMQ=NMQ[-1]

    hyears=years[-1]
    adf=data.frame(hyears, MQ, NMQ)

    zyp=zyp.trend.vector(adf$NMQ, adf$hyears, "yuepilon")
    linmod=lm(adf$NMQ~adf$hyears)






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


