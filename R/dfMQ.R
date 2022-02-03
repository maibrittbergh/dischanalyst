
#' MQ Trend for multiple stations
#'
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param seasonal character; default:"Y". Possible Inputs:  "Y"( MQ Trend of (hydrological) Years); "WI"(MQ Trend of Winters during years); "SP"(MQ Trend of Springs during (hydrological) years); "SU"(MQ Trend of Summers during (hydrological) years); "AU"(MQ Trend of Autums during (hydrological) years)
#' @param mod numeric; possible input: 1,2,3. default value: 1; output of both: \link[zyp]{zyp.trend.vector}, \link[stats]{lm}. Defines the way to calculate intercept and slope. For mod=2  \link[zyp]{zyp.trend.vector} with PreWhitening by "yuepilon-method" is used. Sen-Slope-Approach used to define direction of the trend and the significance is  determined by Kendall's P-Value computed for the final detrendet time series. For mod=3: \link[stats]{lm} with a least squares approach is used.
#'
#'
#'@import ggplot2
#'@import zyp
#'
#' @return dataframe. Contains for every station columns: "Stationname", "intercept-zyp","slope-zyp","significance-zyp","linear model- intercept","linear model-slope","normalized slope", "Season" .
#' @export
#'
#' @examples
#' \dontrun{ dfMQ(data, seasonal="AU", 3)}
#'
dfMQ=function(data, seasonal="Y", mod=1){


leng=length(data)

mata=data

if (mod==1){

df <- data.frame(matrix(ncol = 8, nrow = leng))
colnames(df)=c("Stationname", "intercept-zyp","slope-zyp","significance-zyp","linear model- intercept","linear model-slope","normalized_slope", "Season" )


for(k in 1:leng){
data=mata
  datan=mata[[k]]
  dates=mata[[k]][,1]
  values=mata[[k]][,2]
  MEAN=mean(values)


if (seasonal=="Y"){

  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){
    yearmin=years[i]
    yearmax=years[i+1]

    min=paste(yearmin,"-", "11-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "10-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    h=datan[start:end, ]
    MQ[i]=round(mean(h[,2],0))

  }

  results=data.frame(cbind(MQ, years[-l]))


  model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])



   df[k,]=c(names(data[k]), int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100, "YEAR" )





}else if (seasonal=="WI"){


  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){
    yearmin=years[i]
    yearmax=years[i+1]

    min=paste(yearmin,"-", "11-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "01-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

  h=datan[start:end, ]
    MQ[i]=round(mean(h[,2],0))

  }



  results=data.frame(cbind(MQ, years[-l]))




  model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  df[k,]=c(names(data[k]), int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100, "WINTER" )



}else if (seasonal=="SP"){



  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "02-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "04-30")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

    h=datan[start:end, ]
    MQ[i]=round(mean(h[,2],0))

  }



  results=data.frame(cbind(MQ, years[-l]))




  model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  df[k,]=c(names(data[k]), int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100, "SPRING" )

}else if (seasonal=="SU"){



  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "05-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "07-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

 h=datan[start:end, ]
    MQ[i]=round(mean(h[,2],0))

  }



  results=data.frame(cbind(MQ, years[-l]))



  model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])


  df[k,]=c(names(data[k]), int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100, "SUMMER" )


}else if (seasonal=="AU"){



  l=nrow(datan)
  startyear=as.numeric(substr(datan[1,1],1,4))
  endyear=as.numeric(substr(datan[l,1],1,4))-1
  years=c(startyear:endyear)
  l=length(years)-1
  MQ=rep(0,l)

  for (i in 1:l){

    yearmax=years[i+1]

    min=paste(yearmax,"-", "08-01")
    min=sub(" - ", "-",min)
    max= paste(yearmax,"-", "10-31")
    max=sub(" - ", "-",max)
    start=grep(min, datan[,1])
    end=grep(max, datan[,1])

 h=datan[start:end, ]
    MQ[i]=round(mean(h[,2],0))

  }



  results=data.frame(cbind(MQ, years[-l]))




  model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")

  linmod=lm(MQ~V2, results)
  slop=as.numeric(model[2])
  sig=as.numeric(model[6])
  int=as.numeric(model[11])

  df[k,]=c(names(data[k]), int, slop, sig, linmod$coefficients[1] ,linmod$coefficients[2], (slop/MEAN)*100, "AUTUMN" )


}

}


  return(df)

}
else if(mod==2){
  #zyp only

  df <- data.frame(matrix(ncol = 6, nrow = leng))
  colnames(df)=c("Stationname", "intercept-zyp","slope-zyp","significance-zyp","normalized slope", "Season" )


  for(k in 1:leng){
    data=mata
    datan=mata[[k]]
    dates=mata[[k]][,1]
    values=mata[[k]][,2]
    MEAN=mean(values)


    if (seasonal=="Y"){

      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){
        yearmin=years[i]
        yearmax=years[i+1]

        min=paste(yearmin,"-", "11-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }

      results=data.frame(cbind(MQ, years[-l]))


      model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")

      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])



      df[k,]=c(names(data[k]), int, slop, sig,(slop/MEAN)*100, "YEAR" )





    }else if (seasonal=="WI"){


      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){
        yearmin=years[i]
        yearmax=years[i+1]

        min=paste(yearmin,"-", "11-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "01-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))




      model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")


      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])

      df[k,]=c(names(data[k]), int, slop, sig,  (slop/MEAN)*100, "WINTER" )



    }else if (seasonal=="SP"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "02-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "04-30")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))




      model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")


      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])

      df[k,]=c(names(data[k]), int, slop, sig,  (slop/MEAN)*100, "SPRING" )

    }else if (seasonal=="SU"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "05-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "07-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))



      model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")


      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])


      df[k,]=c(names(data[k]), int, slop, sig, (slop/MEAN)*100, "SUMMER" )


    }else if (seasonal=="AU"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "08-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))




      model=zyp.trend.vector(results[,1], results[,2] , "yuepilon")

      slop=as.numeric(model[2])
      sig=as.numeric(model[6])
      int=as.numeric(model[11])

      df[k,]=c(names(data[k]), int, slop, sig, (slop/MEAN)*100, "AUTUMN" )


    }

  }


  return(df)









}
else if (mod==3){



  df <- data.frame(matrix(ncol = 5, nrow = leng))
  colnames(df)=c("Stationname", "linear model- intercept","linear model-slope","normalized slope", "Season" )


  for(k in 1:leng){
    data=mata
    datan=mata[[k]]
    dates=mata[[k]][,1]
    values=mata[[k]][,2]
    MEAN=mean(values)


    if (seasonal=="Y"){

      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){
        yearmin=years[i]
        yearmax=years[i+1]

        min=paste(yearmin,"-", "11-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }

      results=data.frame(cbind(MQ, years[-l]))



      linmod=lm(MQ~V2, results)




      df[k,]=c(names(data[k]),  linmod$coefficients[1] ,linmod$coefficients[2], (linmod$coefficients[2]/MEAN)*100, "YEAR" )





    }else if (seasonal=="WI"){


      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){
        yearmin=years[i]
        yearmax=years[i+1]

        min=paste(yearmin,"-", "11-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "01-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))






      linmod=lm(MQ~V2, results)


      df[k,]=c(names(data[k]),  linmod$coefficients[1] ,linmod$coefficients[2], (linmod$coefficients[2]/MEAN)*100, "Winter" )



    }else if (seasonal=="SP"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "02-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "04-30")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))




      linmod=lm(MQ~V2, results)

      df[k,]=c(names(data[k]),  linmod$coefficients[1] ,linmod$coefficients[2], (linmod$coefficients[2]/MEAN)*100, "SRING" )

    }else if (seasonal=="SU"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "05-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "07-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))





      linmod=lm(MQ~V2, results)



      df[k,]=c(names(data[k]),  linmod$coefficients[1] ,linmod$coefficients[2], (linmod$coefficients[2]/MEAN)*100, "SUMMER" )


    }else if (seasonal=="AU"){



      l=nrow(datan)
      startyear=as.numeric(substr(datan[1,1],1,4))
      endyear=as.numeric(substr(datan[l,1],1,4))-1
      years=c(startyear:endyear)
      l=length(years)-1
      MQ=rep(0,l)

      for (i in 1:l){

        yearmax=years[i+1]

        min=paste(yearmax,"-", "08-01")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10-31")
        max=sub(" - ", "-",max)
        start=grep(min, datan[,1])
        end=grep(max, datan[,1])

        h=datan[start:end, ]
        MQ[i]=round(mean(h[,2],0))

      }



      results=data.frame(cbind(MQ, years[-l]))


      linmod=lm(MQ~V2, results)


      df[k,]=c(names(data[k]),  linmod$coefficients[1] ,linmod$coefficients[2], (linmod$coefficients[2]/MEAN)*100, "AUTUMN" )


    }

  }


  return(df)


}


}







