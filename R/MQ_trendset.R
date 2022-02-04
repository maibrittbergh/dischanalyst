


#' MQ_trendset
#'
#' @param metadata Data Frame. Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function
#' @param data list; contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.
#' @param seasonal character; default:"Y". Possible Inputs:  "Y"( MQ Trend of (hydrological) Years); "WI"(MQ Trend of Winters during years); "SP"(MQ Trend of Springs during (hydrological) years); "SU"(MQ Trend of Summers during (hydrological) years); "AU"(MQ Trend of Autums during (hydrological) years)
#'
#' @return  list; containing annual Mean Values (or Summer/Winter/Autumn/Spring), normalized Mean Value and Years.
#' @export
#'
#' @examples
#' \dontrun{ MQ_trendset((metadata, data, "seasonal "SP")}
#'
#'
MQ_trendset=function(metadata, data, seasonal ){




  l=nrow(metadata)

  if(seasonal=="Y"){

    list=vector(mode="list", length=l)
    names(list)=names(data)
    for ( i in 1:l){

      datan=data[[i]]
      ld=nrow(datan)

      startyear=as.numeric(substr(datan$YYYY.MM.DD[1],1,4))+1
      endyear=as.numeric(substr(datan$YYYY.MM.DD[ld],1,4))-1
      years=startyear:endyear

      lm=length(years)-1
      MQ=rep(0,lm)
      NMQ=rep(0,lm)
      MEAN=round(mean(datan[,2],0))

      for (t in 1:lm){
        yearmin=years[t]
        yearmax=years[t+1]

        min=paste(yearmin,"-", "11")
        min=sub(" - ", "-",min)


        max= paste(yearmax,"-", "10")
        max=sub(" - ", "-",max)
        start=min(grep(min, datan[,1]))
        end=max(grep(max, datan[,1]))

        h=datan[start:end, ]
        mean=round(mean(h[,2],2))
        MQ[t]=mean
        NMQ[t]=round(mean/MEAN,2)
      }


      hyears=years[1:lm]




      matrix= cbind(MQ, hyears, NMQ)



      list[[i]]=matrix



    }
    names(list)=names(data)
    return(list)


  }else if(seasonal=="WI"){



    list=vector(mode="list", length=l)
    for ( i in 1:l){

      datan=data[[i]]
      ld=nrow(datan)

      startyear=as.numeric(substr(datan$YYYY.MM.DD[1],1,4))+1
      endyear=as.numeric(substr(datan$YYYY.MM.DD[ld],1,4))-1
      years=startyear:endyear

      lm=length(years)-1
      MQ=rep(0,lm)
      NMQ=rep(0,lm)
      MEAN=round(mean(datan[,2],0))

      for (t in 1:lm){
        yearmin=years[t]
        yearmax=years[t+1]

        min=paste(yearmin,"-", "11")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "01")
        max=sub(" - ", "-",max)
        start=min(grep(min, datan[,1]))
        end=max(grep(max, datan[,1]))

        h=datan[start:end, ]
        mean=round(mean(h[,2],2))
        MQ[t]=mean
        NMQ[t]=round(mean/MEAN,2)
      }


      hyears=years[1:lm]




      matrix= cbind(MQ, hyears, NMQ)



      list[[i]]=matrix



    }

    names(list)=names(data)
    return(list)




  }else if(seasonal=="SP"){


    list=vector(mode="list", length=l)
    for ( i in 1:l){

      datan=data[[i]]
      ld=nrow(datan)

      startyear=as.numeric(substr(datan$YYYY.MM.DD[1],1,4))+1
      endyear=as.numeric(substr(datan$YYYY.MM.DD[ld],1,4))-1
      years=startyear:endyear

      lm=length(years)-1
      MQ=rep(0,lm)
      NMQ=rep(0,lm)
      MEAN=round(mean(datan[,2],0))

      for (t in 1:lm){
        yearmin=years[t]
        yearmax=years[t+1]

        min=paste(yearmax,"-", "02")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "04")
        max=sub(" - ", "-",max)
        start=min(grep(min, datan[,1]))
        end=max(grep(max, datan[,1]))

        h=datan[start:end, ]
        mean=round(mean(h[,2],2))
        MQ[t]=mean
        NMQ[t]=round(mean/MEAN,2)
      }



      hyears=years[1:lm]




      matrix= cbind(MQ, hyears, NMQ)



      list[[i]]=matrix



    }

    names(list)=names(data)
    return(list)





  }else if(seasonal=="SU"){

    list=vector(mode="list", length=l)
    for ( i in 1:l){

      datan=data[[i]]
      ld=nrow(datan)

      startyear=as.numeric(substr(datan$YYYY.MM.DD[1],1,4))+1
      endyear=as.numeric(substr(datan$YYYY.MM.DD[ld],1,4))-1
      years=startyear:endyear

      lm=length(years)-1
      MQ=rep(0,lm)
      NMQ=rep(0,lm)
      MEAN=round(mean(datan[,2],0))

      for (t in 1:lm){
        yearmin=years[t]
        yearmax=years[t+1]

        min=paste(yearmax,"-", "05")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "07")
        max=sub(" - ", "-",max)
        start=min(grep(min, datan[,1]))
        end=max(grep(max, datan[,1]))

        h=datan[start:end, ]
        mean=round(mean(h[,2],2))
        MQ[t]=mean
        NMQ[t]=round(mean/MEAN,2)
      }


      hyears=years[1:lm]




      matrix= cbind(MQ, hyears, NMQ)


      list[[i]]=matrix



    }

    names(list)=names(data)
    return(list)





  }else if (seasonal=="AU"){

    list=vector(mode="list", length=l)
    for ( i in 1:l){

      datan=data[[i]]
      ld=nrow(datan)

      startyear=as.numeric(substr(datan$YYYY.MM.DD[1],1,4))+1
      endyear=as.numeric(substr(datan$YYYY.MM.DD[ld],1,4))-1
      years=startyear:endyear

      lm=length(years)-1
      MQ=rep(0,lm)
      NMQ=rep(0,lm)
      MEAN=round(mean(datan[,2],0))

      for (t in 1:lm){
        yearmin=years[t]
        yearmax=years[t+1]

        min=paste(yearmax,"-", "08")
        min=sub(" - ", "-",min)
        max= paste(yearmax,"-", "10")
        max=sub(" - ", "-",max)
        start=min(grep(min, datan[,1]))
        end=max(grep(max, datan[,1]))

        h=datan[start:end, ]
        mean=round(mean(h[,2],2))
        MQ[t]=mean
        NMQ[t]=round(mean/MEAN,2)
      }


      hyears=years[1:lm]




      matrix= cbind(MQ, hyears, NMQ)



      list[[i]]=matrix

      names(list)=names(data)

    }


    return(list)





  }

}
