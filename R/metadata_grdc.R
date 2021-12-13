
#' Create Metadata for GRDC Dataset
#'
#' @param Country character; abbrevation used in GRDC Dataset for specific country. e.g. "DE" for Germany.
#' @param path character; pathway to grdc_discharge folder on computer
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' metadata_germany=metadata_grdc("DE,"/Users/username/Desktop/folderone/datafolder/grdc_03_2021/grdc_disc/" )
#' }
#'
metadata_grdc=function(Country, path){
  files=list.files(path)
  l=length(files)
  vec=as.logical(rep(0,l))




  for (i in 1:l){
    g=c(path,"/" ,files[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=150)
    s
    p=grep("Country", s)

    vec[i]=identical(s[p+1],  Country)
  }

  COUNTRY=which(vec==TRUE)
  file_Country=files[COUNTRY]

  # GRDC_NUMBER


  no_length=length(file_Country)
  grdc_no=rep(0, no_length)
  for (i in 1:no_length){
    grdc_no[i]= substring(grdc_nom[i], 1,7)
  }
  grdc_no=as.numeric(grdc_no)




  #river
  river=rep(0, no_length)
  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=80)
    p=grep("River", s)

    river[i]=paste(s[(p+1)], s[p+2] )

  }

  river=sub("#","", river)
  river

  #station

  station=rep(0, no_length)
  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=80)
    p=grep("Station", s)

    station[i]=paste(s[(p+1)], s[p+2] )

  }

  station=sub("#","", station)

  station

  #country


  country=rep(0, no_length)
  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=80)
    p=grep("Country", s)

    country[i]=s[(p+1)]

  }

  #catchment area

  catch_area=rep(0, no_length)

  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=80)
    p=grep("area", s)

    catch_area[i]=as.numeric(s[(p+2)])

  }


  catch_area

  #altitude

  altitude=rep(0, no_length)

  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=80)
    p=grep("Altitude", s)

    altitude[i]=as.numeric(s[(p+3)])

  }
  altitude
  # startday

  startday=rep(0, no_length)

  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=120)
    p=grep("Time", s)

    startday[i]=s[(p+2)]

  }
  startday

  #startyear

  startyear=rep(0, no_length)
  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=120)
    p=grep("Time", s)

    startyear[i]=as.numeric(substring(s[p+2],1,4))

  }
  startyear

  #endday
  endday=rep(0, no_length)

  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=120)
    p=grep("Time", s)

    endday[i]=s[(p+4)]

  }

  endday

  #endyear

  endyear=rep(0, no_length)
  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=120)
    p=grep("Time", s)

    endyear[i]=as.numeric(substring(s[p+4],1,4))

  }

  endyear



  #time_series

  d_years=rep(0, no_length)
  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=120)
    p=grep("years", s)

    d_years[i]=as.numeric(s[p+1])

  }

  d_years


  #Longitude
  longitude=rep(0, no_length)
  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=60)
    p=grep("Longitude", s)

    longitude[i]=as.numeric(s[p+2])

  }

  longitude
  #Latitude
  latitude=rep(0, no_length)
  for (i in 1:no_length){
    g=c(path,"/" ,file_Country[i])
    h=paste(g, collapse=" ")
    h=sub(" / ", "/",h)
    s=scan(h, what="character", nmax=60)
    p=grep("Latitude", s)

    latitude[i]=as.numeric(s[p+2])

  }
  latitude
  #Create Data.Frame
  metadata=cbind(grdc_no, river, station, country, catch_area, altitude, startday, endday, startyear,endyear, d_years, longitude, latitude)
  return(metadata)



}
