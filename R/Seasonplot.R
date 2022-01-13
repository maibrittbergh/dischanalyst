
#' Seasonplot
#'
#' @description function returns plot of a specific time frame within one year.
#' By defining a Startyear and an Endyear it enables you to compare a few years (at least 4).
#'
#' @param data list; River from GRDC - Dataset. Output of \link[dischanalyst]{grdc_readr}. Type: list; list entries: measurement stations. For every Station: Date since begin of Measurements (as character) and Value (as numeric).
#' @param station character- must equal name of station in data.
#' @param month_start character- which month of the year. E.g. Febuary= "02"
#' @param month_end character- which month of the year. E.g. Febuary= "02"
#' @param hyear to gain on overview of the whole year.
#' @param Startyear numeric.
#' @param Endyear numeric. Endyear-Startyear>=4.
#'
#' @return plot of Seasons
#' @export
#'
#'@import ggplot2
#'@import gridExtra
#' @examples
#' \dontrun{
#'seasonplot(mosel, "COCHEM", hyear=T, Startyear=2010,Endyear= 2018  )
#' }
seasonplot= function(data, station, month_start, month_end, hyear=F, Startyear, Endyear ){
n= Endyear-Startyear
if (n<4){return(paste("Please choose a timespan of at least four years.Condition: Endyear-Startyear>=4"))}
  if(hyear==F){

    nbr=which(names(data)==station)
    d=data[[nbr]]
    r=nrow(d)
    firstyear=d[1,1]
    lastyear=d[r,1]
    firstyear=as.numeric(substr(firstyear,1,4))+1
    lastyear=as.numeric(substr(lastyear,1,4))-1

    years=firstyear:lastyear

    ly=length(years)
    begin=rep("." , ly)
    end=rep("." , ly)
    for ( i in 1:ly){
      o=sub(" ","",paste(as.character(years[i]),"-", month_start, "-01"))
      t=sub("- ", "-", o)
      f=sub(" -", "-", t)
      begin[i]=f
      u=sub(" ","",paste(as.character(years[i]),"-", month_end, "-28"))
      h=sub("- ", "-", u)
      j=sub(" -", "-", h)
      end[i]=j
    }



    Value=rep(0,r)
    Year=rep(0,r)
    mean=rep(0,r)


    for ( i in 1: ly){

      start=which(d[,1]==begin[i])
      last=which(d[,1]==end[i])
      data=d[start:last,2]
      Value[start:last]=d[start:last,2]
      mean[start:last]=mean(Value)
      Year[start:last]=years[i]


    }


    offseason=which(Value==0)     #Define Offseason
    Value=Value[-offseason]
    Year=Year[-offseason]
    month=month[-offseason]

    mean=mean[-offseason]










    Y=length(Year)

    y=length(years)
    days=rep(0,Y)
    for ( i in 1: y){
      d=which(Year==years[i])
      l=length(d)
      e=1:l
      days[d]=e
    }



    matrix=data.frame(cbind(Value,Year, days))
    Start=min(which(matrix$Year==Startyear))

    End=max(which(matrix$Year==Endyear))
    matrix=matrix[Start:End,]

    title=paste("Seasonal Discharge Analysis from",firstyear,"to",lastyear, "at", station )
    subtitl=paste("The season is from:01.",month_start,"to 01.", month_end)
    graph= ggplot(matrix, aes(x=days, y=Value, group=Year, color=Year))+geom_line()+ scale_colour_gradient(low="orange", high="blue")#scale_x_date(date_labels=("%m-%d")) +
    theme(legend.position="right", legend.box = "vertical")+ylab("Discharge Value")+xlab("Time [Days]")+
      labs(title=title)
    print(graph)

  }else{



    nbr=which(names(data)==station)
    d=data[[nbr]]
    r=nrow(d)
    firstyear=d[1,1]
    lastyear=d[r,1]
    firstyear=as.numeric(substr(firstyear,1,4))+1
    lastyear=as.numeric(substr(lastyear,1,4))-1

    years=firstyear:lastyear

    #11,12,01, e 02,03,04, z 05,06,07, d 08, 09, 10v
    ly=length(years)
    begin_e=rep("." , ly)
    end_e=rep("." , ly)
    begin_z=rep("." , ly)
    end_z=rep("." , ly)
    begin_d=rep("." , ly)
    end_d=rep("." , ly)
    begin_v=rep("." , ly)
    end_v=rep("." , ly)
    for ( i in 1:ly){
      o=sub(" ","",paste(as.character(years[i]),"-", "10", "-01"))
      t=sub("- ", "-", o)
      f=sub(" -", "-", t)
      begin_e[i]=f
      u=sub(" ","",paste(as.character(years[i]),"-", "12", "-31"))
      h=sub("- ", "-", u)
      j=sub(" -", "-", h)
      end_e[i]=j




      o=sub(" ","",paste(as.character(years[i]),"-", "01", "-01"))
      t=sub("- ", "-", o)
      f=sub(" -", "-", t)
      begin_z[i]=f
      u=sub(" ","",paste(as.character(years[i]),"-", "04", "-31"))
      h=sub("- ", "-", u)
      j=sub(" -", "-", h)
      end_z[i]=j
      o=sub(" ","",paste(as.character(years[i]),"-", "05", "-01"))
      t=sub("- ", "-", o)
      f=sub(" -", "-", t)
      begin_d[i]=f
      u=sub(" ","",paste(as.character(years[i]),"-", "08", "-31"))
      h=sub("- ", "-", u)
      j=sub(" -", "-", h)
      end_d[i]=j
      o=sub(" ","",paste(as.character(years[i]),"-", "9", "-01"))
      t=sub("- ", "-", o)
      f=sub(" -", "-", t)
      begin_v[i]=f
      u=sub(" ","",paste(as.character(years[i]),"-", "12", "-32"))
      h=sub("- ", "-", u)
      j=sub(" -", "-", h)
      end_v[i]=j
    }





    eValue=rep(0,r)
    eYear=rep(0,r)

    zValue=rep(0,r)
    zYear=rep(0,r)

    dValue=rep(0,r)
    dYear=rep(0,r)

    vValue=rep(0,r)
    vYear=rep(0,r)
    for ( i in 1: ly){

      start=which(d[,1]==begin_e[i])
      last=which(d[,1]==end_e[i])
      data=d[start:last,2]
      eValue[start:last]=d[start:last,2]
      eYear[start:last]=years[i]

      start=which(d[,1]==begin_z[i])
      last=which(d[,1]==end_z[i])
      data=d[start:last,2]
      zValue[start:last]=d[start:last,2]
      zYear[start:last]=years[i]

      start=which(d[,1]==begin_d[i])
      last=which(d[,1]==end_d[i])
      data=d[start:last,2]
      dValue[start:last]=d[start:last,2]
      dYear[start:last]=years[i]

      start=which(d[,1]==begin_v[i])
      last=which(d[,1]==end_v[i])
      data=d[start:last,2]
      vValue[start:last]=d[start:last,2]
      vYear[start:last]=years[i]

    }





    eoffseason=which(eValue==0)
    zoffseason=which(zValue==0)
    doffseason=which(dValue==0)
    voffseason=which(vValue==0)


    #Define Offseason
    eValue=eValue[-eoffseason]
    eYear=eYear[-eoffseason]



    zValue=zValue[-zoffseason]
    zYear=zYear[-zoffseason]

    dValue=dValue[-doffseason]
    dYear=dYear[-doffseason]


    vValue=vValue[-voffseason]
    vYear=vYear[-voffseason]


    y=length(years)
    days=rep(0,Y)
    for ( i in 1: y){
      d=which(eYear==years[i])
      l=length(d)
      e=1:l
      days[d]=e
      edays=days
    }

    days=rep(0,Y)

    for ( i in 1: y){

      d=which(zYear==years[i])
      l=length(d)
      e=1:l
      days[d]=e
      zdays=days
    }



    days=rep(0,Y)
    for ( i in 1: y){
      d=which(dYear==years[i])
      l=length(d)
      e=1:l
      days[d]=e
      ddays=days
    }
    days=rep(0,Y)
    for ( i in 1: y){
      d=which(vYear==years[i])
      l=length(d)
      e=1:l
      days[d]=e
      vdays=days

    }






    ematrix=data.frame(cbind(eValue,eYear, edays))

    Start=min(which(ematrix$eYear==Startyear))

    End=max(which(ematrix$eYear==Endyear))

    ematrix=ematrix[Start:End,]


    zmatrix=data.frame(cbind(zValue,zYear, zdays))
    Start=min(which(zmatrix$zYear==Startyear))

    End=max(which(zmatrix$zYear==Endyear))
    zmatrix=zmatrix[Start:End,]




    dmatrix=data.frame(cbind(dValue,dYear, ddays))
    Start=min(which(dmatrix$dYear==Startyear))

    End=max(which(dmatrix$dYear==Endyear))
    dmatrix=dmatrix[Start:End,]

    vmatrix=data.frame(cbind(vValue,vYear, vdays))
    Start=min(which(vmatrix$vYear==Startyear))

    End=max(which(vmatrix$vYear==Endyear))
    vmatrix=vmatrix[Start:End,]




    #subtitl=paste("The season: 01.10-31.12")
    #egraph= ggplot(ematrix, aes(x=edays, y=eValue, group=eYear, color=eYear))+geom_path()+ scale_colour_gradient(low="orange", high="blue")+#scale_x_date(date_labels=("%m-%d")) +
      theme(legend.position="none", legend.box = "vertical")+ylab("Discharge Value")+xlab("Time [Days]")+
      labs(subtitle = subtitl)





    subtitl=paste("The season: 01.01-30.04")
    zgraph= ggplot(zmatrix, aes(x=zdays, y=zValue, group=zYear, color=zYear))+geom_line()+ scale_colour_gradient(low="orange", high="blue")+#scale_x_date(date_labels=("%m-%d")) +
      theme(legend.position="none", legend.box = "vertical")+ylab("Discharge Value")+xlab("Time [Days]")+
      labs(subtitle = subtitl)
    print(zgraph)


    subtitl=paste("The season: 01.05-31.08")
    dgraph= ggplot(dmatrix, aes(x=ddays, y=dValue, group=dYear, color=dYear))+geom_line()+ scale_colour_gradient(low="orange", high="blue")+#scale_x_date(date_labels=("%m-%d")) +
      theme(legend.position="none", legend.box = "vertical")+ylab("Discharge Value")+xlab("Time [Days]")+
      labs(subtitle = subtitl)
    print(dgraph)

    subtitl=paste("The season: 01.09-01.12")
    vgraph= ggplot(vmatrix, aes(x=vdays, y=vValue, group=vYear, color=vYear))+geom_line()+ scale_colour_gradient(low="orange", high="blue")+#scale_x_date(date_labels=("%m-%d")) +
      theme(legend.position="none", legend.box = "vertical",)+ylab("Discharge Value")+xlab("Time [Days]")+
      labs(subtitle = subtitl)
    print(dgraph)

    # Create plot with legend
    ggp1_legend <- ggplot(vmatrix, aes(x = vdays, y = vValue, col = vYear)) +
      geom_point() + scale_colour_gradient( low="orange", high="blue")+
      theme(legend.position = "bottom")+  labs(color="")

    extract_legend <- function(my_ggp) {
      step1 <- ggplot_gtable(ggplot_build(my_ggp))
      step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
      step3 <- step1$grobs[[step2]]
      return(step3)}


      shared_legend <- extract_legend(ggp1_legend)

      grid.arrange(arrangeGrob( zgraph, dgraph, vgraph, ncol=3),
                   shared_legend, nrow = 2, heights = c(10, 1), top=paste("Discharge Values from", Startyear, "to", Endyear, "in Seasons"))




  }

}





