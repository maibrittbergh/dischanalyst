

#' GRDCC River Dataset Reader.
#'
#'
#'
#' @param data Dataframe;  a Dataframe including columns: "river" (rivername as character), "station" (rivername as character) and "grdc_no" (grdc number as double)
#' @param rivername character; name of the demanded river. Must equal name in column "river" in the Dataframe
#' @param path character; Pathway of your computer where the GRDC dataset is saved.
#'
#' @return  dataset of the demanded river
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mosel=grdc_readr(data,"MOSELLE RIVER","/Users/username/Desktop/folderone/datafolder/grdc_03_2021/grdc_disc/" )
#' }
#'
#'
#'
#'
#'
#'
grdc_readr=function(data, rivername, path ){
  data_fluss= data[which(data$river==rivername),]
  grdc_no=data_fluss$grdc_no
  l=length(grdc_no)
  read=c(1:l)
  for (i in 1:l){

    grdc_numb=grdc_no[i]
    grdc_numb=as.character(grdc_numb)
    read[i]=paste(path,grdc_numb,"_Q_Day.Cmd.txt")
  }
  read=sub(" ","", read)
  read=sub(" _","_", read)
  name= vector(mode = "list", length = l)
  for (i in 1:l){
    Tabelle=read.table(read[i], header=T, sep=";", dec=".", na.strings = c())[-2] #-999 als NA Value
    Tabelle$YYYY.MM.DD=as.Date(Tabelle$YYYY.MM.DD)
    Tabelle$Value[(which(Tabelle$Value<0))] = 0
    name[[i]]=Tabelle #hours,minutes rausgeschmissen
  }

  for (i in 1:l){
    names(name)[[i]]= data_fluss$station[i]
  }

  return(name)
}
