

#' GRDC River Dataset Reader.
#'
#'@decription Function to read in data.frames of all stations at one river. Returns list. Each list entry corresponds to one measuring station of the river.
#'
#' @param metadata metadata "matrix" "array" ;  Metadata of grdc dataset. Can be created by \link[dischanalyst]{metadata_grdc} function.
#' @param rivername character; Name of the demanded river. Must equal name in column "river" of metadataset.
#' @param path character; Pathway directing to local folder containing the GRDC-Dataset.
#'
#' @return  Function returns list. Each entry contains discharge measurements as well as the corresponding dates of the measurements at a specific station of the given river.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' mosel=grdc_readr(metadata,"MOSELLE RIVER","/Users/username/Desktop/folderone/datafolder/grdc_03_2021/grdc_disc/" )
#' }
#'
#'
#'
#'
#'
#'
#'
#'

grdc_readr=function(metadata, rivername, path ){
  data_fluss= metadata[which(metadata$river==rivername),]
  grdc_no=data_fluss$grdc_no
  l=length(grdc_no)
  read=c(1:l)
  for (i in 1:l){

    grdc_numb=grdc_no[i]
    grdc_numb=as.character(grdc_numb)
    read[i]=paste(path,"/",grdc_numb,"_Q_Day.Cmd.txt")
  }
  read=sub(" ","", read)
  read=sub(" _","_", read)
  read=sub("/ ", "/", read)
  name= vector(mode = "list", length = l)
  ?read.table
  for (i in 1:l){
    Tabelle=read.table(read[i], header=T, sep=";", dec=".", na.strings = "NA")[-2]#-999 als NA Value
    Tabelle$YYYY.MM.DD=as.Date(Tabelle$YYYY.MM.DD)
    Tabelle$Value[(which(Tabelle$Value<0))] = NA
    name[[i]]=Tabelle #hours,minutes rausgeschmissen
  }

  for (i in 1:l){
    names(name)[[i]]= data_fluss$station[i]
  }

  return(name)
}


