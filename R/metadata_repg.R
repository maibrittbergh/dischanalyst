#' Representative levels in Germany
#'
#'@description this fucntion only contains representative levels for Germany. If
#'
#' @param metadata "matrix" "array" ;  metadata of grdc dataset. Can be created by \link[dischanalyst]{metadata_grdc} function
#' @param mark logical; default=F; if FALSE: function returns new, metadata containing representative stations. if TRUE: function returns same metadata (of GRDC-Germany Dataset) and adds column that identifies if station is representative.
#'
#' @return "matrix". Same Arrangement like metadata (input- parameter). Contains representative stations.
#' @export
#'
#' @examples
#'\dontrun{metadata_repg(metadata_germany)
#'metadata_germany=metadata_repg(metadata_germany, mark=T)}
#'
#'

metadata_repg=function(metadata, mark=F){

  relstat=c("HOHENSAATEN-FINOW", "DRESDEN", "MAGDEBURG-STROMBRUECKE",
            "RATHENOW UP", "CALBE-GRIZEHNE", "INTSCHEDE",  "HANN.-MUENDEN", "VLOTHO",
            "VERSEN-WEHRDURCHSTICH", "GREVEN", "MAXAU", "KAUB", "KOELN", "COCHEM", "WUERZBURG" , "ROCKENAU SKA", "ACHLEITEN", "BURGHAUSEN", "WASSERBURG", "LANDSBERG", "KEMPTEN")
if(mark==F){
 #relevante Stationen
  l=length(relstat)
  rows=rep(0,l)
  for ( i in 1:l){

    rows[i]=which(metadata$station==relstat[i])
  }



  metadata_repg=metadata[rows,]
  return(metadata_repg)}
  #(von Ost nach West):
  # Ratzdorf / Oder: nicht gefunden  gab es nicht, habe "HOHENSAATEN-FINOW" genommen , oder ist "EISENHUETTENSTADT" besser?
  #Nienburg / Saale  #Nienburg garb es nicht. Habe CALBE-GRIZEHNE genommen
  #Höxter; / Weser #höxter gab es nicht, habe "HANN.-MUENDEN", "VLOTHO" genommen
  #Lingen-Darme / Ems # gab es nicht habe "VERSEN-WEHRDURCHSTICH", "GREVEN" genommen

  #Neu Ulm, Achleiten/ Donau #neu ulm gibts nicht


  if(mark==T){

    l=length(relstat)
    rows=rep(0,l)
    for ( i in 1:l){

      rows[i]=which(metadata$station==relstat[i])
    }



    representative=rep(F,nrow(metadata))
    representative[rows]=T
    metadata$rep_stat=representative
    return(metadata)
  }



}







