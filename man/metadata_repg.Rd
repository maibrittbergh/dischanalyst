% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata_repg.R
\name{metadata_repg}
\alias{metadata_repg}
\title{Representative Stations in Germany}
\usage{
metadata_repg(metadata, mark = F)
}
\arguments{
\item{metadata}{"matrix" "array" ;  Metadata of grdc_dataset. Can be created by \link[dischanalyst]{metadata_grdc} function.}

\item{mark}{logical; default=F; if FALSE: Function returns new, metadata containing representative stations. if TRUE: function returns same metadata (of GRDC-Germany Dataset) and adds column that identifies if station is representative.}
}
\value{
"matrix". Same Arrangement like metadata. Contains representative stations or marks representative stations in original/input metadata.
}
\description{
Function is searching in input-metadata for representative stations. Depending on the settings, if mark=T it returns the original metadata and adds a column containing logical values. TRUE if station is representative, FALSE if its not. Or, if mark=F it returns a metadata-data.frame only containing the representative stations.
}
\examples{
\dontrun{metadata_repg(metadata_germany)
metadata_germany=metadata_repg(metadata_germany, mark=T)}


}
