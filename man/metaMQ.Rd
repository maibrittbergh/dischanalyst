% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metaMQ.R
\name{metaMQ}
\alias{metaMQ}
\title{Area-wide MQ-Trend}
\usage{
metaMQ(Startyear, Endyear, metadata, data)
}
\arguments{
\item{Startyear}{numeric; Startyear of timerange.}

\item{Endyear}{numeric; Endyear of timerange.}

\item{metadata}{data.frame; Overview of GRDC-Dataset.  The metadata can be created by \link[dischanalyst]{metadata_grdc} function}

\item{data}{list; Contains all stations that the discharge analysis should consider. List can be created by \link[dischanalyst]{grdc_list}. Each entry of the list contains the existing discharge measurements (as numeric) and the corresponding dates (as character) for the station.}
}
\value{
dataframe; metadata of stations whose measurement series are at least as long as the given time frame. The Dataframe includes different approaches to calculate the trend of the annual Normalized MQ (within the whole Year/the Spring/the Summer/the Autumn/the Winter)
\describe{
  \item{intercept_zyp}{intercept created by \link[zyp]{zyp.trend.vector}}
  \item{slope_zyp}{slope created by \link[zyp]{zyp.trend.vector}}
  \item{sig_zyp}{significance (Kendall's P-Value) for the final detrended time-series}
  \item{intercept_ls}{intercept created by \link[stats]{lm}}
  \item{slope_ls}{slope created by \link[stats]{lm}}
}
}
\description{
Function creates dataframe. Stations in metadataset are filtered by the given timeframe. To guarantee comparability between stations the measurement series of the stations are adapted to the time frame and shortened to the of the time frame.
}
\examples{
\dontrun{
MQtf1820_2019=dataset(1820, 2019, metadata, data)
}




}
