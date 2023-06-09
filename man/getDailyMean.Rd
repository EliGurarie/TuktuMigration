% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDailyMean.R
\name{getDailyMean}
\alias{getDailyMean}
\title{Calculate Daily Mean}
\usage{
getDailyMean(x, idcol = "ID", timecol = "Time", ...)
}
\arguments{
\item{x}{a data frame (or simple feature) with the ID, the Time (as date and time), the x and y coordinates in metric system, 
and the Lon and Lat coordinates in WGS84}

\item{idcol}{character string of the name of the ID column}

\item{timecol}{character string of the name of the time column}
}
\value{
A data frame (or simple feature) with the daily mean locations
}
\description{
Calculate Daily Mean
}
\examples{
require(TuktuTools)

data(caribou)

caribou_dailymean <- getDailyMean(caribou)
head(caribou_dailymean)
}
