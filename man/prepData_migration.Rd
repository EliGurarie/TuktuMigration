% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepData_migration.R
\name{prepData_migration}
\alias{prepData_migration}
\title{Prep data for Hierarchical Migration analysis}
\usage{
prepData_migration(
  data,
  id.col = "ID",
  x.col = NULL,
  y.col = NULL,
  time.col = "Time",
  yday.col = NULL
)
}
\arguments{
\item{data}{movement data to analyze, a simple feature in a metric coordinate system 
(i.e. not longitude-latitude), or a data frame with X and Y columns in meters}

\item{id.col}{name of column with indiviudal id's}

\item{time.col}{column name of time column.  Must be POSIX}

\item{yday.col}{if time.col is NULL, the column that contains the day of year. 
Eventually, this can be fractional day of year}

\item{{x.col, y.col}}{if a data frame, column names for X and Y coordinates}
}
\value{
outputs a list with two elements: \code{raw_data} is just the input data in its 
original form, \code{stan_data} is the data processed and reasy to be analyzed in the 
hierarchical migration analysis.
}
\description{
Simplifies a simple feature with an ID and time column into a data frame suitable for 
hierarchical migration analysis
}
