% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitHierarchicalMigration_stan.R
\name{fitHierarchicalMigration}
\alias{fitHierarchicalMigration}
\title{Hierarchical estimation of migration timing}
\usage{
fitHierarchicalMigration(
  data_prepped,
  initial_values = NULL,
  chains = 4,
  iter = 1000,
  ...
)
}
\arguments{
\item{data_prepped}{Data prepped for hierarchical migration analysis - strictly output of 
\code{\link{prepData_migration}}.}

\item{initial_values}{Initial seed values for MCMC as returned by \code\link{}}

\item{...}{}
}
\description{
See example for details.
}
\examples{
require(TuktuTools)
require(TuktuMigration)

#  Load, view, and initialize data -----

data("simulated_migrations")

simulated_migrations.sf <- simulated_migrations \%>\% 
  st_as_sf(coords = c("X","Y"), crs = 4326) \%>\%
  st_transform(3979)

sims_dailymean <- getDailyMean(simulated_migrations.sf)
scan_tracks(sims_dailymean, time.col = "yday", legend = FALSE, 
            col = rainbow(18))

sims_prepped <- prepData_migration(sims_dailymean)
inits <- getInitialValues(sims_prepped, t_mean = 120, dt_mean = 20)

## This runs the sampling - which can taks some time.
## Otherwise just load the output of the fit

eval <- FALSE; if(eval){
  sims_fit <- fitHierarchicalMigration(sims_prepped, initial_values = inits, 
                                       iter = 1000, cores = 4)
} else data("sims_fit")

plotHierarchicalMigration(sims_fit, type = "fit")
plotHierarchicalMigration(sims_fit, type = "density")
plotHierarchicalMigration(sims_fit, type = "chains")
plotHierarchicalMigration(sims_fit, type = "map")
}
