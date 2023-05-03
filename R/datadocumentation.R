#' Simulated migrations of 18 caribou
#'
#' Simulated migration data provided to illustrate some of the features of the 
#' TuktuTools and TuktuMigration package.  Two objects are loaded: 
#' \describe{
#' \item{\code{simulated_migrations} is a data frame with Lat-Long coordinates.}
#' \item{\code{simulated_migrations.sf} is a georeferenced simple feature object.} 
#'}
#'
#' @details These are simulated data of 18 caribou over the period of a 
#' spring migration in 2014, with data from March 31 through June 24.  The 
#' simulations were generated using statistical summaries of spring migration 
#' of Bathurst caribou in 2014 reported by Gurarie et al. (2019).  The tracks 
#' were generated via some modifications of the \code{\link[marcher]{simulate_shift}} 
#' function in the \code{marcher} packages. 
#' 
#' @usage 
#' data(simulated_migrations)
#' 
#' @name simulated_migrations
#' 
#' @format data frame with the following columns: 
#' #' \describe{
#'   \item{ID}{Numberic ID of animal}
#'   \item{Name}{Name of animal}
#'   \item{Time}{Date and time of each GPS location (POSIX)}
#'   \item{Lon,Lat}{Coordinates locations in WGS84}
#'   \item{geometry}{simple feature geometry}}
#' 
#' @example examples/example_simulated_migrations.R
#'
#' @source Gurarie et al. (2019) Tactical departures and strategic arrivals: Divergent effects of climate and weather on caribou spring migrations. Ecosphere 10.12 (2019): e02971.
#' @source Gurarie & Cheraghi (2017). marcher: Migration and Range Change Estimation in R. R package version 0.0-2,
#' @keywords data
"simulated_migrations"


#' Estimated migrations for simulated data
#'
#' Output of \code{\link{fitHierarchicalMigration}} function as applied to simulated migration data
#' \code{\link{simulated_migrations}}. 
#'
#' @details This is a list of three: \code{stan_data} - the data inputted into the STAN sampler, 
#' \code{raw_data} - the raw simple feature data, and \code{migration_fit}, the complete STAN sampler
#' ouput - i.e. all the MCMC chains and summary stats. 
#' 
#' @usage 
#' data(sims_fit)
#' 
#' @name sims_fit
#' @keywords data
"sims_fit"
