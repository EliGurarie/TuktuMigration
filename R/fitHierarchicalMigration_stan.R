#' Hierarchical estimation of migration timing
#' 
#' See example for details.  
#' 
#' @param data_prepped Data prepped for hierarchical migration analysis - strictly output of 
#' \code{\link{prepData_migration}}.  
#' @param initial_values Initial seed values for MCMC as returned by \code\link{}
#' @param ... 
#' 
#' @export
#' @example examples/example_estimate_migrations.R
#' 

fitHierarchicalMigration <- function(data_prepped, 
                                     initial_values = NULL, 
                                     chains = 4, 
                                     iter = 1000, ...){
  
  migration_fit <- try(rstan::sampling(stanmodels$hierarchicalMigration, 
                                data = data_prepped$stan_data, 
                                init = initial_values, 
                                iter = iter, 
                                chains = chains, ...))
  
  myresults <- list(stan_data = data_prepped$stan_data, 
                    raw_data = data_prepped$raw_data, 
                    migration_fit = migration_fit)
  return(myresults)
}

