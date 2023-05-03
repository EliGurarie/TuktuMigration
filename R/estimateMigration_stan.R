#' Hierarchical estimation of migration timing
#' 
#' 
#' 
#' @export
#' 
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

