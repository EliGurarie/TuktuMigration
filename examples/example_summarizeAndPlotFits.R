# fit (or load) hierarchical migration from simulated data
require(TuktuMigration)

eval <- FALSE; if(eval){
  sims_fit <- fitHierarchicalMigration(sims_prepped, initial_values = inits, 
                                       iter = 1000, cores = 4)
} else data("sims_fit")

fit <- sims_fit

# obtain summaries
s <- summarizeMigrationFit(fit)

# time estimates
s$time_estimates

# ranging area summaries
s$areas.sf %>% st_area/1e6

# plotting the summary fits

plotHierarchicalMigration(fit, type = "fit")
plotHierarchicalMigration(fit, type = "map")

# assessing the MCMC convergence

plotHierarchicalMigration(fit, type = "density")
plotHierarchicalMigration(fit, type = "chains")
