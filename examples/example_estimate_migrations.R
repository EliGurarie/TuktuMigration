rm(list=ls())
require(TuktuTools)
require(TuktuMigration)
require(rstan)

# Initialize data -----

data("simulated_migrations")

simulated_migrations.sf <- simulated_migrations %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>%
  st_transform(3979)

#source("../TuktuTools/R/getDailyMean.R")

sims_dailymean <- getDailyMean(simulated_migrations.sf)
scan_tracks(sims_dailymean, time.col = "yday", legend = FALSE, 
            col = rainbow(18))


#source("R/getInits.R")
#source("R/prepData_migration.R")


# Load Rstan -----

#require(rstan)
eval <- FALSE; if(eval){
  estimateMigrations <- stan_model("inst/stan/estimateMigration.stan")
  save(estimateMigrations, file = "sandbox/estimateMigrations.rda")
} else load("sandbox/estimateMigrations.rda")


sims_prepped <- prepData_migration(sims_dailymean)
inits <- getInits(sims_prepped, t_mean = 120, dt_mean = 20)
sims_fit <- fitHierarchicalMigration(sims_prepped, initial_values = inits, 
                                     iter = 1000, cores = 4)

source("R/mapFits.R")
source("R/summarizeFit.R")

plotHierarchicalMigration(sims_fit, type = "fit")
plotHierarchicalMigration(sims_fit, type = "density")
plotHierarchicalMigration(sims_fit, type = "chains")
plotHierarchicalMigration(sims_fit, type = "map")
