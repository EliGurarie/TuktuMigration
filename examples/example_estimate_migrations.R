rm(list=ls())
require(TuktuTools)
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


source("R/getInits.R")
source("R/prepData_migration.R")


# Load Rstan -----

#require(rstan)
eval <- FALSE; if(eval){
  estimateMigrations <- stan_model("inst/stan/estimateMigration.stan")
  save(estimateMigrations, file = "sandbox/estimateMigrations.rda")
} else load("sandbox/estimateMigrations.rda")


sims_prepped <- prepData_migration(sims_dailymean)
inits <- getInits(sims_prepped, t_mean = 120, dt_mean = 20)
f <- fitSpringMigration(sims_prepped)



eval <- FALSE; if(eval){
  migration.fit <- sampling(estimateMigrations, 
                            data = sims_migration, 
                            init = inits, iter = 1000, 
                            chains = 4, cores = 4)
  
  myresults <- list(data_raw = sims.dailymean, 
                    data_input = sims_migration, 
                    migration_fit = migration.fit)
  save(myresults, file = "sandbox/myresults.rda")
} else load("sandbox/myresults.rda")


source("R/mapFits.R")
source("R/summarizeFit.R")
fit_summaries <- summarizeMigrationFit(myresults)

require(mapview)
with(fit_summaries, 
     mapview(areas.sf) + mapview(centroids.sf, color = "red") + 
       mapview(lines.sf, zcol = "id")
)
