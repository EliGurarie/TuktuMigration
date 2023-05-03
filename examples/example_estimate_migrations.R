require(TuktuTools)
require(TuktuMigration)

# Initialize data -----

data("simulated_migrations")

simulated_migrations.sf <- simulated_migrations %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>%
  st_transform(3979)

sims_dailymean <- getDailyMean(simulated_migrations.sf)
scan_tracks(sims_dailymean, time.col = "yday", legend = FALSE, 
            col = rainbow(18))

sims_prepped <- prepData_migration(sims_dailymean)
inits <- getInits(sims_prepped, t_mean = 120, dt_mean = 20)
sims_fit <- fitHierarchicalMigration(sims_prepped, initial_values = inits, 
                                     iter = 1000, cores = 4)

plotHierarchicalMigration(sims_fit, type = "fit")
plotHierarchicalMigration(sims_fit, type = "density")
plotHierarchicalMigration(sims_fit, type = "chains")
plotHierarchicalMigration(sims_fit, type = "map")
