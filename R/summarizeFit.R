#' Summarize hierarchical migration fit
#' 
#' @param fit output of \link{fitHierarchicalMigration}
#' 
#' @return A list including estimates of migration times (start and duration) and standard deviations, 
#' centroid of respective starting and ending ranges, and estimated ellipses of the ranging areas before
#' and after the migration
#' 
#' @export

summarizeMigrationFit <- function(fit){
  
  data_raw <- fit$raw_data
  data_input <- fit$stan_data
  migration_fit <- fit$migration_fit
  
  # make data into simple feature lines
  
  data_lines <- with(data_input, 
       data.frame(id = factor(id), x = x * 1e3, y = y * 1e3) %>% 
         st_as_sf(coords = c("x","y"), 
                  crs = st_crs(data_raw))) %>% group_by(id)  %>% 
    summarize(do_union = FALSE) %>% st_cast("LINESTRING")
  
  # diagnose chains for each year
  
  chains <- rstan::As.mcmc.list(migration_fit)
  phats <- summary(chains)$quantiles[,"50%"]
  
  Mus <- phats[grep("mean", names(phats))]
  sds <- phats[grep("sd", names(phats))]
  sigmas <- phats[grepl("sigma", names(phats)) & grepl("mu", names(phats))] %>% as.list
  rhos <-  phats[grepl("rho", names(phats))] %>% as.list
  
  Mu1 <- Mus[c(1,3)]
  Mu2 <- Mus[c(2,4)]
  
  sx1 <- sigmas$mux1_sigma
  sy1 <- sigmas$muy1_sigma
  sx2 <- sigmas$mux2_sigma
  sy2 <- sigmas$muy2_sigma
  
  S1 <- matrix(c(sx1^2, sx1*sy1*rhos$rho1, sx1*sy1*rhos$rho1, sy1^2), nrow = 2)
  S2 <- matrix(c(sx2^2, sx2*sy2*rhos$rho2, sx2*sy2*rhos$rho2, sy2^2), nrow = 2)
  
  xy.results <- cbind(Mus[c("mux_mean[1]", "mux_mean[2]")], Mus[c("muy_mean[1]", "muy_mean[2]")])
  
  centroids.sf <- st_as_sf((xy.results * 1e3) %>% data.frame, coords = c("X1","X2"),
                           crs = st_crs(data_raw)) %>% st_transform(4326)
  
  makeSF.polygon <- function(xy){
    (xy * 1e3) %>% data.frame %>% 
      st_as_sf(coords = c("X1","X2"), crs = st_crs(data_raw)) %>% 
      st_transform(4326) %>% summarize(do_union = FALSE) %>% 
      st_cast("POLYGON")
  }
  
  Areas <- data.frame(area = c("Area1.95", "Area2.95", "Area1.50", "Area2.50"), 
    rbind(
    Area1.95 = mixtools::ellipse(Mu1, S1, alpha = 0.05, draw = FALSE, npoints = 1000) %>% makeSF.polygon,
    Area2.95 = mixtools::ellipse(Mu2, S2, alpha = 0.05, draw = FALSE, npoints = 1000) %>% makeSF.polygon,
    Area1.50 = mixtools::ellipse(Mu1, S1, alpha = 0.5, draw = FALSE, npoints = 1000) %>% makeSF.polygon,
    Area2.50 = mixtools::ellipse(Mu2, S2, alpha = 0.5, draw = FALSE, npoints = 1000) %>% makeSF.polygon)
  ) %>% st_as_sf
  
  # xysigma
  
  quantiles <- summary(chains)$quantiles
  phats <- t(cbind(estimate = quantiles[,"50%"], CI.low = quantiles[,"2.5%"], 
                      CI.high = quantiles[,"97.5%"])) %>% data.frame
  
  T.hats <- phats[,c("t_mean", "dt_mean", "t_sd", "dt_sd")] %>% mutate(
    departure = t_mean, 
    departure.sd  = t_sd, 
    arrival = t_mean + dt_mean, 
    arrival.sd = (t_sd + dt_sd)/2, 
    duration = dt_mean,
    duration.sd = dt_sd, 
    t_mean = NULL, dt_mean = NULL, t_sd = NULL, dt_sd = NULL) %>% t

  return(list(time_estimates = T.hats, 
              centroids.sf = centroids.sf,
              areas.sf = Areas,
              lines.sf = data_lines))
}

