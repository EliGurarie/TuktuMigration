#' @export

mapFits <- function(myfit){
  #require(coda)
  #require(mixtools)
  #require(gplots)
  #require(lattice)
  
  migration.fit <- myfit$migration.fit
  my.data <- myfit$my.data
  my.df <- myfit$my.df
  ll <- my.df %>% st_transform(4326) %>% st_coordinates
  xy <- (my.df %>% st_coordinates)
  my.df <- my.df %>% mutate(lat = ll[,2], lon = ll[,1], 
                            x = xy[,1]/1e3, y = xy[,2]/1e3) %>% st_as_sf
  
  myyear <- my.df$year[1]
  
  # diagnose chains for each year
  
  phats <- summary(rstan::As.mcmc.list(migration.fit))$quantiles[,"50%"]
  
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
    
  S1 <- matrix(c(sx1, sqrt(sx1*sy1)*rhos$rho1, sqrt(sx1*sy1)*rhos$rho1, sy1), nrow = 2)
  S2 <- matrix(c(sx2, sqrt(sx2*sy2)*rhos$rho2, sqrt(sx2*sy2)*rhos$rho2, sy2), nrow = 2)
  
  Area1.95 <- ellipse(Mu1, S1, alpha = 0.05, draw = FALSE)
  Area2.95 <- ellipse(Mu2, S2, alpha = 0.05, draw = FALSE)
  Area1.50 <- ellipse(Mu1, S1, alpha = 0.5, draw = FALSE)
  Area2.50 <- ellipse(Mu2, S2, alpha = 0.5, draw = FALSE)
  
  mux1s <- phats[grepl("mux1", names(phats))]
  muy1s <- phats[grepl("muy1", names(phats))]
  mux2s <- phats[grepl("mux2", names(phats))]
  muy2s <- phats[grepl("muy2", names(phats))]
  
  # convert all of these to lat-long
  
  ll.lm <- lm(cbind(lon, lat) ~ poly(x,2)*poly(y,2), data = my.df) 
  
  xy.results <- list(Mus = rbind(Mu1, Mu2), 
                     Area1.95 = Area1.95, Area2.95 = Area2.95, 
                     Area1.50 = Area1.50, Area2.50 = Area2.50)
  
  ll.results <- lapply(xy.results, function(xy) 
    predict(ll.lm, newdata = data.frame(x = xy[,1], y = xy[,2]))) %>% lapply(as.data.frame)	
  
  Areas.sf <- ldply(ll.results, function(xy){
    if(nrow(xy) > 2)
      poly <- st_as_sf(xy, coords = c("lon","lat"), crs = 4326) %>% summarize(do_union = FALSE) %>% 
    st_cast("POLYGON") else poly <- NULL
    return(poly)
  }) %>% st_as_sf
  
  #mapview::mapview(Areas.sf, zcol = ".id")

  p.latlon <-	 
      ggplot(my.df, aes(x = lon, y = lat, col=ID)) + 
      coord_map() + geom_path() + 
      labs(x = "Longitude", y = "Latitude") + 
      scale_color_discrete(l = 50) + guides(col = FALSE) + 
    geom_polygon(data = ll.results$Area1.95, aes(x = lon, y = lat), fill = alpha("blue", .2), col="blue") + 
    geom_polygon(data = ll.results$Area1.50, aes(x = lon, y = lat), fill = alpha("blue", .2), col="blue") + 
    geom_polygon(data = ll.results$Area2.95, aes(x = lon, y = lat), fill = alpha("green", .2), col="green") + 
    geom_polygon(data = ll.results$Area2.50, aes(x = lon, y = lat), fill = alpha("green", .2), col="green") 
  
  model.fit = data.frame(
    yday.model = with(my.data, c(min(yday), phats["t_mean"] + c(0, phats["dt_mean"]), max(yday))),
    lat.model = rep(ll.results$Mu[,2], each = 2),
    lon.model = rep(ll.results$Mu[,1], each = 2))
  

  p.tlon <- ggplot(my.df, aes(yday, lon, col = ID)) + 
    geom_path(alpha = 0.5) + geom_point(alpha = 0.5, size = 0.5) + 
    ggthemes::theme_few() + theme(legend.position="none") + scale_colour_hue(l = 40) + 
    geom_path(data = model.fit, aes(yday.model, lon.model), col="darkred", alpha = 0.5,  lwd=2) + 
    xlab("Day of year") + ylab("Longitude")
  
  p.tlat <- ggplot(my.df, aes(yday, lat, col= ID)) + 
    geom_path(alpha = 0.5) + geom_point(alpha = 0.5, size = 0.5) + 
    ggthemes::theme_few() + theme(legend.position="none") + scale_colour_hue(l = 40) + 
    geom_path(data = model.fit, aes(yday.model, lat.model), col="darkred", alpha = 0.5,  lwd=2) + 
    xlab("Day of year") + ylab("Latitude")
  
  require(gridExtra)
  gridExtra::grid.arrange(p.latlon, p.tlat, p.tlon, layout_matrix = cbind(c(1,1), c(1,1),c(1,1), 2:3, 2:3))
}		
