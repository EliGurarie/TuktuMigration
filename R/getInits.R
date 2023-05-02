#' Get Initial values for Hierarchical Migration Estimation
#' 
#' 
#' @export

getInits <- function(data, 
                     t_mean = 110, dt_mean = 20, 
                     mu1_mean = NULL, mu2_mean = NULL, 
                     chains = 4, plotme = TRUE, K = 1, 
                     ddays = 15){
  stan_data <- data$stan_data
  
  start.days <- with(stan_data, which(yday <= (min(yday) + ddays)))
  end.days <- with(stan_data, which(yday >= (max(yday) - ddays)))
  
  if(is.null(mu1_mean)) mu1_mean = c(median(stan_data$x[start.days]), 
                                     median(stan_data$y[start.days]))  
  if(is.null(mu2_mean)) mu2_mean = c(median(stan_data$x[end.days]), 
                                     median(stan_data$y[end.days]))  
  
  constrain <- function(x, min = -Inf, max = Inf) pmax(x, min) %>% pmin(max)
  
  inits.raw <- with(stan_data, 
                    list(mu1_mean = mu1_mean,
                    mu2_mean = mu2_mean,
                    mux1_sigma = sd(x), mux2_sigma = sd(x),
                    muy1_sigma = sd(y), muy2_sigma = sd(y),
                    t_mean = t_mean))
  
  makeInitsList <- function(a, data, inits.raw, K = K){
    
    xy.start <- ddply(as.data.frame(data), "id", head, 1)
    xy.end <- ddply(as.data.frame(data), "id", tail, 1)
    
    mux1.sd <- sd(xy.start$x)
    muy1.sd <- sd(xy.start$y)
    mux2.sd <- sd(xy.end$x)
    muy2.sd <- sd(xy.end$y)
    
    k <- length(unique(data$id))
    
    initlist <- with(data,
                     list(mu1_mean = (inits.raw$mu1_mean + rnorm(2, sd = mux1.sd)*K)  %>% 
                            cbind(c(max(x), max(y))) %>% apply(1, min) %>% 
                            cbind(c(min(x), min(y))) %>% apply(1, max) ,
                          mu2_mean = (inits.raw$mu2_mean + rnorm(2, sd = mux2.sd)*K) %>% 
                            cbind(c(max(x), max(y))) %>% apply(1, min) %>% 
                            cbind(c(min(x), min(y))) %>% apply(1, max),
                          mux1_sigma = rnorm(1,sqrt(var(x)), sd = sqrt(var(x)))^2*K,
                          mux2_sigma = rnorm(1,sqrt(var(x)), sd = sqrt(var(x)))^2*K,
                          muy1_sigma = rnorm(1,sqrt(var(y)), sd = sqrt(var(x)))^2*K,
                          muy2_sigma = rnorm(1,sqrt(var(y)), sd = sqrt(var(x)))^2*K,
                          t_mean = (t_mean + rnorm(1, sd = 3)*K) %>% constrain(min = min(yday), max = max(yday)), 
                          dt_mean = (dt_mean + rnorm(1, sd = 1)*K) %>% constrain(min = 0.1), 
                          t_sd = 10 + rnorm(1, sd = 1)*K %>% constrain(min = 0.1), 
                          dt_sd = (5 + rnorm(1, sd = 0.5)*K) %>% constrain(min = 0.1),
                          sigma_r = (10 + rnorm(1, sd = 2)*K) %>% constrain(min = 0.1), 
                          sigma_m = (20 + rnorm(1, sd = 3)*K) %>% constrain(min = 0.1), 
                          phi_r = 0.1, 
                          phi_m = .8))
    
    
    initlist$mu1 = daply(data %>% data.frame, "id", function(df) df[1,c("x", "y")]) %>% 
      as.numeric %>% matrix(ncol=2)
    initlist$mu2 = daply(data %>% data.frame, "id", function(df) df[nrow(df),c("x", "y")]) %>% 
      as.numeric %>% matrix(ncol=2) 
    
    initlist$t <- with(initlist, t_mean + rnorm(k, sd = 5)*K) %>% constrain(min = min(data$yday), max = max(data$yday))
    initlist$dt <- with(initlist, dt_mean + rnorm(k, sd = 1)*K) %>% constrain(min = 0.1, max = max(data$yday))
    return(initlist)
  }
  
  inits <- lapply(1:chains, makeInitsList, data = stan_data, inits.raw = inits.raw, K = K)
  
  if(plotme){
    layout(cbind(c(1,1), 2:3))
    
    with(stan_data, plot(x, y, pch = 19, col = rgb(0,0,0,.4), cex= 0.5, asp = 1))
    with(inits.raw, points(rbind(mu1_mean, mu2_mean), col = 2:3, cex = 2, lwd = 4, lty = 2))
    lapply(inits, function(l) with(l, points(rbind(mu1_mean, mu2_mean), col = 2:3, cex = 2, lwd = 2, pch = 4)))
    
    with(stan_data, plot(yday, x, pch = 19, col = rgb(0,0,0,.4), cex= 0.5))  
    with(inits.raw, abline(v = c(t_mean, t_mean+dt_mean), col=2:3, lwd = 2))
    lapply(inits, function(l) with(l, abline(v = c(t_mean, t_mean+dt_mean), col = 2:3, lty = 3)))
    
    with(stan_data, plot(yday, y, pch = 19, col = rgb(0,0,0,.4), cex= 0.5))  
    with(inits.raw, abline(v = c(t_mean, t_mean+dt_mean), col=2:3, lwd = 2))
    lapply(inits, function(l) with(l, abline(v = c(t_mean, t_mean+dt_mean), col = 2:3, lty = 3)))
    
    title(paste("Initial values for", chains, "chains"), outer = TRUE, line = -1)
  }
  
  return(inits)
}
