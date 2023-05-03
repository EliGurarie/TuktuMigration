#' Plotting hierarchical migration fits
#'
#'
#' @param fit output of \link{fitHierarchicalMigration}
#' @param plotfit whether to plot the fitted range ellipses and time series 
#' @param plotdensity whether to plot the posterior densities for the chains
#' @param plotchains whether to plot the MCMC chains
#'
#' @export
#' 

plotHierarchicalMigration <- function(fit, 
                                      pars = c("mux_mean[1]", "mux1_sigma", "muy_mean[1]", "muy1_sigma", "rho1",
                                               "mux_mean[2]", "mux2_sigma", "muy_mean[2]", "muy2_sigma", "rho2",
                                               "t_mean", "t_sd", "dt_mean", "dt_sd", "A", 
                                               "sigma_ranging", "sigma_migration"), 
                                      type= c("fit","density","chains","map")){
  
  migration_fit <- fit$migration_fit
  stan_data <- fit$stan_data
  
  phats <- summary(migration_fit)$summary[,"50%"]
  Mus <- phats[grep("mean", names(phats))]
  sds <- phats[grep("sd", names(phats))]
  Sigmas <- phats[grepl("sigma", names(phats)) & grepl("mu", names(phats))]
  rhos <- phats[grep("rho", names(phats))]
  S1 <- matrix(c(Sigmas[1]^2, rhos[1]*Sigmas[1]*Sigmas[2],
                 rhos[1]*Sigmas[1]*Sigmas[2], Sigmas[2]^2), 2, 2)
  S2 <- matrix(c(Sigmas[3]^2, rhos[2]*Sigmas[3]*Sigmas[4],
                 rhos[2]*Sigmas[3]*Sigmas[4], Sigmas[4]^2), 2, 2)
  Mu1 <- Mus[grepl("[1]", names(Mus))]
  Mu2 <- Mus[grepl("[2]", names(Mus))]
  Area1.95 <- mixtools::ellipse(Mu1, S1, draw = FALSE)
  Area2.95 <- mixtools::ellipse(Mu2, S2, draw = FALSE)
  Area1.50 <- mixtools::ellipse(Mu1, S1, alpha = 0.5, draw = FALSE)
  Area2.50 <- mixtools::ellipse(Mu2, S2, alpha = 0.5, draw = FALSE)
  mux1s <- phats[grepl("mux1", names(phats))]
  muy1s <- phats[grepl("muy1", names(phats))]
  mux2s <- phats[grepl("mux2", names(phats))]
  muy2s <- phats[grepl("muy2", names(phats))]
  yday.model <- with(stan_data, c(min(yday), phats["t_mean"] + c(0, phats["dt_mean"]), max(yday)))
  y.model <- with(stan_data, rep(phats[c("muy_mean[1]", "muy_mean[2]")], each = 2))
  x.model <- with(stan_data, rep(phats[c("mux_mean[1]", "mux_mean[2]")], each = 2))
  
  n.ind <- max(stan_data$id)
  palette(gplots::rich.colors(n.ind))
  
  # plot this puppy!
  
  if(type == "fit"){
    par(mar = c(0, 4, 0, 0), oma = c(4, 0, 4, 4), mgp = c(2,.5,0), 
        cex.lab = 1.25, tck = 0.01, xpd=NA)
    layout(rbind(c(1, 2), c(1, 3)))
    with(stan_data, plot(x,y, asp=1, col=id, pch = 19, cex=0.5))
    d_ply(data.frame(stan_data), "id", function(df) with(df, lines(x,y, col = id)))
    polygon(Area1.95, col = alpha("darkred", .2), bor = NA)
    polygon(Area2.95, col = alpha("darkred", .2), bor = NA)
    polygon(Area1.50, col = alpha("darkred", .7), bor = NA)
    polygon(Area2.50, col = alpha("darkred", .7), bor = NA)
    with(stan_data,plot(yday, y, col= alpha(id,.5), pch = 19, cex=0.5, xaxt="n", xlab=""))
    d_ply(data.frame(stan_data), "id",
          function(df) with(df, lines(yday, y, col = alpha(id,.5))))
    lines(yday.model, y.model, col="darkred", lwd = 4)
    with(stan_data,plot(yday, x, col= alpha(id,.5), pch = 19, cex=0.5,  xlab=""))
    d_ply(data.frame(stan_data), "id",
          function(df) with(df, lines(yday, x, col = alpha(id,.5))))
    lines(yday.model, x.model, col="darkred", lwd = 4)
  }
  # diagnostic plots
  pars.fit <- names(migration_fit)
  
  if(type == "density") 
    print(stan_dens(migration_fit, par = pars, separate_chains = TRUE, ncol = 5))
  if(type == "chains") 
    print(stan_trace(migration_fit, par = pars))
  
  if(type == "map"){
    fit_summaries <- summarizeMigrationFit(fit)
    with(fit_summaries, 
         mapview::mapview(areas.sf) + 
           mapview::mapview(centroids.sf, color = "red") + 
            mapview::mapview(lines.sf, zcol = "id"))
  }
}		
