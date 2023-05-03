# Tuktu Migration <img src="man/figures/logo.svg" align="right" height="139" />

An R package for estimating collective migration behavior the hierarchical migration model developed in [Gurarie et al. (2019)](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.2971) to estimate migration timing for barren-ground caribou across North America.  This package uses a Bayesian MCMC sampler using [RStan](https://mc-stan.org/users/interfaces/rstan).  

A resulting analysis looks something like this, illustrating the hierarchial range ellipse and estimates (with population-level standard deviations) of migration departure and arrival times for a population of migrating caribou:

![](sandbox/FittedPlot.png)

To install, run:

```
devtools::install_github("egurarie/TuktuMigration")
```

Running the estimatoin relies on functions from the live, developmente version of the [TuktuTools](https://github.com/ocouriot/TuktuTools) package as well, which can be installed via:

```
devtools::install_github("ocouriot/TuktuTools", build_vignettes = TRUE)
```

The other key dependency is the `rstan` package. 
