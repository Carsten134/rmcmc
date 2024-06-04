## `rmcmc` easy to use implementations of MCMC-Algorithms
This package is part of a seminar-work we have to do in TU-Dortmund. As of right now it is planed to hold implementations of the Metropolis Hastings, Gibbs and Slice sampling algorithms.

## Getting started
### Installation
First you need to install and add the package `devtools` package to your library. For this, enter the following code in your R-console.

```{R}
install.packages(c("usethis", "devtools"))

library(usethis)
library(devtools)
```

Then execute:
```{R}
install_github("https://github.com/Carsten134/rmcmc")
```

Once you installed the package you can just add it to your library like any other package:
```{R}
library(rmcmc)
```
### Documentation
The documentation for every function is accessible with `?`. Enter in your R-Console:
```{r}
?*your_function_name
```

### Implemented functions
Right now we have implemented:
- `metropolis_hastings`: metropolis hastings implementation (very slow a.t.m.)
- `gibbs_samping`: gibb's sampling algorithm
- `simple_slice`: slice sampling for unimodal distributions
- `OneDSlice`: slice algorithm for univariate distributions