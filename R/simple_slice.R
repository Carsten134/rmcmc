#' @title Slice sampling initial implementation
#' @description
#'  This was the first implementation of the slice sampling. This implementation is only for unimodal distributions, where \eqn{f^{-1}([y,+\infty])} is an interval.
#'
#' @param it integer: number of iterations
#' @param x0 numeric: starting point for iteration
#' @param f function: density you want to model
#' @param inv.l function: lower bound of interval \eqn{f^{-1}([y, +\infty])}
#' @param inv.u function: returns upper bound of interval \eqn{f^{-1}([y, +\infty])}
#'
#' @examples
#'    ## Standard normal distribution with bad initial value:
#'    set.seed(74765)
#'    x.firsttest <- simple_slice(1000, x0 = 15, f = function(x) {exp(-x^2/2)/sqrt(2 * pi)},
#'                               inv.l = function(x) {- sqrt(-2 * log(x * sqrt(2 * pi)))},
#'                               inv.u = function(x) {sqrt(-2 * log(x * sqrt(2 * pi)))})
#'
#'    hist(x.firsttest[-1])
#'    which.max(x.firsttest[-1])
#'    x.firsttest[1:10]
#'    ## really fast burn-in. only first 3 samples are unlikely for target
#'    ## distribution.
#'    ## Better choice of x0 results in faster burn-in
#'
#'
#'    ## Try out another example from Roberts and Casella: exp-distribution:
#'    x.exp <- simple_slice(10000, x0 = 1, f = function(x) {0.5 * exp(-sqrt(x))},
#'                         inv.l = function(x) {0},
#'                         inv.u = function(x) {log(2 * x)^2})
#'
#'    hist(x.exp[x.exp < 70], breaks = 0:70)
#'
#'
simple_slice <- function(it, x0, f, inv.l, inv.u) {
  x <- numeric(it)
  x[1] <- x0
  for(i in 2:it) {
    u <- runif(1, min = 0, max = f(x[i - 1]))

    ## Inverse has to be more flexible
    x[i] <- runif(1, min = inv.l(u), max = inv.u(u))
  }

  return(x)
}

