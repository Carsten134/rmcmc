#' @title One D slice implementation
#' @description
#' This implemenation does not need a function to determine the reverse image. Instead holistic techniques will be used, similar to the accept reject condition.
#'
#' @param it integer: number of iterations (or sample size)
#' @param x0 numeric: starting value
#' @param f function: density to be modeled
#' @param L.lim numeric: lower limit for candidate slice (default \code{-Inf})
#' @param R.lim numeric: upper limit for candidate slice (default \code{Inf})
#'
#' @examples
#' ## Normal distribution:
#'  xnormal <- OneDSlice(10000, x0 = 1, w = 1/8,
#'                       f = function(x) {exp(-x^2/2)/sqrt(2 * pi)})
#'
#'  hist(xnormal)
#'  ## Works.
#'
#'  ## Multimodal case:
#'  xmm <- OneDSlice(10000, x0 = 1, w = 1/8,
#'                   f = function(x) {exp(-x^2/2)/sqrt(2 * pi) +
#'                       exp(-(x - 4)^2/2)/sqrt(2 * pi)})
#'
#'  hist(xmm, freq = FALSE, ylim = c(0, 0.25), breaks = 100)
#'  curve((exp(-x^2/2)/sqrt(2 * pi) +
#'           exp(-(x - 4)^2/2)/sqrt(2 * pi))/2,
#'        from = -4, to = 8, add = TRUE)
#'  ## works as well.
#'
#'  xmm2 <- OneDSlice(10000, x0 = 1, w = 1/8,
#'                    f = function(x) {exp(-x^2/2)/sqrt(2 * pi) +
#'                        exp(-(x - 4)^2/2)/sqrt(2 * pi) +
#'                        exp(-((x + 5)/3)^2/2)/sqrt(2 * pi * 9)})
#'
#'  hist(xmm2, freq = FALSE, ylim = c(0, 0.15), breaks = 100)
#'  curve((exp(-x^2/2)/sqrt(2 * pi) +
#'           exp(-(x - 4)^2/2)/sqrt(2 * pi)+
#'           exp(-((x + 5)/3)^2/2)/sqrt(2 * pi * 9))/3,
#'        from = -15, to = 8, add = TRUE)
#'  ## Looks perfect.
#'
#'  ## Try out case with bad initial value:
#'  ##...
#'
#'  ## What if distribution is bounded, like exponential distribution?
#'  ## In this case we need to set the lower limit for the slice L.lim = 0.
#'
#'  xexp <- OneDSlice(10000, x0 = 1, w = 1/8,
#'                    f = function(x) {0.5 * exp(-x * 0.5)},
#'                    L.lim = 0)
#'
#'  hist(xexp, freq = FALSE, breaks = 100)
#'  curve(0.5 * exp(-x * 0.5), from = 0, to = 10, add = TRUE)
#'
#'
#'
OneDSlice <- function(it, x0, w, f, L.lim = -Inf, R.lim = Inf) {
  #browser()
  ## Vector of simulated data:
  x <- numeric(it)
  x[1] <- x0

  for(i in 2:it) {
    ## uniformly generate auxiliary variable
    u <- runif(1, min = 0, max = f(x[i - 1]))

    ## Determine sampling interval:
    L <- x[i - 1] - w * runif(1)
    if(L < L.lim) {
      L <- L.lim
    }

    R <- L + w
    if(R > R.lim) {
      R <- R.lim
    }

    ## Expand interval until lower and upper bound are outside slice:
    while(f(L) > u) {
      L <- L - w
      if(L < L.lim) {
        L <- L.lim
        break
      }
    }
    while(f(R) > u) {
      R <- R + w
      if(R > R.lim) {
        R <- R.lim
        break
      }
    }

    ## sample from [L, R]. Accept, if x_star is in [L, R], else shrink interval
    repeat {
      xstar <- runif(1, L, R)
      if(f(xstar) > u) {
        x[i] <- xstar
        break
      }
      else {
        if(xstar < x[i - 1]) {L <- xstar}
        if(xstar > x[i - 1]) {R <- xstar}
      }
    }

  }
  return(x)
}
