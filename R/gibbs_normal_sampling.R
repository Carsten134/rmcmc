#' Gibbs MVnormal sampling implementation
#'
#' @param n integer for size of sample
#' @param mu vector for mean values
#' @param sigma matrix for covariance
#' @param init vector of inital values of the chains in
#'
#' @return a \code{dataframe} as the sample
#'
MVNormGibbs <- function(n, mu, simga, init) {
  d <- length(mu)

  sample_raw <- numeric(d*n)
  sample_raw[1:d] <- init

  for(i in 2:n) { # for each observation...
    for (j in 1:d) { # for each dimension...
      current <- d*(i-1)+j
      x.given <- sample_raw[(current-(d-1)):(current-1)]
      # condition the density for the dimension on the last k-1 values
      x <- rcmvnorm(1, mu, sigma,
                    dependent.ind = j,
                    given.ind = (1:d)[-j],
                    X.given = x.given)

      # ...and then append the sample
      sample_raw[current] <- x
    }
  }
  sample <- list()
  for(j in 1:d) {
    sample[[as.character(j)]] <- sample_raw[seq(j, d*n, d)]
  }

  return(data.frame(sample))
}
