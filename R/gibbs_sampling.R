#' Gibbs sampling implementation
#'
#' @param cond list of conditional distributions
#' @param init vector of inital values of the chains in
#' @param n sample size
#'
#' @return a \code{dataframe} with the given list keys from \code{cond} as variables
#'
#' @examples
#'  # instantiating conditionals
#'  cond <- list(x_1 = function(x_2){return(rnorm(1, x_2))},
#'               x_2 = function(x_1){return(rnorm(1, x_1))})
#'
#'  # simualte sample
#'  sample <- gibbs(cond, c(1,1), 1000)
#'
#'  # plot sample
#'  hist(sample$x_1)
#'  hist(sample$x_2)
gibbs <- function(cond, init, n) {
  ## checking conditions for execution (just general typechecking) #############
  for(func in cond) {
    if (!is.function(func)){
      stop(cat("error in given cond argument: ", func, "is not a function"))
    }
  }
  for (i in init) {
    if (!is.numeric(i)) {
      stop(cat("error in given init argument: ", i, "is not numeric"))
    }
  }

  if (length(cond) != length(init)) {
    stop("error with given arguments cond and init: They don't have the same length")
  }

  ## generating the sample #####################################################

  vars <- names(cond)
  k <- length(cond)
  sample_raw <- numeric(k*n)

  # take initial values into the raw sample
  for (j in 1:k) {
    sample_raw[j] <- init[[vars[j]]]
  }

  # flattened generation
  for(i in 2:n) {
    for (j in 1:k) {
      x <- do.call(cond[[vars[j]]], as.list(tail(sample_raw, (k-1))))
      sample_raw[(k*(i-1)+j)] <- x
    }
  }

  sample <- list()
  for(j in 1:length(vars)) {
    sample[[vars[j]]] <- sample_raw[seq(j, k*n, k)]
  }

  return(data.frame(sample))

}
