
#' @title logistic function
#'
#'
#' @describtion \eqn{\text{expit}(x) = \frac{1}{1+\exp(-x)}}
expit <- function(x) {
  return(1/(1+exp(-x)))
}

#' @title Utility for bayesian logistic regression
#'
#' @description This function implements a kernel of a posterior distribution. Do not use this function. Use \code{bayes_logit_reg_slice} instead.
#'
#'
p <- function(beta, y, X, prior, scaling = 1){
  if (length(beta) == 1) {
    eta <- X*beta
  } else{
    eta <- X %*% beta
  }

  theta <- expit(eta)
  probs <- y * theta + (1-y) * (1-theta)

  return(scaling*(prod(probs)*prior(beta)))
}



#' @title Bayesian logistic regression using slicing algorithm
#'
#' @description
#'  This function return a sample of bayesian logistic regression \eqn{y \sim X} where \eqn{X} is a regression-matrix. With the following assumptions: \deqn{y_i |\theta_i \sim Ber( \theta_i)\\\theta_i = \text{expit}(\eta_i) \\\eta_i = x_i\beta}
#'  Where \eqn{x_i} is the \eqn{ith} row of the regression matrix \eqn{X}. To sample from the distribution \eqn{\beta | y, X}
#'
#' @param num_samples numeric: number of samples drawn
#' @param y numeric: binary classification variable
#' @param X matrix: regression matrix
#' @param prior function -> numeric: prior for \eqn{\beta} (default \eqn{beta\overset{iid}\sim \mathcal N(0,4)}, according to Kruschke (2015) \href{https://books.google.de/books?hl=de&lr=&id=FzvLAwAAQBAJ&oi=fnd&pg=PP1&dq=kruschke+2015+doing+bayesian+data+analysis&ots=ChqpP-vgXM&sig=z-P9IFkyQrkOltp6TTSlXUONRGU&redir_esc=y#v=onepage&q&f=false}{book}
#' @param scaling numeric: sometimes the floating point representation does not offer enough accuracy for the kernel as values become incereasingly small as the sample size (length of \eqn{y}) increases. Scale them up, if needed.
#' @param interval_width vector: go to \code{?MVSlice} and look at \code{w} parameter
#' @param L.lim vector: go to \code{?MVSlice} and look at \code{L.lim} parameter
#' @param R.lim vector: go to \code{?MVSlice} and look at \code{R.lim} parameter
#' @param staring_values vector: go to \code{?MVSlice} and look at \code{x0} parameter
#'
#' @examples
#' # single dimension case
#' n <- 1000
#' p_prob <- 0.5
#'
#' y <- as.numeric(runif(n) < p_prob)
#' X <- matrix(data =rep(1, n), ncol= 1)
#'
#' b = bayes_logit_reg_slice(100, y, X)
#' paste("true probability : ", p_prob)
#' paste("estimated probability: ", expit(median(b)))
#'
#' # moving onto the multidimensional case
#' n <- 1000
#'
#' # simulating sample
#' X <- matrix(data = c(rep(1, n), seq(-10, 10, length.out = n)), ncol=2)
#' props <- expit(X[,2]+2)
#' sample_y <- function(prob) {as.numeric(runif(1) <= prob)}
#' y <- sapply(props, sample_y)
#'
#' # apply bayesian logistic regression
#' p_d <- function(beta) {p(beta, y, X)}
#'
#' beta_samples <- bayes_logit_reg_slice(1000, y, X, scaling = 1000)
#'
#' beta_0 <- median(beta_samples[,1])
#' beta_1 <- median(beta_samples[,2])
#'
#' beta <- c(beta_0, beta_1)
#'
#' n <- X %*% beta
#'
#' prop_hat <- expit(n)
#'
#' plot(X[,2],prop_hat, type="l")
#' plot(X[,2],props, type="l")
#'
bayes_logit_reg_slice <- function(num_samples, y, X,
                                 prior = function(beta){prod(dnorm(beta, 0, 2))},
                                 scaling = 1,
                                 intveral_width = rep(3, length(X[1,])),
                                 L.lim = rep(-Inf, length(X[1,])),
                                 R.lim = rep(Inf, length(X[1,])),
                                 starting_values = rep(0, length(X[1,]))) {

  func <- function(beta){p(beta, y, X, prior, scaling)}

  return(MVSlice(num_samples, starting_values, intveral_width, func, L.lim, R.lim))
}
