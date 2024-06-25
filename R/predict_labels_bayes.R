#' @title predicting labels for bayesian logistic regression
#'
#' @describtion  This function works together with the \code{bayes_logit_reg_slice} function. Choose some aggregation of your data and apply this function with new data to get some predicted labels.
#'
#' @param beta vector: some beta parameters
#' @param X martix: new regression matrix
#' @param threshold numeric: Threshold for switching to \eqn{\hat y = 1}
#'
#' @examples
#' # example code
#'  n <- 1000
#'
#'  # simulating sample
#'  X <- matrix(data = c(rep(1, n), seq(-10, 10, length.out = n)), ncol=2)
#'  props <- expit(X[,2]+2)
#'  sample_y <- function(prob) {as.numeric(runif(1) <= prob)}
#'  y <- sapply(props, sample_y)
#'
#'  beta_samples <- bayesian.logit.slice(1000, y, X, scaling = 1000)
#'
#'  beta_hat <- apply(beta_samples, 2, mean)
#'  X_test <- matrix(data = c(rep(1, 100), seq(-10, 10, length.out = 100)), ncol=2)
#'
#'  predict_labels_bayes_logit(beta_hat, X_test)
#'
predict_labels_bayes_logit <- function(beta, X, threshold = 0.5) {
  eta <- X %*% beta
  probs <- expit(eta)
  y_hat <- as.numeric(probs >= threshold)
  return(y_hat)
}
