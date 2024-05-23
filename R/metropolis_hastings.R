# metropolis hastings algorithm:
#

#' metropolis Hastings implementation
#'
#' Returns sample of size \code{n} using the Metropolis Hastings algorithm.
#'
#' @examples
#'  # Define the target distribution P (e.g., standard normal distribution)
#'  P <- function(x) { dnorm(x, mean = 0, sd = 1) }
#'
#'  # Define the proposal distribution q (e.g., normal distribution centered at the current state)
#'  q <- function(x, x_prime) { dnorm(x_prime, mean = x, sd = 1) }
#'
#'  # Define the function to sample from the proposal distribution
#'  q_sample <- function(x) { rnorm(1, mean = x, sd = 1) }
#'
#'  # Initial value
#'  x0 <- 0
#'
#'  # Number of iterations
#'  iterations <- 10000
#'
#'  # Run the Metropolis-Hastings algorithm
#'  samples <- metropolis_hastings(P, q, q_sample, x0, iterations)
#'
#' @param x_0  numeric staring point of the marcov chain
#' @param p desired distribution to sample from. Has to be a function
#'  that returns the density at a given point x (note, it can also be a kernel)
#'  e.g. `p(x) -> density / value`
#' @param q proposal density, has to be a function to return density at point x
#'  e.g. `q(x) -> density`
#' @param q_sample conditional proposal distribution to sample from
#'  e.g. `q_sample(condition) -> random value ~ q | condition`
#' @param n size of the sample (NOT number of iterations)
metropolis_hastings<- function(x_0, p, q, q_sample, n) {
  # instantiate the chain
  chain <- numeric(n)

  # set first element to the starting point
  x. <- x_0
  x <- x_0
  chain[1] <- x.

  # start iteration
  t <- 2
  while(t <= n) {
    # get candidate
    x. <- q_sample(x.)

    # determine acceptance probability
    acceptance.term <- (p(x.)*q(x, x.)) / (p(x) * q(x., x))
    alpha <- min(1, acceptance.term)

    # accept / reject
    u <- runif(1)
    if (u <= alpha) {
      # store value in the chain and iterate further
      x <- x.
      chain[t] <- x.
      t = t + 1
    }
  }

  return(chain)
}
