% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metropolis_hastings.R
\name{metropolis_hastings}
\alias{metropolis_hastings}
\title{metropolis Hastings implementation}
\usage{
metropolis_hastings(x_0, p, q, q_sample, n)
}
\arguments{
\item{x_0}{numeric staring point of the marcov chain}

\item{p}{desired distribution to sample from. Has to be a function
that returns the density at a given point x (note, it can also be a kernel)
e.g. `p(x) -> density / value`}

\item{q}{proposal density, has to be a function to return density at point x
e.g. `q(x) -> density`}

\item{q_sample}{conditional proposal distribution to sample from
e.g. `q_sample(condition) -> random value ~ q | condition`}

\item{n}{size of the sample (NOT number of iterations)}
}
\description{
Returns sample of size \code{n} using the Metropolis Hastings algorithm.
}
\examples{
 # Define the target distribution P (e.g., standard normal distribution)
 P <- function(x) { dnorm(x, mean = 0, sd = 1) }

 # Define the proposal distribution q (e.g., normal distribution centered at the current state)
 q <- function(x, x_prime) { dnorm(x_prime, mean = x, sd = 1) }

 # Define the function to sample from the proposal distribution
 q_sample <- function(x) { rnorm(1, mean = x, sd = 1) }

 # Initial value
 x0 <- 0

 # Number of iterations
 iterations <- 10000

 # Run the Metropolis-Hastings algorithm
 samples <- metropolis_hastings(x0, P, q, q_sample, iterations)

}
