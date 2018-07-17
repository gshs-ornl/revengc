\name{dqpois_trunc}
\alias{dqpois_trunc}
\title{Right-truncated Poisson or Quasi-Poisson density}
\usage{
dqpois_trunc(n, lambda, quasipoisson_phi, upperbound)
}

\arguments{
\item{n}{A vector of (non-negative integer) quantiles.}
\item{lambda}{A vector of (non-negative) means.}
\item{quasipoisson_phi}{A numeric class value to help with overdispersion.  If the value = 1, the original Poisson mean-variance relationship holds, which implies mean = variance.  When the value is > 1, the user is accounting for overdispersion (variance becomes proportional to the mean by quasipoisson_phi value).  The value must strictly be >= 1.}
\item{upperbound}{Non-negative numeric value representing a right-truncated point.}
}

\value{A numeric class output.  Example section shows formulas.}

\description{This function calculates a right-truncated Poisson (\eqn{\phi = 1}) or Quasi-Poisson (\eqn{\phi != 1}) density.  }



\examples{
# Please refer to vignettes for more information
# Below is dqpois_trunc function

dqpois = function(n, lambda, quasipoisson_phi) {
  mu = lambda
  k = mu/(quasipoisson_phi - 1)
  r = dnbinom(n, mu = mu, size = k)
  return(r)
}

pqpois = function(n, lambda, quasipoisson_phi, lower.tail) {
  mu = lambda
  k = mu/(quasipoisson_phi - 1)
  r = pnbinom(n, mu = mu, size = k, lower.tail = lower.tail)
  return(r)
}

dqpois_trunc<-function(n,lambda,quasipoisson_phi, upperbound){
  density.output = dqpois(n,lambda, quasipoisson_phi)*
  (1/(1-(pqpois((upperbound-1),lambda,quasipoisson_phi, lower.tail=FALSE))))
  return(density.output)
} 

}


\references{
{Saffari, S. E., Adnan, R., & Greene, W. (2011). Handling of over-dispersion of count data via truncation using Poisson regression model. \emph{Journal of Computer Science and Computational Mathematics}, 1(1), 1-4.}
}