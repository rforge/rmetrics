\name{MonteCarlo.ExpressCertificate.Classic}
\Rdversion{1.1}
\encoding{latin1}
\alias{MonteCarlo.ExpressCertificate.Classic}
\alias{Conditional.MonteCarlo.ExpressCertificate.Classic}
\alias{MonteCarlo.ExpressCertificate}
\title{
Monte Carlo valuation of Classic Express Certificates
}
\description{
Monte Carlo valuation methods for Express Classic Certificates using the Euler scheme
or sampling from conditional densities
}

\usage{
MonteCarlo.ExpressCertificate.Classic(S, X, T, K, r, r_d, 
  sigma, ratio = 1, mc.steps = 1000, mc.loops = 20)
Conditional.MonteCarlo.ExpressCertificate.Classic(S, X, T, K, r, r_d, 
  sigma, ratio = 1, mc.loops = 20, conditional.random.generator = "rnorm")
MonteCarlo.ExpressCertificate(S, X, T, K, B,  
 r, r_d, sigma, mc.steps = 1000, mc.loops = 20, payoff.function)  
}

\arguments{
  \item{S}{the asset price, a numeric value}
  \item{X}{a vector of early exercise prices ("Bewertungsgrenzen"), , vector of length (n-1)}
  \item{T}{a vector of evaluation times measured in years ("Bewertungstage"), vector of length n}
  \item{K}{vector of fixed early cash rebates in case of early exercise, length (n-1)}
  \item{B}{barrier level}
  \item{r}{the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa.}
  \item{r_d}{the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa.}
  \item{sigma}{the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa.}
  \item{ratio}{ratio, number of underlyings one certificate refers to, a numeric value;
                e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset}
  \item{mc.steps}{ Monte Carlo steps in one path }
  \item{mc.loops}{ Monte Carlo Loops (iterations) }
  \item{conditional.random.generator}{A pseudo-random or quasi-random (Halton-Sequence, Sobol-Sequence)
    generator for the conditional distributions, one of \code{"rnorm"},\code{"rnorm.halton"},\code{"rnorm.sobol"}}
  \item{payoff.function}{payoff function}
}
\details{
The conventional Monte Carlo uses the Euler scheme with \code{mc.steps} steps in order 
to approximate the continuous-time stochastic process.

The conditional Monte Carlo samples from conditional densities \eqn{f(x_{i+1}|x_i)} for \eqn{i=0,\dots,(n-1))}, 
which are univariate normal
distributions for the log returns of the Geometric Brownian Motion and Jump-diffusion model:
\eqn{f(x_1,x_2,..,x_n) = f(x_n|x_{n-1}) \cdot \dots \cdots  f(x_2|x_1) \cdot f(x_1|x_0)}
The conditional Monte Carlo does not need the \code{mc.steps} points 
in between and has a much better performance.
}

\value{
returns a list of
\item{stops}{stops}
\item{prices}{vector of prices, length \code{mc.loops}} 
\item{p}{Monte Carlo estimate of the price = \code{mean(prices)}}
\item{S_T}{vector of underlying prices at maturity}
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }

\keyword{ math }
