\name{NormalLaplaceDistribution}
\alias{NormalLaplaceDistribution}
\alias{dnl}
\alias{pnl}
\alias{qnl}
\alias{rnl}


\title{
  Normal Laplace Distribution
}
\description{
  Density function, distribution function, quantiles and random number
  generation for the normal Laplace distribution, with
  parameters \eqn{\beta}{beta} (skewness), \eqn{\delta}{delta}
  (scale), \eqn{\mu}{mu} (location) and \eqn{\nu}{nu} (shape). 
}
\usage{
dnl(x, mu = 0, sigma = 1, alpha = 1, beta = 1,
    param = c(mu,sigma,alpha,beta), log = FALSE)
pnl(q, mu = 0, sigma = 1, alpha = 1, beta = 1,
    param = c(mu,sigma,alpha,beta))
qnl(p, mu = 0, sigma = 1, alpha = 1, beta= 1,
    param = c(mu,sigma,alpha,beta), log = FALSE,
    tol = 10^(-5), nInterpol = 100, subdivisions = 100, ...) 
rnl(n, mu = 0, sigma = 1, alpha = 1, beta = 1,
    param = c(mu,sigma,alpha,beta))

}
\arguments{
  \item{x,q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of random variates to be generated.}
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{sigma}{Scale parameter \eqn{\sigma}{sigma}, default is 1.}
  \item{alpha}{Skewness parameter \eqn{\alpha}{alpha}, default is 1.}
  \item{beta}{Shape parameter \eqn{\beta}{beta}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu,sigma,alpha,beta)}.}
  \item{log}{Logical; if \code{log = TRUE}, probabilities are given as
    log(p).}
  \item{tol}{Specified level of tolerance when checking if
    parameter beta is equal to 0.}
  \item{subdivisions}{The maximum number of subdivisions used to
    integrate the density and determine the accuracy of the distribution
    function calculation.}
  \item{nInterpol}{Number of points used in \code{qnl} for cubic
    spline interpolation of the distribution function.}
  \item{\dots}{Passes arguments to \code{uniroot}.}
}
\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones. 

  The density function is

  \deqn{f(y)=\frac{\alpha\beta}{\alpha+\beta}\phi\left( \frac{y-\mu}{ \alpha }%
  \right)[R((\alpha\sigma)-\frac{(y-\mu)}{\sigma})+R(\beta \sigma+\frac{(y-\mu)}{\sigma})]}{}%
            

  The distribution function is
  \deqn{F(y)=\Phi(\frac{y-\mu}{\sigma})-\phi(\frac{y-\mu}{\sigma})[\beta R%
  (\alpha\sigma-\frac{y-\mu}{\sigma})-\alpha R(\beta\sigma+\frac{y-\mu}{\sigma})]/(\alpha+\beta)}{}%
 

  The Mill's Ratio is
  \deqn{R(z)=\frac{1-\Phi(z)}{\phi(z)}}{}%
  since the Mill's Ratio is not stable for z greater than 7 
  or so, we use the log function to get around the instability.

  Generation of random observations from the normal Laplace distribution
  using \code{rnl} is based on the representation
  \deqn{Y\stackrel{d}{=}Z+W}{}%
  where \eqn{Z} and \eqn{W}
 are independent random variables with
  \deqn{Z\sim N(\mu,\sigma^2)}{}%
  and \eqn{W} following an asymmetric Laplace distribution with pdf 
  \deqn{f_W(w) = \left\{ \begin{array}{ll}%
  \frac{\alpha\beta}{\alpha+\beta}\exp^{\beta w} &
  \textrm{for $w\le0$}\\ %
  \frac{\alpha\beta}{\alpha+\beta}\exp^{-\beta w} & \textrm{for $w>0$}%
  \end{array} \right.}{}



}
\value{
  \code{dnl} gives the density function, \code{pnl} gives the
  distribution function, \code{qnl} gives the quantile function and
  \code{rnl} generates random variates.

}
\references{
  
  William J. Reed. (2006) The Normal-Laplace Distribution and Its
  Relatives. In \emph{Advances in Distribution Theory, Order Statistics
  and Inference}, pp. 61--74.  {Birkhauser},
  Boston.

}

\author{
  David Scott \email{d.scott@auckland.ac.nz}, Jason Shicong Fu

}

\examples{
param  <-  c(0,1,3,2)
par(mfrow = c(1,2))


##curves of density and distribution
curve(dnl(x,param ),-5,5,n = 1000)
title("Density of the Normal Laplace Distribution")
curve(pnl(x,param ),-5,5,n = 1000)
title("Distribution funcion of the Normal Laplace Distribution")


##example of density and random numbers 
 par(mfrow = c(1,1))
 param1  <-  c(0,1,1,1)
 data1  <-  rnl(1000, param = param1)
 curve(dnl(x, param = param1),
       from=-5,to=5,n = 1000, col = 2)
 hist(data1, freq = FALSE, add = TRUE)
 title("Density and Histogram")




}
\keyword{distribution}
