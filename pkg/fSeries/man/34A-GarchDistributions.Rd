\name{GarchDistributions}

\alias{GarchDistributions}

\alias{dsnorm}
\alias{psnorm}
\alias{qsnorm}
\alias{rsnorm}

\alias{dstd}
\alias{pstd}
\alias{qstd}
\alias{rstd}

\alias{dsstd}
\alias{psstd}
\alias{qsstd}
\alias{rsstd}

\alias{dged}
\alias{pged}
\alias{qged}
\alias{rged}

\alias{dsged}
\alias{psged}
\alias{qsged}
\alias{rsged}


\alias{normFit}
\alias{snormFit}
\alias{stdFit}
\alias{sstdFit}
\alias{gedFit}
\alias{sgedFit}


\title{GARCH Distributions and Parameter Estimation}


\description{
    
    A collection and description of functions to compute 
    density, distribution function, quantile function and 
    to generate random variates for the skew normal, the skew 
    Student-t, and skew generalized error distribution. In
    addition maximum likelihood estimators are available to 
    fit the parameters of a distribution and to compute 
    basic statistical properties. 
	\cr
    
    The functions are:
    
    \tabular{ll}{
	\code{[dpqr]norm} \tab Normal distribution from R's base package, \cr
	\code{[dpqr]snorm} \tab Skew Normal distribution, \cr
	\code{[dpqr]std} \tab Symmetric Student-t Distribution, \cr
	\code{[dpqr]sstd} \tab Skew Student-t Distribution, \cr
	\code{[dpqr]ged} \tab Symmetric GED distribution, \cr
	\code{[dpqr]sged} \tab Skew GED distribution. }
	
	The estimatore are:
	
	The functions are:
	
	\tabular{ll}{
	\code{normFit} \tab MLE parameter fit for a Normal distribution, \cr
	\code{snormFit} \tab MLE parameter fit for a skew Normal distribution, \cr
	\code{stdFit} \tab MLE parameter fit for a Sudent-t distribution, \cr
	\code{stdFit} \tab MLE parameter fit for a skew Sudent-t distribution, \cr
	\code{gedFit} \tab MLE parameter fit for a generalized error distribution, \cr
	\code{nigFit} \tab MLE parameter fit for a skew generalized error distribution. }
}


\usage{
dsnorm(x, mean = 0, sd = 1, xi = 1.5)
psnorm(q, mean = 0, sd = 1, xi = 1.5)
qsnorm(p, mean = 0, sd = 1, xi = 1.5)
rsnorm(n, mean = 0, sd = 1, xi = 1.5)

dstd(x, mean = 0, sd = 1, nu = 5)
pstd(q, mean = 0, sd = 1, nu = 5)
qstd(p, mean = 0, sd = 1, nu = 5)
rstd(n, mean = 0, sd = 1, nu = 5)

dsstd(x, mean = 0, sd = 1, nu = 5, xi = 1.5)
psstd(q, mean = 0, sd = 1, nu = 5, xi = 1.5)
qsstd(p, mean = 0, sd = 1, nu = 5, xi = 1.5)
rsstd(n, mean = 0, sd = 1, nu = 5, xi = 1.5)

dged(x, mean = 0, sd = 1, nu = 2)
pged(q, mean = 0, sd = 1, nu = 2)
qged(p, mean = 0, sd = 1, nu = 2)
rged(n, mean = 0, sd = 1, nu = 2)

dsged(x, mean = 0, sd = 1, nu = 2, xi = 1.5)
psged(q, mean = 0, sd = 1, nu = 2, xi = 1.5)
qsged(p, mean = 0, sd = 1, nu = 2, xi = 1.5)
rsged(n, mean = 0, sd = 1, nu = 2, xi = 1.5)

normFit(x, \dots)
snormFit(x, \dots)
stdFit(x, \dots)
sstdFit(x, \dots)
gedFit(x, \dots)
sgedFit(x, \dots)
}


\arguments{

    \item{mean, sd, nu, xi}{
        location parameter \code{mean},
        scale parameter \code{sd},
        shape parameter \code{nu}, 
        skewness parameter \code{xi}.
        }
    \item{n}{
        number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    \item{\dots}{
		parameters parsed to the optimization function \code{nlm}.
		}
    
}


\value{

    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates, \cr
    all values are numeric vectors.
    \cr
    
    The functions \code{*Fit} return a list with the following components:
  
  	\item{estimate}{
		the point at which the maximum value of the log liklihood 
		function is obtained.
		}
  	\item{objective}{
		the value of the estimated maximum, i.e. the value of the
		log liklihood function.
		}
  	\item{message}{
		an integer indicating why the optimization process terminated.
		}
  	\item{code}{
		an integer indicating why the optimization process terminated.\cr
		1: relative gradient is close to zero, current iterate is probably 
			solution; \cr
		2: successive iterates within tolerance, current iterate is probably 
			solution; \cr
		3: last global step failed to locate a point lower than \code{estimate}. 
			Either \code{estimate} is an approximate local minimum of the 
			function or \code{steptol} is too small; \cr
 		4: iteration limit exceeded; \cr
 		5: maximum step size \code{stepmax} exceeded five consecutive times. 
 			Either the function is unbounded below, becomes asymptotic to a 
 			finite value from above in some direction or \code{stepmax} 
			is too small.
			}
  	\item{gradient}{
		the gradient at the estimated maximum.
		}
  	\item{steps}{
		number of function calls.
		}
    
    
}

\details{
  
    \bold{Symmetric Normal Distibution:}
    \cr\cr
    The functions for the normal distribution are part of R's
    base package. The functions for the symmetric Student-t 
    distribution are rescaled in such a way that they have unit 
    variance in contrast to the Student-t family \code{dt}, \code{pt}, 
    \code{qt} and \code{rt} which are part of R's base package.
    The generalized error distribution functions are defined as 
    described by Nelson (1991).
    \cr
    
    \bold{Skew Normal Distribution:}
    \cr\cr
    The skew normal distribution functions are defined as described
    by Fernandez and Steel (2000).
    cr
    
    \bold{Parameter Estimation:}
    \cr\cr
	The function \code{\link{nlm}} is used to minimize the "negative" maximum 
	log-likelihood function. \code{nlm} carries out a minimization using 
	a Newton-type algorithm.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{

Nelson D.B. (1991);
    \emph{Conditional Heteroscedasticity in Asset Returns: A New Approach},
    Econometrica, 59, 347--370.
    
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}


\examples{
## SOURCE("fSeries.34A-GarchDistributions")

## snorm -
   xmpSeries("\nStart: Skew Normal Distribuion:  > ")
   par(mfrow = c(2, 2), cex = 0.75)
   set.seed(1953)
   r = rsnorm(n = 1000, mean = 1, sd = 0.5, xi = 1.5)
   plot(r, type = "l", main = "snorm: xi = 1.5")
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue4")
   x = seq(-4, 6, 0.1)
   lines(x, dsnorm(x = x, mean = 1, sd = 0.5, xi = 1.5))
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue4")
   lines(x, psnorm(x, mean = 1, sd = 0.5, xi = 1.5))
   # Compute quantiles:
   qsnorm(psnorm(q = -4:6, mean = 1, sd = 0.5, xi = 1.5), 
     mean = 1, sd = 0.5, xi = 1.5) 
     
## sstd -
   xmpSeries("\nStart: Skew Student-t Distribution:  > ")
   par(mfrow = c(2, 2), cex = 0.75)
   set.seed(1953)
   r = rsstd(n = 1000, nu = 4, xi = 1.5)
   # Print Variance:
   var(r)
   plot(r, type = "l", main = "sstd: xi = 1.5")
   # Plot empirical density and compare with true density:
   hist(r, n = 30, xlim = c(-5, 5), probability = TRUE, 
     border = "white", col = "steelblue4")
   x = seq(-5, 5, 0.1)
   lines(x, dsnorm(x = x, xi = 1.5))
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue4")
   lines(x, psstd(x, xi = 1.5))
   # Compute quantiles:
   qsstd(psstd(q = -5:5, xi = 1.5), xi = 1.5) 
   
## sged -
   xmpSeries("\nStart: Skew Generalized Error Distribuion:  > ")
   par(mfrow = c(2, 2), cex = 0.75)
   set.seed(1953)
   r = rsged(n = 1000, mean = 1, sd = 0.5, xi = 1.5)
   plot(r, type = "l", main = "sged: xi = 1.5")
   # Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white", col = "steelblue4")
   x = seq(-1, 5, 0.1)
   lines(x, dsged(x = x, mean = 1, sd = 0.5, xi = 1.5))
   # Plot df and compare with true df:
   plot(sort(r), (1:1000/1000), main = "Probability", col = "steelblue4")
   lines(x, psged(x, mean = 1, sd = 0.5, xi = 1.5))
   # Compute quantiles:
   qsged(psged(q = -1:5, mean = 1, sd = 0.5, xi = 1.5), 
     mean = 1, sd = 0.5, xi = 1.5) 

## snormFit -
   xmpSeries("\nStart: MLE Fit to skew Normal Density > ")
   options(warn = -1) # suppress negative logs from nlm
   normFit(rnorm(1000))
   snormFit(rnorm(1000))
   
## sstdFit -
   xmpSeries("\nNext: MLE Fit to skew Student-t Density > ")
   sstdFit(rsstd(1000, mean = 1, sd = 1.5, nu = 7, xi = 2/3))
   
## sgedFit -
   xmpSeries("\nNext: MLE Fit to skew Generalized Error Density > ")
   sgedFit(rsged(1000, mean = -1, sd = 0.5, nu = 3, xi = 3/2),
   	 print.level = 2)
}


\keyword{distribution}

