\name{SmoothedSplineDistribution}

\alias{SmoothedSplineDistribution}

\alias{dssd}
\alias{pssd}
\alias{qssd}
\alias{rssd}

\title{Smoothed Spline Distribution}


\description{
    
    A collection and description of functions to compute 
    density, distribution function, quantile function 
    and to generate random variates for empirical 
    distributions. Estimates are done using smoothing 
    spline ANOVA models with cubic spline, linear spline, 
    or thin-plate spline marginals for numerical variables.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{dssd} \tab Spline smoothed density, \cr
    \code{pssd} \tab spline smoothed probability function, \cr
    \code{qssd} \tab spline smoothed quantiles, \cr
    \code{rssd} \tab random deviates drawn from a ssd.}
    
}


\usage{
dssd(x, param)
pssd(q, param)
qssd(p, param)
rssd(n, param)
}


\arguments{

    \item{n}{
        number of observations.
        } 
    \item{p}{
        a numeric vector of probabilities.
        }
    \item{x, q}{
        a numeric vector of quantiles.
        }
    \item{param}{
    	a S3 object specifying the parameters as
    	returned by the function \code{ssdFit}.
    	}
    
}


\value{

    All values are numeric vectors:
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
    
}


\details{

	This is an easy to use version for the functions implemented
	in Chong Gu's contributed R package "gss" which is downloadable
	from the CRAN server. If you require more functionality, e.g.
	to tailor the parameter estimate we recommend to install Gu's 
	package. The installation does not interfere with the functions
	implemented in Rmetrics.
	
	Note, before you can use the functions you have to estimate the
	parameters \code{param} using the function \code{ssdFit}.
	
}


\note{

	The functions do not implement the full functionality provided
	by R's contributed package "gss". Only the "cubic" spline method
	is provided and most of the optional arguments are set to default
	values. Since the original "gss" package does not interfere with
	Rmetrics you can load it in parallel. 
	
	It's worth to note that the "gss" package does not work under SPlus,
	but the modified and adapted functions \code{ssdFit} and \code{*ssd}
	can be used.
		
}
	

\author{

    Chong Gu for the code from R's contributed package 'gss', \cr
    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\references{

Gu C., Wang, J. (2003);
 	emph{Penalized likelihood density estimation: Direct cross-validation 
 	and scalable approximation}, 
 	Statistica Sinica, 13, 811--826. 

Gu C. (2002);
	\emph{Smoothing Spline ANOVA Models},
	New York, Springer-Verlag. 

}


\examples{
## SOURCE("fBasics.13C-SmoothedSplineDistribution")

## ssd -
\dontrun{
   xmpBasics("\nStart: Spline Smoothed Distribution > ")
   par(mfcol = c(2, 1), cex = 0.5)
   set.seed(1953)
   x = rnorm(1000)
   param = ssdFit(x)
   # Plot empirical density and compare with fitted density:
   hist(x, n = 25, probability = TRUE, border = "white", col = "steelblue4") 
   s = seq(min(x), max(x), 0.1)
   lines(s, dssd(s, param), lwd = 2)
   # Plot df and compare with true df:
   plot(sort(x), (1:1000/1000), main = "Probability", col = "steelblue4")
   lines(s, pssd(s, param), lwd = 2)
   # Compute quantiles:
   qssd(pssd(seq(-3, 3, 1), param), param) 
}
}


\keyword{distribution}

