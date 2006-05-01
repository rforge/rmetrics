\name{GevDistribution}

\alias{GevDistribution}

\alias{dgev}
\alias{pgev}
\alias{qgev}
\alias{rgev}
\alias{gevMoments}

\alias{devd}
\alias{pevd}
\alias{qevd}
\alias{revd}
\alias{evdMoments}

\alias{gevMoments}


\title{Generalized Extreme Value Distribution}


\description{
  
    A collection and description functions to compute
    the generalized extreme value distribution. The 
    functions compute density, distribution function, 
    quantile function and generate random deviates 
    for the GEV, for the Frechet, Gumbel, and Weibull 
    distributions. 
    \cr
    
    The GEV distribution functions are:
    
    \tabular{ll}{
    \code{dgev} \tab density of the GEV Distribution, \cr
    \code{pgev} \tab probability function of the GEV Distribution, \cr
    \code{qgev} \tab quantile function of the GEV Distribution, \cr
    \code{rgev} \tab random variates from the GEV Distribution. \cr
    \code{[dpqr]frechet} \tab Frechet Distribution, \cr
    \code{[dpqr]gumbel} \tab Gumbel Distribution, \cr
    \code{[dpqr]weibull} \tab Weibull Distribution, \cr
    \code{[dpqr]evd} \tab an alternative call for the GEV Distribution, \cr
    \code{gevMoments} \tab computes true mean and variance, \cr
    \code{evdMoments} \tab yet another function to compute moments. }
    
}


\usage{
dgev(x, xi = 1, mu = 0, sigma = 1, log = FALSE)
pgev(q, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
qgev(p, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
rgev(n, xi = 1, mu = 0, sigma = 1)
gevMoments(xi = 0, mu = 0, sigma = 1)

devd(x, location = 0, scale = 1, shape = 0, log = FALSE)
pevd(q, location = 0, scale = 1, shape = 0, lower.tail = TRUE)
qevd(p, location = 0, scale = 1, shape = 0, lower.tail = TRUE)
revd(n, location = 0, scale = 1, shape = 0)
evdMoments(location = 0, scale = 1, shape = 0) 
}


\arguments{

    \item{add}{
        [gevrlevelPlot] - \cr
        whether the return level should be added graphically to a
        time series plot; if \code{FALSE} a graph of the profile
        likelihood curve showing the return level and its confidence
        interval is produced.
        }
    \item{block}{
        block size.
        }
    \item{alphaRange, xiRange}{
        [saparmPlot] - \cr
        plotting ranges for \code{alpha} and \code{xi}. By default the
        values are automatically selected.
        }
    \item{ci}{
        [hillPlot] - \cr
        probability for asymptotic confidence band; for no
        confidence band set \code{ci} to zero.
        }
    \item{description}{
        a character string which allows for a brief description.
        }
    \item{doplot}{
        a logical. Should the results be plotted?
        \cr
        [shaparmPlot] - \cr
        a vector of logicals of the same lengths as tails
        defining for wich tail depths plots should be created,
        by default plots will be generated for a tail depth of 5
        percent. By default \code{c(FALSE, FALSE, FALSE, FALSE, 
        TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)}.
        }
    \item{kBlocks}{
        [gevrlevelPlot] - \cr
        specifies the particular return level to be estimated; default 
        set arbitrarily to 20.
        }
    \item{labels}{
        [hillPlot] - \cr
        whether or not axes should be labelled.
        }
    \item{location, scale, shape}{
        \code{loc} is the location parameter, 
        \code{scale} the scale parameter,
        and \code{shape} is the shape parameter.
        The default values are \code{loc=0}, \code{scale=1}, and 
        \code{shape=0}.
        }
    \item{log}{
        a logical, if \code{TRUE}, the log density is returned.
        }
    \item{lower.tail}{
        a logical, if \code{TRUE}, the default, then
        probabilities are \code{P[X <= x]}, otherwise, \code{P[X > x]}.
        }
    \item{model}{
        [gevSim][gumbelSim] - \cr
        a list with components \code{shape}, \code{location} and 
        \code{scale} giving the parameters of the GEV distribution.
        By default the shape parameter has the value -0.25, the
        location is zero and the scale is one.
        To fit random deviates from a Gumbel distribution set 
        \code{shape=0}.
        }
    \item{n}{
        [gevSim][gumbelSim] - \cr
        number of generated data points, an integer value.
        \cr
        [rgev][revd] - \cr        
        the number of observations.
        } 
    \item{object}{
        [summary][grlevelPlot] - \cr
        a fitted object of class \code{"gevFit"}.
        }
    \item{plottype}{
        [hillPlot] - \cr
        whether \code{alpha}, \code{xi} (1/alpha) or
        \code{quantile} (a quantile estimate) should be plotted.
        }
    \item{p}{
        [qgev][qevs] - \cr
        a numeric vector of probabilities.
        [hillPlot] - \cr
        probability required when option \code{quantile} is
        chosen.
        }
    \item{q}{
        [pgev][pevs] - \cr
        a numeric vector of quantiles.
        }
    \item{seed}{
        [gevSim] - \cr
        an integer value to set the seed for the random number generator.
        }
    \item{start}{
        [hillPlot] - \cr
        lowest number of order statistics at which to plot
        a point.
        }
    \item{title}{
        [gevFit] - \cr
        a character string which allows for a project title.
        }
    \item{type}{
        a character string denoting the type of parameter estimation,
        either by maximum likelihood estimation \code{"mle"}, the
        default value, or by the probability weighted moment menthod
        \code{"pwm"}.
        }
    \item{which}{
        [plot][summary] - \cr
        a vector of logicals, one for each plot, denoting which plot 
        should be displayed. Alkternatively if \code{which="ask"} the
        user will be interactively asked which of the plots should be
        desplayed. By default \code{which="all"}.
        }
    \item{x}{
        [dgev][devd] - \cr
        a numeric vector of quantiles.
        \cr
        [gevFit] - \cr
        data vector. In the case of \code{method="mle"} the interpretation 
        depends on the value of block: if no block size is specified then 
        data are interpreted as block maxima; if block size is set, then data
        are interpreted as raw data and block maxima are calculated. 
        \cr
        [hillPlot][shaparmPlot] - \cr
        the data from which to calculate the shape parameter, a
        numeric vector.
        \cr
        [print][plot] - \cr
        a fitted object of class \code{"gevFit"}.
        }
    \item{xi, mu, sigma}{
        [*gev] - \cr
        \code{xi} is the shape parameter, \code{mu} the location parameter,
        and \code{sigma} is the scale parameter. The default values are 
        \code{xi=1}, \code{mu=0}, and \code{sigma=1}. Note, if \code{xi=0} 
        the distribution is of type Gumbel.
        }
    \item{\dots}{
        [gevFit] - \cr
        control parameters optionally passed to the 
        optimization function. Parameters for the optimization
        function are passed to components of the \code{control} argument of
        \code{optim}. 
        \cr
        [hillPlot] - \cr
        other graphics parameters.
        \cr
        [plot][summary] - \cr
        arguments passed to the plot function.
        }    

}


\value{
    
    \code{d*} returns the density, \cr
    \code{p*} returns the probability, \cr
    \code{q*} returns the quantiles, and \cr
    \code{r*} generates random variates. \cr
    
    All values are numeric vectors.
    
}


\details{

    \bold{Generalized Extreme Value Distribution:}
    \cr\cr
    Computes density, distribution function, quantile function and 
    generates random variates for the Generalized Extreme Value 
    Distribution, GEV, for the Frechet, Gumbel, and Weibull 
    distributions.

}
  

\note{

    \bold{Generalized Extreme Value Distribution:}
    \cr\cr
    Here we implemented the notation for the arguments as used
    by the GEV functions in the EVIS package or SPlus/FinMetrics 
    module. Additional arguments to these packages are the \code{log} 
    and the \code{lower.tail} arguments, underlying the code
    from R's \code{evd} package.
    \cr
    An alternative usage is proposed by the \code{evd} package.
    There the following arguments are used:
    \cr
    \code{*gev(x, loc = 0, scale = 1, shape = 0, ...)}
    \cr   
    What you prefer is a matter of taste. The GEV functions from
    the \code{evd} package are renamed from \code{*gev} to \code{*evd}
    so that both versions are available. 
    \cr   
    In addition functions for the density, probability, quantiles, 
    and the generation of random variates for the 
    Frechet \code{[dpqr]frechet},
    Gumbel \code{[dpqr]gumbel}, and
    Weibull \code{[dpqr]weibull} are also available.
    \cr   
    If you stay with both versions you can access the help page for 
    \code{evds}'s function \code{dgev} by \code{help(dgev, package="evd")}.
    
}


\references{

Coles S. (2001);
    \emph{Introduction to Statistical Modelling of Extreme Values},
    Springer.
    
Embrechts, P., Klueppelberg, C., Mikosch, T. (1997);
    \emph{Modelling Extremal Events}, 
    Springer.
    
}


\author{
  
    Alec Stephenson for R's \code{evd} and \code{evir} package, and \cr
    Diethelm Wuertz for this \R-port.
    
}

  
\examples{
\dontrun{
## SOURCE("fExtremes.052A-GevModelling")

## *gev  -
   xmpExtremes("\nStart: GEV Frechet >")
   # Create and plot 1000 GEV/Frechet distributed rdv:
   par(mfrow = c(3, 3))
   r = rgev(n = 1000, xi = 1)
   plot(r, type = "l", main = "GEV/Frechet Series")
   ## Plot empirical density and compare with true density:
   ## Omit values greater than 500 from plot
   hist(r[r<10], n = 25, probability = TRUE, xlab = "r", 
     xlim = c(-5, 5), ylim = c(0, 1.1), main = "Density")
   x = seq(-5, 5, by=0.01)
   lines(x, dgev(x, xi = 1), col = 2)
   ## Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
     cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   q = seq(-5,5, by=0.1)
   lines(q, pgev(q, xi=1), col=2)
## Compute quantiles, a test:
   qgev(pgev(seq(-5, 5, 0.25), xi = 1), xi = 1) 
   
## *gev -
   xmpExtremes("\nNext: GEV Gumbel >")
   # Create and plot 1000 Gumbel distributed rdv:
   ##> r = rgev(n = 1000, xi = 0)
   ##> plot(r, type = "l", main = "Gumbel Series")
   ## Plot empirical density and compare with true density:
   ##>hist(r[abs(r)<10], nclass = 25, freq = FALSE, xlab = "r", 
   ##>   xlim = c(-5,5), ylim = c(0,1.1), main = "Density")
   ##>x = seq(-5, 5, by = 0.01)
   ##>lines(x, dgev(x, xi = 0), col=2)
   ## Plot df and compare with true df:
   ##>plot(sort(r), (1:length(r)/length(r)), 
   ##>  xlim = c(-3, 6), ylim = c(0, 1.1),
   ##>   cex=0.5, ylab = "p", xlab="q", main="Probability")
   ##>q = seq(-5, 5, by = 0.1)
   ##>lines(q, pgev(q, xi = 0), col = 2)
   ## Compute quantiles, a test:
   ##>qgev(pgev(seq(-5, 5, 0.25), xi = 0), xi = 0)   

## *gev -
   xmpExtremes("\nNext: GEV Weibull >")
   # Create and plot 1000 Weibull distributed rdv:
   r = rgev(n = 1000, xi = -1)
   plot(r, type = "l", main = "Weibull Series")
   ## Plot empirical density and compare with true density:
   hist(r[abs(r)<10], nclass = 25, freq = FALSE, xlab = "r", 
     xlim=c(-5,5), ylim=c(0,1.1), main="Density")
   x = seq(-5, 5, by=0.01)
   lines(x, dgev(x, xi = -1), col = 2)
   ## Plot df and compare with true df:
   plot(sort(r), (1:length(r)/length(r)), 
     xlim = c(-3, 6), ylim = c(0, 1.1),
     cex = 0.5, ylab = "p", xlab = "q", main = "Probability")
   q=seq(-5, 5, by = 0.1)
   lines(q, pgev(q, xi = -1), col = 2)
   ## Compute quantiles, a test:
   qgev(pgev(seq(-5, 5, 0.25), xi = -1), xi = -1)   
}
}


\keyword{models}

