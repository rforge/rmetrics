\name{GevModelling}

\alias{GevModelling}

\alias{dgev}
\alias{pgev}
\alias{qgev}
\alias{rgev}

\alias{devd}
\alias{pevd}
\alias{qevd}
\alias{revd}

\alias{fGEV}
\alias{fGEV-class}

\alias{gevSim}
\alias{gevFit}
\alias{print.fGEV}
\alias{plot.fGEV}
\alias{summary.fGEV}
\alias{gevrlevelPlot}

\alias{hillPlot}
\alias{shaparmPlot}
\alias{shaparmPickands}
\alias{shaparmHill}
\alias{shaparmDEHaan}



\title{Generalized Extreme Value Modelling}


\description{
  
    A collection and description functions to compute
    the generalized extreme value distribution and to
    estimate it parameters. The functions compute 
    density, distribution function, quantile function 
    and generate random deviates for the GEV, for the 
    Frechet, Gumbel, and Weibull distributions. To model
    the GEV three types of approaches for parameter 
    estimation are provided: Maximum likelihood
    estimation, probability weighted moment method,
    and estimation by the MDA approach. MDA includes
    functions for the Pickands, Einmal-Decker-deHaan, 
    and Hill estimators together with several plot 
    variants.
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
    \code{[dpqr]evd} \tab an alternative call for the GEV Distribution. }
    
    The GEV modelling functions are:
    
    \tabular{ll}{
    \code{gevSim} \tab generates data from the GEV, \cr
    \code{gevFit} \tab fits empirical or simulated data to the distribution, \cr 
    \code{print} \tab print method for a fitted GEV object, \cr
    \code{plot} \tab plot method for a fitted GEV object, \cr 
    \code{summary} \tab summary method for a fitted GEV object, \cr
    \code{gevrlevelPlot} \tab k-block return level with confidence intervals. }
    
    Maximum Domain of Attraction estimators:
    
    \tabular{ll}{
    \code{hillPlot} \tab shape parameter and Hill estimate of the tail index, \cr
    \code{shaparmPlot} \tab variation of shape parameter with tail depth. }
     
}


\usage{
dgev(x, xi = 1, mu = 0, sigma = 1, log = FALSE)
pgev(q, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
qgev(p, xi = 1, mu = 0, sigma = 1, lower.tail = TRUE)
rgev(n, xi = 1, mu = 0, sigma = 1)

devd(x, loc = 0, scale = 1, shape = 0, log = FALSE)
pevd(q, loc = 0, scale = 1, shape = 0, lower.tail = TRUE)
qevd(p, loc = 0, scale = 1, shape = 0, lower.tail = TRUE)
revd(n, loc = 0, scale = 1, shape = 0)

gevSim(model = list(shape = -0.25, location = 0, scale = 1), n = 1000, seed = NULL)
gevFit(x, type = c("mle", "pwm"), gumbel = FALSE, title = NULL, description = NULL, \dots)
\method{print}{fGEV}(x, \dots)
\method{plot}{fGEV}(x, which = "all", \dots)
\method{summary}{fGEV}(object, doplot = TRUE, which = "all", \dots)
gevrlevelPlot(object, k.blocks = 20, add = FALSE, \dots)

hillPlot(x, option = c("alpha", "xi"), start = 15, ci = 0.95, 
    reverse = FALSE, doplot = TRUE, labels = TRUE, \dots) 
shaparmPlot(x, revert = FALSE, standardize = FALSE, tails = 0.01*(1:10), 
    doplot = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), 
    which = c(TRUE, TRUE, TRUE), both.tails = TRUE, doprint = TRUE, 
    xi.range = c(-0.5, 1.5), alpha.range = c(0, 10)) 
}


\arguments{

    \item{add}{
        [gevrlevelPlot] - \cr
        whether the return level should be added graphically to a
        time series plot; if \code{FALSE} a graph of the profile
        likelihood curve showing the return level and its confidence
        interval is produced.
        }
    \item{alpha.range, xi.range}{
        [saparmPlot] - \cr
        plotting ranges.
        }
    \item{autoscale}{
        [hillPlot] - \cr
        whether or not plot should be automatically
        scaled; if not, \code{xlim} and \code{ylim} graphical
        parameters may be entered.
        }
    \item{both.tails}{
        [shaparmPlot] - \cr
        a logical, decides whether or not both tails should be 
        investigated. By default TRUE. If FALSE only the lower
        tail will be investigated.
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
    \item{doprint}{
        [shaparmPlot] - \cr
        a logical, decides whether or not for all tail depths the
        result for the shape parameter \code{1/alpha} should be
        printed.
        }
    \item{gumbel}{
        [gevFit] - \cr
        a logical, by default FALSE. To fit a Gumbel model with fixed
        \code{shape=0} set \code{gumbel=TRUE}.
        }
    \item{k.blocks}{
        [gevrlevelPlot] - \cr
        specifies the particular return level to be estimated; default 
        set arbitrarily to 20.
        }
    \item{labels}{
        [hillPlot] - \cr
        whether or not axes should be labelled.
        }
    \item{loc, scale, shape}{
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
        [gevSim] - \cr
        a list with components \code{shape}, \code{location} and 
        \code{scale} giving the parameters of the GEV distribution.
        By default the shape parameter has the value -0.25, the
        location is zero and the scale is one.
        To fit random deviates from a Gumbel distribution set 
        \code{shape=0}.
        }
    \item{n}{
        [gevSim] - \cr
        number of generated data points, an integer value.
        \cr
        [rgev][revd] - \cr        
        the number of observations.
        } 
    \item{object}{
        [summary][grlevelPlot] - \cr
        a fitted object of class \code{"gevFit"}.
        }
    \item{option}{
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
    \item{reverse}{
        [hillPlot] - \cr
        whether plot is to be by increasing threshold, \code{TRUE}, or 
        increasing number of order statistics \code{FALSE}.
        }
    \item{revert}{
        [shaparmPlot] - \cr
        a logical value, by default FALSE, if set to TRUE the
        sign of the vector will be reverted: \code{x = -x}.
        }
    \item{seed}{
        [gevSim] - \cr
        an integer value to set the seed for the random number generator.
        }
    \item{start, end}{
        [hillPlot] - \cr
        lowest and highest number of order statistics at which to plot
        a point.
        }
    \item{standardize}{
        [shaparmPlot] - \cr
        a logical value, by default FALSE, if set to 
        TRUE the vector \code{x} will be standardized: 
        \code{x = (x-mean(x))/sqrt(var(x))}.
        }
    \item{tails}{
        [shaparmPlot] - \cr
        a numeric vector of tail depths to be considered; by
        default ten values ranging from 0.1 to 1.0 in steps of 0.1
        corresponding to values ranging from 1 to 10 percent.
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
        [shaparmPlot] - \cr
        a vector of 3 logicals indicating which plots from the 
        three methods will be created. The first entry decides
        for the Pickands estimator, the second for the Hill 
        estimator, and the last for the Deckers-Einmahl-deHaan
        estimator. By default all three will be created.
        By default \code{c(TRUE, TRUE, TRUE)}.
        \cr
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
    \cr

    \code{gevSim}
    \cr
    returns a vector of data points from the simulated series.
    \cr

    \code{gevFit}
    \cr
    returns an object of class \code{gev} describing the fit.
    \cr
  
    \code{print.summary}
    \cr
    prints a report of the parameter fit.
    \cr
    
    \code{summary}
    \cr
    performs diagnostic analysis. The method provides two different 
    residual plots for assessing the fitted GEV model.  
    \cr
  
    \code{gevrlevelPlot}
    \cr
    returns a vector containing the lower 95\% bound of the confidence 
    interval, the estimated return level and the upper 95\% bound. 
    \cr
    
    \code{hillPlot}
    \cr
    displays a plot.
    \cr
   
    \code{shaparmPlot} 
    \cr
    returns a list with one or two entries, depending on the
    selection of the input variable \code{both.tails}. The two 
    entries \code{upper} and \code{lower} determine the position of 
    the tail. Each of the two variables is again a list with entries 
    \code{pickands}, \code{hill}, and \code{dehaan}. If one of the 
    three methods will be discarded the printout will display zeroes.
    
}


\details{

    \bold{Generalized Extreme Value Distribution:}
    \cr\cr
    Computes density, distribution function, quantile function and 
    generates random variates for the Generalized Extreme Value 
    Distribution, GEV, for the Frechet, Gumbel, and Weibull 
    distributions.
    \cr
        
    \bold{Parameter Estimation:}
    \cr\cr
    \code{gevFit} estimates the parameters either by the probability 
    weighted moment method, \code{method="pwm"} or by maximum log 
    likelihood estimation \code{method="mle"}. 
    As a limiting case the Gumbel distribution can be selected. The 
    summary method produces diagnostic plots for fitted GEV or Gumbel 
    models.
    \cr
  
    \bold{Methods:}
    \cr\cr
    \code{print.gev}, \code{plot.gev} and \code{summary.gev} are
    print, plot, and summary methods for a fitted object of class 
    \code{gev}. Concerning the summary method, the data are 
    converted to unit exponentially distributed residuals under null 
    hypothesis that GEV fits. Two diagnostics for iid exponential data 
    are offered. The plot method provides two different residual plots 
    for assessing the fitted GEV model. Two diagnostics for 
    iid exponential data are offered. 
    \cr
  
    \bold{Return Level Plot:}
    \cr\cr
    \code{gevrlevelPlot} calculates and plots the k-block return level 
    and 95\% confidence interval based on a GEV model for block maxima, 
    where \code{k} is specified by the user. The k-block return level 
    is that level exceeded once every \code{k} blocks, on average. The 
    GEV likelihood is reparameterized in terms of the unknown return 
    level and profile likelihood arguments are used to construct a 
    confidence interval. 
    \cr
    
    \bold{Hill Plot:}
    \cr\cr
    The function \code{hillPlot} investigates the shape parameter and 
    plots the Hill estimate of the tail index of heavy-tailed data, or 
    of an associated quantile estimate. This plot is usually calculated 
    from the alpha perspective. For a generalized Pareto analysis of 
    heavy-tailed data using the \code{gpdFit} function, it helps to 
    plot the Hill estimates for \code{xi}. 
    \cr
  
    \bold{Shape Parameter Plot:}
    \cr\cr
    The function \code{shaparmPlot} investigates the shape parameter and 
    plots for the upper and lower tails the shape parameter as a function 
    of the taildepth. Three approaches are considered, the \emph{Pickands} 
    estimator, the \emph{Hill} estimator, and the
    \emph{Decker-Einmal-deHaan} estimator.
    
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
    \cr
    
    \bold{Generalized Extreme Value Distribution:}
    \cr\cr
    If method \code{"mle"} is selected the parameter fitting in \code{gevFit} 
    is passed to the internal function \code{gev.mle} or \code{gumbel.mle}
    depending on the value of \code{gumbel}, \code{FALSE} or \code{TRUE}.
    On the other hand, if method \code{"pwm"} is selected the parameter 
    fitting in \code{gevFit} is passed to the internal function 
    \code{gev.pwm} or \code{gumbel.pwm} again depending on the value of 
    \code{gumbel}, \code{FALSE} or \code{TRUE}.
        
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

## gevSim -
## gevFit -
   # Simulate GEV Data:
   xmpExtremes("\nStart: Simulte GEV Sample >")
   # Use default length n=1000
   x = gevSim(model = list(shape = 0.25, location =0 , scale = 1))
   # Fit GEV Data by Probability Weighted Moments:
   fit = gevFit(x, type = "pwm") 
   print(fit)
   # Summarize Results:
   par(mfcol = c(3, 2))
   summary(fit)
   
## gevFit -
   # Fit GEV Data by Max Log Likelihood Method:
   xmpExtremes("\nNext: Estimate Parameters >")
   fit = gevFit(x, type = "mle") 
   print(fit)
   # Summarize Results:
   summary(fit) 
    
## gevSim -
## gevFit -
   # Simulate Gumbel Data:
   xmpExtremes("\nNext: Simulte Gumbel Sample >")
   # Use default length n=1000
   ##> x = gevSim(model = list(shape = 0, location = 0, scale = 1))
   # Fit Gumbel Data by Probability Weighted Moments:
   ##> fit = gevFit(x, type = "pwm", gumbel = TRUE) 
   ##> print(fit)
   # Summarize Results:
   ##> par(mfcol = c(3, 2))
   ##> summary(fit)  
   
## Fit Gumbel Data by Max Log Likelihood Method:
   xmpExtremes("\nNext: Estimate Parameters >")
   ##> fit = gevFit(x, type = "mle", gumbel = TRUE) 
   ##> print(fit)
   # Summarize Results:
   ##> summary(fit)     
   ##> xmpExtremes("Press any key to continue >") 
   
## Return levels based on GEV Fit:
   # BMW Stock Data:
   xmpExtremes("\nNext: Compute BMW Return Levels >")
   par(mfrow = c(2, 1))
   data(bmw)
   # Fit GEV to monthly Block Maxima:
   fit = gevFit(-bmw, block = "month") 
   # Calculate the 40 month return level
   gevrlevelPlot(fit, k.block = 40, main = "BMW: Return Levels")   
   
## Return levels based on GEV Fit:
   xmpExtremes("\nNext: Compute SIEMENS Return Levels >")
   # Siemens Stock Data:
   data(siemens)
   # Fit GEV to monthly Block Maxima:
   fit = gevFit(-siemens, block = "month") 
   # Calculate the 40 month return level
   gevrlevelPlot(fit, k.block = 40, main = "SIEMENS: Return Levels")
   
## Interactive Plot:
   ##> par(mfrow = c(1, 1), ask = TRUE)
   ##> plot(fit)
   
## hillPlot -
   xmpExtremes("\nStart: Hill Estimator >")
   # Hill plot of heavy-tailed Danish fire insurance data 
   # and BMW stock data for estimated 0.999 quantile
   par(mfrow = c(2, 2))
   data(bmw)
   hillPlot(bmw)
   hillPlot(bmw, option = "quantile", end = 500, p = 0.999)
   data(danish)
   hillPlot(danish)
   hillPlot(danish, option = "quantile", end = 500, p = 0.999)
   
## shaparmPlot -
   xmpExtremes("\nNext: Shape Parameter Plots >")
   par(mfcol = c(3, 2), cex = 0.6)
   data(bmw)
   shaparmPlot(bmw)

## shaparmPlot -
   xmpExtremes("\nNext: Simulated Frechet Data >")
   par(mfcol = c(3, 2), cex = 0.6)
   set.seed(4711)
   x = rgev(10000, xi = 1/4)
   shaparmPlot(x, revert = TRUE, both.tails = FALSE)
   lines(c(0.01, 0.1), c(4, 4), col = "steelblue3") # True Value
}
}


\keyword{models}

