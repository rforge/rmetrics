\name{NonLinModelling}

\alias{NonLinModelling}

\alias{tentSim}
\alias{henonSim}
\alias{ikedaSim}
\alias{logisticSim}
\alias{lorentzSim}
\alias{roesslerSim}


\title{Chaotic Time Series Modelling}


\description{

    A collection and description of functions to 
    simulate different types of chaotic time series 
    maps.
    \cr
    
    Chaotic Time Series Maps:
        
    \tabular{ll}{
    \code{tentSim} \tab Simulates data from the Tent Map, \cr
    \code{henonSim} \tab simulates data from the Henon Map, \cr
    \code{ikedaSim} \tab simulates data from the Ikeda Map, \cr
    \code{logisticSim} \tab simulates data from the Logistic Map, \cr
    \code{lorentzSim} \tab simulates data from the Lorentz Map, \cr
    \code{roesslerSim} \tab simulates data from the Roessler Map. }  
    
}


\usage{
tentSim(n = 1000, n.skip = 100, parms = c(a = 2), start = runif(1), 
    doplot = FALSE)
henonSim(n = 1000, n.skip = 100, parms = c(a = 1.4, b = 0.3), 
    start = runif(2), doplot = FALSE)
ikedaSim(n = 1000, n.skip = 100, parms = c(a = 0.4, b = 6.0, c = 0.9), 
    start = runif(2), doplot = FALSE)
logisticSim(n = 1000, n.skip = 100, parms = c(r = 4), start = runif(1), 
    doplot = FALSE)
lorentzSim(times = seq(0, 40, by = 0.01), parms = c(sigma = 16, r = 45.92, 
    b = 4), start = c(-14, -13, 47), doplot = TRUE, \dots)
roesslerSim(times = seq(0, 100, by = 0.01), parms = c(a = 0.2, b = 0.2, c = 8.0),
    start = c(-1.894, -9.920, 0.0250), doplot = TRUE, \dots) 
}


\arguments{
  
    \item{doplot}{
        a logical flag. Should a plot be displayed?
        }
    \item{n, n.skip}{
        [henonSim][ikedaSim][logisticSim] - \cr
        the number of chaotic time series points to be generated and the 
        number of initial values to be skipped from the series. 
        }
    \item{parms}{
        the named parameter vector characterizing the chaotic map.
        }
    \item{start}{
        the vector of start values to initiate the chaotic map.
        }
    \item{times}{
        [lorentzSim][roesslerSim] - \cr
        the sequence of time series points at which to generate the map. 
        }
    \item{\dots}{
        arguments to be passed.
        }

}

    
\value{

    [*Sim] - \cr
    All functions return invisible a vector of time series data. 
        
}


\references{

Brock, W.A., Dechert W.D., Sheinkman J.A. (1987); 
    \emph{A Test of Independence Based on the Correlation 
    Dimension}, 
    SSRI no. 8702, Department of Economics, University of 
    Wisconsin, Madison.

Eckmann J.P., Oliffson Kamphorst S., Ruelle D. (1987), 
    \emph{Recurrence plots of dynamical systems}, 
    Europhys. Letters 4, 973.
    
Hegger R., Kantz H., Schreiber T. (1999);
    \emph{Practical implementation of nonlinear time series 
    methods: The TISEAN package},
    CHAOS 9, 413--435.

Kennel M.B., Brown R., Abarbanel H.D.I. (1992); 
    \emph{Determining embedding dimension for phase-space 
    reconstruction using a geometrical construction}, 
    Phys. Rev. A45, 3403. 
    
Rosenstein M.T., Collins J.J., De Luca C.J. (1993);
    \emph{A practical method for calculating largest Lyapunov 
    exponents from small data sets}, 
    Physica D 65, 117.
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\seealso{

    \code{RandomInnovations}.
    
}


\examples{
## NA -
                  
}


\keyword{models}

