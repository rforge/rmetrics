\name{ChaosStatistics}

\alias{ChaosStatistics}


\alias{mutualPlot}
\alias{falsennPlot}
\alias{recurrencePlot}
\alias{separationPlot}
\alias{lyapunovPlot} 


\title{Chaotic Time Series Statistics}


\description{

    A collection and description of functions to 
    investigate the chaotic behavior of time series 
    processes. 
    \cr
    
    Functions to Analyse Chaotic Time Series:
    
    \tabular{ll}{
    \code{mutualPlot} \tab Returns mutual information, \cr
    \code{falsennPlot} \tab returns false nearest neigbours, \cr
    \code{recurrencePlot} \tab returns a recurrence plot, \cr
    \code{separationPlot} \tab returns a space-time separation plot, \cr
    \code{lyapunovPlot} \tab computes maximum lyapunov exponent. }
    
}


\usage{   
mutualPlot(x, partitions = 16, lag.max = 20, doplot = TRUE, \dots)  
falsennPlot(x, m, d, t, rt = 10, eps = NULL, doplot = TRUE, \dots) 
recurrencePlot(x, m, d, end.time, eps, nt = 10, doplot = TRUE, \dots)
separationPlot(x, m, d, mdt, idt = 1, doplot = TRUE, \dots)
lyapunovPlot(x, m, d, t, ref, s, eps, k = 1, doplot = TRUE, \dots) 
}


\arguments{
  
    \item{d}{
        an integer value setting the value of the time delay.
        }
    \item{eps}{
        [falsennPlot] - \cr
        a numeric value setting the value of the neighbour diameter.
        If NULL, which is the default value, then the value will be
        automatically setted to \code{eps=sd(x)/10}.
        \cr
        [lyapunovPlot] - \cr
        the radius where to find nearest neighbours.
        \cr
        [recurrencePlot] - \cr
        the neighbourhood threshold.
        }
    \item{doplot}{
        a logical flag. Should a plot be displayed?
        }
    \item{end.time}{
        [recurrencePlot] - \cr
        ending time as number of observations.
        }
    \item{idt}{
        [separationPlot] - \cr
        an integer value setting the number of observation steps in
        each iterations. By default 1.
        }
    \item{k}{
        [lyapunovPlot] - \cr
        an integer setting th enumber of considered neighbours.
        By default 1.
        }
    \item{lag.max}{
        [mutualPlot] - \cr
        an integer value setting the number of maximum lags, by 
        default 20.
        }
    \item{m}{
        [*Plot] - \cr
        an integer value setting the value of the maximum embedding 
        dimension.
        }
    \item{mdt}{
        [separationPlot] - \cr
        an integer value setting the number of iterations.
        }
    \item{nt}{
        [recurrencePlot] - \cr
        observations in each step which will be plotted, by default 10.
        Increasing \code{nt} reduces number of points plotted which
        is usefule especially with highly sampled data.
        }
    \item{rt}{
        [falsennPlot] - \cr
        an integer value setting the value for the escape factor. By
        default 10.
        }
    \item{partitions}{
        [mutualPlot] - \cr
        an integer value setting the number of bins, by default 16.
        }
    \item{ref}{
        [lyapunovPlot] - \cr
        the number of points to take into account.
        }
    \item{s}{ 
        [lyapunovPlot] - \cr
        the iterations along which follow the neighbours of each point.
        }
    \item{t}{
        [*Plot] - \cr
        an integer value setting the value for the Theiler window.
        }
    \item{x}{
        [*Plot] - \cr
        a numeric vector, or an object either of class 'ts' or
        of class 'timeSeries'.
        }
    \item{\dots}{
        arguments to be passed.
        }

}


\details{

    \bold{Phase Space Representation:}
    \cr\cr  
    The function \code{mutualPlot} estimates and plots the mutual 
    information index of a given time series for a specified number 
    of lags. The joint probability distribution function is estimated 
    with a simple bi-dimensional density histogram.
    \cr 
    The function \code{falsennPlot} uses the Method of false nearest 
    neighbours to help deciding the optimal embedding dimension.
    \cr
        
    \bold{Non-Stationarity:}
    \cr\cr
    The funcdtion \code{recurrencePlot} creates a recurrence plot as 
    proposed by Eckmann et al. [1987]. 
    \cr
    The function \code{separationPlot} creates a space-time separation
    plot qs introduced by Provenzale et al. [1992]. It plots the 
    probability that two points in the reconstructed phase-space have 
    distance smaller than epsilon in function of epsilon and of the 
    time between the points, as iso-lines at levels 10, 20, ..., 100
    percent levels. The plot can be used to decide the Theiler time 
    window.
    \cr
    
    \bold{Lyapunov Exponents:}
    \cr\cr
    The function \code{lyapunovPlot} evaluates and plots the largest 
    Lyapunov exponent of a dynamic system from a univariate time series.
    The estimate of the Lyapunov exponent uses the algorithm of Kantz.
    In addition, the function computes the regression coefficients of 
    a user specified segment of the sequence given as input. 
    \cr
    
    \bold{Dimensions and Entropies:}
    \cr\cr
    The function \code{C2} computes the sample correlation integral on 
    the provided time series for the specified length scale and 
    Theiler window. It uses a naiv algorithm: simply returns the 
    fraction of points pairs nearer than eps. It is prefarable to use 
    the function \code{d2}, which takes roughly the same time, but 
    computes the correlation sum for multiple length scales and 
    embedding dimensions at once.
    \cr
    The function \code{d2} computes the sample correlation integral 
    over given length scales \code{neps} for embedding dimensions 
    \code{1:m} for a given Theiler window. The slope of the linear 
    segment in the log-log plot gives an estimate of the correlation 
    dimension.

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

