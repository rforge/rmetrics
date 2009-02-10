\name{fPFOLIOBACKTEST-class}


\alias{fPFOLIOBACKTEST}
\alias{class-fPFOLIOBACKTEST}

\alias{fPFOLIOBACKTEST-class}

\alias{show,fPFOLIOBACKTEST-method}


\title{Specification of portfolio backtesting}


\description{
  
    Specifies portfolio backtesting object.
    
}


\usage{       
\S4method{show}{fPFOLIOBACKTEST}(object)
}


\arguments{

    \item{object}{
        an S4 object of class \code{fPFOLIOBACKTEST}.
        }

}       

\details{

    \bold{Portfolio Backtest Structure:}
    \cr\cr
    
    The S4 class \code{fPFOLIOBACKTEST} specifies portfolio backtesting. 
    The slots are:\cr
    
    \describe{
        \item{@windows}{
            a list, setting the \code{windows} function that defines the 
            rolling windows, 
	        and the set of window specific parameters \code{params}.  
	        E.g The window horizon
	        is set as a parameter \code{horizon = "24m"}
            }
    	\item{@strategy}{
    	    a list, setting the portfolio \code{strategy} to implement 
    	    during the backtest, and any strategy specific parameters 
    	    are found in \code{params}.
    		}
    	\item{@smoother}{
    	    a list, specifying the smoothing style, given as a 
    	    \code{smoother} function, and any smoother specific parameters 
    	    are stored in the list \code{params}.
    		}
    	\item{@messages}{
    	    a list, any messages collected during the backtest}
            }
 
}


\value{
  
    \code{portfolioBacktest}\cr
    \cr
    returns an S4 object of class \code{"fPFOLIOBACKTEST"}.

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}




