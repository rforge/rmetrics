\name{HypothesisTesting}

\alias{HypothesisTesting}

\alias{fHTEST}
\alias{fHTEST-class}

\alias{show.fHTEST}
\alias{show,fHTEST-method}

\alias{pPlot}
\alias{pTable}
\alias{qTable}



\title{Tests Class Representation and Utilities}


\description{
    
    Class representation, methods and utility 
    functions for objects of class 'fHTEST'.
    \cr
    
    The class representation and methods are:

    \tabular{ll}{
    \code{fHTEST} \tab Representation for an S4 object of class "fHTEST", \cr
    \code{show} \tab S4 print method. }
    
    Utility Functions:
    \tabular{ll}{
    \code{pPlot} \tab  General finite sample probability plot, \cr
    \code{pTable} \tab interpolated probabilities from finite sample table, \cr
    \code{qTable} \tab interpolated quantiles from finite sample table. }
         
}


\usage{
show.fHTEST(object)

pPlot(X, nN = 100, nStat = 100, logN = TRUE, logStat = FALSE, 
    fill = FALSE, linear = TRUE, digits = 8, doplot = TRUE, \dots)
pTable(X, Stat, N, digits = 4)
qTable(X, p, N, digits = 4)
}


\arguments{
  
    \item{digits}{
        an integer value with the number of rounding digits.
        }
    \item{doplot}{
        a logical flag. Should a plot be displayed?
        }
    \item{fill}{
        [pPlot] - \cr
        a logical flag deciding if missing data should be filled
        with asymptotic values zero and one.
        }
    \item{linear}{
        [pPlot] - \cr
        a logical flag indicating the type of interpolation.
        }
    \item{logN, logStat}{
        [pPlot] - \cr
        two logical flags deciding if the data should be on a 
        logarithmic scale or not.
        }
    \item{N}{
        an integer value or vector of sample sizes.
        }
    \item{nN, nStat}{
        [pPlot] - \cr
        two integer values with the size of the table.
        }
    \item{object}{
        [show] - \cr
        an S4 object of class \code{"fHTEST"}.
        }
    \item{p}{
        a numeric value or vector of probabilities.
        }
    \item{Stat}{
        a numeric value or vector of quantiles or statistic values.
        }
    \item{X}{
        [pPlot][*Table] - \cr
        a data frame or matrix of a finite sample test table.
        }
    \item{\dots}{
        [pPlot] - \cr
        additional arguments to be passed.
        }
       
}


\value{

    In contrast to R's output report from S3 objects of class \code{"htest"}
    a different output report is produced. The tests return an S4 object 
    of class \code{"fHTEST"}. The object contains the following slots:
    
    \item{@call}{
        the function call.  
        }
    \item{@data}{
        the data as specified by the input argument(s).
        }
    \item{@test}{
        a list whose elements contail the results from the statistical
        test. The information provided is similar to a list object of
        class{"htest"}.
        }
    \item{@title}{
        a character string with the name of the test. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{@description}{
        a character string with an optional user defined description. 
        By default just the current date when the test was applied will
        be returned.}
    
    
    The slot \code{@test} returns an object of class \code{"list"} 
    containing the following elements:

    \item{statistic}{
        the value(s) of the test statistic.
        }
    \item{p.value}{
        the p-value(s) of the test.
        }
    \item{parameters}{
        a numeric value or vector of parameters.
        }
    \item{estimate}{
        a numeric value or vector of sample estimates.
        }
    \item{conf.int}{
        a numeric two row vector or matrix of 95% confidence levels.
        }
    \item{method}{
        a character string indicating what type of test was performed.
        }
    \item{data.name}{
        a character string giving the name(s) of the data.
        }
        
     The functions \code{pPlot}, \code{pTable}, and \code{qTable}
     plot and iterpolate finite sample test statistic data from a 
     table. The table is a data frame or a matrix where columns
     denote the size and rows the probabilities. The column and 
     row names must hold the sizes and probabilities as character
     strings. The values of the matrix hold the statistic values.
     \cr
}



\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fBasics.5C-HypothesisTesting")
\dontrun{
## fHTEST -
   getClass("fHTEST")

## pPlot -
## jbTable -
   # Interpolated plot of Small Jarque Bera Table:
   X = jbTable(type = "LM", size = "small")
   pPlot(X, linear = TRUE, logStat = TRUE)
   pPlot(X, linear = TRUE, logStat = TRUE, fill = TRUE, main = "JB LM")
   pPlot(X, linear = FALSE, logStat = TRUE)
   pPlot(X, linear = FALSE, logStat = TRUE, fill = TRUE)
    
## [pq]Table -
## jbTable - 
   # Jarque Bera B q and p Table:
   X = jbTable(type = "LM", size = "small")
   p = (1:99)/100
   plot(qTable(X, p, N = 100), p, type = "b")
   Stat = seq(0.01, 15, length = 100)
   plot(Stat, pTable(X, Stat, N = 100), type = "b")  
}
}


\keyword{htest}

