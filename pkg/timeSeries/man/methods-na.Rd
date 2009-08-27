\name{na}


\title{Handling Missing Time Series Values}


\alias{na}
\alias{na.omit}
\alias{na.omit,timeSeries-method}
\alias{na.omit.timeSeries}

\alias{removeNA}
\alias{substituteNA}
\alias{interpNA}


\description{
    
    Functions for handling missing values in 'timeSeries' 
    objects or in objects which can be transformed 
    into a vector or a two dimensional matrix.
    \cr
    
    The functions are listed by topic.
    \cr
    
    \tabular{ll}{  
    \code{na.omit} \tab Handles NAs, \cr
    \code{removeNA} \tab Removes NAs from a matrix object, \cr
    \code{substituteNA} \tab substitute NAs by zero, the column mean or median, \cr
    \code{interpNA} \tab interpolates NAs using R's "approx" function. }

}


\usage{
\S4method{na.omit}{timeSeries}(object, method = c("r", "s", "z", "ir", "iz", "ie"), 
    interp = c("before", "linear", "after"), \dots)

removeNA(x, \dots)
substituteNA(x, type = c("zeros", "mean", "median"), \dots)
interpNA(x, method = c("linear", "before", "after"), \dots)
}


\arguments{

    \item{interp, type}{
        [nna.omit][substituteNA] - \cr
        Three alternative methods are provided to remove NAs from the
        data: 
        \code{type="zeros"} replaces the missing values by zeros,
        \code{type="mean"} replaces the missing values by the column mean,
        \code{type="median"} replaces the missing values by the the column
        median.
        }
    \item{method}{
        [na.omit] - \cr
        Specifies the method how to handle NAs. 
        One of the applied vector strings: \cr
        \code{method="s"} na.rm = FALSE, skip, i.e. do nothing, 
        \code{method="r"} remove NAs, 
        \code{method="z"} substitute NAs by zeros, 
        \code{method="ir"} interpolate NAs and 
            remove NAs at the beginning and end of the series, 
        \code{method="iz"} interpolate NAs and 
            substitute NAs at the beginning and end of the series, 
        \code{method="ie"} interpolate NAs and 
            extrapolate NAs at the beginning and end of the series, 
        [interpNA] - \cr
        Specifies the method how to interpolate the matrix column
        by column. One of the applied vector strings: 
        \code{method="linear"}, \code{method="before"} or 
        \code{method="after"}.\cr
        For the  interpolation the function \code{approx} is used.
        }
    \item{object}{ 
        an object of class("timeSeries").
        }
    \item{x}{
        a numeric matrix, or any other object which can be transformed
        into a matrix through \code{x = as.matrix(x, ...)}. If \code{x}
        is a vector, it will be transformed into a one-dimensional matrix.
        }
    \item{\dots}{
        arguments to be passed to the function \code{as.matrix}.
        }
}


\details{

    \bold{Missing Values in Price and Index Series:}
    
    Applied to \code{timeSeries} objects the function \code{removeNA}
    just removes rows with NAs from the series. For an interpolation
    of time series points one can use the function \code{interpNA}.
    Three different methods of interpolation are offered: \code{"linear"}
    does a linear interpolation, \code{"before"} uses the previous value,
    and \code{"after"} uses the following value. Note, that the 
    interpolation is done on the index scale and not on the time scale.
    
    \bold{Missing Values in Return Series:}
    
    For return series the function \code{substituteNA} may be useful. The 
    function allows to fill missing values either by \code{method="zeros"}, 
    the \code{method="mean"} or the \code{method="median"} value of the 
    appropriate columns.
    
}


\note{

    The functions \code{removeNA}, \code{substituteNA} and \code{interpNA} 
    are older implementations. Please use in all cases if possible the 
    new function \code{na.omit}.
    
}   


\references{

Troyanskaya O., Cantor M., Sherlock G., Brown P., Hastie T., 
    Tibshirani R., Botstein D., Altman R.B., (2001); 
    \emph{Missing Value Estimation Methods for DNA microarrays}
    Bioinformatics 17, 520--525.

}


\examples{
## Create a Matrix -
   X = matrix(rnorm(100), ncol = 5)
   
## Replace a Single NA Inside - 
   X[3, 5] = NA
   
## Replace Three in a Row Inside - 
   X[17, 2:4] = c(NA, NA, NA)
   
## Replace Three in a Column Inside - 
   X[13:15, 4] = c(NA, NA, NA)
   
## Replace Two at the Right Border - 
   X[11:12, 5] = c(NA, NA)
   
## Replace One in the Lower Left Corner - 
   X[20, 1] = NA
   print(X)
     
## Remove Rows with NAs - 
   removeNA(X)
   
## Subsitute NA's by Zeros or Column Mean - 
   substituteNA(X, type = "zeros")
   substituteNA(X, type = "mean")
   
## Interpolate NA's Linearily - 
   interpNA(X, method = "linear")
   # Note the corner missing value cannot be interpolated!
   
## Take Previous Values in a Column - 
   interpNA(X, method = "before")
   # Also here, the corner value is excluded
}


\keyword{math}

