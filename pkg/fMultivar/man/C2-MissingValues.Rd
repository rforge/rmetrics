\name{MissingValues}

\alias{MissingValues}

\alias{removeNA}
\alias{substituteNA}
\alias{interpNA}
\alias{knnNA}



\title{Handling Missing Values}


\description{
    
    A collection and description of functions 
    for handling missing values in 'timeSeries' 
    objects or in objects which can be transformed 
    into a vector or a two dimensional matrix.
    \cr
    
    The functions are listed by topic.
    \cr
 
    
    \tabular{ll}{  
	\code{removeNA} \tab Removes NAs from a matrix object, \cr
	\code{substituteNA} \tab substitute NAs by zero, the column mean or median, \cr
	\code{interpNA} \tab interpolates NAs using R's "approx" function, \cr
	\code{knnNA} \tab imputes NAs by the "knn"-Algorithm from R's EMV package. }

}


\usage{
removeNA(x, ...)
substituteNA(x, type = c("zeros", "mean", "median"), ...)
interpNA(x, method = c("linear", "before", "after"), ...)
knnNA(x, k = max(dim(as.matrix(x))[1]*0.01,2), correlation = FALSE, ...)

}


\arguments{

	\item{correlation}{
		[knnNA] - \cr
		a logical value, if TRUE the selection of the neighbours is based 
		on the sample correlation. The neighbours with the highest 
		correlations are selected.
	}
	\item{k}{
		[knnNA] - \cr
		the number of neighboors (rows) to estimate the missing values.
		}
	\item{method}{
	 	[interpNA] - \cr
	 	Specifies the method how to interpolate the matrix column
	 	by column. One of the applied vector strings: 
	 	\code{method="linear"}, \code{method="before"} or 
	 	\code{method="after"}.
    	For the  interpolation the function \code{approx} is used.
	 	}
	\item{type}{
	 	[substituteNA] - \cr
	 	Three alternative methods are provided to remove NAs from the
	 	data: 
	 	\code{type="zeros"} replaces the missing values by zeros,
	 	\code{type="mean"} replaces the missing values by the column mean,
	 	\code{type="median"} replaces the missing values by the the column
	 	median.
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
	
	The function \code{knnNA} estimates missing values of a timeSeries 
	object or of a matrix based on a k-th neighbours algorithm. Missing 
	values can be either -Inf, Inf, NA, or NaN. 
	Based on the Euclidian distance, the algorithm selects the k-th 
	nearest rows (that do not contain any missing values) to the one 
	containing at least one missing value, based on the Euclidian distance 
	or the sample correlation. Then the missing values are replaced by the 
	average of the neighbours. Note, that if a row only contains missing 
	values then the estimation is not possible.\cr
	[EMV:knn].
	
	\bold{Missing Values in Return Series:}
	
	For return series the function \code{substituteNA} may be useful. The 
	function allows to fill missing values either by \code{method="zeros"}, 
	the \code{method="mean"} or the \code{method="median"} value of the 
	appropriate columns.
	
}


\references{

Troyanskaya O., Cantor M., Sherlock G., Brown P., Hastie T., 
Tibshirani R., Botstein D., Altman R.B., (2001); 
	\emph{Missing Value Estimation Methods for DNA microarrays}
	Bioinformatics 17, 520--525.

}


\examples{
## SOURCE("fMultivar.C2-MissingValues")

## Create a Matrix with NAs:
   X = matrix(rnorm(100), ncol = 5)
   # a single NA inside:
   X[3, 5] = NA
   # three in a row inside:
   X[17, 2:4] = c(NA, NA, NA)
   # three in a column inside:
   X[13:15, 4] = c(NA, NA, NA)
   # two at the right border:
   X[11:12, 5] = c(NA, NA)
   # one in the lower left corner:
   X[20, 1] = NA
   print(X)
     
## Remove rows with NA's
   removeNA(X)
   # Now we have only 12 lines!
   
## Subsitute NA's by zeros or column mean
   substituteNA(X, type = "zeros")
   substituteNA(X, type = "mean")
   
## Interpolate NA's liearily:
   interpNA(X, method = "linear")
   # Note the corner missing value cannot be interpolated!
   # Take previous values in a column:
   interpNA(X, method = "before")
   # Also here, the corner value is excluded
   
## Interpolate using the knn Algorithm:
   knnNA(X)
}


\author{

   	Raphael Gottardo for the \code{knn} function, \cr
   	Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{math}

