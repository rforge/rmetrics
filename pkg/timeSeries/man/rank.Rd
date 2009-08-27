\name{rank}


\title{Sample Ranks of a Time Series}


\alias{rank,timeSeries-method}


\description{
    
    Return the sample ranks of the values of a 
    'timeSeries' object.
      
}


% \usage{
% \S4method{rank}{timeSeries}(x, na.last = TRUE, ties.method =
% c("average", "first", "random", "max", "min"))
% }


% \arguments{
        
%     \item{x}{
%         an univariate object of class \code{timeSeries}.
%         }
%     \item{\dots}{
%         arguments to be passed to the default rank function .
%         }  
       
% }


\value{

    returns the ranks of a \code{timeSeries} object
        
}


\examples{
## Load Microsoft Data -
   data(MSFT) 
   X = 100 * returns(MSFT)

## Compute the Ranks -
   head(rank(X[, "Open"]), 10)
   
## Only Interested in the Vector, then use -
   head(rank(series(X[, "Open"])), 10)
}


\keyword{chron}

