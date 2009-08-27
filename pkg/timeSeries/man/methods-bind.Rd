\name{bind}


\title{Bind two timeSeries objects}

\alias{merge}
\alias{cbind}
\alias{rbind}
\alias{cbind2}
\alias{rbind2}

% \alias{cbind,timeSeries-method} 
% \alias{rbind,timeSeries-method} 

\alias{cbind.timeSeries}
\alias{rbind.timeSeries}

% \alias{c.timeSeries}

\alias{merge,ANY,timeSeries-method}
\alias{merge,timeSeries,ANY-method}
\alias{merge,timeSeries,missing-method}
\alias{merge,timeSeries,numeric-method}
\alias{merge,timeSeries,matrix-method}
\alias{merge,timeSeries,timeSeries-method}
\alias{merge,matrix,timeSeries-method}
\alias{merge,numeric,timeSeries-method}
  
\alias{merge.timeSeries}

\alias{cbind2,ANY,timeSeries-method}
\alias{cbind2,timeSeries,ANY-method}
\alias{cbind2,timeSeries,missing-method}
\alias{cbind2,timeSeries,timeSeries-method}
\alias{rbind2,ANY,timeSeries-method}
\alias{rbind2,timeSeries,ANY-method}
\alias{rbind2,timeSeries,missing-method}
\alias{rbind2,timeSeries,timeSeries-method}


\description{
  
  Binds two 'timeSeries' objects either by column or row.        
  
}


% \usage{
% \S4method{merge}{timeSeries,timeSeries}(x, y, ...)
% \S4method{cbind2}{timeSeries,timeSeries}(x, y)
% \S4method{rbind2}{timeSeries,timeSeries}(x, y)
% \S4method{cbind2}{timeSeries,timeSeries}(x, y)
% }


% \arguments{
  
% %   \item{units}{
% %     an optional character string, which allows to overwrite the 
% %     current column names of a \code{timeSeries} object. By default 
% %     \code{NULL} which means that the column names are selected
% %     automatically.
% %   }

%   \item{x, y}{
%     two objects of class \code{timeSeries}.
%   }

% %   \item{\dots}{
% %     arguments passed to returned timeSeries object.
% %   }

% }


\value{

  returns a S4 object of class \code{timedate}. 
}


\examples{
## Load Microsoft Data Set -
   data(MSFT)
   x = MSFT[1:12, ]

## Bind Columnwise -
   X <- cbind(x[, "Open"], returns(x[, "Open"]))
   colnames(X) <- c("Open", "Return")
   X

## Bind Rowwise - 
   Y <- rbind(x[1:3, "Open"], x[10:12, "Open"])
   Y
}


\keyword{chron}

