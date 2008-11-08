\name{sample}


\title{Resampling 'timeDate' Objects}


\alias{sample}
\alias{sample,timeDate-method}


\description{
    
    Resamples a 'timeDate' object.
}


% \usage{
% \S4method{sample}{timeDate}(x, \dots)
% }
% \arguments{
%   \item{x}{ Either a (numeric, complex, character or logical) vector of
%     more than one element from which to choose, or a positive
%     integer.}
%   \item{size}{non-negative integer giving the number of items to choose.}
%   \item{replace}{Should sampling be with replacement?}
%   \item{prob}{A vector of probability weights for obtaining the elements of
%       the vector being sampled.}
% }


\value{
    
    returns an object of class \code{"timeDate"}.

}


\examples{
## c -
   # Create Character Vectors:
   dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
   dts
   tms = c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
   tms
   
## "+/-" - 
   # Add One Day to a Given timeDate Object:
   GMT = timeDate(dts, zone = "GMT", FinCenter = "GMT")
   GMT
   ZUR = timeDate(dts, zone = "GMT", FinCenter = "Europe/Zurich")
   ZUR
   
## c - 
   # Concatenate and Replicate timeDate Objects:
   c(GMT[1:2], ZUR[1:2])
   c(ZUR[1:2], GMT[1:2])
   
## rep - 
   rep(ZUR[2], times = 3)
   rep(ZUR[2:3], times = 2)  
}


\keyword{chron}

