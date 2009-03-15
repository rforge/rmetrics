\name{time}


\title{timeSeries, Positions}


\alias{time}
\alias{time<-}
 
% % \alias{index}
% % \alias{index<-}

\alias{time,timeSeries-method}
\alias{time<-,timeSeries-method}

% \alias{index.timeSeries}
% \alias{index<-.timeSeries}

\alias{sample,timeSeries-method}
\alias{sort,timeSeries-method}
\alias{rev,timeSeries-method}

\alias{start,timeSeries-method}
\alias{end,timeSeries-method}


\description{
    
    Functions and methods extracting and modifying positions 
    of 'timeSeries' objects.       
    \cr
    
    The functions and methods for the Generation of 'timeSeries' Objects
    are: 

    \tabular{ll}{   
    \code{time.timeSeries} \tab Extracts time positions from a
    'timeSeries', \cr 
    \code{time<-.timeSeries} \tab Assign time positions to a
    'timeSeries', \cr 
 
%     \code{index.timeSeries} \tab Extracts index positions from a
%     'timeSeries', \cr 
%     \code{index<-.timeSeries} \tab Assign index positions to a
%     'timeSeries', \cr 

    \code{sample.timeSeries} \tab Resamples a 'timeSeries' object in
    time, \cr 
    \code{sort.timeSeries} \tab Sorts reverts a 'timeSeries' object in
    time, \cr 
    \code{rev.timeSeries} \tab Reverts a 'timeSeries' object in time,
    \cr  
    \code{start.timeSeries} \tab Extracts start date of a 'timeSeries' 
    object, \cr  
    \code{end.timeSeries} \tab Extracts end date of a 'timeSeries'
    object. } 
        
}


\usage{

\S4method{time}{timeSeries}(x, \dots)
\S4method{time}{timeSeries}(x) <- value

% \method{index}{timeSeries}(x, \dots)
% \method{index}{timeSeries}(x) <- value

\S4method{start}{timeSeries}(x, \dots)
\S4method{end}{timeSeries}(x, \dots)

% \S4method{sample}{timeSeries}(x, \dots)
\S4method{sort}{timeSeries}(x, \dots)
\S4method{rev}{timeSeries}(x)
}


\arguments{

%     \item{method}{ 
%         [alignDailySeries] - \cr
%         the method to be used for the alignment. A character string, one
%         of \code{"before"}, use the data from the row whose position is
%         just before the unmatched position, or \code{"after"}, use the 
%         data from the row whose position is just after the unmatched 
%         position, or \code{"linear"}, interpolate linearly between 
%         \code{"before"} and \code{"after"}. 
%       }
      
    \item{value}{
        a valid value for the component of \code{time(x)}.
        }
    \item{x}{
        [as] - \cr   
        a \code{matrix} type object to be converted.\cr
        [as.vector][as.matrix][as.data.frame] -\cr
        [applySeries] - \cr
        [cut][end][mergeSeries][plot][print][rev][start] - \cr
        an object of class \code{timeSeries}.
        } 
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    \code{timeSeries}\cr
    \code{read.timeSeries}\cr
    \code{as.timeSeries}
    \cr
    return a S4 object of class \code{timeSeries}.
    \cr
    
    \code{seriesData}\cr
    \code{seriesPositions}
    \cr
    extract the \code{@.Data} and \code{@position} slots from a 
    \code{timeSeries} object. Thus, \code{seriesData} returns an 
    object of class \code{matrix}, and \code{seriesPositions}
    returns an object of class \code{timeDate}.
    \cr
    
    \code{is.timeSeries}
    \cr
    returns \code{TRUE} or \code{FALSE} depending on whether its
    argument is of \code{timeSeries} type or not.
    \cr
        
    \code{aggregateSeries}\cr
    \code{applySeries}\cr
    \code{cutSeries}\cr
    \code{mergeSeries}\cr
    \code{returnSeries}\cr
    \code{revSeries}
    \cr
    return a S4 object of class \code{timeSeries}.
    \cr
    
    \code{end, start}
    \cr
    return a S4 object of class \code{timedate}. These are the 
    start and end dates of a \code{timeSeries} object.
    \cr
    
    \code{as.vector}\cr
    \code{as.matrix}\cr
    \code{as.data.frame}
    \cr
    these are methods which convert a S4 object of class \code{timeSeries}
    either to a vector, a matrix or to a data frame.
    \cr
    
    \code{plot}\cr
    \code{lines}\cr
    \code{points}\cr
    \code{print}
    \cr
    plot and print methods for an object of class \code{timeSeries}.
    Note that the plot function requires the packages \code{its} and
    \code{Hmisc}.
    \cr
        
}


\examples{   
## Create dummy timeSeries:     
   X = timeSeries(matrix(rnorm(24), 12), timeCalendar())
   
## series positions -
   time(X)
}


\keyword{chron}

