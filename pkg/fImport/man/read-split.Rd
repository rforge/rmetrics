\name{readSplit}


\alias{charvecSplit}
\alias{dataSplit}


\title{Splitting downloaded data sets}


\description{

    Some helpful data set splitting utilities. 
    
}


\usage{
charvecSplit(x, split=" ", col=1, format="\%F")
dataSplit(x, split=" ", col=-1)
}


\arguments{

    \item{x}{
        character vector to be splitted. 
    }
    \item{split}{
        the split character, by default a blank.
    }
    \item{col}{
        an integer value or vector, the columns to be selected.
    }
    \item{format}{
        the date format of the character vector, by default
        the ISO-8601 date format.
    }
}


%\value{}


\keyword{programming}

