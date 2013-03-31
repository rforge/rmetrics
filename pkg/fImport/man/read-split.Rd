\name{read-split}


\alias{charvecSplit}
\alias{dataSplit}
\alias{stringSplit}


\title{Splitting downloaded data sets}


\description{

    Helpful data set and charvec splitting utilities. 
    
}


\usage{
dataSplit(x, split=" ", col=-1)
charvecSplit(x, split=" ", col=1, format="\%F")
stringSplit(x, split=" ", col=NULL) 
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


\references{

Diethelm Wuertz, Yohan Chalabi, and Andrew Ellis, (2010);
    \emph{Financial Market Data for R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich,
    www.rmetrics.org.
    
}


\keyword{programming}

