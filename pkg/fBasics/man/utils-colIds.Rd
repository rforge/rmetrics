\name{Ids}


\alias{Ids}

\alias{colIds}
\alias{rowIds}
\alias{colIds<-}
\alias{rowIds<-}


\title{Set and Retrieve Column/Row Names}


\description{
    
    Sets and retrieves column and row names. The functions
    are for compatibility with SPlus. 
  
}


\usage{
colIds(x, \dots)
rowIds(x, \dots)
}


\arguments{

    \item{x}{
        a numeric matrix.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\details{
    
    Usualli in R the functions \code{colnames}, and \code{rownames} are 
    used to retrieve and set the names of matrices. The functions 
    \code{rowIds} and \code{colIds}, are S-Plus like synonyms.
    
}

\examples{
## pascal -
   # Create Pascal Matrix:
   P = pascal(3)
   P
 
## rownames -  
   rownames(P) <- letters[1:3]
   P   
   
## colIds<- -  
   colIds(P) <- as.character(1:3)
   P                            
}


\keyword{math}

