\name{assets-distance}


\alias{assetsDist}

\alias{corDist}     
\alias{kendallDist}  
\alias{spearmanDist}    
\alias{mutinfoDist}            
\alias{euclideanDist}   
\alias{maximumDist}     
\alias{manhattanDist}   
\alias{canberraDist}    
\alias{binaryDist}      
\alias{minkowskiDist}         
\alias{braycurtisDist}  
\alias{mahalanobisDist} 
\alias{jaccardDist}     
\alias{sorensenDist}   


\title{Distance Measures}


\description{
  
    Allows to measure the distance or similarity between assets.
    
}
    

\usage{
assetsDist(x, method="cor", \dots)

corDist(x)  
kendallDist(x) 
spearmanDist(x) 
  
mutinfoDist(x, nbin=10)
         
euclideanDist(x)    
maximumDist(x)      
manhattanDist(x)  
canberraDist(x)   
binaryDist(x)      
minkowskiDist(x) 
         
braycurtisDist(x)   
mahalanobisDist(x) 
jaccardDist(x)     
sorensenDist(x) 
}


\arguments{
  
    \item{x}{
        any rectangular time series object which can be converted by the 
        function \code{dist()} into a distance object. 
        }
    \item{method}{
        a character string, the method from which to compute the
        distances. Allowed methods include \code{cor}, \code{kendall},
        \code{spearman}, \code{mutinfo}, \code{euclidean}, \code{maximum}, 
        \code{manhattan}, \code{canberra}, \code{binary}, \code{minkowski},
        \code{braycurtis}, \code{mahalanobis}, \code{jaccard}, 
        \code{difference}, or \code{sorensen}.
        }
    \item{nbin}{
        an integer value, the number of bins, by default 10.
        }  
    \item{\dots}{
        optional argument to be passed the distance function.
        }
        
}


\value{
  
    an object of class \code{dist}.
    
}


\details{

    \code{corDist}, \code{kendallDist}, and \code{spearmanDist} call the 
    base \code{cov} function from R.
    
    \code{mutinfoDist} calls the function \code{mutinfo} from the contributed
    R package \code{bioDist}.
    
    \code{euclideanDist}, \code{maximumDist}, \code{manhattanDist}, 
    \code{canberraDist}, \code{binaryDist}, and \code{minkowskiDist} 
    are functions build on top of R's base package.
    
    \code{braycurtisDist}, \code{mahalanobisDist}, \code{jaccardDist},  
    and \code{sorensenDist} call functions from
    the contributed R package \code{ecodist}.
    
}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\examples{  
## LPP -
   # Load Swiss Pension Fund Data:
   LPP <- LPP2005REC[, 1:6]
   head(LPP)

## Returns correlation distance measure
   corDist(LPP)  
   
## Returns kendalls correlation distance measure  
   kendallDist(LPP) 
   
## Returns spearmans correlation distance measure
   spearmanDist(LPP)          

## Return mutual information distance measure
   mutinfoDist(LPP)             

## Return Euclidean distance measure
   euclideanDist(LPP)
   
## Return maximum distance measure
   maximumDist(LPP)              

## Return Manhattan distance measure  
   manhattanDist(LPP)            

## Return Canberra distance measure
   canberraDist(LPP)             

## Return binary distance measure
   binaryDist(LPP)               

## Return Minkowsky distance measure
   minkowskiDist(LPP)            

## Return Bray Curtis distance measure
   braycurtisDist(LPP)           
   
## Return Mahalanobis distance measure
   # mahalanobisDist(LPP)          
   
## Return Jaccard distance mesaure
   jaccardDist(LPP)         
   
## Return Sorensen distance measure
   sorensenDist(LPP)               
}


\keyword{stats}

