\name{sstdSlider}


\alias{sstdSlider}


\title{Skew Student-t Distribution Slider}


\description{
    
    Displays interactively the dependence of the skew Student-t 
    distribution on its parameters. 
          
}


\usage{
sstdSlider(type = c("dist", "rand"))
}


\arguments{

    \item{type}{
        a character string denoting which interactive plot should
        be displayed. Either a distribution plot \code{type="dist"},
        the default value, or a random variates plot, \code{type="rand"}.
        }
    
}


\value{

    a Tcl object.
 
}


\references{
   
Fernandez C., Steel M.F.J. (2000); 
    \emph{On Bayesian Modelling of Fat Tails and Skewness},
    Preprint, 31 pages. 
    
}



\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
\dontrun{  
## sstdSlider -
   require(tcltk)
   sstdSlider("dist")
   sstdSlider("rand")
}
}


\keyword{distribution}

