\name{stdSlider}


\alias{stdSlider}


\title{Student-t Distribution Slider}


\description{
    
    Displays interactively the dependence of the Student-t 
    distribution on its parameters. 
          
}


\usage{
stdSlider(type = c("dist", "rand"))
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


%\references{    
%}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
\dontrun{  
## stdSlider -
   require(tcltk)
   stdSlider("dist")
   stdSlider("rand")
}
}


\keyword{distribution}

