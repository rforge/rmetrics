\name{plot-methods}


\docType{methods}


\alias{plot-methods}
\alias{plot,ANY,ANY-method}
\alias{plot,fREG,missing-method}


\title{Regression Model Plot Methods}


\description{

    Plots results obtained from a fitted regression model. 

}


\usage{
\S4method{plot}{fREG,missing}(x, which = "ask", \dots)
}


\arguments{

    \item{x}{
        an object of class 'fREG'.
        } 
    \item{which}{
        a character string selectiong which plot should be displayed.
        By default \code{which="ask"} which allows to generate plots
        interactively.
        }
    \item{\dots}{
        additional arguments to be passed to the underlying plot
        functions.
        }
 
}


\section{Methods}{
    \describe{

    \item{x = "ANY", y = "ANY"}{
        Generic function.
        }
    \item{x = "fREG", y = "missing"}{
        Plot function to display results obtained from a fitted 
        regression model. 
        }
        
    }
}


\details{

    The plots are a set of graphs which are common to the regression
    models implemented in the function \code{regFit}. This includes
    linear regression models \code{use="lm"}, 
    robust linear regression models \code{use="rlm"}, 
    generalized linear regression models \code{use = "glm"}, 
    generalized additive regression models \code{use = "gam"}, 
    prjection pursuit regression models \code{use = "ppr"}, 
    neural network regression models \code{use = "nnet"}, and
    polytochomous MARS models \code{use = "polymars"}.
    
    In addition one can also use the original plot functions of the
    original models, .e.g. \code{plot(slot(object, "fit)}.
    
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## regSim -
   x = regSim(model = "LM3", n = 50)
  
## regFit -
   fit = regFit(Y ~ X1 + X2 + X3, data = x, use = "lm")
   
## plot -
}


\keyword{models}

