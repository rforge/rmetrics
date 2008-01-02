\name{fREG-class}


\docType{class}


\alias{fREG-class}


\title{Class "fREG"}


\description{ 

    The class 'fREG' represents a fitted model of an heteroskedastic
    time series process.
}


\section{Objects from the Class}{

    Objects can be created by calls of the function \code{regFit}
    and \code{gregFit}. The returned objects represent parameter 
    estimates of linear and generalized linear models.
    
}


\section{Slots}{
    \describe{

    \item{\code{call}:}{Object of class \code{"call"}: 
        the call of the \code{garch} function.
        } 
    \item{\code{formula}:}{Object of class \code{"formula"}: 
        the formula used in parameter estimation.
        }   
    \item{\code{family}:}{Object of class \code{"character"}: 
        the family objects provide a convenient way to specify 
        the details of the models used by function \code{grefFit}
        For details we refer to the documentation for the function
        \code{glm} in R's base package on how such model fitting 
        takes place. 
        }      
    \item{\code{method}:}{Object of class \code{"character"}: 
        a string denoting the regression model in use, i.e. one
        of those listed in the \code{use} argument of the function 
        \code{regFit} or \code{gregFit}.
        }
    \item{\code{data}:}{Object of class \code{"list"}: 
        a list with at least two entries named \code{x} containing the 
        data frame used for the estimation, and \code{data} with the
        object of the rectangular input data.
        }
    \item{\code{fit}:}{Object of class \code{"list"}: 
        a list with the results from the parameter estimation. The entries
        of the list depend on the selected algorithm, see below.
        }
    \item{\code{residuals}:}{Object of class \code{"numeric"}: 
        a numeric vector with the residual values.
        }
    \item{\code{fitted}:}{Object of class \code{"numeric"}: 
        a numeric vector with the fitted values.
        }
    \item{\code{title}:}{Object of class \code{"character"}: 
        a title string.
        }
    \item{\code{description}:}{Object of class \code{"character"}: 
        a string with a brief description.
        }
    
    }
}       


\section{Methods}{
    \describe{
    
    \item{show}{\code{signature(object = "fREG")}: 
        prints an object of class 'fREG'.
        }    
    \item{plot}{\code{signature(x = "fREG", y = "missing)}: 
        plots an object of class 'fREG'.
        }  
    \item{summary}{\code{signature(object = "fREG")}: 
        summarizes results and diagnostic analysis of an object 
        of class 'fREG'.
        }

    \item{predict}{\code{signature(object = "fREG")}: 
        forecasts mean and volatility from an object of class 'fREG'.
        }
    \item{fitted}{\code{signature(object = "fREG")}: 
        extracts fitted values from an object of class 'fREG'.
        }     
    \item{residuals}{\code{signature(object = "fREG")}: 
        extracts fresiduals from an object of class 'fREG'.
        }
        
    \item{coef}{\code{signature(object = "fREG")}: 
        extracts fitted coefficients from an object of class 'fREG'.
        }
     \item{formula}{\code{signature(x = "fREG")}: 
        extracts formula expression from an object of class 'fREG'.
        } 
        
    }
}


\author{

    Diethelm Wuertz and Rmetrics Core Team.
    
}


\keyword{programming}

