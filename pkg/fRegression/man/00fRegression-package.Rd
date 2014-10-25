\name{fRegression-package}


\alias{fRegression-package}
\alias{fRegression}


\docType{package}


\title{Regression Modelling Package}


\description{

    The Rmetrics "fRegression" package is a collection of functions 
    for linear and non-linear regression modelling.

}


\details{

    \tabular{ll}{
        Package:    \tab fRegression\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.0.1\cr
        Date:       \tab 2014\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2014 Rmetrics Association\cr
        Repository: \tab R-FORGE\cr
        URL:        \tab \url{https://www.rmetrics.org}
    }

}

    
\section{1 Introduction}{

    Regression modelling, especially linear modelling, LM, is a widely
    used application in financial engineering. In finance it mostly 
    appears in form that a variable is modelled as a linear or more complex 
    relationship as a function of other variables. For example the 
    decision of buying or selling in a trading model may be triggered 
    by the outcome of a regression model, e.g. neural networks are a 
    well known tool in this field.

}


\section{2 Fitting Regression Models}{

    Rmetrics has build a unique interface to several regression
    models available in the base and contributed packages of R. 
    The following regression models are interfaced and 
    available through a common function \code{regFit}. The 
    argument \code{use} allows to select the desired model:
    
    \preformatted{
    regFit          fits regression models
    - lm            fits a linear model [stats]
    - rlm           fits a LM by robust regression [MASS]
    - glm           fits a generliazed linear model [stats]
    - gam           fits a generlized additive model [mgcv]
    - ppr           fits a projection pursuit regression model [stats]
    - nnet          fits a single hidden-layer neural network model [nnet]
    - polymars      fits an adaptive polynomial spline regression [polspline]
    }
    
    An advantage of the \code{regFit} function is, that all the
    underlying functions of its family can be called with the same
    list of arguments, and the value returned is always an unique
    object, an object of class \code{"fREG"} with the following slots:
    \code{@call}, \code{@formula}, \code{@method}, \code{@data}, 
    \code{@fit}, \code{@residuals}, \code{@fitted}, \code{@title}, 
    and \code{@description}.
    
    Furthermore, independent of the selected regression model applied 
    we can use the same S4 methods for all types of regressions. This 
    includes, \code{print},\code{plot}, \code{summary}, \code{predict}, 
    \code{fitted}, \code{residuals}, \code{coef}, \code{vcov}, and 
    \code{formula} methods.
    
    It is possible to add further regression models to this framework
    either his own implementations or implementations available through 
    other contributed R packages. Suggestions include \code{biglm},
    \code{earth} amongst others.

}


\section{2 Simulation of Regression Models}{

    contains a function to simulate artificial regression models, 
    mostly used for testing.
    
    \preformatted{
    regSim          simulates artificial regression model data sets
    }
    
}


\section{3 Extractor Functions}{

    These generic functions are:
    
    \preformatted{
    fitted          extracts fitted values from a fitted 'fREG' object
    residuals       extracts residuals from a fitted 'fREG' object
    coef            extracts coefficients from a fitted 'fREG' object
    formula         extracts formula expression from a fitted 'fREG' object
    vcov            extracts variance-covariance matrix of fitted parameters
    }
    
}


\section{4 Forecasting}{

    The function \code{predict} returns predicted values based on the 
    fitted model object.
    
    \preformatted{
    predict         forecasts from an object of class 'fREG'
    }
    
}


\section{4 Reporting Functions}{

    For printing and plotting use the functions:
    
    \preformatted{
    print           prints the results from a regression fit
    plot            plots the results from a gression fit
    summary         returns a summary report       
    }
    
}


\section{About Rmetrics:}{

    The \code{fRegression} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package}


