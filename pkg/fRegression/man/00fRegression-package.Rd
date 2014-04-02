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

    Regression modelling, especially linear modelling, LM, ?glmis a widely
    used application in finance. In finance it mostly appears in the 
    form that a variable is modelled as a linear or more complex 
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
    lm        stats       fits a linear model
    rlm       MASS        fits a LM by robust regression using an M 
                          estimator
    glm       stats       fits a generliazed linear model
    gam       mgcv        fits a generlized additive model with 
                          integrated smoothness estimation
    ppr       stats       fits a projection pursuit regression model
    nnet      nnet        fits a single hidden-layer neural network model
    polymars  polspline   fits an adaptive polynomial spline regression
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

    contains functions to simulate artificial regression models.
    
    \preformatted{
    regSim          simulates artificial regression model data sets
                    for testing issues.
    }
    
}


\section{3 Extractor Functions}{

    These are:
    
    \preformatted{
    fitted          extracts fitted values from a fitted 'fREG' object
    residuals       extracts residuals from a fitted 'fREG' object
    coef            extracts coefficients from a fitted 'fREG' object
    formula         extracts formula expression from a fitted 'fREG' object
    vcov            extracts variance-covariance matrix of fitted parameters
    }
    
    Function \code{fitted} will extract fitted values for the selected
    regression model. 
    
    Function \code{residuals} will extract model residuals for the 
    selection regression model.
    
    Function \code{coef} will extract the matrix of coefficients with 
    standard errors, t-statistics and p-values.
    
    Function \code{formula} and its specific methods provide a way of 
    extracting formulae which have been included in regression models.
    
    Function \code{vcov} returns the variance-covariance matrix of 
    the parameters of the fitted regression model object.
    
    All functions are generic.
    
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

\author{

    Diethelm Wuertz.
    
}


\keyword{package}


