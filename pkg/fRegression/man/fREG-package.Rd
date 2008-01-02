\name{fRegression-package}


\alias{fRegression-package}


\alias{fRegression}


\docType{package}


\title{Regression Modelling Package}


\description{

    Package of functions for regression modelling.
}


\details{

    \tabular{ll}{
        Package:    \tab fRegression\cr
        Type:       \tab Package\cr
        Version:    \tab 261.73.1\cr
        Date:       \tab 2008\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2008 Diethelm Wuertz, Rmetrics Foundation\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }
    
       
    Regression modelling, especially linear modelling, is a widely
    used application in finance. In finance it mostly appears in the 
    form that a variable is modelled as a linear or more complex 
    relationship as a function of other variables. For example the 
    decision of buying or selling in a trading model may be triggered 
    by the outcome of a regression model, e.g. neural networks are a 
    well known tool in this field.
    
    Rmetrics has build a simple interface to several regression
    models available in the base and contributed packages of R. By 
    default the following regression models are interfaced and available
    through the function \code{regFit}:
    
    ...
    
    All the functions of the \code{regFit} and \code{gregFit} family 
    return the same object, an object of class \code{"fREG"} with the 
    following slots:
    call, formula, method, data, fit, residuals, fitted, title, and
    description.
    
    Thus, independent of the regression model applied we can use the 
    same S4 methods for all types of regression. This uncludes, print,
    plot, summary, predict, fitted, residuals, coef, and formula 
    methods.
    
    Evenmore the user can add further regression models to this framework
    either his own implementations or implementations available through 
    contributed R packages. 

}


\section{Class Representation}{

    contains functions to simulate artificial regression models.
    
    Functions:
    
    \tabular{ll}{
    \code{regSim} \tab Simulates artificail regression model data sets. }
    
}


\section{Simulation of Regression Models}{

    contains functions to fit the parameters regression models
    
    Functions:
    
    \tabular{ll}{
    \code{regFit} \tab Estimates the parameters of a regression model, \cr
    \code{gregFit} \tab Estimates the parameters of a generalized regression model, \cr
    \code{fitted} \tab Extracts fitted values from a fitted 'fREG' object, \cr
    \code{residuals} \tab Extracts residuals from a fitted 'fREG' object, \cr
    \code{coef} \tab Extracts coefficients from a fitted 'fREG' object, \cr
    \code{formula} \tab Extracts formula expression from a fitted 'fREG' object. }
    
}


\section{Forecasting}{

    contains functions to forcecast from a regression model.
    
    Functions:
    
    \tabular{ll}{
    \code{predict} \tab Forecasts from an object of class 'fREG'.}
    
}


\author{

    Diethelm Wuertz and Rmetrics Core Team.
    
}


\keyword{package}

