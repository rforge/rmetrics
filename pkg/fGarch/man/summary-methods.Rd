\name{summary-methods}


\docType{methods}


\alias{summary-methods}
\alias{summary,ANY-method}
\alias{summary,fGARCH-method}


\title{GARCH Summary Methods}


\description{

    Summary methods for GARCH Modelling. 

}


\section{Methods}{
    \describe{

    \item{object = "ANY"}{
        Generic function
        }
    \item{object = "fGARCH"}{
        Summary function for objects of class \code{"fGARCH"}.
        }
        
    }
}


\section{How to read a diagnostic summary report?}{

    The first four sections return the title, the call, the
    mean and variance formula, and the conditional distribution:

        \preformatted{ 
        Title:
         GARCH Modelling 
        
        Call:
         garchFit(~ garch(1, 1), data = garchSim(), trace = FALSE) 
        
        Mean and Variance Equation:
         ~arch(0)
        
        Conditional Distribution:
         dnorm 
        }
    
    
    The next three sections return the estimated coefficients, and
    an error analysis including standard errors, t values, and
    probabilities, as well as the log Lilelihood values from 
    optimization:
    
        \preformatted{
        Coefficient(s):
                  mu         omega        alpha1         beta1  
        -5.79788e-05   7.93017e-06   1.59456e-01   2.30772e-01  
        
        Error Analysis:
                 Estimate  Std. Error  t value Pr(>|t|)
        mu     -5.798e-05   2.582e-04   -0.225    0.822
        omega   7.930e-06   5.309e-06    1.494    0.135
        alpha1  1.595e-01   1.026e-01    1.554    0.120
        beta1   2.308e-01   4.203e-01    0.549    0.583
        
        Log Likelihood:
         -843.3991    normalized:  -Inf 
        }
    
    
    The next section provides results on standardized residuals
    tests, including statistic and p values, and on information
    criterion statistic including AIC, BIC, SIC, and HQIC:
    
        \preformatted{ 
        Standardized Residuals Tests:
                                        Statistic p-Value    
         Jarque-Bera Test   R    Chi^2  0.4172129 0.8117146  
         Shapiro-Wilk Test  R    W      0.9957817 0.8566985  
         Ljung-Box Test     R    Q(10)  13.05581  0.2205680  
         Ljung-Box Test     R    Q(15)  14.40879  0.4947788  
         Ljung-Box Test     R    Q(20)  38.15456  0.008478302
         Ljung-Box Test     R^2  Q(10)  7.619134  0.6659837  
         Ljung-Box Test     R^2  Q(15)  13.89721  0.5333388  
         Ljung-Box Test     R^2  Q(20)  15.61716  0.7400728  
         LM Arch Test       R    TR^2   7.049963  0.8542942  
         
        Information Criterion Statistics:
                 AIC      BIC      SIC     HQIC 
            8.473991 8.539957 8.473212 8.500687  
        }    

}

    
\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## garchSim -
   x = garchSim(n = 200)

## garchFit - 
   fit = garchFit(formula = x ~ garch(1, 1), data = x, trace = FALSE)
   summary(fit)
}


\keyword{models}

