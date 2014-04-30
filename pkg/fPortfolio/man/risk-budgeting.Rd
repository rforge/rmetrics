\name{risk-budgeting}


%\alias{pfolioReturn}
\alias{sampleCOV}
\alias{normalVaR}
\alias{modifiedVaR}
\alias{sampleVaR}
\alias{budgetsSampleCOV}
\alias{budgetsNormalVAR}
\alias{budgetsModifiedVAR}
\alias{budgetsNormalES}
\alias{budgetsModifiedES}


\title{Risk Budgeting}


\description{
  
    Functions for risk budgeting.
    
}


\usage{
%pfolioReturn(x, weights, geometric=TRUE)
sampleCOV(x)
normalVaR(x, alpha=0.05)
modifiedVaR(x, alpha=0.05)
sampleVaR(x, alpha=0.05)

budgetsSampleCOV(x, weights, mu=NULL, Sigma=NULL)

budgetsNormalVAR(x, weights, alpha=0.05, mu=NULL, Sigma=NULL)
budgetsModifiedVAR(x, weights, alpha=0.05, mu=NULL, Sigma=NULL, 
  M3=NULL, M4=NULL) 
  
budgetsNormalES(x, weights, alpha=0.05, mu=NULL, Sigma=NULL)
budgetsModifiedES(x, weights, alpha=0.05, mu=NULL, Sigma=NULL, 
  M3=NULL, M4=NULL)
}


\arguments{

    \item{x}{x}
    \item{weights}{weights}
    \item{alpha}{alpha}
    \item{mu,Sigma}{mean and covariance}
    \item{M3, M4}{M3 and M4}

}


\references{

Wuertz, D., Chalabi, Y., Chen W., Ellis A. (2009);
    \emph{Portfolio Optimization with R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich.
    
}


\keyword{models}


