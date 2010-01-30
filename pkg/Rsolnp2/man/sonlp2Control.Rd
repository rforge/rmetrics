\name{solnp2Control}

\alias{solnp2Control}

\title{Control variables for Rsolnp2}


\description{

    Returns control list
    
}


\usage{
solnp2Control(rho = 1, majit = 10, minit = 10, delta = 1e-5, tol = 1e-4)
}


\arguments{

    \item{rho}{
        a numeric value. The penalty parameter.}
  
    \item{majit}{
        an integer value. The maximum number of major iterations.}

    \item{minit}{
        an integer value. The maximum number of minor iterations.}

    \item{delta}{
        a numeric value. The relative step size in forward difference 
        evaluation.}

    \item{tol}{
        a numeric value. The tolerance on feasibility and optimality.}

}


\seealso{

  \code{solnp2}, \code{solnp2NLP},
  and packages \code{Rnlminb2} and \code{Rsolnp2}.
  
}


\keyword{optimize}

