\name{getArgs}

\title{getArgs}
\alias{getArgs}

\description{

  Gets arguments from a S4 function and signature.

}

\usage{

getArgs(f, signature = character())

}

\arguments{

  \item{f}{
    A generic function or the character-string name of one.
  }
  \item{signature}{
    A match of formal argument names for 'f' with the
    character-string names of corresponding classes.
    }
}

\examples{

args(returns)
getArgs(returns)
getArgs("returns")
getArgs(returns, "timeSeries")
getArgs("returns", "timeSeries")

}