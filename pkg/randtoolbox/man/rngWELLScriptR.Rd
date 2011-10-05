\name{rngWELLScriptR}
\alias{rngWELLScriptR}
\title{An implementation of the recurrence of WELL generators in R language}

\description{
  An implementation of the recurrence of WELL generators in R language
  for testing purposes. It is too slow to be used for random number
  generation.
}
  
\usage{ 
  rngWELLScriptR(n, s, generator)
} 

\arguments{
  \item{n}{ Integer. The length of the output sequence.}
  \item{s}{ An 0,1-matrix representing the state of the required WELL generator
            as obtained by \code{getWELLState()}.}
  \item{generator}{ Character string. Name of the generator from the list
  \code{"512a", "521a", "521b", "607a", "607b", "800a", "800b", "1024a", "1024b", "19937a",
  "19937c", "19937b", "21701a", "23209a", "23209b", "44497a", "44497b"}.
  }
}

\value{
  A numeric vector of length \code{n}.
}

\seealso{
  \code{\link{getWELLState}}
}

\examples{
  set.generator("WELL", order=512, version="a", seed=123456)
  s <- getWELLState()
  x <- runif(500)
  y <- rngWELLScriptR(500, s, "512a")
  all(x == y)
  # [1] TRUE
}

