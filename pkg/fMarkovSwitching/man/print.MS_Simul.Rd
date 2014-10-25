\name{print.MS_Simul}
\alias{print.MS_Simul}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Print a Markov Switching Simulation Object}
\description{
This function will print to screen a Markov Switching object obtained with MS_Regress_Sim
}
\usage{
print.MS_Simul(MS_Simul_In)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MS_Simul_In}{A S4 object obtained with MS_Regress_Sim}
}

\value{
Returns nothing. Prints simulated model to screen
}

\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\link{MS_Regress_Simul}},\code{\link{plot.MS_Simul}} }
\examples{
# Assuming the mySimModel is a S4 object from the use of MS_Regress_Sim

#plot(mySimModel)

}

\keyword{models}
\keyword{regression}
