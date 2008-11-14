\name{plot.MS_Simul}
\alias{plot.MS_Simul}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Plot a Simulated Markov Switching Process}
\description{This generic function plots the output from the use of MS_Regress_For}
\usage{plot(MS_Simul_In)}

\arguments{
  \item{MS_Simul_In}{A S4 object obtained with MS_Regress_Sim}
}

\value{Returns nothing. Plots a graphical Device}
\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\link{MS_Regress_Simul}},\code{\link{print.MS_Simul}},\code{\link{dim.MS_Model}} }
\examples{
# Assuming the mySimModel is a S4 object from the use of MS_Regress_Sim

#plot(mySimModel)
}
\keyword{models}
\keyword{regression}
