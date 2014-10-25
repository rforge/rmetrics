\name{plot.MS_Model}
\alias{plot.MS_Model}

\title{Function to Plot the Fitted Processes of a Markov Switching Model}
\description{Using as input a S4 "MS_Model" object, this function plots:\cr
-Fitted Filtered Probabilities (t¦t)\cr
-Fitted Smoothed Probabilities (by means of Kim's Algorithm)\cr
-Fitted Conditional Mean (based on filtered probabilities of t¦t)\cr
-Fitted Conditional standard Deviation (based on filtered probabilities of t¦t)
}
\usage{plot(MS_Model_In)}
\arguments{
  \item{MS_Model_In}{A S4 object (a output from MS_Regress_Fit)}
}

\value{Returns Nothing. Plots the time series in a graphical device}

\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\link{MS_Regress_Fit}},\code{\link{print.MS_Model}},\code{\link{dim.MS_Model}} }
\examples{
# Assuming the mySimModel is a S4 object from the use of MS_Regress_Sim

#plot(mySimModel)
}
\keyword{models}
\keyword{regression}
