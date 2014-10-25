\name{print.MS_Model}
\alias{print.MS_Model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Print (output to screen) a Markov Switching Model}
\description{
This function will output to screen a model fitted with MS_Regress_Fit
}
\usage{print(MS_Model_In)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MS_Model_In}{A S4 object obtained with MS_Regress_Fit}

}
\value{
Returns nothing. Prints model in screen
}

\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\link{MS_Regress_Fit}},\code{\link{dim.MS_Model}},\code{\link{plot.MS_Model}}}
\examples{
# Assuming the myModel is a S4 object from the use of MS_Regress_Fit

#print(myModel)
}
\keyword{models}
\keyword{regression}
