\name{plot.acdModel}
\alias{plot.acdModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to plot an ACD S4 object}
\description{This function plots the results from the use of \code{ACD_Fit}}
\usage{plot.acdModel(modelOut)}

\arguments{
  \item{modelOut}{A S4 object from \code{ACD_Fit}}
}

\value{Returns nothing. Plot the graphics.}
\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}
\seealso{\code{\link{ACD_Fit}},\code{\link{ACD_Simul}}}
\examples{

data(myDur)


x=myDur
pLag=1
qLag=1
distrib='exp'

typeACD='ACD'

modelOut<-ACD_Fit(x,qLag,pLag,distrib,typeACD)
plot(modelOut)


}
