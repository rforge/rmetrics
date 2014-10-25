\name{print.acdModel}
\alias{print.acdModel}
\title{Function to print ACD model}
\description{Prints the coefficients of the model to the screen}
\usage{print.acdModel(modelOut)}

\arguments{
  \item{modelOut}{A S4 object from \code{ACD_Fit}}
}

\value{Returns nothing}
\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\seealso{\code{\link{plot.acdModel}}}
\examples{

data(myDur)

x=myDur
pLag=1
qLag=1
distrib='exp'

typeACD='ACD'

modelOut<-ACD_Fit(x,qLag,pLag,distrib,typeACD)
print(modelOut)

}
