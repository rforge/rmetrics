\name{MS_Regress_Fit}
\alias{MS_Regress_Fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to Estimate a Markov Switching Regression}

\description{This function will estimate by maximum likelihood a univariate MS model based on input arguments}

\usage{
MS_Regress_Fit(dep, indep, S, k = 2, distIn = "Normal")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dep}{The dependent/explained variable (a vector)}
  \item{indep}{the independent/explanatory variables (a matrix)}
  \item{S}{Input argument which controls which indep variables switch (see documentation for more details)}
  \item{k}{Number of states in MS model (a positive integer, e.g. \code{k=2})}
  \item{distIn}{Assumed distribution for residue ("Normal" or "t")}
}

\value{
  Returns a S4 "MS_Model" object the following slots:

\item{@filtProb}{A matrix with the filtered probabilities from the Model")}
\item{@smoothProb}{A matrix with the smooth probabilities from the Model (Kim's algorithm)}
\item{@Coeff}{A list structure with all estimated coefficients (see documentation for more details)}
\item{@CondMean}{A matrix of fitted conditional means, conditional on the probabilities of t¦t}
\item{@Coeff_Std}{A list with standard errors of coefficients (with same members as @Coeff)}
\item{@LL}{The maximum log likelihood found by optimizing algorithm}
\item{@k}{The number of states in MS model}
\item{@paramVec}{Estimated parameters in vector notation (with the same values as @Coeff}
\item{@stateDur}{The Expected duration of each regime}
\item{@nParameter}{The number of parameters in the model}
\item{@nParameter}{The number of parameters in the model}
\item{@sizeModel}{A list with the size of the estimated model (see documentation for details)}
\item{@distrib}{The distribution assumption ("Normal" or "t")}

}
\references{
\cite{ALEXANDER, C. (2008) 'Market Risk Analysis: Practical Financial Econometrics' Wiley}\cr
\cite{HAMILTON, J., D. (1994) 'Time Series Analysis' Princeton University Press}\cr
\cite{HAMILTON, J., D. (2005) 'Regime Switching Models' Palgrave Dictionary of Economic}\cr
\cite{KIM, C., J., NELSON, C., R. (1999) 'State Space Model with Regime Switching: Classical and Gibbs-Sampling Approaches with Applications' The MIT press}
}
\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}

\note{A MatLab version of the code is also available without loss of funcionality. See matlab exchange site (http://www.mathworks.com/matlabcentral/fileexchange)
and search for "Marcelo Perlin."
}

\seealso{\code{\link{MS_Regress_Simul}},\code{\link{MS_Regress_For}},\code{\link{MS_Regress_Lik}}}
\examples{

# defining some data (from package)

data(indep)
data(dep)

S<-c(1,0,0)	# where to switch (in this case in the olny indep)
k<-2		# number of states
distIn<-"Normal"#distribution assumption

myModel<-MS_Regress_Fit(dep,indep,S,k)	# fitting the model
print(myModel)	# printing output
plot(myModel)	# plotting output
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}
\keyword{regression}
