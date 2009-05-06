\name{ACD_Simul}
\alias{ACD_Simul}
\title{Function for Simulation of an ACD model}
\description{This function simulates an ACD process based on input arguments.}
\usage{ACD_Simul(nr, Coeff, distrib = "exp", typeACD = "ACD")}

\arguments{
  \item{nr}{Number of observations in simulation}
  \item{Coeff}{List with all coefficients (check example)}
  \item{distrib}{Distribution assumption for disturbance ('exp' or 'weibull'}
  \item{typeACD}{Type of ACD model to be simulated ('ACD', 'log', 'BC' or 'EX')}
}

\value{Returns a vector with the simulated duration series}

\author{Marcelo Perlin - ICMA/UK <\email{marceloperlin@gmail.com}>}
\seealso{\code{\link{ACD_Fit}}}

\examples{
nr=1000

pLag=1
qLag=1
distrib='weibull'

typeACD='BC'

Coeff<-list(w=.05  ,
            q=0.016,
            p=.95  ,
            y=.9   ,
            delta=.4)

simDur<-ACD_Simul(nr,Coeff,distrib,typeACD)

plot(simDur,xlab='Event',ylab='Simulated Duration',type='l')

}   