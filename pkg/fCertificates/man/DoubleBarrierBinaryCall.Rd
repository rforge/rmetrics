\name{DoubleBarrierBinaryCall}
\alias{DoubleBarrierBinaryCall}
\alias{InlineWarrant}
\title{ Double Barrier Binary Call valuation using pricing by duplication}
\description{
  Valuation of a Double Barrier Binary Call, aka "Inline Warrant"
}
\usage{
DoubleBarrierBinaryCall(S, K, L, U, T, r, r_d, sigma, ratio=1, nmax = 20)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{K}{ the fixed cash rebate }
  \item{L}{ the lower barrier, a numeric value. }
  \item{U}{ the upper barrier, a numeric value. }
  \item{T}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{ratio}{ratio, number of underlyings one certificate refers to, a numeric value;
                e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset}
  \item{nmax}{ maximum number of iterations. Defaults to 20. }
}
\details{
Double Barrier Binary Calls offer a fixed payoff if the underlying stays in the predetermined range [L,U] during the lifetime.
If one of the barriers have been hit the certifiate is knocked out and will be worthless.
  
This method implements the Hui (1996) approach, which is a iteration up to a maximum number \code{nmax}.
  
Also known as:
\itemize{
\item Inline Warrant
\item Range Warrant
}
}
\value{
the price (scalar or vector) of the Double Barrier Binary Call
}

\references{ 
Sutrick, Teall, Tucker, Wei (1997). The Range of Brownian Motion Processes. 
\emph{The Journal of Financial Engineering}, \bold{6}, 31--46

Hui (1996). One-Touch Barrier Binary Option Values.
\emph{Applied Financial Economics}, \bold{6}, 343--346

Haug (2007). The complete Guide to Option Pricing Formulas. 
\emph{Wiley & Sons}, 2nd edition, 180--181
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }
\seealso{ \code{\link[fExoticOptions]{DoubleBarrierOption}} }

\examples{
p <- DoubleBarrierBinaryCall(S=100, K=10, L=80, U=120, T=1, 
   r=0.02, r_d=0, sigma=0.2)
   
# Reproduce Table 4-23 in Haug (2007)
S <- 100
T <- 0.25
L <- c(80, 85, 90, 95)
U <- c(120, 115, 110, 105)
r <- 0.05
b <- 0.03
r_d <- 0.02
K <- 10
sigma <- c(0.1, 0.2, 0.3, 0.5)
p <- matrix(NA, 4, 4)
for (i in 1:4)
{
 for (j in 1:4)
 {
   p[i,j] <- DoubleBarrierBinaryCall(S=S, K=K, L=L[i], U=U[i], T=T, 
     r=r, r_d=r_d, sigma=sigma[j])
 }
}

X=cbind(L, U, p)
colnames(X)=c("L", "U", "sigma=0.1", "sigma=0.2", "sigma=0.3", "sigma=0.5")
X  
}

\keyword{ math }