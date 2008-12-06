\name{ipop}

\alias{ipop}

\title{Quadratic Programming Solver}

\description{
  ipop solves the quadratic programming problem :\cr
  \eqn{\min(c'*x + 1/2 * x' * H * x)}\cr
  subject to: \cr
  \eqn{b <= A * x <= b + r}\cr
  \eqn{l <= x <= u}
}

\usage{
ipop(c, H, A, b, l, u, r, sigf = 12, maxiter = 100, margin = 1e-08, bound = 10, verb = 0)
}

\arguments{
  \item{c}{Vector or one column matrix appearing in the quadratic function}
  \item{H}{square matrix appearing in the quadratic function, or the
    decomposed form \eqn{Z} of the \eqn{H} matrix where \eqn{Z} is a
    \eqn{n x m} matrix with \eqn{n > m} and \eqn{ZZ' = H}.}
  \item{A}{Matrix defining the constrains under which we minimize the
    quadratic function}
  \item{b}{Vector or one column matrix defining the constrains}
  \item{l}{Lower bound vector or one column matrix}
  \item{u}{Upper bound vector or one column matrix}
  \item{r}{Vector or one column matrix defining constrains}
  \item{sigf}{Precision (default: 7 significant figures)}
  \item{maxiter}{Maximum number of iterations}
  \item{margin}{how close we get to the constrains}
  \item{bound}{Clipping bound for the variables}
  \item{verb}{Display convergence information during runtime}
}

\details{
  ipop uses an interior point method to solve the quadratic programming
  problem. \cr
  The \eqn{H} matrix can also be provided in the decomposed form \eqn{Z}
  where \eqn{ZZ' = H} in that case the Sherman Morrison Woodbury formula
  is used internally.
}

\value{

A list with components:

\item{weights}{Vector containing the solution of the quadratic programming problem}
\item{status}{Numeric value either 0 or 1 giving the status of the optimization, where 1 means the process failed}
\item{dual}{The dual solution of the problem}
\item{message}{Character string describing the type of convergence}
\item{iterations}{Number of iterations the algorithm needed}
}

\references{
  R. J. Vanderbei\cr
  \emph{LOQO: An interior point code for quadratic programming}\cr
  Optimization Methods and Software 11, 451-484, 1999 \cr
  \url{http://www.sor.princeton.edu/~rvdb/ps/loqo3.ps.gz}
}


\examples{

## Solve a simple portfolio optimization problem
 
# Generate random normal returns for 4 assets
x <- matrix(rnorm(400), nc = 4)

# Statistics
nAssets <- ncol(x)
Mu <- colMeans(x)
Sigma <- cov(x)
targetReturn <- mean(Mu)

# Create H matrix etc.
c <- rep(0, nAssets)
H <- Sigma
b <- c(targetReturn, 1) 
A <- rbind(Mu, rep(1, nAssets))
r <- c(0, 0)
l <- rep(0, nAssets)
u <- l + 1

ipop(c, H, A, b, l, u, r)

}

\keyword{optimize}
