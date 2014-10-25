\name{getRedemptionTime}
\alias{getRedemptionTime}
\alias{getRedemptionTimesForMatrix}
\title{Redemption times}
\description{
Return redemption index
}
\usage{
getRedemptionTime(S, n, X)
getRedemptionTimesForMatrix(S, n, X)
}
\arguments{
  \item{S}{A (n x 1) vector of prices at valuation dates or a (N x n) matrix.}
  \item{n}{number of valuation dates; integer value.}
  \item{X}{A vector of call levels (length (n - 1)).}
}
\details{
For a price vector of \eqn{n} prices at valuation dates \eqn{(S(t_1),\ldots,S(t_n))'},
determine the first redemption index \eqn{i} such as \eqn{S(t_i) \ge X(t_i), \forall_{j < i} S(t_j) \le X(t_j)}
(\eqn{i=1,\ldots,(n-1)} or \eqn{i=n} if \eqn{S(t_1) \le X(t_1), \ldots, S(t_{n-1}) \le X(t_{n-1})})
}

\value{
\code{getRedemptionTime} returns a scalar; \code{getRedemptionTimesForMatrix}
returns a \eqn{N \times 1}{N x 1} vector.
}
\author{
Stefan Wilhelm
}
\seealso{
 \code{\link{calcRedemptionProbabilities}} and \code{\link{simRedemptionProbabilities}}
}

\examples{
 S <- c(90, 95, 110, 120)
 X <- c(100, 100, 100)
 getRedemptionTime(S, n=4, X)
 # 3
}
