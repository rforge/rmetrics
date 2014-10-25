\name{payoffExpress}
\Rdversion{1.1}
\alias{payoffExpressClassic}
\alias{payoffExpressCappedBonusType1}
\alias{payoffExpressBonusType1}
\alias{payoffExpressML0AN5}

\title{Defining payoff functions for Express Certificates}
\description{
Defining common or particular payoff functions for Express Certificates
}

\usage{
payoffExpressClassic(i, n, S, m, K)
payoffExpressML0AN5(i, n, S, m, K, B, S0)
payoffExpressCappedBonusType1(i, n, S, m, K, B)
payoffExpressBonusType1(i, n, S, m, K, B)
}

\arguments{
  \item{i}{The redemption date (\eqn{i=1,...,n})}
  \item{n}{The number of valuation dates}
  \item{S}{A vector of length n for the prices at the valuation dates, i.e. \eqn{S(t_1),...,S(t_n)}}
  \item{m}{A vector of length n for the running minimum at the valuation dates, i.e. \eqn{m(t_1),...,m(t_n)}}
  \item{K}{A vector of fixed cash rebates at early redemption times}
  \item{B}{A barrier level to be monitored}
  \item{S0}{underlying start price}
}

\details{
Payoff structure of express certificates can be either path independent or path dependent, 
while monitoring a barrier \code{B}.

\bold{Path independent payoffs:}\cr
The function \code{payoffExpressClassic} implements the following payoff at \eqn{t_i}:
\deqn{p(t_i) = K(t_i) \quad \textrm{for} \quad i < n, \quad \textrm{else} \quad S(t_n)}{%
      p(t_i) = K[i] for i < n, else S[n]
     }

\bold{Path dependent payoffs:}\cr
The function \code{payoffExpressCappedBonusType1} implements the following payoff: \cr
\tabular{rll}{
 \eqn{p(t_i) =} \tab \eqn{K(t_i)}{K[i]} \tab \eqn{\textrm{ for } \quad i < n}{for i < n} \cr
                \tab \eqn{S(t_n)}{S[n]} \tab \eqn{\textrm{ for } i = n \textrm{ and } m(t_n) \le B}{for i == n and m[n] <= B} \cr
                \tab \eqn{K(t_n)}{K[n]} \tab \eqn{\textrm{ for } i = n \textrm{ and } m(t_n) > B}{for i == n and m[n] > B}
}
In case the barrier has not been hit during the lifetime, a fixed bonus payment \eqn{K(t_n)}{K[n]} is payed
and the payoff is therefore capped.

The function \code{payoffExpressBonusType1} implements the following payoff: \cr
\tabular{rll}{
 \eqn{p(t_i)} \tab \eqn{K(t_i)}{K[i]}                          \tab \eqn{\textrm{ for } \quad i < n}{for i < n} \cr
              \tab \eqn{S(t_n)}{S[n]}                          \tab \eqn{\textrm{ for } i = n \textrm{ and } m(t_n) \le B}{for i == n and m[n] <= B} \cr
              \tab \eqn{\max{(K(t_n),S(t_n))}}{max(K[n],S[n])} \tab \eqn{\textrm{ for } i = n \textrm{ and } m(t_n) > B}{for i == n and m[n] > B}
}
Unlike in the \code{payoffExpressCappedBonusType1}, 
this payoff is not capped for the case (\eqn{S(t_n) > K(t_n)})

The function \code{payoffExpressML0AN5} is an example of an quite complicated payoff including
path dependence and coupon payments. See also the certificate prospectus \url{../inst/doc/ML0AN5.pdf}.
}

\value{
returns the certificate payoff (Not discounted payoff!) for the given inputs at time i
}

\author{Stefan Wilhelm \email{wilhelm@financial.com} }

\seealso{
See also the generic pricing function \code{\link{SimulateGenericExpressCertificate}}
}