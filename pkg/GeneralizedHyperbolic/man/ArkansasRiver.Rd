\name{ArkansasRiver}
\alias{ArkansasRiver}
\docType{data}
\title{
  Soil Electrical Conductivity
}
\description{
  Electrical conductivity of soil paste extracts from the Lower Arkansas
  River Valley, at sites upstream and downstream of the John Martin
  Reservoir.
}
\usage{data(ArkansasRiver)}
\format{
  The format is:
List of 2
 $ upstream  : num [1:823] 2.37 3.53 3.06 3.35 3.07 ...
 $ downstream: num [1:435] 8.75 6.59 5.09 6.03 5.64 ...
}
\details{
  Electrical conductivity is a measure of soil water salinity.
}
\source{
This data set was supplied by Eric Morway (\email{emorway@usgs.gov}).
}
\references{
  Eric D. Morway and Timothy K. Gates (2011)
  Regional assessment of soil water salinity across an extensively
  irrigated river valley.
  Journal of Irrigation and Drainage Engineering,
  doi:10.1061/(ASCE)IR.1943-4774.0000411
}
\examples{
data(ArkansasRiver)
lapply(ArkansasRiver, summary)
upstream <- ArkansasRiver[[1]]
downstream <- ArkansasRiver[[2]]
## Fit normal inverse Gaussian
## Hyperbolic can also be fitted but fit is not as good
fitUpstream <- nigFit(upstream)
summary(fitUpstream)
par(mfrow = c(2,2))
plot(fitUpstream)
fitDownstream <- nigFit(downstream)
summary(fitDownstream)
plot(fitDownstream)
par(mfrow = c(1,1))
## Combined plot to compare
## Reproduces Figure 3 from Morway and Gates (2011)
hist(upstream, col = "grey", xlab = "", ylab = "", cex.axis = 1.25,
     main = "", breaks = seq(0,20, by = 1), xlim = c(0,15), las = 1,
     ylim = c(0,0.5), freq = FALSE)
param <- coef(fitUpstream)
nigDens <- function(x) dnig(x, param = param)
curve(nigDens, 0, 15, n = 201, add = TRUE,
      ylab = NULL, col = "red", lty = 1, lwd = 1.7)

hist(downstream, add = TRUE, col = "black", angle = 45, density = 15,
     breaks = seq(0,20, by = 1), freq = FALSE)
param <- coef(fitDownstream)
nigDens <- function(x) dnig(x, param = param)
curve(nigDens, 0, 15, n = 201, add = TRUE,
      ylab = NULL, col = "red", lty = 1, lwd = 1.7)

mtext(expression(EC[e]), side = 1, line = 3, cex = 1.25)
mtext("Frequency", side = 2, line = 3, cex = 1.25)
legend(x = 7.5, y = 0.250, c("Upstream Region","Downstream Region"),
       col = c("black","black"), density = c(NA,25),
       fill = c("grey","black"), angle = c(NA,45),
       cex = 1.25, bty = "n", xpd = TRUE)
}
\keyword{datasets}
