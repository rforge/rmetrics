library(HyperbolicDist)

sampSize <- 100000
param <- c(0,1,2,3)
dataVector <- rnl(sampSize, param)

### Test nlMean
nlMean(param)
mean(dataVector)

ddist <- function(x, order, param, about) {
  (x - about)^order * dnl(x, param = param)
}
integrate(ddist, -30,30, param = param, order = 1,
          about = 0, subdivisions = 1000,
          rel.tol = .Machine$double.eps^0.5)[[1]]



### Test nlVar
nlVar(param)
var(dataVector)
mn <- nlMean(param)

ddist <- function(x, order, param, about) {
  (x - about)^order * dnl(x, param = param)
}

integrate(ddist, -30,30, param = param, order = 2,
          about = mn, subdivisions = 1000,
          rel.tol = .Machine$double.eps^0.5)[[1]]



### Test nlSkew
nlSkew(param)
skewness(dataVector)

ddist <- function(x, order, param, about) {
  (x - about)^order * dnl(x, param = param)
}
param <- c(0,1,2,3)
mn <- nlMean(param)
nlVar(param)
m3<-integrate(ddist, -30,30, param = param, order = 3,
          about = mn, subdivisions = 1000,
          rel.tol = .Machine$double.eps^0.5)[[1]]
sigma3<-nlVar(param)^(3/2)
nlSkew<-m3/sigma3
nlSkew

### Test nlKurt
nlKurt(param)
kurtosis(dataVector)

ddist <- function(x, order, param, about) {
  (x - about)^order * dnl(x, param = param)
}
param <- c(param)
mn <- nlMean(param)
nlVar(param)
m4<-integrate(ddist, -30,30, param = param, order = 4,
          about = mn, subdivisions = 1000,
          rel.tol = .Machine$double.eps^0.5)[[1]]
sigma4<-nlVar(param)^2
nlKurt<-m4/sigma4-3
nlKurt
