require(DistributionUtils)
##source("../R/incompleteBesselK.R")

options(digits = 15)
for (i in 0:9){
  ibf <- incompleteBesselK(0.01, 4, i, nmax = 100)
  print(ibf)
}

options(digits = 15)
numIBF <- numeric(10)
for (i in 0:9){
  ibf <- incompleteBesselK(4, 0.01, i, nmax = 100)
  numIBF[i + 1] <- ibf
  print(ibf)
}

besselFn <- function(t, x, y, nu) {
  (t^(-nu - 1))*exp(-x*t - y/t)
}

intIBF <- numeric(10)
for (i in 0:9){
  ibf <- integrate(besselFn, 1, Inf, x = 4, y = 0.01, nu = i)
  intIBF[i + 1] <- ibf$value
  print(ibf)
}

numIBF - intIBF


system.time({
            numIBF <- numeric(10)
            for (i in 0:9){
              ibf <- incompleteBesselK(4, 0.01, i, tol = 10^(-7), nmax = 100)
              numIBF[i + 1] <- ibf
              print(ibf)
            }
})

system.time({
            numIBF <- numeric(10)
            for (i in 0:9){
              ibf <- incompleteBesselK(4, 0.01, i, nmax = 100)
              numIBF[i + 1] <- ibf
              print(ibf)
            }
})

