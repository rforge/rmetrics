### Create parameter sets for Generalized Hyperbolic
###
### ghypLargeParam
mus <- c(-1,0,1,2)
deltas <- c(1,2,5,10)
xis <- rep(c(0.1,0.3,0.5,0.7,0.9), 1:5)
chis <- c(0,-0.25,0.25,-0.45,0,0.45,-0.65,-0.3,0.3,0.65,
          -0.85,-0.4,0,0.4,0.85)
xiChis <- cbind(xis, chis)
lambdas <- c(-1,-0.5,0,0.5,1,2)

maxrows <- length(mus)*length(deltas)*NROW(xiChis)*length(lambdas)
ghypLargeParam <- matrix(nrow=maxrows,ncol=5)
rownum <- 1
for (i in 1:length(mus)){
  for (j in 1:length(deltas)){
    for (k in 1:NROW(xiChis)){
      for (l in 1:length(lambdas)){
          param <- ghypChangePars(3,2,
                     c(mus[i],deltas[j],xiChis[k,1],xiChis[k,2],lambdas[l]))
          ghypLargeParam[rownum,] <- param
          rownum <- rownum + 1
      }
    }
  }
}

### ghypSmallParam
mus <- c(0,1)
deltas <- c(1,5)
xis <- rep(c(0.1,0.3,0.7), c(1,2,4))
chis <- c(0,-0.25,0.25,-0.65,-0.3,0.3,0.65)
xiChis <- cbind(xis, chis)
lambdas <- c(-0.5,0,1)

maxrows <- length(mus)*length(deltas)*NROW(xiChis)*length(lambdas)
ghypSmallParam <- matrix(nrow=maxrows,ncol=5)
rownum <- 1
for (i in 1:length(mus)){
  for (j in 1:length(deltas)){
    for (k in 1:NROW(xiChis)){
      for (l in 1:length(lambdas)){
          param <- ghypChangePars(3,2,
                     c(mus[i],deltas[j],xiChis[k,1],xiChis[k,2],lambdas[l]))
          ghypSmallParam[rownum,] <- param
          rownum <- rownum + 1
      }
    }
  }
}

### ghypLargeShape
mus <- 0
deltas <- 1
xis <- rep(c(0.1,0.3,0.5,0.7,0.9), 1:5)
chis <- c(0,-0.25,0.25,-0.45,0,0.45,-0.65,-0.3,0.3,0.65,
          -0.85,-0.4,0,0.4,0.85)
xiChis <- cbind(xis, chis)
lambdas <- c(-1,-0.5,0,0.5,1,2)

maxrows <- length(mus)*length(deltas)*NROW(xiChis)*length(lambdas)
ghypLargeShape <- matrix(nrow=maxrows,ncol=5)
rownum <- 1
for (i in 1:length(mus)){
  for (j in 1:length(deltas)){
    for (k in 1:NROW(xiChis)){
      for (l in 1:length(lambdas)){
          param <- ghypChangePars(3,2,
                     c(mus[i],deltas[j],xiChis[k,1],xiChis[k,2],lambdas[l]))
          ghypLargeShape[rownum,] <- param
          rownum <- rownum + 1
      }
    }
  }
}

### ghypSmallShape
mus <- 0
deltas <- 1
xis <- rep(c(0.1,0.3,0.7), c(1,2,4))
chis <- c(0,-0.25,0.25,-0.65,-0.3,0.3,0.65)
xiChis <- cbind(xis, chis)
lambdas <- c(-0.5,0,1)

maxrows <- length(mus)*length(deltas)*NROW(xiChis)*length(lambdas)
ghypSmallShape <- matrix(nrow=maxrows,ncol=5)
rownum <- 1
for (i in 1:length(mus)){
  for (j in 1:length(deltas)){
    for (k in 1:NROW(xiChis)){
      for (l in 1:length(lambdas)){
          param <- ghypChangePars(3,2,
                     c(mus[i],deltas[j],xiChis[k,1],xiChis[k,2],lambdas[l]))
          ghypSmallShape[rownum,] <- param
          rownum <- rownum + 1
      }
    }
  }
}


