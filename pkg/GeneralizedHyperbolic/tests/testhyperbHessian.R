require(GeneralizedHyperbolic)
require(minqa)

setwd("../R")
### Load in new functions
source("hyperbFitStart.R")
source("hyperbFit.R")
source("summary.hyperbFit.R")
source("hyperbHessian.R")

options(warn = 1)

paramSampleSize <- 1
sampleSize <- 100
trials <- 100


data(ghypParam)
testParam <- ghypSmallShape[,1:4]

np <- NROW(testParam)
paramNum <- sample(1:np, paramSampleSize, replace = FALSE)
paramVals <- matrix(testParam[paramNum,], ncol = 4)
nv <- NROW(paramVals)

hyperbSample <- matrix(nrow = sampleSize, ncol = trials)
fitResults <- vector("list", nv)
for (i in 1:nv){
  fitResults[[i]] <- vector("list", trials)
}

cat("##############################################\n")
for (i in 1:nv){
  param <- paramVals[i,]
  print(param)
  for (j in 1:trials) {
    cat("\n\nSample", j, "for parameter set", i, "\n")
    ## generating data
    hyperbSample[,j] <- rhyperb(sampleSize, param=param)
    ## fit hyperbolic
    fitResults[[i]][[j]] <-
      hyperbFit(hyperbSample[,j], criterion="MLE", method="BFGS",
                controlBFGS = list(maxit = 1000))
    print(summary.hyperbFit(fitResults[[i]][[j]], hessian = TRUE,
                            hessianMethod = "exact"))
  }
}

cat("##############################################\n")
for (i in 1:nv){
  param <- paramVals[i,]
  print(param)
  for (j in 1:trials) {
    cat("\n\nSample", j, "for parameter set", i, "\n")
    ## generating data
    hyperbSample[,j] <- rhyperb(sampleSize, param=param)
    ## fit hyperbolic
    fitResults[[i]][[j]] <-
      hyperbFit(hyperbSample[,j], criterion="MLE", method="BFGS",
                controlBFGS = list(maxit = 1000))
    print(summary.hyperbFit(fitResults[[i]][[j]], hessian = TRUE,
                            hessianMethod = "tsHessian"))
  }
}

cat("##############################################\n")
for (i in 1:nv){
  param <- paramVals[i,]
  print(param)
  for (j in 1:trials) {
    cat("\n\nSample", j, "for parameter set", i, "\n")
    ## generating data
    hyperbSample[,j] <- rhyperb(sampleSize, param=param)
    ## fit hyperbolic
    fitResults[[i]][[j]] <-
      hyperbFit(hyperbSample[,j], criterion="MLE", method="nlm")
    print(summary.hyperbFit(fitResults[[i]][[j]], hessian = TRUE,
                            hessianMethod = "exact"))
  }
}

cat("##############################################\n")
for (i in 1:nv){
  param <- paramVals[i,]
  print(param)
  for (j in 1:trials) {
    cat("\n\nSample", j, "for parameter set", i, "\n")
    ## generating data
    hyperbSample[,j] <- rhyperb(sampleSize, param=param)
    ## fit hyperbolic
    fitResults[[i]][[j]] <-
      hyperbFit(hyperbSample[,j], criterion="MLE", method="Nelder-Mead")
    print(summary.hyperbFit(fitResults[[i]][[j]], hessian = TRUE,
                            hessianMethod = "exact"))
  }
}



cat("##############################################\n")
for (i in 1:nv){
  param <- paramVals[i,]
  print(param)
  for (j in 1:trials) {
    cat("\n\nSample", j, "for parameter set", i, "\n")
    ## generating data
    hyperbSample[,j] <- rhyperb(sampleSize, param=param)
    ## fit hyperbolic
    fitResults[[i]][[j]] <-
      hyperbFit(hyperbSample[,j], criterion="MLE", method="L-BFGS-B",
                controlLBFGSB = list(maxit = 1000))
    print(summary.hyperbFit(fitResults[[i]][[j]], hessian = TRUE,
                            hessianMethod = "exact"))
  }
}
cat("##############################################\n")
for (i in 1:nv){
  param <- paramVals[i,]
  print(param)
  for (j in 1:trials) {
    cat("\n\nSample", j, "for parameter set", i, "\n")
    ## generating data
    hyperbSample[,j] <- rhyperb(sampleSize, param=param)
    ## fit hyperbolic
    fitResults[[i]][[j]] <-
      hyperbFit(hyperbSample[,j], criterion="MLE", method="nlminb",
                controlNLMINB = list(iter.max = 1000))
    print(summary.hyperbFit(fitResults[[i]][[j]], hessian = TRUE,
                            hessianMethod = "exact"))
  }
}

cat("##############################################\n")
for (i in 1:nv){
  param <- paramVals[i,]
  print(param)
  for (j in 1:trials) {
    cat("\n\nSample", j, "for parameter set", i, "\n")
    ## generating data
    hyperbSample[,j] <- rhyperb(sampleSize, param=param)
    ## fit hyperbolic
    fitResults[[i]][[j]] <-
      hyperbFit(hyperbSample[,j], criterion="MLE", method="bobyqa")
    print(summary.hyperbFit(fitResults[[i]][[j]], hessian = TRUE,
                            hessianMethod = "exact"))
  }
}

cat("##############################################\n")
for (i in 1:nv){
  param <- paramVals[i,]
  print(param)
  for (j in 1:trials) {
    cat("\n\nSample", j, "for parameter set", i, "\n")
    ## generating data
    hyperbSample[,j] <- rhyperb(sampleSize, param=param)
    ## fit hyperbolic
    fitResults[[i]][[j]] <-
      hyperbFit(hyperbSample[,j], criterion="MLE", method="constrOptim")
    print(summary.hyperbFit(fitResults[[i]][[j]], hessian = TRUE,
                            hessianMethod = "exact"))
  }
}

