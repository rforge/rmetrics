
#
# Example:
#   Urca Functions Addons
#
# Description:
#   This file summarizes the R functions from R's contributed
#   package 'urca' written by Bernhard Pfaff and the function 
#   'BIC' from the nlme package required from the 'urca' package.
#
#   In general you will not need the functions listed in this file.
#   Install the 'urca' package together with the 'nlme' package
#   and Rmetrics functions for unit root testing will run. However,
#   if you are working with an operating system for which no
#   compiled versions of these two packages are availalbe, you
#   can just execute the command
#
#       xmpfSeries() und select 'funUrca' from the menu list
#
#   This will load the functions to run the unit root tests.
#    


################################################################################
# PART I: Copy from nlme ...


BIC =
function (object, ...) 
{
    UseMethod("BIC")
}


BIC.logLik =
function(object, ...)
{
    # BIC for logLik objects
    ans = -2 * (c(object) - attr(object, "df") * log(attr(object, "nobs"))/2)
    
    # Return Value:
    ans
}


BIC.lm = 
function(object, ...)
{
    # BIC for various fitted objects
    if ((rt = nargs()) > 1) {
        object = list(object, ...)
        val = lapply(object, logLik)
        val = as.data.frame(t(sapply(val, 
            function(el) c(attr(el, "df"), BIC(el)))))
        names(val) = c("df", "BIC")
        row.names(val) = as.character(match.call()[-1])
    } else {
        val = BIC(logLik(object))
    }
    
    # Return Value:
    val
}


################################################################################
# PART II: Copy of urca ...


# Package: urca
# Version: 0.7-9
# Date: 2005-03-09
# Title: Unit root and cointegration tests for time series data
# Author: Bernhard Pfaff <bernhard.pfaff@pfaffikus.de>
# Maintainer: Bernhard Pfaff <bernhard@pfaffikus.de>
# Depends: R (>= 2.0.0)
# Imports: nlme, methods, graphics, stats
# LazyLoad: yes
# Description: Unit root and cointegration tests encountered in applied 
#    econometric analysis are implemented.
# License: GPL version 2 or newer
# URL: http://www.r-project.org 
# Packaged: Wed Mar  9 16:31:06 2005; bp



#
# Setting classes for unit root tests
#

setClass("urca", representation(test.name="character"))

setClass("ur.ers", representation(y="vector",
                                  yd="vector",
                                  type="character",
                                  model="character",
                                  lag="integer",
                                  cval="matrix",
                                  teststat="numeric",
                                  testreg="ANY"),
         contains="urca")

setClass("ca.jo", representation(x="ANY",
                                 Z0="matrix",
                                 Z1="matrix",
                                 ZK="matrix",
                                 type="character",
                                 model="character",
                                 const="logical",
                                 lag="integer",
                                 P="integer",
                                 season="ANY",
                                 dumvar="ANY",
                                 cval="ANY",
                                 teststat="ANY",
                                 lambda="vector",
                                 Vorg="matrix",
                                 V="matrix",
                                 W="matrix",
                                 PI="matrix",
                                 DELTA="matrix",
                                 GAMMA="matrix",
                                 R0="matrix",
                                 RK="matrix",
                                 bp="ANY"),
         contains="urca")

setClass("cajo.test", representation(Z0="matrix",
                                     Z1="matrix",
                                     ZK="matrix",
                                     H="ANY",
                                     A="ANY",
                                     B="ANY",
                                     type="character",
                                     const="logical",
                                     teststat="numeric",
                                     pval="vector",
                                     lambda="vector",
                                     Vorg="matrix",
                                     V="matrix",
                                     W="matrix",
                                     PI="matrix",
                                     DELTA="ANY",
                                     DELTA.bb="ANY",
                                     DELTA.ab="ANY",
                                     DELTA.aa.b="ANY",
                                     GAMMA="matrix"),
         contains="urca")

setClass("ur.kpss", representation(y="vector",
                                   type="character",
                                   lag="integer",
                                   cval="matrix",
                                   teststat="numeric",
                                   res="vector"),
         contains="urca")

setClass("ca.po", representation(z="ANY",
                                 type="character",
                                 model="character",
                                 lag="integer",
                                 cval="matrix",
                                 res="matrix",
                                 teststat="numeric",
                                 testreg="ANY"),
         contains="urca")

setClass("ur.pp", representation(y="vector",
                                 type="character",
                                 model="character",
                                 lag="integer",
                                 cval="matrix",
                                 teststat="numeric",
                                 testreg="ANY",
                                 auxstat="matrix",
                                 res="vector"),
         contains="urca")

setClass("ur.df", representation(y="vector",
                                 model="character",
                                 lags="integer",
                                 cval="matrix",
                                 res="vector",
                                 teststat="numeric",
                                 testreg="ANY"),
         contains="urca")

setClass("ur.sp", representation(y="vector",
                                 type="character",
                                 polynomial="integer",
                                 signif="numeric",
                                 teststat="numeric",
                                 cval="numeric",
                                 res="vector",
                                 testreg="ANY"),
         contains="urca")

setClass("ur.za", representation(y="vector",
                                 model="character",
                                 lag="integer",
                                 teststat="numeric",
                                 cval="vector",
                                 bpoint="integer",
                                 tstats="vector",
                                 res="vector",
                                 testreg="ANY"),
         contains="urca")

setClassUnion("otherornull", c("ANY", "character", "integer", "matrix", "vector", "NULL")) 

setClass("sumurca", representation(classname="character",
                                   test.name="character",
                                   testreg="ANY",
                                   teststat="otherornull",
                                   cval="otherornull",
                                   bpoint="otherornull",
                                   signif="otherornull",
                                   model="otherornull",
                                   type="otherornull",
                                   auxstat="otherornull",
                                   lag="otherornull",
                                   H="otherornull",
                                   A="otherornull",
                                   lambda="otherornull",
                                   pval="otherornull",
                                   V="otherornull",
                                   W="otherornull",
                                   P="otherornull")
         )
                                   



#
# Functions for unit root tests and cointegration analysis
#

#
# Elliott, Rothenberg & Stock-Test
#
ur.ers <- function(y, type=c("DF-GLS", "P-test"), model=c("constant", "trend"), lag.max=4){
  type <- match.arg(type)
  model <- match.arg(model)
  lag.max <- as.integer(lag.max)
  if(lag.max < 0){
    warning("\nlag.max bust be greater or equal to one and integer; setting lag.max=4")
  lag.max <- 4}
  lag.max <- lag.max+1
  idx <- 2:lag.max
  y <- na.omit(as.vector(y))
  nobs <- length(y)
  if(nobs < 50){
    rowsel <- 1
  }else if(nobs < 100){
    rowsel <- 2
  }else if(nobs <= 200){
    rowsel <- 3
  }else if(nobs > 200){
    rowsel <- 4}
  if(model=="constant"){
    ahat <- 1 - 7.0/nobs
    ya <- c(y[1], y[2:nobs]-ahat*y[1:(nobs-1)])
    za1 <- c(1, rep(1-ahat, nobs-1))
    yd.reg <- summary(lm(ya ~ -1 + za1))
    yd <- y - coef(yd.reg)[1]
  }else if(model=="trend"){
    ahat <- 1 - 13.5/nobs
    ya <- c(y[1], y[2:nobs]-ahat*y[1:(nobs-1)])
    za1 <- c(1, rep(1-ahat, nobs-1))
    trd <- 1:nobs
    za2 <- c(1, trd[2:nobs]-ahat*trd[1:(nobs-1)])
    yd.reg <- summary(lm(ya ~ -1 + za1 + za2))
    yd <- y - coef(yd.reg)[1] - coef(yd.reg)[2]*trd 
  }
  what <- function(x, z=y){
    z.l <-  z[1:(nobs-1)]
    z.diff <- diff(z)
    z.dlags <- embed(diff(z), x)[, -1]
    data.what <- data.frame(cbind(z.diff[-(1:(x-1))], z.l[-(1:(x-1))], z.dlags))
    bic <- BIC(lm(data.what))
    return(bic)
  }
  if(type=="P-test"){
    cvals.ptest <- array(c(1.87, 1.95, 1.91, 1.99, 2.97, 3.11, 3.17, 3.26, 3.91, 4.17, 4.33, 4.48, 4.22, 4.26, 4.05, 3.96, 5.72, 5.64, 5.66, 5.62, 6.77, 6.79, 6.86, 6.89), c(4, 3, 2))
    res <- residuals(yd.reg)
    if(model=="constant"){
      null.res <- c(0, diff(y))
      cvals <- as.matrix(t(cvals.ptest[rowsel, , 1]))
      model <- "with intercept"
    }else if(model=="trend"){
      null.res <- c(0, diff(y))
      null.res <- null.res - mean(null.res)
      cvals <- as.matrix(t(cvals.ptest[rowsel, , 2]))
      model <- "with intercept and trend"
    }
  sig.null <- sum(null.res^2)
  sig.res <- sum(res^2)
  if(lag.max > 1){
    bic <- sapply(idx, what, z=y)
    BIC.opt <- which.min(bic)+1
    y.l <-  y[1:(nobs-1)]
    y.diff <- diff(y)
    y.dlags <- embed(diff(y), BIC.opt)[, -1]
    data.what <- data.frame(cbind(y.diff[-(1:(BIC.opt-1))], y.l[-(1:(BIC.opt-1))], y.dlags))
    what.reg <- summary(lm(data.what))
    npar <- nrow(what.reg$coef)
    sumlc <- sum(what.reg$coef[3:npar,1])
    lag.max <- BIC.opt-1
  }else if(lag.max <= 1){
    y.diff <- diff(y)
    y.l <- y[1:(nobs-1)]
    what.reg <- summary(lm(y.diff ~ y.l))
    sumlc <- 0
    lag.max <- lag.max-1
  }
  what.sq <- what.reg$sigma^2/(1-sumlc)^2
  teststat <- (sig.res - ahat*sig.null)/what.sq
  test.reg <- NULL
  }else if(type=="DF-GLS"){
    if(model=="constant"){
      cvals <- as.matrix(t(c(-2.5658-1.960/nobs-10.04/(nobs**2),-1.9393-0.398/nobs,-1.6156-0.181/nobs)))
      model <- "with intercept"
    }else if(model=="trend"){
      cvals.dfgls.tau <- matrix(-1*c(3.77, 3.58, 3.46, 3.48, 3.19, 3.03, 2.93, 2.89, 2.89, 2.74, 2.64, 2.57), nrow=4, ncol=3)
      cvals <- as.matrix(t(cvals.dfgls.tau[rowsel,]))
      model <- "with intercept and trend"
    }
    yd.l <-  yd[1:(nobs-1)]
    yd.diff <- diff(yd)
    if(lag.max > 1){
      yd.dlags <- embed(diff(yd), lag.max)[, -1]
      data.dfgls <- data.frame(cbind(yd.diff[-(1:(lag.max-1))], yd.l[-(1:(lag.max-1))], yd.dlags))
      colnames(data.dfgls) <- c("yd.diff", "yd.lag", paste("yd.diff.lag", 1:(lag.max-1), sep=""))
      dfgls.form <- formula(paste("yd.diff ~ -1 + ", paste(colnames(data.dfgls)[-1], collapse=" + ")))
    }else if(lag.max <=1){
      data.dfgls <- data.frame(cbind(yd.diff, yd.l))
      colnames(data.dfgls) <- c("yd.diff", "yd.lag")
      dfgls.form <- formula("yd.diff ~ -1 + yd.lag")
    }
    dfgls.reg <- summary(lm(dfgls.form, data=data.dfgls))
    teststat <- coef(dfgls.reg)[1,3]
    test.reg <- dfgls.reg
    lag.max <- lag.max-1
  }
  colnames(cvals) <- c("1%", "5%", "10%")
  rownames(cvals) <- c("critical values")
  new("ur.ers", y=y, yd=yd, type=type, model=model, lag=as.integer(lag.max), cval=round(cvals, 2), teststat=teststat, testreg=test.reg, test.name="Elliot, Rothenberg \& Stock")
}
#
# Johansen Procedure
#
ca.jo <- function(x, type=c("eigen", "trace"), constant=FALSE, K=2, spec=c("longrun", "transitory"), season=NULL, dumvar=NULL, ctable=c("A1", "A2", "A3")){
  x <- as.matrix(x)
  type <- match.arg(type)
  spec <- match.arg(spec)
  ctable <- match.arg(ctable)
  K <- as.integer(K)
  P <- ncol(x)
  arrsel <- P 
  N <- nrow(x)
  if(!is.null(season)){
    s <- season - 1
  }else{
    s <- 0
  }
  if(N*P < P + s*P + K*P^2 + P*(P + 1)/2)
    stop("\nInsufficient degrees of freedom.\n")
  if(P > 5)
    warning("\nToo many variables, critical values cannot be computed.\n")
  if(!(is.null(season))){
    dum <- (diag(season) - 1/season)[,-season]
    dums <- dum
    while(nrow(dums) < N){
      dums <- rbind(dums, dum)}
    dums <- dums[1:N,]
    if(NA%in%x){
      idx.NA <- 1:N
      ind <- as.logical(sapply(idx.NA,  function(z) sum(is.na(x[z,])*1)))
      ind2 <- ind*(1:N)
      dums <- dums[-ind2,]
    }
  }
  if(!(is.null(dumvar))){
    dumvar <- as.matrix(dumvar)
    if(!(nrow(dumvar)==nrow(x))){
      stop("\nUnequal row length between dummy variables and x matrix.\n")
    }
    if(NA%in%x){
      idx.NA <- 1:N
      ind <- as.logical(sapply(idx.NA,  function(z) sum(is.na(x[z,])*1)))
      ind2 <- ind*(1:N)
      dumvar <- dumvar[-ind2,]
    }
  }
  x <- na.omit(x)
  N <- nrow(x)
  Z <- embed(diff(x), K)
  Z0 <- Z[,1:P]
  a1 <- array(c(2.816, 12.099, 18.697, 24.712, 30.774, 3.962, 14.036, 20.778, 27.169, 33.178, 6.936, 17.936, 25.521, 31.943, 38.341, 2.816, 13.338, 26.791, 43.964, 65.063, 3.962, 15.197, 29.509, 47.181, 68.905, 6.936, 19.310, 35.397, 53.792, 76.955), c(5, 3, 2))
  a2 <- array(c(6.691, 12.783, 18.959, 24.917, 30.818, 8.083, 14.595, 21.279, 27.341, 33.262, 11.576, 18.782, 26.154, 32.616, 38.858, 6.691, 15.583, 28.436, 45.248, 65.956, 8.083, 17.844, 31.256, 48.419, 69.977, 11.576, 21.962, 37.291, 55.551, 77.911), c(5, 3, 2))
  a3 <- array(c(7.563, 13.781, 19.796, 25.611, 31.592, 9.094, 15.752, 21.894, 28.167, 34.397, 12.740, 19.834, 26.409, 33.121, 39.672, 7.563, 17.957, 32.093, 49.925, 71.472, 9.094, 20.168, 35.068, 53.347, 75.328, 12.741, 24.988, 40.198, 60.054, 82.969), c(5, 3, 2))
  if(ctable=="A1"){
    cvals <- a1
  }else if(ctable=="A2"){
    cvals <- a2
  }else if(ctable=="A3"){
    cvals <- a3
  }                                         
  if(constant){
    if(spec=="longrun"){
      ZK <- cbind(x[-c((N-K+1):N),], 1)
    }else if(spec=="transitory"){
      ZK <- cbind(x[-N,], 1)[K:(N-1),]
    }
    Z1 <- Z[,-c(1:P)]
    P <- P + 1
    idx <- 0:(P-2)
    model <- "without linear trend and constant in cointegration"
  }else{
    Z1 <- Z[,-c(1:P)]
    Z1 <- cbind(1, Z1)
    if(spec=="longrun"){
      ZK <- x[-c((N-K+1):N),]
    }else if(spec=="transitory"){
      ZK <- x[-N,][K:(N-1),]
    }
    idx <- 0:(P-1)
    model <- "with linear trend"
  }
  N <- nrow(Z0)
  if(!(is.null(season))){
    Z1 <- cbind(Z1, dums[-(1:K),])
  }
  if(!(is.null(dumvar))){
    Z1 <- cbind(Z1, dumvar[-(1:K),])
  }
  M00 <- crossprod(Z0)/N
  M11 <- crossprod(Z1)/N
  MKK <- crossprod(ZK)/N
  M01 <- crossprod(Z0, Z1)/N
  M0K <- crossprod(Z0, ZK)/N
  MK0 <- crossprod(ZK, Z0)/N
  M10 <- crossprod(Z1, Z0)/N
  M1K <- crossprod(Z1, ZK)/N
  MK1 <- crossprod(ZK, Z1)/N
  M11inv <- solve(M11)
  R0 <- Z0 - t(M01%*%M11inv%*%t(Z1))
  RK <- ZK - t(MK1%*%M11inv%*%t(Z1))
  S00 <- M00 - M01%*%M11inv%*%M10
  S0K <- M0K - M01%*%M11inv%*%M1K
  SK0 <- MK0 - MK1%*%M11inv%*%M10
  SKK <- MKK - MK1%*%M11inv%*%M1K
  Ctemp <- chol(SKK, pivot=TRUE)
  pivot <- attr(Ctemp, "pivot")
  oo <- order(pivot)
  C <- t(Ctemp[,oo])
  Cinv <- solve(C)
  S00inv <- solve(S00)
  valeigen <- eigen(Cinv%*%SK0%*%S00inv%*%S0K%*%t(Cinv))
  lambda <- valeigen$values
  e <- valeigen$vector
  V <- t(Cinv)%*%e
  if(constant){
    rownames(V) <- c(colnames(x), "constant")
  }else{
    rownames(V) <- colnames(x)
  }
  Vorg <- V
  V <- sapply(1:P, function(x) V[,x]/V[1,x])
  W <- S0K%*%V%*%solve(t(V)%*%SKK%*%V)
  PI <- S0K%*%solve(SKK)
  DELTA <- S00 - S0K%*%V%*%solve(t(V)%*%SKK%*%V)%*%t(V)%*%SK0
  GAMMA <- M01%*%M11inv - PI%*%MK1%*%M11inv
  if(type=="trace"){
    type <- "trace statistic"
    teststat <- as.matrix(rev(sapply(idx, function(x) -N*sum(log(1 - lambda[(x+1):P])))))
    colnames(teststat) <- "trace"
    if(arrsel > 5){
      cval <- NULL
    }else{
      cval <- round(cvals[1:arrsel, ,2],2)
      colnames(cval) <- c("10\%", "5\%", "1\%")
      rownames(cval) <- c(paste("r <= ", (arrsel-1):1, " |",sep=""), "r = 0  |")
    }
  }else if(type=="eigen"){
    type <- "maximal eigenvalue statistic (lambda max)"
    teststat <- as.matrix(rev(sapply(idx, function(x) -N*log(1 - lambda[x+1]))))
    colnames(teststat) <- "lambda max."
    if(arrsel > 5){
      cval <- NULL
    }else{
      cval <- round(cvals[1:arrsel, ,1],2)
      colnames(cval) <- c("10\%", "5\%", "1\%")
      rownames(cval) <- c(paste("r <= ", (arrsel-1):1, " |",sep=""), "r = 0  |")
    }
  }
  new("ca.jo", x=x, Z0=Z0, Z1=Z1, ZK=ZK, type=type, model=model, const=constant, lag=K, P=arrsel, season=season, dumvar=dumvar, cval=cval, teststat=as.vector(teststat), lambda=lambda, Vorg=Vorg, V=V, W=W, PI=PI, DELTA=DELTA, GAMMA=GAMMA, R0=R0, RK=RK, bp=NA, test.name="Johansen-Procedure")
}
#
# auxiliary function for residuals' diagnostics and tests
#
alphaols <- function(z, reg.number = NULL) 
{
  if (!(class(z) == "ca.jo")) {
    stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
  }
  R0 <- z@R0
  RKV <- z@RK%*%z@V
  if (!is.null(reg.number)) {
    reg.number <- as.integer(reg.number)
    if (reg.number > ncol(z@Z0) || reg.number < 1) {
      stop("\nPlease, provide a valid number of the regression within \n the VECM, numbering from 1st to last row.\n")
    }
  return(lm(R0[, reg.number] ~ -1 + RKV))
  }
  else if (is.null(reg.number)) {
    return(lm(R0 ~ -1 + RKV))
  }
}
alrtest <- function(z, A, r){
  if(!(class(z)=="ca.jo")){
    stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
  }
  r <- as.integer(r)
  A <- as.matrix(A)
  if(!(nrow(A)==z@P)){
    stop("\nRow number of 'A' is unequal to VAR order.\n")
  }
  if(r >= z@P || r<1){
    stop("\nCount of cointegrating relationships is out of allowable range.\n")
  }
  type <- "Estimation and testing under linear restrictions on beta"
  B <- qr.Q(qr(A), complete=TRUE)[,-c(1:ncol(A))]
  N <- nrow(z@Z0)
  M00 <- crossprod(z@Z0)/N
  M11 <- crossprod(z@Z1)/N
  MKK <- crossprod(z@ZK)/N
  M01 <- crossprod(z@Z0, z@Z1)/N
  M0K <- crossprod(z@Z0, z@ZK)/N
  MK0 <- crossprod(z@ZK, z@Z0)/N
  M10 <- crossprod(z@Z1, z@Z0)/N
  M1K <- crossprod(z@Z1, z@ZK)/N
  MK1 <- crossprod(z@ZK, z@Z1)/N
  M11inv <- solve(M11)
  S00 <- M00 - M01%*%M11inv%*%M10
  S0K <- M0K - M01%*%M11inv%*%M1K
  SK0 <- MK0 - MK1%*%M11inv%*%M10
  SKK <- MKK - MK1%*%M11inv%*%M1K
  Sab <- t(A)%*%S00%*%B
  Skb <- t(S0K)%*%B
  Sbb <- t(B)%*%S00%*%B
  Sbbinv <- solve(Sbb)
  RA <- z@R0%*%A - z@R0%*%B%*%Sbbinv%*%t(Sab)
  RK <- z@RK - z@R0%*%B%*%Sbbinv%*%t(Skb)
  Saa.b <- crossprod(RA, RA)/N
  Sak.b <- crossprod(RA, RK)/N
  Ska.b <- crossprod(RK, RA)/N
  Skk.b <- crossprod(RK, RK)/N
  Ctemp <- chol(Skk.b, pivot=TRUE)
  pivot <- attr(Ctemp, "pivot")
  oo <- order(pivot)
  C <- t(Ctemp[,oo])
  Cinv <- solve(C)
  Saa.binv <- solve(Saa.b)
  valeigen <- eigen(Cinv%*%Ska.b%*%Saa.binv%*%Sak.b%*%t(Cinv))
  lambda.res <- valeigen$values
  e <- valeigen$vector
  V <- t(Cinv)%*%e
  V <- as.matrix(V[,1:r])
  Vorg <- V
  idx <- 1:r
  V <- sapply(idx, function(x) V[ , x] / V[1,x])
  PHI <- solve(t(A)%*%A)%*%Sak.b%*%Vorg
  ALPHA <- as.matrix(A%*%PHI)
  ALPHAorg <- ALPHA
  ALPHA <- sapply(idx, function(x) ALPHA[ , x] * Vorg[1,x])
  PI <- ALPHA %*% t(V)
  GAMMA <- M01%*%M11inv - PI%*%MK1%*%M11inv
  DELTA.bb <- Sbb
  DELTA.ab <- Sab - t(A)%*%ALPHA%*%t(V)%*%Skb
  DELTA.aa.b <- Saa.b - t(A)%*%ALPHA%*%t(ALPHA)%*%A
  lambda <- z@lambda
  teststat <- N*sum(log((1-lambda.res[1:r])/(1-lambda[1:r])))
  df <- r*(z@P - ncol(A))
  pval <- c(1-pchisq(teststat, df), df)
  new("cajo.test", Z0=z@Z0, Z1=z@Z1, ZK=z@ZK, const=z@const, H=NULL, A=A, B=B, type=type, teststat=teststat, pval=pval, lambda=lambda.res, Vorg=Vorg, V=V, W=ALPHA, PI=PI, DELTA=NULL, DELTA.bb=DELTA.bb, DELTA.ab=DELTA.ab, DELTA.aa.b=DELTA.aa.b, GAMMA=GAMMA, test.name="Johansen-Procedure")
}
ablrtest <- function(z, H, A, r){
    if(!(class(z)=="ca.jo")){
      stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
    }
    r <- as.integer(r)
    A <- as.matrix(A)
    H <- as.matrix(H)
    if(!(nrow(A)==z@P)){
      stop("\nRow number of 'A' is unequal to VAR order.\n")
    }
    if(r >= z@P || r<1){
      stop("\nCount of cointegrating relationships is out of allowable range.\n")
    }
    if(z@const==TRUE){
      P <- z@P + 1
    }else{
      P <- z@P
    }
    if(!(nrow(H)==P)){
      stop("\nRow number of 'H' is unequal to VAR order.\n")
    }
    type <- "Estimation and testing under linear restrictions on alpha and beta"
    N <- nrow(z@Z0)
    B <- qr.Q(qr(A), complete=TRUE)[,-c(1:ncol(A))]
    M00 <- crossprod(z@Z0)/N
    M11 <- crossprod(z@Z1)/N
    MKK <- crossprod(z@ZK)/N
    M01 <- crossprod(z@Z0, z@Z1)/N
    M0K <- crossprod(z@Z0, z@ZK)/N
    MK0 <- crossprod(z@ZK, z@Z0)/N
    M10 <- crossprod(z@Z1, z@Z0)/N
    M1K <- crossprod(z@Z1, z@ZK)/N
    MK1 <- crossprod(z@ZK, z@Z1)/N
    M11inv <- solve(M11)
    S00 <- M00 - M01%*%M11inv%*%M10
    S0K <- M0K - M01%*%M11inv%*%M1K
    SK0 <- MK0 - MK1%*%M11inv%*%M10
    SKK <- MKK - MK1%*%M11inv%*%M1K
    Sab <- t(A)%*%S00%*%B
    Skb <- t(S0K)%*%B
    Sbb <- t(B)%*%S00%*%B
    Sbbinv <- solve(Sbb)
    RA <- z@R0%*%A - z@R0%*%B%*%Sbbinv%*%t(Sab)
    RK <- z@RK - z@R0%*%B%*%Sbbinv%*%t(Skb)
    Saa.b <- crossprod(RA, RA)/N
    Sak.b <- crossprod(RA, RK)/N
    Ska.b <- crossprod(RK, RA)/N
    Skk.b <- crossprod(RK, RK)/N
    Ctemp <- chol(t(H)%*%Skk.b%*%H, pivot=TRUE)
    pivot <- attr(Ctemp, "pivot")
    oo <- order(pivot)
    C <- t(Ctemp[,oo])
    Cinv <- solve(C)
    Saa.binv <- solve(Saa.b)
    valeigen <- eigen(Cinv%*%t(H)%*%Ska.b%*%Saa.binv%*%Sak.b%*%H%*%t(Cinv))
    lambda.res <- valeigen$values
    e <- valeigen$vector
    V <- H%*%t(Cinv)%*%e
    Vorg <- V
    idx <- 1:r
    V <- sapply(idx, function(x) V[ , x] / V[1,x])
    PHI <- solve(t(A)%*%A)%*%Sak.b%*%Vorg
    ALPHA <- as.matrix(A%*%PHI)
    ALPHAorg <- ALPHA
    ALPHA <- sapply(idx, function(x) ALPHA[ , x] * Vorg[1,x])
    PI <- ALPHA %*% t(V)
    GAMMA <- M01%*%M11inv - PI%*%MK1%*%M11inv
    DELTA.bb <- Sbb
    DELTA.ab <- Sab - t(A)%*%ALPHA%*%t(V)%*%Skb
    DELTA.aa.b <- Saa.b - t(A)%*%ALPHA%*%t(ALPHA)%*%A
    lambda <- z@lambda
    teststat <- N*sum(log((1-lambda.res[1:r])/(1-lambda[1:r])))
    df <- r*(z@P - ncol(A)) + r*(z@P - ncol(H))
    pval <- c(1-pchisq(teststat, df), df)
    new("cajo.test", Z0=z@Z0, Z1=z@Z1, ZK=z@ZK, const=z@const, H=H, A=A, B=B, type=type, teststat=teststat, pval=pval, lambda=lambda.res, Vorg=Vorg, V=V, W=ALPHA, PI=PI, DELTA=NULL, DELTA.bb=DELTA.bb, DELTA.ab=DELTA.ab, DELTA.aa.b=DELTA.aa.b, GAMMA=GAMMA, test.name="Johansen-Procedure")
}
blrtest <- function(z, H, r){
  if(!(class(z)=="ca.jo")){
    stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
  }
  if(r >= z@P || r < 1){
    stop("\nCount of cointegrating relationships is out of allowable range.\n")
  }
  if(z@const==TRUE){
    P <- z@P + 1
  }else{
    P <- z@P
  }
  r <- as.integer(r)
  H <- as.matrix(H)
  if(!(nrow(H)==P)){
    stop("\nRow number of 'H' is unequal to VAR order.\n")
  }
  type <- "Estimation and testing under linear restrictions on beta"
  N <- nrow(z@Z0)
  M00 <- crossprod(z@Z0)/N
  M11 <- crossprod(z@Z1)/N
  MKK <- crossprod(z@ZK)/N
  M01 <- crossprod(z@Z0, z@Z1)/N
  M0K <- crossprod(z@Z0, z@ZK)/N
  MK0 <- crossprod(z@ZK, z@Z0)/N
  M10 <- crossprod(z@Z1, z@Z0)/N
  M1K <- crossprod(z@Z1, z@ZK)/N
  MK1 <- crossprod(z@ZK, z@Z1)/N
  M11inv <- solve(M11)
  S00 <- M00 - M01%*%M11inv%*%M10
  S0K <- M0K - M01%*%M11inv%*%M1K
  SK0 <- MK0 - MK1%*%M11inv%*%M10
  SKK <- MKK - MK1%*%M11inv%*%M1K
  Ctemp <- chol(t(H)%*%SKK%*%H, pivot=TRUE)
  pivot <- attr(Ctemp, "pivot")
  oo <- order(pivot)
  C <- t(Ctemp[,oo])
  Cinv <- solve(C)
  S00inv <- solve(S00)
  valeigen <- eigen(Cinv%*%t(H)%*%SK0%*%S00inv%*%S0K%*%H%*%t(Cinv))
  e <- valeigen$vector
  V <- H%*%t(Cinv)%*%e
  Vorg <- V
  idx <- ncol(V)
  V <- sapply(1:idx, function(x) V[,x]/V[1,x])
  W <- S0K%*%V%*%solve(t(V)%*%SKK%*%V)
  PI <- W %*% t(V)
  DELTA <- S00 - S0K%*%V%*%solve(t(V)%*%SKK%*%V)%*%t(V)%*%SK0
  GAMMA <- M01%*%M11inv - PI%*%MK1%*%M11inv
  lambda.res <- valeigen$values
  lambda <- z@lambda
  teststat <- N*sum(log((1-lambda.res[1:r])/(1-lambda[1:r])))
  df <- r*(P - ncol(H))
  pval <- c(1-pchisq(teststat, df), df)
  new("cajo.test", Z0=z@Z0, Z1=z@Z1, ZK=z@ZK, const=z@const, H=H, A=NULL, B=NULL, type=type, teststat=teststat, pval=pval, lambda=lambda.res, Vorg=Vorg, V=V, W=W, PI=PI, DELTA=DELTA, DELTA.bb=NULL, DELTA.ab=NULL, DELTA.aa.b=NULL, GAMMA=GAMMA, test.name="Johansen-Procedure")
}
bh5lrtest <- function (z, H, r) 
{
    if (!(class(z) == "ca.jo")) {
        stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
    }
    if (r >= z@P || r < 1) {
        stop("\nCount of cointegrating relationships is out of allowable range.\n")
    }
    if (z@const == TRUE) {
        P <- z@P + 1
    } else {
        P <- z@P
    }
    r <- as.integer(r)
    H <- as.matrix(H)
    if (!(nrow(H) == P)) {
        stop("\nRow number of 'H' is unequal to VAR order.\n")
    }
    if(ncol(H) > r - 1){
      stop("\nToo many columns in H for provided r.\n")
    }
    r1 <- ncol(H)
    r2 <- r - r1
    type <- "Estimation and testing under partly known beta"
    N <- nrow(z@Z0)
    M00 <- crossprod(z@Z0)/N
    M11 <- crossprod(z@Z1)/N
    MKK <- crossprod(z@ZK)/N
    M01 <- crossprod(z@Z0, z@Z1)/N
    M0K <- crossprod(z@Z0, z@ZK)/N
    MK0 <- crossprod(z@ZK, z@Z0)/N
    M10 <- crossprod(z@Z1, z@Z0)/N
    M1K <- crossprod(z@Z1, z@ZK)/N
    MK1 <- crossprod(z@ZK, z@Z1)/N
    M11inv <- solve(M11)
    S00 <- M00 - M01 %*% M11inv %*% M10
    S0K <- M0K - M01 %*% M11inv %*% M1K
    SK0 <- MK0 - MK1 %*% M11inv %*% M10
    SKK <- MKK - MK1 %*% M11inv %*% M1K
    S00.h <- S00 - S0K%*%H%*%solve(t(H)%*%SKK%*%H)%*%t(H)%*%SK0
    S0K.h <- S0K - S0K%*%H%*%solve(t(H)%*%SKK%*%H)%*%t(H)%*%SKK
    SK0.h <- SK0 - SKK%*%H%*%solve(t(H)%*%SKK%*%H)%*%t(H)%*%SK0
    SKK.h <- SKK - SKK%*%H%*%solve(t(H)%*%SKK%*%H)%*%t(H)%*%SKK
    valeigen <- eigen(SKK.h)
    C <- valeigen$vectors[ ,1:(P-r1)]%*%diag(1/sqrt(valeigen$values[1:(P-r1)]))
    valeigen <- eigen(t(C)%*%SK0.h%*%solve(S00.h)%*%S0K.h%*%C)
    PSI <- C%*%valeigen$vectors[,1:r2]
    Dtemp <- chol(t(H)%*%SKK%*%H, pivot = TRUE)
    pivot <- attr(Dtemp, "pivot")
    oo <- order(pivot)
    D <- t(Dtemp[, oo])
    Dinv <- solve(D)
    rho <- eigen(Dinv %*% t(H) %*% SK0 %*% solve(S00) %*% S0K %*% H %*% t(Dinv))
    Vorg <- cbind(H, PSI)
    idx <- ncol(PSI)
    PSI <- sapply(1:idx, function(x) PSI[, x]/PSI[1, x])
    V <- cbind(H, PSI)
    W <- S0K %*% V %*% solve(t(V) %*% SKK %*% V)
    PI <- W %*% t(V)
    DELTA <- S00 - S0K %*% V %*% solve(t(V) %*% SKK %*% V) %*% t(V) %*% SK0
    GAMMA <- M01 %*% M11inv - PI %*% MK1 %*% M11inv
    lambda.res <- valeigen$values
    lambda <- z@lambda
    teststat <- N *(sum(log(1 - rho$values[1:r1])) + sum(log(1-lambda.res[1:r2])) - sum(log(1 - lambda[1:r])))
    df <- (P - r)*r1
    pval <- c(1 - pchisq(teststat, df), df)
    new("cajo.test", Z0 = z@Z0, Z1 = z@Z1, ZK = z@ZK, const = z@const,         H = H, A = NULL, B = NULL, type = type, teststat = teststat, 
        pval = pval, lambda = lambda.res, Vorg = Vorg, V = V, 
        W = W, PI = PI, DELTA = DELTA, DELTA.bb = NULL, DELTA.ab = NULL,       DELTA.aa.b = NULL, GAMMA = GAMMA, test.name = "Johansen-Procedure")
}
bh6lrtest <- function (z, H, r, r1, conv.val=0.0001, max.iter=50) 
{
    if (!(class(z) == "ca.jo")) {
        stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
    }
    if (r >= z@P || r < 1) {
        stop("\nCount of cointegrating relationships is out of allowable range.\n")
    }
    if (z@const == TRUE) {
        P <- z@P + 1
    } else {
        P <- z@P
    }
    r <- as.integer(r)
    H <- as.matrix(H)
    if (!(nrow(H) == P)) {
        stop("\nRow number of 'H' is unequal to VAR order.\n")
    }
    s <- ncol(H)
    r2 <- r - r1
    lambda <- z@lambda
    type <- "Estimation and testing under partly known beta"
    N <- nrow(z@Z0)
    M00 <- crossprod(z@Z0)/N
    M11 <- crossprod(z@Z1)/N
    MKK <- crossprod(z@ZK)/N
    M01 <- crossprod(z@Z0, z@Z1)/N
    M0K <- crossprod(z@Z0, z@ZK)/N
    MK0 <- crossprod(z@ZK, z@Z0)/N
    M10 <- crossprod(z@Z1, z@Z0)/N
    M1K <- crossprod(z@Z1, z@ZK)/N
    MK1 <- crossprod(z@ZK, z@Z1)/N
    M11inv <- solve(M11)
    S00 <- M00 - M01 %*% M11inv %*% M10
    S0K <- M0K - M01 %*% M11inv %*% M1K
    SK0 <- MK0 - MK1 %*% M11inv %*% M10
    SKK <- MKK - MK1 %*% M11inv %*% M1K
    Dtemp <- chol(t(H)%*%SKK%*%H, pivot = TRUE)
    pivot <- attr(Dtemp, "pivot")
    oo <- order(pivot)
    D <- t(Dtemp[, oo])
    Dinv <- solve(D)
    valeigen <- eigen(Dinv %*% t(H) %*% SK0 %*% solve(S00) %*% S0K %*% H %*% t(Dinv))
    beta1 <- H%*%valeigen$vectors[,1:r1]
    i <- 0
    last <- 1
    diff <- 1
    while(diff > conv.val){
      S00.b1 <- S00 - S0K%*%beta1%*%solve(t(beta1)%*%SKK%*%beta1)%*%t(beta1)%*%SK0
      S0K.b1 <- S0K - S0K%*%beta1%*%solve(t(beta1)%*%SKK%*%beta1)%*%t(beta1)%*%SKK
      SK0.b1 <- SK0 - SKK%*%beta1%*%solve(t(beta1)%*%SKK%*%beta1)%*%t(beta1)%*%SK0
      SKK.b1 <- SKK - SKK%*%beta1%*%solve(t(beta1)%*%SKK%*%beta1)%*%t(beta1)%*%SKK
      valeigen <- eigen(SKK.b1)
      C <- valeigen$vectors[ ,1:(P-r1)]%*%diag(1/sqrt(valeigen$values[1:(P-r1)]))
      valeigen <- eigen(t(C)%*%SK0.b1%*%solve(S00.b1)%*%S0K.b1%*%C)
      lambda.res <- valeigen$values
      diff <- t(lambda.res-last)%*%(lambda.res-last)
      last <- lambda.res
      beta2 <- C%*%valeigen$vectors[,1:r2]
      S00.b2 <- S00 - S0K%*%beta2%*%solve(t(beta2)%*%SKK%*%beta2)%*%t(beta2)%*%SK0
      S0K.b2 <- S0K - S0K%*%beta2%*%solve(t(beta2)%*%SKK%*%beta2)%*%t(beta2)%*%SKK
      SK0.b2 <- SK0 - SKK%*%beta2%*%solve(t(beta2)%*%SKK%*%beta2)%*%t(beta2)%*%SK0
      SKK.b2 <- SKK - SKK%*%beta2%*%solve(t(beta2)%*%SKK%*%beta2)%*%t(beta2)%*%SKK
      valeigen <- eigen(t(H)%*%SKK.b2%*%H)
      C <- valeigen$vectors[ ,1:s]%*%diag(1/sqrt(valeigen$values[1:s]))
      valeigen <- eigen(t(C)%*%t(H)%*%SK0.b2%*%solve(S00.b2)%*%S0K.b2%*%H%*%C)
      beta1 <- H%*%valeigen$vectors[,1:r1]
      i <- i + 1
      if(i>max.iter){
        warning("\nNo convergence, used last iterations values.\n")
        break
      }
    }
    Vorg <- cbind(beta1, beta2)
    V <- Vorg
    idx <- ncol(V)
    V <- sapply(1:idx, function(x) V[, x]/V[1, x])
    W <- S0K %*% V %*% solve(t(V) %*% SKK %*% V)
    PI <- W %*% t(V)
    DELTA <- S00 - S0K %*% V %*% solve(t(V) %*% SKK %*% V) %*% t(V) %*% SK0
    GAMMA <- M01 %*% M11inv - PI %*% MK1 %*% M11inv
    Dtemp <- chol(t(beta1)%*%SKK%*%beta1, pivot = TRUE)
    pivot <- attr(Dtemp, "pivot")
    oo <- order(pivot)
    D <- t(Dtemp[, oo])
    Dinv <- solve(D)
    valeigen <- eigen(Dinv %*% t(beta1) %*% SK0 %*% solve(S00) %*% S0K %*% beta1 %*% t(Dinv))
    rho <- valeigen$values
    teststat <- N*(sum(log(1-rho[1:r1])) + sum(log(1-lambda.res[1:r2])) - sum(log(1-lambda[1:r])))
    df <- (P - s - r2)*r1
    pval <- c(1 - pchisq(teststat, df), df)
    new("cajo.test", Z0 = z@Z0, Z1 = z@Z1, ZK = z@ZK, const = z@const, H = H, A = NULL, B = NULL, type = type, teststat = teststat, pval = pval, lambda = lambda.res, Vorg = Vorg, V = V, W = W, PI = PI, DELTA = DELTA, DELTA.bb = NULL, DELTA.ab = NULL, DELTA.aa.b = NULL, GAMMA = GAMMA, test.name = "Johansen-Procedure")
}
cajools <- function(z, reg.number=NULL)
{
  if(!(class(z) == "ca.jo") && !(class(z) == "cajo.test")){
    stop("\nPlease, provide object of class 'ca.jo' or 'cajo.test' as 'z'.\n")
  }
  if (!is.null(reg.number)){
    reg.number <- as.integer(reg.number)
    if (reg.number > ncol(z@Z0) || reg.number < 1){
      stop("\nPlease, provide a valid number of the regression within \n the VECM, numbering from 1st to last row.\n")
    }
    return(lm(z@Z0[, reg.number] ~ z@Z1 + z@ZK - 1))
  }else if(is.null(reg.number)){
    return(lm(z@Z0 ~ z@Z1 + z@ZK -1))
  }
}
cajolst <- function (x, trend = TRUE, K = 2, season = NULL) 
{
    x <- as.matrix(x)
    K <- as.integer(K)
    if(K < 2){
      stop("\nK must be at least K=2.\n")
    }
    P <- ncol(x)
    arrsel <- P
    N <- nrow(x)
    if (!is.null(season)) {
        s <- season - 1
    }
    else {
        s <- 0
    }
    if (N * P < P + s * P + K * P^2 + P * (P + 1)/2) 
        stop("\nInsufficient degrees of freedom.\n")
    if (P > 5) 
        warning("\nToo many variables, critical values cannot be computed.\n")
    if (!(is.null(season))) {
        dum <- (diag(season) - 1/season)[, -season]
        dums <- dum
        while (nrow(dums) < N) {
            dums <- rbind(dums, dum)
        }
        dums <- dums[1:N, ]
        if (NA %in% x) {
            idx.NA <- 1:N
            ind <- as.logical(sapply(idx.NA, function(z) sum(is.na(x[z, 
                ]) * 1)))
            ind2 <- ind * (1:N)
            dums <- dums[-ind2, ]
        }
    }
    x2 <- na.omit(x)
    Ntot <- nrow(x2)
    y <- embed(x2, (K + 1))
    rhs <- y[, -c(1:P)]
    if (!trend) {
        rhs <- y[, -c(1:P)]
    }
    else {
        trd <- seq(K + 1, nrow(y) + K)
        rhs <- cbind(trd, y[, -c(1:P)])
    }
    N <- nrow(y)
    if (!(is.null(season))) {
        rhs <- cbind(dums[-(1:K), ], rhs)
    }
    lhs <- y[, 1:P]
    idx <- 1:(N - 1)
    tau <- function(t) {
        dt <- c(rep(0, t), rep(1, N - t))
        det(crossprod(resid(lm(lhs ~ dt + rhs))))
    }
    tau.hat <- sapply(idx, tau)
    tau.opt <- which.min(tau.hat) + K
    tau.bp <- tau.opt + 1
    dt <- c(rep(0, tau.opt), rep(1, N - tau.opt))
    if(!trend & is.null(season)){
      rhs.aux <- dt
    } else {
      rhs.aux <- cbind(dt, rhs[, -c((ncol(rhs)-K*ncol(x)+1):ncol(rhs))])
    }
    reg.opt <- lm(lhs ~ rhs.aux)
    dt <- c(rep(0, tau.opt), rep(1, Ntot - tau.opt))
    uv <- c(rep(1, Ntot))
    if (!trend) {
        if (!is.null(season)) {
            yfit <- x - uv%*%t(coef(reg.opt)[1, ]) - dt %*% t(coef(reg.opt)[2, ]) - dums %*% coef(reg.opt)[3:(2 + season - 1), ]
        }else{
            yfit <- x - uv%*%t(coef(reg.opt)[1, ]) - dt %*% t(coef(reg.opt)[2, ])
        }
    }else if (trend){
        trd <- 1:Ntot
        if (!is.null(season)) {
            yfit <- x - uv%*%t(coef(reg.opt)[1, ]) - dt %*% t(coef(reg.opt)[2, ]) - dums %*% coef(reg.opt)[3:(2 + season - 1), ] - trd %*% t(coef(reg.opt)[season + 2, ])
        }else{
            yfit <- x - uv%*%t(coef(reg.opt)[1, ]) - dt %*% t(coef(reg.opt)[2, ]) - trd %*% t(coef(reg.opt)[3, ])
        }
    }
    x <- na.omit(yfit)
    N <- nrow(x)
    spec <- "transitory"
    Z <- embed(diff(x), K)
    Z0 <- Z[, 1:P]
    Z1 <- Z[, -c(1:P)]
    ZK <- x[-N, ][K:(N - 1), ]
    idx <- 0:(P - 1)
    if (trend) {
      cvals <- matrix(c(5.423, 13.784, 25.931, 42.083, 61.918, 6.785, 15.826, 28.455, 45.204, 65.662, 10.042, 19.854, 33.757, 51.601, 73.116), nrow=5, ncol=3)
      model <- "with linear trend in shift correction"
    }else if(!trend){
      cvals <- matrix(c(2.996, 10.446, 21.801, 36.903, 55.952, 4.118, 12.276, 24.282, 40.067, 59.749, 6.888, 16.420, 29.467, 46.305, 67.170), nrow=5, ncol=3)
      model <- "without linear trend in shift correction"
    }
    N <- nrow(Z0)
    M00 <- crossprod(Z0)/N
    M11 <- crossprod(Z1)/N
    MKK <- crossprod(ZK)/N
    M01 <- crossprod(Z0, Z1)/N
    M0K <- crossprod(Z0, ZK)/N
    MK0 <- crossprod(ZK, Z0)/N
    M10 <- crossprod(Z1, Z0)/N
    M1K <- crossprod(Z1, ZK)/N
    MK1 <- crossprod(ZK, Z1)/N
    M11inv <- solve(M11)
    R0 <- Z0 - t(M01 %*% M11inv %*% t(Z1))
    RK <- ZK - t(MK1 %*% M11inv %*% t(Z1))
    S00 <- M00 - M01 %*% M11inv %*% M10
    S0K <- M0K - M01 %*% M11inv %*% M1K
    SK0 <- MK0 - MK1 %*% M11inv %*% M10
    SKK <- MKK - MK1 %*% M11inv %*% M1K
    Ctemp <- chol(SKK, pivot = TRUE)
    pivot <- attr(Ctemp, "pivot")
    oo <- order(pivot)
    C <- t(Ctemp[, oo])
    Cinv <- solve(C)
    S00inv <- solve(S00)
    valeigen <- eigen(Cinv %*% SK0 %*% S00inv %*% S0K %*% t(Cinv))
    lambda <- valeigen$values
    e <- valeigen$vector
    V <- t(Cinv) %*% e
    rownames(V) <- colnames(x)
    Vorg <- V
    V <- sapply(1:P, function(x) V[, x]/V[1, x])
    W <- S0K %*% V %*% solve(t(V) %*% SKK %*% V)
    PI <- S0K %*% solve(SKK)
    DELTA <- S00 - S0K %*% V %*% solve(t(V) %*% SKK %*% V) %*% 
        t(V) %*% SK0
    GAMMA <- M01 %*% M11inv - PI %*% MK1 %*% M11inv
    type <- "trace statistic"
    teststat <- as.matrix(rev(sapply(idx, function(x) N * sum(log(1 + lambda[(x + 1):P])))))
    colnames(teststat) <- "trace"
    if (arrsel > 5) {
      cval <- NULL
    } else {
      cval <- round(cvals[1:arrsel, ], 2)
      colnames(cval) <- c("10%", "5%", "1%")
      rownames(cval) <- c(paste("r <= ", (arrsel - 1):1, " |", sep = ""), "r = 0  |")
    }
    new("ca.jo", x = x, Z0 = Z0, Z1 = Z1, ZK = ZK, type = type,         model = model, const = FALSE, lag = K, P = arrsel, 
        season = season, dumvar = NULL, cval = cval, teststat = as.vector(teststat), 
        lambda = lambda, Vorg = Vorg, V = V, W = W, PI = PI, 
        DELTA = DELTA, GAMMA = GAMMA, R0 = R0, RK = RK, bp = tau.bp, 
        test.name = "Johansen-Procedure")
}
lttest <- function(z, r){
  if(!(class(z)=="ca.jo")){
    stop("\nObject 'x' must be of class 'ca.jo'\n")
  }
  r <- as.integer(r)
  if(r >= z@P || r < 1){
    stop("\nProvide number of cointegration vectors in valid range.\n")
  }
  idx <- r + 1
  df <- length(idx:z@P)
  N <- nrow(z@Z0)
  test1 <- ca.jo(z@x, constant=TRUE, K=z@lag, season=z@season)
  lambda1 <- test1@lambda
  test2 <- ca.jo(z@x, constant=FALSE, K=z@lag, season=z@season)
  lambda2 <- test2@lambda
  teststat <- -N*sum(log((1-lambda1[idx:z@P])/(1-lambda2[idx:z@P])))
  pval <- 1 - pchisq(teststat, df)
  lttest <- as.matrix(t(c(teststat, pval)))
  colnames(lttest) <- c("test statistic", "p-value")
  rownames(lttest) <- "LR test"
  cat("LR-test for no linear trend\n")
  cat("\n")
  cat(paste("H0: H*2(r<=", r, ")\n", sep=""))
  cat(paste("H1: H2(r<=", r, ")\n", sep=""))
  cat("\n")
  cat("Test statistic is distributed as chi-square\n")
  cat(paste("with", df, "degress of freedom\n", sep=" "))
  print(round(lttest, 2))
}
plotres <- function (x){
  if (!(class(x) == "ca.jo"))
    stop("\nObject is not of class 'ca.jo' \n")
  resids <- x@Z0 - x@Z1%*%t(x@GAMMA) - x@ZK%*%t(x@PI)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = c(1, 1))
  for (i in 1:x@P) {
    layout(matrix(c(1, 2, 1, 3), 2, 2))
    plot.ts(resids[, i], main = paste("Residuals of ", i, ". VAR regression", sep = ""), ylab = "", xlab = "")
    abline(h = 0, col = "red")
    acf(x@R0[, i], main = "Autocorrelations of Residuals")
    pacf(x@R0[, i], main = "Partial Autocorrelations of Residuals")
    if (interactive()){
      cat("\nType <Return> to continue: ")
      readline()
    }
  }
}
#
# KPSS-Test
#
ur.kpss <- function(y, type=c("mu", "tau"), lags=c("short", "long", "nil"), use.lag=NULL){
  y <- na.omit(as.vector(y))
  n <- length(y)
  type <- match.arg(type)
  lags <- match.arg(lags)
  if(!(is.null(use.lag))){
    lmax <- as.integer(use.lag)
    if(lmax < 0){
      warning("\nuse.lag has to be positive and integer; lags='short' used.")
    lmax <- trunc(4*(n/100)^0.25)}
  }else if(lags == "short"){
    lmax <- trunc(4*(n/100)^0.25)
  }else if(lags == "long"){
    lmax <- trunc(12*(n/100)^0.25)
  }else if(lags == "nil"){
    lmax <- 0
  }
  if(type=="mu"){
    cval <- as.matrix(t(c(0.347, 0.463, 0.574, 0.739)))
    colnames(cval) <- c("10%", "5%", "2.5%", "1%")
    rownames(cval) <- "critical values"
    res <- y - mean(y)
  }else if(type=="tau"){
    cval <- as.matrix(t(c(0.119, 0.146, 0.176, 0.216)))
    colnames(cval) <- c("10%", "5%", "2.5%", "1%")
    rownames(cval) <- "critical values"
    trend <- 1:n
    res <- residuals(lm(y ~ trend))
  }
  S <- cumsum(res)
  nominator <- sum(S^2)/n^2
  s2 <- sum(res^2)/n
  if(lmax == 0){
    denominator <- s2
  }else{
    index <- 1:lmax
    x.cov <- sapply(index, function(x) t(res[-c(1:x)])%*%res[-c((n-x+1):n)])
    bartlett <- 1-index/(lmax+1)
    denominator <- s2 + 2/n*t(bartlett)%*%x.cov
  }
  teststat <- nominator/denominator
  new("ur.kpss", y=y, type=type, lag=as.integer(lmax), teststat=as.numeric(teststat), cval=cval, res=res , test.name="KPSS") 
}
#
# Phillips-Ouliaris Test
#
ca.po <- function(z, demean=c("none", "constant", "trend"), lag=c("short", "long"), type=c("Pu", "Pz"), tol=NULL){
  z <- na.omit(as.matrix(z))
  if(ncol(z)<2 || ncol(z)>6){
    stop("Please provide a matrix with at least two and maximal six columns")}
  demean <- match.arg(demean)
  lag <- match.arg(lag)
  type <- match.arg(type)
  nobs <- nrow(z)
  m <- ncol(z)
  zl <- z[2:nobs,]
  zr <- z[1:(nobs-1),]
  nobs <- nobs-1
  if(lag == "short"){
    lmax <- trunc(4*(nobs/100)^0.25)
  }else if(lag == "long"){
    lmax <- trunc(12*(nobs/100)^0.25)
  }
  if(demean=="none"){
    ari3 <- 1
    model <- "none"
    res <- residuals(lm(zl ~ zr - 1))
    if(type=="Pu"){
      resu <- residuals(lm(z[,1] ~ z[,-1] -1))
      test.reg <- summary(lm(z[,1] ~ z[,-1] -1))
    }else if(type=="Pz"){
      test.reg <- summary(lm(zl ~ zr - 1))}
  }else if(demean=="constant"){
    ari3 <- 2
    model <- "with constant only"
    res <- residuals(lm(zl ~ zr))
    if(type=="Pu"){
      resu <- residuals(lm(z[,1] ~ z[,-1]))
      test.reg <- summary(lm(z[,1] ~ z[,-1]))
    }else if(type=="Pz"){
      test.reg <- summary(lm(zl ~ zr))}
  }else if(demean=="trend"){
    ari3 <- 3
    model <- "with constant and linear trend"
    trd <- 1:nobs
    res <- residuals(lm(zl ~ zr + trd))
    if(type=="Pu"){
      resu <- residuals(lm(z[,1] ~ z[,-1] + trd))
      test.reg <- summary(lm(z[,1] ~ z[,-1] + trd))
    }else if(type=="Pz"){
      test.reg <- summary(lm(zl ~ zr + trd))}
  }
  xi2 <- 1/nobs*t(res)%*%res
  index <- 1:lmax
  wsl <- 1-index/(lmax+1)
  xi.mat <- sapply(index, function(x){
    wsl[x]*(t(res[-c(1:x),])%*%res[-c((nobs-x+1):nobs),] +  t(res[-c((nobs-x+1):nobs),])%*%res[-c(1:x),])
  })
  xi.mat <- array(xi.mat, c(m, m, lmax))
  smat <- matrix(0, m, m)
  for(i in 1:lmax)
    smat <- smat + xi.mat[,,i]
  omega <- xi2 + 1/nobs*smat
  if(type=="Pz"){
    Mzz <- (1/nobs*t(zl)%*%zl)
    Mzzinv <- solve(Mzz, tol=tol)
    teststat <- nobs*sum(diag(omega%*%Mzzinv))
    cvals <- array(c(20.3933, 26.7022, 33.5359, 39.2826, 44.3725, 25.9711, 32.9392, 40.1220, 46.2691, 51.8614, 38.3413, 46.4097, 55.7341, 63.2149, 69.4939, 27.8536, 33.6955, 39.6949, 45.3308, 50.3537, 33.713, 40.5252, 46.7281, 53.2502, 57.7855, 48.0021, 53.8731, 63.4128, 71.5214, 76.7705, 41.2488, 46.1061, 52.0015, 57.3667, 61.6155, 48.8439, 53.8300, 60.2384, 65.8706, 70.7416, 65.1714, 69.2629, 78.3470, 84.5480, 91.0392)  , c(5, 3, 3))
    cval <- as.matrix(t(cvals[m-1, ,ari3]))
  }else if(type=="Pu"){
    w11 <- omega[1,1]
    w21 <- omega[2:m,1]
    O22 <- omega[-1,-1]
    O22inv <- solve(O22, tol=tol)
    w112 <- w11 - t(w21)%*%O22inv%*%w21
    ssqr <- 1/nobs*sum(resu^2)
    teststat <- as.numeric(nobs*w112/ssqr)
    cvals <- array(c(33.9267, 62.1436, 99.2664, 143.0775, 195.6202, 40.8217, 71.2751, 109.7426, 155.8019, 210.2910, 55.1911, 89.6679, 131.5716, 180.4845, 237.7723, 47.5877, 80.2034, 120.3035, 168.8572, 225.2303, 55.2202, 89.7619, 132.2207, 182.0749, 241.3316, 71.9273, 109.4525, 153.4504, 209.8054, 270.5018, 71.9586, 113.4929, 163.1050, 219.5098, 284.0100, 81.3812, 124.3933, 175.9902, 234.2865, 301.0949, 102.0167, 145.8644, 201.0905, 264.4988, 335.9054), c(5, 3, 3))
    cval <- as.matrix(t(cvals[m-1, ,ari3]))
    res <- as.matrix(resu)
  }
  colnames(cval) <- c("10\%", "5\%", "1\%")
  rownames(cval) <- "critical values"
  new("ca.po", z=z, type=type, model=model, lag=as.integer(lmax), cval=cval, res=res, teststat=teststat, testreg=test.reg, test.name="Phillips \& Ouliaris")
}
#
# Augmented-Dickey-Fuller Test
#
ur.df <- function (y, type = c("none", "drift", "trend"), lags = 1) 
{
    if (ncol(as.matrix(y)) > 1) 
        stop("\ny is not a vector or univariate time series.\n")
    if (any(is.na(y))) 
        stop("\nNAs in y.\n")
    lag <- as.integer(lags)
    if (lag < 0) 
        stop("\nLags must be set to an non negative integer value.\n")
    CALL <- match.call()
    DNAME <- deparse(substitute(y))
    type <- type[1]
    x.name <- deparse(substitute(y))
    lags <- lags + 1
    z <- diff(y)
    n <- length(z)
    x <- embed(z, lags)
    z.diff <- x[, 1]
    z.lag.1 <- y[lags:n]
    tt <- lags:n
    if (lags > 1) {
        z.diff.lag = x[, 2:lags]
        if (type == "none") {
            result <- lm(z.diff ~ z.lag.1 - 1 + z.diff.lag)
        }
        if (type == "drift") {
            result <- lm(z.diff ~ z.lag.1 + 1 + z.diff.lag)
        }
        if (type == "trend") {
            result <- lm(z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
        }
    }
    else {
        if (type == "none") {
            result <- lm(z.diff ~ z.lag.1 - 1)
        }
        if (type == "drift") {
            result <- lm(z.diff ~ z.lag.1 + 1)
        }
        if (type == "trend") {
            result <- lm(z.diff ~ z.lag.1 + 1 + tt)
        }
    }
    testreg <- summary(result)
    res <- residuals(testreg)
    if (type == "none") 
        coefNum <- 1
    else coefNum <- 2
    teststat <- testreg$coefficients[coefNum, 1]/testreg$coefficients[coefNum, 
        2]
    if (type == "none"){ 
        cval <- rbind(
          c(-2.66, -1.95, -1.60),
          c(-2.62, -1.95, -1.61),
          c(-2.6,  -1.95, -1.61),
          c(-2.58, -1.95, -1.62),
          c(-2.58, -1.95, -1.62),
          c(-2.58, -1.95, -1.62))
        tau <- 'tau1'
      }
    if (type == "drift"){ 
        cval <- rbind(
          c(-3.75, -3.00, -2.63),
          c(-3.58, -2.93, -2.60),
          c(-3.51, -2.89, -2.58),
          c(-3.46, -2.88, -2.57),
          c(-3.44, -2.87, -2.57),
          c(-3.43, -2.86, -2.57))
        tau <- 'tau2'
      }
    if (type == "trend"){ 
        cval <- rbind(
          c(-4.38, -3.60, -3.24),
          c(-4.15, -3.50, -3.18),
          c(-4.04, -3.45, -3.15),
          c(-3.99, -3.43, -3.13),
          c(-3.98, -3.42, -3.13),
          c(-3.96, -3.41, -3.12))
        tau <- 'tau3'
      }
    if(n < 25)
      cvals <- t(cval[1,])
    if(n < 50)
      cvals <- t(cval[2,])
    if(n < 100)
      cvals <- t(cval[3,])
    if(n < 250)
      cvals <- t(cval[4,])
    if(n <= 500)
      cvals <- t(cval[5,])
    if(n > 500)
      cvals <- t(cval[6,])
    colnames(cvals) <- c('1%', '5%', '10%')
    rownames(cvals) <- tau
   
    new("ur.df", y = y, model = type, cval=cvals, lags=lag, teststat = teststat, testreg=testreg, res=res, test.name="Augmented Dickey-Fuller Test")
}
#
# Phillips-Perron Test
#
ur.pp <- function(x, type=c("Z-alpha", "Z-tau"), model=c("constant", "trend"), lags=c("short", "long"), use.lag=NULL){
  x <- na.omit(as.vector(x))
  n <- length(x)
  y <- x[-1]
  y.l1 <- x[-n]
  n <- n-1
  lags <- match.arg(lags)
  model <- match.arg(model)
  type <- match.arg(type)
  if(!(is.null(use.lag))){
    lmax <- as.integer(use.lag)
    if(lmax < 0){
      warning("\nuse.lag has to be positive and integer; lags='short' used.")
      lmax <- trunc(4*(n/100)^0.25)}
  }else if(lags == "short"){
    lmax <- trunc(4*(n/100)^0.25)
  }else if(lags == "long"){
    lmax <- trunc(12*(n/100)^0.25)}
  if(model=="trend"){
    cval <- as.matrix(t(c(-3.9638-8.353/n-47.44/(n^2), -3.4126-4.039/n-17.83/(n^2), -3.1279-2.418/n-7.58/(n^2))))
    colnames(cval) <- c("1%", "5%", "10%")
    rownames(cval) <- "critical values"
    model <- "with intercept and trend"
    trend <- (1:n) - n/2
    test.reg <- summary(lm(y ~ y.l1 + trend))
    res <- residuals(test.reg)
    my.tstat <- coef(test.reg)[1, 3]
    beta.tstat <- coef(test.reg)[3, 3]
    res <- residuals(test.reg)
    s <- 1/n*(sum(res^2))
    myybar <- (1/n^2)*sum((y-mean(y))^2)
    myy <- (1/n^2)*sum(y^2)
    mty <- (n^(-5/2))*(t(1:n)%*%y)
    my <- (n^(-3/2))*sum(y)
    idx <- 1:lmax
    coprods <- sapply(idx, function(l) t(res[-c(1:l)])%*%(res[-c((n-l+1):n)]))
    weights <- 1 - idx/(lmax+1)
    sig <- s + (2/n)*(t(weights)%*%coprods)
    lambda <- 0.5*(sig-s)
    lambda.prime <- lambda/sig
    M <- (1-n^(-2))*myy - 12*mty^2 + 12*(1 + 1/n)*mty*my - (4 + 6/n + 2/n^2)*my^2
    my.stat <- sqrt(s/sig)*my.tstat - lambda.prime*sqrt(sig)*my/(sqrt(M)*sqrt((M+my^2)))
    beta.stat <- sqrt(s/sig)*beta.tstat - lambda.prime*sqrt(sig)*(0.5*my - mty)/(sqrt(M/12)*sqrt(myybar))
    aux.stat <- as.matrix(c(round(my.stat, 4), round(beta.stat, 4)))
    rownames(aux.stat) <- c("Z-tau-mu", "Z-tau-beta")
    colnames(aux.stat) <- "aux. Z statistics"
    if(type=="Z-tau"){
      tstat <- (coef(test.reg)[2, 1] - 1)/coef(test.reg)[2, 2]
      teststat <- sqrt(s/sig)*tstat-lambda.prime*sqrt(sig)/sqrt(M)
    }else if(type=="Z-alpha"){
      alpha <- coef(test.reg)[2, 1]
      teststat <- n*(alpha-1)-lambda/M
    }
  }else if(model=="constant"){
    cval <- as.matrix(t(c(-3.4335-5.999/n-29.25/(n^2), -2.8621-2.738/n-8.36/(n^2), -2.5671-1.438/n-4.48/(n^2))))
    colnames(cval) <- c("1%", "5%", "10%")
    rownames(cval) <- "critical values"
    model <- "with intercept"
    test.reg <- summary(lm(y ~ y.l1))
    my.tstat <- coef(test.reg)[1, 3]
    res <- residuals(test.reg)
    s <- 1/n*(sum(res^2))
    myybar <- (1/n^2)*sum((y-mean(y))^2)
    myy <- (1/n^2)*sum(y^2)
    my <- (n^(-3/2))*sum(y)
    idx <- 1:lmax
    coprods <- sapply(idx, function(l) t(res[-c(1:l)])%*%(res[-c((n-l+1):n)]))
    weights <- 1 - idx/(lmax+1)
    sig <- s + (2/n)*(t(weights)%*%coprods)
    lambda <- 0.5*(sig-s)
    lambda.prime <- lambda/sig
    my.stat <- sqrt(s/sig)*my.tstat + lambda.prime*sqrt(sig)*my/(sqrt(myy)*sqrt(myybar))
    aux.stat <- as.matrix(round(my.stat, 4))
    rownames(aux.stat) <- "Z-tau-mu"
    colnames(aux.stat) <- "aux. Z statistics"
    if(type=="Z-tau"){
      tstat <- (coef(test.reg)[2, 1] - 1)/coef(test.reg)[2, 2]
      teststat <- sqrt(s/sig)*tstat-lambda.prime*sqrt(sig)/sqrt(myybar)
    }else if(type=="Z-alpha"){
      alpha <- coef(test.reg)[2, 1]
      teststat <- n*(alpha-1)-lambda/myybar
    }
  }
  new("ur.pp", y=y, type=type, model=model, lag=as.integer(lmax), cval=cval, teststat=as.numeric(teststat), testreg=test.reg, auxstat=aux.stat, res=res, test.name="Phillips-Perron")
}
#
# Schmidt-Phillips Test
#
ur.sp <- function(y, type=c("tau", "rho"), pol.deg=c(1, 2, 3, 4), signif=c(0.01, 0.05, 0.1)){
  y <- na.omit(as.vector(y))
  type <- match.arg(type)
  signif.val <- c(0.01, 0.05, 0.1)
  if(!(signif %in% signif.val)){
    warning("\nPlease, provide as signif one of c(0.01, 0.05, 0.1); signif=0.01 used")
  signif <- 0.01}
  if(!(pol.deg %in% c(1:4))){
    warning("\nPlease, provide as polynomial degree one of c(1, 2, 3, 4); po.deg=1 used")
  pol.deg <- 1}
  n <- length(y)
  lag <- trunc(12*(n/100)^0.25)
  idx <- 1:lag
  trend1 <- 1:n
  y.diff <- diff(y)
  if(pol.deg==1){
    yi.hat <- (y[n]-y[1])/(n-1)
    phi.y <- y[1]-yi.hat
    S.hat <- y - phi.y - yi.hat*trend1
    S.hat.l1 <- S.hat[-n]
    test.reg <- summary(lm(y.diff ~ 1 + S.hat.l1))
    sp.data <- data.frame(cbind(y[2:n],  y[1:(n-1)], trend1[2:n]))
    colnames(sp.data) <- c("y", "y.lagged", "trend.exp1")
    corr.reg <- summary(lm(sp.data))
    res <- residuals(corr.reg)
    sig.eps <- (1/n)*sum(res^2)
    coprods <- sapply(idx, function(x) t(res[-c(1:x)])%*%(res[-c((n-x):(n-1))]))
    weights <- (2*(lag-idx)/lag)^2
    sig <- sig.eps + (2/n)*(t(weights)%*%coprods)
    omega2.hat <- sig.eps/sig
  }else if(pol.deg==2){
    trend2 <- trend1^2
    S.hat <- c(0, cumsum(residuals(summary(lm(y.diff ~ trend1[2:n])))))
    test.reg <- summary(lm(y.diff ~ S.hat[-n] + trend1[2:n]))
    sp.data <- data.frame(cbind(y[2:n],  y[1:(n-1)], trend1[2:n], trend2[2:n]))
    colnames(sp.data) <- c("y", "y.lagged", "trend.exp1", "trend.exp2")
    corr.reg <- summary(lm(sp.data))
    res <- residuals(corr.reg)
    sig.eps <- (1/n)*sum(res^2)
    coprods <- sapply(idx, function(x) t(res[-c(1:x)])%*%(res[-c((n-x):(n-1))]))
    weights <- (2*(lag-idx)/lag)^2
    sig <- sig.eps + (2/n)*(t(weights)%*%coprods)
    omega2.hat <- sig.eps/sig
  }else if(pol.deg==3){
    trend2 <- trend1^2
    trend3 <- trend1^3    
    S.hat <- c(0, cumsum(residuals(summary(lm(y.diff ~ trend1[2:n] + trend2[2:n])))))
    test.reg <- summary(lm(y.diff ~ S.hat[-n] + trend1[2:n] + trend2[2:n]))
    sp.data <- data.frame(cbind(y[2:n],  y[1:(n-1)], trend1[2:n], trend2[2:n], trend3[2:n]))
    colnames(sp.data) <- c("y", "y.lagged", "trend.exp1", "trend.exp2", "trend.exp3")
    corr.reg <- summary(lm(sp.data))
    res <- residuals(corr.reg)
    sig.eps <- (1/n)*sum(res^2)
    coprods <- sapply(idx, function(x) t(res[-c(1:x)])%*%(res[-c((n-x):(n-1))]))
    weights <- (2*(lag-idx)/lag)^2
    sig <- sig.eps + (2/n)*(t(weights)%*%coprods)
    omega2.hat <- sig.eps/sig
  }else if(pol.deg==4){
    trend2 <- trend1^2
    trend3 <- trend1^3
    trend4 <- trend1^4
    S.hat <- c(0, cumsum(residuals(summary(lm(y.diff ~ trend1[2:n] + trend2[2:n] + trend3[2:n])))))
    test.reg <- summary(lm(y.diff ~ S.hat[-n] + trend1[2:n] + trend2[2:n] + trend3[2:n]))
    sp.data <- data.frame(cbind(y[2:n],  y[1:(n-1)], trend1[2:n], trend2[2:n], trend3[2:n], trend4[2:n]))
    colnames(sp.data) <- c("y", "y.lagged", "trend.exp1", "trend.exp2", "trend.exp3", "trend.exp4")
    corr.reg <- summary(lm(sp.data))
    res <- residuals(corr.reg)
    sig.eps <- (1/n)*sum(res^2)
    coprods <- sapply(idx, function(x) t(res[-c(1:x)])%*%(res[-c((n-x):(n-1))]))
    weights <- (2*(lag-idx)/lag)^2
    sig <- sig.eps + (2/n)*(t(weights)%*%coprods)
    omega2.hat <- sig.eps/sig
  }
  if(type=="rho"){
    rho <- n*coef(test.reg)[2,1]
    teststat <- rho/omega2.hat
    cval <- .spcv(obs=n, type="rho", pol.deg=pol.deg, signif=signif)
  }else if(type=="tau"){
    tau <- coef(test.reg)[2,3]
    teststat <- tau/sqrt(omega2.hat)
    cval <- .spcv(obs=n, type="tau", pol.deg=pol.deg, signif=signif)
  }
  new("ur.sp", y=y, type=type, polynomial=as.integer(pol.deg), teststat=as.numeric(teststat), cval=cval, signif=signif, res=res, testreg=corr.reg, test.name="Schmidt-Phillips")
}
#
# Function for critical values of ur.sp
#
.spcv <- function(obs, type=c("tau", "rho"), pol.deg=c(1, 2, 3, 4), signif=c(0.01, 0.025, 0.05, 0.1)){
  obs <- as.integer(obs)
  obs.ranges <- c(25, 50, 100, 200, 500, 1000, 1e30)
  type <- match.arg(type)
  dim.1 <- which(obs.ranges >= obs, arr.ind=TRUE)[1]
  signif.val <- c(0.01, 0.05, 0.1)
  if(!(signif %in% signif.val)){
    warning("\nPlease, provide as signif one of c(0.01, 0.05, 0.1), signif=0.01 used")
    signif <- 0.01}
  dim.2 <- which(signif==signif.val, arr.ind=TRUE)
  if(!(pol.deg %in% c(1:4))){
    warning("\nPlease, provide as polynomial degree one of c(1, 2, 3, 4); pol.deg=1 used")
    pol.deg <- 1}
  dim.3 <- pol.deg
  if(type=="tau"){
  cvs.tau <- -1*c(3.9, 3.73, 3.63, 3.61, 3.59, 3.58, 3.56, 3.18, 3.11, 3.06, 3.04, 3.04, 3.02, 3.02, 2.85, 2.8, 2.77, 2.76, 2.76, 2.75, 2.75, 4.52, 4.28, 4.16, 4.12, 4.08, 4.06, 4.06, 3.78, 3.77, 3.65, 3.6, 3.55, 3.55, 3.53, 3.52, 3.41, 3.34, 3.31, 3.28, 3.26, 3.26, 3.26, 5.07, 4.73, 4.59, 4.53, 4.5, 4.49, 4.44, 4.26, 4.08, 4.03, 3.99, 3.96, 3.95, 3.93, 3.89, 3.77, 3.72, 3.69, 3.68, 3.68, 3.67, 5.57, 5.13, 4.99, 4.9, 4.85, 4.83, 4.81, 4.7, 4.47, 4.39, 4.33, 4.31, 4.31, 4.29, 4.3, 4.15, 4.1, 4.06, 4.03, 4.03, 4.01)
  cv.array <- array(cvs.tau, dim=c(7, 3, 4), dimnames=c("obs", "signif", "pol.deg"))
  cval <- cv.array[dim.1, dim.2, dim.3]
}else if(type=="rho"){
    cvs.rho <- -1*c(20.4, 22.8, 23.8, 24.8, 25.3, 25.3, 25.2, 15.7, 17.0, 17.5, 17.9, 18.1, 18.1, 18.1, 13.4, 14.3, 14.6, 14.9, 15.0, 15.0, 15.0, 24.6, 28.4, 30.4, 31.8, 32.4, 32.5, 32.6, 20.1, 22.4, 23.7, 24.2, 24.8, 24.6, 24.7, 17.8, 19.5, 20.4, 20.7, 21.0, 21.1, 21.1, 28.1, 33.1, 36.3, 38.0, 39.1, 39.5, 39.7, 23.8, 27.0, 29.1, 30.1, 30.6, 30.8, 30.6, 21.5, 24.0, 25.4, 26.1, 26.6, 26.7, 26.7, 31.0, 37.4, 41.8, 44.0, 45.3, 45.7, 45.8, 26.9, 31.2, 34.0, 35.2, 36.2, 36.6, 36.4, 24.7,28.1, 30.2, 31.2, 31.8, 32.0, 31.9)
  cv.array <- array(cvs.rho, dim=c(7, 3, 4), dimnames=c("obs", "signif", "pol.deg"))
  cval <- cv.array[dim.1, dim.2, dim.3]}
  return(cval)
}
#
# Zivot-Andrews Test
#
ur.za <- function(y, model=c("intercept", "trend", "both"), lag){
  y <- na.omit(as.vector(y))
  n <- length(y)
  model <- match.arg(model)
  lag <- as.integer(lag)
  if(length(lag) > 1 || lag < 1){
    warning("\nPlease, specify maximal number of lags for differenced series as positive integer; lags=1 is now used.")
    lags <- 1}
  datmat <- matrix(NA, n, lag + 3)
  if(n < ncol(datmat) + 2){
    stop("\nInsufficient number of obeservations.")}
  idx <- 1:(n-1)
  trend <- seq(1, n)
  datmat[,1] <- y
  datmat[,2] <- c(NA, y)[1:n]
  datmat[,3] <- trend
  for(i in 1:lag){
    datmat[ , i + 3] <- c(rep(NA, i + 1), diff(y))[1:n]}
  datmat <- as.data.frame(datmat)
  colnames(datmat) <- c("y", "y.l1", "trend", paste("y.dl", 1:lag, sep=""))
  if(model=="intercept"){
    roll <- function(z){
      du <- c(rep(0, z), rep(1, (n-z)))
      rollmat <- cbind(datmat, du)
      roll.reg <- coef(summary(lm(rollmat)))
      (roll.reg[2,1]-1.0)/roll.reg[2,2]
    }
    roll.stat <- sapply(idx, roll)
    cval <- c(-5.34, -4.8, -4.58)
    bpoint <- which.min(roll.stat)
    du <- c(rep(0, bpoint), rep(1, (n-bpoint)))
    testmat <- cbind(datmat, du)
    test.reg <- summary(lm(testmat)) 
  }else if(model=="trend"){
    roll <- function(z){
      dt <- c(rep(0, z), 1:(n-z))
      rollmat <- cbind(datmat, dt)
      roll.reg <- coef(summary(lm(rollmat)))
      (roll.reg[2,1]-1.0)/roll.reg[2,2]
    }
    roll.stat <- sapply(idx, roll)
    cval <- c(-4.93, -4.42, -4.11)
    bpoint <- which.min(roll.stat)
    dt <- c(rep(0, bpoint), 1:(n-bpoint))
    testmat <- cbind(datmat, dt)
    test.reg <- summary(lm(testmat)) 
  }else if(model=="both"){
    test.reg <- summary(lm(datmat))
    roll <- function(z){
      du <- c(rep(0, z), rep(1, (n-z)))
      dt <- c(rep(0, z), 1:(n-z))
      rollmat <- cbind(datmat, du, dt)
      roll.reg <- coef(summary(lm(rollmat)))
      (roll.reg[2,1]-1.0)/roll.reg[2,2]
    }
    roll.stat <- sapply(idx, roll)
    cval <- c(-5.57, -5.08, -4.82)
    bpoint <- which.min(roll.stat)
    du <- c(rep(0, bpoint), rep(1, (n-bpoint)))
    dt <- c(rep(0, bpoint), 1:(n-bpoint))
    testmat <- cbind(datmat, du, dt)
    test.reg <- summary(lm(testmat)) 
  }
  teststat <- roll.stat[bpoint]
  new("ur.za", y=y, model=model, lag=lag, teststat=teststat, cval=cval, bpoint=bpoint, tstats=roll.stat, res=test.reg$residuals, testreg=test.reg, test.name="Zivot-Andrews")
}
#
# Setting methods for classes
#
show.urca <- function(object){
  title <- paste("#", object@test.name, "Unit Root / Cointegration Test #", sep=" ")
  row <- paste(rep("#", nchar(title)), collapse="")
  cat("\n")
  cat(row, "\n")
  cat(title, "\n")
  cat(row, "\n")
  cat("\n")
  cat("The value of the test statistic is:", round(object@teststat, 4), "\n")
  cat('\n')
}

setMethod("show", "ur.kpss", show.urca)
setMethod("show", "ca.jo", show.urca)
setMethod("show", "cajo.test", show.urca)
setMethod("show", "ca.po", show.urca)
setMethod("show", "ur.pp", show.urca)
setMethod("show", "ur.df", show.urca)
setMethod("show", "ur.sp", show.urca)
setMethod("show", "ur.za", show.urca)
setMethod("show", "ur.ers", show.urca)

setMethod("summary", "ur.ers", function(object){
  return(new("sumurca", classname="ur.ers", test.name=object@test.name, testreg=object@testreg, teststat=object@teststat, cval=object@cval, bpoint=NULL, signif=NULL, model=object@model, type=object@type, auxstat=NULL, lag=NULL, H=NULL, A=NULL, lambda=NULL, pval=NULL, V=NULL, W=NULL, P=NULL))  
})

setMethod("summary", "ca.jo", function(object){
  return(new("sumurca", classname="ca.jo", test.name=object@test.name, testreg=NULL, teststat=object@teststat, cval=object@cval, bpoint=NULL, signif=NULL, model=object@model, type=object@type, auxstat=NULL, lag=NULL, H=NULL, A=NULL, lambda=object@lambda, pval=NULL, V=object@V, W=object@W, P=object@P))
})

setMethod("summary", "cajo.test", function(object){
  return(new("sumurca", classname="cajo.test", test.name=object@test.name, testreg=NULL, teststat=object@teststat, cval=NULL, bpoint=NULL, signif=NULL, model=NULL, type=object@type, auxstat=NULL, lag=NULL, H=object@H, A=object@A, lambda=object@lambda, pval=object@pval, V=object@V, W=object@W, P=NULL))
})

setMethod("summary", "ur.kpss", function(object){
  return(new("sumurca", classname="ur.kpss", test.name=object@test.name, testreg=NULL, teststat=object@teststat, cval=object@cval, bpoint=NULL, signif=NULL, model=NULL, type=object@type, auxstat=NULL, lag=object@lag, H=NULL, A=NULL, lambda=NULL, pval=NULL, V=NULL, W=NULL, P=NULL))
})

setMethod("summary", "ca.po", function(object){
  return(new("sumurca", classname="ca.po", test.name=object@test.name, testreg=object@testreg, teststat=object@teststat, cval=object@cval, bpoint=NULL, signif=NULL, model=object@model, type=object@type, auxstat=NULL, lag=NULL, H=NULL, A=NULL, lambda=NULL, pval=NULL, V=NULL, W=NULL, P=NULL))
})

setMethod("summary", "ur.pp", function(object){
  return(new("sumurca", classname="ur.pp", test.name=object@test.name, testreg=object@testreg, teststat=object@teststat, cval=object@cval, bpoint=NULL, signif=NULL, model=object@model, type=object@type, auxstat=object@auxstat, lag=NULL, H=NULL, A=NULL, lambda=NULL, pval=NULL, V=NULL, W=NULL, P=NULL))
})

setMethod("summary", "ur.df", function(object){
  return(new("sumurca", classname="ur.df", test.name=object@test.name, testreg=object@testreg, teststat=object@teststat, cval=object@cval, bpoint=NULL, signif=NULL, model=object@model, type=NULL, auxstat=NULL, lag=NULL, H=NULL, A=NULL, lambda=NULL, pval=NULL, V=NULL, W=NULL, P=NULL))
})

setMethod("summary", "ur.sp", function(object){
  return(new("sumurca", classname="ur.sp", test.name=object@test.name, testreg=object@testreg, teststat=object@teststat, cval=object@cval, bpoint=NULL, signif=object@signif, model=NULL, type=NULL, auxstat=NULL, lag=NULL, H=NULL, A=NULL, lambda=NULL, pval=NULL, V=NULL, W=NULL, P=NULL))
})

setMethod("summary", "ur.za", function(object){
  return(new("sumurca", classname="ur.za", test.name=object@test.name, testreg=object@testreg, teststat=object@teststat, cval=object@cval, bpoint=object@bpoint, signif=NULL, model=NULL, type=NULL, auxstat=NULL, lag=NULL, H=NULL, A=NULL, lambda=NULL, pval=NULL, V=NULL, W=NULL, P=NULL))
})

setMethod("show", "sumurca", function(object){
  if(object@classname=="ur.za"){
    title <- paste("#", object@test.name, "Unit Root Test #", sep=" ")
    row <- paste(rep("#", nchar(title)), collapse="")
    cat("\n")
    cat(row, "\n")
    cat(title, "\n")
    cat(row, "\n")
    cat("\n")
    print(object@testreg)
    cat('\n')
    cat('Teststatistic:', round(object@teststat, 4), '\n')
    cat('Critical values: 0.01=', object@cval[1], '0.05=', object@cval[2], '0.1=', object@cval[3], '\n')
    cat('\n')
    cat('Potential break point at position:', object@bpoint, '\n')
    cat('\n')
    invisible(object)
  }else if(object@classname=="ur.sp"){
    title <- paste("#", object@test.name, "Unit Root Test #", sep=" ")
    row <- paste(rep("#", nchar(title)), collapse="")
    cat("\n")
    cat(row, "\n")
    cat(title, "\n")
    cat(row, "\n")
    cat("\n")
    print(object@testreg)
    cat('\n')
    cat('Value of test-statistic is:', round(object@teststat, 4), '\n')
    cat('Critical value for a significance level of', object@signif, '\n')
    cat('is:', object@cval, '\n')
    cat('\n')
    invisible(object)
  }else if(object@classname=="ur.pp"){
    title <- paste("#", object@test.name, "Unit Root Test #", sep=" ")
    row <- paste(rep("#", nchar(title)), collapse="")
    cat("\n")
    cat(row, "\n")
    cat(title, "\n")
    cat(row, "\n")
    cat("\n")
    cat('Test regression', object@model, '\n')
    cat('\n')
    print(object@testreg)
    cat('\n')
    cat('Value of test-statistic, type:', object@type,' is:', round(object@teststat, 4), '\n')
    cat('\n')
    print(object@auxstat)
    cat('\n')
    cat('Critical values for Z statistics: \n')
    print(object@cval)
    cat('\n')
    invisible(object)
  }else if(object@classname=="ur.df"){
    title <- paste("#", object@test.name, "Unit Root Test #", sep=" ")
    row <- paste(rep("#", nchar(title)), collapse="")
    cat("\n")
    cat(row, "\n")
    cat(title, "\n")
    cat(row, "\n")
    cat("\n")
    cat('Test regression', object@model, '\n')
    cat('\n')
    print(object@testreg)
    cat('\n')
    cat('Value of test-statistic is:', round(object@teststat, 4), '\n')
    cat('\n')
    cat('Critical values for test statistics: \n')
    print(object@cval)
    cat('\n')
    invisible(object)
  }else if(object@classname=="ca.po"){
    title <- paste("#", object@test.name, "Unit Root Test #", sep=" ")
    row <- paste(rep("#", nchar(title)), collapse="")
    cat("\n")
    cat(row, "\n")
    cat(title, "\n")
    cat(row, "\n")
    cat("\n")
    cat("Test of type", object@type, "\ndetrending of series", object@model, "\n")
    cat("\n")
    print(object@testreg)
    cat('\n')
    cat('Value of test-statistic is:', round(object@teststat, 4), '\n')
    cat('\n')
    cat('Critical values of', object@type, "are:\n")
    print(object@cval)
    cat('\n')
    invisible(object)
    }else if(object@classname=="ur.kpss"){
      title <- paste("#", object@test.name, "Unit Root Test #", sep=" ")
      row <- paste(rep("#", nchar(title)), collapse="")
      cat("\n")
      cat(row, "\n")
      cat(title, "\n")
      cat(row, "\n")
      cat("\n")
      cat('Test is of type:', object@type, 'with', object@lag, 'lags. \n')
      cat('\n')
      cat('Value of test-statistic is:', round(object@teststat, 4), '\n')
      cat('\n')
      cat('Critical value for a significance level of: \n')
      print(object@cval)
      cat('\n')
      invisible(object)
    }else if(object@classname=="cajo.test"){
      title <- paste("#", object@test.name, "#", sep=" ")
      row <- paste(rep("#", nchar(title)), collapse="")
      cat("\n")
      cat(row, "\n")
      cat(title, "\n")
      cat(row, "\n")
      cat("\n")
      cat(object@type, "\n")
      cat("\n")
      cat("The VECM has been estimated subject to: \n")
      cat("beta=H*phi and/or alpha=A*psi\n")
      if(!is.null(object@H)){
        cat("\n")
        print(object@H)
        cat("\n")
      }
      if(!is.null(object@A)){
        cat("\n")
        print(object@A)
        cat("\n")
      }
      cat("Eigenvalues of restricted VAR (lambda):\n")
      print(round(object@lambda, 4))
      cat('\n')
      cat("The value of the likelihood ratio test statistic:\n")
      cat(round(object@teststat, 2), "distributed as chi square with", object@pval[2], "df.\n")
      cat("The p-value of the test statistic is:", round(object@pval[1], 2), "\n")
      cat("\n")
      cat("Eigenvectors, normalised to first column\n")
      cat("of the restricted VAR:\n")
      cat("\n")
      print(round(object@V, 4))
      cat("\n")
      cat("Weights W of the restricted VAR:\n")
      cat("\n")
      print(round(object@W, 4))
      cat("\n")
      invisible(object)
    }else if(object@classname=="ca.jo"){
      title <- paste("#", object@test.name, "#", sep=" ")
      row <- paste(rep("#", nchar(title)), collapse="")
      cat("\n")
      cat(row, "\n")
      cat(title, "\n")
      cat(row, "\n")
      cat("\n")
      cat("Test type:", object@type, ",", object@model, "\n")
      cat("\n")
      cat("Eigenvalues (lambda):\n")
      print(object@lambda)
      cat('\n')
      if(!(is.null(object@cval))){
        res1 <- as.matrix(round(object@teststat, 2))
        colnames(res1) <- "test"
        result <- cbind(res1, object@cval)
        cat("Values of teststatistic \& critical values of test:\n")
        cat("\n")
        print(result)
        cat("\n")
      }else{
        cat("Values of test statistic\n")
        cat("\n")
        result <- as.matrix(object@teststat)
        rownames(result) <- c(paste("r <= ", (object@P-1):1, " |",sep=""), "r = 0  |")
        print(result)
        cat("\n")
        invisible(x)
      }
      cat("Eigenvectors, normalised to first column:\n")
      cat("(These are the cointegration relations)\n")
      cat("\n")
      print(object@V)
      cat("\n")
      cat("Weights W:\n")
      cat("(This is the loading matrix)\n")
      cat("\n")
      print(object@W)
      cat("\n")
      invisible(object)
    }else if(object@classname=="ur.ers"){
      title <- paste("#", object@test.name, "Unit Root Test #", sep=" ")
      row <- paste(rep("#", nchar(title)), collapse="")
      cat("\n")
      cat(row, "\n")
      cat(title, "\n")
      cat(row, "\n")
      cat("\n")
      cat("Test of type", object@type, "\ndetrending of series", object@model, "\n")
      cat("\n")
      if(!is.null(object@testreg)){
        print(object@testreg)
        cat('\n')
      }
      cat('Value of test-statistic is:', round(object@teststat, 4), '\n')
      cat('\n')
      cat('Critical values of', object@type, "are:\n")
      print(object@cval)
      cat('\n')
      invisible(object)
    }
})

setMethod("plot", signature(x="ur.ers", y="missing"), function(x){
  if(is.null(x@testreg)){
    stop("No plot method for P-test available")}
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  layout(matrix(c(1, 2, 3, 1, 2, 4), 3 , 2))
  suppressWarnings(plot.ts(diff(x@yd)[-c(1:x@lag)], main="Diagram of fit for test regression", sub=paste("detrending ", x@model, " and ", x@lag, " lagged differences used in test regression",  sep=""), ylab="Actual and fitted values", xlab=""))
  lines(diff(x@yd)[-c(1:x@lag)] - resid(x@testreg), col="seagreen")
  plot.ts(resid(x@testreg), main="Residuals", ylab="", xlab="")
  abline(h=0, col="red")
  acf(resid(x@testreg), main="Autocorrelations of Residuals")
  pacf(resid(x@testreg), main="Partial Autocorrelations of Residuals")
})

setMethod("plot", signature(x="ca.jo", y="missing"), function(x){
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(2,1))
  if(x@P==nrow(x@V)){
    ci <- x@x%*%x@V
  }else{
    ci <- x@x%*%x@V[-(x@P+1),]
  }
  for( i in 1:x@P){
    plot.ts(x@x[,i], main=paste("Time series plot of y", i, sep=""), ylab="")
    plot.ts(ci[,i], main=paste("Cointegration relation of ", i, ". variable", sep=""), ylab="")
    if(interactive()){
      cat("\nType <Return> to continue: ")
      readline()
    }
  }
})

setMethod("plot", signature(x="ur.kpss", y="missing"), function(x){
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  layout(matrix(c(1, 2, 1, 3), 2 , 2))
  plot.ts(x@res, main=paste("Residuals from test regression of type:", x@type, " with", x@lag, "lags", sep=" "), ylab="residuals", xlab="")
  abline(h=0, col="red")
  acf(x@res, main="Autocorrelations of Residuals")
  pacf(x@res, main="Partial Autocorrelations of Residuals")
})

setMethod("plot", signature(x="ca.po", y="missing"), function(x){
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  if(x@type=="Pu"){
    layout(matrix(c(1, 2, 1, 3), 2 , 2))
    suppressWarnings(plot.ts(x@res[,1], main="Residuals of CI-regression for y1", sub=paste("detrending:", x@model, sep=" "), ylab="", xlab=""))
    abline(h=0, col="red")
    acf(x@res[,1], main="Autocorrelations of Residuals")
    pacf(x@res[,1], main="Partial Autocorrelations of Residuals")
  }else if(x@type=="Pz"){
    m <- ncol(x@z)
    for( i in 1:m){
      layout(matrix(c(1, 2, 1, 3), 2 , 2))
      suppressWarnings(plot.ts(x@res[,i], main=paste("Residuals of CI-regression with y", i, " as lhs", sep=""), sub=paste("detrending:", x@model, sep=" "), ylab="", xlab=""))
      abline(h=0, col="red")
      acf(x@res[,i], main="Autocorrelations of Residuals")
      pacf(x@res[,i], main="Partial Autocorrelations of Residuals")
      if(interactive()){
        cat("\nType <Return> to continue: ")
        readline()
      }
    }
  }     
})

setMethod("plot", signature(x="ur.pp", y="missing"), function(x){
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  layout(matrix(c(1, 2, 3, 1, 2, 4), 3 , 2))
  plot.ts(x@y[-1], main=paste("Diagram of fit for model", x@model, sep=" "), ylab="Actual and fitted values", xlab="")
  lines(x@y - x@res, col="seagreen")
  plot.ts(x@res, main="Residuals", ylab="", xlab="")
  abline(h=0, col="red")
  acf(x@res, main="Autocorrelations of Residuals")
  pacf(x@res, main="Partial Autocorrelations of Residuals")
})

setMethod("plot", signature(x="ur.df", y="missing"), function(x){
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  layout(matrix(c(1, 2, 3, 1, 2, 4), 3 , 2))
  if(x@lags <= 1){
    plot.ts(x@y[-1], main=paste("Diagram of fit for model", x@model, sep=" "), ylab="Actual and fitted values", xlab="")
    lines(x@y[-1] - x@res, col="seagreen")
  }else{
    plot.ts(x@y[-c(1:(x@lags+1))], main=paste("Diagram of fit for model", x@model, sep=" "), ylab="Actual and fitted values", xlab="")
    lines(x@y[-c(1:(x@lags+1))] - x@res, col="seagreen")
  }
  plot.ts(x@res, main="Residuals", ylab="", xlab="")
  abline(h=0, col="red")
  acf(x@res, main="Autocorrelations of Residuals")
  pacf(x@res, main="Partial Autocorrelations of Residuals")
})

setMethod("plot", signature(x="ur.sp", y="missing"), function(x){
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  layout(matrix(c(1, 2, 3, 1, 2, 4), 3 , 2))
  plot.ts(x@y[-1], main=paste("Diagram of fit for model with polynomial degree of ", x@polynomial, sep="") , ylab="Actual and fitted values", xlab="")
  lines(x@y[-1] - x@res, col="seagreen")
  plot.ts(x@res, main="Residuals", ylab="", xlab="")
  abline(h=0, col="red")
  acf(x@res, main="Autocorrelations of Residuals")
  pacf(x@res, main="Partial Autocorrelations of Residuals")
})

setMethod("plot", signature(x="ur.za", y="missing"), function(x){
  oldpar <- par(no.readonly=TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  yvals <- sort(c(x@cval, x@tstats))
  n <- length(x@y)
  xvals <- pretty(1:n)
  plot.ts(x@tstats, main="Zivot and Andrews Unit Root Test", ylab="t-statistics for lagged endogenous variable", ylim=c(min(yvals), max(yvals)))
  abline(h=x@cval, col=c("red", "blue", "seagreen"))
  if(x@teststat < x@cval[3]){
    abline(v=x@bpoint, col="red", lty=2)}
  mtext(paste("Model type:", x@model, sep=" "), side=1, line=4)
  legend(x=n, y=max(yvals), c("1% c.v.", "2.5% c.v.", "5% c.v."), col=c("red", "blue", "seagreen"), xjust=1, yjust=1, lty=1, horiz=TRUE, cex=0.66, bty="n")
})
  

################################################################################

