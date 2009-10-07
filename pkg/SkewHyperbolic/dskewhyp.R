######Density function###################################################
dskewhyp <- function (x, mu = 0, delta = 1, beta = 1, nu = 1,
                      param = c(mu,delta,beta,nu),
                      log = FALSE, tolerance = .Machine$double.eps ^ 0.5,
                      logPars = FALSE){


    if (length(param) !=4)
        stop("param vector must contain 4 values")

    param<-as.numeric(param)

    mu<-param[1]
    delta<-param[2]
    beta<-param[3]
    nu<-param[4]

    if( delta < 0) stop("Delta must be greater than 0")
    if( nu < 0 ) stop("Nu must be greater than 0")
    #working density
    if (log == FALSE){
        if (abs(beta) > tolerance) {         #beta !=0
           dskewhyp <-(2^((1-nu)/2)*delta^nu*abs(beta)^((nu+1)/2)*
                       besselK(x=sqrt(beta^2*(delta^2+(x-mu)^2)),nu=(nu+1)/2 )*
                      exp(beta*(x-mu)))/ (gamma(nu/2)*sqrt(pi)*
                                           (sqrt(delta^2+(x-mu)^2))^((nu+1)/2))
       } else  {
            dskewhyp <-((gamma((nu+1)/2))/(sqrt(pi)*delta*gamma(nu/2)))*
               (1+((x-mu)^2)/(delta^2))^(-(nu+1)/2)
        }
    }
#####################
    #trial density using exponential scaling
    #if(log == FALSE){
    #    if (abs(beta) > tolerance) {         #beta !=0
    #       dskewhyp <-(2^((1-nu)/2)*delta^nu*abs(beta)^((nu+1)/2)*
    #                   besselK(x=sqrt(beta^2*(delta^2+(x-mu)^2)),nu=(nu+1)/2 )*
    #                  exp(beta*delta*((x-mu)/delta - sqrt(1 +
    #                  ((x - mu)/delta)^2))))/
    #                   (gamma(nu/2)*sqrt(pi)*
    #                    (sqrt(delta^2+(x-mu)^2))^((nu+1)/2))
    #   } else  {
    #        dskewhyp <-((gamma((nu+1)/2))/(sqrt(pi)*delta*gamma(nu/2)))*
    #           (1+((x-mu)^2)/(delta^2))^(-(nu+1)/2)
    #    }
    #}
###################

    if (log == TRUE){

        if(abs(beta) > tolerance){
            dskewhyp <- ((1-nu)/2)*log(2) + nu*log(delta) +
                ((nu+1)/2)*log(abs(beta)) + log(besselK(x=sqrt(beta^2*
                (delta^2+(x-mu)^2)),nu=(nu+1)/2)) +
                beta*(x-mu) - lgamma(nu/2) - log(sqrt(pi)) -
                ((nu+1)/2)*log(sqrt(delta^2 + (x - mu)^2))
    }else{
        dskewhyp <- lgamma((nu+1)/2) - log(sqrt(pi)) - log(delta) -
            lgamma(nu/2) - ((nu+1)/2)*log(1 + ((x - mu)^2)/delta^2)
    }
    dskewhyp <- replace(dskewhyp,dskewhyp == -Inf, NA)
}
return(dskewhyp)
}


######Distribution function##############################################
pskewhyp <- function(q, mu = 0, delta = 1, beta = 1, nu = 1,
                     param = c(mu,delta,beta,nu), log = FALSE,
                     lower.tail = TRUE, small = 10^(-6), tiny = 10^(-10),
                     subdivisions = 100, accuracy = FALSE, ...){

    if (length(param) != 4)
        stop("param vector must contain 4 values")

    param <- as.numeric(param)

    mu <- param[1]
    delta <- param[2]
    beta <- param[3]
    nu <- param[4]

    if( delta < 0) stop("Delta must be greater than 0")
    if( nu < 0 ) stop("Nu must be greater than 0")

    if (log == TRUE)
        stop("This function is not yet implemented")

    bks <- skewhypBreaks(param=param,small=small,tiny=tiny,...)
    xTiny <- bks$xTiny
    xSmall <- bks$xSmall
    lowBreak <- bks$lowBreak
    highBreak <- bks$highBreak
    xLarge <- bks$xLarge
    xHuge <- bks$xHuge
    modeDist <- bks$modeDist

    qSort <- sort(q)
    qTiny <- which(qSort < xTiny)
    qSmall <- which(qSort < xSmall)
    qLow <- which(qSort < lowBreak)
    qLessEqMode <- which(qSort <= modeDist)
    qGreatMode <- which(qSort > modeDist)
    qHigh <- which(qSort > highBreak)
    qLarge <- which(qSort > xLarge)
    qHuge <- which(qSort > xHuge)

    if (length(qLow) > 0)#beware empty groups
        qLessEqMode <- qLessEqMode[qLessEqMode > max(qLow)]
    if (length(qHigh) > 0)
        qGreatMode <- qGreatMode[qGreatMode < min(qHigh)]
    if (length(qSmall) > 0)
        qLow <- qLow[qLow > max(qSmall)]
    if (length(qLarge) > 0)
        qHigh <- qHigh[qHigh < min(qLarge)]
    if (length(qTiny) > 0)
        qSmall <- qSmall[qSmall > max(qTiny)]
    if (length(qHuge) > 0)
        qLarge <- qLarge[qLarge < min(qHuge)]

    intFun <- rep(NA, length(q))
    if (length(qTiny) > 0)  intFun[qTiny] <- 0
    if (length(qHuge) > 0)  intFun[qHuge] <- 1

    intErr <- rep(NA, length(q))
    if (length(qTiny) > 0)  intErr[qTiny] <- tiny
    if (length(qHuge) > 0)  intErr[qHuge] <- tiny

    dskewhypInt <- function(q) dskewhyp(q, param = param, log = FALSE)

    resSmall <- safeIntegrate(dskewhypInt, xTiny, xSmall, subdivisions, ...)
    resLarge <- safeIntegrate(dskewhypInt, xLarge, xHuge, subdivisions, ...)
    intSmall <- resSmall$value
    intLarge <- resLarge$value
    errSmall <- tiny + resSmall$abs.error
    errLarge <- tiny + resLarge$abs.error

    resLow <- safeIntegrate(dskewhypInt, xSmall, lowBreak, subdivisions, ...)
    resHigh <- safeIntegrate(dskewhypInt, highBreak, xLarge, subdivisions,...)
    intLow <- intSmall + resLow$value
    intHigh <- intLarge + resHigh$value
    errLow <- errSmall + resLow$abs.error
    errHigh <- errLarge + resHigh$abs.error

    for (i in qSmall) {
        intRes <- safeIntegrate(dskewhypInt, xTiny, qSort[i],subdivisions,...)
        intFun[i] <- intRes$value
        intErr[i] <- intRes$abs.error + tiny
    }
    for (i in qLarge) {
        intRes <- safeIntegrate(dskewhypInt, qSort[i], xHuge,subdivisions,...)
        intFun[i] <- 1 - intRes$value
        intErr[i] <- intRes$abs.error + tiny
    }
    for (i in qLow) {
        intRes <- safeIntegrate(dskewhypInt, xSmall,qSort[i],subdivisions,...)
        intFun[i] <- intRes$value + intSmall
        intErr[i] <- intRes$abs.error + errSmall
    }
    for (i in qHigh) {
        intRes <- safeIntegrate(dskewhypInt, qSort[i],xLarge,subdivisions,...)
        intFun[i] <- 1 - intRes$value - intLarge
        intErr[i] <- intRes$abs.error + errLarge
    }
    for (i in qLessEqMode) {
        intRes <- safeIntegrate(dskewhypInt,lowBreak,qSort[i],subdivisions,...)
        intFun[i] <- intRes$value + intLow
        intErr[i] <- intRes$abs.error + errLow
    }
    for (i in qGreatMode) {
        intRes <- safeIntegrate(dskewhypInt, qSort[i], highBreak,
                                subdivisions,...)
        intFun[i] <- 1 - intRes$value - intHigh
        intErr[i] <- intRes$abs.error + errLarge
    }

    if(lower.tail) {
        ifelse((accuracy), return(list(value = intFun[rank(q)],
                error = intErr[rank(q)])), return(intFun[rank(q)]))
    }else{
        ifelse((accuracy),return(list(value = 1 - intFun[rank(q)],
                error = intErr[rank(q)])), return(1 - intFun[rank(q)]))
    }
}

######Quantile Function####################################################
qskewhyp <- function(p, mu = 0, delta = 1, beta = 1, nu = 1,
                     param = c(mu,delta,beta,nu), small = 10^(-6),
                     tiny = 10^(-10), deriv = 0.3, nInterpol = 100,
                     subdivisions = 100, ...) {

    if (length(param) != 4)
        stop("param vector must contain 4 values")
    if( p<0 )
        stop("probabilities must be between 0 and 1")
    if( p>1)
        stop("probabilities must be between 0 and 1")

    param <- as.numeric(param)

    mu <- param[1]
    delta <- param[2]
    beta <- param[3]
    nu <- param[4]

    if( delta < 0) stop("Delta must be greater than 0")
    if( nu < 0 ) stop("Nu must be greater than 0")

    bks <- skewhypBreaks(param=param, small=small, tiny=tiny, deriv=deriv)
    xTiny <- bks$xTiny
    xSmall <- bks$xSmall
    lowBreak <- bks$lowBreak
    highBreak <- bks$highBreak
    xLarge <- bks$xLarge
    xHuge <- bks$xHuge
    modeDist <- bks$modeDist

    yTiny <- pskewhyp(xTiny, param=param)
    ySmall <- pskewhyp(xSmall, param=param)
    yLowBreak <- pskewhyp(lowBreak, param=param)
    yModeDist <- pskewhyp(modeDist, param=param)
    yHighBreak <- pskewhyp(highBreak, param=param)
    yLarge <- pskewhyp(xLarge, param=param)
    yHuge <- pskewhyp(xLarge, param=param)

    pSort <- sort(p)
    pTiny <- which(pSort < yTiny)
    pSmall <- which(pSort < ySmall)
    pLow <- which(pSort < yLowBreak)
    pLessMode <- which(pSort <= yModeDist)
    pGreatMode <- which(pSort > yModeDist)
    pHigh <- which(pSort > yHighBreak)
    pLarge <- which(pSort > yLarge)
    pHuge <- which(pSort > yHuge)

    if(length(pTiny > 0)) pSmall <- pSmall[pSmall > max(pTiny)]
    if(length(pSmall > 0)) pLow <- pLow[pLow > max(pSmall)]
    if(length(pLow > 0)) pLessMode <- pLessMode[pLessMode > max(pLow)]
    if(length(pHigh > 0)) pGreatMode <- pGreatMode[pGreatMode < min(pHigh)]
    if(length(pLarge > 0)) pHigh <- pHigh[pHigh < min(pLarge)]
    if(length(pHuge > 0)) PLarge <- pLarge[pLarge < min(pHuge)]
    qSort <- rep(NA, length(pSort))
    if(length(pTiny > 0)) qSort[pTiny] <- -Inf
    if(length(pHuge > 0)) qSort[pHuge] <- Inf


    #for each group work out quantiles
    if(length(pTiny > 0)){######should we have this section shouldnt they all be -Inf?????????????????????????????????????????
        for(i in pTiny){
            zeroFun <- function(x) pskewhyp(x, param=param) - pSort[i]
            interval <- c(xTiny - (xSmall - xTiny), xTiny)
            while(zeroFun(interval[1])*zeroFun(interval[2]) > 0){
                interval[1] <- interval[1] - (xSmall - xTiny)
            }
            qSort[i] <- uniroot(zeroFun,interval)$root
        }
    }

    if(length(pSmall > 0)){
        xValues <- seq(xTiny,xSmall, length.out = nInterpol)
        pskewhypValues <- pskewhyp(xValues, param=param, small=small,
                      tiny=tiny,subdivisions=subdivisions, accuracy=FALSE)
        pskewhypSpline <- splinefun(xValues, pskewhypValues)
        for(i in pSmall){
            zeroFun <- function(x) pskewhypSpline(x) - pSort[i]
            if(zeroFun(xTiny) >= 0) qSort[i] <- xTiny
            else{
                if(zeroFun(xSmall) <= 0) qSort[i] <- xSmall
                else qSort[i] <-
                    uniroot(zeroFun, interval=c(xTiny,xSmall),...)$root
            }
        }
    }
    if(length(pLow > 0)){
        xValues <- seq(xSmall, lowBreak, length.out=nInterpol)
        pskewhypValues <- pskewhyp(xValues, param=param, small=small,
                      tiny=tiny, subdivisions=subdivisions, accuracy=FALSE)
        pskewhypSpline <- splinefun(xValues, pskewhypValues)
        for (i in pLow){
            zeroFun <- function(x) pskewhypSpline(x) - pSort[i]
            if(zeroFun(xSmall) >= 0) qSort[i] <- xSmall
            else{
                if(zeroFun(lowBreak) <= 0) qSort[i] <- lowBreak
                else qSort[i] <-
                    uniroot(zeroFun, interval=c(xSmall,lowBreak),...)$root
               }
        }
    }
    if(length(pLessMode > 0)){
        xValues <- seq(lowBreak, modeDist, length.out=nInterpol)
        pskewhypValues <- pskewhyp(xValues, param=param, small=small,
                      tiny=tiny, subdivisions=subdivisions, accuracy=FALSE)
        pskewhypSpline <- splinefun(xValues, pskewhypValues)
        for(i in pLessMode){
            zeroFun <- function(x) pskewhypSpline(x) - pSort[i]
            if(zeroFun(lowBreak) >= 0) qSort[i] <- lowBreak
            else{
                if(zeroFun(modeDist) <= 0) qSort[i] <- modeDist
                else qSort[i] <-
                    uniroot(zeroFun, interval=c(lowBreak,modeDist),...)$root
            }
        }

    }
    if(length(pGreatMode > 0)){
        xValues <- seq(modeDist, highBreak, length.out=nInterpol)
        pskewhypValues <- pskewhyp(xValues, param=param, small=small,
                      tiny=tiny, subdivisions=subdivisions, accuracy=FALSE)
        pskewhypSpline <- splinefun(xValues, pskewhypValues)
        for(i in pGreatMode){
            zeroFun <- function(x) pskewhypSpline(x) - pSort[i]
            if(zeroFun(modeDist) >= 0) qSort[i] <- modeDist
            else{
                if(zeroFun(highBreak) <= 0) qSort[i] <- highBreak
                else qSort[i] <-
                    uniroot(zeroFun, interval=c(modeDist,highBreak),...)$root
            }
        }
    }
    if(length(pHigh > 0)){
        xValues <- seq(highBreak, xLarge, length.out=nInterpol)
        pskewhypValues <- pskewhyp(xValues, param=param, small=small,
                      tiny=tiny, subdivisions=subdivisions, accuracy=FALSE)
        pskewhypSpline <- splinefun(xValues, pskewhypValues)
        for(i in pHigh){
            zeroFun <- function(x) pskewhypSpline(x) - pSort[i]
            if(zeroFun(highBreak) >= 0) qSort[i] <- highBreak
            else{
                if(zeroFun(xLarge) <= 0) qSort[i] <- xLarge
                else qSort[i] <-
                    uniroot(zeroFun, interval=c(highBreak,xLarge),...)$root
            }
        }
    }
    if(length(pLarge > 0)){
        xValues <- seq(xLarge, xHuge, length.out=nInterpol)
        pskewhypValues <- pskewhyp(xValues, param=param, small=small,
                      tiny=tiny, subdivisions=subdivisions, accuracy=FALSE)
        pskewhypSpline <- splinefun(xValues, pskewhypValues)
        for(i in pLarge){
            zeroFun <- function(x) pskewhypSpline(x) - pSort[i]
            if(zeroFun(xLarge) >= 0) qSort[i] <- xLarge
            else{
                if(zeroFun(xHuge) <= 0) qSort[i] <- xHuge
                else qSort[i] <-
                    uniroot(zeroFun, interval=c(xLarge,xHuge),...)$root
            }
        }
    }
    if(length(pHuge > 0)){
        for(i in pHuge){
            zeroFun <- function(x) pskewhyp(x,param=param) - pSort[i]
            interval <- c(xHuge, xHuge + (xHuge - xLarge))
            while(zeroFun(interval[1])*zeroFun(interval[2]) > 0){
                interval[2] <- interval[2] + (xHuge - xLarge)
            }
            qSort[i] <- uniroot(zeroFun, interval)$root
        }
    }

    return(qSort[rank(p)])
}


######Random number function###############################################
rskewhyp <- function (n, mu = 0, delta = 1, beta = 1, nu = 1,
                       param = c(mu,delta,beta,nu), log = FALSE) {

    if (length(param) !=4)
        stop("param vector must contain 4 values")

    param <- as.numeric(param)

    mu <- param[1]
    delta <- param[2]
    beta <- param[3]
    nu <- param[4]


    if( delta < 0) stop("Delta must be greater than 0")
    if( nu < 0 ) stop("Nu must be greater than 0")

    if (log==TRUE)
        stop("This function is not yet implemented")


    y <- 1/rgamma(n, shape = nu/2, scale = 2/delta^2)
    sigma <- sqrt(y)
    z <- rnorm(n)
    rskewhyp <- mu + beta*sigma^2 + sigma*z

    return(rskewhyp)
}


