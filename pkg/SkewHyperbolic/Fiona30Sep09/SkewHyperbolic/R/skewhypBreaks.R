#####Break function#######################################################
skewhypBreaks <- function(mu = 0, delta = 1, beta = 1, nu = 1,
                          param = c(mu,delta,beta,nu), small = 10^(-6),
                          tiny = 10^(-10), deriv=0.3,...) {

    if (length(param) != 4) stop("param vector must contain 4 values")

    param <- as.numeric(param)

    mu <- param[1]
    delta <- param[2]
    beta <- param[3]
    nu <- param[4]

    if( delta < 0) stop("Delta must be greater than 0")
    if( nu < 0 ) stop("Nu must be greater than 0")

    #Calculate the more extreme breaks
    CalcRange <- skewhypCalcRange(param=param, tol=tiny,...)
    xTiny <- CalcRange[1]
    xHuge <- CalcRange[2]
    CalcRange <- skewhypCalcRange(param=param, tol=small,...)
    xSmall <- CalcRange[1]
    xLarge <- CalcRange[2]
    modeDist <- skewhypMode(param = param)

    #find low break
    xDeriv <- seq(from = xSmall, to = modeDist, length.out = 101)
    derivVals <- ddskewhyp(x = xDeriv, param = param)
    maxDeriv <- max(derivVals)

    breaksize <- deriv*maxDeriv

    breakFun <- function(x) ddskewhyp(x, param=param) - breaksize

    if( (maxDeriv < breaksize) || (derivVals[1] > breaksize)) {
        lowBreak <- xSmall
    }else{
        whichMax <- which.max(derivVals)
        lowBreak <- uniroot(breakFun, c(xSmall, xDeriv[whichMax]),...)$root
    }

    #find high break
    xDeriv <- seq(from = modeDist, to = xLarge, length.out = 101)
    derivVals <- -ddskewhyp(x = xDeriv, param = param)
    maxDeriv <- max(derivVals)

    breaksize <- deriv*maxDeriv

    breakFun <- function(x) -ddskewhyp(x,param = param) - breaksize

    if( (maxDeriv < breaksize) || (derivVals[101] > breaksize)) {
        highBreak <- xLarge
    }else{
        whichMax <- which.max(derivVals)
        highBreak <- uniroot(breakFun, c(xDeriv[whichMax], xLarge),...)$root
    }

    #list of the breaks
    breaks <- c(xTiny,xSmall,lowBreak,highBreak,xLarge,xHuge,modeDist)
    breaks <- list(xTiny = breaks[1], xSmall = breaks[2],
                   lowBreak = breaks[3], highBreak = breaks[4],
                   xLarge = breaks[5], xHuge = breaks[6],
                   modeDist = breaks[7])

    return(breaks)
}

######Calculate Range function################################################
skewhypCalcRange <- function(mu = 0, delta = 1, beta = 1, nu = 1,
                             param = c(mu,delta,beta,nu), tol= 10^(-5), ...){

    if (length(param) != 4) stop("param vector must contain 4 values")

    param <- as.numeric(param)

    mu <- param[1]
    delta <- param[2]
    beta <- param[3]
    nu <- param[4]

    if( delta < 0) stop("Delta must be greater than 0")
    if( nu < 0 ) stop("Nu must be greater than 0")

    #find xHigh, xLow and Mode
    modeDist <- skewhypMode(param = param)
    xHigh <- modeDist + 0.1
    while (dskewhyp(x=xHigh, param=param, log=FALSE) > tol){
        xHigh <- xHigh + 0.1
    }
    xLow <- modeDist - 0.1
    while (dskewhyp(x=xLow, param=param, log=FALSE) > tol){
        xLow <- xLow - 0.1
    }

    #find xLower and xUpper
    zeroFun <- function(x) dskewhyp(x=x, param=param, log=FALSE) - tol

    xUpper <- uniroot(zeroFun, c(modeDist,xHigh),...)$root
    xLower <- uniroot(zeroFun, c(xLow, modeDist),...)$root

    #put it all together
    range <- c(xLower, xUpper)

    return(range)

}

######Derivative function#################################################
ddskewhyp <- function(x, mu = 0, delta = 1, beta = 1, nu = 1,
                      param = c(mu,delta,beta,nu),log = FALSE,
                      tolerance = .Machine$double.eps ^ 0.5) {

    param<-as.numeric(param)

    mu<-param[1]
    delta<-param[2]
    beta<-param[3]
    nu<-param[4]

    if( delta < 0) stop("Delta must be greater than 0")
    if( nu < 0 ) stop("Nu must be greater than 0")

    if (log==TRUE) stop("This function is not yet implemented")

    if (abs(beta) > tolerance) { #beta != 0
        ddskewhyp <- 1/2*2^(1/2 - 1/2*nu)*delta^nu*abs(beta)^(1/2*nu + 1/2)*
            (-besselK(nu = 1/2*nu + 3/2, x = (beta^2*(delta^2 + (x-mu)^2))^
            (1/2)) + (1/2*nu + 1/2)/(beta^2*(delta^2 + (x - mu)^2))^(1/2)*
            besselK(nu = 1/2*nu + 1/2, x = (beta^2*(delta^2 + (x - mu)^2))^
            (1/2)))/(beta^2*(delta^2 + (x - mu)^2))^(1/2)*beta^2*(2*x - 2*mu)*
            exp(beta*(x - mu))/gamma(1/2*nu)/pi^(1/2)/(((delta^2 + x^2 -
            2*x*mu + mu^2)^(1/2))^(1/2*nu + 1/2)) + 2^(1/2 - 1/2*nu)*
            delta^nu*abs(beta)^(1/2*nu + 1/2)*besselK(nu = 1/2*nu + 1/2,
            x=(beta^2*(delta^2 + (x - mu)^2))^(1/2))*beta*exp(beta*(x - mu))/
            gamma(1/2*nu)/pi^(1/2)/(((delta^2 + x^2 - 2*x*mu + mu^2)^(1/2))^
            (1/2*nu + 1/2)) - 1/2*2^(1/2 - 1/2*nu)*delta^nu*abs(beta)^(1/2*
            nu + 1/2)*besselK(nu = 1/2*nu + 1/2, x = (beta^2*(delta^2 +
            (x - mu)^2))^(1/2))*exp(beta*(x - mu))/gamma(1/2*nu)/pi^(1/2)/
            (((delta^2 + x^2 - 2*x*mu + mu^2)^(1/2))^(1/2*nu + 1/2))*
            (1/2*nu + 1/2)/(delta^2 + x^2 - 2*x*mu + mu^2)*(2*x - 2*mu)

    }else{ #beta = 0
        ddskewhyp <- 2*gamma(1/2*nu + 1/2)/pi^(1/2)/delta^3/gamma(1/2*nu)*
            (1 + (x - mu)^2/delta^2)^(-1/2*nu - 1/2)*(-1/2*nu - 1/2)*
            (x - mu)/(1 + (x - mu)^2/delta^2)
    }
    return(ddskewhyp)
}
