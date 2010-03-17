######Calculate Range function################################################
skewhypCalcRange <- function(mu = 0, delta = 1, beta = 1, nu = 1,
                             param = c(mu,delta,beta,nu), tol= 10^(-5), ...){

    ## check parameters
    parResult <- skewhypCheckPars(param)
    case <- parResult$case
    errMessage <- parResult$errMessage
    if(case == "error") stop(errMessage)
    mu <- param[1]
    delta <- param[2]
    beta <- param[3]
    nu <- param[4]

    upperProb <- function(x){
        px <- integrate(dskewhyp, x, Inf, param = param)$value
        return(px)
    }

    ## find xHigh, xLow and Mode
    modeDist <- skewhypMode(param = param)
    xHigh <- modeDist + stepSize(delta, nu, beta, "right")
    while (upperProb(xHigh) > tol){
        xHigh <- xHigh + stepSize(xHigh - modeDist, nu, beta, "right")
    }

    lowerProb <- function(x){
        px <- integrate(dskewhyp, -Inf, x, param = param)$value
        return(px)
    }

    xLow <- modeDist  - stepSize(delta, nu, beta, "left")
    while (lowerProb(xLow) > tol){
        xLow <- xLow - stepSize(modeDist - xLow, nu, beta, "left")
    }

    ##find xLower and xUpper
    zeroFun <- function(x) upperProb(x) - tol
    xUpper <- uniroot(zeroFun, c(modeDist,xHigh),...)$root
    zeroFun <- function(x) lowerProb(x) - tol
    xLower <- uniroot(zeroFun, c(xLow, modeDist),...)$root

    ##put it all together
    range <- c(xLower, xUpper)

    return(range)

}


stepSize <- function(dist, nu, beta, side = c("right","left"))
{
    ## Purpose: determine the step size for a skewhyperbolic
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: David Scott, Date: 17 Mar 2010, 21:50
    side <- match.arg(side)
    if (beta > 0){
        step <- ifelse(side == "right", exp(dist/nu), dist)
    }
    if (beta < 0){
        step <- ifelse(side == "left", exp(dist/nu), dist)
    }
    if (isTRUE(all.equal(beta, 0))){
        step <- exp(dist/nu)
    }
    return(step)
}


