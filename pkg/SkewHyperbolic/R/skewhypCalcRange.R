######Calculate Range function################################################
skewhypCalcRange <- function(mu = 0, delta = 1, beta = 1, nu = 1,
                             param = c(mu,delta,beta,nu), tol= 10^(-5), ...){

    #check parameters
    parResult <- skewhypCheckPars(param)
    case <- parResult$case
    errMessage <- parResult$errMessage
    if(case == "error") stop(errMessage)
    mu <- param[1]
    delta <- param[2]
    beta <- param[3]
    nu <- param[4]

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

