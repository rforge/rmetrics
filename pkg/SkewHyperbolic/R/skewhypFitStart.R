skewhypFitStart <- function(x, breaks = NULL, startValues = "LA",
                             paramStart = NULL, ...)
{
  if (startValues == "US") {
    ## user supplied starting values
    x <- as.numeric(na.omit(x))
    svName <- "User Specified"
    if (is.null(paramStart))  stop("paramStart must be specified")
    if (!is.null(paramStart)) {
      ## check parameters
      parResult <- skewhypCheckPars(paramStart)
      case <- parResult$case
      errMessage <- parResult$errMessage
      if(case == "error") stop(errMessage)
      paramStart <- c(beta = paramStart[1], delta = paramStart[2],
                      mu = paramStart[3], nu = paramStart[4])
    }
    names(paramStart) <- c("mu", "delta", "beta", "nu")
    histData <- hist(x, plot = FALSE, right = FALSE)
    if (is.null(breaks)) {
      breaks <- histData$breaks
    }
    midpoints <- histData$mids
    empDens <- ifelse(!is.finite(log(histData$density)), NA,
                      histData$density)
    ## maxIndex <- order(empDens, na.last = FALSE)[length(empDens)]
    list(paramStart = paramStart, breaks = breaks,
         midpoints = midpoints, empDens = empDens, svName = svName)
  } else {
    if(startValues == "LA") {
      ## combination of moments and linear approx to log density
      x <- as.numeric(na.omit(x))
      svName <- "Linear Approximation"
      start <- skewhypFitStartLA(x, breaks = breaks)
      par <- as.vector(start$param)
      list(paramStart = c(mu  = par[1], "delta" = par[2],
                          beta= par[3], "nu"    = par[4]),
           breaks = start$breaks,
           midpoints = start$mids,
           empDens = start$dens,
           svName = svName)
    } else {
      if (startValues == "MM"){
        x <- as.numeric(na.omit(x))
        svName <- "Method of Moments"
        start <- skewhypFitStartMM(x, nuTol = 1e-6, ...)
        par <- as.vector(start$param)
        list(paramStart = c(mu  = par[1], "delta" = par[2],
                            beta= par[3], "nu"    = par[4]),
             breaks = start$breaks,
             midpoints = start$mids,
             empDens = start$dens,
             svName = svName)
      } else {
        stop("invalid 'startValues' string: ", startValues)
      }
    }
  }
}

###### Linear Approximation function #######################################
skewhypFitStartLA <- function(x, breaks = NULL){

  ## fit using combination of method of moments and a linear approximation
  ## to the log density in the tails

  ## histogram
  if(is.null(breaks)) breaks <- 30
  histInfo <- hist(x, plot = FALSE, breaks = breaks)
  mids <- as.matrix(histInfo$mids)
  ## density
  density <- density(x)
  ## find the closest density estimate to the midpoint
  func <- function(x) which.min(abs(density$x - x))
  estx <- apply(mids, 1, func)
  esty <- density$y[estx]
  ## lower and upper tail
  quantiles <- quantile(x, probs = c(0.1,0.9))
  lower <-  which(mids <= quantiles[1])
  upper <-  which(mids >= quantiles[2])
  xLower <- mids[lower]
  xUpper <- mids[upper]
  yLower <- esty[lower]
  yUpper <- esty[upper]
  ## stop if either tail has less than two points
  if( length(xLower) < 4 | length(xUpper) < 4 )
    stop("not enough points to model tail behaviour")

  ## determine skewness
  skew <- skewness(x)

  if( abs(skew) < 0.1 ) {     ## SYMMETRY ##
    beta <- 0
    mu <-  mean(x)
    ## fit models
    upperModel <- lm( log(yUpper) ~ log(abs(xUpper)) )
    lowerModel <- lm( log(yLower) ~ log(abs(xLower)) )
    ## estimates
    nuLow <- -2*(upperModel$coeff[2] + 1)
    nuUpp <- -2*(lowerModel$coeff[2] + 1)
    ## average to get nu
    nu <- mean(c(nuLow,nuUpp))
    ## nu must be >4
    if(nu <= 4) nu <- 4.1
    ##find delta
    delta <- sqrt( var(x)*(nu - 2) )
  } else if( skew >= 0.1 ) {    ## POSITIVE SKEW ##
    ## heavy tail is upper tail
                                        # fit models
    lowerModel <- lm( log(yLower) ~ log(abs(xLower)) + abs(xLower) )
    upperModel <- lm( log(yUpper) ~ log(abs(xUpper)) )
                                        # estimates
    estLow <- lowerModel$coeff[c(2,3)]
    nuLow <- -2*(estLow[1] + 1)
    betaLow <- (estLow[2]/ - 2)
    estUpp <- upperModel$coeff[2]
    nuUpp <- -2*(estUpp + 1)
                                        # nu must be > 0
    nu <- if(nuLow > 0) nuLow else nuUpp
    beta <- betaLow
                                        # nu must be > 4
    if(nu <= 4) nu <- 4.01
                                        # solve for delta
    var <- var(x)
    sol1 <- - 1/4*(nu - 4 - (nu^2 - 8*nu + 16 + 8*beta^2*var*nu -
                             32*beta^2*var)^(1/2))/beta^2*(nu - 2)
    sol2 <- -1/4*(nu - 4 + (nu^2 - 8*nu + 16 + 8*beta^2*var*nu -
                            32*beta^2*var)^(1/2))/beta^2*(nu - 2)
                                        # delta^2 must be positive
    delta2 <- if(sol1 > 0) sol1 else sol2
    delta <- sqrt(delta2)
                                        #solve for mu
    mu <-  mean(x) - beta*delta2/(nu - 2)
  }

  ## NEGATIVE SKEW ##
  if(skew <= -0.1) { # heavy tail is lower tail
    ## fit models
    lowerModel <- lm( log(yLower) ~ log(abs(xLower)) )
    upperModel <- lm( log(yUpper) ~ log(abs(xUpper)) + abs(xUpper) )
    ## estimates
    estLow <- lowerModel$coeff[2]
    nuLow <- -2*(estLow + 1)
    estUpp <- upperModel$coeff[c(2,3)]
    nuUpp <- -2*(estUpp[1] + 1)
    betaUpp <- -(estUpp[2] / -2)
    ## nu must be > 0
    nu <- if(nuLow > 0) nuLow else nuUpp
    beta <- betaUpp
    ## nu must be > 4
    if(nu <= 4) nu <- 4.1
    ## solve for delta
    var <- var(x)
    sol1 <- -1/4*(nu - 4 - (nu^2 - 8*nu + 16 + 8*beta^2*var*nu -
                            32*beta^2*var)^(1/2))/beta^2*(nu - 2)
    sol2 <- -1/4*(nu - 4 + (nu^2 - 8*nu + 16 + 8*beta^2*var*nu -
                            32*beta^2*var)^(1/2))/beta^2*(nu - 2)
    ## delta^2 must be positive
    delta2 <- if(sol1 > 0) sol1 else sol2
    delta <- sqrt(delta2)
    ## solve for mu
    mu <- mean(x) - beta*delta2/(nu-2)
  }

  ## return results
  param <- c(mu,delta,beta,nu)
  names(param) <- c("mu","delta","beta","nu")
  out <- list(param = param, breaks = histInfo$breaks,
              mids = as.vector(mids), dens = esty)
  return(out)
}

###### Method of Moments function #######################################
skewhypFitStartMM <- function(x, nuTol = 1e-6, nuStart = 5, ...){

  ## from Aas& Haff (2004)
  require(DistributionUtils)
  require(numDeriv)
  ## Calculate moments
  m1 <- mean(x, na.rm = TRUE)
  m2 <- var(x, na.rm = TRUE)
  m3 <- skewness(x, na.rm = TRUE)
  m4 <- kurtosis(x, na.rm = TRUE)

  ## Estimate nu
  nuFn <- function(nu, m3, m4){
    kp <- (1/(3*nu^2 - 2*nu - 32))*
      (1 - sqrt(1 - (3*nu^2 - 2*nu - 32)*
                (12*(5*nu - 22) - (nu - 6)*(nu - 8)*m4)/
                (216*(nu -2)^2*(nu - 4))))
    fn <- (4 - 6*(nu + 2)*(nu - 2)*kp)*sqrt(2)*sqrt(nu - 4)*
      sqrt(1 - 6*(nu - 2)*(nu - 4)*kp) - m3*(nu - 6)
    fn^2
  }

  nuSoln <- nlm(nuFn, p = nuStart, m3 = m3, m4 = m4, fscale = 0, ...)
  solnMin <- nuSoln$minimum
  if (abs(solnMin) > nuTol){
    stop("Minimum of function to find nu is greater than nuTol")
  } else {
    nu <- nuSoln$estimate
    kp <- (1/(3*nu^2 - 2*nu - 32))*
      (1 - sqrt(1 - (3*nu^2 - 2*nu - 32)*
                (12*(5*nu - 22) - (nu - 6)*(nu - 8)*m4)/
                (216*(nu - 2)^2*(nu - 4))))
    delta2 <- (6*(nu - 2)^2*(nu - 4)*m2)*kp
    if (delta2 < 0){
      stop("delta^2 is negative in method of moments estimates")
    } else {
      delta <- sqrt(delta2)
    }
    beta <- sign(m3)*sqrt((nu - 2)*(nu - 4)*(m2*(nu - 2) - delta2)/(2*delta2^2))
    mu <- m1 - beta*delta2/(nu - 2)
    ## return results
    param <- c(mu,delta,beta,nu)
    names(param) <- c("mu","delta","beta","nu")
    out <- list(param = param)
    return(out)
  }
}


