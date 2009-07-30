## <---------------------------------------------------------------------->
fit2factor <- function(data, ttm, deltat = 1 / 260,
                       s0 = data[1,1],
                       delta0 = 0,
                       mu = 0.1, sigmaS = 0.3,
                       kappa = 1, alpha = 0, sigmaE = 0.5,
                       rho = 0.75, lambda = 0,
                       meas.sd = rep(0.02, ncol(data)),
                       opt.pars = c(s0 = FALSE, delta0 = FALSE, mu = TRUE,
                                    sigmaS = TRUE, kappa = TRUE, alpha = TRUE,
                                    sigmaE = TRUE, rho = TRUE, lambda = TRUE),
                       opt.meas.sd = c("scalar", "all", "none"),
                       r = 0.05,
                       silent = FALSE, ...)
{
  call <- match.call()

  opt.meas.sd <- match.arg(opt.meas.sd)

  time.0 <- proc.time()

  if(!all(opt.pars %in% c(TRUE, FALSE)) || (length(opt.pars) != 9))
    {
      stop("'opt.pars' is misspecified: Elements must be either",
           "'TRUE' or 'FALSE' and the length must be 9!\n")
    }

  ## Internal function to compute the log-likelihood
  log.likelihood.2f <- function(thetaOpt, thetaConst, thetaNames,
                                data, ttm, deltat, r, d, n,
                                meas.sd, opt.meas.sd, silent)
    {
      theta <- c(thetaOpt, thetaConst)
      theta <- theta[thetaNames]

      if(opt.meas.sd == "scalar"){
        gg <- theta["meas.sd1"] * meas.sd
      }else if(opt.meas.sd == "all"){
        gg <- theta[10:(10 + d - 1)]
      }else{                            #opt.pars.sd == "none"
        gg <- meas.sd
      }

      ## Build State Space Elements
      stateSpace <- .state.space.2f(y = data, ttm = ttm,
                                    deltat = deltat,
                                    x0 = theta["log.s0"], delta0 = theta["delta0"],
                                    kappa = theta["kappa"], mu = theta["mu"],
                                    alpha = theta["alpha"], lambda = theta["lambda"],
                                    sigmaS = theta["sigmaS"], sigmaE = theta["sigmaE"],
                                    rho = theta["rho"],
                                    gg = gg, r = r, d = d, n = n)

      logLikelihood <- fkf(a0 = stateSpace$a0,
                           P0 = stateSpace$P0,
                           Tt = stateSpace$Tt,
                           dt = stateSpace$dt,
                           HHt = stateSpace$HHt,
                           yt = stateSpace$yt,
                           Zt = stateSpace$Zt,
                           ct = stateSpace$ct,
                           GGt = stateSpace$GGt)$logLik

      theta.backup <<- rbind(theta.backup, c(logLikelihood, theta))

      if(!silent)
        {
          print(paste("logL: ",sprintf("% .8E", logLikelihood),
                      "; Theta: ",
                      paste(sprintf("% .4E", thetaOpt),
                            collapse = ", "), sep = ""))
        }

      ## Returning Filter Call
      return(-logLikelihood)

    }

  theta <- c(log(s0), delta0, mu, sigmaS, kappa,
             alpha, sigmaE, rho, lambda, meas.sd)

  data <- log(as.matrix(data))
  ttm <- as.matrix(ttm)

  ## Initialization
  d <- ncol(data)                      # Dimension of the observations
  n <- nrow(data)                      # Number of observations

  if(length(meas.sd) != d){
    stop("length(meas.sd) must be of the same dimension as 'data'!")
  }

  if(any(meas.sd <= 0)){
    stop("All elements of 'meas.sd' must be greater than 0!")
  }

  thetaNames <- c("log.s0", "delta0", "mu", "sigmaS", "kappa",
                  "alpha", "sigmaE", "rho", "lambda",
                  paste("meas.sd", 1:d, sep = ""))

  names(theta) <- thetaNames

  if(opt.meas.sd == "scalar"){
    opt.pars <- c(opt.pars, TRUE, rep(FALSE, d - 1))
    theta["meas.sd1"] <- 1
  }else if(opt.meas.sd == "all"){
    opt.pars <- c(opt.pars, rep(TRUE, d))
  }else{                                #opt.pars.sd == "none"
    opt.pars <- c(opt.pars, rep(FALSE, d))
  }

  ## Estimate all parameters if opt.pars misspecified
  names(opt.pars) <- thetaNames

  thetaOpt <- theta[opt.pars]
  thetaConst <- theta[!opt.pars]

  theta.backup <- rbind(c(NA, unname(theta)))
  colnames(theta.backup) <- c("logLik", thetaNames)

  mle <- try(optim(par = thetaOpt, fn = log.likelihood.2f,
                   thetaConst = thetaConst, thetaNames = thetaNames,
                   data = data, ttm = ttm, deltat = deltat,
                   r = r, d = d, n = n, meas.sd = meas.sd,
                   opt.meas.sd = opt.meas.sd,
                   silent = silent, ...))

  if(class(mle) == "try-error"){
    convergence <- NA
    n.iter <- nrow(theta.backup)
    message <- mle
    thetaOpt <- theta.backup[nrow(theta.backup), ]
  }else{
    convergence <- mle$convergence
    n.iter <- mle$counts[1]
    message <- NULL
    thetaOpt <- mle$par
  }

  theta <- c(thetaOpt, thetaConst)
  theta <- theta[thetaNames]

  if(opt.meas.sd == "scalar"){
    gg <- theta["meas.sd1"] * meas.sd
  }else if(opt.meas.sd == "all"){
    gg <- theta[10:(10 + d - 1)]
  }else{                                #opt.pars.sd == "none"
    gg <- meas.sd
  }

  stateSpace <- .state.space.2f(y = data, ttm = ttm,
                                deltat = deltat,
                                x0 = theta["log.s0"], delta0 = theta["delta0"],
                                kappa = theta["kappa"], mu = theta["mu"],
                                alpha = theta["alpha"], lambda = theta["lambda"],
                                sigmaS = theta["sigmaS"], sigmaE = theta["sigmaE"],
                                rho = theta["rho"],
                                gg = gg,
                                r = r, d = d, n = n)

  filtered.ts <- fkf(a0 = stateSpace$a0,
                     P0 = stateSpace$P0,
                     Tt = stateSpace$Tt,
                     dt = stateSpace$dt,
                     HHt = stateSpace$HHt,
                     yt = stateSpace$yt,
                     Zt = stateSpace$Zt,
                     ct = stateSpace$ct,
                     GGt = stateSpace$GGt)

##   state <- cbind(S = exp(filtered.ts$att[1,]), delta = filtered.ts$att[2,])

  return(new("fit.schwartz2factor",
             call = call,
             s0 = unname(exp(theta["log.s0"])),
             delta0 = unname(theta["delta0"]),
             mu = unname(theta["mu"]),
             sigmaS = unname(theta["sigmaS"]),
             kappaE = unname(theta["kappa"]),
             alpha = unname(theta["alpha"]),
             sigmaE = unname(theta["sigmaE"]),
             rhoSE = unname(theta["rho"]),
             n.iter = unname(n.iter),
             llh = unname(filtered.ts$logLik),
             converged = convergence == 0,
             error.code= unname(convergence),
             error.message = "",
             fitted.params = opt.pars,
             trace.pars = theta.backup,
             r = unname(r),
             alphaT = unname(theta["alpha"] - theta["lambda"] / theta["kappa"]),
             lambdaE = unname(theta["lambda"]),
             meas.sd = abs(gg),
             deltat = deltat))
}

