### <======================================================================>
"dgig" <- function(x, lambda = 1, chi = 1, psi = 1, logvalue = FALSE)
{
    .check.gig.pars(lambda, chi, psi)

    x.raw  <- x

    x.raw[x.raw < 0] <- NA
    x.raw[x.raw == 0] <- 0

    x <- x.raw[is.finite(x.raw) & x.raw > 0]
    if(length(x) == 0){
        return(x.raw)
    }

    if(psi == 0){
        ## Inverse Gamma -> student t case
        beta <- 0.5 * chi
        alpha <- -lambda
        gig.density <- beta^alpha / gamma(alpha) * x^(-alpha - 1) * exp(-beta / x)
    }else if(chi == 0){
        ## Gamma -> VG case
        beta <- 0.5 * psi
        alpha <- lambda
        gig.density <- beta^alpha / gamma(alpha) * x^(alpha - 1) * exp(-beta * x)

    }else{
        gig.density <- sqrt(psi / chi)^lambda / (2 * besselK(sqrt(chi * psi), lambda)) *
            x^(lambda - 1) * exp(-0.5 * (chi / x + psi * x))
    }

    x.raw[is.finite(x.raw) & x.raw > 0] <- gig.density
    if(logvalue){
        return(log(x.raw))
    }else{
        return(x.raw)
    }

}
### <---------------------------------------------------------------------->



### <======================================================================>
"Egig" <- function(lambda, chi, psi, func = c("x", "logx", "1/x", "var"), check.pars = TRUE)
{
    if(check.pars)
        .check.gig.pars(lambda, chi, psi)

    chi.eps <- .Machine$double.eps

    func <- match.arg(func)
    params <- suppressWarnings(cbind(as.vector(lambda),
                                     as.vector(chi),
                                     as.vector(psi)))
    lambda <- params[,1]
    chi <- params[,2]
    psi <- params[,3]

    if(func == "x"){
        if(all(psi == 0)){
            ## Inv Gamma -> Student-t
            beta <- 0.5 * chi
            alpha <- -lambda
            alpha[alpha <= 1] <- NA # expectation is not defined if alpha <= 1
            return(beta / (alpha - 1))
        }else if(all(chi == 0)){
            ## Gamma -> VG
            beta <- 0.5 * psi
            alpha <- lambda
            return(alpha / beta)
        }else{
            ## GIG -> ghyp, hyp, NIG
            chi[abs(chi) < chi.eps] <- sign(chi[abs(chi) < chi.eps]) * chi.eps
            chi[chi == 0] <- chi.eps

            alpha.bar <- sqrt(chi * psi)
            term1 <- 0.5 * log(chi / psi)
            term2 <- .besselM3(lambda + 1, alpha.bar, logvalue = TRUE)
            term3 <- .besselM3(lambda, alpha.bar, logvalue = TRUE)
            return(exp(term1 + term2 - term3))
        }
    }else if(func == "logx"){
        if(all(psi == 0)){
            ## Inv Gamma -> Student-t
            beta <- 0.5 * chi
            alpha <- -lambda
            return(log(beta) - digamma(alpha))
        }else if(all(chi == 0)){
            ## Gamma -> VG
            beta <- 0.5 * psi
            alpha <- lambda
            return(digamma(alpha) - log(beta))
        }else{
            ## GIG -> ghyp, hyp, NIG

            chi[abs(chi) < chi.eps] <- sign(chi[abs(chi) < chi.eps]) * chi.eps
            chi[chi == 0] <- chi.eps

            alpha.bar <- sqrt(chi * psi)
            besselKnu <- function(nu, xx, expon.scaled = FALSE) {besselK(xx, nu, expon.scaled)}
            Kderiv <- numDeriv::grad(besselKnu, lambda,
                                     method.args = list(eps = 1e-8, show.details = FALSE),
                                     xx = alpha.bar)
            return(0.5 * log(chi / psi) + Kderiv / besselK(alpha.bar, lambda))
        }
    }else if(func == "1/x"){
        if(all(psi == 0)){
            ## Inv Gamma -> Student-t
            beta <- 0.5 * chi
            alpha <- -lambda
            return(alpha / beta)
        }else if(all(chi == 0)){
            ## Gamma -> VG
            warning("Case 'chi == 0' and 'func = 1/x' is not implemented")
            return(NA)
        }else{
                                        # GIG -> ghyp, hyp, NIG
            chi[abs(chi) < chi.eps] <- sign(chi[abs(chi) < chi.eps]) * chi.eps
            chi[chi==0] <- chi.eps

            alpha.bar <- sqrt(chi * psi)
            term1 <- -0.5 * log(chi / psi)
            term2 <- .besselM3(lambda - 1, alpha.bar, logvalue = TRUE)
            term3 <- .besselM3(lambda, alpha.bar, logvalue = TRUE)
            return(exp(term1 + term2 - term3))
        }
    }else if(func == "var"){
        if(all(psi == 0)){
            ## Inv Gamma -> Student-t
            beta <- 0.5 * chi
            alpha <- -lambda
            tmp.var <- beta^2 / ((alpha - 1)^2 * (alpha - 2))
            tmp.var[alpha <= 2] <- Inf # variance is Inf if alpha <= 2
            return(tmp.var)
        }else if(all(chi == 0)){
            ## Gamma -> VG
            beta <- 0.5 * psi
            alpha <- lambda
            return(alpha / (beta^2))
        }else{
            ## GIG -> ghyp, hyp, NIG
            chi[abs(chi) < chi.eps] <- sign(chi[abs(chi) < chi.eps]) * chi.eps
            chi[chi == 0] <- chi.eps

            alpha.bar <- sqrt(chi * psi)
            term1 <- 0.5 * log(chi / psi)
            term2 <- .besselM3(lambda + 1, alpha.bar, logvalue = TRUE)
            term3 <- .besselM3(lambda, alpha.bar, logvalue = TRUE)
            var.term1 <- log(chi / psi)
            var.term2 <- .besselM3(lambda + 2, alpha.bar, logvalue = TRUE)
            var.term3 <- .besselM3(lambda, alpha.bar, logvalue = TRUE)
            return(exp(var.term1 + var.term2 - var.term3) - exp(term1 + term2 - term3)^2)
        }

    }
}
## <---------------------------------------------------------------------->



### <======================================================================>
"ESgig" <- function(alpha, lambda = 1, chi = 1, psi = 1, distr = c("return", "loss"), ...)
{
    distr <- match.arg(distr)

    .check.gig.pars(lambda, chi, psi)

    value.raw <- qgig(alpha, lambda, chi, psi, ...)

    if(all(is.na(value.raw))){
        return(value.raw)
    }

    pdf.args <- list(lambda = lambda, chi = chi, psi = psi)

    value.es <- matrix(value.raw[!is.na(value.raw)], ncol = 1)

    if(distr == "return"){
        value.es <- apply(value.es, MARGIN = 1, FUN = .p.default, pdf = ".integrate.moment.gig",
                          lower = 0, pdf.args = pdf.args)
        value.es <- value.es / alpha
    }else{
        value.es <- apply(value.es, MARGIN = 1, FUN = .p.default, pdf = ".integrate.moment.gig",
                          upper = Inf, pdf.args = pdf.args)
        value.es <- value.es / (1 - alpha)
    }

    value.raw[!is.na(value.raw)] <- value.es

    return(value.es)
}
### <---------------------------------------------------------------------->



### <======================================================================>
"pgig" <- function(q, lambda = 1, chi = 1, psi = 1, ...)
{
    .check.gig.pars(lambda, chi, psi)

    q.raw  <- q

    q.raw[q.raw < 0] <- NA
    q.raw[q.raw == 0] <- 0

    q <- q.raw[is.finite(q.raw) & q.raw > 0]
    if(length(q) == 0){
        return(q.raw)
    }

    q <- matrix(q, ncol = 1)
    pdf.args <- list(lambda = lambda, chi = chi, psi = psi)
    value <- apply(q, MARGIN = 1, FUN = .p.default, pdf = "dgig", lower = 0,
                   pdf.args = c(pdf.args, list(...)))

    q.raw[is.finite(q.raw) & q.raw > 0] <- value
    return(q.raw)
}


### <======================================================================>
"qgig" <- function(p, lambda = 1, chi = 1, psi = 1, method = c("integration","splines"),
                   spline.points = 200, subdivisions = 200, root.tol = .Machine$double.eps^0.5,
                   rel.tol = root.tol^1.5, abs.tol = rel.tol, ...)

{
    p.raw  <- p

    method <- match.arg(method)

    p.raw[p.raw < 0 | p.raw > 1] <- NaN
    p.raw[p.raw == 1] <- Inf
    p.raw[p.raw == 0] <- 0

    ## If only !is.finite quantiles are passed return NA, NaN, Inf, -Inf
    p <- p.raw[is.finite(p.raw)]
    if(length(p) == 0){
        return(p.raw)
    }
    ##   Use Newton's method to find the range of the quantiles
    internal.bisection <- function(lambda, chi, psi, p, tol, rel.tol, abs.tol, subdivisions)
    {

        iter <- 0
        range.found <- FALSE
        step.size <- sqrt(Egig(lambda, chi, psi, func = "var"))

        ## ---------> workarround
        if(is.na(step.size)){
            step.size <- 0.1
        }
        ## ---------> end workarround

        q.0 <- Egig(lambda, chi, psi, func = "x")
        q.upper <- q.0 + step.size

        while(!range.found & iter < 100){
            iter <- iter + 1
            p.upper <- pgig(q = q.upper, lambda, chi, psi, rel.tol = rel.tol, abs.tol = abs.tol,
                            subdivisions = subdivisions) - p

            if(any(is.na(p.upper))){
                warning("Unable to determine interval where the quantiles are in-between.")
                return(NA)
            }
            ##      cat("upper : ", p.upper, " q.upper: ", q.upper, "\n")
            if(p.upper <= 0){
                q.upper <- q.upper + step.size
                next
            }
            if(p.upper > 0){
                range.found <- TRUE
            }
        }
        if(iter >= 100){
            warning("Unable to determine interval where the quantiles are in-between.")
        }

        pdf.args <- list(lambda = lambda, chi = chi, psi = psi)

        q.root <- .q.default(p, pdf = "dgig", pdf.args = pdf.args,
                             interval = c(0 + .Machine$double.eps, q.upper), tol = root.tol,
                             p.lower = 0, rel.tol = rel.tol, abs.tol = abs.tol,
                             subdivisions = subdivisions)

        return(q.root)
    }
    ## end of Newton iteration


    gig.expected <- Egig(lambda, chi, psi, func = "x")
    q.expected <- pgig(gig.expected, lambda, chi, psi,
                       subdivisions = subdivisions,
                       rel.tol = rel.tol, abs.tol= abs.tol)
    if(q.expected > max(p)){
        interval <- c(0, gig.expected)
    }else{
        interval.max <- internal.bisection(lambda, chi, psi, max(p),
                                           root.tol, rel.tol, abs.tol, subdivisions)
        interval <- c(0, interval.max * 1.1)
    }
    if(any(is.na(interval))){ # -> Failed to determine bounds for the quantiles
        p.raw[is.finite(p.raw)] <- NA
        return(p.raw)
    }

    if(method=="integration"){
        ## The integration method
        pdf.args <- list(lambda = lambda, chi = chi, psi = psi)
        p <- matrix(p, ncol = 1)
        value <- apply(p, MARGIN = 1, FUN = .q.default, pdf = "dgig",
                       pdf.args = pdf.args, interval = interval,
                       tol = root.tol, p.lower = 0, rel.tol = rel.tol,
                       abs.tol = abs.tol, subdivisions = subdivisions)
    }else{
        ## The spline method
        interval.seq <- seq(min(interval), max(interval), length = spline.points)
        ## Compute the distribution function to be interpolated by splines
        p.interval <- pgig(q = interval.seq, lambda = lambda, chi = chi,
                           psi = psi, rel.tol = rel.tol, abs.tol = abs.tol,
                           subdivisions = subdivisions)
        ## Spline function
        spline.distribution.func <- splinefun(interval.seq, p.interval)

        ## root function:   condition: quantile.root.func == 0
        quantile.root.func <- function(x, tmp.p){
            spline.distribution.func(x) - tmp.p
        }
        value <- p
        for(i in 1:length(p)){
            value[i] <- uniroot(quantile.root.func, interval = interval,
                                tmp.p = p[i], tol = root.tol)$root
        }
        value <- as.vector(value)
    }
    p.raw[is.finite(p.raw)] <- value
    return(p.raw)
}
### <---------------------------------------------------------------------->


### <======================================================================>
"rgig" <- function(n = 10, lambda = 1, chi = 1, psi = 1)
{
    if((chi < 0) | (psi < 0))
        stop("Invalid parameters for GIG")
    if((chi == 0) & (lambda <= 0))
        stop("Invalid parameters for GIG")
    if((psi == 0) & (lambda >= 0))
        stop("Invalid parameters for GIG")

    if((chi == 0) & (lambda > 0)){
        ## Gamma distribution -> Variance Gamma
        return(rgamma(n, shape = lambda, rate = psi / 2))
    }
    if((psi == 0) & (lambda < 0)){
        ## Inverse gamma distribution -> Student-t
        return(1 / rgamma(n, shape = - lambda, rate = chi / 2))
    }

    ## IG distribution:
    ## An implementation of the algorithm described in Raible (2000),
    ## copied from the package fBasics.
    if(lambda == -0.5)
    {
        U <- runif(n)
        V <- rnorm(n)^2
        delta <- sqrt(chi)

        z1 <- function(v, delta, gamma)
        {
            delta/gamma + v/(2 * gamma^2) - sqrt(v * delta/(gamma^3) +
                                                 (v/(2 * gamma^2))^2)
        }

        z2 <- function(v, delta, gamma)
        {
            (delta/gamma)^2/z1(v = v, delta = delta, gamma = gamma)
        }

        pz1 <- function(v, delta, gamma) {
            delta/(delta + gamma * z1(v = v, delta = delta, gamma = gamma))
        }

        s = (1 - sign(U - pz1(v = V, delta = delta, gamma = sqrt(psi))))/2

        return(z1(v = V, delta = delta, gamma = sqrt(psi)) * s +
               z2(v = V, delta = delta, gamma = sqrt(psi)) * (1 - s))

    }else if(lambda == 1){
        ## hyp distribution:
        ## An implementation of the algorithm described in Dagpunar (1989),
        ## copied from Rmetrics package 'fBasics'.
        alpha <- sqrt(psi / chi)
        beta <- sqrt(psi * chi)
        m <- sign(beta)

        g <- function(y)
        {
            return(0.5 * beta * y^3 - y^2 * (0.5 * beta + 2) +
                   y * (-0.5 * beta) + 0.5 * beta)
        }

        upper <- 1

        while (g(upper) <= 0)
        {
            upper <- 2 * upper
        }

        yM <- uniroot(g, interval = c(0, 1))$root
        yP <- uniroot(g, interval = c(1, upper))$root
        a <- (yP - 1) * exp(-0.25 * beta * (yP + 1/yP - 2))
        b <- (yM - 1) * exp(-0.25 * beta * (yM + 1/yM - 2))
        c <- -0.25 * beta * 2
        output <- numeric(n)

        for (i in 1:n) {
            need.value <- TRUE
            while (need.value == TRUE) {
                R1 <- runif(1)
                R2 <- runif(1)
                Y <- 1 + a * R2/R1 + b * (1 - R2)/R1
                if (Y > 0) {
                    if (-log(R1) >= 0.25 * beta * (Y + 1/Y) + c) {
                        need.value <- FALSE
                    }
                }
            }
            output[i] <- Y
        }
        return(output/alpha)

    }else{
        ## GIG distribution:
        ## An implementation of the algorithm described in Dagpunar (1989),
        ## copied from Rmetrics package 'fBasics'.
        alpha = sqrt(psi / chi)
        beta = sqrt(psi * chi)
        m = (lambda - 1 + sqrt((lambda - 1)^2 + beta^2))/beta

        g = function(y) {
            0.5 * beta * y^3 - y^2 * (0.5 * beta * m + lambda + 1) +
                y * ((lambda - 1) * m - 0.5 * beta) + 0.5 * beta *
                    m
        }
        upper = m

        while (g(upper) <= 0){
            upper = 2 * upper
        }

        yM = uniroot(g, interval = c(0, m))$root
        yP = uniroot(g, interval = c(m, upper))$root
        a = (yP - m) * (yP/m)^(0.5 * (lambda - 1)) * exp(-0.25 *
                                                         beta * (yP + 1/yP - m - 1/m))
        b = (yM - m) * (yM/m)^(0.5 * (lambda - 1)) * exp(-0.25 *
                                                         beta * (yM + 1/yM - m - 1/m))
        c = -0.25 * beta * (m + 1/m) + 0.5 * (lambda - 1) * log(m)
        output = numeric(n)

        for (i in 1:n) {
            need.value = TRUE
            while (need.value == TRUE) {
                R1 = runif(1)
                R2 = runif(1)
                Y = m + a * R2/R1 + b * (1 - R2)/R1
                if (Y > 0) {
                    if (-log(R1) >= -0.5 * (lambda - 1) * log(Y) +
                        0.25 * beta * (Y + 1/Y) + c) {
                        need.value = FALSE
                    }
                }
            }
            output[i] = Y
        }
        return(output/alpha)

    }

}
### <---------------------------------------------------------------------->
## "rgig" <- function(n = 10, lambda = 1, chi = 1, psi = 1, envplot = FALSE,
##                    messages = FALSE)
## {
##     ## This source code is copied from the S-Plus library QRMlib
##     ## from Alexander J. McNeil (2005) designed to accompany the book
##     ## Quantitative Risk Management, Concepts, Techniques and Tools.
##     ## see http://www.math.ethz.ch/~mcneil/book/QRMlib.html

##     if((chi < 0) | (psi < 0))
##         stop("Invalid parameters for GIG")
##     if((chi == 0) & (lambda <= 0))
##         stop("Invalid parameters for GIG")
##     if((psi == 0) & (lambda >= 0))
##         stop("Invalid parameters for GIG")
##     if((chi == 0) & (lambda > 0))
##         return(rgamma(n, shape = lambda, rate = psi / 2))
##     if((psi == 0) & (lambda < 0))
##         return(1 / rgamma(n, shape = - lambda, rate = chi / 2))

##     ## NIG distribution
##     ## An implementation of the algorithm described in Raible (2000),
##     ## copied from the package fBasics
##     if(lambda == -0.5)
##     {
##         U <- runif(n)
##         V <- rnorm(n)^2
##         delta <- sqrt(chi)

##         z1 <- function(v, delta, gamma)
##         {
##             delta/gamma + v/(2 * gamma^2) - sqrt(v * delta/(gamma^3) +
##                                                  (v/(2 * gamma^2))^2)
##         }

##         z2 <- function(v, delta, gamma)
##         {
##             (delta/gamma)^2/z1(v = v, delta = delta, gamma = gamma)
##         }

##         pz1 <- function(v, delta, gamma) {
##             delta/(delta + gamma * z1(v = v, delta = delta, gamma = gamma))
##         }

##         s = (1 - sign(U - pz1(v = V, delta = delta, gamma = sqrt(psi))))/2

##         return(z1(v = V, delta = delta, gamma = sqrt(psi)) * s +
##                z2(v = V, delta = delta, gamma = sqrt(psi)) * (1 - s))

##     }

##     message <- NULL

##     if(abs(lambda) < 1)
##         message <- "Not necessarily efficient rejection method\n"
##     neglambda <- F
##     if(lambda < 0){
##         neglambda = T
##         lambda <- abs(lambda)
##         tmp <- c(chi, psi)
##         chi <- tmp[2]
##         psi <- tmp[1]
##     }

##     efunc <- function(x, lambda, chi, psi)
##     {
##         (x^(lambda - 1)) * exp( - (chi/x + psi * x)/2)
##     }

##     calcmode <- function(lambda, chi, psi)
##     {
##         if(psi > 0)
##             return(((lambda - 1) + sqrt(chi * psi + (1 - lambda)^
##                                         2))/psi)
##         else if(psi == 0)
##             return(chi/(2 - 2 * lambda))
##         else stop("Problem in mode function")
##     }

##     themode <- calcmode(lambda, chi, psi)
##     assign("lambda", lambda)
##     assign("chi", chi)
##     assign("psi", psi)
##     assign("themode", themode)
##     assign("calcmode", calcmode)
##     assign("efunc", efunc)
##     if(lambda < 1){
##         theta <- 0.01
##         objective <- function(theta)
##         {
##             if(theta <= 0)
##                 out <- NA
##             else {
##                 Delta1 <- (exp(themode * theta) - 1)/theta
##                 Delta2 <- (2 * exp(( - themode * psi)/2))/
##                     psi
##                 xL <- calcmode(lambda, chi, psi + 2 * theta)
##                 xH <- chi/(2 - 2 * lambda)
##                 S1 <- efunc(xL, lambda, chi, psi + 2 * theta)
##                 S2 <- efunc(xH, lambda, chi, 0)
##                 out <- Delta1 * S1 + Delta2 * S2
##             }
##             return(out)
##         }
##         out <- optimize(objective, interval=c(1e-5,100),tol=1e-8)
##     }else{
##         theta <- c(0.01, psi/4)
##         objective <- function(theta)
##         {
##             if((theta[1] <= 0) | (theta[2] <= 0))
##                 out <- NA
##             else if((psi - 2 * theta[2]) < 0)
##                 out <- NA
##             else {
##                 Delta1 <- (exp(themode * theta[1]) - 1)/theta[1]
##                 Delta2 <- exp( - themode * theta[2])/theta[2]
##                 xL <- calcmode(lambda, chi, psi + 2 * theta[1])
##                 xH <- calcmode(lambda, chi, psi - 2 * theta[2])
##                 S1 <- efunc(xL, lambda, chi, psi + 2 * theta[1])
##                 S2 <- efunc(xH, lambda, chi, psi - 2 * theta[2])
##                 out <- Delta1 * S1 + Delta2 * S2
##             }
##             out
##         }
##         out <- optim(theta, objective, control=list(maxit = 10000))
##         if(!(out$convergence==0))
##             message <- paste(message,
##                              "Problems finding optimal s and p (use option envplot for reassurance)",
##                              "\n")
##     }
##     if(lambda < 1) {
##         spar <- out$minimum
##         ppar <- psi/2
##     }
##     else {
##         spar <- out$par[1]
##         ppar <- out$par[2]
##     }
##     xL <- calcmode(lambda, chi, psi + 2 * spar)
##     xH <- calcmode(lambda, chi, psi - 2 * ppar)
##     S1 <- efunc(xL, lambda, chi, psi + 2 * spar)
##     S2 <- efunc(xH, lambda, chi, psi - 2 * ppar)
##     Delta1 <- (exp(themode * spar) - 1)/spar
##     Delta2 <- exp( - themode * ppar)/ppar
##     k <- 1/((Delta1/S2) + (Delta2/S1))
##     k1 <- k/S2
##     k2 <- k/S1
##     rpar <- k1 * Delta1
##     if(envplot) {
##         xdat <- seq(from = 0.01, to = themode * 20, length = 1000)
##         envelope2 <- (xdat <= themode) * exp(spar * xdat) * S1 + (
##                       xdat > themode) * exp( - ppar * xdat) * S2
##         envelope <- (xdat <= themode) * exp(spar * xdat) * k1 + (xdat >
##                      themode) * exp( - ppar * xdat) * k2
##         ydat <- efunc(xdat, lambda, chi, psi)
##         yr <- range(ydat, envelope, envelope2)
##         plot(xdat, ydat, ylim = yr, type = "l")
##         abline(v = themode)
##         lines(xdat, envelope, col = 2)
##         lines(xdat, envelope2, lty = 2, col = 2)
##     }
##     xsim = numeric()
##     iter=0



##     rgigsim = function(n, rpar, spar, ppar, k1, k2, lambda, chi, psi, s1, s2)
##     {
##         ef = function (x, lambda, chi, psi)
##         { x^(lambda-1) * exp(-0.5*(psi*x + chi/x)) }

##         U = runif(n)
##         idx = (U<=rpar)

##         nLow = sum(idx)
##         x = log( 1 + spar*U[idx]/k1 ) / spar
##         level = log( ef(x, lambda, chi, psi+2*spar ) / s1 )
##         xsim = x[log( runif(nLow) )<=level]

##         x = -log( ppar*(1-U[!idx])/k2 ) / ppar
##         level = log( ef(x, lambda, chi, psi-2*ppar ) / s2 )
##         xsim = c(xsim, x[ log( runif(n-nLow) ) <= level ] )

##         xsim
##     }


##     while (length(xsim)<n) {
##         m = n-length(xsim)
##         iter = iter+m
##         xx = rgigsim(n = m, rpar = rpar, spar = spar, ppar = ppar, k1 = k1, k2 = k2,
##         lambda = lambda, chi = chi, psi = psi, s1 = S1, s2 = S2)
##         xsim = c(xsim,xx)
##     }
##     efficiency <- length(xsim)/iter
##     message <- paste(message, "Efficiency", round(efficiency * 100, 1),"\n")
##     if(messages)
##         cat(message)
##     if(neglambda){
##         return(1/xsim)
##     }else{
##         return(xsim)
##     }
## }
