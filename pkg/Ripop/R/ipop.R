
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA


################################################################################
# FUNCTION:                 DESCRIPTION:
#  ipop                      "ipop" solves a quadratic programming problem
################################################################################


ipop <- 
function(c, H, A, b, l, u, r, 
    sigf = 12, maxiter = 100, margin = 1e-8, bound = 10, verb = 0)
{
    # A function implemented by Diethelm Wuertz

    # Description:
    #   "ipop" solves a quadratic programming problem
    
    # Details:
    #   minimize     c' * primal + 1/2 primal' * H * primal
    #   subject to   b <= A*primal <= b + r
    #                l <= primal <= u
    #                d is the optimizer itself
    #   returns primal and dual variables (i.e. x and the Lagrange
    #   multipliers for b <= A * primal <= b + r)
    #   for additional documentation see
    #       R. Vanderbei
    #       LOQO: An Interior Point Code for Quadratic Programming, 1992
    #   Author:      R version Alexandros Karatzoglou, 
    #                orig. matlab Alex J. Smola
    #   Created:     12/12/97
    #   R Version:   12/08/03
    #   Updated:     13/10/05
    #   Code:        A modified copy from contributed R package kernlab
    #                released under the GNU Public License
    #   Note:        A QP Solver entirely written in R from contributed 
    #                Package kernlab ...
    
    # FUNCTION:
    
    # Check input dimensions:
    if(!is.matrix(H)) stop("H must be a matrix")
    if(!is.matrix(A)&&!is.vector(A)) stop("A must be a matrix or a vector")
    if(!is.matrix(c)&&!is.vector(c)) stop("c must be a matrix or a vector")
    if(!is.matrix(l)&&!is.vector(l)) stop("l must be a matrix or a vector")
    if(!is.matrix(u)&&!is.vector(u)) stop("u must be a matrix or a vector")
    n <- dim(H)[1]
  
    # Check for a decomposed H matrix
    if(n == dim(H)[2]) smw <- 0
    if(n > dim(H)[2]) smw <- 1
    if(n < dim(H)[2]) {
        smw <- 1
        n <- dim(H)[2]
        H <- t(H)
    }
       
    # Check for compatibilities:
    if (is.vector(A)) A <- matrix(A, 1)
    m <- dim(A)[1]
    primal <- rep(0, n)
    if(missing(b)) bvec <- rep(0, m)
    if(n != length(c)) stop("H and c are incompatible!")
    if(n != ncol(A)) stop("A and c are incompatible!")
    if(m != length(b)) stop("A and b are incompatible!")
    if(n != length(u)) stop("u is incopatible with H")
    if(n != length(l)) stop("l is incopatible with H")
    c <- matrix(c)
    l <- matrix(l)
    u <- matrix(u)
    m <- nrow(A)
    n <- ncol(A)
    H.diag <- diag(H)
    if(smw == 0) H.x <- H else if (smw == 1) H.x <- t(H)
    b.plus.1 <- max(svd(b)$d) + 1
    c.plus.1 <- max(svd(c)$d) + 1
    one.x <- -matrix(1,n,1)
    one.y <- -matrix(1,m,1)
    
    # Starting point:
    if(smw == 0) diag(H.x) <- H.diag + 1 else smwn <- dim(H)[2]
    H.y <- diag(1,m)
    c.x <- c
    c.y <- b
    
    # Solve the system: 
    #   [-H.x A' A H.y] [x, y] = [c.x c.y]
    if(smw == 0) {
        AP <- matrix(0,m+n,m+n)
        xp <- 1:(m+n) <= n
        AP[xp, xp] <- -H.x
        AP[xp == FALSE,xp] <- A
        AP[xp, xp == FALSE] <- t(A)
        AP[xp == FALSE, xp== FALSE] <- H.y
        s.tmp <- solve(AP, c(c.x, c.y), tol = .Machine$double.eps, LINPACK = TRUE)
        x <- s.tmp[1:n]
        y <- s.tmp[-(1:n)]
    } else {
        V <- diag(smwn)
        smwinner <- chol(V + crossprod(H))
        smwa1 <- t(A)
        smwc1 <- c.x
        smwa2 <- smwa1 - (H %*% solve(smwinner, solve(t(smwinner),
            crossprod(H, smwa1)), tol = .Machine$double.eps, LINPACK = TRUE))
        smwc2 <- smwc1 - (H %*% solve(smwinner, solve(t(smwinner),
            crossprod(H, smwc1)), tol = .Machine$double.eps, LINPACK = TRUE)) 
        y <- solve(A %*% smwa2 + H.y , c.y + A %*% smwc2, tol = .Machine$double.eps, LINPACK = TRUE)
        x <- smwa2 %*% y - smwc2
    }
    g <- pmax(abs(x - l), bound)
    z <- pmax(abs(x), bound)
    t <- pmax(abs(u - x), bound)
    s <- pmax(abs(x), bound)
    v <- pmax(abs(y), bound)
    w <- pmax(abs(y), bound)
    p <- pmax(abs(r - w), bound)
    q <- pmax(abs(y), bound)
    mu <- as.vector(crossprod(z,g) + crossprod(v,w) + 
        crossprod(s,t) + crossprod(p,q))/(2 * (m + n))
    sigfig <- 0
    counter <- 0
    alfa <- 1
    if (verb > 0) # print at least one status report
        cat("Iter    PrimalInf  DualInf  SigFigs  Rescale  PrimalObj  DualObj",
            "\n")

    # Iterate:
    while (counter < maxiter) {
        
        # Update the iteration counter
        counter <- counter + 1
        
        # Central path (predictor)
        if(smw == 0) 
            H.dot.x <- H %*% x
        else if (smw == 1)
            H.dot.x <- H %*% crossprod(H,x)
        rho <- b - A %*% x + w
        nu <- l - x + g
        tau <- u - x - t
        alpha <- r - w - p
        ## DW:
        if(is.numeric(alpha)) Alpha = t(t(alpha)) else Alpha = alpha
        sigma <- c  - crossprod(A, y) - z + s + H.dot.x
        beta <- y + q - v
        gamma.z <- - z
        gamma.w <- - w
        gamma.s <- - s
        gamma.q <- - q
        
        # Instrumentation:
        x.dot.H.dot.x <-  crossprod(x, H.dot.x)
        primal.infeasibility <- 
            ## DW: max(svd(rbind(rho, tau, alpha, nu))$d) / b.plus.1
            max(svd(rbind(rho, tau, Alpha, nu))$d) / b.plus.1
        dual.infeasibility <- max(svd(rbind(sigma,t(t(beta))))$d) / c.plus.1
        primal.obj <- crossprod(c,x) + 0.5 * x.dot.H.dot.x
        dual.obj <- crossprod(b,y) - 0.5 * x.dot.H.dot.x + 
            crossprod(l, z) - crossprod(u,s) - crossprod(r,q)
        old.sigfig <- sigfig
        sigfig <- max(-log10(abs(primal.obj - dual.obj) /
            (abs(primal.obj) + 1)), 0)
        if (sigfig >= sigf) break
        if (verb > 0) # final report
            cat( counter, "\t", signif(primal.infeasibility,6), 
                signif(dual.infeasibility,6), sigfig, alfa, primal.obj, 
                dual.obj,"\n")
        
        # Some more intermediate variables (the hat section)
        hat.beta <- beta - v * gamma.w / w
        hat.alpha <- alpha - p * gamma.q / q
        hat.nu <- nu + g * gamma.z / z
        hat.tau <- tau - t * gamma.s / s
        
        # The diagonal terms:
        d <- z / g + s / t
        e <- 1 / (v / w + q / p)
        
        # Initialization before the big cholesky:
        if (smw == 0) diag(H.x) <- H.diag + d
        diag(H.y) <- e
        c.x <- sigma - z * hat.nu / g - s * hat.tau / t
        c.y <- rho - e * (hat.beta - q * hat.alpha / p)
        # and solve the system [-H.x A' A H.y] [delta.x, delta.y] <- [c.x c.y]
        if(smw == 0) {
            AP[xp,xp] <- -H.x
            AP[xp == FALSE, xp== FALSE] <- H.y
            s1.tmp <- solve(AP, c(c.x,c.y), tol = .Machine$double.eps, LINPACK = TRUE)
            delta.x<-s1.tmp[1:n] ; delta.y <- s1.tmp[-(1:n)]
        } else {
            V <- diag(smwn)
            # Chunkmult:
            Z = t(H)
            csize = 2000
            colscale = d
            n <- dim(Z)[1]
            m <- dim(Z)[2]
            d <- sqrt(colscale)
            nchunks <- ceiling(m/csize)
            res <- matrix(0,n,n)
            for( i in 1:nchunks) {
                lowerb <- (i - 1) * csize + 1 
                upperb <- min(i * csize, m)
                buffer <- t(Z[,lowerb:upperb,drop = FALSE])
                bufferd <- d[lowerb:upperb]
                buffer <- buffer / bufferd
                res <- res + crossprod(buffer)
            }
            smwinner <- chol(V + res)
            smwa1 <- t(A)
            smwa1 <- smwa1 / d
            smwc1 <- c.x / d
            smwa2 <- t(A) - (H %*% solve(smwinner,solve(t(smwinner),
                crossprod(H,smwa1)), tol = .Machine$double.eps, LINPACK = TRUE))
            smwa2 <- smwa2 / d 
            smwc2 <- (c.x - (H %*% solve(smwinner,solve(t(smwinner),
                crossprod(H,smwc1)), tol = .Machine$double.eps, LINPACK = TRUE)))/d 
            delta.y <- solve(A %*% smwa2 + H.y , c.y + A %*% smwc2, tol = .Machine$double.eps, LINPACK = TRUE)
            delta.x <- smwa2 %*% delta.y - smwc2
        }
        
        # Backsubstitution:
        delta.w <- - e * (hat.beta - q * hat.alpha / p + delta.y)
        delta.s <- s * (delta.x - hat.tau) / t
        delta.z <- z * (hat.nu - delta.x) / g
        delta.q <- q * (delta.w - hat.alpha) / p
        delta.v <- v * (gamma.w - delta.w) / w
        delta.p <- p * (gamma.q - delta.q) / q
        delta.g <- g * (gamma.z - delta.z) / z
        delta.t <- t * (gamma.s - delta.s) / s
        
        # Compute update step now (Sebastian's trick):
        alfa <- - (1 - margin) / 
            min(c(delta.g / g, delta.w / w, delta.t / t, delta.p / p, 
                delta.z / z, delta.v / v, delta.s / s, delta.q / q, -1))
        newmu <- (crossprod(z,g) + crossprod(v,w) + 
            crossprod(s,t) + crossprod(p,q))/(2 * (m + n))
        newmu <- mu * ((alfa - 1) / (alfa + 10))^2
        gamma.z <- mu / g - z - delta.z * delta.g / g
        gamma.w <- mu / v - w - delta.w * delta.v / v
        gamma.s <- mu / t - s - delta.s * delta.t / t
        gamma.q <- mu / p - q - delta.q * delta.p / p
        
        # Some more intermediate variables (the hat section)
        hat.beta <- beta - v * gamma.w / w
        hat.alpha <- alpha - p * gamma.q / q
        hat.nu <- nu + g * gamma.z / z
        hat.tau <- tau - t * gamma.s / s
        
        # Initialization before the big cholesky
        c.x <- sigma - z * hat.nu / g - s * hat.tau / t
        c.y <- rho - e * (hat.beta - q * hat.alpha / p)

        # And solve the system: 
        #   [-H.x A' A H.y] [delta.x, delta.y] <- [c.x c.y]
        if (smw == 0) {
            AP[xp,xp] <- -H.x
            AP[xp == FALSE, xp== FALSE] <- H.y
            s1.tmp <- solve(AP, c(c.x,c.y), tol = .Machine$double.eps, LINPACK = TRUE)
            delta.x<-s1.tmp[1:n] ; delta.y<-s1.tmp[-(1:n)]
        } else if (smw == 1) {
            smwc1 <- c.x / d
            smwc2 <- (c.x - (H %*% solve(smwinner,solve(t(smwinner),
                crossprod(H,smwc1)), tol = .Machine$double.eps, LINPACK = TRUE))) / d
            delta.y <- solve(A %*% smwa2 + H.y , c.y + A %*% smwc2, tol = .Machine$double.eps, LINPACK = TRUE)
            delta.x <- smwa2 %*% delta.y - smwc2
        }
        
        # Backsubstitution:
        delta.w <- - e * (hat.beta - q * hat.alpha / p + delta.y)
        delta.s <- s * (delta.x - hat.tau) / t
        delta.z <- z * (hat.nu - delta.x) / g
        delta.q <- q * (delta.w - hat.alpha) / p
        delta.v <- v * (gamma.w - delta.w) / w
        delta.p <- p * (gamma.q - delta.q) / q
        delta.g <- g * (gamma.z - delta.z) / z
        delta.t <- t * (gamma.s - delta.s) / s
        
        # Compute the updates
        alfa <- - (1 - margin) / 
            min(c(delta.g / g, delta.w / w, delta.t / t, delta.p / p, 
                delta.z / z, delta.v / v, delta.s / s, delta.q / q, -1))
        x <- x + delta.x * alfa
        g <- g + delta.g * alfa
        w <- w + delta.w * alfa
        t <- t + delta.t * alfa
        p <- p + delta.p * alfa
        y <- y + delta.y * alfa
        z <- z + delta.z * alfa
        v <- v + delta.v * alfa
        s <- s + delta.s * alfa
        q <- q + delta.q * alfa
        mu <- newmu
    }
    
    # Final report:
    if (verb > 0)   
      cat( counter, primal.infeasibility, dual.infeasibility, sigfig, 
        alfa, primal.obj, dual.obj)

    # List the results:
    primal <- x
    dual <- drop(y)
    if ((sigfig > sigf) & (counter < maxiter)) {
        status = 0
        message  <- 'converged'
    } else { 
        # must have run out of counts
        status = 1
        if ((primal.infeasibility > 10.0e5) & (dual.infeasibility > 10.0e5))
          message <- 'primal and dual infeasible'
        if (primal.infeasibility > 10.0e5)
          message <- 'primal infeasible'
        if (dual.infeasibility > 10.0e5)
          message <- 'dual infeasible'
        else # don't really know
          message <- 'slow convergence, change bound?'
    }
    
    list(
        weights = primal, status = status,
        dual = dual, message = message, iterations = counter)
}


        
################################################################################

