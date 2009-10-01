

.nlminb2Test <-
function()
{
    # DESCRIPTION:
    #   Test Function:
    
    # DETAILS:
    #   Minimize Risk:
    #       x %*% cov %*% x
    #   Subject to:
    #       sum(x) - 1  = 0
    #       mean %*% x - targetReturn = 0
    #       -x <= 0
    #       x - 1 <= 0
    
    # FUNCTION:
    
    # Get Data:
    require(fEcofin)
    data(LPP2005REC)
    X = 100 * as.matrix(LPP2005REC[, 2:7])
    mean = colMeans(X)
    cov = cov(X)
    targetReturn = mean(X)
    N = length(mean)

    # quadprog:  
    require(quadprog)
    Dmat = cov/2
    zero = rep(0, times = N)
    dvec = zero
    Diag = diag(length(mean))
    Amat = rbind(rep(1, times = N), mean, Diag, -Diag)   
    bvec = c(1, targetReturn, zero, zero - 1)
    ans1 = round(solve.QP(Dmat, dvec, t(Amat), bvec, meq = 2)$solution, 3)
    print(round(ans1, 3))
    
    # nlminb2:
    f      <- function(x) { 0.5 * (x %*% cov %*% x)[[1]] }
    eqFun  <- function(x) { c(sum(x) - 1, (mean %*% x - targetReturn)[[1]]) }
    leqFun <- function(x) { -c(-x, x-1) }
    start = rep(1/6, times = 6)
    ans2 = .nlminb2(start, f, eqFun, leqFun, lower = 0, upper = 1)$par
    print(round(ans2, 3))
    
    # Return Value:
    invisible()
}

