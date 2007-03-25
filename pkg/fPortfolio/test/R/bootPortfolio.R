
require(boot)
require(fSeries)


################################################################################


meanBoot =
function(x, R = 100)
{
    # Transform:
    x = as.matrix(x)
    
    # Internal One Variable Bootstrap:
    oneBoot <- 
    function (x, statistic, R, ...) {
        # Inspired by package simpleboot ...
        statisticFun <- match.fun(statistic)
        bootFun <- function(x, idx) { statisticFun(x[idx], ...) }
        boot(data = x, statistic = bootFun, R = R)
    }
    
    # Settings:
    N = dim(x)[2]
    t0 = rep(0, N)
    t = NULL
    
    # Loop over all Mean Vector Elements:
    for (n in 1:N) {
        z = oneBoot(x = X[ ,n], statistic = mean, R = R)
        t0[n] = z$t0
        t = cbind(t, z$t)      
    }
    
    # Column Mean and Standard Deviation:
    m.vec = colMeans(t)
    v.vec = sqrt(colVars(t))
    
    # Result:
    ans = list(original = t0, bias = t0-m.vec, estimate = m.vec, se = v.vec,
        sample = t)
    class(ans) = "sboot"
    
    # Return Value:
    ans
}   


# ------------------------------------------------------------------------------


covBoot = 
function(x, R = 100)
{
    # Transform
    x = as.matrix(x)
    
    # Internal Covariance Function
    Cov <- 
    function(x, y) cov(cbind(x, y))[1, 2]
    
    # Internal Pairs Bootstrap:
    pairsBoot <-
    function(x, y, statistic, R, ...) { 
        # Inspired by package simpleboot ...
        Data <- cbind(x, y)
        statisticFun <- match.fun(statistic)
        bootFun <- function(x, idx) { statisticFun(x[idx,1], x[idx,2], ...) }
        boot(data = Data, statistic = bootFun, R = R)
    }

    # Settings:
    N = dim(x)[2]
    t0 =  matrix(rep(0, N*N), N)
    t = NULL
    
    # Loop over all Covariance Matrix Elements:
    for (m in 1:N) {
        for (n in m:N) {
            z = pairsBoot(x = x[, m], y = x[, n], Cov, R = R)
            t0[m, n] = t0[n, m] = z$t0
            t = cbind(t, z$t)      
        }
    }
    
    # Column Mean and Standard Deviation:
    M = colMeans(t)
    V = sqrt(colVars(t))
    m.mat = v.mat = matrix(rep(0, N*N), N)
    k = 0
    for (m in 1:N) {
        for (n in m:N) {
            k = k+1
            m.mat[m, n] = m.mat[n, m] = M[k]
            v.mat[m, n] = v.mat[n, m] = V[k]
        }
    }
    
    # Result:
    ans = list(original = t0, bias = t0-m.mat, estimate = m.mat, se = v.mat,
        sample = t)
    class(ans) = "sboot"
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


print.sboot =
function(x)
{
    print(x$estimate)
}


# ------------------------------------------------------------------------------


require(boot)


X = cbind(rnorm(100), rnorm(100), rnorm(100))

covBoot(X)
meanBoot(X)

x = rnorm(100)

covBoot(x)
meanBoot(x)


# ------------------------------------------------------------------------------

