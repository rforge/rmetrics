require(DistributionUtils)
require(SkewHyperbolic)

sampleSize <- c(10, 50, 100, 500, 1000)
n <- length(sampleSize)

##Skew Hyperbolic
param = c(0,1,0,10)
for (i in 1:n){
    set.seed(123)
    x <- rskewhyp(sampleSize[i],param = param)
    p <- pDist("skewhyp", q = x, param = param)
    print(system.time(qDist("skewhyp", p = p, param = param)))
}

for (i in 1:n){
    set.seed(123)
    x <- rskewhyp(sampleSize[i],param = param)
    p <- pskewhyp(x, param = param)
    print(system.time(qskewhyp(p, param = param)))
}


##Generalized Hyperbolic
for (i in 1:n){
    set.seed(123)
    x <- rghyp(sampleSize[i])
    p <- pDist("ghyp", q = x)
    print(system.time(qDist("ghyp", p = p)))
}
for (i in 1:n){
    set.seed(123)
    x <- rghyp(sampleSize[i])
    p <- pDist("ghyp", q = x)
    print(system.time(qDist("ghyp", p = p, method = "integrate")))
}

for (i in 1:n){
    set.seed(123)
    x <- rghyp(sampleSize[i])
    p <- pghyp(x)
    print(system.time(qghyp(p)))
}

for (i in 1:n){
    set.seed(123)
    x <- rghyp(sampleSize[i])
    p <- pghyp(x)
    print(system.time(qghyp(p, method = "integrate")))
}

##Normal
for (i in 1:n){
    set.seed(123)
    x <- rnorm(sampleSize[i])
    p <- pDist("norm", q = x)
    print(system.time(qDist("norm", p = p)))
}

for (i in 1:n){
    set.seed(123)
    x <- rnorm(sampleSize[i])
    p <- pnorm(x)
    print(system.time(qnorm(p)))
}


##T
for (i in 1:n){
    set.seed(123)
    x <- rt(sampleSize[i], df = 2)
    p <- pDist("t", q = x, df = 2)
    print(system.time(qDist("t", p = p, df = 2)))
}

for (i in 1:n){
    set.seed(123)
    x <- rt(sampleSize[i], df = 2)
    p <- pt(x, df = 2)
    print(system.time(qt(p, df = 2)))
}


##Gamma
for (i in 1:n){
    set.seed(123)
    x <- rgamma(sampleSize[i], shape = 2)
    p <- pDist("gamma", q = x, shape = 2)
    print(system.time(qDist("gamma", p = p, shape = 2)))
}

for (i in 1:n){
    set.seed(123)
    x <- rgamma(sampleSize[i], shape = 2)
    p <- pgamma(x, shape = 2)
    print(system.time(qgamma(p, shape = 2)))
}
