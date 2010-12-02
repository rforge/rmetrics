require(DistributionUtils)
require(GeneralizedHyperbolic)
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/pghyp.R")
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/qghyp.R")
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/rghyp.R")
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/ghypCheckPars.R")
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/ghypMeanVarMode.R")
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/dghyp.R")
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/ghypCalcRange.R")
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/ghypMom.R")
source("C:/Users/xli053/Documents/Hyperbolic/ghypDone/ghypParam.R")
source("C:/Users/xli053/Documents/Hyperbolic/DUtils/Moran/moranTest.R")

## Alpha = 0.9
sampleSize <- c(20, 50, 100, 500)
prob <- rep(0, length(sampleSize))
result = 0
##Bootstrap numbers
n <- 10000

##Normal Distribution

for (j in 1:length(sampleSize))
{
    for(i in 1:n)
    {
        data = rnorm(sampleSize[j], mean = 0, sd = 1)
        muhat = mean(data)
        sigmahat = sqrt(var(data) *
                        (sampleSize[j] - 1) / sampleSize[j])
        result = result + moranTest(data, "norm",
        alpha = 0.9, mean = muhat, sd = sigmahat)
    }
    prob[j] = 1 - result / n
    result = 0
}
prob

##Exponential Distribution
lambda = 3
for (j in 1:length(sampleSize))
{
    for(i in 1:n)
    {
        data = rexp(sampleSize[j], rate = lambda)
        lambdahat = 1 / mean(data)
        result = result + moranTest(data, "exp", alpha = 0.9, rate = lambdahat)
    }
    prob[j] = 1 - result / n
    result = 0
}
prob

## Alpha = 0.95
sampleSize <- c(20, 50, 100, 500)
prob <- rep(0, length(sampleSize))
result = 0
##Bootstrap numbers
n <- 10000

##Normal Distribution

for (j in 1:length(sampleSize))
{
    for(i in 1:n)
    {
        data = rnorm(sampleSize[j], mean = 0, sd = 1)
        muhat = mean(data)
        sigmahat = sqrt(var(data) *
                        (sampleSize[j] - 1) / sampleSize[j])
        result = result + moranTest(data, "norm",
        alpha = 0.95, mean = muhat, sd = sigmahat)
    }
    prob[j] = 1 - result / n
    result = 0
}
prob

##Exponential Distribution
lambda = 3
for (j in 1:length(sampleSize))
{
    for(i in 1:n)
    {
        data = rexp(sampleSize[j], rate = lambda)
        lambdahat = 1 / mean(data)
        result = result + moranTest(data, "exp",
                                    alpha = 0.95, rate = lambdahat)
    }
    prob[j] = 1 - result / n
    result = 0
}
prob

##Generalized Hyperbolic Distribution
##difficultSmalls <- c(1:6, 10:12)
##smallShape <- ghypSmallShape[-difficultSmalls, ]

##difficultSmallp <- c(1:6, 10:12, 25:27, 31:33, 43:48, 52:54, 67:69, 73:75)
##smallParam <- ghypSmallParam[-difficultSmallp, ]

##difficultLarges <- c(1:12, 19:24, 37:42, 64:66)
##largeShape <- ghypLargeShape[-difficultLarges, ]
##difficultLargep <- c(1:12, 19:24, 37:42, 64:66, 91:102, 109:114, 127:132,
                     ##154:156, 187:192, 199:204, 217:222, 244:246, 256,
                     ##277:282, 289:294, 307:312, 334:336, 346, 361:372,
                     ##379:384, 397:402, 424:426, 457:462, 469:474, 487:492,
                     ##514:516, 547:552, 559:564, 577:582, 604:606, 616,
                     ##637:642, 649:654, 667:672, 694:696, 706, 721:732,
                     ##739:744, 758:762, 784:786, 811:822, 829:834, 848:852,
                     ##874:876, 907:912, 919:924, 937:942, 964:966, 976,
                     ##997:1002, 1009:1014, 1027:1032, 1054:1056, 1066,
                     ##1081:1086, 1092, 1100:1104, 1119:1122, 1144:1146,
                     ##1171:1182, 1189:1194, 1208:1212, 1234:1236, 1262,
                     ##1265:1272, 1279:1284, 1298:1302, 1324:1326, 1357:1362,
                     ##1369:1374, 1387:1392, 1414:1416)
##largeParam <- ghypLargeParam[-difficultLargep, ]

##testParam <- rbind(smallShape, smallParam)
##testParam <- rbind(testParam, largeShape)
##testParam <- rbind(testParam, largeParam)

## sample parameter values
##np <- nrow(testParam)[1]
##paramNum <- sample(1:np, 1, replace = FALSE)
##paramVals <- testParam[paramNum,,drop = FALSE]

##for (j in 1:length(sampleSize))
##{
    ##for(i in 1:n)
    ##{
        ##data = rghyp(sampleSize[j], param = paramVals)
        ##result = result + moranTest(data, "ghyp",
        ##alpha = 0.9, param = paramVals)
    ##}
    ##prob[j] = 1 - result / n
    ##result = 0
##}
##prob

