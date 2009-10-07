########################################################
###
### Test if sample means agree with theoretical
###
########################################################

### set up
date()
library(HyperbolicDist)
library(actuar)

### load the skewhyp files
source("dskewhyp.R")

###set parameters
n <- 10000
testSet <- "Example"

### which set to use?
if(testSet == "Large"){# 420
    mus <- c(-10, 0, 10)
    deltas <- c(1, 5, 10, 50)
    betas <- c(-80, -40, -20, 0, 10, 50, 100)
    nus <- c(1, 5, 10, 20, 50)
}
if(testSet == "Small"){# 90
    mus <- c(0, 10)
    deltas <- c(1, 5, 10)
    betas <- c(-100, -20, 0, 10, 50)
    nus <- c(1, 10, 50)
}
if(testSet == "Example"){# 24
    mus <- c(0)
    deltas <- c(0.01,1,5)
    betas <- c(0,2,4,10)
    nus <- c(10,15)
}

### set up the parameter sets to test
maxrows <- length(mus)*length(deltas)*length(betas)*length(nus)
paramSets <- matrix(nrow = maxrows, ncol = 4)
rownum <- 1
for(i in 1:length(mus)){
    for(j in 1:length(deltas)){
        for(k in 1:length(betas)){
            for(l in 1:length(nus)){
                param <- c(mus[i],deltas[j],betas[k],nus[l])
                paramSets[rownum,] <- param
                rownum <- rownum + 1
            }
        }
    }
}

### set up matrix for results
results <- matrix(nrow = maxrows, ncol = 8)
results[,5:8] <- paramSets


### set up pdf output
#pdf("testskewhypMom.pdf", height = 7, width = 11)
par(mfrow = c(1, 2))
par(oma = c(2, 0, 4, 0))
set.seed(123)
###go through each parameter set
for( i in 1:maxrows){
    param = paramSets[i,]
    mu <- param[1]
    delta <- param[2]
    beta <- param[3]
    nu <- param[4]
    results[i,1] <- mu + delta^2*beta/(nu - 2)
    results[i,3] <- 2*delta^4*beta^2/((nu - 2)^2*(nu - 4)) +
                    delta^2/(nu - 2)   

    dataVector <- rskewhyp(n, param = param)

    results[i,2] <- mean(dataVector)
    results[i,4] <- var(dataVector)
  }


plot(results[,1],results[,2])
plot(results[,3],results[,4])

results

dev.off()





###q(save="no")
