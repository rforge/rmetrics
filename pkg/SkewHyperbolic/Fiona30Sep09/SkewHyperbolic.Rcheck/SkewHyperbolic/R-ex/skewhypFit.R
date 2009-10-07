### Name: skewhypFit
### Title: Fit the Skew Hyperbolic Student's t-Distribution to Data
### Aliases: skewhypFit plot.skewhypFit print.skewhypFit summary.skewhypFit
### Keywords: distribution

### ** Examples

## See how well skewhypFit works
param <- c(0,1,4,10)
data <- rskewhyp(500, param=param)
fit <- skewhypFit(data)
## Use data set NOK/EUR as per Aas&Haff
data(lrnokeur)
nkfit <- skewhypFit(lrnokeur, method = "nlm")
## Use data set DJI
data(lrdji)
djfit <- skewhypFit(lrdji)



