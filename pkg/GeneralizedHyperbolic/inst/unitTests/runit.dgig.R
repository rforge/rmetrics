### Unit tests of function dgig

### Functions with name test.* are run by R CMD check or by make if
### LEVEL=1 in call to make
### Functions with name levelntest.* are run by make if
### LEVEL=n in call to make
### Functions with name graphicstest.* are run by make if
### LEVEL=graphics in call to make

### Graphical Test for Generalized Hyperbolic Distribution
graphicstest.dgig <- function()
{
    ## Purpose: Level 1 test of dgig
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: David Scott, Date:  19 April 2013

    ## Important is to test for large sqrt(psi*chi) to avoid underflow
    ## Purpose: Test the dgig function with small range parameters
    n <- 1000
    data(gigParam)
    smallparam <- gigSmallParam
    smallchi <- smallparam[, 1]
    smallpsi <- smallparam[, 2]
    smalllambda <- smallparam[, 3]
    sden <- 0
    ## open file for graphical output
    graphicsOutput <- paste(pathReport, "dgig.pdf", sep = "")
    cat("Graphics output in file ", graphicsOutput, "\n")
    pdf(file = graphicsOutput, height = 7,width = 10)
    par(mfrow = c(1, 2), oma = c(5, 5, 5, 5))
    for (i in 1 : nrow(smallparam))
    {
        x <- rgig(n, param = smallparam[i, ])
        sde <- density(x, bw = 0.1)$y
        sden <- dgig(x,param = smallparam[i, ])
        hist(x, freq = FALSE, breaks = 20, ylim = c(0, max(sden, sde)),
             main = "", xlab = "sample")
        mtext(expression(bold("Graph Test of dgig")), line = 3.5, cex = 1.15)
        mtext(bquote(paste(lambda ==.(smalllambda[i]), ",",
                           psi ==.(smallpsi[i]), ",",
                           chi ==.(smallchi[i]),sep = "")),
              line = 2.25, cex = 1.15)
        curve(dgig(x, param = smallparam[i, ]), add = TRUE, col = "red")
        logHist(x, main = "", breaks = 20, htype = "h")
        mtext(expression(bold("Log Graph Test of dgig")),
              line = 3.5, cex = 1.15)
        mtext(bquote(paste(lambda ==.(smalllambda[i]), ",",
                           psi ==.(smallpsi[i]), ",",
                           chi ==.(smallchi[i]),sep = "")),
              line = 2.25, cex = 1.15)
        curve(log(dgig(x, param = smallparam[i, ])), add = TRUE, col = "red")
        i <- i + 1
    }



    ## Purpose: Test the dgig function with large range parameters
    largeparam <- gigLargeParam
    largechi <- largeparam[, 1]
    largepsi <- largeparam[, 2]
    largelambda <- largeparam[, 3]
    lden <- 0
    par(mfrow = c(1, 2), oma = c(5, 5, 5, 5))
    for (i in 1 : nrow(largeparam))
    {
        x <- rgig(n, param = largeparam[i, ])
        lde <- density(x, bw = 0.1)$y
        lden <- dgig(x, param = largeparam[i, ])
        hist(x, freq = FALSE, breaks = 20, ylim = c(0, max(lden, lde)),
             main = "", xlab = "sample")
        mtext(expression(bold("Graph Test of dgig")),
              line = 3.5, cex = 1.15)
        mtext(bquote(paste(lambda ==.(largelambda[i]), ",",
                           psi ==.(largepsi[i]), ",",
                           chi ==.(largechi[i]),sep = "")),
              line = 2.25, cex = 1.15)
        curve(dgig(x, param = largeparam[i, ]), add = TRUE, col = "red")
        logHist(x, main = "", breaks = 20, htype = "h")
        mtext(expression(bold("Log Graph Test of dgig")),
              line = 3.5, cex = 1.15)
        mtext(bquote(paste(lambda ==.(largelambda[i]),",",
                           psi ==.(largepsi[i]),",",
                           chi ==.(largechi[i]),sep = "")),
              line = 2.25, cex = 1.15)
        curve(log(dgig(x,param = largeparam[i, ])), add = TRUE, col = "red")
        i <- i + 1
    }

    ## Purpose: Test extreme gig parameters
    extremeParam <- matrix(c(10000,100,-0.5,
                              10000,100,0.0,
                              10000,100,0.5,
                              10000,100,1.0,
                              10000,100,5.0,
                              100,10000,-0.5,
                              100,10000,0.0,
                              100,10000,0.5,
                              100,10000,1.0,
                              100,10000,5.0),
                       byrow = TRUE, ncol = 3)
    extremeparam <- extremeParam
    extremechi <- extremeparam[, 1]
    extremepsi <- extremeparam[, 2]
    extremelambda <- extremeparam[, 3]
    lden <- 0
    par(mfrow = c(1, 2), oma = c(5, 5, 5, 5))
    for (i in 1 : nrow(extremeparam))
    {
        x <- rgig(n, param = extremeparam[i, ])
        lde <- density(x, bw = 0.1)$y
        lden <- dgig(x, param = extremeparam[i, ])
        hist(x, freq = FALSE, breaks = 20, ylim = c(0, max(lden, lde)),
             main = "", xlab = "sample")
        mtext(expression(bold("Graph Test of dgig")),
              line = 3.5, cex = 1.15)
        mtext(bquote(paste(lambda ==.(extremelambda[i]), ",",
                           psi ==.(extremepsi[i]), ",",
                           chi ==.(extremechi[i]),sep = "")),
              line = 2.25, cex = 1.15)
        curve(dgig(x, param = extremeparam[i, ]), add = TRUE, col = "red")
        logHist(x, main = "", breaks = 20, htype = "h")
        mtext(expression(bold("Log Graph Test of dgig")),
              line = 3.5, cex = 1.15)
        mtext(bquote(paste(lambda ==.(extremelambda[i]),",",
                           psi ==.(extremepsi[i]),",",
                           chi ==.(extremechi[i]),sep = "")),
              line = 2.25, cex = 1.15)
        curve(log(dgig(x,param = extremeparam[i, ])), add = TRUE, col = "red")
        i <- i + 1
    }

    dev.off()

    return()
}


