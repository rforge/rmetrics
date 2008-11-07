test.COPPosterior <- function()
{
    NUMTESTSIMULATIONS <- 1000
    .unitTestPath <- BLCOPOptions("unitTestPath")
    testEnvironment <- new.env()
    
    dispersion <- c(.376,.253,.360,.333,.360,.600,.397,.396,.578,.775) / 1000
    sigma <- BLCOP:::.symmetricMatrix(dispersion, dim = 4)
    caps <- rep(1/4, 4)
    mu <- 2.5 * sigma %*% caps
    dim(mu) <- NULL
    marketDistribution <- mvdistribution("mt", mean = mu, S = sigma, df = 5 )
    pick <- matrix(0, ncol = 4, nrow = 1, dimnames = list(NULL, c("SP", "FTSE", "CAC", "DAX")))
    pick[1,4] <- 1
    vdist <- list(distribution("unif", min = -0.02, max = 0))

    views <- COPViews(pick, vdist, 0.2, c("SP", "FTSE", "CAC", "DAX"))
    tmp <- load(file.path(.unitTestPath, "posteriorsims.RData"))
    assign(tmp, value = get(tmp), envir = testEnvironment)
    set.seed(3)
    posterior <- COPPosterior(marketDistribution, views, numSimulations = NUMTESTSIMULATIONS)
    # TODO: check why this is not exactly equal
    checkEquals(posterior@posteriorSims, testEnvironment$posteriorSims, tol = 1.5)
}