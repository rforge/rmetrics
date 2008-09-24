


.rollingDrawdowns <-
function(x, period = "24m", by = "1m", ...)
{
    stopifnot(isUnivariate(x))
    
    # Create Rolling Windows:
    rW = rollingWindows(x, period, by)

    # Roll the Function and Save in List:
    ans = NULL
    for (i in 1:length(rW$from)) {

        X = window(x, start = rW$from[i], end = rW$to[i])
        Y = min(drawdowns(X))
        ans = c(ans, Y)
    }

    # Return Value
    invisible(ans)
}


.rollingEMA <-
function(x, period = "24m", by = "1m", lambda = 0.1, ...)
{
    stopifnot(isUnivariate(x))
    
    # Create Rolling Windows:
    rW = rollingWindows(x, period, by)

    # Roll the Function and Save in List:
    ans = NULL
    for (i in 1:length(rW$from)) {

        X = window(x, start = rW$from[i], end = rW$to[i])
        Y = rev(emaTA(X, lambda))[1]
        ans = c(ans, Y)
    }

    # Return Value
    invisible(ans)
}



par(mfrow=c(3,3)); for (i in 1:9) plot(.rollingDrawdowns(X[, i], "3m"), type = "b")
par(mfrow=c(3,3)); for (i in 1:9) plot(.rollingEMA(abs(X[, i]), "3m"), type = "b")