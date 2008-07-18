RmetricsOptions <- new.env(hash = TRUE)

setRmetricsOptions <-
    function(...)
{
    x <- list(...)
    nm <- names(x)
     if (is.null(nm) || "" %in% nm)
        stop("all arguments must be named")
    sapply(nm, function(nm) assign(nm, x[[nm]],
                                 envir = fUtilities:::RmetricsOptions))
    invisible()
}

getRmetricsOptions <-
    function(x = NULL, unset = "")
{
    if (is.null(x))
        x <- ls(all.names = TRUE, envir = fUtilities:::RmetricsOptions)
    unlist(mget(x, envir = fUtilities:::RmetricsOptions, mode = "any",
                ifnotfound = as.list(unset)))
}
