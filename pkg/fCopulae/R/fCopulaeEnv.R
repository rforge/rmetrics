fCopulaeEnv <- new.env(hash = TRUE)

setfCopulaeEnv <-
    function(...)
{
    x <- list(...)
    nm <- names(x)
     if (is.null(nm) || "" %in% nm)
        stop("all arguments must be named")
    sapply(nm, function(nm) assign(nm, x[[nm]],
                                 envir = fCopulaeEnv))
    invisible()
}

getfCopulaeEnv <-
    function(x = NULL, unset = "")
{
    if (is.null(x))
        x <- ls(all.names = TRUE, envir = fCopulaeEnv)
###     unlist(mget(x, envir = fCopulaeEnv, mode = "any",
###                 ifnotfound = as.list(unset)), recursive = FALSE)
    get(x, envir = fCopulaeEnv, mode = "any")
}
