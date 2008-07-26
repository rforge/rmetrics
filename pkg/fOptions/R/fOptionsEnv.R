fOptionsEnv <- new.env(hash = TRUE)

setfOptionsEnv <-
    function(...)
{
    x <- list(...)
    nm <- names(x)
     if (is.null(nm) || "" %in% nm)
        stop("all arguments must be named")
    sapply(nm, function(nm) assign(nm, x[[nm]],
                                 envir = fOptionsEnv))
    invisible()
}

getfOptionsEnv <-
    function(x = NULL, unset = "")
{
    if (is.null(x))
        x <- ls(all.names = TRUE, envir = fOptionsEnv)
###     unlist(mget(x, envir = fOptionsEnv, mode = "any",
###                 ifnotfound = as.list(unset)), recursive = FALSE)
    get(x, envir = fOptionsEnv, mode = "any")
}


