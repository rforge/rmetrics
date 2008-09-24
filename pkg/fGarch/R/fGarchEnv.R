
.fGarchEnv <- new.env(hash = TRUE)

.setfGarchEnv <-
    function(...)
{
    x <- list(...)
    nm <- names(x)
     if (is.null(nm) || "" %in% nm)
        stop("all arguments must be named")
    sapply(nm, function(nm) assign(nm, x[[nm]],
                                 envir = .fGarchEnv))
    invisible()
}

.getfGarchEnv <-
    function(x = NULL, unset = "")
{
    if (is.null(x))
        x <- ls(all.names = TRUE, envir = .fGarchEnv)
###     unlist(mget(x, envir = .fGarchEnv, mode = "any",
###                 ifnotfound = as.list(unset)), recursive = FALSE)
    get(x, envir = .fGarchEnv, mode = "any")
}
