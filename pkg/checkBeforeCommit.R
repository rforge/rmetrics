checkBeforeCommit  <-
    function(package = "Rmetrics", lib = NULL, outdir = NULL, ...)
{
    stopifnot(is.character(package))

    installFile <- "installRmetrics.R"
    if(!file.exists(installFile)) {
        user <- Sys.getenv("USER")
        myDir <-
            switch(user,
                   "maechler" = "~/R/D/R-forge/Rmetrics",
                   "yankee" = "~/projects/rmetrics/",
                   "wuertz" = stop(" please fix in checkBeforeCommit()"),
                   ## otherwise:
                   stop("unknown user: please fix in checkBeforeCommit()"))

        setwd(file.path(myDir, "pkg"))
        ##                    ------- on R-forge

        if(!file.exists(installFile))
            stop(installFile," is not in current directory",
                 "(",getwd(),")")
    }
    message("in ", getwd(),": source()ing ", installFile,":")
    source(installFile)

    ## Set library and outdir paths
    if (is.null(lib)) {
        lib <- .libPaths()[1]
        message("will install the R packages into ", lib)
    }
    if (is.null(outdir)) outdir <- "../Rcheck"

    ## if outdir does not exist, create it
    if (!file.exists(outdir)) dir.create(outdir)

    ## extract list of Rmetrics packages
    pkgsRmetrics <- getDepends("Rmetrics")
    if (package == "Rmetrics") ## 'Rmetrics' is virtual: check all of them:
        package <- pkgsRmetrics
    stopifnot(package %in% pkgsRmetrics)

    ## remove package which do not depend on the package we want to test
    ## # 1 possibility (to be discussed)
    idx <- min(match(package, pkgsRmetrics))
    pkgsToCheck <- pkgsRmetrics[seq(idx, length(pkgsRmetrics))]
    ## 2 possibility (to be discussed)
###     pkgsToCheck <- package
###     for (i in seq(pkgsRmetrics)) {
###         if (package %in% getDepends(pkgsRmetrics[i]))
###             pkgsToCheck <- c(pkgsToCheck, pkgsRmetrics[i])
###     }


    ## Run R CMD check ...
    Rbin <- file.path(R.home(), "bin", "R")
    Rcmd <- paste(Rbin, "CMD check")
    options <- paste("--library=", lib,
                     " --outdir=", outdir, sep = "")
    cmd <- paste(Rcmd, options, paste(pkgsToCheck, collapse = " "), ...)
    try(system(cmd))

    logWarning <- NULL
    logNOTE <- NULL
    ## check for ERRORs and WARNINGs
    for (i in seq_along(pkgsToCheck)) {
        pkg <- pkgsToCheck[i]
        dirCheck <- paste(pkg, ".Rcheck", sep = "")
        logFile <- file.path(outdir, dirCheck, "00check.log")
        log  <- readLines(logFile)

        ERROR <- grep("ERROR", log)
        WARNING <- grep("WARNING", log)
        NOTE <- grep("NOTE", log)

        if (length(ERROR)) {
            msg <- paste("More details in", logFile)
            stop(msg)
        }
        if (length(WARNING)) {
            logWarning <- c(logWarning,
            "\n ####################### WARNING #######################\n",
            paste(" In package", pkg, "\n"),
            paste(paste(log[seq(WARNING, WARNING+5)], collapse = "\n "), "\n"),
            paste("\n More details in", logFile, "\n"),
            paste(" ####################### WARNING #######################\n"))
        }
        if (length(NOTE)) {
            logNOTE <- c(logNOTE,
            "\n #######################  NOTE  #######################\n",
            paste(" In package", pkg, "\n"),
            paste(paste(log[seq(NOTE, NOTE+8)], collapse = "\n "), "\n"),
            paste("\n More details in", logFile, "\n"),
            paste(" #######################  NOTE  #######################\n"))
        }
    }

    ## print Notes and Warnings:
    message(logNOTE)
    message(logWarning)

    ## Return
    STATUS <- !(length(WARNING) || length(ERROR))
    return(STATUS)
}
