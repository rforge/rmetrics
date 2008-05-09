checkBeforeCommit  <-
    function(pkgs = "Rmetrics", lib = NULL, outdir = NULL, ...)
{
    stopifnot(is.character(pkgs))

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
    pkgsRmetrics <- getDESCR("Rmetrics", "Depends")
    stopifnot(pkgs %in% c(pkgsRmetrics, "Rmetrics"))

    ## search for packages which depends on the package we want to check
    listDepends <- lapply(pkgsRmetrics, getDepends,
                          group = pkgsRmetrics, "Depends")
    names(listDepends) <- pkgsRmetrics
    tocheck <- sapply(lapply(listDepends, "%in%", pkgs), any)
    pkgsToCheck <-
        if (pkgs == "Rmetrics") # Rmetrics virtual package -> check all
            pkgsRmetrics
        else
            c(pkgs, names(tocheck)[tocheck])

    # pkgs in good order for install
    pkgsToCheck <- pkgsRmetrics[sort(match(pkgsToCheck, pkgsRmetrics))]

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
            for (line in WARNING) {
                logWarning <- c(logWarning,
                 "\n ####################### WARNING #######################\n",
                 paste(" In package", pkg, "\n"),
                 paste(paste(log[seq(line, line+5)], collapse = "\n "), "\n"),
                 paste("\n More details in", logFile, "\n"),
                 paste(" ####################### WARNING #######################\n"))
            }
        }
        if (length(NOTE)) {
            for (line in NOTE) {
                logNOTE <- c(logNOTE,
                 "\n #######################  NOTE  #######################\n",
                 paste(" In package", pkg, "\n"),
                 paste(paste(log[seq(line, line+8)], collapse = "\n "), "\n"),
                 paste("\n More details in", logFile, "\n"),
                 paste(" #######################  NOTE  #######################\n"))
            }
        }
    }
    ## print Notes and Warnings:
    message(logNOTE)
    message(logWarning)

    ## Return
    STATUS <- !(length(WARNING) || length(ERROR))
    return(STATUS)
}
