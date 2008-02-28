checkBeforeCommit  <-
    function(package, lib = NULL, outdir = NULL, ...)
{
    stopifnot(is.character(package))

    source("installRmetrics.R")

    # Set library and outdir paths
    if (is.null(lib)) lib <- .libPaths()[1]
    if (is.null(outdir)) outdir <- "Rcheck"

    # if outdir does not exist, create it
    if (!file.exists(outdir)) dir.create(outdir)

    # extract list of Rmetrics packages
    pkgsRmetrics <- getDepends("Rmetrics")
    stopifnot(package %in% pkgsRmetrics)

    # remove package which do not depend on the package we want to test
    idx <- min(match(package, pkgsRmetrics))
    pkgsToCheck <- pkgsRmetrics[seq(idx, length(pkgsToCheck))]

    # Run R CMD check ...
    Rbin <- paste(R.home(), "bin", "R", sep = "/")
    Rcmd <- paste(Rbin, "CMD check")
    options <- paste("--library=", lib, " --outdir=", outdir, sep = "")
    cmd <- paste(Rcmd, options, paste(pkgsToCheck, collapse = " "), ...)
    try(system(cmd))

    logWarning <- NULL
    # check for ERRORs and WARNINGs
    for (i in seq(pkgsToCheck)) {

        dirCheck <- paste(pkgsToCheck[i], ".Rcheck", sep = "")
        logFile <- paste(outdir, dirCheck, "00check.log", sep ="/")
        log  <- readLines(logFile)

        ERROR <- grep("ERROR", log)
        WARNING <- grep("WARNING", log)

        if (length(ERROR)) {
            msg <- paste("More details in", logFile)
            stop(msg)
        }
        if (length(WARNING)) {
            logWarning <- c(logWarning,
            "\n ####################### WARNING #######################\n",
            paste(" In package", pkgsToCheck[i], "\n"),
            paste(paste(log[seq(WARNING, WARNING+5)], collapse = "\n "), "\n"),
            paste("\n More details in", logFile, "\n"),
            paste(" ####################### WARNING #######################\n"))
        }
    }

    # print WARINGs
    cat(logWarning)

    # Return
    if (length(WARNING) | length(ERROR)) STATUS <- FALSE else STATUS <- TRUE
    return(STATUS)
}
