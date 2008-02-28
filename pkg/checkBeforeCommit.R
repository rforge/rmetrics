checkBeforeCommit  <-
    function(package, lib = NULL, outdir = NULL, ...)
{
    stopifnot(is.character(package))

    source("installRmetrics.R")

    # Set library and outdir paths
    if (is.null(lib)) lib <- .libPaths()[1]
    if (is.null(outdir)) outdir <- "Rcheck"

    # if outdir does not exist, create it
    if (file.exists(outdir)) dir.create(outdir, showWarnings = FALSE)

    # extract list of Rmetrics packages
    pkgsRmetrics <- getDepends("Rmetrics")
    stopifnot(package %in% pkgsRmetrics)

    # remove package which do not depend on the package we want to test
    idx <- max(match(package, pkgsRmetrics))
    pkgsToCheck <- pkgsRmetrics[seq(idx)]

    # Run R CMD check ...
    Rbin <- paste(R.home(), "bin", "R", sep = "/")
    Rcmd <- paste(Rbin, "CMD check")
    options <- paste("--library=", lib, " --outdir=", outdir, sep = "")
    cmd <- paste(Rcmd, options, paste(pkgsToCheck, collapse = " "), ...)
    try(system(cmd))

    # check for ERRORs and WARNINGs
    for (i in seq(pkgsToCheck)) {

        dirCheck <- paste(pkgsToCheck[i], ".Rcheck", sep = "")
        logFile <- paste(outdir, dirCheck, "00check.log", sep ="/")
        log  <- readLines(logFile)

        ERROR <- grep("ERROR", log)
        WARNING <- grep("WARNING", log)
        if (length(ERROR)) {
            cat("\n ######################## ERROR ########################\n")
            cat(" In package", pkgsToCheck[i], "\n\n")
            cat(paste(log[seq(ERROR, length(ERROR))], collapse = "\n"), "\n")
            cat("\n More details in", logFile, "\n")
            cat(" ####################### WARNING #######################\n")
            stop("There was an ERROR")
        }
        if (length(WARNING)) {
            cat("\n ####################### WARNING #######################\n")
            cat(" In package", pkgsToCheck[i], "\n")
            cat(paste(log[seq(WARNING-1, WARNING+3)], collapse = "\n"), "\n")
            cat("\n More details in", logFile, "\n")
            cat(" ####################### WARNING #######################\n")
            warning("There are WARNINGs")
        }
    }

    # Return
    if (length(WARNING) && length(ERROR)) STATUS <- FALSE else STATUS <- TRUE
    return(STATUS)
}
