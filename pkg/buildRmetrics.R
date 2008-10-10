buildRmetrics  <-
    function(pkgs = "all", ...)
{
    stopifnot(is.character(pkgs))

    installFile <- "installRmetrics.R"
    if(!file.exists(installFile)) {
        user <- Sys.getenv("USER")
        myDir <-
            switch(user,
                   "maechler" = "~/R/D/R-forge/Rmetrics",
                   "yankee" = "~/r/",
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

    ## extract list of Rmetrics packages
    pkgsRmetrics <- c(getDESCR("Rmetrics", "Depends"), "Rmetrics")
    stopifnot(pkgs %in% c(pkgsRmetrics, "all"))

    if (pkgs == "all")
        pkgs <- pkgsRmetrics

    ## Run R CMD check ...
    Rbin <- file.path(R.home(), "bin", "R")
    Rcmd <- paste(Rbin, "CMD build")
    # options <- paste("--library=", lib,
    # " --outdir=", outdir, sep = "")
    cmd <- paste(Rcmd, paste(pkgs, collapse = " "), ...)
    message("Running: ", cmd, " ...")
    run <- try(log <- system(cmd, intern = FALSE))

###     logError <- NULL
###     logWarning <- NULL
###     logNOTE <- NULL

###     ERROR <- grep("ERROR", log)
###     WARNING <- grep("WARNING", log)
###     NOTE <- grep("NOTE", log)

###     if (length(ERROR)) {
###         for (line in ERROR) {
###             logError <-
###                 c(logError,
###                   "\n ####################### ERROR #######################\n",
###                   paste(paste(log[seq(line, line+5)], collapse = "\n "), "\n"),
###                   paste(" ####################### WARNING #######################\n"))
###         }
###         stop(logError)
###     }
###     if (length(WARNING)) {
###         for (line in WARNING) {
###             logWarning <- c(logWarning,
###                             "\n ####################### WARNING #######################\n",
###                             paste(paste(log[seq(line, line+5)], collapse = "\n "), "\n"),
###                             paste(" ####################### WARNING #######################\n"))
###         }
###     }
###     if (length(NOTE)) {
###         for (line in NOTE) {
###             logNOTE <- c(logNOTE,
###                          "\n #######################  NOTE  #######################\n",
###                          paste(paste(log[seq(line, line+8)], collapse = "\n "), "\n"),
###                          paste(" #######################  NOTE  #######################\n"))
###         }
###     }

###     ## print Notes and Warnings:
###     message(logNOTE)
###     message(logWarning)

    ## Return
    STATUS <- !(is(run, "try-error"))
    return(STATUS)
}



