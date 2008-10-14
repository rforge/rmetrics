dependsRmetrics <-
    function(pkgs = "all", contrib =  "http://stat.ethz.ch/CRAN/src/contrib")
{
    stopifnot(is.character(pkgs))

    installFile <- "installRmetrics.R"
    if(!file.exists(installFile))
        stop(installFile," is not in current directory",
             "(",getwd(),")")

    message("source()ing ", installFile, " in ", getwd(),"... ", appendLF = FALSE)
    source(installFile)
    message("OK")

    ## extract list of Rmetrics packages
    pkgsRmetrics <- .packagesRmetrics()
    stopifnot(pkgs %in% c(pkgsRmetrics, "all"))

    if (any(pkgs == "all")) pkgs <- pkgsRmetrics

    # downloading list of available packages
    message("downloading list of available packages... ", appendLF = FALSE)
    info <- available.packages()
    message("OK")

    idx <- unlist(sapply(pkgs, grep, info[,"Depends"]))
    idx <- c(idx, unlist(sapply(pkgs, grep, info[,"Suggests"])))
    idx <- unique(idx)

    pkgsDepends <- rownames(info)[idx]
    pkgsDepends <- pkgsDepends[!(pkgsDepends %in% pkgsRmetrics)]

    pkgsDepends
}
