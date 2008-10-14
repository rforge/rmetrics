dependsRmetrics <-
    function(pkg = "all", contrib =  "http://stat.ethz.ch/CRAN/src/contrib")
{
    stopifnot(is.character(pkgs))

    installFile <- "installRmetrics.R"
    if(!file.exists(installFile))
        stop(installFile," is not in current directory",
             "(",getwd(),")")

    message("source()ing ", installFile, "in ",
            getwd(),"... ", appendLF = FALSE)
    source(installFile)
    message("OK")

    ## extract list of Rmetrics packages
    pkgsRmetrics <- .packagesRmetrics()
    stopifnot(pkg %in% c(pkgsRmetrics, "all"))

    if (pkg == "all")
        pkg <- pkgsRmetrics

    # downloading list of available packages
    message("downloading list of available packages... ", appendLF = FALSE)
    info <- available.packages()
    message("OK")

    idx <- unlist(sapply(pkg, grep, info[,"Depends"]))
    idx <- c(idx, unlist(sapply(pkg, grep, info[,"Suggests"])))
    idx <- unique(idx)

    pkgsDepends <- rownames(info)[idx]
    pkgsDepends <- pkgsDepends[!(pkgsDepends %in% pkgsRmetrics)]

    pkgsDepends
}
