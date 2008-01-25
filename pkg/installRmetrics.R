################################################################################
## Install Rmetrics packages
##
## Install Rmetrics packages using the installRmetrics.R script. Open an
## R process and set its working directory to this directory.
## Then type the following :
##
## > source("installRmetrics.R")
## > installRmetrics()
##
## This installs all Rmetrics packages from source and installs dependent
## packages from CRAN server if they are not already installed.
################################################################################

installRmetrics  <-
    function(repoRmetrics = NULL, ...)
{

    file <- "Rmetrics/DESCRIPTION"
    pkgInfo <- tools:::.split_description(tools:::.read_description(file))
    pkgs <- names(pkgInfo$Depends)

    # extract dependencies of third packages
    fileDepends = paste(pkgs, "DESCRIPTION", sep = "/")
    pkgsDepends <- NULL
    for (i in seq(length(fileDepends))) {
        pkgInfoDepends <-
           tools:::.split_description(tools:::.read_description(fileDepends[i]))
        pkgsDepends <- c(pkgsDepends, names(pkgInfoDepends$Depends))
    }

    # remove Rmetrics packages
    pkgsDepends <- pkgsDepends[!(pkgsDepends %in% pkgs)]

    # remove double entries
    # ...

    # Remove Rdonlp2 from list because it is installed from local directory
    pkgsDepends <- pkgsDepends[(pkgsDepends != "Rdonlp2")]

    # disable unnecessary warning message when package is not installed
    ow <- options("warn")

    # install third packages only not already installed
    for (i in seq(length(pkgsDepends))) {
        options(warn = -1)
        if (!require(pkgsDepends[i], character.only = TRUE, quietly = TRUE)) {
            options(ow)
            install.packages(pkgsDepends[i], ...)
        }
    }
    # Note Rdonlp2 is not part of Rmetrics !!
    options(warn = -1)
    if (!require(Rdonlp2, quietly = TRUE)) {
        options(ow)
        install.packages("Rdonlp2", repos = repoRmetrics, type = "source", ...)
    }

    options(ow) # set default warning option

    # install Rmetrics packages from local files
    install.packages(pkgs, repos = repoRmetrics, type = "source", ...)

    # install Rmetrics package
    install.packages("Rmetrics", repos = repoRmetrics, type = "source", ...)

    # Return
    return (require("Rmetrics"))
}
