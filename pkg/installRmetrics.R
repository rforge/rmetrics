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
## Suggested packages are installed if "suggests = TRUE"
################################################################################

installRmetrics  <-
    function(repoRmetrics = NULL, repoCRAN = "http://stat.ethz.ch/CRAN/",
             suggests = TRUE)
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
        if (suggests)
            pkgsDepends <- c(pkgsDepends, names(pkgInfoDepends$Suggests))
    }

    # remove Rmetrics packages and double entries
    pkgsDepends <- unique(pkgsDepends[!(pkgsDepends %in% pkgs)])

    # Remove Rdonlp2 from list because it is installed from local directory
    pkgsDepends <- pkgsDepends[(pkgsDepends != "Rdonlp2")]

    # disable unnecessary warning message when package is not installed
    ow <- options("warn")
    options(warn = -1)
    # install third packages if not already installed
    for (i in seq(length(pkgsDepends))) {
        if (!require(pkgsDepends[i], character.only = TRUE, quietly = TRUE)) {
            install.packages(pkgsDepends[i], repos = repoCRAN)
        }
    }
    # Note Rdonlp2 is not part of Rmetrics !!
    if (!require(Rdonlp2, quietly = TRUE)) {
        install.packages("Rdonlp2", repos = repoRmetrics, type = "source")
    }
    options(ow) # set default warning option

    # install Rmetrics packages from local files
    install.packages(pkgs, repos = repoRmetrics, type = "source")

    # install Rmetrics package
    install.packages("Rmetrics", repos = repoRmetrics, type = "source")

    # Return
    return (require("Rmetrics"))
}
