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
    function(repoCRAN = "http://stat.ethz.ch/CRAN/", suggests = TRUE, ...)
{
    pkgs <- getDepends("Rmetrics")

    # extract dependencies of third packages
    pkgsDepends <- NULL
    for (i in seq(length(pkgs))) {
        pkgsDepends <- c(pkgsDepends, getDepends(pkgs[i]))
        if (suggests)
            pkgsDepends <- c(pkgsDepends, getSuggests(pkgs[i]))
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
            install.packages(pkgsDepends[i], repos = repoCRAN, ...)
        }
    }
    ### # Note Rdonlp2 is not part of Rmetrics !!
    ### if (!require(Rdonlp2, quietly = TRUE)) {
    ### install.packages("Rdonlp2", repos = repoRmetrics, type = "source", ...)
    ### }
    options(ow) # set default warning option

    # install Rmetrics packages from local files
    install.packages(pkgs, repos = NULL, type = "source", ...)

    # install Rmetrics package
    install.packages("Rmetrics", repos = NULL, type = "source", ...)

    OK <- require("Rmetrics")

    # Return
    return(OK)
}


getDepends <-
    function(package)
{
    stopifnot(is.character(package))
    ans <- NULL
    for (i in seq(length(package))) {
        file <- paste(package[i], "DESCRIPTION", sep = "/")
        pkgInfo <- tools:::.split_description(tools:::.read_description(file))
        ans <- c(ans, names(pkgInfo$Depends))
    }

    # Return
    ans
}

getSuggests <-
    function(package)
{
    stopifnot(is.character(package))
    ans <- NULL
    for (i in seq(length(package))) {
        file <- paste(package[i], "DESCRIPTION", sep = "/")
        pkgInfo <- tools:::.split_description(tools:::.read_description(file))
        ans <- names(pkgInfo$Suggests)
    }

    # Return
    ans
}

getPath <-
    function(package)
{
    # extract path of package in order to set working directory
    path <- unlist(strsplit(package, "/"))
    if (path[length(path)] == "") path <- path[-length(path)]
    package <- path[length(path)]
    path <- paste(path[-length(path)], collapse = "/")
    ans <- list(package = package, path = path)
    ans
}
