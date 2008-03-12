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
    pkgs <- getDepends("Rmetrics")# from current directory tree

    ## extract dependencies of third packages
    pkgsDepends <- NULL
    for (i in seq_along(pkgs)) {
        pkgsDepends <- c(pkgsDepends, getDepends(pkgs[i]),
                         if (suggests) getSuggests(pkgs[i]))
    }

    ## remove Rmetrics packages and duplicate entries
    ## --> only "outside dependencies"
    pkgsDepends <- unique(pkgsDepends[!(pkgsDepends %in% pkgs)])

    ## Remove Rdonlp2 from list because it is installed from local directory
    pkgsDepends <- pkgsDepends[pkgsDepends != "Rdonlp2"]

    ## disable unnecessary warning message when package is not installed
    ow <- options(warn = -1)
    ## install third packages if not already installed
    for (i in seq_along(pkgsDepends)) {
        if (!require(pkgsDepends[i], character.only = TRUE, quietly = TRUE)) {
            message("installing package", pkgsDepends[i],
                    " from CRAN ", repoCRAN, " ...")
            install.packages(pkgsDepends[i], repos = repoCRAN, ...)
        }
    }
    ### # Note Rdonlp2 is not part of Rmetrics !!
    ### if (!require(Rdonlp2, quietly = TRUE)) {
    ### install.packages("Rdonlp2", repos = repoRmetrics, type = "source", ...)
    ### }
    options(ow) # set default warning option

    ## install Rmetrics packages from local files
    install.packages(pkgs, repos = NULL, type = "source", ...)

    ## install Rmetrics package
    install.packages("Rmetrics", repos = NULL, type = "source", ...)

    OK <- require("Rmetrics")

    ## Return
    return(OK)
}


getDESCR <- function(package, infokind)
{
    stopifnot(is.character(package))
    unlist(lapply(package, function(pkg)
              {
                  file <- file.path(pkg, "DESCRIPTION")
                  descr <- tools:::.read_description(file)
                  tools:::.split_description(descr)[[ infokind ]]
              }), recursive = FALSE)
}

getDepends <- function(package)  names(getDESCR(package, "Depends"))

getSuggests <- function(package) names(getDESCR(package, "Suggests"))

## MM: Hmm, I think this is just what  basename()  and  dirname() do:
getPath <- function(pkgFile)
{
    ## extract path of package in order to set working directory
    path <- unlist(strsplit(pkgFile, "/"))
    if (path[length(path)] == "") path <- path[-length(path)]
    ## return
    list(package = path[length(path)],
         path = paste(path[-length(path)], collapse = "/"))
}
