################################################################################
## **Install Rmetrics packages**
##
## This script installs Rmetrics packages either from source or from
## remote server (i.e. R-Forge). It ensures that all dependent
## Rmetrics packages are installed from the same location, i.e. remote
## server. This is important to avoid compatibility problem between
## development packages and packages available on CRAN.
##
## *An Example with fSeries*
##
## _Local packages_
##
## Open an R process and set its working directory to this directory.
## Then type the following :
##
## > source("installRmetrics.R")
## > installRmetrics("fSeries")
##
## _Packages at R-Forge_
##
## > source("installRmetrics.R")
## > installRmetrics("fSeries", repos="http://R-Forge.R-project.org")
##
################################################################################

installRmetrics  <-
    function(pkgs = "Rmetrics", repos = NULL,
             CRAN = "http://stat.ethz.ch/CRAN/",
             type = "source", suggests = FALSE, ...)
{

    stopifnot(is.character(pkgs))

    # get description of packages
    if (is.null(repos)) {
        type <- "source" # install from local directory
    } else {
        address <- contrib.url(repos, type)
        try(available <- available.packages(address, method = "auto"),
            silent = TRUE)
        if (!NROW(available)) {
            type <- "source" # try to retrieve  source package
            address <- contrib.url(repos, type)
            try(available <- available.packages(address, method = "auto"),
                silent = TRUE)
        }
        if (!NROW(available))
            stop(gettextf("unable to access index for repository %s", repos),
                 call. = FALSE)
    }

    # list of Rmetrics packages
    infokind <- c("Depends", "Imports", if (suggests) "Suggests")
    pkgsRmetrics <- getDESCR("Rmetrics", infokind,
                             if (!is.null(repos)) available)
    pkgsRmetrics <- c(pkgsRmetrics, "Rmetrics")

    # test if requested package is part of Rmetrics
    if (!(pkgs %in% c(pkgsRmetrics, "Rmetrics")))
        stop(gettextf("'%s' is not part of Rmetrics",
                      deparse(substitute(pkgs))))

    pkgsDepends <- getDepends(pkgs, pkgsRmetrics, infokind,
                              if (!is.null(repos)) available)

    ## remove Rmetrics packages and duplicate entries
    ## --> only "outside dependencies"
    all <- c(pkgsDepends, pkgs)
    depends <- unique(all[!(all %in% pkgsRmetrics)])
    pkgs <- unique(all[(all %in% pkgsRmetrics)])

    ## Remove Rdonlp2 and Rsocp because they are not available at CRAN server
    depends <- depends[!(depends %in% c("Rdonlp2", "Rsocp", "Cdonlp2",
                                        "ClpSolve", "Csocp"))]

    ## disable unnecessary warning message when package is not installed
    ow <- options(warn = -1)
    ## install third party packages if not already installed
    for (i in seq_along(depends)) {
        if (!require(depends[i], character.only = TRUE, quietly = TRUE)) {
            message("\ninstalling package ", depends[i],
                    " from CRAN ", CRAN, " ...")
            install.packages(depends[i], repos = CRAN, type = type, ...)
        }
    }
    ### # Note Rdonlp2 is not part of Rmetrics !!
    ### if (!require(Rdonlp2, quietly = TRUE)) {
    ### install.packages("Rdonlp2", repos = repoRmetrics, type = "source", ...)
    ### }
    options(ow) # set default warning option

    # pkgs in good order for install
    pkgs <- pkgsRmetrics[sort(match(pkgs, pkgsRmetrics))]

    ## install Rmetrics packages
    install.packages(pkgs, repos = repos, type = type, ...)

    ## Return
    return(TRUE)
}

getDESCR <- function(package, infokind, available = NULL)
{
    stopifnot(is.character(package))
    ans <- unlist(lapply(package, function(pkg)
                     {
                         if (is.null(available)) {
                             # if available NULL try to read from
                             # local directroy
                             descr <- file.path(pkg, "DESCRIPTION")
                             descr <- tools:::.read_description(descr)
                         } else {
                             descr <- available[pkg, ]
                         }
                         tools:::.split_description(na.omit(descr))[ infokind ]
                         # na.omit important when reading files obtain from
                         # available.packages
                     }), recursive = TRUE)
    as.character(ans)
}

getDepends <- function(package, group, infokind, available = NULL)
{
    # extract recursively dependencies of a package which belongs to a
    # specific group of packages
    getDESCR <- match.fun(getDESCR)
    pkgsDepends <- NULL
    pkgsTested <- NULL
    while (length(package)) {
        pkgsDepends <-  c(pkgsDepends,
                          unlist(getDESCR(package, infokind, available)))
        pkgsTested <- c(pkgsTested, package)
        test <- pkgsDepends[pkgsDepends %in% group]
        package <- test[!(test %in% pkgsTested)]
    }
    unique(as.character(pkgsDepends))
}
