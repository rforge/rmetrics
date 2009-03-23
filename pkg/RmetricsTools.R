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
## > source("RmetricsTools.R")
## > install.RmetricsDev("timeDate")
##
################################################################################

pkgsRmetricsDev <- function()
    c("fUtilities",
      "fEcofin",
      "fCalendar",
      "fSeries",
      "timeDate",
      "timeSeries",
      "fImport",
      "fBasics",
      "fArma",
      "fGarch",
      "fNonlinear",
      "fUnitRoots",
      "fTrading",
      "fMultivar",
      "fRegression",
      "fExtremes",
      "fCopulae",
      "fBonds",
      "fOptions",
      "fExoticOptions",
      "fAsianOptions",
      "fAssets",
      "fPortfolio")

      ### "Rdonlp2",
      ### "Ripop",
      ### "RlpSolve",
      ### "RlpSolveAPI",
      ### "Rquadprog",
      ### "Rsocp",
      ### "fPortfolioSolver",
      ### "fPortfolioBacktest")

# ------------------------------------------------------------------------------

install.RmetricsDev  <-
    function(pkgs = pkgsRmetricsDev(), repos = NULL,
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

    pkgsRmetrics <- pkgsRmetricsDev()

    # test if requested package is part of Rmetrics
    if (!any(pkgs %in% pkgsRmetrics))
        stop(gettextf("'%s' is not part of Rmetrics",
                      deparse(substitute(pkgs))))

    infokind <- c("Depends", "Imports", if (suggests) "Suggests")
    pkgsDepends <- getDepends(pkgs, pkgsRmetrics, infokind,
                              if (!is.null(repos)) available)

    ## remove Rmetrics packages and duplicate entries
    ## --> only "outside dependencies"
    all <- c(pkgsDepends, pkgs)
    depends <- unique(all[!(all %in% pkgsRmetrics)])
    pkgs <- unique(all[(all %in% pkgsRmetrics)])

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

    options(ow) # set default warning option

    # pkgs in good order for install
    pkgs <- pkgsRmetrics[sort(match(pkgs, pkgsRmetrics))]

    ## install Rmetrics packages
    install.packages(pkgs, repos = repos, type = type, ...)

    ## Return
    invisible(TRUE)
}


# ------------------------------------------------------------------------------
# for compatibility purpose with previous function

installRmetrics <- function(...)
{
    .Deprecated("install.RmetricsDev")
    install.RmetricsDev(...)
}

# ------------------------------------------------------------------------------

getDESCR <- function(package, infokind, available = NULL)
{
    stopifnot(is.character(package))
    ans <- lapply(package, function(pkg)
              {
                  if (is.null(available)) {
                      # if available NULL try to read from
                      # local directroy
                      descr <- file.path(pkg, "DESCRIPTION")
                      descr <- tools:::.read_description(descr)
                  } else {
                      descr <- available[pkg, ]
                  }
                  lapply(tools:::.split_description(na.omit(descr))[infokind],
                         names)
                  # na.omit important when reading files obtain from
                  # available.packages
              })
    unlist(ans)
}

# ------------------------------------------------------------------------------

# FXIME : should use utils:::getDependencies
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

# ------------------------------------------------------------------------------

dependsRmetrics <-
    function(pkgs = pkgsRmetricsDev(),
             contrib =  "http://stat.ethz.ch/CRAN/src/contrib")
{
    stopifnot(is.character(pkgs))

    ## extract list of Rmetrics packages
    pkgsRmetrics <- pkgsRmetricsDev()
    stopifnot(pkgs %in% pkgsRmetrics)

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

# ------------------------------------------------------------------------------

checkBeforeCommit  <-
    function(pkgs = pkgsRmetricsDev(), lib = NULL, outdir = NULL, ...)
{
    stopifnot(is.character(pkgs))

    ## Set library and outdir paths
    if (is.null(lib)) {
        lib <- .libPaths()[1]
        message("will install the R packages into ", lib)
    }
    if (is.null(outdir)) outdir <- "../Rcheck"

    ## if outdir does not exist, create it
    if (!file.exists(outdir)) dir.create(outdir)

    ## extract list of Rmetrics packages
    pkgsRmetrics <- pkgsRmetricsDev()
    stopifnot(pkgs %in% pkgsRmetrics)

    ## search for packages which depends on the package we want to check
    listDepends <- lapply(pkgsRmetrics, getDepends,
                          group = pkgsRmetrics, "Depends")
    names(listDepends) <- pkgsRmetrics
    tocheck <- sapply(lapply(listDepends, "%in%", pkgs), any)
    pkgsToCheck <-
        if ("Rmetrics" %in% pkgs) # Rmetrics virtual package -> check all
            pkgsRmetrics
        else
            c(pkgs, names(tocheck)[tocheck])

    # pkgs in good order for install
    pkgsToCheck <- unique(pkgsRmetrics[sort(match(pkgsToCheck, pkgsRmetrics))])

    ## Run R CMD check ...
    Rbin <- file.path(R.home(), "bin", "R")
    Rcmd <- paste(Rbin, "CMD check")
    options <- paste("--library=", lib,
                     " --outdir=", outdir, sep = "")
    cmd <- paste(Rcmd, options, paste(pkgsToCheck, collapse = " "), ...)
    try(system(cmd))

    logWarning <- NULL
    logNOTE <- NULL
    ## check for ERRORs and WARNINGs
    for (i in seq_along(pkgsToCheck)) {
        pkg <- pkgsToCheck[i]
        dirCheck <- paste(pkg, ".Rcheck", sep = "")
        logFile <- file.path(outdir, dirCheck, "00check.log")
        log  <- readLines(logFile)

        ERROR <- grep("ERROR", log)
        WARNING <- grep("WARNING", log)
        NOTE <- grep("NOTE", log)

        if (length(ERROR)) {
            msg <- paste("More details in", logFile)
            stop(msg)
        }
        if (length(WARNING)) {
            for (line in WARNING) {
                logWarning <- c(logWarning,
                 "\n ####################### WARNING #######################\n",
                 paste(" In package", pkg, "\n"),
                 paste(paste(log[seq(line, line+5)], collapse = "\n "), "\n"),
                 paste("\n More details in", logFile, "\n"),
                 paste(" ####################### WARNING #######################\n"))
            }
        }
        if (length(NOTE)) {
            for (line in NOTE) {
                logNOTE <- c(logNOTE,
                 "\n #######################  NOTE  #######################\n",
                 paste(" In package", pkg, "\n"),
                 paste(paste(log[seq(line, line+8)], collapse = "\n "), "\n"),
                 paste("\n More details in", logFile, "\n"),
                 paste(" #######################  NOTE  #######################\n"))
            }
        }
    }
    ## print Notes and Warnings:
    message(logNOTE)
    message(logWarning)
    warnings()

    ## Return
    STATUS <- !(length(WARNING) || length(ERROR))
    return(STATUS)
}

# ------------------------------------------------------------------------------

# this function can help to create a NAMESPACE

## # remove any existing namespace
## # search for global variables and use global env
## # generate NAMESPACE
## # adapt zzz.R file with .onLoad function
## # check manually NAMESPACE

## ## before starting install packages without namespace

genNAMESPACE <- function(pkgs = c("timeDate", "timeSeries", "fBasics",
                         "fGarch", "fCopulae", "fAssets", "fPortfolio"), ...)
{

    stopifnot(is.character(pkgs))

    RmetricsPkgs <- pkgsRmetricsDev()

    for (pkg in pkgs) {

        user <- Sys.getenv("USER")

        # Change here your personal path to the your local packages
        pkgPath <-
            switch(user,
                   "yankee" = file.path("~/r", pkg),
                   stop("edit your path in genNAMSPACE"))

        # test if given directory exists
        if (!file.exists(pkgPath))
            stop(gettextf("package in %s does not exist", pkgPath))

        # first remove current NAMEPSACE
        nmSpace <- file.path(pkgPath , "NAMESPACE")
        suppressWarnings(file.remove(nmSpace))

        message("\n")
        message("install.packages()ing without NAMESPACE : ", pkgPath)
        install.packages(pkgPath, repos = NULL, ...)

        message("\n")
        message("generating NAMESPACE : ", nmSpace)
        t <- try(.genNAMESPACE(pkg, RmetricsPkgs, nmSpace))
        if (inherits(t, "try-error"))
            stop("could not generate NAMESPACE")

        message("\n")
        message("install.packages()ing with NAMESPACE : ", pkgPath)
        install.packages(pkgPath, repos = NULL, ...)
    }
}

# ------------------------------------------------------------------------------

.genNAMESPACE <-
    function(pkg, RmetricsPkgs,
             file = paste("NAMESPACE", pkg, sep = "."))
{

    # FIXME : why is it not possible to generate namespace of different packages ?

    findGlobalsPackage <- function(pname)
    {
        pname <- paste("package", pname, sep = ":")
        if (!pname %in% search())
            stop("package must be loaded")
        if (pname %in% loadedNamespaces())
            findGlobalsEnv(getNamespace(pname))
        else
            findGlobalsEnv(as.environment(pname))
    }
    findGlobalsEnv <- function (env)
    {
        globals <- NULL
        for (n in ls(env, all.names = TRUE)) {
            v <- get(n, envir = env)
            if (typeof(v) == "closure") {
                # message("findGlobals :", n)
                globals <- c(globals, findGlobals(v))
            }
        }
        unique(globals)
    }

    ##
    stopifnot(is.character(pkg))

    ## Make sure to unload unneeded packages
    ss <- search()
    ref <- c(".GlobalEnv", "package:stats", "package:graphics",
             "package:grDevices", "package:utils", "package:datasets",
             "package:methods", "Autoloads", "package:base")
    while (!is.na(pos <- seq_along(search())[-match(ref, search())][1]))
        detach(pos = pos)

    ##
    stopifnot(suppressPackageStartupMessages(require(codetools)))
    stopifnot(suppressPackageStartupMessages(require(pkg, character.only = TRUE)))

    ## all functions
    env <- paste("package", pkg, sep = ":")
    nm <- ls(env, all.name = TRUE)
    all <- nm[unlist(lapply(nm, function(n)
                            exists(n, where = env,  mode = "function",
                                   inherits = FALSE)))]

    ##
    # remove all unneeded functions
    no <- c(".First.lib", ".Last.lib", ".onLoad", ".onAttach", ".onUnload",
            "RmetricsOptions")
    all <- all[!(all %in% no)]

    ##
    # depends packages
    hh <- library(help=pkg, character.only = TRUE)
    descr <- hh$info[[1]]
    deps <- unlist(strsplit(descr[grep("Depends", descr)], ","))
    deps <- sub("[[:space:]]+", "", deps[-1])

    ##
    # S3
    cc <- unique(unlist(strsplit(all, "\\.")))
    cc <- cc[!(cc %in% "")]
    idx <- sapply(cc, isClass)
    S3class <- cc[idx]
    pattern <-
        paste(paste("\\.", c(S3class, "default", "data\\.frame"),
                    "$", sep = ""), collapse = "|")
    S3 <- all[grep(pattern, all)]
    if(length(idxDot <- grep("^\\.", S3))) S3 <- S3[-idxDot]
    S3method <- unique(sub(pattern, "", S3))
    ### check if really a method ...
    ow <- options("warn" = 2, "show.error.messages" = FALSE)
    test <- sapply(S3method, function(f) {
        try <- try(as.logical(length(methods(f))))
        ans <-
            if (is(try, "try-error"))
                FALSE
            else
                as.logical(try)})
    options(ow)
    if (length(test)) S3method <- S3method[test]
    S3method <- sapply(S3method, function(f) {
        if (any(f %in% c("[", "[<-"))) {
            fname <-
                all[grep(paste(f, ".", sep = ""), all, fixed = TRUE)]
            sub(paste(f, ".", sep = ""), "", fname, fixed = TRUE)
        } else {
            fname <-
                all[grep(paste("^", f, "\\.", sep = ""), all)]
            sub(paste("^", f, "\\.", sep = ""), "", fname)
        }}, simplify = FALSE, USE.NAMES = TRUE)
    S3names <- vector("character",
                      ifelse(length(S3method), sum(sapply(S3method, length)), 0))
    idx <- 0
    for (i in names(S3method))
        for (j in S3method[[i]])
            S3names[idx <- idx + 1] <- paste(i, j, sep = ".")
    # take care of function defined in .GlobalEnv
    S3pkg <- sapply(names(S3method), function(x)
                    sub("package:", "", find(x)[1]))
    S3pkg <- data.frame(pkg = S3pkg, func = names(S3pkg))
    rownames(S3pkg) <- NULL
    if (any(test <- (S3pkg[,1] %in% ".GlobalEnv"))){
        print(S3pkg)
        stop("S3pkg")
    }

    ##
    S4class <- getClasses(where = paste("package:", pkg, sep = ""))
    genericsList <- getGenerics(where = paste("package:", pkg, sep = ""))
    S4methods <- genericsList[!(genericsList@package %in% pkg)]
    S4generics <- genericsList[(genericsList@package %in% pkg)]
    S4names <- unique(genericsList)
    # take care of function defined in .GlobalEnv
    S4pkg <- data.frame(pkg = genericsList@package, func = genericsList@.Data)
    if (any(test <- (S4pkg[,1] %in% ".GlobalEnv"))){
        print(S4pkg)
        stop("S4pkg")
    }

    ##
    # new functions
    new <- all[!(all %in% unique(c(S3names, S4names)))]

    #####
    #### remove all hidden functions from new
    ### if (length(idx <- grep("^\\.", new)))
    ### new <- new[-idx]


    ## Not only check function definitions, but also S4 methods
    ## [a version of this should be part of codetools eventually] :
    findMethodGlobalEnv <- function(env)
    {
        globals <- NULL
        for (g in methods::getGenerics(where = env))
	    for (m in methods::findMethods(g, where = env)) {
		fun <- methods::getDataPart(m)
                globals <- c(globals, findGlobals(fun))
	    }
        unique(globals)
    }
    findMethodGlobalsPackage <- function(pname)
    {
        pname <- paste("package", pname, sep = ":")
        if (!pname %in% search())
            stop("package must be loaded")
        if (pname %in% loadedNamespaces())
            findGlobalsEnv(getNamespace(pname))
        else
            findGlobalsEnv(as.environment(pname))
    }

    # import full namespace of other package to stay on the safe side ...

    ##
    # search for all globals in order to include them in import
    globals <- unique(c(findGlobalsPackage(pkg), findMethodGlobalsPackage(pkg)))
    impGlobals <- sapply(globals, function(x) {
        ans <- sub("package:", "", find(x)[1])
        if (!is.na(ans) && ans == ".GlobalEnv") {
            ans <- sub("package:", "", find(x)[2])
        }
        ans})
    # remove all entries from base package
    impGlobals <- impGlobals[!(impGlobals %in% "base")]
    globalsPkg <- data.frame(pkg = impGlobals, func = names(impGlobals))
    rownames(globalsPkg) <- NULL

    ##
    # list of function to import
    imp <- unique(rbind(S3pkg, S4pkg, globalsPkg,
                        data.frame(pkg ="timeDate", func ="setRmetricsOptions"),
                        data.frame(pkg ="timeDate", func ="getRmetricsOptions")))
    if (any(is.na(imp))) {
        print(pkg)
        print(imp[is.na(imp),])
        warnings("there are functions not available")
        imp <- imp[!is.na(imp[,1]),]
    }
    # remove base, pkg and some special functions (plot and summary from urca)
    imp <- imp[!(imp$pkg %in% c("base", pkg)),]
    # put Rmetrics pkgs at the end of list
    imp <- rbind(imp[!(imp$pkg %in% RmetricsPkgs),],
                 imp[(imp$pkg %in% RmetricsPkgs),])
    # remove duplicated entries. It should use first the definition
    # from a base pacakge rather than from a Rmetrics pkgs because
    # Rmetrics at the end of imp
    imp <- imp[!duplicated(imp$func),]

    ###     for (idx in match("urca", imp$pkg))
    ###         if (imp$func[idx] %in% c("plot", "summary"))
    ###             imp <- imp[-idx,]

    imp <- tapply(imp$func, as.character(imp$pkg), function(x)
                  paste("\"", x, "\"", sep = "", collapse = ",\n           "))
    if (any(names(imp) == ".GlobalEnv"))
        stop("there are functions defined in '.GlobalEnv'")
    # check if package in imp has a NAMESPACE and make sure to keep Rmetrics pkg
    imp <- imp[((names(imp) %in% RmetricsPkgs) ||
               packageHasNamespace(names(imp), file.path(R.home(), "library")))]

    ## FIXME with timeSeries and cbind cbind.timeSeries

###     impPkg <- c(na.omit(unique(c(impGlobals, "timeDate"))))
###     impPkg <- impPkg[!(impPkg %in% pkg)]  # avoid cyclic name space dependencies
###     # check if package in imp has a NAMESPACE and make sure to keep Rmetrics pkg
###     imp <- impPkg[(impPkg %in% RmetricsPkgs) || packageHasNamespace(impPkg, file.path(R.home(), "library"))]

    ##
    # should we include C or Fortran code ?
    SRC <- file.exists(file.path(R.home(), "library", pkg, "libs"))

    ##
    # write NAMESPACE file
    op <- options("useFancyQuotes")
    options(useFancyQuotes = FALSE)
    out <- file(file, "wt")
    cat("
################################################
## import name space
################################################
\n", file = out)

        if (ln <- length(imp))
            for (i in seq(ln))
            cat("importFrom(", "\"", names(imp)[i], "\",\n           ",
                imp[[i]], ")\n", sep = "", file = out)

###     ## YC : rather than importing specific function, we now import whole
###     ## namespace of other packages to stay on the safe side
###     for (pp in imp)
###         cat("import(", "\"", pp, "\")\n", sep = "", file = out)

    ###     if (length(deps))
    ###         for (dep in deps)
    ###             if (any(dep %in% RmetricsPkgs))
    ###                 cat("import(", dQuote(dep), ")\n", sep = "", file = out)
    if (SRC) {
        cat("
################################################
## useDynLib
################################################
\n", file = out)
        cat("useDynLib(", "\"", pkg, "\"", ")\n", sep = "", file = out)
    }
    cat("
################################################
## S4 classes
################################################
\n", file = out)
    if (length(S4class) > 0) {
        cat("exportClasses(", file = out)
        cat(paste("\"", S4class, "\"", sep = "", collapse = ",\n              "),
            ")\n", file = out)
    }
    if (length(S4generics) > 0) {
        cat("export(", file = out)
        cat(paste("\"", S4generics, "\"", sep = "", collapse = ",\n        "),
            ")\n", file = out)
    }
    if (length(S4methods) > 0) {
        cat("exportMethods(", file = out)
        cat(paste("\"", S4methods, "\"", sep = "", collapse = ",\n              "),
            ")\n", file = out)
    }

    cat("
################################################
## S3 classes
################################################
\n", file = out)
    for (i in names(S3method))
    {
        for (j in S3method[[i]])
        {
            cat("S3method(", dQuote(i), ", ", dQuote(j), ")\n", sep = "",
                file = out)
        }
    }

## YC : Not needed since we are still using S3 methods of cbind and
## rbind but will need this code eventually
###     if (pkg == "timeSeries") {
###         cat("
### ################################################
### ## Special Case
### ################################################

### if (getRversion() < \"2.9.0\") {
###     S3method(\"cbind\", \"timeSeries\")
###     S3method(\"rbind\", \"timeSeries\")
### }\n", file = out)
###     }

    cat("
################################################
## functions
################################################
\n", file = out)
    if (length(new) > 0) {
        cat("export(\n    ", file = out)
        cat(paste("\"", new, "\"", sep = "", collapse = ",\n    "),
            ")\n", file = out)
    }

    ################################################
    close(out)
    options(op)

    invisible(TRUE)
}

# ------------------------------------------------------------------------------

upVersion <- function(pkgs)
{
    sapply(pkgs, function(pkg) {
        dcfFile <- file.path(pkg, "DESCRIPTION")
        dcf <- read.dcf(dcfFile)
        ## in Rmetrics first number correspond to R version and
        ## second is the number of time the package was uploaded to CRAN
        Rver <- paste(R.version[c("major", "minor")], collapse = "")
        Rver <- sub("\\.","", Rver)

        pkgVersion <- dcf[,"Version"]
        pkgOldVersion <- as.numeric(unlist(strsplit(pkgVersion, "\\.")))
        pkgNewVersion <- paste(Rver, pkgOldVersion[2]+1, sep = ".")

        dcf[,"Version"] <- pkgNewVersion
        message("Updated version number of ", pkg,
                " (", paste(pkgOldVersion, collapse = "."),
                "->", paste(pkgNewVersion, sep = "."), ")")
        write.dcf(dcf, file = dcfFile)
    })

    invisible(TRUE)
}

# ------------------------------------------------------------------------------

checkVersion <- function(pkgs = pkgsRmetricsDev())
{
    message("Downloading packages info from CRAN ... ", appendLF = FALSE)
    info <- available.packages(contriburl = contrib.url(getOption("repos"),
                               type = "source"))
    message("OK")

    for (pkg in pkgs) {
        dcfFile <- file.path(pkg, "DESCRIPTION")
        pkgVersion <- read.dcf(dcfFile, fields = "Version")

        if (info[pkg, "Version"] == pkgVersion)
            stop(pkg, " has same local version number as package on CRAN!")

        message("\n", pkg)
        message("CRAN  : ", info[pkg, "Version"])
        message("Local : ", pkgVersion)
    }
}

buildRmetrics <- function(pkgs = pkgsRmetricsDev(), outdir = NULL,
                          update.version = FALSE, ...)
{

    stopifnot(is.character(pkgs))

    ## extract list of Rmetrics packages
    pkgsRmetrics <- pkgsRmetricsDev()
    stopifnot(pkgs %in% pkgsRmetrics)

    # reorder list of packages
    pkgs <- pkgsRmetrics[pkgsRmetrics %in% pkgs]

    # update Date and revision number in DESCRIPTION file
    message("Updating Date and Revision in DCF... ", appendLF = FALSE)
    for (pkg in pkgs) {
        dcfFile <- file.path(pkg, "DESCRIPTION")
        dcf <- read.dcf(dcfFile)

        # get Date
        dcf[,"Date"] <- format(Sys.Date())

        # get svn rev number
        cmd <- paste("svn info", pkg)
        t <- try(svn <- system(cmd, intern = TRUE))
        if (inherits(t, "try-error"))
            warning("Could not update svn revision number in DESC file")
        else {
            rev <- sub("Last Changed Rev: ", "",
                       svn[grep("Last Changed Rev", svn)])
            if ("Revision" %in% colnames(dcf)) # check if "REV" already present
                dcf[,"Revision"] <- rev
            else {
                # insert REV just after Version field
                cn <- grep("Version", colnames(dcf))
                dcf <- cbind(dcf[,seq(cn),drop=FALSE],
                             Revision = rev,
                             dcf[,seq(cn+1, ncol(dcf)),drop=FALSE])
            }
        }
        # update DCF
        write.dcf(dcf, file = dcfFile)
        message("OK")

    }

    # update Changelog file
    message("Updating ChangeLog... ", appendLF = FALSE)
    for (pkg in pkgs) {
        svn2cl <- file.path("..", "share", "svn2cl.sh")
        if (file.exists(svn2cl)) {
            cmd <- paste(svn2cl, pkg,
                         "--ignore-message-starting !",
                         "-o", file.path(pkg, "ChangeLog"))
            t <- try(system(cmd))
            if (inherits(t, "try-error"))
                stop("Could not generate ChageLog file")
            message("OK")
        } else {
            message("ERROR")
            warning("Could not locate svn2cl.sh script")
        }
    }

    # build package
    message("Building the packages... ")
    build <-
        sapply(pkgs, function(pkg, ...) system(paste("R CMD build", pkg, ...)))
    if (any(build))
        stop("\nProblem in building the packages\n", build)

    dir <- dir()
    pkgsIdx <- sapply(paste(pkgs, "_", sep = ""), grep, dir)
    pkgsBuild <- dir[pkgsIdx]

    ## outdir
    if (!is.null(outdir)) {

        ## if outdir does not exist, create it
        if (!file.exists(outdir)) dir.create(outdir)

        ## move packages to outdir
        message("moving packages to ", outdir, "... ", appendLF = FALSE)
        rename <- sapply(pkgsBuild, function(pkg)
                         file.rename(pkg, file.path(outdir, pkg)))
        if (any(!rename))
            stop("\nProblem in moving packages to outdir")
        message("OK")
    }

    message("\nTo install or check the packages Please use the order:\n",
            paste(pkgsBuild, collapse = " "))

    ## update version number
    if (update.version) upVersion(pkgs)

    invisible(pkgs)
}

# ------------------------------------------------------------------------------

## checkSummaryRmetrics <-
##     function(pkgs,
##              url = "http://cran.r-project.org/web/checks/check_summary.html")
## {

##     message(gettextf("read information at %s", url))
##     t <- try(html <- readLines(url))
##     if (inherits(t, "try-error"))
##         stop(gettextf("Could not read %s", url))



##     ## check <- html[grep(pkgs, html)]
##     summary <- matrix(ncol = 11)
##     colnames(summary) <- c("Package",
##                            "Version",
##                            "R-devel Linux ix86",
##                            "R-devel Linux ix86_64 (GCC)",
##                            "R-devel Linux ix86_64 (SUN)",
##                            "R-devel Windows ix86",
##                            "R-patch Linux ix86",
##                            "R-patch Linux ix64",
##                            "R-release Linux ix86",
##                            "R-release Mac OS X ix86",
##                            "R-release Windows ix86")
##     count <- 1

##     for (pkg in pkgs) {
##         lines <- html[grep(pkg, html)]
##         for (line in lines) {

##             # split by tags <td></td>
##             str <- strsplit(line, "<td>|</td>", fixed = FALSE)[[1]]

##             # remove empty lines
##             str <- str[-grep("^[[:space:]]*$", str)]
##             str <- gsub("^[[:space:]]*", "", str)
##             str <- gsub("[[:space:]]*$", "", str)

##             # remove unneded fields
##             str <- str[-c(1,11,12)]


##             new <- strsplit(line, "<a href=\"|</a>")[[1]]
##             version <- gsub("[[:space:]]*</td>[[:space:]]*<td>[[:space:]]*|[[:space:]]*</td>[[:space:]]*<td>[[:space:]]*", "", new[3])

##             # remove unneeded lines
##             info <- new[-grep("<td>", new)]


##             strsplit(line, "<font.*>|</font>")



##             summary[count, 1] <- pkg
##             summary[count, 2] <-

##             count <- count + 1
##         }
##     }


## }
