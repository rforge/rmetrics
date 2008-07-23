# this file can help to create a NAMESPACE

findGlobalsPackage <-
    function(pname)
{
    pname <- paste("package", pname, sep = ":")
    if (!pname %in% search())
        stop("package must be loaded")
    if (pname %in% loadedNamespaces())
        findGlobalsEnv(getNamespace(pname))
    else
        findGlobalsEnv(as.environment(pname))
}

findGlobalsEnv <-
function (env)
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

genNAMESPACE <-
    function(pkg,
             file = paste("NAMESPACE", pkg, sep = "."),
             RmetricsPkgs)
{

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

    ##
    all <- listFunctions(pkg, character.only = TRUE)

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
    # take care of function defined in .GlobalEnv and take the second entry of find
    S3pkg <- sapply(names(S3method), function(x) {
        ans <- sub("package:", "", find(x)[1])
        if(ans == ".GlobalEnv")
            ans <- sub("package:", "", find(x)[2])
        ans})
    S3pkg <- data.frame(pkg = S3pkg, func = names(S3pkg))
    rownames(S3pkg) <- NULL

    ##
    S4class <- getClasses(where = paste("package:", pkg, sep = ""))
    genericsList <- getGenerics(where = paste("package:", pkg, sep = ""))
    S4methods <- genericsList[!(genericsList@package %in% pkg)]
    S4generics <- genericsList[(genericsList@package %in% pkg)]
    S4names <- unique(genericsList)
    # take care of function defined in .GlobalEnv and take the second entry of find
    if (any(test <- (genericsList@package %in% ".GlobalEnv")))
        genericsList@package[test] <-
            sapply(genericsList@.Data[test], function(x)
                   sub("package:", "", find(as.character(x))[2]))
    S4pkg <- data.frame(pkg = genericsList@package, func = genericsList@.Data)

    ##
    # new functions
    new <- all[!(all %in% unique(c(S3names, S4names)))] # ???

    #####
    #### remove all hidden functions from new
    ### if (length(idx <- grep("^\\.", new)))
    ### new <- new[-idx]

    ##
    # search for all globals in order to include them in import
    globals <- findGlobalsPackage(pkg)
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
                        data.frame(pkg ="fUtilities", func ="setRmetricsOptions"),
                        data.frame(pkg ="fUtilities", func ="getRmetricsOptions")))
    if (any(is.na(imp))) {
        print(pkg)
        print(imp[is.na(imp),])
        warnings("there are functions not available")
        imp <- imp[!is.na(imp[,1]),]
    }
    # remove base, pkg and some special functions (plot and summary from urca)
    imp <- imp[!(imp$pkg %in% c("base", pkg)),]

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
}

###
# What to do now ?
##

# remove any existing namespace
# search for global variables and use global env
# generate NAMESPACE
# adapt zzz.R file with .onLoad function
# check manually NAMESPACE

## before starting install packages without namespace

RmetricsPkgs <- c("fUtilities", "fEcofin", "timeDate", "timeSeries",
                  "fImport", "fBasics", "fArma", "fGarch",
                  "fNonlinear", "fUnitRoots", "fTrading", "fMultivar",

                  "fRegression", "fExtremes", "fCopulae", "fOptions",
                  "fExoticOptions", "fAsianOptions", "fAssets",
                  "fPortfolio")

## R CMD INSTALL fEcofin timeDate timeSeries fImport fBasics fArma fGarch fNonlinear fUnitRoots fTrading fMultivar fRegression fExtremes fCopulae  fOptions fExoticOptions fAsianOptions fAssets fPortfolio

# problem with pkg urca in dependencies

for (pkg in RmetricsPkgs)
    genNAMESPACE(pkg, file = file.path("~/r", pkg, "NAMESPACE"), RmetricsPkgs)
