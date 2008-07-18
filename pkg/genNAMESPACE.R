# this file can help to create a NAMESPACE

# remove any existing namespace
# search for global variables and use global env
# generate NAMESPACE
# adapt zzz.R file with .onLoad function
# check manually NAMESPACE

## fUtilities, fEcofin,  timeDate, timeSeries,
## fImport, fBasics, fArma, fGarch, fNonlinear, fUnitRoots, fTrading,
## fMultivar, fRegression, fExtremes, fCopulae, fOptions,
## fExoticOptions, fAsianOptions, fAssets, fPortfolio

genNAMESPACE <-
    function(pkg,
            file = paste("NAMESPACE", pkg, sep = ".")
            )
{
    stopifnot(require(pkg, character.only = TRUE))

    all <- listFunctions(pkg, character.only = TRUE)

    # remove all unneeded functions
    no <- c(".First.lib", ".Last.lib", ".onLoad", ".onAttach", ".onUnload",
            "RmetricsOptions")
    all <- all[!(all %in% no)]

    # depends packages to be included in NAMESPACE
    hh <- library(help=pkg, character.only = TRUE)
    descr <- hh$info[[1]]
    deps <- unlist(strsplit(descr[grep("Depends", descr)], ","))
    deps <- sub("[[:space:]]+", "", deps[-1])

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
    ow <- options("warn" = 2)
    test <- sapply(S3method, function(f)
               {
                   try <- try(as.logical(length(methods(f))))
                   ans <-
                       if (is(try, "try-error"))
                           FALSE
                       else
                           as.logical(try)
                   })
    options(ow)
    if (length(test)) S3method <- S3method[test]

    S3method <- sapply(S3method, function(f)
                   {
                       if (any(f %in% c("[", "[<-"))) {
                           fname <-
                               all[grep(paste(f, ".", sep = ""), all, fixed = TRUE)]
                           sub(paste(f, ".", sep = ""), "", fname, fixed = TRUE)
                       } else {
                           fname <-
                               all[grep(paste("^", f, "\\.", sep = ""), all)]
                           sub(paste("^", f, "\\.", sep = ""), "", fname)
                       }
                   }, simplify = FALSE, USE.NAMES = TRUE)

    S3names <- vector("character", ifelse(length(S3method), sum(sapply(S3method, length)), 0))
    idx <- 0
    for (i in names(S3method))
        for (j in S3method[[i]])
            S3names[idx <- idx + 1] <- paste(i, j, sep = ".")
    S3pkg <- sub("package:", "", unlist(sapply(S3names, find)))

    ### SS <- sapply(S3method, function(mm) {
    ### am <- methods(mm)
    ### idx <- (am %in% all)
    ### names <- sub("package:", "", as.matrix(attr(am, "info"))[idx, "from"])

    ### ans <- am[idx]
    ### names(ans) <- names
    ### ans
    ### })

    # S4
    # S4 <- capture.output(showMethods(classes = S4class))
    ## S4method <- sapply(sapply(S4[grep("Function", S4)], strsplit, " "),"[",2)
    ## if(test <- any(S4method %in% S3method)) # check if not already in S3method
    ##     stop(gettext("S4 method '%s' already reported as an S3 method",
    ##                  paste(S4method[test])))
    ## S4pkg <- sapply(sapply(S4[grep("Function", S4)], strsplit, " |)"),"[",4)
    ## S4generic <-  all[sapply(all, isGeneric)]
    ## if(test <- any(S4generic %in% S4method))
    ##     stop(gettext("S4 generic '%s' already reported as an S4 method",
    ##                  paste(S4generic[test])))
    ## # S4table <- data.frame("method" = S4method, "package" = S4pkg)
    ## S4names <- as.character(S4method)

    S4class <- getClasses(where = paste("package:", pkg, sep = ""))
    genericsList <- getGenerics(where = paste("package:", pkg, sep = ""))
    S4methods <- genericsList[!(genericsList@package %in% pkg)]
    S4generics <- genericsList[(genericsList@package %in% pkg)]
    S4pkg <- unique(genericsList@package)
    S4names <- unique(genericsList)

    # new functions
    new <- all[!(all %in% unique(c(S3names, S4names)))] # ???

    # should we include C or Fortran code ?
    SRC <- file.exists(file.path(R.home(), pkg, "src"))

    op <- options("useFancyQuotes")
    options(useFancyQuotes = FALSE)

    out <- file(file, "wt")
    cat("
################################################
## import name space
################################################
\n", file = file)
    if (length(ns <- unlist(c(S4pkg, S3pkg, deps))))
        for (imp in sort(unique(ns)))
            if (!any(imp == c(".GlobalEnv", "base", pkg)))
                cat("import(", dQuote(imp), ")\n", sep = "", file = out)
    if (SRC) {
        cat("
################################################
## useDynLib
################################################
\n", file = file)
        cat("useDynLib(",pkg,")\n", sep = "", file = out)
    }
    cat("
################################################
## S4 classes
################################################
\n", file = out)
    ## if (length(S4class))
    ##     cat("
    ## #  S4 classes
    ## exportClass(\n   ", paste(dQuote(S4class), collapse = ",\n    "), "\n)\n",
    ##         file = out)
    ## if (length(S4generic))
    ##     cat("
    ##   S4 generic functions
    ## export(\n   ", paste(dQuote(S4generic), collapse = ",\n    "),  "\n)\n",
    ##         file = file, append = TRUE)
    ## if (length(S4method))
    ##     cat("
    ## # S4 methods
    ## exportMethods(\n   ", paste(dQuote(S4method), collapse = ",\n    "),  "\n)\n",
    ##         file = file, append = TRUE)

    if (length(S4class) > 0) {
        cat("exportClasses(\n    ", file = out)
        cat(paste("\"", S4class, "\"", sep = "", collapse = ",\n     "),
            "\n)\n", file = out)
    }
    if (length(S4generics) > 0) {
        cat("export(\n    ", file = out)
        cat(paste("\"", S4generics, "\"", sep = "", collapse = ",\n    "),
            "\n)\n", file = out)
    }
    if (length(S4methods) > 0) {
        cat("exportMethods(\n    ", file = out)
        cat(paste("\"", S4methods, "\"", sep = "", collapse = ",\n    "),
            "\n)\n", file = out)
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
            "\n)\n", file = out)
    }

    ################################################
    close(out)
    options(op)
}
