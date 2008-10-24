buildRmetrics <- function(pkgs = "all", outdir = NULL, ...)
{

    stopifnot(is.character(pkgs))

    installFile <- "installRmetrics.R"
    if(!file.exists(installFile))
        stop(installFile," is not in current directory",
             "(",getwd(),")")

    message("source()ing ", installFile, " in ",
            getwd(),"... ", appendLF = FALSE)
    source(installFile)
    message("OK")

    ## extract list of Rmetrics packages
    pkgsRmetrics <- .packagesRmetrics()
    if (any(pkgs == "all")) pkgs <- pkgsRmetrics
    stopifnot(pkgs %in% pkgsRmetrics)

    message("building the packages ...", appendLF = FALSE)
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
        message("moving packages to ", outdir, "...", appendLF = FALSE)
        rename <- sapply(pkgsBuild, function(pkg)
                         file.rename(pkg, file.path(outdir, pkg)))
        if (any(!rename))
            stop("\nProblem in moving packages to outdir")
        message("OK")
    }

    message("\nTo install or check the packages Please use the order:\n",
            paste(pkgsBuild, collapse = " "))

    invisible(pkgsBuild)
}
