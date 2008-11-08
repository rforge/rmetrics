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

    return()
}

checkVersion <- function(pkgs)
{
    message("Downloading packages info from CRAN ... ", appendLF = FALSE)
    info <- available.packages(contriburl = contrib.url(getOption("repos"), type = "source"))
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



buildRmetrics <- function(pkgs = "all",
                          outdir = NULL,
                          update.version = FALSE,
                          ...)
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

    # reorder list of packages
    pkgs <- pkgsRmetrics[pkgsRmetrics %in% pkgs]

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

    ## update version number
    if (update.version) upVersion(pkgs)

    invisible(pkgs)
}
