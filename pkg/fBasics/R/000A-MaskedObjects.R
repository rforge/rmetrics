
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2004, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:
# sort
# sort.default
# log
# log.default
# var
# var.default
# data
# "rownames<-"
# "rownames<-.default"
# "colnames<-"
# "colnames<-.default"
################################################################################


.conflicts.OK = TRUE


################################################################################


sort = 
function(x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE)
{
    UseMethod("sort")
}


# ------------------------------------------------------------------------------


sort.default =
function (x, partial = NULL, na.last = NA, decreasing = FALSE, 
method = c("shell", "quick"), index.return = FALSE) 
{   # A Copy of the sort() function from R's base package
    
    if (isfact <- is.factor(x)) {
        if (index.return) 
            stop("'index.return' only for non-factors")
        lev <- levels(x)
        nlev <- nlevels(x)
        isord <- is.ordered(x)
        x <- c(x)
    } else if (!is.atomic(x)) 
        stop("'x' must be atomic")
    if (has.na <- any(ina <- is.na(x))) {
        nas <- x[ina]
        x <- x[!ina]
    }
    if (index.return && !is.na(na.last)) 
        stop("'index.return' only for 'na.last = NA'")
    if (!is.null(partial)) {
        if (index.return || decreasing || isfact || !missing(method)) 
            stop("unsupported options for partial sorting")
        if (!all(is.finite(partial))) 
            stop("non-finite 'partial'")
        y <- .Internal(psort(x, partial))
    } else {
        nms <- names(x)
        method <- if (is.numeric(x)) 
            match.arg(method)
        else "shell"
        switch(method, quick = {
            if (!is.null(nms)) {
                if (decreasing) 
                  x <- -x
                y <- .Internal(qsort(x, TRUE))
                if (decreasing) 
                  y$x <- -y$x
                names(y$x) <- nms[y$ix]
                if (!index.return) 
                  y <- y$x
            }
            else {
                if (decreasing) 
                  x <- -x
                y <- .Internal(qsort(x, index.return))
                if (decreasing) 
                  if (index.return) 
                    y$x <- -y$x
                  else y <- -y
            }
        }, shell = {
            if (index.return || !is.null(nms)) {
                o <- sort.list(x, decreasing = decreasing)
                y <- if (index.return) 
                  list(x = x[o], ix = o)
                else x[o]
            }
            else y <- .Internal(sort(x, decreasing))
        })
    }
    if (!is.na(na.last) && has.na) 
        y <- if (!na.last) 
            c(nas, y)
        else c(y, nas)
    if (isfact) 
        y <- (if (isord) 
            ordered
        else factor)(y, levels = seq(len = nlev), labels = lev)
    y
}


################################################################################


log = 
function(x, base = exp(1)) 
{
    UseMethod("log")
}


# ------------------------------------------------------------------------------


log.default =
function(x, base = exp(1))
{   # A Copy of the log() function from R's base package

    if (missing(base)) .Internal(log(x)) else .Internal(log(x, base))
}


################################################################################


var = 
function(x, y = NULL, na.rm = FALSE, use) 
{
    UseMethod("var")
}


# ------------------------------------------------------------------------------


var.default =
function(x, y = NULL, na.rm = FALSE, use) 
{
    if (missing(use)) 
        use <- if (na.rm) "complete.obs" else "all.obs"
    na.method <- 
        pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs"))
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    else stopifnot(is.atomic(x))
    if (is.data.frame(y)) 
        y <- as.matrix(y)
    else stopifnot(is.atomic(y))
    .Internal(cov(x, y, na.method, FALSE))
}


################################################################################


data = 
function(..., list = character(0), package = NULL, lib.loc = NULL,
verbose = getOption("verbose"), envir = .GlobalEnv)
{
    fileExt = function(x) sub(".*\\.", "", x)
    names = c(as.character(substitute(list(...))[-1]), list)
    ## Find the directories of the given packages and maybe the working
    ## directory.
    if (!is.null(package)) {
        if (!is.character(package))
            stop("'package' must be a character string or NULL")
        if (any(package %in% "base")) warning(
            "datasets have been moved from package 'base' to package 'datasets'")
        if (any(package %in% "stats")) warning(
            "datasets have been moved from package 'stats' to package 'datasets'")
        package[package %in% c("base", "stats")] = "datasets"
    }
    paths = .find.package(package, lib.loc, verbose = verbose)
    if (is.null(lib.loc))
        paths = c(.path.package(package, TRUE), 
            if(is.null(package)) getwd(), paths)
    paths = unique(paths[file.exists(paths)])
    ## Find the directories with a 'data' subdirectory.
    paths = paths[tools::file_test("-d", file.path(paths, "data"))]
    dataExts = tools:::.make_file_exts("data")
    if(length(names) == 0) {
        ## List all possible data sets.
        ## Build the data db.
        db = matrix(character(0), nr = 0, nc = 4)
        for(path in paths) {
            entries = NULL
            ## Use "." as the 'package name' of the working directory.
            packageName <-
                if(tools::file_test("-f", file.path(path, "DESCRIPTION")))
                    basename(path)
                else
                    "."
            ## Check for new-style 'Meta/data.rds'
            if(tools::file_test("-f", INDEX <-
                file.path(path, "Meta", "data.rds"))) {
                entries = .readRDS(INDEX)
            } else {
                ## No index: should only be true for ./data >= 2.0.0
                dataDir = file.path(path, "data")
                entries = tools::list_files_with_type(dataDir, "data")
                if(length(entries) > 0) {
                    entries <-
                        unique(tools::file_path_sans_ext(basename(entries)))
                    entries = cbind(entries, "")
                }
            }
            if(NROW(entries) > 0) {
                if(is.matrix(entries) && ncol(entries) == 2)
                    db = rbind(db, cbind(packageName, dirname(path), entries))
                else
                    warning(gettextf("data index for package '%s' is invalid and will be ignored", packageName), domain=NA, call.=FALSE)
            }
        }
        colnames(db) = c("Package", "LibPath", "Item", "Title")

        footer = if(missing(package))
            paste("Use ", sQuote(paste("data(package =",
                ".packages(all.available = TRUE))")), "\n",
                "to list the data sets in all *available* packages.", sep = "")
            else
                NULL
        y = list(title = "Data sets", header = NULL, results = db,
            footer = footer)
        class(y) = "packageIQR"
        return(y)
    }
    paths = file.path(paths, "data")
    for(name in names) {
        found = FALSE
        for(p in paths) {
            ## does this package have "Rdata" databases?
            if (tools::file_test("-f", file.path(p, "Rdata.rds"))) {
                rds = .readRDS(file.path(p, "Rdata.rds"))
                if (name %in% names(rds)) {
                    ## found it, so copy objects from database
                    found = TRUE
                    if(verbose)
                        cat("name=", name, ":\t found in Rdata.rdb\n")
                    thispkg = sub(".*/([^/]*)/data$", "\\1", p)
                    thispkg = sub("_.*$", "", thispkg) # versioned installs.
                    thispkg = paste("package:", thispkg, sep="")
                    objs = rds[[name]] # guaranteed an exact match
                    lazyLoad(file.path(p, "Rdata"), envir = envir,
                             filter = function(x) x %in% objs)
                    break
                }
            }
            ## check for zipped data dir
            if(tools::file_test("-f", file.path(p, "Rdata.zip"))) {
                if(tools::file_test("-f",fp = file.path(p, "filelist")))
                    files = file.path(p, scan(fp, what="", quiet = TRUE))
                else {
                    warning(gettextf("file 'filelist' is missing for directory '%s'", p), domain = NA)
                    next
                }
            } else {
                files = list.files(p, full = TRUE)
            }
            files = files[grep(name, files, fixed = TRUE)]
            if (length(files) > 1) {
                ## more than one candidate
                o = match(fileExt(files), dataExts, nomatch = 100)
                paths0 = dirname(files)
                paths0 = factor(paths0, levels=paths0)
                files = files[order(paths0, o)]
            }
            if (length(files) > 0) {
                ## have a plausible candidate (or more)
                for(file in files) {
                    if(verbose)
                        cat("name=", name, ":\t file= ...",
                            .Platform$file.sep, basename(file), "::\t", sep = "")
                    ext = fileExt(file)
                    ## make sure the match is really for 'name.ext'
                    if(basename(file) != paste(name, ".", ext, sep = ""))
                        found = FALSE
                    else {
                        found = TRUE
                        zfile = zip.file.extract(file, "Rdata.zip")
                        if(zfile != file) on.exit(unlink(zfile))
                        switch(ext,
                            R = , r =sys.source(zfile, chdir = TRUE,
                            envir = envir), RData = , rdata = , rda =
                            load(zfile, envir = envir), TXT = , txt = , 
                            tab = assign(name,
                            read.table(zfile, header = TRUE), envir = envir),
                            CSV = , csv = assign(name,
                            read.table(zfile, header = TRUE, sep = ";"),
                            envir = envir),
                            found = FALSE)
                    }
                    if (found) break # from files
                }
                if (verbose) cat(if(!found) "*NOT* ", "found\n")
            }
            if (found) break # from paths
        }
        if(!found)
            warning(gettextf("data set '%s' not found", name), domain = NA)
    }
    # DW added
    for (name in names) {
        # if the data set can be transformed in a timeSeriesobject 
        # then do it ... 
        tS = as.character(!is.character(try(as.timeSeries(
            eval(parse(text = paste("x = ", name)))), silent = TRUE)))  
        if (as.logical(tS)) {
            x = as.timeSeries(name)
            eval(parse(text = paste(name, "<<- x")))
        }
    }
    # DW
    invisible(names)
}


################################################################################


"rownames<-" = 
function(x, value)
{
    UseMethod("rownames<-")
}


# ------------------------------------------------------------------------------
    

"rownames<-.default" =
function(x, value)
{
    dn <- dimnames(x)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd <- length(dim(x))) < 1)
            stop("attempt to set rownames on object with no dimensions")
        dn <- vector("list", nd)
    }
    if(length(dn) < 1)
        stop("attempt to set rownames on object with no dimensions")
    if(is.null(value)) dn[1] <- list(NULL) else dn[[1]] <- value
    dimnames(x) <- dn
    x
}


################################################################################


"colnames<-" = 
function(x, value)
{
    UseMethod("colnames<-")
}


# ------------------------------------------------------------------------------
    

"colnames<-.default" =
function(x, value)
{
    dn <- dimnames(x)
    if(is.null(dn)) {
        if(is.null(value)) return(x)
        if((nd <- length(dim(x))) < 2) stop(
            "attempt to set colnames on object with less than two dimensions")
        dn <- vector("list", nd)
    }
    if(length(dn) < 2) stop(
        "attempt to set colnames on object with less than two dimensions")
    if(is.null(value)) dn[2] <- list(NULL) else dn[[2]] <- value
    dimnames(x) <- dn
    x
}


################################################################################
