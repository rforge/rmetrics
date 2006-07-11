
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
# MA 02111-1307 USA

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
# FUNCTION:                 DESCRIPTION:
#  datax                     Loads timeSeries objects from demo files
# FUNCTION:                 DESCRIPTION:
#  myFinCenter               Sets my financial center
#  currentYear               Sets date of the current year
#  myUnits                   Sets date units
#  xmpCalendar               Sets prompt
#  xmpfCalendar              Popups the example menu
#  .read.fCalendar.00Index
# FUNCTION:                 DESCRIPTION:
#  modify                    Modify a 'timeSeries' object
#  modify.default            S3 Default Method
#  atoms                     Extract atoms from 'timeSeries' object
#  atoms.default             S3 Default Method
################################################################################


datax = 
function(..., list = character(0), package = NULL, lib.loc = NULL,
verbose = getOption("verbose"), envir = .GlobalEnv)
{   # An extended copy of the var() function from R's base package

    # data:
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
    
    # REQUIRES FCALENDAR !! -> move it to fCalendar
    # DW added:
    for (name in names) {
        # If the data set can be transformed in a timeSeriesobject 
        # then do it ... 
        z = eval(parse(text = paste("x = ", name)))
        # print(c(class = class(z)))
        if (class(z) == "data.frame") {
            tS = as.character(!is.character(try(as.timeSeries(
                eval(parse(text = paste("x = ", name)))), silent = TRUE)))  
            # print(c(tS = as.logical(tS)))
            if (as.logical(tS)) {
                z = as.timeSeries(name)
                eval(parse(text = paste(name, "<<- z")))
            }
        }
    }
    # DW
    
    invisible(names)
}
  

################################################################################  
# Default Parameters:
 
   
.currentYear = 
function()    
{
    # Check Time Zone:
    TZ = Sys.getenv("TZ")  
    if (TZ[[1]] != "GMT") {
        Sys.putenv(TZ = "GMT")
        TZ.RESET = TRUE
    } else {
        TZ.RESET = FALSE
    }
    
    # Current Year:
    if (class(version) != "Sversion") {
        currentYear = as.POSIXlt(Sys.time())$year + 1900
    } else { 
        currentDate = timeDate(date(), in.format="%w %m %d %H:%M:%S %Z %Y")
        currentYear = as.integer(attr(years(currentDate), "levels"))
    } 
    
    # Return Value:
    if (TZ.RESET) Sys.putenv(TZ = TZ)
    currentYear 
}


# ------------------------------------------------------------------------------

    
myFinCenter = "Zurich"


# ------------------------------------------------------------------------------


myUnits = "days"


currentYear = .currentYear()  


# ------------------------------------------------------------------------------
    
    
# Set a timezone if none found in environment variables or options()
# ... as suggested by Dirk Eddelbuettel, thanks Dirk.

##  if (class(version) != "Sversion") {
##      if (Sys.getenv("TZ") == "") {
##          if (is.null(getOption("TZ"))) {
##              cat("No timezone information found, using default of GMT\n")
##              Sys.putenv("TZ" = "GMT") 
##          } else {
##              cat("No timezone information found, applying option() value of",
##                  getOption("TZ"), "\n")
##              Sys.putenv("TZ" = getOption("TZ")) 
##          } 
##      }
##  }


# ------------------------------------------------------------------------------


xmpCalendar = 
function(prompt = "") 
{   # A function implemented by Diethelm WUertz

    # Description:
    #   Sets prompt
    
    # FUNCTION:
    
    # Return Value:
    invisible(prompt)
}

    
# ------------------------------------------------------------------------------


xmpfCalendar = 
function() 
{   # A function implemented by Diethelm WUertz

    # Description:
    #   Popups the example menu
    
    # FUNCTION:
    
    # Popup:
    path = paste(.Library,"/fCalendar", sep = "") 
    entries = .read.fCalendar.00Index (file.path(path, "demoIndex"))    
    example = select.list(entries[,1])
    selected = 0
    for (i in 1:length(entries[,1])) {
        if (example == entries[i,1]) selected = i
    }
    if (example == "") {
        cat("\nNo demo selected\n")
    } else {
        cat("\nLibrary: ", "fCalendar", "\nExample: ", 
            entries[selected, 1], "\nTitle:   ", entries[selected, 2], "\n")
        source(paste(path, "/demo/", example, ".R", sep = ""))
    }
    if (TRUE) {
        cat("\n") 
    }
    
    # Return Value:
    invisible()
}


# ------------------------------------------------------------------------------


.read.fCalendar.00Index = 
function (file) 
{
    if (is.character(file)) {
        if (file == "") {
            file <- stdin()
        } else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    }
    if (!inherits(file, "connection")) 
        stop(paste("argument", 
            sQuote("file"), "must be a character string or connection"))
    y <- matrix("", nr = 0, nc = 2)
    x <- paste(readLines(file), collapse = "\n")
    for (chunk in unlist(strsplit(x, "\n[       \n]*\n"))) {
        entries <- try({
            if (regexpr("(   |  )", chunk) == -1) 
                NULL
            else {
                chunk <- gsub("\n[      ]+", "  ", chunk)
                x <- strsplit(unlist(strsplit(chunk, "\n")), "[    ]")
                cbind(unlist(lapply(x, "[[", 1)), unlist(lapply(x, 
                    function(t) {
                        paste(t[-c(1, which(nchar(t) == 0))], collapse = " ")
                    })))
            }
        })
        if (!inherits(entries, "try-error") && NCOL(entries) == 2) 
            y <- rbind(y, entries)
    }
    colnames(y) <- c("Item", "Description")
    y
}


# ******************************************************************************
# R - Modifications and Problems:


modify =
function(x, method, units) 
{
    UseMethod("modify") 
}


# ------------------------------------------------------------------------------


modify.default =
function(x, method = c("sort", "round", "trunc"), units = NULL )
{   
    # Modify:
    ans = NA
    if (method[1] == "sort") return(sort(x))
    if (method[1] == "round") return(round(x))
    if (method[1] == "trunc") return(trunc(x))
    
    # Return Value:
    ans
}   


# ------------------------------------------------------------------------------


atoms = 
function(x, ...) 
{
    UseMethod("atoms")
}


# ------------------------------------------------------------------------------


atoms.default = 
function(x, ...) 
{
    invisible(x)
}


################################################################################

 