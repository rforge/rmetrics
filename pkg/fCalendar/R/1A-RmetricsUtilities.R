
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
# You should have received A copy of the GNU Library General 
# Public License along with this library; if not, write to the 
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
# MA  02111-1307  USA

# Copyrights (C)
# for this R-port: 
#   1999 - 2006, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                     DESCRIPTION:
#  as.matrix.ts                  Converts univariate ts to 1-column matrix
#  as.matrix.mts                 Converts multivariate ts to matrix
# FUNCTION:                     DESCRIPTION:
#  .description                  Sets default description string
#  .unirootNA                    Computes zero without error exit    
# FUNCTION:                     DESCRIPTION:
#  modify                        Modifies a 'timeSeries' object
#  modify.default                Default Method
#  atoms                         Extracts atoms from 'timeSeries' object
#  atoms.default                 Default Method
# FUNCTION:                     DESCRIPTION:
#  .datax                        Loads timeSeries objects from demo files
# FUNCTION/VALUE:               DESCRIPTION: 
#  currentYear                   Sets date of the current year
#  .currentYear                  Sets date of the current year
#  myUnits                       Sets date units
# FUNCTION:                     DESCRIPTION [REQUIRES DATE]:
#  .fjulian                      Transform formatted dates to julian day numbers
#  .julian                       Implements SPlus like 'julian'
# FUNCTION:                     DESCRIPTION
#  .isISO8601                    Checks if the date/time is ISO8601 formatted
# FUNCTION:                     DESCRIPTION:
#  .isPOSIX                      Checks for an object of class POSIX
# GENERAL UTILITIES:            DESCRIPTION:
#  .by2seconds
#  .print                        Internal print method
#  .plot                         Internal plot method
#  .summary                      Internal summary method
#  .predict                      Internal predict method
################################################################################


.conflicts.OK = TRUE


################################################################################


as.POSIXlt = 
function(x, tz = "")
{
    # FUNCTION:
    
    # Return Value:
    UseMethod("as.POSIXlt")
}


# ------------------------------------------------------------------------------


as.POSIXlt.default =
function (x, tz = "") 
{   
    # FUNCTION:
    
    # As Posix:
    fromchar <- function(x) {
        xx <- x[1]
        if (is.na(xx)) {
            j <- 1
            while (is.na(xx) && (j <- j + 1) <= length(x)) xx <- x[j]
            if (is.na(xx)) 
                f <- "%Y-%m-%d"
        }
        if (is.na(xx) || !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%S")) || 
            !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%S")) || 
            !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M")) || !is.na(strptime(xx, 
            f <- "%Y/%m/%d %H:%M")) || !is.na(strptime(xx, f <- "%Y-%m-%d")) || 
            !is.na(strptime(xx, f <- "%Y/%m/%d"))) {
            res <- strptime(x, f)
            if (nchar(tz)) 
                attr(res, "tzone") <- tz
            return(res)
        }
        stop("character string is not in a standard unambiguous format")
    }
    if (inherits(x, "POSIXlt")) 
        return(x)
    if (inherits(x, "Date")) 
        return(.Internal(Date2POSIXlt(x)))
    tzone <- attr(x, "tzone")
    if (inherits(x, "date") || inherits(x, "dates")) 
        x <- as.POSIXct(x)
    if (is.character(x)) 
        return(fromchar(unclass(x)))
    if (is.factor(x)) 
        return(fromchar(as.character(x)))
    if (is.logical(x) && all(is.na(x))) 
        x <- as.POSIXct.default(x)
    if (!inherits(x, "POSIXct")) 
        stop(gettextf("do not know how to convert '%s' to class \"POSIXlt\"", 
            deparse(substitute(x))))
    if (missing(tz) && !is.null(tzone)) 
        tz <- tzone[1]
    .Internal(as.POSIXlt(x, tz))
}


# ------------------------------------------------------------------------------


as.matrix.ts = 
function(x) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Transform: 
    ans = as.matrix.default(unclass(x)) 
    attr(ans, "tsp")<-NULL
    rownames(ans)<-NULL
    colnames(ans)<-NULL
    
    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


as.matrix.mts = 
function(x) 
{   # A function implemented by Diethelm Wuertz
  
    # FUNCTION:
    
    # Transform: 
    ans = as.matrix.default(unclass(x)) 
    attr(ans, "tsp")<-NULL
    rownames(ans)<-NULL
    colnames(ans)<-NULL
    
    # Return Value:
    ans
}


################################################################################


.description =
function()
{   # A function implemented by Diethelm Wuertz

    # Descriptions:
    #   Sets default description string:
   
    # FUNCTION:
    
    # Description String:
    ans = paste(as.character(date()), "by user:", Sys.getenv("USERNAME"))
    
    # Return Value:
    ans
}


################################################################################


.unirootNA = 
function(f, interval, lower = min(interval), upper = max(interval), 
tol = .Machine$double.eps^0.25, ...) 
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Searches the interval from lower to upper for a 
    #   root (i.e., zero) of the function f with respect 
    #   to its first argument. 
    
    # Arguments:
    #   see 'uniroot'
    
    # Value:
    #   Returns the x value of f where the root is located. If
    #   now root exists NA will be returned. In the last case
    #   the function doesn't terminate with an error like in the
    #   case of the standard function uniroot.

    # Details:
    #   R:
    #   uniroot(f, interval, lower = min(interval), upper = max(interval),
    #       tol = .Machine$double.eps^0.25, 
    #       maxiter = 1000, ...)
    #   uniroot(f, interval, lower = min(interval), upper = max(interval), 
    #       tol = .Machine$double.eps^.25, 
    #       keep.xy = F, f.lower = NA,  f.upper = NA, ...) 

    # Example:
    #   .unirootNA(sin, c(1, 2)); .unirootNA(sin, c(-1, 1))

    # FUNCTION:
    
    # There is no Root:  
    if (is.null(args(f))) {  
        if (f(lower) * f(upper) >=0) return(NA)  
    } else {
        if (f(lower, ...) * f(upper, ...) >= 0) return(NA)
    } 
      
    # There is a Root:  
    ans = uniroot(f = f, interval = interval, lower = lower, 
        upper = upper, tol = tol, ...)
    
    # Return Value:
    ans$root
}  


################################################################################

################################################################################
#  modify                    Modifies a 'timeSeries' object
#  modify.default            Default Method
#  atoms                     Extracts atoms from 'timeSeries' object
#  atoms.default             Default Method


modify =
function(x, method, units) 
{   # A function implemented by Diethelm WUertz

    # FUNCTION:
    
    # Return Value:
    UseMethod("modify") 
}


# ------------------------------------------------------------------------------


modify.default =
function(x, method = c("sort", "round", "trunc"), units = NULL )
{   # A function implemented by Diethelm WUertz
 
    # FUNCTION:
    
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
{   # A function implemented by Diethelm WUertz
   
    # FUNCTION:
    
    # Return Value:
    UseMethod("atoms")
}


# ------------------------------------------------------------------------------


atoms.default = 
function(x, ...) 
{   # A function implemented by Diethelm WUertz
 
    # FUNCTION:
    
    # Return Value:
    invisible(x)
}


################################################################################
#  .datax                    Loads timeSeries objects from demo files


.datax = 
function(..., list = character(0), package = NULL, lib.loc = NULL,
verbose = getOption("verbose"), envir = .GlobalEnv)
{   # An extended copy of the var() function from R's base package

    # FUNCTION:
    
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
#  currentYear               Sets date of the current year
#  .currentYear              Sets date of the current year
#  myUnits                   Sets date units


.currentYear = 
function()    
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
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


currentYear = .currentYear() 


# ------------------------------------------------------------------------------


myUnits = "days"


################################################################################
#  .fjulian            Transform formatted dates to julian day numbers
#  .julian             Implements SPlus like 'julian'


.fjulian = 
function(fdates, origin = 19600101, order = 'mdy', cc = NULL, swap = 20)
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Transforms formatted dates (fdates) from several formats 
    #   as 8/11/73 11Aug1973, ... into ISO-8601 Gregorian dates
    #   ... makes use of C-Program char_date.c implemented by 
    #   Terry Therneau
    
    # Notes:
    #   cc - Century, becoming obsolete with the introduction of swap.
    
    # Example:
    #   fdates = c("8/11/73", "08-11-73", "August 11 1973", "Aug11/73")
    #   .fjulian(fdates) 
    #   fdates = c("11/8/73", "11-08-73", "11 August 1973", "11Aug73")
    #   .fjulian(fdates, order = 'dmy')
    
    # Note:
    #   Requires R-package "date"
  
    # FUNCTION:
    
    # Requires
    # require(date)
    
    # Formats:
    order.vec = switch(order,
        'ymd'= c(1,2,3),
        'ydm'= c(1,3,2),
        'mdy'= c(2,3,1),
        'myd'= c(2,1,3),
        'dym'= c(3,1,2),
        'dmy'= c(3,2,1),
        stop("Invalid value for 'order' option"), 
        PACKAGE = "date")
    nn = length(fdates)
    temp = .C("char_date", 
        as.integer(nn),
        as.integer(order.vec),
        as.character(fdates),
        month = integer(nn),
        day = integer(nn),
        year = integer(nn),
        PACKAGE = "date")
    month = temp[[4]]
    day = temp[[5]]
    year = temp[[6]]
    yy = year - 100 * floor (year/100)
    
    # Swap:
    cc = 19 + trunc(sign(swap-yy)+1)/2
    year = cc*100 + yy
    
    # Origin:
    cc0 = origin %/% 1000000
    yymmdd0 = origin - cc0*1000000
    yy0 = yymmdd0 %/% 10000
    mm0 = yymmdd0 %/% 100 - yy0*100
    dd0 = yymmdd0 - yy0*10000 - mm0*100

    # Result:
    ans = .julian(month, day, year, origin = c(mm0, dd0, cc0*100+yy0))
    
    # Return Value:
    ans
}


# ******************************************************************************


.julian =
function(m, d, y, origin = c(month = 1, day = 1, year = 1960))
{   # A function implemented by Diethelm Wuertz
    
    # Description:
    #   This function is a synonyme for Splus' "julian()" with the
    #   same list of arguments.
    
    # Note:
    #   SPlus like function.
   
    # FUNCTION:
    
    # Selection:
    .R = TRUE
    .S = FALSE
    
    # Implementation under R:
    if(.R) {    
        only.origin = all(missing(m), missing(d), missing(y))
        if(only.origin) m = d = y = NULL    # return days since origin
        nms = names(d)
        max.len = max(length(m), length(d), length(y))  
        # prepend new origin value and rep out to common max. length:
        m = c(origin[1], rep(m, length = max.len))
        d = c(origin[2], rep(d, length = max.len))
        y = c(origin[3], rep(y, length = max.len))  
        # code from julian date in the S book (p.269)
        y = y + ifelse(m > 2, 0, -1)
        m = m + ifelse(m > 2, -3, 9)
        c = y %/% 100
        ya = y - 100 * c
        out = (146097 * c) %/% 4 + (1461 * ya) %/% 4 + 
            (153 * m + 2) %/% 5 + d + 1721119   
        # now subtract the new origin from all dates
        if(!only.origin) {
            if(all(origin == 0)) out = out[-1] else out = out[-1] - out[1] }    
        names(out) = nms
        result = out }
    
    # Synonyme for S:
    if(.S) {
        result = julian(m = m, d = d, y = y, origin. = origin)}

    # Return Value:
    result
}


################################################################################
# FUNCTION:              DESCRIPTION
#  .isISO8601             Checks if the date/time is ISO8601 formatted


.isISO8601 =
function(x)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Checks if the date/time string is ISO8601 formatted
    
    # Example:
    #   isISO8601(c("2007-01-01", "2007-12-31" ))
    #   isISO8601(c("2007-01-01", "2007-12-31" ))[[1]]
    #   isISO8601("2007-Jan-01")[[1]]
    #   isISO8601("2007-01-01 15:00:000")[[1]]
    
    # FUNCTION:
    
    # Check:
    stopifnot(class(x) == "character")
    
    # Test: 
    options(warn = -1)
    format = .whichFormat(x)
    ans = FALSE
    if (format == "%Y-%m-%d %H:%M:%S") ans = TRUE
    if (format == "%Y-%m-%d") ans = TRUE
    if (format == "%Y%m%d%H%M%S") ans = TRUE
    if (format == "%Y%m%d") ans = TRUE
    attr(ans, "control")<- format
    
    # Return Value:
    ans
}


################################################################################
# FUNCTION ADDON:      DESCRIPTION:
#  .isPOSIX             Checks for an object of class POSIX


.isPOSIX =
function(x)
{   # A function written by Diethelm Wuertz

    # Description:
    #   Checks for an object of class POSIX

    # FUNCTION:
    
    # Check:
    ans = inherits(x, "POSIXt")
    
    # Return Value:
    ans
}


################################################################################
# GENERAL UTILITIES:            DESCRIPTION:
#  .print                        Internal print method
#  .plot                         Internal plot method
#  .summary                      Internal summary method


.by2seconds =
function(by = "1 h")
{   # A function written by Diethelm Wuertz

    # Description:
    #   Convert 'by' string into numeric value of seconds.
    
    # FUNCTION:
    
    # Convert:
    by = strsplit(by, " ")[[1]]
    byTime = as.integer(by[1])
    byUnits = substr(by[2], 1, 1)
    timeUnits = c(1, 60, 3600)
    names(timeUnits) = c("s", "m", "h")
    bySeconds = byTime * timeUnits[byUnits]
    names(bySeconds) = "secs"
    
    # Return Value:
    bySeconds
}


# ------------------------------------------------------------------------------


.print =
function(x, ...)
{
    UseMethod(".print")
}


# ------------------------------------------------------------------------------


.plot =
function(x, ...)
{
    UseMethod(".plot")
}


# ------------------------------------------------------------------------------


.summary =
function(object, ...)
{
    UseMethod(".summary")
}


# ------------------------------------------------------------------------------


.predict =
function(object, ...)
{
    UseMethod(".predict")
}


################################################################################

