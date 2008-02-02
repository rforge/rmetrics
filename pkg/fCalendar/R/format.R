
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
#   1999 - 2008, Diethelm Wuertz, Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:                 GENERATION OF TIMEDATE OBJECTS:
#  .whichFormat              Returns format string called by 'timeDate'
#  .midnightStandard         Corrects midnight standard called by 'timeDate'
#  .formatFinCenter          Internal called by timeDate
################################################################################


.whichFormat =
function(charvec, silent = FALSE)
{   # A function implemented by Diethelm Wuertz

    # Charvec String:
    charvec = as.character(charvec)

    # Specifications:
    NCHAR = mean(nchar(charvec))
    NCHAR = nchar(charvec[1])
    SUBSTR = (substring(charvec[1], 5, 5) == "-")

    # American Format:
    if (regexpr("/....", charvec[1])[[1]] > 0) return("%m/%d/%Y")
    if (regexpr("-...-....", charvec[1])[[1]] > 0) return("%d-%b-%Y")

    # Human readable ISO:
    if (NCHAR ==  4 & !SUBSTR) return("%Y")
    if (NCHAR ==  7 &  SUBSTR) return("%Y-%m")
    if (NCHAR == 10 &  SUBSTR) return("%Y-%m-%d")
    if (NCHAR == 13 &  SUBSTR) return("%Y-%m-%d %H")
    if (NCHAR == 16 &  SUBSTR) return("%Y-%m-%d %H:%M")
    if (NCHAR == 19 &  SUBSTR) return("%Y-%m-%d %H:%M:%S")

    # Short ISO:
    if (NCHAR ==  6 & !SUBSTR) return("%Y%m")
    if (NCHAR ==  8 & !SUBSTR) return("%Y%m%d")
    if (NCHAR == 10 & !SUBSTR) return("%Y%m%d%H")
    if (NCHAR == 12 & !SUBSTR) return("%Y%m%d%H%M")
    if (NCHAR == 14 & !SUBSTR) return("%Y%m%d%H%M%S")

    # Otherwise:
    if (!silent)
    warning("Could not determine time(date) format")
    
    # Return Value:
    "unknown"
}


# ------------------------------------------------------------------------------


.midnightStandard <- 
function(charvec, format)
{   # A function written by Diethelm Wuertz   
    # and entirely rewritten by Martin Maechler
    
    # Description:
    
    # FUNCTION:
    
    ## Midnight Standard & conversion to isoFormat:

    ## Motivation: strptime() {et al}  cannot deal with "24:00:00"
    ##         In that case, subtract 1 seconds convert and re-add it

    paste0 <- function(...) paste(..., sep = '')
    
    # Missing Format:
    if (missing(format)) format = .whichFormat(charvec)
    
    # Format:
    rng.nch <- range(nchar(charvec[!is.na(charvec)]))
    if(rng.nch[1] != rng.nch[2])
    stop("'charvec' has non-NA entries of different number of characters")
    nch <- rng.nch[1]

    n <- length(charvec)
    s <- rep(0, n)

    ## Do two common formats *fast* (for large n), and then use 
    ## flexible approach:
    
    # ISO-8601 Midnight Standard:
    if (length(grep("%H:%M:%S", format, fixed = TRUE)) == 1) {
        if(length(ii <- grep("24:00:00", charvec, fixed=TRUE)) > 0) {
            s[ii] <- 1
            charvec[ii] <- gsub("24:00:00", "23:59:59", charvec[ii], fixed=TRUE)
        }
    } else if (length(grep("%H%M%S$", format)) == 1) {
        ## format *ends* in  %H%M%S, i.e. last 6 chars are time
        ch.time <- substr(charvec, nch-6+1, nch)
        if(length(ii <- grep("240000$", ch.time)) > 0) {
            s[ii] <- 1
            charvec[ii] <- paste(substr(charvec[ii], 1, nch-6),
                gsub("240000$", "235959", ch.time[ii]), sep = "")
        }
    } else {
        ## Very general approach, to work for any valid format:
        forms <- c("%Y", "%m", "%d",  "%H","%M","%S")
        nums  <- c("2003","01","31",  "23","59","58") # pairwise different
        fDate <- format
        for(i in seq_along(forms)) {
            ## make sure, we don't have nums[i] already :
            if(length(grep(nums[i], fDate, fixed=TRUE)))
            fDate <- gsub(nums[i], paste(rep("x", nchar(nums[i])), collapse=""),
                      fDate, fixed=TRUE)
            fDate <- sub(forms[i], nums[i], fDate, fixed=TRUE)
        }
        ## in the ISO case, now have  fDate == "2001-01-31 23:59:58"
        names(nums) <- forms
        ## at which character positions in charvec do I need to look for %H, ... :
        iHMS <- sapply(nums[c("%H","%M","%S")], regexpr, text=fDate, fixed=TRUE)
        if(iHMS["%H"] >= 1) { 
            ## have "%H" -- otherwise, nothing to do!
            has.S <- iHMS["%S"] >= 1
            has.M <- iHMS["%M"] >= 1
            if(has.S && !has.M) stop("invalid format: has '%S' but no '%M'")
            ## 3 remaining cases:  (H,M,S), (H,M), (H)
            m. <- 1 + has.M + has.S # in {1,2,3}
            HMStab <- matrix(unlist(lapply(iHMS[1:m.],
                function(ic) substr(charvec, start=ic, stop=ic+1))), n, m.)
            twenty4 <- paste0("24", if(has.M)"00", if(has.S)"00")
            isMidN <- twenty4 == apply(HMStab, 1, paste, collapse='')
            if(any(isMidN)) { 
                ## need midnight correction
                s[isMidN] <- 1
                ## now *need* seconds, so we can subtract and add 1 sec :
                if(!has.S) {
                    if(!has.M) {
                    iHMS["%M"] <- nchar(fDate) + 1
                    format <-  paste0(format,  "%M")
                    fDate  <-  paste0(fDate,   "00")
                    charvec <- paste0(charvec, "00")
                    }
                    iHMS["%S"] <- nchar(fDate) + 1
                    format <-  paste0(format,  "%S")
                    charvec <- paste0(charvec, "00")
                }
                substr(charvec[isMidN], iHMS["%H"], iHMS["%H"]+1) <- "23"
                substr(charvec[isMidN], iHMS["%M"], iHMS["%M"]+1) <- "59"
                substr(charvec[isMidN], iHMS["%S"], iHMS["%S"]+1) <- "59"
            }
        }
    }
    
    ## Convert "charvec" to standard ISO format:
    ans = format(s + strptime(charvec, format), "%Y-%m-%d %H:%M:%S")

    # Return Value:
    ans
}


# ------------------------------------------------------------------------------


.midnightStandard.OLD = 
function(charvec, format)
{   # A function implemented by Diethelm Wuertz
  
    # FUNCTION:
    
    # Format:
    nchar.iso = mean(nchar(charvec))
    isoFormat = "%Y-%m-%d %H:%M:%S"
    
    # ISO-8601 Midnight Standard:
    s = rep(0, length(charvec))
    if (nchar.iso == 19) {
        s[grep("24:00:00", charvec)] = 1
        charvec = gsub("24:00:00", "23:59:59", charvec) 
        # Convert "charvec" to standard ISO format:
        charvec = format(strptime(charvec, format)+s, isoFormat)
    }
    if (nchar.iso == 14) {
        # Fixed DW 2006-03-13
        charvec.date = substr(charvec, 1, 8)
        charvec.time = substr(charvec, 9, 14)
        s[grep("240000", charvec.time)] = 1
        sub.charvec = substr(charvec, 9, 14)
        # charvec = gsub("240000", "235959", charvec) 
        charvec.time = gsub("240000", "235959", charvec.time) 
        charvec = paste(charvec.date, charvec.time, sep = "")
        # Convert "charvec" to standard ISO format:
        charvec = format(strptime(charvec, format)+s, isoFormat)
    }   
    
    # Return Value:
    charvec 
}


# ------------------------------------------------------------------------------


.formatFinCenter <- 
function(charvec, FinCenter, type = c("gmt2any", "any2gmt"))
{   # A function implemented by Diethelm Wuertz

    # Description:
    #   Internal function used by function timeDate()

    if (FinCenter == "GMT")
        return(charvec)

    ## else start working:

    type <- match.arg(type)
    signum <- switch(type,
                     "gmt2any" = +1,
                     "any2gmt" = -1)
    ##  otherwise give error


        # Get the DST list from the database:
        dst.list = rulesFinCenter(FinCenter)
        # Update list with last entry:
        z = as.matrix(dst.list)
        z[dim(z)[1], ]
        vec1 = as.vector(c(z[, 1], "2099-01-01 00:00:00"))
        vec2 = as.vector(c(z[, 2], rev(z[, 2])[1]))
        dst.list = data.frame(ruleChanges = as.character(vec1),
            offSet = as.integer(vec2))
        # Extract the dates when DST was changed:
        dst.dates = as.character(dst.list[, 1])
        # Extract the Offsets to GMT
        dst.offsets = as.character(dst.list[, 2])
        # The new dates ar the charvec's:
        new.dates = charvec
        # The new offsets are still unknown:
        new.offsets = rep(NA, length(charvec))
        # Combine all Dates and Offsets:
        dates = c(dst.dates, new.dates)
        offsets = c(dst.offsets, new.offsets)
        # The number of Offsets:
        n = length(dates)
        # Order the Dates:
        o = order(dates)
        # Dates and Offsets in the right order:
        o.dates = dates[o]
        o.offsets = offsets[o]
        # The points at which we have to determine the offsets
        xout = (1:n)[is.na(o.offsets)]
        # The date indexes:
        x = (1:n)[-xout]
        # The corresponding offsets
        y = o.offsets[x]
        # The new offsets:
        yout = approx(x, y , xout, method = "constant")$y
        # All dates:
        m = length(dst.dates)
        # Put them in the right order:
        # Added DW: 2005-05-27
        idx = order(o[which(o>m)])
        offSets = yout[idx]
        dt = strptime(charvec, "%Y-%m-%d %H:%M:%S")

    ## Return Value:
    format(dt + signum * offSets, format="%Y-%m-%d %H:%M:%S")
}


################################################################################

