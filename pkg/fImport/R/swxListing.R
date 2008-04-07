
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
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# FUNCTION:         LPP/SWX DATA IMPORT:
#  swxListing        Downloads SWX Instrument Lists from www.swx.com
################################################################################


.swxListing <-
    function(
        category = c("equity", "bond", "customer"), 
        abbreviate = 60) 
{
    # A function implemented by Diethelm Wuertz
    
    # Desription:
    #   Downloads SWX Index List     
    
    # FUNCTION:
    
    # Require:
    require(gdata)
    
    # Settings:
    category = match.arg(category)

    # For S-Plus Compatibility:
    method <- if(is.R()) NULL else "wget"
    
    # to prevent bug in dev 2.7.0 in linux
    method <- if(Sys.info()["sysname"]=="Linux") "wget" else NULL
    
    # Download and temporarily store:
    URL = "http://www.swx.com/index_info/online/calculated_indices.xls"
    file = "tempfile.xls"
    download.file(url = URL, destfile = file, mode = "wb")
    
    # Convert XLS to csv:
    sheet = 1:3
    names(sheet) = c("equity", "bond", "customer")
    # cat("... transform XLS to csv File", "\n")
    x = read.xls(file, sheet = sheet[category], skip = 5)
    
    # Adapt x:
    listing = as.matrix(cbind(x[-1, c(6, 4, 5, 2)]))
    for (i in 1:4) listing[, i] = gsub("[ \t]+", " ", listing[,i])
    index = grep("^$", listing[, 1])
    listing = listing[-index, ] 
    listing = gsub("\256", "(R)", listing)
    colnames(listing) = c("Symbol", "ISIN", "Valor", "Description")
    rownames(listing) = 1:NROW(listing)
    
    # Abbreviate ?
    listing[, 4] = abbreviate(listing[, 4], minlength = abbreviate, 
        use.classes = TRUE, dot = FALSE, method = "left.kept")
                
    # Attributes:
    attr(listing, "control") <- c(catgory = category, symbols = NCOL(listing))
    
    # Return Value:
    listing
}

   
################################################################################

