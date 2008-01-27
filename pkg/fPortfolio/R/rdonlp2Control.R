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


rdonlp2Control <- 
    function(            
    # setup
    iterma = 4000, nstep = 20,fnscale = 1,
    report = FALSE, rep.freq = 1,
    # perfomance and tunings
    tau0 = 1.0, tau = 0.1, del0 = 1.0,
    # termination criteria
    epsx = 1e-5, delmin = 0.1*del0,
    epsdif = 1e-8, nreset.multiplier = 1,
    # numerical differentiation
    difftype = 3, epsfcn = 1e-16, taubnd = 1.0,
    hessian = FALSE,
    # information
    te0 = TRUE, te1 = FALSE, te2 = FALSE, te3 = FALSE,
    silent = FALSE, intakt = TRUE )
{
    # NOte:
    #   DW
    #   onLoad <- function(libname, pkgname){
    #       verbose <- .Options$Hverbose
    #       if(!length(verbose) || verbose){
    #           cat("Rdonlp2 - a wrapper library for \"DONLP2 (C) Peter Spellucci\"\n\n")
    #       }
    #       library.dynam("Rdonlp2", pkgname, libname)
    #       invisible()
    #   }
    
    # FUNCTION:
    
    # Return Value:
    list(
        iterma = as.integer(iterma), 
        nstep = as.integer(nstep),
        fnscale = fnscale,
        report = report,
        rep.freq = as.integer(ifelse(rep.freq<1, 1, rep.freq)),
        tau0 = tau0, 
        tau = tau, 
        del0 = del0,
        epsx = epsx, 
        delmin = delmin, 
        epsdif = epsdif,
        nreset.multiplier = nreset.multiplier,
        difftype = as.integer(ifelse(!difftype%in%c(1,2,3), 3, difftype)),
        epsfcn = epsfcn, 
        taubnd = taubnd, 
        hessian = hessian,
        te0 = te0, 
        te1 = te1, 
        te2 = te2, 
        te3 = te3,
        silent = silent, 
        intakt = intakt)
}


################################################################################

