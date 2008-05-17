
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
#   1999 - Diethelm Wuertz, GPL
#   2007 - Rmetrics Foundation, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
# for code accessed (or partly included) from other sources:
#   see Rmetric's copyright and license files


################################################################################
# Package: quadprog
# Version: <CRAN>
# Date: <CRAN>
# Title: Functions to solve Quadratic Programming Problems.
# Author: S original by Berwin A. Turlach <berwin.turlach@anu.edu.au>
#   R port by Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
# Maintainer: Andreas Weingessel <Andreas.Weingessel@ci.tuwien.ac.at>
# Description: This package contains routines and documentation for
#   solving quadratic programming problems.
# License: GPL-2
################################################################################


################################################################################
# FUNCTION:                    DESCRIPTION:
#  rquadprog                    Interface to quadprog solver
################################################################################


rquadprog <-
    function(Dmat, dvec, Amat, bvec, meq)
{
    # Running ...
    # print("Running rquadprog ...")
    
    # Settings:
    n = nrow(Dmat)
    q = ncol(Amat)
    r = min(n, q)
    work = rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)

    # Optimize:
    ans = .Fortran("qpgen2",
        as.double(Dmat),
        dvec = as.double(dvec),
        as.integer(n),
        as.integer(n),
        sol = as.double(rep(0, n)),
        crval = as.double(0),
        as.double(Amat),
        as.double(bvec),
        as.integer(n),
        as.integer(q),
        as.integer(meq),
        iact = as.integer(rep(0, q)),
        nact = as.integer(0),
        iter = as.integer(rep(0, 2)),
        work = as.double(work),
        ierr = as.integer(0),
        PACKAGE = "quadprog")

    # Return Value:
    ans
}


################################################################################