
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this library; if not, write to the
# Free Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA  02111-1307  USA


################################################################################
# FUNCTION:               DESCRIPTION:    
#  .nlminb2Env              Create NLMINB2 Environment
#  .setnlminb2Env           Set NLMINB2 Environment       
#  .getnlminb2Env           Get NLMINB2 Environment
################################################################################


.nlminb2Env <- 
new.env(hash = TRUE)


# ------------------------------------------------------------------------------


.setnlminb2Env <-
function(...)
{
    x <- list(...)
    nm <- names(x)
     if (is.null(nm) || "" %in% nm)
        stop("all arguments must be named")
    sapply(nm, function(nm) assign(nm, x[[nm]], envir = .nlminb2Env))
    invisible()
}


# ------------------------------------------------------------------------------


.getnlminb2Env <-
function(x = NULL, unset = "")
{
    if (is.null(x))
        x <- ls(all.names = TRUE, envir = .nlminb2Env)
        ###     unlist(mget(x, envir = .nlminb2Env, mode = "any",
        ###         ifnotfound = as.list(unset)), recursive = FALSE)
    get(x, envir = .nlminb2Env, mode = "any")
}


################################################################################

