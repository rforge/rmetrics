
# This R package is free software; you can redistribute it and/or
# modify it under the terms of the GNU Library General Public
# License as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This R package is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General
# Public License along with this R package; if not, write to the
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


.First.lib =
function(lib, pkg)
{

###     # Startup Mesage and Desription:
###     MSG <- if(getRversion() >= "2.5") packageStartupMessage else message
###     dsc <- packageDescription(pkg)
###     if(interactive() || getOption("verbose")) {
###         # not in test scripts
###         MSG(sprintf("Rmetrics Package %s (%s) loaded.", pkg, dsc$Version))
###     }

    # setting Rmetrics global variables
    setRmetricsOptions(myFinCenter = "GMT",
                       currentYear = as.POSIXlt(Sys.time())$year + 1900,
                       myUnits = "days")
}


.onLoad <-
    function(libname, pkgname)
{

    # setting Rmetrics global variables
    setRmetricsOptions(myFinCenter = "GMT",
                       currentYear = as.POSIXlt(Sys.time())$year + 1900,
                       myUnits = "days")

    ## IMO it is normal that a function in a previous import is
    ## replaced by its generic. It should not create a warning. But a
    ## warning should be generated when a generic is replaced in a
    ## previous import.

    ## It would greatly help the maintanance of packages which import
    ## S4 classes and generics because one could use the simple
    ## "import" instruction rather than "importFrom" in the NAMESPACE.

    tmp <- function(self, ns, vars, generics, packages) {
        addImports <- function(ns, from, what) {
            imp <- structure(list(what), names = getNamespaceName(from))
            imports <- getNamespaceImports(ns)
            setNamespaceInfo(ns, "imports", c(imports, imp))
        }
        namespaceIsSealed <- function(ns)
            environmentIsLocked(ns)
        makeImportExportNames <- function(spec) {
            old <- as.character(spec)
            new <- names(spec)
            if (is.null(new)) new <- old
            else new[new == ""] <- old[new == ""]
            names(old) <- new
            old
        }
        whichMethodMetaNames <- function(impvars) {
            if(!.isMethodsDispatchOn())
                return(numeric())
            mm <- ".__T__"
            seq_along(impvars)[substr(impvars, 1L, nchar(mm, type = "c")) == mm]
        }

        # ---------------
        # YC
        if (getRversion() < "2.9.1") #-> anyDuplicated introduced in r48558
            anyDuplicated <- function(...)
                any(duplicated(...))
        # ---------------

        if (is.character(self))
            self <- getNamespace(self)
        ns <- asNamespace(ns)
        if (missing(vars)) impvars <- getNamespaceExports(ns)
        else impvars <- vars
        impvars <- makeImportExportNames(impvars)
        impnames <- names(impvars)
        if (anyDuplicated(impnames)) {
            stop("duplicate import names ",
                 paste(impnames[duplicated(impnames)], collapse = ", "))
        }
        if (isNamespace(self) && isBaseNamespace(self)) {
            impenv <- self
            msg <- "replacing local value with import:"
            register <- FALSE
        }
        else if (isNamespace(self)) {
            if (namespaceIsSealed(self))
                stop("cannot import into a sealed name space")
            impenv <- parent.env(self)
            msg <- "replacing previous import:"
            register <- TRUE
        }
        else if (is.environment(self)) {
            impenv <- self
            msg <- "replacing local value with import:"
            register <- FALSE
        }
        else stop("invalid import target")
        which <- whichMethodMetaNames(impvars)
        if(length(which)) {
            ## If methods are already in impenv, merge and don't import
            delete <- integer()
            for(i in which) {
                methodsTable <- .mergeImportMethods(impenv, ns, impvars[[i]])
                if(is.null(methodsTable))
                {} ## first encounter, just import it
                else { ##
                    delete <- c(delete, i)
                    ## eventually mlist objects will disappear, for now
                    ## just don't import any duplicated names
                    mlname = sub("__T__", "__M__", impvars[[i]], fixed=TRUE)
                    ii = match(mlname, impvars, 0L)
                    if(ii > 0)
                        delete <- c(delete, ii)
                    if(!missing(generics)) {
                        genName <- generics[[i]]
                        if(i > length(generics) || !nzchar(genName))
                        {warning("got invalid index for importing ",mlname); next}
                        fdef <- methods:::getGeneric(genName,
                                                     where = impenv,
                                                     package = packages[[i]])
                        if(is.null(fdef))
                            warning(gettextf("Found methods to import for function \"%s\" but not the generic itself",
                                             genName))
                        else
                            methods:::.updateMethodsInTable(fdef, ns, TRUE)
                    }
                }
            }
            if(length(delete)) {
                impvars <- impvars[-delete]
                impnames <- impnames[-delete]
            }
        }

        # --------
        # YC :
        for (n in impnames)
            if (exists(n, envir = impenv, inherits = FALSE)) {
                if (.isMethodsDispatchOn() && methods:::isGeneric(n, ns)) {
                    ## warning if generic overwrites a function which
                    ## it was not derived from
                    genNs <- methods:::slot(get(n, envir = ns), "package")
                    genImpenv <- environmentName(environment(get(n, envir = impenv)))
                    genWarn1 <- (!identical(genNs, genImpenv))
                    ## warning if generic overwrites another generic
                    genWarn2 <- methods:::isGeneric(n, impenv)
                    if (genWarn1 || genWarn2)
                        warning(msg, " ", n)
                } else warning(msg, " ", n)
            }
        # --------

        importIntoEnv(impenv, impnames, ns, impvars)
        if (register) {
            addImports(self, ns,
                       if (missing(vars)) TRUE else impvars)
        }
    }

    require(utils)
    assignInNamespace("..Old..namespaceImportFrom", base::namespaceImportFrom,
                      ns = "base")
    environment(tmp) <- baseenv()
    assignInNamespace("namespaceImportFrom", tmp, ns = "base")
}



if(!exists("Sys.setenv", mode = "function")) # pre R-2.5.0, use "old form"
    Sys.setenv <- Sys.putenv


################################################################################

