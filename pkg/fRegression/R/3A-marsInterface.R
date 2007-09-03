
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

# Copyrights (C)
# for this R-port: 
#   1999 - 2007, Diethelm Wuertz, GPL
#   Diethelm Wuertz <wuertz@itp.phys.ethz.ch>
#   info@rmetrics.org
#   www.rmetrics.org
# for the code accessed (or partly included) from other R-ports:
#   see R's copyright and license files
# for the code accessed (or partly included) from contributed R-ports
# and other sources
#   see Rmetrics's copyright file


################################################################################
# BUILTIN:              FROM MDA - MARS DESCRIPTION:
#  .mars                 Internal Function
#  .mars.formula         Internal Function
#  .mars.default         Internal Function
#  .predict.mars         Internal Function
#  .model.matrix.mars    Internal Function
################################################################################


################################################################################
# BUILTIN - PACKAGE DESCRIPTION:
#  Package: mda
#  Version: 0.2-23
#  Author: S original by Trevor Hastie & Robert Tibshirani.  
#    R port by Friedrich Leisch, Kurt Hornik and Brian D. Ripley.
#  Maintainer: Kurt Hornik <Kurt.Hornik@R-project.org>
#  Description: Mixture and flexible discriminant analysis, multivariate
#    additive regression splines (MARS), BRUTO, ...
#  Title: Mixture and flexible discriminant analysis
#  Depends: class, R (>= 1.5.0)
#  License: GPL version 2
#  Packaged: Sat Jan 31 13:31:19 2004; hornik


.mars = 
function(formula, data, ...) 
{   # A function implemented by Diethelm Wuertz

    # FUNCTION:
    
    # Settings:
    m = match.call(expand = FALSE)
    m$contrasts = m$... = NULL
    m[[1]] = as.name("model.frame")
    m = eval(m, parent.frame())
    na.act = attr(m, "na.action")
    Terms = attr(m, "terms")
    attr(Terms, "intercept") = 0
    X = model.matrix(Terms, m, contrasts)
    Y = model.extract(m, response)
    w = model.extract(m, weights)
    if (length(w) ==  0) w = rep(1, nrow(X))
    
    # Fit:
    fit = .mars.default(X, Y, w, ...)
    fit$terms = Terms   
    
    # Result:
    fit$family = c("", "")
    fit$parameters = as.vector(fit$coefficients)
    fit$model = data
    
    # Return Value:
    fit 
}


# ------------------------------------------------------------------------------


.mars.default = 
function (x, y, w = rep(1, nrow(x)), wp, degree = 1, nk = max(21, 
2 * ncol(x) + 1), penalty = 2, thresh = 0.001, prune = TRUE, 
trace.mars = FALSE, forward.step = TRUE, prevfit = NULL, ...) 
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   A (modified) copy from contributed R-package 'mda' 
    
    # FUNCTION:
        
    # MARS:
    this.call = match.call()
    if ((nk%%2) != 1) nk = nk - 1
    x = as.matrix(x)
    np = dim(x)
    n = np[1]
    p = np[2]
    y = as.matrix(y)
    nclass = ncol(y)
    if (is.null(np)) {
        np = c(length(x), 1)
        x = as.matrix(x) 
    }
    if (forward.step) {
        interms = 1
        lenb = nk
        bx = matrix(rep(0, nrow(x) * nk), nrow = n)
        res = matrix(rep(0, nrow(x) * ncol(y)), nrow = n)
        fullin = rep(0, nk)
        cuts = NULL
        factor = NULL 
    } else {
        bx = model.matrix.mars(prevfit, x, full = TRUE)
        interms = ncol(bx)
        lenb = prevfit$lenb
        o = prevfit$all.terms
        fullin = rep(0, ncol(bx))
        fullin[o] = 1
        res = prevfit$res
        factor = prevfit$factor
        cuts = prevfit$cuts
        if (missing(penalty)) 
            penalty = prevfit$penalty
        degree = prevfit$degree
        nk = lenb
        thresh = prevfit$thresh 
    }
    if (missing(penalty) & (degree > 1)) {
        penalty = 3
    }
    if (!missing(wp)) {
        if (any(wp <= 0)) 
            stop("wp should all be positive")
        wp = sqrt(wp/sum(wp))
        y = y * outer(rep(1, n), wp) 
    } else {
        wp = NULL
    }
    tagx = x
    storage.mode(tagx) = "integer"
    for (j in 1:p) {
        tagx[, j] = order(x[, j]) 
    }
    bestin = rep(0, nk)
    flag = matrix(rep(0, nk * p), nrow = nk, ncol = p)
    if (is.null(cuts)) {
        cuts = matrix(rep(0, nk * p), nrow = nk, ncol = p)
    }
    if (is.null(factor)) {
        dir = matrix(rep(0, nk * p), nrow = nk, ncol = p) 
    } else {
        dir = factor 
    }
    alpha = rep(0, nclass)
    beta = matrix(rep(0, nk * nclass), nrow = nk)
    bestgcv = 0
    storage.mode(y) = "double"
    storage.mode(x) = "double"
    storage.mode(bx) = "double"
    storage.mode(flag) = "integer"
    storage.mode(cuts) = "double"
    storage.mode(dir) = "double"
    storage.mode(res) = "double"
    storage.mode(beta) = "double"
    lenscrat = 1 + n + 2 * n * nk + 4 * nk * nk + 3 * nk + 3 * 
        nk * nclass + 3 * nclass + 28 * n + 51
    junk = .Fortran("marss", as.integer(n), as.integer(n), as.integer(p), 
        as.integer(nclass), as.matrix(y), as.matrix(x), as.double(w), 
        as.matrix(tagx), as.integer(degree), as.integer(nk), 
        as.double(penalty), as.double(thresh), as.logical(forward.step), 
        as.integer(interms), as.logical(prune), bx = as.matrix(bx), 
        fullin = as.integer(fullin), lenb = as.integer(lenb), 
        bestgcv = as.double(bestgcv), bestin = as.integer(bestin), 
        flag = as.matrix(flag), cuts = as.matrix(cuts), dir = as.matrix(dir), 
        res = as.matrix(res), alpha = as.double(alpha), beta = as.matrix(beta), 
        double(lenscrat), integer(4 * nk), trace.mars, PACKAGE = "mda")
    lenb = junk$lenb
    all.terms = seq(lenb)[junk$fullin[1:lenb] == 1]
    selected.terms = seq(lenb)[junk$bestin[1:lenb] == 1]
    coefficients = junk$beta[seq(selected.terms), , drop = FALSE]
    residuals = junk$res
    fitted.values = y - residuals
    if (!is.null(wp)) {
        TT = outer(rep(1, n), wp)
        residuals = residuals/TT
        fitted.values = fitted.values/TT
        coefficients = coefficients/outer(rep(1, length(selected.terms)), wp) 
    }
    dir = junk$dir[seq(lenb), , drop = FALSE]
    dimnames(dir) = list(NULL, dimnames(x)[[2]])
    cutss = junk$cuts[seq(lenb), , drop = FALSE]
    x = junk$bx[, selected.terms, drop = FALSE]
    
    # Return Value:
    structure(list(call = this.call, all.terms = all.terms, 
        selected.terms = selected.terms, 
        penalty = penalty, degree = degree, nk = nk, thresh = thresh, 
        gcv = junk$bestgcv, factor = dir, cuts = cutss, 
        residuals = residuals, 
        fitted.values = fitted.values, lenb = junk$lenb, 
        coefficients = coefficients
        #x = x
        ), 
        class = "mars")
}


# ------------------------------------------------------------------------------


.predict.mars = 
function (object, newdata, se.fit = FALSE, ...) 
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   A (modified) copy from contributed R-package 'mda' 
    
    # FUNCTION:
    
    # Select Object:
    if (class(object)[1] == "fREG") object = object@fit
    
    # Predict:
    if (missing(newdata)) {
        z <- fitted(object)
        if (is.null(z)) {
            stop("need to supply newdata")
        } else {
            ans = z
        }
    } else {
        tt = terms(object)
        Terms = delete.response(tt)
        x <- model.frame(Terms, newdata)
        x = as.matrix(x)
        dd = dim(x)
        n = dd[1]
        p = dd[2]
        which = object$selected.terms
        nterms = length(which)
        dir = object$factor
        cut = object$cuts
        
        modelMatrix = matrix(1, nrow = n, ncol = nterms)
        which = which[-1]
        for (i in seq(along = which)) {
            j = which[i]
            if (all(dir[j, ] == 0)) { stop("error in factor or which") }
            temp1 = 1
            for (k in 1:p) {
                if (dir[j, k] != 0) {
                    temp2 = dir[j, k] * (x[, k] - cut[j, k])
                    temp1 = temp1 * temp2 * (temp2 > 0) 
                } 
            }
            modelMatrix[, i + 1] = temp1
        }    
        ans = modelMatrix %*% object$coefficients
        ans = as.vector(ans)
        names(ans) = rownames(newdata)
    }
    
    # Add Standard Errors ?
    if (se.fit) {
        se.fitted = NA*ans
        names(se.fitted) = names(ans)
        ans = list(fit = ans, se.fit = se.fitted)
    }
    
    # Return Value:
    ans
}



# ------------------------------------------------------------------------------


.showcuts.mars = 
function(object)
{   # A function implemented by Diethelm Wuertz

    # Note:
    #   A (modified) copy from contributed R-package 'mda' 
    
    # FUNCTION:
    
    # Select Object:
    if (class(object)[1] == "fREG") object = object@fit
    
    # Cuts:
    tmp = object$cuts[object$sel, ]
    dimnames(tmp) = list(NULL, names(trees)[-3])
    
    # Return Value:
    tmp
}


################################################################################
