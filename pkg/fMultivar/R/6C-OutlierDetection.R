

################################################################################
# *****  Notes on domouSe library  *****
# 
# ***  Installation  ***
# 
# To install the domouSe library:
# 
# 1) Verify (and, if need, change) the first three lines (variables SS,
#    F77C and LDRDO) of the file init to ensure that they are compatible
#    with your environment.
# 
# 2) Execute this file.  Type:
#     init
#    Note:  Under certain environments, the FORTRAN compilation step 
#               causes these messages:
#    fquad.f:
#           Warning on line 99 of fquad.f: there is a branch to label 
#                   24 from outside block
#           Warning on line 108 of fquad.f: there is a branch to label 
#                   25 from outside block
#    robstS.f:
#               as1: Warning: , line 0: Floating exception in conversion 
#              to binary
# 
# 
###############################################################################
#
# ***  R functions  ***
# 
# This library contains 13 functions useful in detection of multivariate
# outliers:
#
# 1) Main functions
# infl.mult:        influence function of all subsets of size k of a data 
#                   matrix or a given subset of size k
# influence:        influence function of a data matrix with various vector
#                   correlation coefficients.
# robuste:          robust estimation of the covariance matrix and mean vector
# rv:               computation of a vector correlation coefficient (4 possible
#                   values, see below)
#
# 2) Utility functions for detection of outliers
# rohlf:            Generalised Gap Test for multivariate outliers (Rohlf)
# wilks:            Wilks's test and Rohlf's distance in detection of 
#                   multivariate statistical outliers
#
# 3) Utility functions
# dom.dyn.load2:    function for the dynamic loading of the object file 
#                   domouSe.o before calling the functions fquad() or 
#                   robuste().  This is a version of the S-PLUS function 
# eigen2:           dyn.load2() eigen analysis of real matrix
# fquad:            approximation of the distribution function f (x) = P(Q <= x)
#                   where Q is a linear combination of Chi-square variables
# gen.comb:         to generate from a "combination" the "next combination"
# infl.A:           used in the computation of the variance of the influence
#                   function
# trace.mat:        trace of a matrix
# varinfl:          variance of the influence function
# 
# ***  Theory  ***
# 
# RV coefficients
# 
#   The four vector correlation coefficients are:
# 
#           tr(S  S  )
#               12 21
# RV = --------------------
#               2      2
#      sqrt[tr(S  )tr(S  )]
#               11     22
# 
# 
#            tr[sqrt(S  S  )]
#                     12 21
# RVls =   --------------------
#          sqrt[tr(S  )tr(S  )]
#                   11     22
# 
# 
#                      -1    2
#         sqrt{tr[(S  S  S  ) ]}
#                   12 22 21
# RVreg = ----------------------
#                      2
#             sqrt[tr(S  )]
#                      11
# 
# 
#              -1 
#       tr(S  S  S  )
#           12 22 21
# RVi = -------------
#          tr(S  )
#              11
# where
#   tr() means trace of a matrix,
#                                    _      _
#                                   |        |
#                                   | S   S  |
#         the covariance matrix S = |  11  12|
#                                   | S   S  |
#                                   |  21  22|
#                                   |_      _|
# and  S   is the covariance matrix between the vector X and X
#       ij                                              i     j
# 
# 
# See also these references:
# 
# - Cleroux, R., Helbling, J.-M., Ranger, N., DOMOUSE: Detection Of  
#   Multivariate Outliers Using S Environment, Proceedings. of 
#       COMPSTAT 10, Vol 1, Physica-Verlag 1992, 71-75.
# - Cleroux, R., Helbling, J.-M., Ranger, N., Detection d'ensembles de 
#       donnees aberrantes en analyse des donnees multivariees, Revue 
#       Statistique Appliquee, XXXVIII (1), 1990,  5-21.
# - Cleroux, R., Helbling, J.-M., Ranger, N., Influential Subsets  
#       Diagnostics Based on Multiple Correlation, Computational Statistics 
#       Quartely 2, 1989, 99-117.
# - Cleroux, R., Helbling, J.-M., Ranger, N., Some Methods of Detecting
#       Multivariate Outliers, Computational Statistics Quartely 3, 1986,
#       177-195.
# 
# ***  Notes  ***
# 
# These functions were written for our own needs, no warrantee of any kind
# is given.  No copyright is claimed but the software can be freely used 
# for non-commercial purposes and may be freely distributed with mentions
# of the authors.
# 
# For information or problems with these functions, e-mail to:
#       ranger@iro.umontreal.ca
#       helbling@masg6.epfl.ch
# 
################################################################################

# dom.dyn.load2 = function(names, userlibs, syslibs, size, path)
# eigen2 = function(x, symmetric)
# fquad = function(coef, x, mult, delta)
# gen.comb = function(vect, i, n, p)
# infl.A = function(type, rv, cov, p)
# infl.mult  = function(type, x, p, cov, ntuple, extrem)
# influence = function(type, x, p, cov, xsub, sub, graph, label, extrem, cal.A, A)
# robuste = function(x)
# rohlf = function(x, graph, titl)
# rv = function(type, x, p, cov, robust)
# trace.mat = function(mat)
# varinfl = function(type, rv, cov, p)
# wilks = function(x, robust, graph, titl)

################################################################################


.dom.dyn.load2 = 
function(names, userlibs = "", syslibs = NULL, size = 100000, path = NULL)
{
    # function dom.dyn.load2
    #
    # This function is a version of the original S function dyn.load.  Taken
    # from the S NEWS and programmed by Bill Dunlap and adapted (argument path).
    #
    # names : the names of object files
    # userlibs : the names of libraries like -limsl or -llinpack
    # syslibs : libraries that almost everyone wants
    #       (warning: machine implementation)
    # size : the expected size (in bytes) of code added to S
    #        (If this is insufficient, ld will be rerun after allocating
    #         more space.)
    
    if (length(names) == 0) return(char(0))
    if (!is.null(path)) names <- paste(path, names, sep = "")
    ld.tail <- paste(sep = " ", collapse = " ", names, userlibs, syslibs)
    method <- 1 # use 'ld -A ...'
    invisible(.Internal(dyn.load(ld.tail, size, method), "S_dynload"))
}


# ------------------------------------------------------------------------------


.eigen2 = 
function(x, symmetric = all(t(x) == x))
{
    # function eigen2
    #
    # This is the function eigen adapted for a real non-symetric matrix.
    # Tahen from S-NEWS network
    
    x <- as.matrix(x)
    storage.mode(x) <- "double"
    d <- dim(x)
    p <- d[1]
    if (p != d[2])
        stop("Eigenvalues only defined for square matrices")
    if (!symmetric) {
        z <- .Fortran("rg",
            as.integer(p),
            as.integer(p),
            x,
            values = double(p),
            ivalues = double(p),
            T,
            vectors = x,
            integer(p),
            double(p),
            error.code = integer(1),
            PACKAGE = "fMultivar")
        if (z$error.code)
            stop(paste("Eigen algorithm (rg) returned error code", 
                z$error.code))
        w <- seq(p)[z$ivalues > 0]
        n <- length(w)
        if (n > 0) {
            z$vectors[, w] <- z$vectors[, w] + (1i) * z$vectors[, w + 1]
            z$vectors[, w + 1] <- Conj(z$vectors[, w])
            z$values <- z$values + (1i) * z$ivalues
        }
        ord <- rev(order(Mod(z$values)))
    } else {
        z <- .Fortran("crs",
            x,
            as.integer(d),
            T,
            values = double(p),
            vectors = matrix(0, p, p),
            double(p),
            double(p),
            error.code = integer(1),
            PACKAGE = "fMultivar")
        if (z$error.code)
            stop(paste("Eigen algorithm (crs) returned error code", 
                z$error.code))
        ord <- p:1
    }
    return(list(values = z$values[ord], vectors = z$vectors[, ord]))
}


# ------------------------------------------------------------------------------


.fquad = 
function(coef, x, mult = rep(1, length(coef)), delta = rep(0, length(coef)))
{
    # function fquad
    #
    # Computes P(Q < x) where Q = sum of Chi-square with multiplicity mult 
    #   and the non-centrality parameters delta.
    # From the FORTRAN subroutine fquad in the chapter 9 of:
    #       'ON THE THEORY AND APPLICATION OF THE GENERAL LINEAR MODEL' DE
    #       J. KOERTS ET A.P.J. ABRAHAMSE, ROTTERDAM UNIVERSITY PRESS, 1969.
    #     METHOD : THE INTEGRAL DERIVED BY IMHOF IS USED AND NUMERICALLY
    #              INTEGRATED BY SIMPSON'S RULE
    #     REFERENCE : 'COMPUTING THE DISTRIBUTION OF QUADRATIC FORMS IN
    #                  NORMAL VARIABLES' BY J.P. IMHOF
    #                  BIOMETRIKA (1961), 48, 3 AND 4, P. 419

    if (any(abs(coef) <= 1e-08)) {
        warning(
            paste("\007\n Warning: the algorithm of fquad is not ``efficient'' for\n some small values of the coefficients < abs(1e-8)\nThese coefficients are forced to zeroes\nVector of coefficients:",
            paste(coef, collapse = ",")))
        coef <- coef[abs(coef) > 1e-08]
    }
    if (length(coef) == 1)
        stop("\007\n Warning: FQUAD is not good for the sum of 1 Chi-square.\n Try the S function pchisq...")
    nc <- length(coef)
    storage.mode(coef) <- "single"
    storage.mode(mult) <- "integer"
    storage.mode(delta) <- "single" #
    # We assume that the object UNIX file fquad.o loaded by the 
    # function dyn.load2
    prob <- .Fortran("fquad",
        d = coef,
        nr = as.integer(nc),
        mult = mult,
        fd = as.single(0),
        eps1 = as.single(0.0001),
        eps2 = as.single(0.0001),
        delta = delta,
        xkrit = as.single(x),
        PACKAGE = "fMultivar")$fd
    prob <- ifelse(prob < 0, 0, prob)
    prob <- ifelse(prob > 1, 1, prob)
    prob
}


# ------------------------------------------------------------------------------


.gen.comb = 
function(vect, i, n, p)
{
    # function gen.comb
    #
    # fonction utilitaire generant a partir d'une combinaison dans vect, la 
    # combinaison "suivante" parmi les nombres 1 a n pris p a la fois

    if (vect[i] == n - p + i) {
        if (i == 1)
            vect <- (1:p)
        else vect <- gen.comb(vect, i - 1, n, p)
    } else {
        vect[i] <- vect[i] + 1
        if (i != p)
            for (j in ((i + 1):p))
                vect[j] <- vect[j - 1] + 1
    }
    vect
}


# ------------------------------------------------------------------------------


.infl.A = 
function(type, rv, cov, p)
{
    # function infl.A

    if (!is.matrix(cov)) {
        if (is.list(cov) && any(names(cov) == "covariance"))
            cov <- cov$covariance
        else stop(
            paste("\nThe argument cov is not a matrix or a list with",
                 "an element named covariance!!!"))
    }
    i1 <- (1:p)
    i2 <- ((p + 1):nrow(cov))
    switch(type,
        rv = {
            A <- cov
            denom <- trace.mat(cov[i1, i2] %*% cov[i2, i1])
            A[i1, i1] <- .Uminus(A[i1, i1])/trace.mat(cov[i1, i1] %*% 
                cov[i1, i1])
            A[i1, i2] <- A[i1, i2]/denom
            A[i2, i1] <- A[i2, i1]/denom
            A[i2, i2] <- .Uminus(A[i2, i2])/trace.mat(cov[i2, i2] %*% 
                cov[i2, i2])
        }
        ,
        rvreg = {
            B <- cov[i1, i2] %*% solve(cov[i2, i2])
            S11 <- B %*% cov[i2, i1]
            trS11S11 <- trace.mat(S11 %*% S11)
            A <- matrix(nrow = nrow(cov), ncol = nrow(cov))
            A[i1, i1] <- .Uminus(cov[i1, i1])/trace.mat(cov[i1, i1] %*%
                cov[i1, i1])
            A[i1, i2] <- S11 %*% B/trS11S11
            A[i2, i1] <- t(A[i1, i2])
            A[i2, i2] <- .Uminus(A[i2, i1]) %*% B
        }
        ,
        rvls = {
            A <- matrix(nrow = nrow(cov), ncol = nrow(cov))
            temp <- eigen(cov[i1, i2] %*% cov[i2, i1])
            if (length(temp$values) == 1) {
                temp$vectors <- as.matrix(temp$vectors)
                temp$values <- as.matrix(temp$values)
            }
            if (any(temp$values < 0)) {
                warning(paste(
                  "\007\n Some eigenvalues are < 0\n Eigenvalues are:",
                  paste(temp$values, collapse = ","), 
                  "\n Negative eigenvalues > -1.E-7 were forced to zeroes\n"
                  ))
                temp$values[temp$values > 
                  -9.9999999999999995e-08 & temp$values < 0] <- 
                  0
            }
            H <- temp$vectors %*% diag(sqrt(temp$values)) %*% t(
                temp$vectors)
            H.1 <- solve(H) %*% cov[i1, i2]
            trH <- trace.mat(H)
            A[i1, i1] <- (.Uminus(diag(p)))/trace.mat(cov[i1, i1])
            A[i1, i2] <- H.1/trH
            A[i2, i1] <- t(H.1)/trH
            A[i2, i2] <- (.Uminus(diag(length(i2))))/trace.mat(cov[
                i2, i2])
            A <- A/2
        }
        ,
        rvi = {
            A <- matrix(nrow = nrow(cov), ncol = nrow(cov))
            B <- cov[i1, i2] %*% solve(cov[i2, i2])
            Setoile <- B %*% cov[i2, i1]
            trSetoile <- trace.mat(Setoile)
            A[i1, i1] <- (.Uminus(diag(p)))/trace.mat(cov[i1, i1])
            A[i1, i2] <- B/trSetoile
            A[i2, i1] <- t(A[i1, i2])
            A[i2, i2] <- .Uminus((t(B) %*% B))/trSetoile
        }
        )
    A * rv
}


# ------------------------------------------------------------------------------


.infl.mult  = 
function(type, x, p, cov, ntuple, 
extrem = ceiling(0.10000000000000001 * nrow(x)))
{
    # function infl.mult

    n <- nrow(x)
    rv <- rv(type, cov, p, T)
    A <- infl.A(type, rv, cov, p)
    lrobust <- ifelse(is.list(cov) && any(names(cov) == "covariance"), T, F
        )
    if (is.list(ntuple)) {
        l <- NULL
        for (i in ntuple)
            l <- c(l, length(i))
        maxl <- max(l)
        resul <- vector("list", maxl)
        for (i in ntuple) {
            l <- length(i)
            x.tuple <- apply(matrix(x[i,  ], nrow = l), 2, mean)
            infl <- l * influence(type, x, p, cov, x.tuple, graph
                 = FALSE, cal.A = FALSE, A = A)$infl
            resul[[l]] <- rbind(resul[[l]], c(i, infl))
        }
        final <- list()
        for (i in resul)
            if (!is.null(i)) {
                final[[length(final) + 1]] <- i
                lf <- length(final)
                names(final)[lf] <- paste(ncol(i) - 1, ".tuple", sep = "")
                dimnames(final[[lf]]) <- list(rep("", nrow(
                  final[[lf]])), c(rep("No.", ncol(final[[lf]]) -
                  1), "Influence"))
            }
    } else {
        comb <- ((n + 1 - ntuple):n)
        l <- prod(comb/(1:length(comb)))
        cat("\007\n It will take many seconds (minutes?, hours?) to compute all the influence functions...\n Be patient...\n")
        min.ext <- matrix(rep(c(rep(NA, ntuple), 10000000000), extrem), 
            byrow = TRUE, nrow = extrem)
        max.ext <- .Uminus(min.ext)
        for (i in (1:l)) {
            comb <- gen.comb(comb, ntuple, n, ntuple)
            xcomb <- apply(matrix(x[comb,  ], nrow = ntuple), 2, 
                mean)
            infl <- ntuple * influence(type, x, p, cov, xcomb, 
                graph = FALSE, cal.A = FALSE, A = A)$infl
            position <- infl < min.ext[, ntuple + 1]
            if (any(position)) {
                pos <- min((1:extrem)[position])
                if (pos == 1)
                  min.ext <- rbind(c(comb, infl), min.ext[
                    .Uminus(extrem),  ])
                else {
                  if (pos == extrem)
                    min.ext <- rbind(min.ext[.Uminus(extrem),  
                      ], c(comb, infl))
                  else min.ext <- rbind(min.ext[1:(pos - 1),  ],
                      c(comb, infl), min.ext[pos:(extrem - 1),  
                      ])
                }
            }
            position <- infl > max.ext[, ntuple + 1]
            if (any(position)) {
                pos <- max((1:extrem)[position])
                if (pos == 1)
                  max.ext <- rbind(c(comb, infl), max.ext[-1, ])
                else {
                  if (pos == extrem)
                    max.ext <- rbind(max.ext[-1,  ], c(comb, infl))
                  else max.ext <- rbind(max.ext[2:pos,  ], c(
                      comb, infl), max.ext[(pos + 1):extrem,  ] )
                }
            }
        }
        dimnames(min.ext) <- list(rep("", extrem), c(rep("No.", ntuple),
            "Influence"))
        dimnames(max.ext) <- dimnames(min.ext)
        final <- list(min = min.ext, max = max.ext)
    }
    attr(final, "Information.domouSe") <- c(paste("Data: object", deparse(
        substitute(x))), paste("Correlation:", type))
    if (lrobust)
        attr(final, "Information.domouSe") <- c(attributes(final)$
            Information.domouSe, "Robust estimation")
    final
}


# ------------------------------------------------------------------------------


.influence = 
function(type, x, p, cov, xsub = NULL, sub = NULL, graph = TRUE, label = TRUE, 
extrem = if (label & is.null(xsub)) ceiling(0.025000000000000001 * nrow(x)), 
cal.A = TRUE, A = NULL)
{
    # function influence

    if (is.list(cov) && any(names(cov) == "covariance")) {
        lrobust <- T
        mean.rob <- cov$mean
        cov <- cov$covariance
    } else lrobust <- F
    if (is.null(xsub))
        xxsub <- x
    else xxsub <- xsub
    if (is.matrix(xxsub)) {
        n <- nrow(xxsub)
        if (is.null(sub))
            sub <- (1:n)
        sub <- sort(sub)
        if (sub[1] < 1 | sub[length(sub)] > n)
            stop("\007\n Error in the argument sub")
        if (lrobust)
            z <- t(apply(xxsub, 1, "-", mean.rob))[sub,  ]
        else z <- t(apply(xxsub, 1, "-", apply(x, 2, mean)))[sub,  ]
    } else {
        n <- 1
        if (lrobust)
            z <- t(apply(matrix(xxsub, nrow = 1), 1, "-", mean.rob))
        else 
            z <- t(apply(matrix(xxsub, nrow = 1), 1, "-", apply(x, 2, mean)))
        sub <- 1
    }
    rv <- rv(type, cov, p, T)
    if (cal.A)
        A <- infl.A(type, rv, cov, p)
    else 
        A <- A
    infl <- diag(z %*% A %*% t(z))
    var <- 2 * trace.mat(A %*% cov %*% A %*% cov)
    lsub <- length(sub)
    if (graph) {
        par(oma = c(0, 0, 3, 0))
        plot(1:lsub, infl, axes = FALSE, xlab = "Sequence Number", ylab = 
            "Influence")
        mtext(paste("Influences (", type, ") (data: object ", deparse(
            substitute(x)), ")", sep = ""), 3, 3)
        if (lrobust)
            mtext("(Robust estimation)", 3, 2, cex = 0.69999999999999996)
        abline(h = 0)
        limite <- 3 * sqrt(var)
        if (limite < par("usr")[4])
            abline(h = limite, lty = 3)
        else 
            cat("Any observation has an influence > 3 * sqrt(Var(Infl))\n")
        if (.Uminus(limite) > par("usr")[3])
            abline(h = .Uminus(limite), lty = 3)
        else 
            cat("Any observation has an influence < -3 * sqrt(Var(Infl))\n")
        axis(2)
        axis(1, 1:lsub, paste(sub))
        box()
        mtext(paste(type, "=", round(rv, 3), " ; Var(Infl(X;", type, 
            "))=", round(var, 3), sep = ""), 3, 1)
        if (label) {
            ordre <- order(infl)
            inflo <- infl[ordre]
            seq <- sub[ordre]   #if (length(dimnames(x)[[1]]) > 0) {
            if (length(dimnames(xxsub)[[1]]) > 0) {
                etiq <- dimnames(xxsub)[[1]][sub]
                etiq <- etiq[ordre]
            } else {
                etiq <- paste(seq)
            }
            names(sub) <- etiq
            if (lsub == n) {
                ext <- c((1:extrem), ((n + 1 - extrem):n))
                text(seq[ext], inflo[ext], paste("    ", etiq[ext]))
            }
        }
        par(oma = rep(0, 4))
        invisible(list(x = sub, y = infl))
    } else {
        if (n <= 1)
            prob.ext <- NULL
        else {
            valprop <- eigen2(cov %*% A, F)$values
            if (any(abs(Im(valprop)) > 1.0000000000000001e-15))
                stop(
                  "\007\n Warning, one of the eigenvales of the matrix SA has a non-zero imaginary part!!!")
            else 
                valprop <- Re(valprop)
            ordre <- order(infl)
            if (is.null(dimnames(xxsub)[[1]]))
                info <- c(paste("(Data #", ordre[1], ")", sep = ""), 
                    paste("(Data #", ordre[lsub], ")", sep = ""))
            else {
                nom <- dimnames(xxsub)[[1]][sub]
                info <- c(paste("(", nom[ordre[1]], ")", sep = ""), 
                    paste("(", nom[ordre[lsub]], ")", sep = ""))
            }
            prob.ext <- matrix(ncol = 3, nrow = 2, dimnames = list(
                c(paste("Min.", info[1]), paste("Max.", info[2]
                )), c("Infl.", "f (Infl.)", "Prob.")))
            prob.ext[, 1] <- c(infl[ordre[1]], infl[ordre[lsub]])
            prob.ext[, 2] <- c(fquad(valprop, prob.ext[1, 1]), 
                fquad(valprop, prob.ext[2, 1]))
            if (prob.ext[1, 2] < 0) {
                warning("The value of the influence function is too small...\n The probabibility is fixed to 0")
                prob.ext[1, 2] <- 0
            }
            if (prob.ext[2, 2] > 1) {
                warning("The value of the influence function is too large...\n The probabibility is fixed to 1")
                prob.ext[2, 2] <- 1
            }
            prob.ext[, 3] <- c(1 - (1 - prob.ext[1, 2])^lsub, prob.ext[2, 2]^lsub)
        }
        if (is.null(dimnames(xxsub)[[1]]))
            names(infl) <- paste("Data #", sub, sep = "")
        else 
            names(infl) <- dimnames(xxsub)[[1]][sub]
        resul <- list(rv = rv, var = var, infl = infl, prob.ext = prob.ext)
        attr(resul, "Information.domouSe") <- c(paste("Data: object", 
            deparse(substitute(x))), paste("Correlation:", type), 
            attributes(cov)$Information.domouSe)
        attr(resul$rv, "Information.domouSe") <- NULL
        attr(resul$var, "Information.domouSe") <- NULL
        resul
    }
}



# ------------------------------------------------------------------------------


.robuste = 
function(x)
{
    # function robuste
    #
    # Subroutine robsts computes the robust estimation of the covariance 
    # matrix of Huber et programmed by Marazzi (program ROBETH).
    
    x <- as.matrix(x)
    nobs <- nrow(x)
    nvar <- ncol(x)
    ncov <- (nvar * (nvar + 1))/2
    storage.mode(x) <- "single" 
    # We assume that the object UNIX file robstS.o loaded by the function
    #   dyn.load2
    resul <- .Fortran("robsts",
        x = x,
        nobs = as.integer(nobs),
        nvar = as.integer(nvar),
        sz = single(nvar),
        ncov = as.integer(ncov),
        a = single(ncov),
        b = single(nvar),
        ainv = single(ncov),
        cov = single(ncov),
        sc1 = single(nvar),
        sc2 = single(ncov),
        istop = integer(2),
        messag = character(1),
        PACKAGE = "fMultivar")
    if (resul$istop[1] != -1) {
        if (resul$istop[2] == 0)
            warning(paste(
                "\007\n The FORTRAN subroutine robusts\n found one(or more) non fatal error message nb.",
                resul$istop[1], "in the subroutine", resul$
                messag, 
                "\n Check the source file of the subroutine robusts..."))
        else 
            stop(paste("\007\n Warning: The FORTRAN subroutine robusts\n fatal found a fatal error message nb.",
                resul$istop[1], "in the subroutine", resul$
                messag, 
                "\n Check the source file of the subroutine robusts..."))
    }
    covect <- resul$cov
    covmat <- matrix(ncol = nvar, nrow = nvar, 
        dimnames = list(dimnames(x)[[2]], dimnames(x)[[2]]))
    n <- 1
    for (i in (1:nvar)) {
        n <- n + i - 1
        covmat[i,  ] <- c(covect[n:(n + i - 1)], rep(0, nvar - i))
        if (i != 1)
            covmat[(1:(i - 1)), i] <- covmat[i, (1:(i - 1))]
    }
    meanvect <- resul$b
    names(meanvect) <- dimnames(x)[[2]]
    resul <- list(mean = meanvect, covariance = covmat)
    attr(resul$mean, "Information.domouSe") <- 
        "Robust estimation of the mean vector"
    attr(resul$covariance, "Information.domouSe") <- 
        "Robust estimation of the covariance matrix"
    resul
}



# ------------------------------------------------------------------------------


.rohlf = 
function(x, graph = TRUE, titl = paste("data: objet", deparse(substitute(x))))
{
    # function rohlf
    #
    # GENERALISED GAP TEST for multivariate outliers
    # Based on "minimal spanning tree" algorithm.  Ref.:
    # ROHLF,F.J.[1975].GENERALIZATION OF THE GAP TEST FOR
    #    THE DETECTION OF MULTIVARIATE OUTLIERS.BIOMETRICS 31, 93-101.

    mst <- mstree(t(t(x)/sqrt(apply(x, 2, var))))
    distance <- sqrt((mst$x[seq(mst$mst)] - mst$x[mst$mst])^2 + (mst$y[seq(
        mst$mst)] - mst$y[mst$mst])^2)
    n1 <- nrow(x) - 1
    distance <- matrix(c((1:n1), mst$mst, distance), ncol = 3, dimnames = 
        list(rep("", n1), c("Data #", "Data #", "Distance")))
    if (graph) {
        plot(mst, type = "n", main = paste("Rohlf's Test (", titl, ")", 
            sep = ""), xlab = "", ylab = "")
        text(mst)
        segments(mst$x[seq(mst$mst)], mst$y[seq(mst$mst)], mst$x[mst$
            mst], mst$y[mst$mst])
        max <- order(distance[, 3])[n1]
        if (distance[max, 3] <= 1)
            npoints <- 2
        else npoints <- ceiling(distance[max, 3]) * 3
        if (mst$x[distance[max, 1]] == mst$x[distance[max, 2]])
            xp <- rep(mst$x[distance[max, 1]], npoints)
        else xp <- seq(mst$x[distance[max, 1]], mst$x[distance[max, 2]],
                length = npoints)
        if (mst$y[distance[max, 1]] == mst$y[distance[max, 2]])
            yp <- rep(mst$y[distance[max, 1]], npoints)
        else yp <- seq(mst$y[distance[max, 1]], mst$y[distance[max, 2]],
                length = npoints)
        points(xp[.Uminus(c(1, npoints))], yp[.Uminus(c(1, npoints))])
        if (is.null(dimnames(x)[[1]]))
            noms <- c("", "")
        else noms <- c(paste("(", dimnames(x)[[1]][distance[max, 1]], 
                ")", sep = ""), paste("(", dimnames(x)[[1]][
                distance[max, 2]], ")", sep = ""))
        mtext(paste("Maximum distance:", round(distance[max, 3], 2), 
            "between data", distance[max, 1], noms[1], "and", 
            distance[max, 2], noms[2]), 3)
        invisible(mst)
    } else {
        attr(distance, "Information.domouSe") <- paste("Data: object", 
            deparse(substitute(x)))
        distance
    }
}


# ------------------------------------------------------------------------------


.rv = 
function(type, x, p, cov = FALSE, robust = FALSE)
{
    # function rv

    if (!cov) {
        if (nrow(x) < ncol(x))
            warning("\007\nNumber of rows of x < number of columns !!!"
                )
        if (robust)
            x <- robuste(x)$covariance
        else x <- var(x)
    }
    else if (is.list(x) && any(names(x) == "covariance"))
        x <- x$covariance
    dimnames(x) <- NULL
    i1 <- (1:p)
    i2 <- ((p + 1):nrow(x))
    rv <- switch(type,
        rv = {
            trace.mat(x[i1, i2] %*% x[i2, i1])/sqrt(trace.mat(x[i1, 
                i1] %*% x[i1, i1]) * trace.mat(x[i2, i2] %*% x[
                i2, i2]))
        }
        ,
        rvreg = {
            num <- x[i1, i2] %*% solve(x[i2, i2]) %*% x[i2, i1]
            sqrt(trace.mat(num %*% num)/trace.mat(x[i1, i1] %*% x[
                i1, i1]))
        }
        ,
        rvls = {
            propre <- eigen(x[i1, i2] %*% x[i2, i1])
            if (p == 1 | length(i2) == 1) {
                propre$vectors <- as.matrix(propre$vectors)
                propre$values <- as.matrix(propre$values)
            }
            if (any(propre$values < 0)) {
                warning(paste(
                  "\007\n Some eigenvalues are < 0\n Eigenvalues are:",
                  paste(propre$values, collapse = ","), 
                  "\n Negative eigenvalues > -1.E-7 were forced to zeroes\n"
                  ))
                propre$values[propre$values > 
                  -9.9999999999999995e-08 & propre$values < 0] <- 
                  0
            }
            trace.mat(propre$vectors %*% diag(sqrt(propre$values)) %*% 
                t(propre$vectors))/sqrt(trace.mat(x[i1, i1]) * 
                trace.mat(x[i2, i2]))
        }
        ,
        rvi = trace.mat(x[i1, i2] %*% solve(x[i2, i2]) %*% x[i2, i1])/
            trace.mat(x[i1, i1]),
        stop("\007 \n Argument type must be one of these values:\n rv rvreg rvls rvi"
            ))
    info <- ifelse(cov, "covariance matrix:", "data:")
    attr(rv, "Information.domouSe") <- c(paste("Based on", info, "object", 
        deparse(substitute(x))), paste("Correlation:", type))
    rv
}


# ------------------------------------------------------------------------------


.trace.mat = 
function(mat)
{
    # function trace.mat: trace of a matrix

    sum(diag(as.matrix(mat)))
}


# ------------------------------------------------------------------------------


.varinfl = 
function(type, rv, cov, p)
{
    # function varinfl

    A <- infl.A(type, rv, cov, p)
    if (is.list(cov) && any(names(cov) == "covariance"))
        var <- 2 * trace.mat(A %*% cov$covariance %*% A %*% cov$
            covariance)
    else var <- 2 * trace.mat(A %*% cov %*% A %*% cov)
    attr(var, "Information.domouSe") <- attr(rv, "Information.domouSe")
    var
}



# ------------------------------------------------------------------------------


.wilks = 
function(x, robust = FALSE, graph = TRUE, 
titl = paste("data: object", deparse(substitute(x))))
{
    # function wilks 
    # REFERENCES:
    # WILKS,S.S.[1963].MULTIVARIATE STATISTICAL OUTLIERS.
    #       SANKHYA SER. A 25, 407-426.
    # For the  statistique D(I):
    # ROHLF,F.J.[1975].GENERALIZATION OF THE GAP TEST FOR THE
    #       DETECTION OF MULTIVARIATE OUTLIERS.  BIOMETRICS 31,98.

    n <- nrow(x)
    if (robust)
        varx <- substitute(function(i)
        robuste(i)$covariance)
    else varx <- substitute(function(i)
        var(i))
    deter <- prod(diag(chol(varx(x) * (n - 1))))^2
    w <- vector("numeric", n)
    d <- w
    for (i in (1:n)) {
        covar <- varx(x[.Uminus(i),  ]) * (n - 2)
        w[i] <- prod(diag(chol(covar)))
        if (robust)
            sx <- x[i,  ] - robuste(x[.Uminus(i),  ])$mean
        else sx <- x[i,  ] - apply(x[.Uminus(i),  ], 2, mean)
        names(sx) <- NULL
        d[i] <- sx %*% solve(covar) %*% sx
    }
    w <- (w^2)/deter
    if (graph) {
        par(oma = c(0, 0, 5, 0), mfrow = c(2, 1))
        plot(1:n, w, type = "l", xlab = "Sequence number", ylab = "W", 
            ticks = F)
        axis(1, 1:n)
        axis(2)
        mtext("Wilks's Test and Rohlf's Distance", 3, 3, T, cex = 1.3)
        mtext(paste("(", titl, ")", ifelse(robust, 
            " (Robust estimation)", ""), sep = ""), 3, 2, T)
        title("Determinants Ratio (Wilks)", cex = 0.80000000000000004)
        indic <- order(w)[1]
        if (is.null(dimnames(x)[[1]]))
            nomi <- ""
        else nomi <- paste("(", dimnames(x)[[1]][indic], ")", sep = "")
        text(indic, w[indic], paste("    ", indic, nomi))
        plot(1:n, d, type = "l", xlab = "Sequence number", ylab = "D", 
            ticks = F)
        axis(1, 1:n)
        axis(2)
        title("Rohlf's Distance", cex = 0.80000000000000004)
        indic <- order(d)[n]
        if (is.null(dimnames(x)[[1]]))
            nomi <- ""
        else nomi <- paste("(", dimnames(x)[[1]][indic], ")", sep = "")
        text(indic, d[indic], paste("    ", indic, nomi))
        par(mfg = c(1, 1, 1, 1), oma = rep(0, 4), new = F)
        invisible()
    } else {
        if (!is.null(dimnames(x)[[1]])) {
            names(w) <- dimnames(x)[[1]]
            names(d) <- names(w)
        }
        resul <- list(w = w, d = d)
        attr(resul$w, "w") <- "Determinants Ratio (Wilks)"
        attr(resul$d, "d") <- "Rohlf's Distance"
        attr(resul, "Information.domouSe") <- paste("Data: object", 
            deparse(substitute(x)))
        if (robust)
            attr(resul, "Information.domouSe") <- c(attr(resul, 
                "Information.domouSe"), "Robust estimation")
        resul
    }
}


################################################################################

