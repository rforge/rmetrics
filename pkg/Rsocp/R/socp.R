.onLoad <-
    function(libname, pkgname)
{
    verbose <- .Options$Hverbose
    if(!length(verbose) || verbose) {
        cat("Rsocp - a wrapper library for \"SOCP (C)\"\n")
        cat("Optimisation over second-order cones\n")
    }
    library.dynam("Rsocp", pkgname, libname)
    invisible()
}

socp  <-
    function(f, A, b, C, d, N, x, z, w, control = list())
{
    # A function implemented by Yohan Chalabi

    # Description:
    # ...

    # Example:
    #
    # min x + y
    # s.t. x^2 + y^2 <= 1
    #      y >= 0
    #
    # f <- c(1,1)
    # A <- matrix(c(1,0,0,0,1,0), ncol = 2)
    # b <- c(0, 0, 0)
    # C <- matrix(c(0,0, 0, 1), ncol = 2)
    # d <- c(1,0)
    # N <- c(2, 1)
    # x <- c(0, 0.5)
    # z <- c(1, 0, 0)
    # w <- c(2,1)
    # fit <- socp(f, A, b, C, d, N, x, z, w)

    # FUNCTION:

    # check control list
    cpos <- list(abs_tol = 1e-8, rel_tol = 1e-6, target = 0,
                 max_iter = 500, Nu = 10, out_mode = 0)
    if (length(control)) {
        nms <- names(control)
        if (!is.list(control) || is.null(nms))
            stop("control argument must be a named list")
        pos <- pmatch(nms, names(cpos))
        if (any(nap <- is.na(pos))) {
            warning(paste("unrecognized control element(s) named `",
                          paste(nms[nap], collapse = ", "), "' ignored",
                          sep = ""))
            pos <- pos[!nap]
            control <- control[!nap]
        }
        if (length(control))
            control <- c(control, cpos[-pos])
        else
            control <- cpos

    } else {
        control <- cpos
    }


    # Set control parameters
    abs_tol <- control["abs_tol"]
    rel_tol <- control["rel_tol"]
    target <- control["target"]
    max_iter <- control["max_iter"]
    Nu <- control["Nu"]
    out_mode <- control["out_mode"]

    # check control parameters
    stopifnot(Nu>=1)
    stopifnot(out_mode %in% 0:3)

    # set parameters
    L <- length(N)
    n <- length(x)
    iter <- max_iter
    mhist <- 0
    nhist <- 0
    ndbl <- 0
    nint <- 0
    info <- 0

    # check arguments
    stopifnot(any(dim(A) == c(sum(N), n)))
    stopifnot(length(b) == sum(N))
    stopifnot(any(dim(C) == c(L, n)))
    stopifnot(length(d) == n)

    # storage convention for socp.c routine
    Ni <- seq(from = 1, length = N[1])
    AA <- rbind(A[Ni,], C[1,])
    bb <- c(b[Ni], d[1])
    zz <- c(z[Ni], w[1])
    for (i in seq(L, from = 2)) {
        Ni <- seq(from = N[i-1] + 1, length = N[i])
        AA <- rbind(AA, A[Ni,], C[i,])
        bb <- c(bb, b[Ni], d[i])
        zz <- c(zz, z[Ni], w[i])
    }
    AA <- as.vector(AA)
    NN <- N + 1

    # compute amount of workspace required
    ### mhist <- out_mode
    ### nhist <- max_iter + 1
    ### ndbl <- 7*sum(N) + 2*n + 10*n + 2*ceiling(sqrt(n)) + 11*L
    ### nint = n
    socp_getwork <- .C("socp_getwork",
                       as.integer(L),
                       as.integer(NN),
                       as.integer(n),
                       as.integer(max_iter),
                       as.integer(out_mode),
                       as.integer(mhist),
                       as.integer(nhist),
                       as.integer(ndbl),
                       as.integer(nint),
                       PACKAGE = "Rsocp")
    mhist <- socp_getwork[[6]]
    nhist <- socp_getwork[[7]]
    ndbl <- socp_getwork[[8]]
    nint <- socp_getwork[[9]]

    # Initialise length of arrays
    hist <- rep(0, nhist*mhist + 1)
    dblwork <- rep(0, ndbl + 1)
    intwork <- rep(0, nint + 1)

    # Call socp.c routine
    socp <- .C("socp",
               as.integer(L),
               as.integer(NN),
               as.integer(n),
               as.double(f),
               as.double(AA),
               as.double(bb),
               as.double(x),
               as.double(zz),
               as.double(abs_tol),
               as.double(rel_tol),
               as.double(target),
               as.integer(iter),
               as.double(Nu),
               as.integer(info),
               as.integer(out_mode),
               as.double(hist),
               as.double(dblwork),
               as.integer(intwork),
               PACKAGE = "Rsocp")

    ans <- list(x = round(socp[[7]], 6))
    ans$z <- round(socp[[8]], 6)
    ans$iter <- socp[[12]]
    ans$hist <- socp[[16]]
    ans$convergence <- as.logical(if (socp[[14]] %in% 1:3) TRUE else FALSE)
    ans$message <- switch(as.character(socp[[14]]),
                          "0" = "Error (0)",
                          "1" = "Absolute tolerance (1)",
                          "2" = "Relative convergence (2)",
                          "3" = "Target value (achieved or unachievable) (3)",
                          "4" = "Maximum iterations (4)")

    # return
    ans
}
