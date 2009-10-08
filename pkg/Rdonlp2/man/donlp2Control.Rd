\name{donlp2Control}

\alias{donlp2Control}

\title{Control variables for Rdonlp2}


\description{
Collection of Control Variables}


\usage{
donlp2Control( 
    iterma = 4000, nstep = 20,fnscale = 1,
    report = FALSE, rep.freq = 1,
    tau0 = 1.0, tau = 0.1, del0 = 1.0,
    epsx = 1e-5, delmin = 0.1*del0,
    epsdif=1e-8, nreset.multiplier = 1,
    difftype=3, epsfcn = 1e-16,taubnd = 1.0,
    hessian = FALSE,
    te0 = TRUE, te1 = FALSE, te2 = FALSE, te3 = FALSE,
    silent = FALSE, intakt = TRUE)
}


\arguments{
    \item{iterma}{
        maximum number of iterations}
  
    \item{nstep}{
        maximum number of tries in the backtracking.}

    \item{fnscale}{
        set -1 to maximize the object function.}

    \item{report}{
        If \code{TRUE}, a list object which contains detailed
        information will be passed to \code{control.fun()} in
        \code{\link{donlp2}}.}

    \item{rep.freq}{
        the frequency of report(positive integer). the report
        will be passed to \code{control.fun} every \code{rep.freq}
        iterations.}

    \item{tau0}{
        the positive amount how much any constaint other than
        abound can be violated. A small \code{tau0} diminishes the
        efficiency of DONLP2, while a large \code{tau0} may degarde the
        reliability of the code.}
	    
    \item{tau}{
        gives a weight between descent for \code{fn} and
        infeasibility and is also used as a safety parameter for chosing the
        penalty weigths. It can be chosen larger zero at will, but useful
        values are between 0.1 and 1. The smaller tau, the more may
        \code{fn} be scaled down. Tau is also used as an additive increase
        for the penalty-weights. Therefore it should not be chosen too
        large, since that degrades the performance.}

    \item{del0}{
        The positive amount by which constraints are considered
        binding. If too small, the indentification of correct sets of
        binding constraints may be delayed. If too large, DONLP2 will escape
        to the full regularlized SQP method(quite costly).  Good values are
        in [0.01, 1.0]}

    \item{epsx}{
        successful temination is indicated if the Kuhn-Tucker
        criteria are satisfied within the value.}

    \item{delmin}{
        constraints are considered as sufficiently satisfied if
        absolute values of their violation are less than the value.}

    \item{epsdif}{
        relative precision in the gradients.}

    \item{nreset.multiplier}{
        maximum number of steps
        (\code{nreset.multiplier} times \code{n}) until a ``restart'' of
        the accumulated quasi-newton-update is tried. Value should be
        integer between 1 and 4.}

    \item{difftype}{1,2,3. numerical differentiation algorithm. The
        algorithm with \code{difftype=2} is nearly identical to one used in
        \code{optim()}. See PDF manual attached in this package. }
  
    \item{epsfcn}{
        relative precision of the function evaluation routine.}
  
    \item{taubnd}{
        The positive amount by which bounds may be violated if
        numerical differention is used.}
  
    \item{hessian}{
        if \code{TRUE}, numeric Hessian matrix is calculated by
        numerical differentiation algorithm specified in \code{difftype}.}

    \item{intakt}{
        if TRUE, informations about current iteration step in
        optimization and final results are output to R console. The amount
        of information depends on \code{te0}, \code{te1}, \code{te2},
        \code{te3}.}

    \item{te0}{
        if TRUE one-line-output for every step is printed.}

    \item{te1}{
        if TRUE post-mortem-dump of accumlated information is printed.}

    \item{te2}{
        if TRUE, more detailed information is ``pretty-printed''.}

    \item{te3}{
        if TRUE, the gradients and approximated Newton-Raphson
        updates(in upper triangler matrix) are printed.}

    \item{silent}{
        If \code{TRUE}, \code{donlp2()} runs quietly, i.e.,
        \code{intakt=FALSE}, .pro/.mes files are not created, and
        \code{te0,te1,te2,te3} are omitted.}
}


\seealso{
  \code{\link{donlp2}}
}


\keyword{optimize}

