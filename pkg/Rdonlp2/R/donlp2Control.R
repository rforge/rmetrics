

################################################################################


donlp2Control <- 
    function()         
{
    list(
        iterma = 4000,
        nstep = 20, 
        fnscale = 1,
        report = FALSE, 
        rep.freq = 1,
        tau0 = 1.0, 
        tau = 0.1, 
        del0 = 1.0,
        epsx = 1.0e-5, 
        delmin = 0.1, # suggested 0.1*del0
        epsdif = 1e-8, 
        nreset.multiplier = 1,
        difftype = 3, 
        epsfcn = 1.0e-16, 
        taubnd = 1.0,
        hessian = FALSE,
        te0 = TRUE, 
        te1 = FALSE, 
        te2 = FALSE, 
        te3 = FALSE,
        silent = TRUE,
        intakt = TRUE)
}


################################################################################

