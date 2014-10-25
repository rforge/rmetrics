

################################################################################
# FUNCTION:
#  socpControl
################################################################################

        
socpControl <- 
    function(abs.tol = 1e-18, rel.tol = 1e-16, target = 0, max.iter = 500,
    Nu = 10, out.mode = 0, BigM.K = 2, BigM.iter = 5)
{   
    # Return Value:
    list(
        abs.tol = abs.tol, 
        rel.tol = rel.tol,
        target = target,
        max.iter = max.iter,
        Nu = Nu,
        out.mode = out.mode,
        BigM.K = BigM.K,
        BigM.iter = BigM.iter)
}


################################################################################

