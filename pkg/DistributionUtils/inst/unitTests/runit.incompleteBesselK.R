### Unit tests of function incompleteBesselK and incompleteBesselKR

### Functions with name test.* are run by R CMD check or by make if
### LEVEL=1 in call to make
### Functions with name levelntest.* are run by make if
### LEVEL=n in call to make

test.incompleteBesselK <- function()
{
    ## Purpose: Level 1 test of incompleteBesselK
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: David Scott, Date: 5 Jan 2012

    ## Values given by Harris (2008)
    checkEquals(incompleteBesselK(0.01, 4, 0), 2.225310761266469)
    checkEquals(incompleteBesselKR(0.01, 4, 0), 2.225310761266469)

    checkEquals(incompleteBesselK(0.01, 4, 9), 0.003246798003149)
    checkEquals(incompleteBesselKR(0.01, 4, 9), 0.003246798003149)

    checkEquals(incompleteBesselK(4.95, 5, 2), 0.000012249987981)
    checkEquals(incompleteBesselKR(4.95, 5, 2), 0.000012249987981)

    checkEquals(incompleteBesselK(10, 2, 6), 0.0000004150045864731308)
    checkEquals(incompleteBesselKR(10, 2, 6), 0.0000004150045864731308)

    checkEquals(incompleteBesselK(3.1, 2.6, 5), 0.000528504325244)
    checkEquals(incompleteBesselKR(3.1, 2.6, 5), 0.000528504325244)

    ### Check values when x > y using numeric integration
    numIBF <- sapply(0:9, incompleteBesselK, x = 4, y = 0.01)

    besselFn <- function(t, x, y, nu) {
        (t^(-nu - 1))*exp(-x*t - y/t)
    }

    intIBF <- sapply(0:9, integrate, f = besselFn, lower = 1, upper = Inf,
                      x = 4, y = 0.01)
    intIBF <- as.numeric(intIBF[1, ])
    numIBF - intIBF
    ## 1.256649992398273e-11
    checkTrue(max(abs(numIBF - intIBF)) < 10^(-10))

  return()
}

