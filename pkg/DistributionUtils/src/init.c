#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Fortran calls */

// ./IncompleteBessel.f :
extern void F77_NAME(incompletebesselk)(double *x, double *y, double *nu, double *eps,
					int *nmax, double *KNu,
					// => result
					double *IBF, int *status);

static const R_FortranMethodDef FortranEntries[] = {
    {"incompletebesselk", (DL_FUNC) &F77_NAME(incompletebesselk), 8},
    {NULL, NULL, 0}
};

void R_init_DistributionUtils(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
