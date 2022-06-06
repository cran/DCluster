/* Based on spdep/serc/init.c by Roger S. Bivand*/

#include "DCluster.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[]  = {
    {NULL, NULL, 0}
};

static R_CallMethodDef CallEntries[] = {
    {"Rkn_poisson", (DL_FUNC) &Rkn_poisson, 3},
    {"Ropgam_iscluster_negbin", (DL_FUNC) &Ropgam_iscluster_negbin, 5},
    {NULL, NULL, 0}
};


void R_init_sumR(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

}
