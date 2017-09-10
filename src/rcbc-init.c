// This file was automatically generated.
// tools::package_native_routine_registration_skeleton

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _rcbc_cpp_cbc_solve(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_rcbc_cpp_cbc_solve", (DL_FUNC) &_rcbc_cpp_cbc_solve, 11},
    {NULL, NULL, 0}
};

void R_init_rcbc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
