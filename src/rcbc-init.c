#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP rcbc_cpp_cbc_solve(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                          SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"rcbc_cpp_cbc_solve", (DL_FUNC) &rcbc_cpp_cbc_solve, 15},
    {NULL, NULL, 0}
};

void R_init_rcbc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
