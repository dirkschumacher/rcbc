#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

void rcbc_cbc_solve(
  /// dimensionality of arguments
  int *nCols,
  int *nRows,
  int *nElements,
  int *nIntegerIndices,
  int *nArgs,
  // dimensionality of non-finite values
  int *nColLowerInf,
  int *nColLowerNInf,
  int *nColUpperInf,
  int *nColUpperNInf,
  int *nRowLowerInf,
  int *nRowLowerNInf,
  int *nRowUpperInf,
  int *nRowUpperNInf,
  // non-finite values
  int *colLowerInf,
  int *colLowerNInf,
  int *colUpperInf,
  int *colUpperNInf,
  int *rowLowerInf,
  int *rowLowerNInf,
  int *rowUpperInf,
  int *rowUpperNInf,
  /// optimization problem
  double *obj,
  int *isMaximization,
  int *rowIndices,
  int *colIndices,
  double *elements,
  int *integerIndices,
  double *colLower,
  double *colUpper,
  double *rowLower,
  double *rowUpper,
  /// CBC arguments
  const char **argsList,
  /// outputs
  double *column_solution,
  double *objective_value,
  int *is_proven_optimal,
  int *is_proven_dual_infeasible,
  int *is_proven_infeasible,
  int *is_node_limit_reached,
  int *is_solution_limit_reached,
  int *is_abandoned,
  int *is_iteration_limit_reached,
  int *is_seconds_limit_reached
);

static const R_CMethodDef CEntries[] = {
    {"rcbc_cbc_solve", (DL_FUNC) &rcbc_cbc_solve, 42},
    {NULL, NULL, 0}
};

void R_init_rcbc(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
