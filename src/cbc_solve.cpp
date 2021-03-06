#include <CbcSolver.hpp>
#include <R.h>

// define infinity for convienence
#define INF std::numeric_limits<double>::infinity();

extern "C" {

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
) {
  // set Inf and -Inf values in bounds
  /// column bounds
  for (int i = 0; i < (*nColLowerInf); ++i)
    colLower[colLowerInf[i]] = INF;
  for (int i = 0; i < (*nColLowerNInf); ++i)
    colLower[colLowerNInf[i]] = -INF;
  for (int i = 0; i < (*nColUpperInf); ++i)
    colUpper[colUpperInf[i]] = INF;
  for (int i = 0; i < (*nColUpperNInf); ++i)
    colUpper[colUpperNInf[i]] = -INF;
  /// row bounds
  for (int i = 0; i < (*nRowLowerInf); ++i)
    rowLower[rowLowerInf[i]] = INF;
  for (int i = 0; i < (*nRowLowerNInf); ++i)
    rowLower[rowLowerNInf[i]] = -INF;
  for (int i = 0; i < (*nRowUpperInf); ++i)
    rowUpper[rowUpperInf[i]] = INF;
  for (int i = 0; i < (*nRowUpperNInf); ++i)
    rowUpper[rowUpperNInf[i]] = -INF;

  // build the constraint matrix in column format
  CoinPackedMatrix matrix(
    true,
    rowIndices,
    colIndices,
    elements,
    *nElements);

  // create solver
  /// initialize solver
  OsiClpSolverInterface solver;
  /// load the problem into the solver
  solver.loadProblem(
    matrix,
    colLower,
    colUpper,
    obj,
    rowLower,
    rowUpper);
  /// set integer variables
  for(int i = 0; i < (*nIntegerIndices); i++) {
    solver.setInteger(integerIndices[i]);
  }
  /// set model sense
  if ((*isMaximization) == 1) {
    solver.setObjSense(-1);
  }

  /// create model
  /// initalize model
  CbcModel model(solver);
  CbcMain0(model);

  // generate solution
  CbcMain1(*nArgs, argsList, model);

  // store outputs
  /// solution
  const double *solverSolution = model.solver()->getColSolution();
  for (int i = 0; i < (*nCols); i++) {
    column_solution[i] = solverSolution[i];
  }

  /// objective value
  *objective_value = model.solver()->getObjValue();
  /// status information
  *is_proven_optimal =
    static_cast<int>(model.isProvenOptimal());
  *is_proven_dual_infeasible =
    static_cast<int>(model.isProvenDualInfeasible());
  *is_proven_infeasible =
    static_cast<int>(model.isProvenInfeasible());
  *is_node_limit_reached =
    static_cast<int>(model.isNodeLimitReached());
  *is_solution_limit_reached =
    static_cast<int>(model.isSolutionLimitReached());
  *is_abandoned =
    static_cast<int>(model.isSolutionLimitReached());
  *is_iteration_limit_reached =
    static_cast<int>(model.isAbandoned());
  const int isIterationLimitReached =
    model.solver()->isIterationLimitReached();
  *is_seconds_limit_reached =
    static_cast<int>(model.isSecondsLimitReached());

  // return void
  return;
}

}
