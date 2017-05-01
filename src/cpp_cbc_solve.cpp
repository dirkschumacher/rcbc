#include <Rcpp.h>
#include <CbcSolver.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
List cpp_cbc_solve(NumericVector obj,
                      bool isMaximization,
                      IntegerVector rowIndices,
                      IntegerVector colIndices,
                      NumericVector elements,
                      IntegerVector integerIndices,
                      NumericVector colLower,
                      NumericVector colUpper,
                      NumericVector rowLower,
                      NumericVector rowUpper,
                      CharacterVector arguments) {

  // build the constraint matrix in column format
  const int nCols = obj.length();
  const int nElements = elements.length();
  CoinPackedMatrix matrix(true,
                          rowIndices.begin(),
                          colIndices.begin(),
                          elements.begin(), nElements);

  // load the problem into the solver
  OsiClpSolverInterface solver;
  solver.loadProblem(matrix,
                     colLower.begin(),
                     colUpper.begin(),
                     obj.begin(),
                     rowLower.begin(),
                     rowUpper.begin());

  // set integer variables
  for(int i = 0; i < integerIndices.length(); i++) {
    solver.setInteger(integerIndices[i]);
  }
  if (isMaximization) {
    solver.setObjSense(-1);
  }
  CbcModel model(solver);
  CbcMain0(model);

  CharacterVector argNames = arguments.names();
  int nNamedArgs = 0;
  for (int i = 0; i < argNames.length(); i++) {
    if (argNames[i].size() > 0) {
      nNamedArgs++;
    }
  }

  int nextFreePos = 2;
  const int nArgs = nextFreePos + arguments.length() + nNamedArgs;
  const char * argList[nArgs];
  argList[0] = "-quit";
  argList[1] = "-solve";
  for (int i = 0; i < arguments.length(); i++) {
    if (argNames[i].size() == 0) {
      argList[nextFreePos] = arguments[i].begin();
      nextFreePos++;
    } else {
      argList[nextFreePos] = argNames[i].begin();
      argList[nextFreePos + 1] = arguments[i].begin();
      nextFreePos = nextFreePos + 2;
    }
  }
  CbcMain1(nArgs, argList, model);
  NumericVector solution(nCols);

  const double *solverSolution = model.solver()->getColSolution();
  for(int i = 0; i < nCols; i++) {
    solution[i] = solverSolution[i];
  }
  std::string status = "unknown";
  bool isOptimal = model.isProvenOptimal();
  bool isInfeasible = model.isProvenInfeasible();
  bool isUnbounded = model.isProvenDualInfeasible();
  bool isNodeLimitedReached = model.isNodeLimitReached();
  bool isSolutionLimitReached = model.isSolutionLimitReached();
  bool isIterationLimitReached = model.solver()->isIterationLimitReached();
  bool isAbandoned = model.isAbandoned();
  if (isOptimal) {
    status = "optimal";
  } else if (isInfeasible) {
    status = "infeasible";
  } else if (isUnbounded) {
    status = "unbounded";
  } else if (isNodeLimitedReached) {
    status = "nodelimit";
  } else if (isSolutionLimitReached) {
    status = "solutionlimit";
  } else if (isAbandoned) {
    status = "abandoned";
  } else if (isIterationLimitReached) {
    status = "iterationlimit";
  }
  const double objValue = model.solver()->getObjValue();
  return List::create(
    Named("column_solution", solution),
    Named("status", status),
    Named("objective_value", objValue),
    Named("is_proven_optimal", isOptimal),
    Named("is_proven_infeasible", isInfeasible),
    Named("is_proven_dual_infeasible", isUnbounded),
    Named("is_node_limit_reached", isNodeLimitedReached),
    Named("is_solution_limit_reached", isSolutionLimitReached),
    Named("is_abandoned", isAbandoned),
    Named("is_iteration_limit_reached", isIterationLimitReached));
}

/*** R
a <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), ncol = 3, nrow = 3)
b <- methods::as(Matrix::Matrix(a), "TsparseMatrix")
cpp_cbc_solve(c(1, 2, 3),
              TRUE,
              b@i,
              b@j,
              b@x,
              c(1, 2, 3),
              c(0, 0, 0),
              c(1, 1, 1),
              c(0, 0, 0),
              c(1, 1, 1),
              c("-logLevel" = "1"))
*/
