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

  const int nArgs =  arguments.length();
  const char * argList[nArgs];
  for (int i = 0; i < arguments.length(); i++) {
    argList[i] = arguments(i).begin();
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
  } else if (isUnbounded) {
    status = "unbounded";
  } else if (isInfeasible) {
    status = "infeasible";
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
