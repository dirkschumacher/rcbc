#include <Rcpp.h>
#include <cbc/coin/CbcModel.hpp>
#include <clp/coin/OsiClpSolverInterface.hpp>
#include <coinutils/coin/CoinPackedMatrix.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
List cpp_cbc_solve(NumericVector obj,
                      bool isMaximization,
                      IntegerVector rowIndices,
                      IntegerVector colIndices,
                      NumericVector elements,
                      LogicalVector isInteger,
                      NumericVector colLower,
                      NumericVector colUpper,
                      NumericVector rowLower,
                      NumericVector rowUpper,
                      int logLevel) {

  // build the constraint matrix in column format
  int nCols = obj.length();
  int nElements = elements.size();
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
  for(int i = 0; i < isInteger.length(); i++) {
    if (isInteger[i]) {
      solver.setInteger(i);
    }
  }
  if (isMaximization) {
    solver.setObjSense(-1);
  }
  solver.setLogLevel(logLevel);
  solver.initialSolve();

  CbcModel model(solver);
  model.setLogLevel(logLevel);
  model.branchAndBound();
  NumericVector solution(nCols);
  const double *solverSolution = model.solver()->getColSolution();
  for(int i = 0; i < nCols; i++) {
    solution[i] = solverSolution[i];
  }
  std::string status = "infeasible";
  if (model.solver()->isProvenOptimal()) {
    status = "optimal";
  }
  if (model.solver()->isProvenDualInfeasible()) {
    status = "unbounded";
  }
  const double objValue = model.solver()->getObjValue();
  return List::create(
    Named("column_solution", solution),
    Named("status", status),
    Named("objective_value", objValue));
}

/*** R
a <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), ncol = 3, nrow = 3)
b <- as(Matrix::Matrix(a), "TsparseMatrix")
cpp_cbc_solve(c(1, 2, 3),
              TRUE,
              b@i,
              b@j,
              b@x,
              c(TRUE, TRUE, TRUE),
              c(0, 0, 0),
              c(1, 1, 1),
              c(0, 0, 0),
              c(1, 1, 1), 1)
*/
