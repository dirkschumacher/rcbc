#include <Rcpp.h>
#include <cbc/coin/CbcModel.hpp>
#include <cbc/coin/CbcSolver.hpp>
#include <clp/coin/OsiClpSolverInterface.hpp>
#include <coinutils/coin/CoinPackedMatrix.hpp>

using namespace Rcpp;

// [[Rcpp::export]]
List cpp_cbc_solve(NumericVector obj,
                      bool isMaximization,
                      IntegerVector rowIndices,
                      IntegerVector colIndices,
                      NumericVector elements,
                      IntegerVector integerIndexes,
                      NumericVector colLower,
                      NumericVector colUpper,
                      NumericVector rowLower,
                      NumericVector rowUpper,
                      CharacterVector arguments) {

  // build the constraint matrix in column format
  const int nCols = obj.length();
  const int nElements = elements.size();
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
  for(int i = 0; i < integerIndexes.length(); i++) {
    solver.setInteger(integerIndexes[i]);
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
              c(1, 2, 3),
              c(0, 0, 0),
              c(1, 1, 1),
              c(0, 0, 0),
              c(1, 1, 1),
              c("-logLevel" = "1"))
*/
