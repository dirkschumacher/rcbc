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
                   CharacterVector arguments,
                   IntegerVector initialIndex,
                   NumericVector initialSolution,
                   CharacterVector initialNames,
                   bool useInitialSolution) {

  // build the constraint matrix in column format
  const R_len_t nCols = obj.length();
  const R_len_t nElements = elements.length();
  CoinPackedMatrix matrix(true,
                          rowIndices.begin(),
                          colIndices.begin(),
                          elements.begin(),
                          nElements);

  // load the problem into the solver
  OsiClpSolverInterface solver;
  solver.loadProblem(matrix,
                     colLower.begin(),
                     colUpper.begin(),
                     obj.begin(),
                     rowLower.begin(),
                     rowUpper.begin());

  // set integer variables
  for(R_len_t i = 0; i < integerIndices.length(); i++) {
    solver.setInteger(integerIndices[i]);
  }

  // set model sense
  if (isMaximization) {
    solver.setObjSense(-1);
  }

  // set variable needs if needed
  // note: we only need these if using a starting solution
  if (useInitialSolution) {
    for (R_len_t i = 0; i < initialIndex.size(); ++i) {
      solver.setColName(initialIndex[i],
                        Rcpp::as<std::string>(initialNames[i]));
    }
  }

  // create model
  CbcModel model(solver);
  CbcMain0(model);

  // set initial solution if specified
  /// declare variable to specify initial solution
  std::vector< std::pair<std::string,double> > initialSolution_data;
  if (useInitialSolution) {
    /// pre-allocate memory for variable
    initialSolution_data.reserve(initialIndex.size());
    /// append pairs to store initial solution information
    for (R_len_t i = 0; i < initialIndex.size(); ++i) {
      initialSolution_data.push_back(
        std::pair<std::string,double>(
            Rcpp::as<std::string>(initialNames[i]),
            initialSolution[i]
        )
      );
    }
    /// specify initial values
    model.setMIPStart(initialSolution_data);
  }

  // specify model arguments
  const int nArgs =  arguments.length();
  std::vector<const char *> argList(nArgs);
  for (int i = 0; i < nArgs; i++) {
    argList[i] = arguments(i).begin();
  }

  // set up model
  CbcMain1(nArgs, argList.data(), model);

  // generate solution
  NumericVector solution(nCols);
  const double *solverSolution = model.solver()->getColSolution();
  for(int i = 0; i < nCols; i++) {
    solution[i] = solverSolution[i];
  }

  // extract outputs
  const bool isIterationLimitReached =
    model.solver()->isIterationLimitReached();
  const double objValue = model.solver()->getObjValue();

  // return results
  return List::create(
    Named("column_solution", solution),
    Named("objective_value", objValue),
    Named("is_proven_optimal", model.isProvenOptimal()),
    Named("is_proven_dual_infeasible", model.isProvenDualInfeasible()),
    Named("is_proven_infeasible", model.isProvenInfeasible()),
    Named("is_node_limit_reached", model.isNodeLimitReached()),
    Named("is_solution_limit_reached", model.isSolutionLimitReached()),
    Named("is_abandoned", model.isAbandoned()),
    Named("is_iteration_limit_reached", isIterationLimitReached),
    Named("is_seconds_limit_reached", model.isSecondsLimitReached())
  );
}

/*** R
a <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 1), ncol = 3, nrow = 3)
b <- methods::as(a, "TsparseMatrix")
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
