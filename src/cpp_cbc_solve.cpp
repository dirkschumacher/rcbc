#include <CbcSolver.hpp>
#include <Rinternals.h>

extern "C" {

SEXP rcbc_cpp_cbc_solve(SEXP obj,
                   SEXP isMaximization,
                   SEXP rowIndices,
                   SEXP colIndices,
                   SEXP elements,
                   SEXP integerIndices,
                   SEXP colLower,
                   SEXP colUpper,
                   SEXP rowLower,
                   SEXP rowUpper,
                   SEXP arguments,
                   SEXP initialIndex,
                   SEXP initialSolution,
                   SEXP initialNames,
                   SEXP useInitialSolution) {

  // build the constraint matrix in column format
  const R_len_t nCols = length(obj);
  const R_len_t nElements = length(elements);
  CoinPackedMatrix matrix(true,
                          INTEGER(rowIndices),
                          INTEGER(colIndices),
                          REAL(elements),
                          nElements);

  // load the problem into the solver
  OsiClpSolverInterface solver;
  solver.loadProblem(matrix,
                     REAL(colLower),
                     REAL(colUpper),
                     REAL(obj),
                     REAL(rowLower),
                     REAL(rowUpper));

  // set integer variables
  for (R_len_t i = 0; i < length(integerIndices); i++) {
    solver.setInteger(INTEGER(integerIndices)[i]);
  }

  // set model sense
  if (asLogical(isMaximization)) {
    solver.setObjSense(-1);
  }

  // create model
  CbcModel model(solver);
  CbcMain0(model);

  // set initial solution if specified
  /// declare variable to specify initial solution
  std::vector< std::pair<std::string,double> > initialSolution_data;
  if (asLogical(useInitialSolution)) {
    const int n = length(initialIndex);
    /// set variable names
    for (R_len_t i = 0; i < n; ++i) {
      solver.setColName(
        INTEGER(initialIndex)[i],
        CHAR(STRING_ELT(initialNames, i))
      );
    }

    /// pre-allocate memory for variable
    initialSolution_data.reserve(n);
    /// append pairs to store initial solution information
    for (R_len_t i = 0; i < n; ++i) {
      initialSolution_data.push_back(
        std::pair<std::string,double>(
          std::string(CHAR(STRING_ELT(initialNames, i))),
          REAL(initialSolution)[i]
        )
      );
    }
    /// specify initial values
    model.setMIPStart(initialSolution_data);
  }

  // specify model arguments
  const int nArgs = length(arguments);
  std::vector<const char *> argList(nArgs);
  for (int i = 0; i < nArgs; i++) {
    argList[i] = CHAR(STRING_ELT(arguments, i));
  }

  // set up model
  CbcMain1(nArgs, argList.data(), model);

  // generate solution
  SEXP solution = PROTECT(allocVector(REALSXP, nCols));
  const double * solverSolution = model.solver()->getColSolution();
  for (int i = 0; i < nCols; i++) {
    REAL(solution)[i] = solverSolution[i];
  }

  // extract outputs
  const bool isIterationLimitReached =
    model.solver()->isIterationLimitReached();
  const double objValue = model.solver()->getObjValue();

  // return results
  SEXP result_list = PROTECT(allocVector(VECSXP, 10));
  SET_VECTOR_ELT(result_list, 0, solution);
  SET_VECTOR_ELT(result_list, 1, PROTECT(ScalarReal(objValue)));
  SET_VECTOR_ELT(result_list, 2, PROTECT(ScalarLogical(model.isProvenOptimal())));
  SET_VECTOR_ELT(result_list, 3, PROTECT(ScalarLogical(model.isProvenDualInfeasible())));
  SET_VECTOR_ELT(result_list, 4, PROTECT(ScalarLogical(model.isProvenInfeasible())));
  SET_VECTOR_ELT(result_list, 5, PROTECT(ScalarLogical(model.isNodeLimitReached())));
  SET_VECTOR_ELT(result_list, 6, PROTECT(ScalarLogical(model.isSolutionLimitReached())));
  SET_VECTOR_ELT(result_list, 7, PROTECT(ScalarLogical(model.isAbandoned())));
  SET_VECTOR_ELT(result_list, 8, PROTECT(ScalarLogical(isIterationLimitReached)));
  SET_VECTOR_ELT(result_list, 9, PROTECT(ScalarLogical(model.isSecondsLimitReached())));

  SEXP result_list_names = PROTECT(allocVector(STRSXP, 10));
  SET_STRING_ELT(result_list_names, 0, Rf_mkChar("column_solution"));
  SET_STRING_ELT(result_list_names, 1, Rf_mkChar("objective_value"));
  SET_STRING_ELT(result_list_names, 2, Rf_mkChar("is_proven_optimal"));
  SET_STRING_ELT(result_list_names, 3, Rf_mkChar("is_proven_dual_infeasible"));
  SET_STRING_ELT(result_list_names, 4, Rf_mkChar("is_proven_infeasible"));
  SET_STRING_ELT(result_list_names, 5, Rf_mkChar("is_node_limit_reached"));
  SET_STRING_ELT(result_list_names, 6, Rf_mkChar("is_solution_limit_reached"));
  SET_STRING_ELT(result_list_names, 7, Rf_mkChar("is_abandoned"));
  SET_STRING_ELT(result_list_names, 8, Rf_mkChar("is_iteration_limit_reached"));
  SET_STRING_ELT(result_list_names, 9, Rf_mkChar("is_seconds_limit_reached"));
  setAttrib(result_list, R_NamesSymbol, result_list_names);
  UNPROTECT(
    3 /* solution, list, list_names */ + 9 /* scalar values */
  );
  return result_list;
}

}
