#include <cpp11.hpp>
#include <CbcSolver.hpp>

using namespace cpp11;

[[cpp11::register]]
writable::list cpp_cbc_solve(doubles obj,
                   bool isMaximization,
                   integers rowIndices,
                   integers colIndices,
                   doubles elements,
                   integers integerIndices,
                   doubles colLower,
                   doubles colUpper,
                   doubles rowLower,
                   doubles rowUpper,
                   strings arguments,
                   integers initialIndex,
                   doubles initialSolution,
                   strings initialNames,
                   bool useInitialSolution) {

  // build the constraint matrix in column format
  const R_len_t nCols = obj.size();
  const R_len_t nElements = elements.size();
  CoinPackedMatrix matrix(true,
                          INTEGER(rowIndices.data()),
                          INTEGER(colIndices.data()),
                          REAL(elements.data()),
                          nElements);

  // load the problem into the solver
  OsiClpSolverInterface solver;
  solver.loadProblem(matrix,
                     REAL(colLower.data()),
                     REAL(colUpper.data()),
                     REAL(obj.data()),
                     REAL(rowLower.data()),
                     REAL(rowUpper.data()));

  // set integer variables
  for(R_len_t i = 0; i < integerIndices.size(); i++) {
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
      solver.setColName(
        initialIndex[i],
        initialNames[i]
      );
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
          initialNames[i],
          initialSolution[i]
        )
      );
    }
    /// specify initial values
    model.setMIPStart(initialSolution_data);
  }

  // specify model arguments
  const int nArgs = arguments.size();
  std::vector<const char *> argList(nArgs);
  for (int i = 0; i < nArgs; i++) {
    argList[i] = CHAR(arguments[i]);
  }

  // set up model
  CbcMain1(nArgs, argList.data(), model);

  // generate solution
  writable::doubles solution(nCols);
  const double *solverSolution = model.solver()->getColSolution();
  for (int i = 0; i < nCols; i++) {
    solution[i] = solverSolution[i];
  }

  // extract outputs
  const bool isIterationLimitReached =
    model.solver()->isIterationLimitReached();
  const double objValue = model.solver()->getObjValue();

  // return results
  using namespace cpp11::literals;
  return list({
    "column_solution"_nm = solution,
    "objective_value"_nm = objValue,
    "is_proven_optimal"_nm = model.isProvenOptimal(),
    "is_proven_dual_infeasible"_nm = model.isProvenDualInfeasible(),
    "is_proven_infeasible"_nm = model.isProvenInfeasible(),
    "is_node_limit_reached"_nm = model.isNodeLimitReached(),
    "is_solution_limit_reached"_nm = model.isSolutionLimitReached(),
    "is_abandoned"_nm = model.isAbandoned(),
    "is_iteration_limit_reached"_nm = isIterationLimitReached,
    "is_seconds_limit_reached"_nm = model.isSecondsLimitReached()
  });
}
