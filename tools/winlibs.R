if(getRversion() < "3.3.0") {
  stop("R version too old. On Windows this package requires at least R-3.3")
}

# Download cbc-2.10.5 from rwinlib
if(!file.exists("../windows/cbc-2.10.5/include/coin/CbcSolver.hpp")){
  download.file(
    "https://github.com/rwinlib/cbc/archive/v2.10.5.zip", "lib.zip",
    quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
