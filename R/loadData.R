#' Load Sample Data
#'
#' This function loads the sample data from the package's extdata directory.
#'
#' @import readr
#' @export
#'
loadData <- function() {
  data_file <- system.file("extdata", "temp.csv", package = "coriM")
  if (file.exists(data_file)) {
    readr::read_csv(data_file)
  } else {
    stop("Data file not found.")
  }
}
