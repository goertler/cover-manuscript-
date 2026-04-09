suppressPackageStartupMessages(library(testthat))

testthat::test_dir(
  "tests/testthat",
  reporter = testthat::SummaryReporter$new()
)
