initialize_project_root <- function(script_name = "run_analysis.R") {
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package 'here' is required to run this analysis.", call. = FALSE)
  }

  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("^--file=", args, value = TRUE)

  # When launched with Rscript from another directory, anchor `here` to this file.
  if (length(script_arg) > 0) {
    script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[1])))
    original_wd <- getwd()
    on.exit(setwd(original_wd), add = TRUE)
    setwd(script_dir)
    suppressMessages(here::i_am(script_name))
    return(invisible())
  }

  # For interactive runs, fall back to the current directory if the script is
  # present.
  if (file.exists(script_name)) {
    suppressMessages(here::i_am(script_name))
    return(invisible())
  }

  stop(
    "Could not determine the project root for run_analysis.R. ",
    "Run the script with Rscript or from the project directory.",
    call. = FALSE
  )
}

initialize_project_root()

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(ggplot2)
  library(sf)
  library(rnaturalearth)
})

source(here::here("R", "utils.R"))
source(here::here("R", "io.R"))
source(here::here("R", "species.R"))
source(here::here("R", "lifestage.R"))
source(here::here("R", "location.R"))
source(here::here("R", "plotting.R"))

ensure_directories(project_path(c("data-derived", "figures")))

analysis_scripts <- c(
  "scripts/01_import_merge.R",
  "scripts/02_filter_included.R",
  "scripts/03_standardize_species.R",
  "scripts/04_standardize_lifestage.R",
  "scripts/05_standardize_location.R",
  "scripts/06_summarize_plot.R",
  "scripts/07_review_process_figure.R"
)

for (script in analysis_scripts) {
  message("Running ", script)
  source(project_path(script), local = globalenv())
}

message("Analysis complete.")
