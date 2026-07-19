#!/usr/bin/env Rscript

# Portable wrapper for the comprehensive analysis battery. The original and
# neutral-ticker runs share the staggered design expected by that script.
# Rerun A is factorial and must instead be analyzed by reproduce_results.R.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) stop("Run this file with Rscript.")

script_path <- normalizePath(sub("^--file=", "", script_arg[1]), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
args <- commandArgs(trailingOnly = TRUE)
run_name <- if (length(args)) args[1] else "original_run"

allowed_runs <- c("original_run", "rerun_B")
if (!run_name %in% allowed_runs) {
  stop("run_name must be one of: ", paste(allowed_runs, collapse = ", "),
       ". Use R/reproduce_results.R 2 for factorial Rerun A.")
}

data_root <- Sys.getenv("ADT_DATA_ROOT", file.path(repo_root, "data"))
csv_file <- normalizePath(
  file.path(data_root, run_name, "experiment_results_final.csv"),
  mustWork = TRUE
)
output_dir <- file.path(repo_root, "output", "full_analysis", run_name)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

analysis_script <- file.path(
  repo_root, "R", "experiment",
  "Attention_driven_trading_Statistical_Analysis_R1.R"
)

old_wd <- getwd()
old_csv <- Sys.getenv("ADT_CSV_FILE", unset = NA_character_)
Sys.setenv(ADT_CSV_FILE = csv_file)
setwd(output_dir)
tryCatch({
  cat("Running full analysis for ", run_name, "\n", sep = "")
  cat("Input:  ", csv_file, "\n", sep = "")
  cat("Output: ", normalizePath(output_dir), "\n", sep = "")
  source(analysis_script, chdir = FALSE)
}, finally = {
  setwd(old_wd)
  if (is.na(old_csv)) {
    Sys.unsetenv("ADT_CSV_FILE")
  } else {
    Sys.setenv(ADT_CSV_FILE = old_csv)
  }
})
