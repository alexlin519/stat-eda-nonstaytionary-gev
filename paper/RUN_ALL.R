## RUN_ALL.R -------------------------------------------------------------------
## THE ONE BUTTON. Put all downloaded files in one folder and run THIS file.
##
##   Easiest way:  open RUN_ALL.R in RStudio and click "Source"
##                 (button at the top right of the editor pane).
##   Command line: Rscript RUN_ALL.R
##
## What it does, in order:
##   1. moves R into this file's folder automatically (no setwd needed)
##   2. checks the other files are present
##   3. installs quantreg once if missing (if the install fails, a built-in
##      fallback is used and the run still completes)
##   4. creates fake_daily_flows.csv if it is not there yet
##   5. runs the full pipeline (run_pipeline.R)
##
## What comes out (all in this same folder):
##   pipeline_output.txt   everything that was printed on screen
##   results_tables/       each results table as a CSV file
##   pipeline_results.rds  every fitted object, for the later evaluation stage
##
## Real data later: edit DATA_CSV and VAR_DEF at the top of run_pipeline.R.
## Nothing in this file changes.

## 1 -- find this file's folder and move there ---------------------------------
get_script_dir <- function() {
  fa <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(fa) > 0) return(dirname(normalizePath(fa[1])))
  for (fr in rev(sys.frames()))
    if (!is.null(fr$ofile)) return(dirname(normalizePath(fr$ofile)))
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path,
                  error = function(e) "")
    if (is.character(p) && nzchar(p)) return(dirname(normalizePath(p)))
  }
  getwd()
}
setwd(get_script_dir())
cat("working folder:", getwd(), "\n")

if (!file.exists("extremes_functions.R") || !file.exists("run_pipeline.R"))
  stop("Cannot find extremes_functions.R / run_pipeline.R in this folder.\n",
       "  Put ALL downloaded files in ONE folder and run RUN_ALL.R from there.\n",
       "  (In a plain R console: setwd(\"path/to/that/folder\"), ",
       "then source(\"RUN_ALL.R\").)")

## 2 -- one-time package check -------------------------------------------------
if (!requireNamespace("quantreg", quietly = TRUE)) {
  message("quantreg not found -- trying a one-time install ...")
  tryCatch(install.packages("quantreg", repos = "https://cloud.r-project.org"),
           error = function(e)
             message("install failed (offline?) -- continuing with the ",
                     "built-in fallback; results still come out."))
}

## 3 -- data ---------------------------------------------------------------------
if (!file.exists("fake_daily_flows.csv")) {
  cat("fake_daily_flows.csv not found -- generating it ...\n")
  source("make_fake_data.R", local = new.env())
}

## 4 -- full pipeline, with everything logged to pipeline_output.txt -------------
while (sink.number() > 0) sink()          # defensive: close any stale sinks
sink("pipeline_output.txt", split = TRUE)
t0 <- Sys.time()
ok <- tryCatch({ source("run_pipeline.R", local = new.env()); TRUE },
               error = function(e) {
                 cat("\nERROR:", conditionMessage(e), "\n"); FALSE })
cat(sprintf("\ndone in %.1f minutes.\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
cat("outputs: pipeline_output.txt | results_tables/ | pipeline_results.rds\n")
sink()
if (!ok) stop("The pipeline stopped with the error shown above ",
              "(also saved in pipeline_output.txt).")
