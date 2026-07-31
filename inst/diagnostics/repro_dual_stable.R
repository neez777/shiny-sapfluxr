# Headless reproduction of the dual-stable zero-flow detection.
#
# Runs the same call the Shiny app makes on the "Define Zero-Flow Changepoints"
# page, without Shiny, waiter, plotly or a browser, using only the example data
# shipped with sapfluxr. Any machine with sapfluxr installed can run it and the
# output is directly comparable:
#
#   Rscript inst/diagnostics/repro_dual_stable.R
#
# If this completes here but the app stalls, the fault is in the Shiny layer.
# If this stalls or errors, the fault is in sapfluxr or its dependencies.

cat("R version : ", R.version.string, "\n", sep = "")
cat("platform  : ", R.version$platform, "\n", sep = "")
for (p in c("sapfluxr", "dplyr", "lubridate", "changepoint", "purrr", "rlang")) {
  v <- tryCatch(as.character(utils::packageVersion(p)),
                error = function(e) "*** MISSING ***")
  cat(sprintf("%-10s: %s\n", p, v))
}
built <- utils::packageDescription("sapfluxr")$Built
cat("sapfluxr built: ", if (is.null(built)) "?" else built, "\n", sep = "")
# Deliberately avoids `%||%`: it is only in base R from 4.4.0, and establishing
# whether this machine has it is part of what the script is checking.
cat("`%||%` in base: ", "%||%" %in% ls(baseenv()), "\n\n", sep = "")

suppressPackageStartupMessages(library(sapfluxr))

step <- function(label, expr) {
  cat("--> ", label, " ... ", sep = "")
  t0 <- Sys.time()
  out <- force(expr)
  cat(sprintf("done (%.2fs)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  out
}

data(example_heat_pulse, package = "sapfluxr")
data(example_weather, package = "sapfluxr")

vh <- step("calc_heat_pulse_velocity", {
  calc_heat_pulse_velocity(example_heat_pulse, methods = c("HRM", "MHR"))
})

weather_vpd <- step("calc_vpd", calc_vpd(example_weather))

# The app prefers the corrected velocity column when present; on raw example
# data only Vh_cm_hr exists.
vh_col <- if ("Vs_cm_hr" %in% names(vh)) "Vs_cm_hr" else "Vh_cm_hr"
cat("    vh rows: ", nrow(vh), " | vh_col: ", vh_col, "\n", sep = "")
cat("    weather rows: ", nrow(weather_vpd), "\n\n", sep = "")

# Mirrors mod_5a_corrections_spacing.R:1388-1406 with the panel defaults.
res <- step("find_dual_stable_periods (static)", {
  find_dual_stable_periods(
    vh_data          = vh,
    weather_data     = weather_vpd,
    vh_col           = vh_col,
    method           = "HRM",
    sensor_position  = "outer",
    vpd_col          = "vpd_kpa",
    predawn_window   = c(2, 6),
    mode             = "static",
    dawn_times       = NULL,
    timezone         = NULL,
    vpd_threshold    = 0.05,
    vpd_stability    = 0.02,
    vh_threshold     = 0.5,
    vh_stability     = 0.1,
    min_n_points     = 4,
    min_segment_days = 3,
    max_changepoints = NULL
  )
})

cat("\n=== RESULT ===\n")
cat("dual-stable dates : ", length(res$dual_stable_dates), "\n", sep = "")
cat("changepoints      : ", nrow(res$changepoints), "\n", sep = "")
cat("VPD-stable dates  : ", res$vpd_results$n_dates_selected, "\n", sep = "")
cat("vh-stable dates   : ", res$vh_results$n_dates_selected, "\n", sep = "")
cat("\nCOMPLETED WITHOUT STALLING\n")
