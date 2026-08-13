# Headless reproduction of the dual-stable zero-flow detection.
#
# Runs the same call the Shiny app makes on the "Define Zero-Flow Changepoints"
# page, without Shiny, waiter, plotly or a browser, reading the same example
# files the app's "Load Example Data" buttons read. Any machine with sapfluxr
# installed can run it and the output is directly comparable:
#
#   Rscript inst/diagnostics/repro_dual_stable.R
#
# It is also safe to source() from an R console -- every interactive prompt is
# suppressed, so it cannot stop halfway waiting for input nobody is watching.
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
cat("timezone  : ", Sys.timezone(), " (UTC", format(Sys.time(), "%z"), ")\n\n", sep = "")

suppressPackageStartupMessages(library(sapfluxr))

step <- function(label, expr) {
  cat("--> ", label, " ... ", sep = "")
  t0 <- Sys.time()
  out <- force(expr)
  cat(sprintf("done (%.2fs)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  out
}

# Read the shipped files, exactly as the app's example-data buttons do
# (mod_1_data_upload.R and mod_util_weather_upload.R:304). Do NOT substitute the
# example_heat_pulse / example_weather objects: those are stored .rda copies
# carrying a different timezone tag, so they do not exercise the app's path.
hp_path <- system.file("extdata", "Sample_HeatPulse_Data.txt", package = "sapfluxr")
wx_path <- system.file("extdata", "Sample_Meteorological_Data.txt", package = "sapfluxr")

hp <- step("read_heat_pulse_data", {
  suppressMessages(read_heat_pulse_data(hp_path, confirm = FALSE))
})

# confirm_parameters defaults to TRUE and prompts whenever interactive() is
# TRUE, which is the case when this file is source()d from a console.
vh <- step("calc_heat_pulse_velocity", {
  suppressMessages(calc_heat_pulse_velocity(
    hp, methods = c("HRM", "MHR"), confirm_parameters = FALSE
  ))
})

weather_vpd <- step("calc_vpd", {
  suppressMessages(calc_vpd(read_weather_data(wx_path, confirm = FALSE)))
})

# The app prefers the corrected velocity column when present; on raw example
# data only Vh_cm_hr exists.
vh_col <- if ("Vs_cm_hr" %in% names(vh)) "Vs_cm_hr" else "Vh_cm_hr"
cat("    vh rows: ", nrow(vh), " | vh_col: ", vh_col, "\n", sep = "")
cat("    weather rows: ", nrow(weather_vpd), "\n", sep = "")
cat("    vh datetime tz: ", format(attr(vh$datetime, "tzone")),
    " | weather datetime tz: ", format(attr(weather_vpd$datetime, "tzone")),
    "\n\n", sep = "")

# Mirrors mod_5a_corrections_spacing.R:1454-1472 with the panel defaults from
# mod_5a_corrections_spacing.R:92, 101, 115, 124 and 211.
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
    vpd_threshold    = 0.5,
    vpd_stability    = 0.1,
    vh_threshold     = 2.0,
    vh_stability     = 0.5,
    min_n_points     = 4,
    min_segment_days = 7,
    max_changepoints = NULL
  )
})

vpd_dates <- res$vpd_results$valid_dates
vh_dates  <- res$vh_results$valid_dates

cat("\n=== RESULT ===\n")
cat("VPD-stable dates  : ", length(vpd_dates), "  [",
    paste(utils::head(format(vpd_dates), 4), collapse = " "), " ...]\n", sep = "")
cat("vh-stable dates   : ", length(vh_dates), "  [",
    paste(utils::head(format(vh_dates), 4), collapse = " "), " ...]\n", sep = "")
cat("overlap           : ", sum(vpd_dates %in% vh_dates), "\n", sep = "")
cat("dual-stable dates : ", length(res$dual_stable_dates), "\n", sep = "")
cat("changepoints      : ", nrow(res$changepoints), "\n", sep = "")

# The class is reported because losing it is a real failure mode: intersect()
# stripped it before R 4.5.0, after which the date lookup matched nothing and
# detection returned no changepoints without ever reporting an error.
cat("dual-stable class : ", paste(class(res$dual_stable_dates), collapse = "/"),
    " (expected: Date)\n", sep = "")

cat("\nExpected on the shipped example data: overlap 8, dual-stable 2.\n")
cat("COMPLETED WITHOUT STALLING\n")
