# Environment report for shiny-sapfluxr
#
# Prints the R version, library paths and every package version the app needs,
# so a failing machine can be diffed against a working one.
#
# Run from the shiny-sapfluxr directory:
#   Rscript inst/diagnostics/env_report.R
#
# Deliberately written in plain, old-R-compatible code: no pipes, no lambda
# shorthand, no `%||%`. This script must run even on an environment too old or
# too broken to start the app.

cat("================ shiny-sapfluxr environment report ================\n\n")

cat("R version   : ", R.version.string, "\n", sep = "")
cat("Platform    : ", R.version$platform, "\n", sep = "")
cat("OS          : ", Sys.info()[["sysname"]], " ", Sys.info()[["release"]], "\n", sep = "")
cat("Run date    : ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n", sep = "")

cat("Library paths (packages are found in this order):\n")
libs <- .libPaths()
for (i in seq_along(libs)) {
  writable <- file.access(libs[i], mode = 2) == 0
  cat(sprintf("  %d. %s%s\n", i, libs[i],
              if (writable) "" else "   [not writable]"))
}
cat("\n")

# Keep this list in sync with the startup guard in app.R.
required <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "shinycssloaders",
  "fresh", "plotly", "DT", "waiter", "leaflet", "htmlwidgets", "dplyr",
  "tidyr", "purrr", "readr", "yaml", "lubridate", "ggplot2", "scales",
  "rlang", "progressr", "suncalc", "R6", "lutz", "zip", "sapfluxr"
)

cat("Package versions:\n")
missing <- character(0)
for (pkg in required) {
  ver <- tryCatch(as.character(utils::packageVersion(pkg)),
                  error = function(e) NA_character_)
  if (is.na(ver)) {
    missing <- c(missing, pkg)
    cat(sprintf("  %-16s MISSING\n", pkg))
  } else {
    loc <- tryCatch(dirname(find.package(pkg)), error = function(e) "?")
    cat(sprintf("  %-16s %-12s %s\n", pkg, ver, loc))
  }
}
cat("\n")

if (length(missing) > 0) {
  cat("Install the missing packages with:\n")
  cat("  install.packages(c(",
      paste0('"', setdiff(missing, "sapfluxr"), '"', collapse = ", "), "))\n",
      sep = "")
  if ("sapfluxr" %in% missing) {
    cat('  remotes::install_github("neez777/sapfluxr")\n')
  }
  cat("\n")
}

# `%||%` is used throughout the app but defined nowhere in it. It resolves via
# the attached purrr (or base R from 4.4.0). Confirm at least one source exists,
# because losing it produces a confusing "could not find function" at runtime.
cat("NULL-coalescing operator `%||%`:\n")
sources <- character(0)
if (getRversion() >= "4.4.0") sources <- c(sources, "base")
for (pkg in c("purrr", "rlang")) {
  ok <- tryCatch({
    "%||%" %in% getNamespaceExports(pkg)
  }, error = function(e) FALSE)
  if (isTRUE(ok)) sources <- c(sources, pkg)
}
if (length(sources) == 0) {
  cat("  NOT AVAILABLE from base, purrr or rlang -- the app will fail.\n\n")
} else {
  cat("  available from: ", paste(sources, collapse = ", "), "\n\n", sep = "")
}

# sapfluxr ships compiled code. A folder copied between machines can carry a
# stale .dll/.so that fails to load or, worse, loads and misbehaves.
cat("sapfluxr compiled code:\n")
loaded <- tryCatch({
  suppressPackageStartupMessages(library(sapfluxr, quietly = TRUE))
  TRUE
}, error = function(e) {
  cat("  FAILED to load sapfluxr: ", conditionMessage(e), "\n", sep = "")
  FALSE
})
if (isTRUE(loaded)) {
  dlls <- names(getLoadedDLLs())
  cat("  sapfluxr loaded OK; shared object present: ",
      "sapfluxr" %in% dlls, "\n", sep = "")
  cat("  installed from : ",
      tryCatch(dirname(find.package("sapfluxr")), error = function(e) "?"),
      "\n", sep = "")
}
cat("\n")

# Which revision of the app is actually being run. A machine running stale code
# is by far the most common cause of "it still does not work".
cat("App revision:\n")
if (dir.exists(".git")) {
  rev <- suppressWarnings(tryCatch(
    system2("git", c("rev-parse", "--short", "HEAD"),
            stdout = TRUE, stderr = TRUE),
    error = function(e) conditionMessage(e)
  ))
  failed <- !is.null(attr(rev, "status")) && attr(rev, "status") != 0
  if (length(rev) > 0 && !failed) {
    status <- suppressWarnings(tryCatch(
      system2("git", c("status", "--porcelain"), stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    ))
    cat("  git commit    : ", rev[1], "\n", sep = "")
    cat("  local changes : ", length(status), " file(s)\n", sep = "")
  } else {
    cat("  git could not report the revision:\n")
    for (line in rev) cat("    ", line, "\n", sep = "")
  }
} else {
  cat("  no .git directory -- this is a copied folder, not a clone,\n")
  cat("  so its revision cannot be determined.\n")
}
if (file.exists("DESCRIPTION")) {
  dcf <- read.dcf("DESCRIPTION", fields = "Version")
  cat("  DESCRIPTION   : version ", as.character(dcf[1, 1]), "\n", sep = "")
} else {
  cat("  DESCRIPTION not found -- run this from the shiny-sapfluxr directory.\n")
}

cat("\n=================== end of environment report ====================\n")
