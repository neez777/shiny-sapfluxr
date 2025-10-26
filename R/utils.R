#' Utility Functions for Shiny Sap Flow App
#'
#' Helper functions used across modules
#'

#' Get formatted date range text from measurements
#'
#' @param measurements Tibble with datetime column
#' @return Character string with formatted date range
#' @keywords internal
get_date_range_text <- function(measurements) {
  if (is.null(measurements) || nrow(measurements) == 0) {
    return("No data")
  }

  date_range <- range(measurements$datetime, na.rm = TRUE)
  paste(
    format(date_range[1], "%Y-%m-%d %H:%M"),
    "to",
    format(date_range[2], "%Y-%m-%d %H:%M")
  )
}

#' Get formatted duration text from measurements
#'
#' @param measurements Tibble with datetime column
#' @return Character string with formatted duration
#' @keywords internal
get_duration_text <- function(measurements) {
  if (is.null(measurements) || nrow(measurements) == 0) {
    return("No data")
  }

  date_range <- range(measurements$datetime, na.rm = TRUE)
  duration_days <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))

  if (duration_days < 1) {
    # Less than 1 day - show hours
    duration_hours <- round(duration_days * 24, 1)
    paste(duration_hours, "hours")
  } else if (duration_days < 7) {
    # Less than 1 week - show days
    paste(round(duration_days, 1), "days")
  } else if (duration_days < 30) {
    # Less than 1 month - show days and weeks
    weeks <- floor(duration_days / 7)
    days <- round(duration_days %% 7, 0)
    paste(weeks, "weeks,", days, "days")
  } else {
    # More than 1 month - show months and days
    months <- floor(duration_days / 30)
    days <- round(duration_days %% 30, 0)
    paste(months, "months,", days, "days")
  }
}

#' Format large numbers with thousands separator
#'
#' @param x Numeric value
#' @param digits Number of decimal places
#' @return Formatted character string
#' @keywords internal
format_number <- function(x, digits = 0) {
  format(round(x, digits), big.mark = ",", scientific = FALSE)
}

#' Get quality flag colour
#'
#' @param flag Quality flag ("OK", "WARNING", "ERROR", "SUSPECT")
#' @return Colour code (hex)
#' @keywords internal
get_quality_colour <- function(flag) {
  switch(
    toupper(flag),
    "OK" = "#4CAF50",
    "WARNING" = "#FF9800",
    "ERROR" = "#F44336",
    "SUSPECT" = "#9C27B0",
    "#666666"  # Default grey
  )
}

#' Get quality flag icon
#'
#' @param flag Quality flag
#' @return Icon name for shiny::icon()
#' @keywords internal
get_quality_icon <- function(flag) {
  switch(
    toupper(flag),
    "OK" = "check-circle",
    "WARNING" = "exclamation-triangle",
    "ERROR" = "times-circle",
    "SUSPECT" = "question-circle",
    "circle"  # Default
  )
}

#' Create a collapsible help section
#'
#' @param title Section title
#' @param content HTML content
#' @return Shiny UI element
#' @keywords internal
help_section <- function(title, content) {
  div(
    class = "help-section",
    shinyWidgets::panel(
      heading = tagList(icon("info-circle"), title),
      status = "default",
      content
    )
  )
}

#' Validate datetime input
#'
#' @param datetime POSIXct datetime
#' @return Logical - TRUE if valid
#' @keywords internal
is_valid_datetime <- function(datetime) {
  !is.null(datetime) && inherits(datetime, "POSIXct") && !is.na(datetime)
}

#' Create download button with custom styling
#'
#' @param outputId Output ID for download handler
#' @param label Button label
#' @param icon Icon name
#' @param class Additional CSS class
#' @return Shiny download button
#' @keywords internal
styled_download_button <- function(outputId, label, icon = "download", class = "btn-primary") {
  downloadButton(
    outputId,
    label,
    icon = icon(icon),
    class = class,
    style = "margin: 5px;"
  )
}

#' Extract method names from vh_results
#'
#' @param vh_results Results tibble from calc_heat_pulse_velocity
#' @return Character vector of unique method names
#' @keywords internal
get_methods_from_results <- function(vh_results) {
  if (is.null(vh_results) || nrow(vh_results) == 0) {
    return(character(0))
  }

  unique(vh_results$method)
}

#' Create method colour palette
#'
#' @param methods Character vector of method names
#' @return Named vector of colours
#' @keywords internal
get_method_colours <- function(methods) {
  # Define colour palette for methods
  colour_palette <- c(
    "HRM" = "#1f77b4",      # Blue
    "MHR" = "#ff7f0e",      # Orange
    "HRMXa" = "#2ca02c",    # Green
    "HRMXb" = "#d62728",    # Red
    "Tmax_Coh" = "#9467bd", # Purple
    "Tmax_Klu" = "#8c564b", # Brown
    "sDMA" = "#e377c2",     # Pink
    "CHPM" = "#7f7f7f",     # Grey
    "DRM" = "#bcbd22"       # Yellow-green
  )

  # Return colours for specified methods
  # If method not in palette, assign from default ggplot2 colours
  method_colours <- sapply(methods, function(m) {
    if (m %in% names(colour_palette)) {
      colour_palette[[m]]
    } else {
      # Use ggplot2 default colours
      scales::hue_pal()(length(methods))[which(methods == m)]
    }
  })

  names(method_colours) <- methods
  return(method_colours)
}

#' Format Vh value for display
#'
#' @param vh Velocity value in cm/hr
#' @param digits Number of decimal places
#' @return Formatted string with units
#' @keywords internal
format_vh <- function(vh, digits = 2) {
  paste(format(round(vh, digits), nsmall = digits), "cm/hr")
}

#' Check if package is available
#'
#' @param pkg Package name
#' @return Logical
#' @keywords internal
package_available <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}
