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

# Probe Visualization Functions ----

#' Validate Probe and Tree Configuration
#'
#' Checks if sensors are positioned within sapwood or heartwood
#'
#' @param probe_config ProbeConfiguration R6 object
#' @param wood_properties WoodProperties R6 object
#' @return List with validation results
#' @keywords internal
validate_probe_tree_config <- function(probe_config, wood_properties) {

  # Extract probe values from R6 objects
  if (inherits(probe_config, "ProbeConfiguration")) {
    # From sensor_positions (in cm, negative for upstream)
    upstream_dist <- abs(probe_config$sensor_positions$upstream_inner)
    downstream_dist <- abs(probe_config$sensor_positions$downstream_inner)

    # Probe dimensions from yaml_data (in mm)
    probe_diameter_mm <- probe_config$yaml_data$probe$diameter  # mm
    probe_length_mm <- probe_config$yaml_data$probe$length      # mm

    # Sensor positions from probe tip (in mm)
    inner_sensor_mm <- probe_config$yaml_data$probe$inner_sensor  # mm from tip
    outer_sensor_mm <- probe_config$yaml_data$probe$outer_sensor  # mm from tip

    # Convert to cm for visualization
    probe_diameter_cm <- probe_diameter_mm / 10
    probe_length_cm <- probe_length_mm / 10

    # Sensor depths from bark surface (probe length - distance from tip)
    outer_sensor_depth <- (probe_length_mm - outer_sensor_mm) / 10  # cm
    inner_sensor_depth <- (probe_length_mm - inner_sensor_mm) / 10  # cm

  } else {
    # Handle list structure from manual mode
    upstream_dist <- if (!is.null(probe_config$sensor_positions$upstream_inner)) {
      abs(probe_config$sensor_positions$upstream_inner)
    } else 0.5

    downstream_dist <- if (!is.null(probe_config$sensor_positions$downstream_inner)) {
      abs(probe_config$sensor_positions$downstream_inner)
    } else 0.5

    # Probe dimensions from yaml_data (in mm)
    probe_diameter_mm <- if (!is.null(probe_config$yaml_data$probe$diameter)) {
      probe_config$yaml_data$probe$diameter
    } else 1.27

    probe_length_mm <- if (!is.null(probe_config$yaml_data$probe$length)) {
      probe_config$yaml_data$probe$length
    } else 35

    # Sensor positions from probe tip (in mm)
    inner_sensor_mm <- if (!is.null(probe_config$yaml_data$probe$inner_sensor)) {
      probe_config$yaml_data$probe$inner_sensor
    } else 7.5

    outer_sensor_mm <- if (!is.null(probe_config$yaml_data$probe$outer_sensor)) {
      probe_config$yaml_data$probe$outer_sensor
    } else 22.5

    # Convert to cm for visualisation
    probe_diameter_cm <- probe_diameter_mm / 10
    probe_length_cm <- probe_length_mm / 10

    # Sensor depths from bark surface (probe length - distance from tip)
    outer_sensor_depth <- (probe_length_mm - outer_sensor_mm) / 10  # cm
    inner_sensor_depth <- (probe_length_mm - inner_sensor_mm) / 10  # cm
  }

  # Extract tree properties from R6 object
  if (inherits(wood_properties, "WoodProperties")) {
    # Tree measurements may be NULL - use defaults if so
    dbh <- wood_properties$tree_measurements$dbh
    bark_depth <- wood_properties$tree_measurements$bark_thickness
    sapwood_depth <- wood_properties$tree_measurements$sapwood_depth

    # Apply defaults for NULL values
    if (is.null(dbh) || is.na(dbh)) dbh <- 20
    if (is.null(bark_depth) || is.na(bark_depth)) bark_depth <- 0.5
    if (is.null(sapwood_depth) || is.na(sapwood_depth)) sapwood_depth <- 3.0

  } else {
    # Handle list structure from manual mode
    dbh <- if (!is.null(wood_properties$tree_measurements$dbh)) {
      wood_properties$tree_measurements$dbh
    } else 20

    bark_depth <- if (!is.null(wood_properties$tree_measurements$bark_thickness)) {
      wood_properties$tree_measurements$bark_thickness
    } else 0.5

    sapwood_depth <- if (!is.null(wood_properties$tree_measurements$sapwood_depth)) {
      wood_properties$tree_measurements$sapwood_depth
    } else 3.0
  }

  # Calculate radius from DBH
  radius <- dbh / 2

  # Calculate sapwood boundary (distance from bark surface)
  cambium_depth <- bark_depth
  sapwood_boundary <- cambium_depth + sapwood_depth

  # Check if sensors are in sapwood
  outer_in_sapwood <- outer_sensor_depth <= sapwood_boundary
  inner_in_sapwood <- inner_sensor_depth <= sapwood_boundary

  # Calculate maximum probe spacing for plot margins
  max_spacing <- max(upstream_dist, downstream_dist)

  return(list(
    radius = radius,
    bark_depth = bark_depth,
    sapwood_depth = sapwood_depth,
    cambium_depth = cambium_depth,
    sapwood_boundary = sapwood_boundary,
    probe_length_cm = probe_length_cm,
    probe_diameter_cm = probe_diameter_cm,
    upstream_dist = upstream_dist,
    downstream_dist = downstream_dist,
    max_spacing = max_spacing,
    outer_sensor_depth = outer_sensor_depth,
    inner_sensor_depth = inner_sensor_depth,
    outer_in_sapwood = outer_in_sapwood,
    inner_in_sapwood = inner_in_sapwood
  ))
}

#' Plot Probe Configuration - Vertical View
#'
#' Creates a side/vertical view of probes inserted into stem
#'
#' @param validation List from validate_probe_tree_config()
#' @return ggplot2 object
#' @keywords internal
plot_probe_vertical <- function(validation) {

  # Define tissue layers (as horizontal bands)
  tissues <- data.frame(
    layer = c("Bark", "Sapwood", "Heartwood"),
    x_start = c(0, validation$bark_depth, validation$sapwood_boundary),
    x_end = c(validation$bark_depth, validation$sapwood_boundary, validation$radius),
    fill_color = c("#8B4513", "#DEB887", "#A0522D"),
    stringsAsFactors = FALSE
  )

  # Define probe positions (downstream, heater, upstream)
  probe_data <- data.frame(
    probe = c("Downstream", "Heater", "Upstream"),
    y_pos = c(validation$downstream_dist, 0, -validation$upstream_dist),
    x_start = 0,
    x_end = validation$probe_length_cm,
    stringsAsFactors = FALSE
  )

  # Sensor positions on both downstream and upstream probes
  sensors <- data.frame(
    sensor = rep(c("Outer", "Inner"), 2),
    x = rep(c(validation$outer_sensor_depth, validation$inner_sensor_depth), 2),
    y = c(
      rep(validation$downstream_dist, 2),  # Downstream sensors
      rep(-validation$upstream_dist, 2)     # Upstream sensors
    ),
    in_sapwood = rep(c(validation$outer_in_sapwood, validation$inner_in_sapwood), 2),
    stringsAsFactors = FALSE
  )

  # Create plot
  p <- ggplot() +
    # Draw tissue layers as rectangles
    geom_rect(
      data = tissues,
      aes(xmin = x_start, xmax = x_end,
          ymin = -validation$upstream_dist - validation$max_spacing,
          ymax = validation$downstream_dist + validation$max_spacing,
          fill = layer),
      alpha = 0.6
    ) +
    scale_fill_manual(
      values = c("Bark" = "#8B4513", "Sapwood" = "#DEB887", "Heartwood" = "#A0522D"),
      name = "Tissue Layer"
    ) +
    # Draw probes as thick lines
    geom_segment(
      data = probe_data,
      aes(x = x_start, xend = x_end, y = y_pos, yend = y_pos),
      linewidth = 1.5, color = "gray30"
    ) +
    # Draw sensors as points
    geom_point(
      data = sensors,
      aes(x = x, y = y),
      size = 4, shape = 21, color = "black",
      fill = ifelse(sensors$in_sapwood, "green3", "red")
    ) +
    # Probe labels
    geom_text(
      data = probe_data,
      aes(x = -0.15, y = y_pos, label = probe),
      hjust = 1, size = 3.5
    ) +
    # Sapwood boundary line
    geom_vline(
      xintercept = validation$sapwood_boundary,
      linetype = "dashed", color = "red", linewidth = 0.8
    ) +
    annotate(
      "text",
      x = validation$sapwood_boundary,
      y = validation$downstream_dist + validation$max_spacing * 0.8,
      label = "Sapwood/Heartwood\nBoundary",
      hjust = -0.1, vjust = 0.5, size = 3, color = "red"
    ) +
    labs(
      title = "Vertical View: Probe Configuration",
      x = "Depth from bark surface (cm)",
      y = "Axial position (cm)"
    ) +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  return(p)
}

#' Plot Probe Configuration - Radial Cross-Section
#'
#' Creates a circular cross-section showing tissue layers and probe positions
#'
#' @param validation List from validate_probe_tree_config()
#' @return ggplot2 object
#' @keywords internal
plot_probe_radial <- function(validation) {

  # Create concentric circles for tissue layers
  theta <- seq(0, 2 * pi, length.out = 100)

  # Bark outer circle (tree radius)
  bark_outer <- data.frame(
    x = validation$radius * cos(theta),
    y = validation$radius * sin(theta),
    layer = "bark_outer"
  )

  # Cambium (inner bark boundary)
  cambium_radius <- validation$radius - validation$bark_depth
  cambium_circle <- data.frame(
    x = cambium_radius * cos(theta),
    y = cambium_radius * sin(theta),
    layer = "cambium"
  )

  # Sapwood/heartwood boundary
  heartwood_radius <- cambium_radius - validation$sapwood_depth
  heartwood_circle <- data.frame(
    x = heartwood_radius * cos(theta),
    y = heartwood_radius * sin(theta),
    layer = "heartwood_boundary"
  )

  # Probe position (assuming probe inserted horizontally from right)
  # Sensors are at different depths from bark surface
  probe_start_x <- validation$radius
  probe_end_x <- validation$radius - validation$probe_length_cm

  sensor_positions <- data.frame(
    sensor = c("Outer", "Inner"),
    x = c(
      validation$radius - validation$outer_sensor_depth,
      validation$radius - validation$inner_sensor_depth
    ),
    y = 0,
    in_sapwood = c(validation$outer_in_sapwood, validation$inner_in_sapwood),
    stringsAsFactors = FALSE
  )

  # Create sapwood ring data
  sapwood_ring <- data.frame(
    x = c(cambium_radius * cos(theta), rev(heartwood_radius * cos(theta))),
    y = c(cambium_radius * sin(theta), rev(heartwood_radius * sin(theta)))
  )

  # Create bark ring data
  bark_ring <- data.frame(
    x = c(validation$radius * cos(theta), rev(cambium_radius * cos(theta))),
    y = c(validation$radius * sin(theta), rev(cambium_radius * sin(theta)))
  )

  # Create plot
  p <- ggplot() +
    # Heartwood (innermost circle)
    geom_polygon(
      data = heartwood_circle,
      aes(x = x, y = y),
      fill = "#A0522D", alpha = 0.6
    ) +
    # Sapwood ring
    geom_polygon(
      data = sapwood_ring,
      aes(x = x, y = y),
      fill = "#DEB887", alpha = 0.6
    ) +
    # Bark ring
    geom_polygon(
      data = bark_ring,
      aes(x = x, y = y),
      fill = "#8B4513", alpha = 0.6
    ) +
    # Boundary circles
    geom_path(
      data = bark_outer,
      aes(x = x, y = y),
      linetype = "solid", color = "black", linewidth = 0.8
    ) +
    geom_path(
      data = cambium_circle,
      aes(x = x, y = y),
      linetype = "solid", color = "black", linewidth = 0.5
    ) +
    geom_path(
      data = heartwood_circle,
      aes(x = x, y = y),
      linetype = "dashed", color = "red", linewidth = 0.8
    ) +
    # Probe line
    geom_segment(
      aes(x = probe_start_x, xend = probe_end_x, y = 0, yend = 0),
      linewidth = 1.5, color = "gray30"
    ) +
    # Sensors
    geom_point(
      data = sensor_positions,
      aes(x = x, y = y),
      size = 5, shape = 21, color = "black",
      fill = ifelse(sensor_positions$in_sapwood, "green3", "red")
    ) +
    # Labels
    annotate("text", x = 0, y = 0, label = "Heartwood", size = 4, fontface = "bold") +
    annotate(
      "text",
      x = (cambium_radius + heartwood_radius) / 2,
      y = validation$radius * 0.5,
      label = "Sapwood", size = 4, fontface = "bold", color = "white"
    ) +
    annotate(
      "text",
      x = validation$radius * 0.9,
      y = validation$radius * 0.5,
      label = "Bark", size = 4, fontface = "bold", color = "white"
    ) +
    # Sensor labels
    geom_text(
      data = sensor_positions,
      aes(x = x, y = y - 0.5, label = sensor),
      size = 3, hjust = 0.5
    ) +
    labs(
      title = "Radial Cross-Section",
      x = "Distance (cm)",
      y = "Distance (cm)"
    ) +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid = element_blank()
    )

  return(p)
}
