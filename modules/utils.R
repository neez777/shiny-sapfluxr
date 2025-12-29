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

# Probe Visualisation Functions ----

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

    # Spacer thickness (external spacer between handle and bark)
    spacer_thickness_mm <- if (!is.null(probe_config$yaml_data$probe$spacer_thickness)) {
      probe_config$yaml_data$probe$spacer_thickness
    } else {
      0
    }

    # Convert to cm for visualisation
    probe_diameter_cm <- probe_diameter_mm / 10
    probe_length_cm <- probe_length_mm / 10
    spacer_thickness_cm <- spacer_thickness_mm / 10

    # Sensor depths from bark surface (probe length - distance from tip - spacer)
    outer_sensor_depth <- (probe_length_mm - outer_sensor_mm - spacer_thickness_mm) / 10  # cm
    inner_sensor_depth <- (probe_length_mm - inner_sensor_mm - spacer_thickness_mm) / 10  # cm

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

    # Spacer thickness (external spacer between handle and bark)
    spacer_thickness_mm <- if (!is.null(probe_config$yaml_data$probe$spacer_thickness)) {
      probe_config$yaml_data$probe$spacer_thickness
    } else {
      0
    }

    # Convert to cm for visualisation
    probe_diameter_cm <- probe_diameter_mm / 10
    probe_length_cm <- probe_length_mm / 10
    spacer_thickness_cm <- spacer_thickness_mm / 10

    # Sensor depths from bark surface (probe length - distance from tip - spacer)
    outer_sensor_depth <- (probe_length_mm - outer_sensor_mm - spacer_thickness_mm) / 10  # cm
    inner_sensor_depth <- (probe_length_mm - inner_sensor_mm - spacer_thickness_mm) / 10  # cm
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

  # Determine which tissue layer each sensor is in
  # Outer sensor
  if (outer_sensor_depth <= bark_depth) {
    outer_tissue <- "bark"
  } else if (outer_sensor_depth <= sapwood_boundary) {
    outer_tissue <- "sapwood"
  } else {
    outer_tissue <- "heartwood"
  }

  # Inner sensor
  if (inner_sensor_depth <= bark_depth) {
    inner_tissue <- "bark"
  } else if (inner_sensor_depth <= sapwood_boundary) {
    inner_tissue <- "sapwood"
  } else {
    inner_tissue <- "heartwood"
  }

  # Legacy boolean flags for backward compatibility
  outer_in_sapwood <- outer_tissue == "sapwood"
  inner_in_sapwood <- inner_tissue == "sapwood"

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
    spacer_thickness_cm = spacer_thickness_cm,
    upstream_dist = upstream_dist,
    downstream_dist = downstream_dist,
    max_spacing = max_spacing,
    outer_sensor_depth = outer_sensor_depth,
    inner_sensor_depth = inner_sensor_depth,
    outer_in_sapwood = outer_in_sapwood,
    inner_in_sapwood = inner_in_sapwood,
    outer_tissue = outer_tissue,
    inner_tissue = inner_tissue
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
  # If there's a spacer, probe starts through the spacer and insertion depth is reduced
  probe_x_start <- -validation$spacer_thickness_cm  # Extends through spacer
  probe_x_end <- validation$probe_length_cm - validation$spacer_thickness_cm  # Reduced insertion depth

  probe_data <- data.frame(
    probe = c("Downstream", "Heater", "Upstream"),
    y_pos = c(validation$downstream_dist, 0, -validation$upstream_dist),
    x_start = probe_x_start,
    x_end = probe_x_end,
    handle_color = c("blue", "red", "blue"),
    stringsAsFactors = FALSE
  )

  # Define probe handles (rectangles positioned outside bark)
  handle_width <- 0.8  # cm (wider to fit text)
  handle_height <- 0.4  # cm

  # Calculate handle position based on spacer thickness
  # If spacer = 0: handles flush with bark (center at -handle_width/2)
  # If spacer > 0: spacer goes from 0 to -spacer_thickness, handles beyond that
  if (validation$spacer_thickness_cm > 0) {
    handle_offset <- -(validation$spacer_thickness_cm + handle_width/2)
  } else {
    handle_offset <- -handle_width/2
  }

  probe_handles <- data.frame(
    probe = c("Downstream", "Heater", "Upstream"),
    x = rep(handle_offset, 3),
    y = c(validation$downstream_dist, 0, -validation$upstream_dist),
    color = c("blue", "red", "blue"),
    text_color = c("white", "black", "white"),
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
    tissue = rep(c(validation$outer_tissue, validation$inner_tissue), 2),
    stringsAsFactors = FALSE
  )

  # Assign colors based on tissue type (green for sapwood, red for bark or heartwood)
  sensors$fill_color <- ifelse(sensors$tissue == "sapwood", "green3", "red")

  # Calculate x-axis limits
  x_max <- validation$probe_length_cm + 2
  # x_min should include handle position with a small margin
  x_min <- handle_offset - handle_width/2 - 0.2  # 0.2 cm margin

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
    )

  # Add spacer boxes if spacer thickness > 0
  if (validation$spacer_thickness_cm > 0) {
    spacer_width <- validation$spacer_thickness_cm
    # Downstream spacer
    p <- p + annotate("rect",
                      xmin = -spacer_width,
                      xmax = 0,
                      ymin = validation$downstream_dist - handle_height/2,
                      ymax = validation$downstream_dist + handle_height/2,
                      fill = "grey70", color = "black", linewidth = 0.3)
    # Heater spacer
    p <- p + annotate("rect",
                      xmin = -spacer_width,
                      xmax = 0,
                      ymin = 0 - handle_height/2,
                      ymax = 0 + handle_height/2,
                      fill = "grey70", color = "black", linewidth = 0.3)
    # Upstream spacer
    p <- p + annotate("rect",
                      xmin = -spacer_width,
                      xmax = 0,
                      ymin = -validation$upstream_dist - handle_height/2,
                      ymax = -validation$upstream_dist + handle_height/2,
                      fill = "grey70", color = "black", linewidth = 0.3)
  }

  # Draw probe handles as rectangles (using annotate to avoid fill scale conflict)
  p <- p +
    annotate("rect",
             xmin = handle_offset - handle_width/2,
             xmax = handle_offset + handle_width/2,
             ymin = validation$downstream_dist - handle_height/2,
             ymax = validation$downstream_dist + handle_height/2,
             fill = "blue", color = "black", linewidth = 0.5) +
    annotate("rect",
             xmin = handle_offset - handle_width/2,
             xmax = handle_offset + handle_width/2,
             ymin = 0 - handle_height/2,
             ymax = 0 + handle_height/2,
             fill = "red", color = "black", linewidth = 0.5) +
    annotate("rect",
             xmin = handle_offset - handle_width/2,
             xmax = handle_offset + handle_width/2,
             ymin = -validation$upstream_dist - handle_height/2,
             ymax = -validation$upstream_dist + handle_height/2,
             fill = "blue", color = "black", linewidth = 0.5) +
    # Add text labels on handles
    annotate("text",
             x = handle_offset, y = validation$downstream_dist,
             label = "Downstream", color = "white", size = 3.2, fontface = "bold") +
    annotate("text",
             x = handle_offset, y = 0,
             label = "Heater", color = "black", size = 3.2, fontface = "bold") +
    annotate("text",
             x = handle_offset, y = -validation$upstream_dist,
             label = "Upstream", color = "white", size = 3.2, fontface = "bold") +
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
      fill = sensors$fill_color
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
    coord_fixed(ratio = 1, xlim = c(x_min, x_max)) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  return(p)
}

#' Plot Probe Configuration - Radial Cross-Section
#'
#' Creates a 75-degree segment cross-section showing tissue layers and probe positions
#'
#' @param validation List from validate_probe_tree_config()
#' @return ggplot2 object
#' @keywords internal
plot_probe_radial <- function(validation) {

  # Create 60-degree segment (centered on probe insertion from right)
  # Center the segment on 0 degrees (probe from right side)
  segment_degrees <- 60
  segment_radians <- segment_degrees * pi / 180
  start_angle <- -segment_radians / 2
  end_angle <- segment_radians / 2

  # Create angles for the arc
  theta <- seq(start_angle, end_angle, length.out = 100)

  # Bark outer arc (tree radius)
  bark_outer <- data.frame(
    x = validation$radius * cos(theta),
    y = validation$radius * sin(theta),
    layer = "bark_outer"
  )

  # Cambium (inner bark boundary)
  cambium_radius <- validation$radius - validation$bark_depth
  cambium_arc <- data.frame(
    x = cambium_radius * cos(theta),
    y = cambium_radius * sin(theta),
    layer = "cambium"
  )

  # Sapwood/heartwood boundary
  heartwood_radius <- cambium_radius - validation$sapwood_depth
  heartwood_arc <- data.frame(
    x = heartwood_radius * cos(theta),
    y = heartwood_radius * sin(theta),
    layer = "heartwood_boundary"
  )

  # Probe position (assuming probe inserted horizontally from right)
  # If there's a spacer, probe insertion depth is reduced
  probe_insertion_depth <- validation$probe_length_cm - validation$spacer_thickness_cm
  probe_start_x <- validation$radius  # Probe starts at bark surface
  probe_end_x <- validation$radius - probe_insertion_depth  # Reduced by spacer

  # Handle dimensions and position (blue box outside bark)
  handle_width <- 0.8  # cm, same as vertical view
  handle_height <- 0.3  # cm half-height for radial view

  # Calculate handle position based on spacer thickness
  if (validation$spacer_thickness_cm > 0) {
    # Handle beyond spacer
    handle_start_x <- validation$radius + validation$spacer_thickness_cm
    handle_end_x <- handle_start_x + handle_width
    # Spacer between bark and handle
    spacer_start_x <- validation$radius
    spacer_end_x <- validation$radius + validation$spacer_thickness_cm
  } else {
    # Handle flush with bark
    handle_start_x <- validation$radius
    handle_end_x <- validation$radius + handle_width
    spacer_start_x <- NULL
    spacer_end_x <- NULL
  }

  sensor_positions <- data.frame(
    sensor = c("Outer", "Inner"),
    x = c(
      validation$radius - validation$outer_sensor_depth,
      validation$radius - validation$inner_sensor_depth
    ),
    y = 0,
    tissue = c(validation$outer_tissue, validation$inner_tissue),
    stringsAsFactors = FALSE
  )

  # Assign colors based on tissue type (green for sapwood, red for bark or heartwood)
  sensor_positions$fill_color <- ifelse(sensor_positions$tissue == "sapwood", "green3", "red")

  # Create heartwood segment (wedge from center)
  heartwood_segment <- data.frame(
    x = c(0, heartwood_radius * cos(theta), 0),
    y = c(0, heartwood_radius * sin(theta), 0)
  )

  # Create sapwood ring segment
  sapwood_ring <- data.frame(
    x = c(
      heartwood_radius * cos(theta),
      rev(cambium_radius * cos(theta))
    ),
    y = c(
      heartwood_radius * sin(theta),
      rev(cambium_radius * sin(theta))
    )
  )

  # Create bark ring segment
  bark_ring <- data.frame(
    x = c(
      cambium_radius * cos(theta),
      rev(validation$radius * cos(theta))
    ),
    y = c(
      cambium_radius * sin(theta),
      rev(validation$radius * sin(theta))
    )
  )

  # Create plot
  p <- ggplot() +
    # Heartwood (innermost segment)
    geom_polygon(
      data = heartwood_segment,
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
    # Boundary arcs
    geom_path(
      data = bark_outer,
      aes(x = x, y = y),
      linetype = "solid", color = "black", linewidth = 0.8
    ) +
    geom_path(
      data = cambium_arc,
      aes(x = x, y = y),
      linetype = "solid", color = "black", linewidth = 0.5
    ) +
    geom_path(
      data = heartwood_arc,
      aes(x = x, y = y),
      linetype = "dashed", color = "red", linewidth = 0.8
    ) +
    # Radial closing lines (to complete the segment)
    geom_segment(
      aes(x = 0, xend = validation$radius * cos(start_angle),
          y = 0, yend = validation$radius * sin(start_angle)),
      linewidth = 0.8, color = "black"
    ) +
    geom_segment(
      aes(x = 0, xend = validation$radius * cos(end_angle),
          y = 0, yend = validation$radius * sin(end_angle)),
      linewidth = 0.8, color = "black"
    ) +
    # Probe line (gray line showing probe insertion)
    geom_segment(
      aes(x = probe_start_x, xend = probe_end_x, y = 0, yend = 0),
      linewidth = 1.5, color = "gray30"
    ) +
    # Sensors
    geom_point(
      data = sensor_positions,
      aes(x = x, y = y),
      size = 5, shape = 21, color = "black",
      fill = sensor_positions$fill_color
    ) +
    # Labels positioned for 60-degree segment
    annotate("text", x = heartwood_radius * 0.5, y = 0, label = "Heartwood", size = 4, fontface = "bold", color = "black") +
    annotate(
      "text",
      x = (cambium_radius + heartwood_radius) / 2,
      y = ((cambium_radius + heartwood_radius) / 2) * sin(segment_radians / 4),
      label = "Sapwood", size = 4, fontface = "bold", color = "black"
    ) +
    annotate(
      "text",
      x = (validation$radius + cambium_radius) / 2,
      y = ((validation$radius + cambium_radius) / 2) * sin(segment_radians / 4),
      label = "Bark", size = 4, fontface = "bold", color = "black"
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
      y = NULL
    ) +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )

  # Add probe handle (blue box outside bark)
  p <- p + annotate("rect",
                    xmin = handle_start_x,
                    xmax = handle_end_x,
                    ymin = -handle_height,
                    ymax = handle_height,
                    fill = "blue", color = "black", linewidth = 0.5)

  # Add spacer box if spacer thickness > 0 (grey box between bark and handle)
  if (validation$spacer_thickness_cm > 0) {
    p <- p + annotate("rect",
                      xmin = spacer_start_x,
                      xmax = spacer_end_x,
                      ymin = -handle_height,
                      ymax = handle_height,
                      fill = "grey70", color = "black", linewidth = 0.3)
  }

  return(p)
}
