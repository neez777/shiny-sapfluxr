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

#' Get standard Plotly styling for a given trace
#'
#' @param method The method name (e.g., "HRM", "MHR")
#' @param sensor The sensor position ("outer", "inner")
#' @param is_corrected Boolean, whether this is corrected data
#' @param is_vpd Boolean, whether this is the VPD trace
#' @param config Optional list of style configurations
#' @return List of line styling parameters for Plotly
#' @keywords internal
get_plot_style <- function(method = NULL, sensor = "outer", is_corrected = FALSE, is_vpd = FALSE, config = NULL) {
  if (is_vpd) {
    # Default VPD style
    col <- "black"
    width <- 1.0
    dash <- "dot"

    # Override from config if available
    if (!is.null(config$special_traces$vpd)) {
      if (!is.null(config$special_traces$vpd$color)) col <- config$special_traces$vpd$color
      if (!is.null(config$special_traces$vpd$width)) width <- config$special_traces$vpd$width
      if (!is.null(config$special_traces$vpd$dash)) dash <- config$special_traces$vpd$dash
    }

    return(list(color = col, width = width, dash = dash))
  }

  # Map method name to config key
  method_key <- method
  if (is.null(method_key)) method_key <- "HRM"

  # Get method settings from config or use defaults
  m_settings <- config$methods[[method_key]]

  # Default colors
  default_colors <- list(
    "HRM"      = list(outer = "#1f77b4", inner = "#aec7e8", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid"),
    "MHR"      = list(outer = "#ff7f0e", inner = "#ffbb78", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid"),
    "Tmax_Klu" = list(outer = "#2ca02c", inner = "#98df8a", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid"),
    "Tmax_Coh" = list(outer = "#d62728", inner = "#ff9896", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid"),
    "sDMA:Tmax_Klu" = list(outer = "#9467bd", inner = "#c5b0d5", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid"),
    "sDMA:MHR" = list(outer = "#e377c2", inner = "#f7b6d2", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid"),
    "sDMA:Tmax_Coh" = list(outer = "#8c564b", inner = "#c49c94", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid"),
    "CHPM"     = list(outer = "#8c564b", inner = "#c49c94", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid"),
    "DRM"      = list(outer = "#e377c2", inner = "#f7b6d2", raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid")
  )

  # Determine Color
  col <- if (!is.null(m_settings)) {
    if (tolower(sensor) == "inner") m_settings$inner else m_settings$outer
  } else {
    def_col <- default_colors[[method_key]] %||% list(outer = "#7f7f7f", inner = "#c7c7c7")
    if (tolower(sensor) == "inner") def_col$inner else def_col$outer
  }

  # Determine Width and Dash
  if (is_corrected) {
    width <- if (!is.null(m_settings)) m_settings$corrected_width else default_colors[[method_key]]$corrected_width %||% 0.7
    dash <- if (!is.null(m_settings)) m_settings$corrected_style else default_colors[[method_key]]$corrected_style %||% "solid"
  } else {
    width <- if (!is.null(m_settings)) m_settings$raw_width else default_colors[[method_key]]$raw_width %||% 1.0
    dash <- if (!is.null(m_settings)) m_settings$raw_style else default_colors[[method_key]]$raw_style %||% "solid"
  }

  return(list(color = col, width = width, dash = dash))
}

#' Get standard Plotly layout (theme_classic style)
#'
#' @param title Plot title
#' @param xtitle X-axis title
#' @param ytitle Y-axis title
#' @param uirevision Optional uirevision string for preserving zoom state
#' @return Plotly layout list
#' @keywords internal
get_standard_layout <- function(title = "", xtitle = "Date", ytitle = "Vh (cm/hr)", uirevision = NULL) {
  l <- list(
    title = title,
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    xaxis = list(
      title = xtitle,
      showline = TRUE,
      linecolor = 'black',
      showgrid = FALSE,
      zeroline = FALSE
    ),
    yaxis = list(
      title = ytitle,
      showline = TRUE,
      linecolor = 'black',
      showgrid = FALSE,
      fixedrange = TRUE,
      zeroline = TRUE,
      zerolinecolor = 'black',
      zerolinewidth = 0.5
    ),
    showlegend = TRUE,
    legend = list(
      orientation = "h",
      x = 0.5, y = -0.15,
      xanchor = "center",
      yanchor = "top"
    ),
    hovermode = "closest",
    margin = list(b = 100, t = 60, l = 60, r = 20)
  )

  if (!is.null(uirevision)) {
    l$uirevision <- uirevision
  }

  return(l)
}

#' Apply standard Plotly config (hide icons)
#'
#' @param p Plotly object
#' @param filename Filename for download
#' @param add_csv_download Logical, whether to add a custom CSV download button to the modebar
#' @return Configured Plotly object
#' @keywords internal
apply_standard_plotly_config <- function(p, filename = "plot_download", add_csv_download = FALSE) {
  config_args <- list(
    p = p,
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "lasso2d", "select2d", "autoScale2d",
      "hoverClosestCartesian", "hoverCompareCartesian",
      "toggleSpikelines"
    ),
    toImageButtonOptions = list(
      format = "png",
      filename = filename,
      height = 600,
      width = 1200,
      scale = 2
    )
  )

  if (isTRUE(add_csv_download)) {
    # Custom JS for downloading visible plot data as CSV
    csv_btn <- list(
      name = "Download Visible Data (CSV)",
      icon = list(
        path = "M19 9h-4V3H9v6H5l7 7 7-7zM5 18v2h14v-2H5z", # Standard download SVG icon
        # Scale to 80% (0.8) and shift right/down slightly to center it within the standard Plotly 1em icon bounds
        transform = "matrix(0.8 0 0 0.8 2.4 2.4)"
      ),
      click = htmlwidgets::JS(sprintf("
        function(gd) {
          var traces = gd.data;
          var xRange = gd.layout.xaxis ? gd.layout.xaxis.range : null;

          var minTime = -Infinity, maxTime = Infinity;
          if (xRange && xRange.length === 2) {
            // Plotly often returns ranges as string dates, convert to timestamp
            var start = typeof xRange[0] === 'string' ? xRange[0].replace(' ', 'T') : xRange[0];
            var end = typeof xRange[1] === 'string' ? xRange[1].replace(' ', 'T') : xRange[1];
            minTime = new Date(start).getTime();
            maxTime = new Date(end).getTime();
          }

          var baseTrace = traces.find(t => t.x && t.x.length > 0);
          if (!baseTrace) return;
          var xData = baseTrace.x;

          var csv = [];
          var header = ['datetime'];
          traces.forEach(function(trace) {
            var colName = trace.name || 'Value';
            header.push('\\\"' + String(colName).replace(/\\\"/g, '\\\"\\\"') + '\\\"');
          });
          csv.push(header.join(','));

          for(var i = 0; i < xData.length; i++) {
             var currentX = xData[i];
             var t = typeof currentX === 'string' ? new Date(currentX.replace(' ', 'T')).getTime() : new Date(currentX).getTime();

             // If not a date or within range, include it
             if (isNaN(t) || (t >= minTime && t <= maxTime)) {
                var row = ['\\\"' + String(currentX).replace(/\\\"/g, '\\\"\\\"') + '\\\"'];
                traces.forEach(function(trace) {
                   var val = (trace.y && trace.y[i] !== undefined && trace.y[i] !== null) ? trace.y[i] : '';
                   row.push('\\\"' + String(val).replace(/\\\"/g, '\\\"\\\"') + '\\\"');
                });
                csv.push(row.join(','));
             }
          }

          var blob = new Blob([csv.join('\\n')], { type: 'text/csv;charset=utf-8;' });
          var link = document.createElement('a');
          link.href = URL.createObjectURL(blob);
          link.setAttribute('download', '%s_data.csv');
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
        }
      ", filename))
    )

    # Explicitly construct the modebar to place CSV button next to the Camera
    config_args$modeBarButtonsToRemove <- NULL
    config_args$modeBarButtons <- list(
      list("toImage", csv_btn),
      list("zoom2d", "pan2d"),
      list("zoomIn2d", "zoomOut2d", "resetScale2d")
    )
  }

  do.call(plotly::config, config_args)
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
    dbh               <- wood_properties$tree_measurements$dbh
    bark_thickness_dbh   <- wood_properties$tree_measurements$bark_thickness_dbh
    bark_thickness_probe <- wood_properties$tree_measurements$bark_thickness_probe
    sapwood_thickness <- wood_properties$tree_measurements$sapwood_thickness

    # Apply defaults for NULL values
    if (is.null(dbh) || is.na(dbh)) dbh <- 20
    if (is.null(bark_thickness_dbh)   || is.na(bark_thickness_dbh))   bark_thickness_dbh   <- 0.5
    if (is.null(bark_thickness_probe) || is.na(bark_thickness_probe)) bark_thickness_probe <- bark_thickness_dbh
    if (is.null(sapwood_thickness) || is.na(sapwood_thickness)) sapwood_thickness <- 3.0

  } else {
    # Handle list structure from manual mode
    dbh <- if (!is.null(wood_properties$tree_measurements$dbh)) {
      wood_properties$tree_measurements$dbh
    } else 20

    bark_thickness_dbh <- if (!is.null(wood_properties$tree_measurements$bark_thickness_dbh)) {
      wood_properties$tree_measurements$bark_thickness_dbh
    } else 0.5

    bark_thickness_probe <- if (!is.null(wood_properties$tree_measurements$bark_thickness_probe)) {
      wood_properties$tree_measurements$bark_thickness_probe
    } else bark_thickness_dbh

    sapwood_thickness <- if (!is.null(wood_properties$tree_measurements$sapwood_thickness)) {
      wood_properties$tree_measurements$sapwood_thickness
    } else 3.0
  }

  # Calculate radius from DBH (uses full bark at DBH site)
  radius <- dbh / 2

  # Tissue boundaries in cm from the probe-site (shaved) OB surface.
  # sapwood_thickness is IB→HW (cambium to heartwood, bark not included).
  cambium_depth    <- bark_thickness_probe
  sapwood_boundary <- cambium_depth + sapwood_thickness

  # For radial plot radius calc, IB radius uses full (DBH-site) bark
  cambium_radius <- radius - bark_thickness_dbh

  # Determine which tissue layer each sensor is in (probe-site reference frame)
  if (outer_sensor_depth <= cambium_depth) {
    outer_tissue <- "bark"
  } else if (outer_sensor_depth <= sapwood_boundary) {
    outer_tissue <- "sapwood"
  } else {
    outer_tissue <- "heartwood"
  }

  if (inner_sensor_depth <= cambium_depth) {
    inner_tissue <- "bark"
  } else if (inner_sensor_depth <= sapwood_boundary) {
    inner_tissue <- "sapwood"
  } else {
    inner_tissue <- "heartwood"
  }

  outer_in_sapwood <- outer_tissue == "sapwood"
  inner_in_sapwood <- inner_tissue == "sapwood"

  max_spacing <- max(upstream_dist, downstream_dist)

  return(list(
    radius = radius,
    bark_thickness_dbh_cm   = bark_thickness_dbh,
    bark_thickness_probe_cm = bark_thickness_probe,
    # bark_depth retained for backward compat with existing plot code;
    # points to probe-site bark (the relevant boundary in the probe reference frame)
    bark_depth = bark_thickness_probe,
    sapwood_thickness = sapwood_thickness,
    cambium_depth = cambium_depth,
    cambium_radius = cambium_radius,
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

  # OB-surface reference frame: x = 0 at the original outer bark surface.
  # All positions are expressed as depth from the OB surface.
  bark_dbh    <- validation$bark_thickness_dbh_cm
  bark_probe  <- validation$bark_thickness_probe_cm
  shaved_depth <- bark_dbh - bark_probe   # depth of channel carved into bark at probe site
  spacer_cm   <- validation$spacer_thickness_cm

  # OB-frame tissue boundaries
  cambium_x     <- bark_dbh                               # IB/cambium boundary
  sapwood_end_x <- bark_dbh + validation$sapwood_thickness   # sapwood / heartwood boundary

  # OB-frame sensor positions (probe-frame depth + shaved_depth offset)
  outer_x <- shaved_depth + validation$outer_sensor_depth
  inner_x <- shaved_depth + validation$inner_sensor_depth

  # OB-frame needle geometry
  # Hub sits behind the shaved surface by spacer_cm; needles extend to tip
  hub_x       <- shaved_depth - spacer_cm
  needle_tip_x <- shaved_depth + validation$probe_length_cm - spacer_cm

  handle_width  <- 0.8  # cm
  handle_height <- 0.4  # cm
  # Handle centre just outside hub (right edge of handle = hub)
  handle_cx <- hub_x - handle_width / 2

  # y-axis extents
  y_lo <- -validation$upstream_dist - validation$max_spacing
  y_hi <-  validation$downstream_dist + validation$max_spacing

  # y-extent of the shaved/notch zone — spans all three probes with margin
  notch_y_lo <- -validation$upstream_dist - handle_height
  notch_y_hi <-  validation$downstream_dist + handle_height

  # x-axis extents
  x_min <- handle_cx - handle_width / 2 - 0.3
  x_max <- max(needle_tip_x, sapwood_end_x) + 1.5

  # ---------- Tissue layer data ----------
  # Bark: three rectangles that together show the shaved notch:
  #   top strip    — full bark height above probe zone
  #   probe zone   — only remaining (post-shave) bark; gap from 0→shaved_depth = the notch
  #   bottom strip — full bark height below probe zone
  bark_rects <- data.frame(
    layer = factor(rep("Bark", 3), levels = c("Bark", "Sapwood", "Heartwood")),
    xmin  = c(0,           shaved_depth, 0),
    xmax  = c(bark_dbh,   bark_dbh,    bark_dbh),
    ymin  = c(notch_y_hi, notch_y_lo,  y_lo),
    ymax  = c(y_hi,       notch_y_hi,  notch_y_lo),
    stringsAsFactors = FALSE
  )

  sapwood_hw <- data.frame(
    layer = factor(c("Sapwood", "Heartwood"), levels = c("Bark", "Sapwood", "Heartwood")),
    xmin  = c(cambium_x,    sapwood_end_x),
    xmax  = c(sapwood_end_x, x_max),
    ymin  = c(y_lo,         y_lo),
    ymax  = c(y_hi,         y_hi),
    stringsAsFactors = FALSE
  )

  all_tissues <- rbind(bark_rects, sapwood_hw)

  # ---------- Probe needle and handle data ----------
  probe_data <- data.frame(
    probe  = c("Downstream", "Heater", "Upstream"),
    y_pos  = c(validation$downstream_dist, 0, -validation$upstream_dist),
    x_start = hub_x,
    x_end   = needle_tip_x,
    stringsAsFactors = FALSE
  )

  # Sensor positions on downstream and upstream probes
  sensors <- data.frame(
    sensor = rep(c("Outer", "Inner"), 2),
    x      = rep(c(outer_x, inner_x), 2),
    y      = c(rep(validation$downstream_dist, 2), rep(-validation$upstream_dist, 2)),
    tissue = rep(c(validation$outer_tissue, validation$inner_tissue), 2),
    stringsAsFactors = FALSE
  )
  sensors$fill_color <- ifelse(sensors$tissue == "sapwood", "green3", "red")

  # ---------- Build plot ----------
  p <- ggplot() +
    geom_rect(
      data = all_tissues,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = layer),
      alpha = 0.6
    ) +
    scale_fill_manual(
      values = c("Bark" = "#8B4513", "Sapwood" = "#DEB887", "Heartwood" = "#A0522D"),
      name   = "Tissue Layer",
      limits = c("Bark", "Sapwood", "Heartwood")
    )

  # Spacer boxes between hub and shaved surface
  if (spacer_cm > 0) {
    for (y_probe in c(validation$downstream_dist, 0, -validation$upstream_dist)) {
      p <- p + annotate("rect",
                        xmin = hub_x, xmax = shaved_depth,
                        ymin = y_probe - handle_height / 2,
                        ymax = y_probe + handle_height / 2,
                        fill = "grey70", color = "black", linewidth = 0.3)
    }
  }

  # Probe handles (annotate to avoid fill-scale conflict)
  for (i in seq_len(nrow(probe_data))) {
    hcol  <- if (probe_data$probe[i] == "Heater") "red" else "blue"
    tcol  <- if (probe_data$probe[i] == "Heater") "black" else "white"
    yc    <- probe_data$y_pos[i]
    label <- probe_data$probe[i]
    p <- p +
      annotate("rect",
               xmin = handle_cx - handle_width / 2,
               xmax = handle_cx + handle_width / 2,
               ymin = yc - handle_height / 2,
               ymax = yc + handle_height / 2,
               fill = hcol, color = "black", linewidth = 0.5) +
      annotate("text",
               x = handle_cx, y = yc,
               label = label, color = tcol, size = 3.2, fontface = "bold")
  }

  p <- p +
    # Probe needle lines
    geom_segment(
      data = probe_data,
      aes(x = x_start, xend = x_end, y = y_pos, yend = y_pos),
      linewidth = 1.5, color = "gray30"
    ) +
    # Sensors
    geom_point(
      data = sensors,
      aes(x = x, y = y),
      size = 4, shape = 21, color = "black",
      fill = sensors$fill_color
    ) +
    # Sensor labels above the downstream probe only
    geom_text(
      data = sensors %>% dplyr::filter(y > 0),
      aes(x = x, y = y + handle_height * 1.5, label = sensor),
      size = 3.5, fontface = "bold", vjust = 0
    ) +
    # Sapwood / heartwood boundary
    geom_vline(xintercept = sapwood_end_x, linetype = "dashed", color = "red", linewidth = 0.8) +
    annotate("text",
             x = sapwood_end_x,
             y = notch_y_hi - 0.2,
             label = "Sapwood/Heartwood\nBoundary",
             hjust = -0.1, vjust = 0.5, size = 3, color = "red") +
    labs(
      title = "Longitudinal Section : Probe Configuration",
      x     = "Depth from outer bark surface (cm)",
      y     = "Axial position (cm)"
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

  # 60° segment centred on 180° (probe inserted from LEFT)
  segment_degrees <- 60
  segment_radians <- segment_degrees * pi / 180
  start_angle     <- pi - segment_radians / 2
  end_angle       <- pi + segment_radians / 2
  theta           <- seq(start_angle, end_angle, length.out = 100)

  # Key radii
  outer_r    <- validation$radius                                           # OB surface
  probe_bk_r <- validation$cambium_radius + validation$bark_thickness_probe_cm  # shaved surface
  cambium_r  <- validation$cambium_radius                                   # IB/cambium
  heartwd_r  <- cambium_r - validation$sapwood_thickness                    # sapwood/HW boundary

  # Angular half-width of the bark notch at the probe insertion point (pi radians)
  # Sized so the notch is visually clear (~20° total) but doesn't dominate the 60° segment
  handle_height <- 0.3  # cm (tangential half-height)
  h_notch <- 1.2  # cm (tangential half-height of the bark notch)

  if (h_notch >= probe_bk_r) h_notch <- probe_bk_r * 0.8

  theta_out_top <- pi - asin(h_notch / outer_r)
  theta_in_top  <- pi - asin(h_notch / probe_bk_r)
  theta_out_bot <- pi + asin(h_notch / outer_r)
  theta_in_bot  <- pi + asin(h_notch / probe_bk_r)

  # ---------- Arc reference paths ----------
  bark_outer_arc <- data.frame(x = outer_r   * cos(theta), y = outer_r   * sin(theta))
  cambium_arc    <- data.frame(x = cambium_r * cos(theta), y = cambium_r * sin(theta))
  heartwd_arc    <- data.frame(x = heartwd_r * cos(theta), y = heartwd_r * sin(theta))
  shaved_arc     <- data.frame(x = probe_bk_r * cos(theta), y = probe_bk_r * sin(theta))

  # ---------- Tissue polygons ----------

  # Heartwood wedge
  hw_seg <- data.frame(
    x = c(0, heartwd_r * cos(theta), 0),
    y = c(0, heartwd_r * sin(theta), 0),
    layer = "Heartwood",
    grp   = "heartwood"
  )

  # Sapwood ring
  sw_ring <- data.frame(
    x = c(heartwd_r * cos(theta), rev(cambium_r * cos(theta))),
    y = c(heartwd_r * sin(theta), rev(cambium_r * sin(theta))),
    layer = "Sapwood",
    grp   = "sapwood"
  )

  # Inner bark ring (cambium → shaved surface) — full 60° segment.
  # This is the bark that REMAINS after shaving; it is present even through the probe zone.
  inner_bk <- data.frame(
    x = c(cambium_r * cos(theta), rev(probe_bk_r * cos(theta))),
    y = c(cambium_r * sin(theta), rev(probe_bk_r * sin(theta))),
    layer = "Bark",
    grp   = "bark_inner"
  )

  # Outer bark ring — split into LEFT and RIGHT pieces, leaving a notch at 180°
  # with horizontal walls matching the probe handle height
  t_out_top_seq <- seq(start_angle, theta_out_top, length.out = 40)
  t_in_top_seq  <- seq(theta_in_top, start_angle, length.out = 40)

  outer_bk_left <- data.frame(
    x = c(outer_r * cos(t_out_top_seq), probe_bk_r * cos(t_in_top_seq)),
    y = c(outer_r * sin(t_out_top_seq), probe_bk_r * sin(t_in_top_seq)),
    layer = "Bark",
    grp   = "bark_outer_left"
  )

  t_out_bot_seq <- seq(theta_out_bot, end_angle, length.out = 40)
  t_in_bot_seq  <- seq(end_angle, theta_in_bot, length.out = 40)

  outer_bk_right <- data.frame(
    x = c(outer_r * cos(t_out_bot_seq), probe_bk_r * cos(t_in_bot_seq)),
    y = c(outer_r * sin(t_out_bot_seq), probe_bk_r * sin(t_in_bot_seq)),
    layer = "Bark",
    grp   = "bark_outer_right"
  )

  all_tissues <- dplyr::bind_rows(hw_seg, sw_ring, inner_bk, outer_bk_left, outer_bk_right)
  all_tissues$layer <- factor(all_tissues$layer, levels = c("Bark", "Sapwood", "Heartwood"))

  # ---------- Probe geometry in radial frame (x = 0 at tree centre) ----------
  # Sensor depths are measured from the SHAVED surface (at x = -probe_bk_r).
  # Hub sits further left by spacer_cm.
  hub_x        <- -(probe_bk_r + validation$spacer_thickness_cm)
  needle_end_x <- -probe_bk_r + (validation$probe_length_cm - validation$spacer_thickness_cm)

  sensor_positions <- data.frame(
    sensor = c("Outer", "Inner"),
    x = c(
      -probe_bk_r + validation$outer_sensor_depth,
      -probe_bk_r + validation$inner_sensor_depth
    ),
    y      = 0,
    tissue = c(validation$outer_tissue, validation$inner_tissue),
    stringsAsFactors = FALSE
  )
  sensor_positions$fill_color <- ifelse(sensor_positions$tissue == "sapwood", "green3", "red")

  handle_width  <- 0.8  # cm (radial extent)
  handle_height <- 0.3  # cm (tangential half-height)
  handle_end_x  <- hub_x - handle_width   # left edge of handle

  # ---------- Build plot ----------
  p <- ggplot() +
    geom_polygon(
      data = all_tissues,
      aes(x = x, y = y, fill = layer, group = grp),
      alpha = 0.6
    ) +
    scale_fill_manual(
      values = c("Bark" = "#8B4513", "Sapwood" = "#DEB887", "Heartwood" = "#A0522D"),
      name   = "Tissue Layer",
      limits = c("Bark", "Sapwood", "Heartwood")
    ) +
    # Outer bark boundary arc (full segment)
    geom_path(data = bark_outer_arc, aes(x = x, y = y),
              linetype = "solid", color = "black", linewidth = 0.8) +
    # Shaved-bark boundary arc (reference line showing inner bark / notch depth)
    geom_path(data = shaved_arc, aes(x = x, y = y),
              linetype = "dashed", color = "#8B4513", linewidth = 0.4) +
    # Sapwood/heartwood boundary arc
    geom_path(data = heartwd_arc, aes(x = x, y = y),
              linetype = "dashed", color = "red", linewidth = 0.8) +
    # Radial segment closing lines
    geom_segment(aes(x = 0, xend = outer_r * cos(start_angle),
                     y = 0, yend = outer_r * sin(start_angle)),
                 linewidth = 0.8, color = "black") +
    geom_segment(aes(x = 0, xend = outer_r * cos(end_angle),
                     y = 0, yend = outer_r * sin(end_angle)),
                 linewidth = 0.8, color = "black") +
    # Probe needle line (from hub to tip)
    geom_segment(aes(x = hub_x, xend = needle_end_x, y = 0, yend = 0),
                 linewidth = 1.5, color = "gray30") +
    # Sensors
    geom_point(
      data = sensor_positions,
      aes(x = x, y = y),
      size = 5, shape = 21, color = "black",
      fill = sensor_positions$fill_color
    ) +
    scale_x_continuous(labels = abs) +
    labs(
      title = "Radial Cross-Section View",
      x     = "Width (cm)",
      y     = NULL
    ) +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    theme(
      legend.position = "bottom",
      plot.title     = element_text(hjust = 0.5, face = "bold"),
      panel.grid     = element_blank(),
      axis.text.y    = element_blank(),
      axis.ticks.y   = element_blank(),
      axis.title.y   = element_blank()
    )

  # Probe handle (blue box, left of hub)
  p <- p + annotate("rect",
                    xmin = handle_end_x, xmax = hub_x,
                    ymin = -handle_height, ymax = handle_height,
                    fill = "blue", color = "black", linewidth = 0.5)

  # Spacer box between handle/hub and shaved surface (grey)
  if (validation$spacer_thickness_cm > 0) {
    p <- p + annotate("rect",
                      xmin = hub_x, xmax = -probe_bk_r,
                      ymin = -handle_height, ymax = handle_height,
                      fill = "grey70", color = "black", linewidth = 0.3)
  }

  return(p)
}
