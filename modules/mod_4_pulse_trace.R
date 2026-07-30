#' Pulse Trace Viewer Module
#'
#' Shiny module for viewing individual pulse temperature traces
#'
#' @param id Module ID
#' @param heat_pulse_data Reactive containing heat_pulse_data
#' @param selected_pulse_id Reactive containing selected pulse ID from plot click
#' @return None (displays pulse trace plot)
#'

# Helpers ----

# Detect the sampling interval (seconds/row) for a pulse from its timestamps,
# mirroring calc_vh_single_pulse(). Falls back to 1.0 s when timestamps are
# identical/unusable.
pt_sampling_interval <- function(pulse_data) {
  interval <- 1.0
  if (nrow(pulse_data) >= 2 && "datetime" %in% names(pulse_data)) {
    n5 <- min(5L, nrow(pulse_data))
    tdiffs <- as.numeric(difftime(pulse_data$datetime[2:n5],
                                  pulse_data$datetime[1:(n5 - 1)], units = "secs"))
    med <- stats::median(tdiffs)
    if (!is.na(med) && med > 0) interval <- med
  }
  interval
}

# Compute baseline-subtracted delta-T for all four sensors from a single pulse's
# raw measurements, honouring the chosen pre-pulse baseline method. time_sec is
# built from the detected sampling interval and offset by the pre-pulse period
# (so t = 0 is the pulse injection), matching the calculation rather than
# assuming 1 Hz / a 30-row pre-pulse. Returns the pulse data frame with time_sec
# and deltaT_do/di/uo/ui.
pt_baseline_deltaT <- function(pulse_data, baseline_method, pre_pulse = 30) {
  interval <- pt_sampling_interval(pulse_data)
  pulse_data$time_sec <- (seq_len(nrow(pulse_data)) - 1) * interval - pre_pulse
  sensors <- c("do", "di", "uo", "ui")

  if (grepl("slope", baseline_method)) {
    pre <- pulse_data[pulse_data$time_sec < 0, ]
    for (s in sensors) {
      fit <- stats::lm(stats::reformulate("time_sec", s), data = pre)
      # Intercept = fitted value at t = 0 (temperature at pulse release)
      pulse_data[[paste0("deltaT_", s)]] <- pulse_data[[s]] - stats::coef(fit)[[1]]
    }
  } else {
    # mean_3s uses the configured short window; mean_30s the full pre-pulse period
    win <- if (identical(baseline_method, "mean_3s")) {
      sapfluxr::get_analysis_param("baseline.short_window_seconds")
    } else {
      pre_pulse
    }
    idx <- pulse_data$time_sec < 0 & pulse_data$time_sec >= -win
    for (s in sensors) {
      b <- mean(pulse_data[[s]][idx], na.rm = TRUE)
      pulse_data[[paste0("deltaT_", s)]] <- pulse_data[[s]] - b
    }
  }
  pulse_data
}

# Recompute heat pulse velocity for every method calculated for a pulse, using
# locally recomputed delta-T. HRM/MHR are ratio-based and vary with the pre-pulse
# baseline; Tmax methods are time-to-peak (baseline-independent) so their Vh is
# taken from the stored results.
pt_live_hpv <- function(pd, results, pulse_id, k, x) {
  # Restrict to this pulse first, dropping missing-pulse rows whose pulse_id is
  # NA (comparing NA == pulse_id would yield NA and break the if() branches).
  res_p <- results[!is.na(results$pulse_id) & results$pulse_id == pulse_id, , drop = FALSE]
  methods <- unique(res_p$method)
  methods <- methods[!is.na(methods)]
  if (length(methods) == 0) return(NULL)

  rows <- list()
  for (m in methods) {
    for (pos in c("outer", "inner")) {
      ds <- if (pos == "outer") "deltaT_do" else "deltaT_di"
      us <- if (pos == "outer") "deltaT_uo" else "deltaT_ui"
      vh <- NA_real_

      if (m == "HRM" && !is.null(k) && !is.null(x)) {
        # Use the HRM window the calculation actually used (falls back to config)
        rr <- res_p[res_p$method == "HRM" & res_p$sensor_position == pos, , drop = FALSE]
        w_start <- if (nrow(rr) > 0 && "hrm_window_start_sec" %in% names(rr) &&
                       !is.na(rr$hrm_window_start_sec[1])) {
          rr$hrm_window_start_sec[1]
        } else sapfluxr::get_analysis_param("hrm.start_seconds")
        w_end <- if (nrow(rr) > 0 && "hrm_window_end_sec" %in% names(rr) &&
                     !is.na(rr$hrm_window_end_sec[1])) {
          rr$hrm_window_end_sec[1]
        } else sapfluxr::get_analysis_param("hrm.end_seconds")
        win <- pd$time_sec >= w_start & pd$time_sec < w_end
        ratio <- mean(pd[[ds]][win] / pd[[us]][win], na.rm = TRUE)
        if (is.finite(ratio) && ratio > 0) vh <- (k / x) * log(ratio) * 3600
      } else if (m == "MHR" && !is.null(k) && !is.null(x)) {
        ratio <- max(pd[[ds]], na.rm = TRUE) / max(pd[[us]], na.rm = TRUE)
        if (is.finite(ratio) && ratio > 0) vh <- (k / x) * log(ratio) * 3600
      } else {
        # Tmax (or missing k/x): use the stored value (baseline-independent)
        rr <- res_p[res_p$method == m & res_p$sensor_position == pos, , drop = FALSE]
        if (nrow(rr) > 0) vh <- rr$Vh_cm_hr[1]
      }

      rows[[length(rows) + 1]] <- data.frame(
        Method = m, Sensor = pos,
        `Vh (cm/hr)` = round(vh, 2),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}


# UI ----
pulseTraceUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Controls
      column(
        width = 3,
        box(
          width = NULL,
          title = "Pulse Trace Controls",
          status = "primary",
          solidHeader = TRUE,

          h5("Selected Pulse"),
          verbatimTextOutput(ns("pulse_info")),

          hr(),

          h5("Sensor Position"),
          radioButtons(
            ns("sensor_position"),
            NULL,
            choices = c("Outer" = "outer", "Inner" = "inner"),
            selected = "outer",
            inline = TRUE
          ),

          hr(),

          h5("Calculation Parameters"),
          p(class = "help-text", style = "font-size: 0.9em;",
            "Vary the parameters to see the effect on the trace and velocities in real time. Defaults match the calculation step."),
          uiOutput(ns("param_controls_ui")),

          h5("Live Velocity"),
          shiny::tableOutput(ns("hpv_readout")),

          hr(),

          h5("Calculation Windows"),
          p(class = "help-text", style = "font-size: 0.9em;",
            "Show calculation windows used by each method. Heat pulse injection is at t=0."),

          uiOutput(ns("show_windows_ui")),

          uiOutput(ns("detrend_slope_ui")),

          hr(),

          actionButton(
            ns("clear_selection"),
            "Clear Selection",
            icon = icon("times"),
            class = "btn-default",
            style = "width: 100%;"
          )
        )
      ),

      # Pulse Trace Plot
      column(
        width = 9,
        box(
          width = NULL,
          title = "Pulse Temperature Trace",
          status = "success",
          solidHeader = TRUE,

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("pulse_trace_plot"), height = "600px"),
            type = 6,
            color = "#3c8dbc"
          )
        )
      )
    )
  )
}

# Server ----
pulseTraceServer <- function(id, heat_pulse_data, selected_pulse_id, vh_results = NULL,
                             plot_settings = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # ========================================================================
    # PERFORMANCE OPTIMISATION: Pre-filtered pulse data
    # ========================================================================
    # Create a reactive that ONLY contains the selected pulse measurements
    # This avoids filtering 11M+ rows every time we render the plot
    # OLD: Filter full dataset on every render (30-60 seconds)
    # NEW: Filter once when pulse changes (instant)
    selected_pulse_data <- reactive({
      pulse_id <- selected_pulse_id()

      if (is.null(pulse_id) || is.na(pulse_id)) {
        return(NULL)
      }

      req(heat_pulse_data())
      req(vh_results)

      data <- heat_pulse_data()

      # Filter measurements by pulse_id (now reliable after standardization!)
#       cat("  Looking for measurements with pulse_id:", pulse_id, "\n")

      pulse_measurements <- data$measurements %>%
        dplyr::filter(pulse_id == !!pulse_id)

      if (nrow(pulse_measurements) == 0) {
        cat("ERROR: No measurements found for pulse_id", pulse_id, "\n")
        return(NULL)
      }

      # DIAGNOSTIC: Verify we got the right data
      meas_datetime <- pulse_measurements$datetime[1]
#       cat("  ✓ Found", nrow(pulse_measurements), "measurements for pulse_id", pulse_id,
#           "starting at", as.character(meas_datetime), "\n")

      # Return just the measurements we need
      pulse_measurements
    })

    # Parameter controls, defaulting to the settings used in the calculation step
    output$param_controls_ui <- renderUI({
      res <- if (!is.null(vh_results)) vh_results() else NULL
      bm <- attr(res, "baseline_method") %||% "mean_30s"
      if (!bm %in% c("mean_30s", "mean_3s", "slope_intercept")) {
        bm <- if (grepl("3", bm)) "mean_3s" else
              if (grepl("slope", bm)) "slope_intercept" else "mean_30s"
      }
      selectInput(
        session$ns("pt_baseline_method"), "Pre-pulse baseline:",
        choices = c("30-second average" = "mean_30s",
                    "3-second average" = "mean_3s",
                    "Slope-intercept" = "slope_intercept"),
        selected = bm
      )
    })

    # Live velocity readout: recompute Vh (and sap velocity vs) for every method
    # calculated for the selected pulse, honouring the parameter controls above.
    output$hpv_readout <- renderTable({
      pulse_id <- selected_pulse_id()
      if (is.null(pulse_id) || is.na(pulse_id) || is.null(vh_results)) return(NULL)

      results <- vh_results()
      pd_raw <- selected_pulse_data()
      if (is.null(results) || is.null(pd_raw) || nrow(pd_raw) == 0) return(NULL)

      baseline_method <- input$pt_baseline_method %||%
        (attr(results, "baseline_method") %||% "mean_30s")
      pre_pulse <- attr(results, "pre_pulse") %||% 30

      pd <- pt_baseline_deltaT(pd_raw, baseline_method, pre_pulse)
      k  <- attr(results, "diffusivity")
      x  <- attr(results, "probe_spacing")

      pt_live_hpv(pd, results, pulse_id, k, x)
    }, striped = TRUE, spacing = "xs", width = "100%", na = "—")

    # Dynamic calculation windows UI based on available methods
    output$show_windows_ui <- renderUI({
      pulse_id <- selected_pulse_id()

      # Default choices (always available)
      all_choices <- list(
        "Baseline (Pre-pulse period, t<0)" = "baseline"
      )

      default_selected <- c("baseline")

      # If we have results, filter to methods calculated for this pulse
      if (!is.null(vh_results) && !is.null(pulse_id) && !is.na(pulse_id)) {
        results <- vh_results()
        if (!is.null(results) && nrow(results) > 0) {
          pulse_methods <- unique(results$method[results$pulse_id == pulse_id])

          # Add method choices based on what was calculated
          if ("HRM" %in% pulse_methods) {
            all_choices[["HRM Window (60-100s after pulse)"]] <- "HRM"
            default_selected <- c(default_selected, "HRM")
          }
          if ("MHR" %in% pulse_methods) {
            all_choices[["MHR Peaks (time to max ΔT)"]] <- "MHR"
          }
          if ("Tmax_Coh" %in% pulse_methods || "Tmax_Klu" %in% pulse_methods) {
            all_choices[["Tmax Peaks (time to max ΔT)"]] <- "Tmax"
          }
        }
      }

      checkboxGroupInput(
        session$ns("show_windows"),
        NULL,
        choices = all_choices,
        selected = default_selected
      )
    })

    # UI for detrending slope (reacts to the interactive pre-pulse control)
    output$detrend_slope_ui <- renderUI({
      bm <- input$pt_baseline_method
      if (is.null(bm)) {
        res <- if (!is.null(vh_results)) vh_results() else NULL
        bm <- attr(res, "baseline_method")
      }
      if (!is.null(bm) && grepl("slope", bm)) {
        tagList(
          hr(),
          checkboxInput(
            session$ns("detrend_slope"),
            "Remove pre-pulse drift (Detrend)",
            value = FALSE
          )
        )
      } else {
        NULL
      }
    })

    # Display selected pulse info
    output$pulse_info <- renderText({
      pulse_id <- selected_pulse_id()

      if (is.null(pulse_id) || is.na(pulse_id)) {
        return("No pulse selected.\n\nClick on a point in the time series plot above to view its pulse trace.")
      }

      # Use pre-filtered data (FAST!)
      pulse_data <- selected_pulse_data()

      if (is.null(pulse_data) || nrow(pulse_data) == 0) {
        return(paste("Pulse ID:", pulse_id, "\nNo data found"))
      }

      # Get datetime
      datetime <- pulse_data$datetime[1]

      paste0(
        "Pulse ID: ", pulse_id, "\n",
        "Date/Time: ", format(datetime, "%Y-%m-%d %H:%M:%S"), "\n",
        "Measurements: ", nrow(pulse_data)
      )
    })

    # Pulse trace plot
    output$pulse_trace_plot <- plotly::renderPlotly({
      style_config <- plot_settings()
      pulse_id <- selected_pulse_id()

      if (is.null(pulse_id) || is.na(pulse_id)) {
        # Empty plot with message
        return(
          plot_ly() %>%
            layout(
              title = list(text = "Click on a point in the time series plot to view pulse trace"),
              xaxis = list(title = "Time relative to heat pulse injection (seconds)"),
              yaxis = list(title = "\u0394T (\u00B0C)")
            )
        )
      }

      # Use pre-filtered data (FAST! No loading needed)
#       cat("Looking for pulse_id:", pulse_id, "\n")

      pulse_data <- selected_pulse_data()

      if (is.null(pulse_data)) {
        cat("No pulse data available (NULL from selected_pulse_data)\n")
        return(
          plot_ly() %>%
            layout(
              title = list(text = paste("No data for Pulse ID:", pulse_id)),
              xaxis = list(title = "Time relative to heat pulse injection (seconds)"),
              yaxis = list(title = "\u0394T (\u00B0C)")
            )
        )
      }

#       cat("Pulse data rows:", nrow(pulse_data), "\n")

      if (nrow(pulse_data) == 0) {
        return(
          plot_ly() %>%
            layout(
              title = list(text = paste("No data for Pulse ID:", pulse_id)),
              xaxis = list(title = "Time relative to heat pulse injection (seconds)"),
              yaxis = list(title = "\u0394T (\u00B0C)")
            )
        )
      }

      # 1. Determine baseline method and pre-pulse period.
      # Prefer the interactive control (defaults to the calculation-step method);
      # fall back to the vh_results attribute if the control is not yet bound.
      res <- if (!is.null(vh_results)) vh_results() else NULL
      baseline_method <- input$pt_baseline_method %||%
        (attr(res, "baseline_method") %||% "mean_30s")
      pt_pre_pulse <- attr(res, "pre_pulse") %||% 30

      # 2. Build the time axis in seconds from the detected sampling interval, with
      # t = 0 at the pulse injection (pre_pulse seconds in), rather than assuming
      # 1 Hz sampling and a 30-row pre-pulse.
      pt_interval <- pt_sampling_interval(pulse_data)
      pulse_data <- pulse_data %>%
        dplyr::mutate(time_sec = (dplyr::row_number() - 1) * pt_interval - pt_pre_pulse)

      # Shading window: mean_3s uses the configured short window; mean_30s the full pre-pulse
      baseline_window_sec <- if (grepl("3-sec|mean_3s", baseline_method)) {
        sapfluxr::get_analysis_param("baseline.short_window_seconds")
      } else {
        pt_pre_pulse
      }

      # 3. Calculate temperature deltas (ΔT) based on the method
      # Define the indices for the baseline window (relative to t=0)
      baseline_indices <- pulse_data$time_sec < 0 & pulse_data$time_sec >= -baseline_window_sec

      if (grepl("slope", baseline_method)) {
        # Slope-intercept: Fit linear models to full pre-pulse (-30 to 0)
        slope_indices <- pulse_data$time_sec < 0
        pre_data <- pulse_data[slope_indices, ]
        
        # Calculate and store model coefficients for each sensor
        fit_do <- lm(do ~ time_sec, data = pre_data)
        fit_di <- lm(di ~ time_sec, data = pre_data)
        fit_uo <- lm(uo ~ time_sec, data = pre_data)
        fit_ui <- lm(ui ~ time_sec, data = pre_data)
        
        slope_models <- list(
          do = list(m = coef(fit_do)[2], c = coef(fit_do)[1]),
          di = list(m = coef(fit_di)[2], c = coef(fit_di)[1]),
          uo = list(m = coef(fit_uo)[2], c = coef(fit_uo)[1]),
          ui = list(m = coef(fit_ui)[2], c = coef(fit_ui)[1])
        )
        
        detrend <- isTRUE(input$detrend_slope)
        
        pulse_data <- pulse_data %>%
          dplyr::mutate(
            deltaT_do = if(detrend) do - (slope_models$do$m * time_sec + slope_models$do$c) else do - slope_models$do$c,
            deltaT_di = if(detrend) di - (slope_models$di$m * time_sec + slope_models$di$c) else di - slope_models$di$c,
            deltaT_uo = if(detrend) uo - (slope_models$uo$m * time_sec + slope_models$uo$c) else uo - slope_models$uo$c,
            deltaT_ui = if(detrend) ui - (slope_models$ui$m * time_sec + slope_models$ui$c) else ui - slope_models$ui$c
          )
          
        attr(pulse_data, "slope_models") <- slope_models
      } else {
        # Standard mean baseline (30s or 3s)
        do_baseline <- mean(pulse_data$do[baseline_indices], na.rm = TRUE)
        di_baseline <- mean(pulse_data$di[baseline_indices], na.rm = TRUE)
        uo_baseline <- mean(pulse_data$uo[baseline_indices], na.rm = TRUE)
        ui_baseline <- mean(pulse_data$ui[baseline_indices], na.rm = TRUE)
        
        pulse_data <- pulse_data %>%
          dplyr::mutate(
            deltaT_do = do - do_baseline,
            deltaT_di = di - di_baseline,
            deltaT_uo = uo - uo_baseline,
            deltaT_ui = ui - ui_baseline
          )
      }

#       cat("Time range:", range(pulse_data$time_sec, na.rm = TRUE), "\n")

      # Create plot with temperature delta traces
      p <- plot_ly()

      # Get selected sensor position
      show_outer <- input$sensor_position == "outer"

      # Add traces based on sensor position
      # Consistent colour scheme: Downstream = Red, Upstream = Blue
      if (show_outer) {
        # Downstream outer
        style_ds <- get_plot_style(method = "Tmax_Coh", sensor = "outer", config = style_config)
        p <- p %>%
          add_trace(
            data = pulse_data,
            x = ~time_sec,
            y = ~deltaT_do,
            type = "scatter",
            mode = "lines+markers",
            name = "Downstream Outer",
            line = style_ds,
            marker = list(size = 4, color = style_ds$color)
          )

        # Upstream outer
        style_us <- get_plot_style(method = "HRM", sensor = "outer", config = style_config)
        p <- p %>%
          add_trace(
            data = pulse_data,
            x = ~time_sec,
            y = ~deltaT_uo,
            type = "scatter",
            mode = "lines+markers",
            name = "Upstream Outer",
            line = style_us,
            marker = list(size = 4, color = style_us$color)
          )
      } else {
        # Downstream inner
        style_ds <- get_plot_style(method = "Tmax_Coh", sensor = "inner", config = style_config)
        p <- p %>%
          add_trace(
            data = pulse_data,
            x = ~time_sec,
            y = ~deltaT_di,
            type = "scatter",
            mode = "lines+markers",
            name = "Downstream Inner",
            line = style_ds,
            marker = list(size = 4, color = style_ds$color)
          )

        # Upstream inner
        style_us <- get_plot_style(method = "HRM", sensor = "inner", config = style_config)
        p <- p %>%
          add_trace(
            data = pulse_data,
            x = ~time_sec,
            y = ~deltaT_ui,
            type = "scatter",
            mode = "lines+markers",
            name = "Upstream Inner",
            line = style_us,
            marker = list(size = 4, color = style_us$color)
          )
      }

      # Add slope trendlines if in Drift View
      slope_models <- attr(pulse_data, "slope_models")
      if (!is.null(slope_models) && !isTRUE(input$detrend_slope)) {
        if (show_outer) {
          # Downstream outer trend
          p <- p %>% add_trace(
            x = pulse_data$time_sec,
            y = slope_models$do$m * pulse_data$time_sec,
            type = "scatter",
            mode = "lines",
            name = "DS Drift Trend",
            line = list(color = style_ds$color, dash = "dash", width = 1),
            hoverinfo = "none",
            showlegend = FALSE
          )
          # Upstream outer trend
          p <- p %>% add_trace(
            x = pulse_data$time_sec,
            y = slope_models$uo$m * pulse_data$time_sec,
            type = "scatter",
            mode = "lines",
            name = "US Drift Trend",
            line = list(color = style_us$color, dash = "dash", width = 1),
            hoverinfo = "none",
            showlegend = FALSE
          )
        } else {
          # Downstream inner trend
          p <- p %>% add_trace(
            x = pulse_data$time_sec,
            y = slope_models$di$m * pulse_data$time_sec,
            type = "scatter",
            mode = "lines",
            name = "DS Drift Trend",
            line = list(color = style_ds$color, dash = "dash", width = 1),
            hoverinfo = "none",
            showlegend = FALSE
          )
          # Upstream inner trend
          p <- p %>% add_trace(
            x = pulse_data$time_sec,
            y = slope_models$ui$m * pulse_data$time_sec,
            type = "scatter",
            mode = "lines",
            name = "US Drift Trend",
            line = list(color = style_us$color, dash = "dash", width = 1),
            hoverinfo = "none",
            showlegend = FALSE
          )
        }
      }

      # Add vertical line at heat pulse injection (now at time 0)
      max_deltaT <- max(pulse_data$deltaT_do, pulse_data$deltaT_di,
                       pulse_data$deltaT_uo, pulse_data$deltaT_ui, na.rm = TRUE)
      p <- p %>%
        add_segments(
          x = 0, xend = 0,
          y = 0, yend = max_deltaT,
          line = list(color = "red", width = 2, dash = "dash"),
          name = "Heat Pulse Injection",
          showlegend = TRUE
        )

      # Calculate deltaT range for window shading
      min_deltaT <- min(pulse_data$deltaT_do, pulse_data$deltaT_di,
                       pulse_data$deltaT_uo, pulse_data$deltaT_ui, na.rm = TRUE)
      max_deltaT <- max(pulse_data$deltaT_do, pulse_data$deltaT_di,
                       pulse_data$deltaT_uo, pulse_data$deltaT_ui, na.rm = TRUE)

      # Add calculation windows as shaded regions
      # Check if show_windows input exists before using it
      show_windows <- if (!is.null(input$show_windows)) input$show_windows else character(0)

      if ("baseline" %in% show_windows) {
        # Baseline window (pre-pulse, from -baseline_window_sec to 0)
        p <- p %>%
          add_trace(
            x = c(-baseline_window_sec, 0, 0, -baseline_window_sec, -baseline_window_sec),
            y = c(min_deltaT, min_deltaT, max_deltaT, max_deltaT, min_deltaT),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(128, 128, 128, 0.1)",
            name = sprintf("Baseline (%s)", baseline_method),
            showlegend = TRUE,
            hoverinfo = "name"
          )
      }

      if ("HRM" %in% show_windows) {
        # HRM window - get actual window times from results
        # Get the sensor position being displayed
        position <- if (show_outer) "outer" else "inner"

        # Default window times (fallback)
        hrm_start <- 60
        hrm_end <- 100

        # Get the actual window times from vh_results for this pulse and method
        if (!is.null(vh_results)) {
          results <- vh_results()
          if (!is.null(results) && nrow(results) > 0) {
            cat("\nHRM Window Debug:\n")
            cat("  vh_results columns:", paste(names(results), collapse = ", "), "\n")

            # Check if window columns exist
            if (!("hrm_window_start_sec" %in% names(results))) {
              cat("  WARNING: hrm_window_start_sec column missing in vh_results!\n")
              cat("  Using default HRM window: 60-100s\n")
            } else if (!("hrm_window_end_sec" %in% names(results))) {
              cat("  WARNING: hrm_window_end_sec column missing in vh_results!\n")
              cat("  Using default HRM window: 60-100s\n")
            } else {
              # Columns exist, try to get values
        hrm_result <- results[!is.na(results$pulse_id) &
                              !is.na(results$method) &
                              !is.na(results$sensor_position) &
                              results$pulse_id == pulse_id &
                              results$method == "HRM" &
                              results$sensor_position == position, ]

              if (nrow(hrm_result) > 0) {
                window_start <- hrm_result$hrm_window_start_sec[1]
                window_end <- hrm_result$hrm_window_end_sec[1]

                cat("  Retrieved window values:", window_start, "-", window_end, "\n")

                # Check if values are NA (calculation may have failed for this pulse)
                if (!is.na(window_start) && !is.na(window_end)) {
                  hrm_start <- window_start
                  hrm_end <- window_end
                  cat("  Using HRM window from results:", hrm_start, "-", hrm_end, "s\n")
                } else {
                  cat("  WARNING: Window values are NA for this pulse - using defaults (60-100s)\n")
                  cat("  This usually means the HRM calculation failed or wasn't performed for this pulse\n")
                }
              } else {
                cat("  No HRM result found for pulse", pulse_id, position, "- using defaults\n")
              }
            }
          }
        }

        # Get temperature values at window boundaries for both sensors
        if (show_outer) {
          # Find temperatures at window start
          start_idx <- which.min(abs(pulse_data$time_sec - hrm_start))
          do_temp_start <- if (length(start_idx) > 0) pulse_data$deltaT_do[start_idx] else NA
          uo_temp_start <- if (length(start_idx) > 0) pulse_data$deltaT_uo[start_idx] else NA
          # Find temperatures at window end
          end_idx <- which.min(abs(pulse_data$time_sec - hrm_end))
          do_temp_end <- if (length(end_idx) > 0) pulse_data$deltaT_do[end_idx] else NA
          uo_temp_end <- if (length(end_idx) > 0) pulse_data$deltaT_uo[end_idx] else NA
        } else {
          # Find temperatures at window start
          start_idx <- which.min(abs(pulse_data$time_sec - hrm_start))
          di_temp_start <- if (length(start_idx) > 0) pulse_data$deltaT_di[start_idx] else NA
          ui_temp_start <- if (length(start_idx) > 0) pulse_data$deltaT_ui[start_idx] else NA
          # Find temperatures at window end
          end_idx <- which.min(abs(pulse_data$time_sec - hrm_end))
          di_temp_end <- if (length(end_idx) > 0) pulse_data$deltaT_di[end_idx] else NA
          ui_temp_end <- if (length(end_idx) > 0) pulse_data$deltaT_ui[end_idx] else NA
        }

        p <- p %>%
          add_trace(
            x = c(hrm_start, hrm_end, hrm_end, hrm_start, hrm_start),
            y = c(min_deltaT, min_deltaT, max_deltaT, max_deltaT, min_deltaT),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(31, 119, 180, 0.15)",
            name = sprintf("HRM Window (%d-%ds after pulse)", hrm_start, hrm_end),
            showlegend = TRUE,
            hoverinfo = "name"
          )

        # Add vertical lines and points at window boundaries (only if valid temperature data exists)
        if (show_outer) {
          # Check if we have valid temperature data
          if (!is.na(do_temp_start) && !is.na(uo_temp_start) &&
              !is.na(do_temp_end) && !is.na(uo_temp_end)) {
            max_start <- max(do_temp_start, uo_temp_start, na.rm = TRUE)
            max_end <- max(do_temp_end, uo_temp_end, na.rm = TRUE)

            p <- p %>%
              add_segments(
                x = hrm_start, xend = hrm_start,
                y = 0, yend = max_start,
                line = list(color = "#1f77b4", width = 1.5, dash = "dot"),
                name = "HRM window start",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = c(hrm_start, hrm_start),
                y = c(do_temp_start, uo_temp_start),
                marker = list(size = 6, color = "#1f77b4"),
                name = "HRM start points",
                showlegend = FALSE
              ) %>%
              add_segments(
                x = hrm_end, xend = hrm_end,
                y = 0, yend = max_end,
                line = list(color = "#1f77b4", width = 1.5, dash = "dot"),
                name = "HRM window end",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = c(hrm_end, hrm_end),
                y = c(do_temp_end, uo_temp_end),
                marker = list(size = 6, color = "#1f77b4"),
                name = "HRM end points",
                showlegend = FALSE
              )
          }
        } else {
          # Check if we have valid temperature data
          if (!is.na(di_temp_start) && !is.na(ui_temp_start) &&
              !is.na(di_temp_end) && !is.na(ui_temp_end)) {
            max_start <- max(di_temp_start, ui_temp_start, na.rm = TRUE)
            max_end <- max(di_temp_end, ui_temp_end, na.rm = TRUE)

            p <- p %>%
              add_segments(
                x = hrm_start, xend = hrm_start,
                y = 0, yend = max_start,
                line = list(color = "#1f77b4", width = 1.5, dash = "dot"),
                name = "HRM window start",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = c(hrm_start, hrm_start),
                y = c(di_temp_start, ui_temp_start),
                marker = list(size = 6, color = "#1f77b4"),
                name = "HRM start points",
                showlegend = FALSE
              ) %>%
              add_segments(
                x = hrm_end, xend = hrm_end,
                y = 0, yend = max_end,
                line = list(color = "#1f77b4", width = 1.5, dash = "dot"),
                name = "HRM window end",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = c(hrm_end, hrm_end),
                y = c(di_temp_end, ui_temp_end),
                marker = list(size = 6, color = "#1f77b4"),
                name = "HRM end points",
                showlegend = FALSE
              )
          }
        }
      }

      if (FALSE) {
        # HRMXa/HRMXb removed — methods no longer supported

        # Get the actual window times from vh_results for this pulse and method
        if (!is.null(vh_results)) {
          results <- vh_results()
          if (!is.null(results) && nrow(results) > 0) {
            
            cat("\n=== HRMXa Window Lookup Debug ===\n")
            cat("  Looking for pulse_id:", pulse_id, "  type:", class(pulse_id), "\n")
            cat("  Sensor position:", position, "\n")
            cat("  Total rows in results:", nrow(results), "\n")
            
            # Check what pulse_ids are available
            available_pulses <- unique(results$pulse_id)
            cat("  Available pulse_ids (first 10):", paste(head(available_pulses, 10), collapse=", "), "\n")
            cat("  Pulse_id type in results:", class(results$pulse_id), "\n")
            
            # Check for HRMXa method
            hrmxa_rows <- results[results$method == "HRMXa", ]
            cat("  Total HRMXa rows:", nrow(hrmxa_rows), "\n")
            
            if (nrow(hrmxa_rows) > 0) {
              cat("  HRMXa pulse_ids:", paste(unique(hrmxa_rows$pulse_id), collapse=", "), "\n")
              cat("  HRMXa positions:", paste(unique(hrmxa_rows$sensor_position), collapse=", "), "\n")
            }
            
            # Try the filter - exclude NA rows explicitly
            hrmxa_result <- results[!is.na(results$pulse_id) &
                                    !is.na(results$method) &
                                    !is.na(results$sensor_position) &
                                    results$pulse_id == pulse_id &
                                    results$method == "HRMXa" &
                                    results$sensor_position == position, ]
            
            cat("  Filtered rows for this pulse:", nrow(hrmxa_result), "\n")

            if (nrow(hrmxa_result) > 0 &&
                "hrmxa_window_start_sec" %in% names(results) &&
                "hrmxa_window_end_sec" %in% names(results)) {
              window_start <- hrmxa_result$hrmxa_window_start_sec[1]
              window_end <- hrmxa_result$hrmxa_window_end_sec[1]

              # Check if values are NA
              if (!is.na(window_start) && !is.na(window_end)) {
                hrm_start <- window_start
                hrm_end <- window_end
                cat("  ✓ Using HRMXa window from results:", hrm_start, "-", hrm_end, "s\n")
              } else {
                cat("  ✗ WARNING: HRMXa window values are NA - using defaults (60-100s)\n")
              }
            } else {
              if (nrow(hrmxa_result) == 0) {
                cat("  ✗ WARNING: No matching HRMXa rows found for this pulse/position\n")
              } else {
                cat("  ✗ WARNING: Window columns not found in results\n")
              }
              cat("  → Falling back to HRM defaults (60-100s)\n")
            }
            cat("=== End Debug ===\n\n")
          }
        }

        # Get temperature values at window boundaries for both sensors
        if (show_outer) {
          # Find temperatures at window start
          start_idx <- which.min(abs(pulse_data$time_sec - hrm_start))
          do_temp_start <- if (length(start_idx) > 0) pulse_data$deltaT_do[start_idx] else NA
          uo_temp_start <- if (length(start_idx) > 0) pulse_data$deltaT_uo[start_idx] else NA
          # Find temperatures at window end
          end_idx <- which.min(abs(pulse_data$time_sec - hrm_end))
          do_temp_end <- if (length(end_idx) > 0) pulse_data$deltaT_do[end_idx] else NA
          uo_temp_end <- if (length(end_idx) > 0) pulse_data$deltaT_uo[end_idx] else NA
        } else {
          # Find temperatures at window start
          start_idx <- which.min(abs(pulse_data$time_sec - hrm_start))
          di_temp_start <- if (length(start_idx) > 0) pulse_data$deltaT_di[start_idx] else NA
          ui_temp_start <- if (length(start_idx) > 0) pulse_data$deltaT_ui[start_idx] else NA
          # Find temperatures at window end
          end_idx <- which.min(abs(pulse_data$time_sec - hrm_end))
          di_temp_end <- if (length(end_idx) > 0) pulse_data$deltaT_di[end_idx] else NA
          ui_temp_end <- if (length(end_idx) > 0) pulse_data$deltaT_ui[end_idx] else NA
        }

        # Add shaded window (royal blue)
        p <- p %>%
          add_trace(
            x = c(hrm_start, hrm_end, hrm_end, hrm_start, hrm_start),
            y = c(min_deltaT, min_deltaT, max_deltaT, max_deltaT, min_deltaT),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(65, 105, 225, 0.2)",  # Royal blue
            name = sprintf("HRMXa Window (%d-%ds after pulse)", hrm_start, hrm_end),
            showlegend = TRUE,
            hoverinfo = "name"
          )

        # Add vertical lines and points at window boundaries (only if valid temperature data exists)
        if (show_outer) {
          # Check if we have valid temperature data
          if (!is.na(do_temp_start) && !is.na(uo_temp_start) &&
              !is.na(do_temp_end) && !is.na(uo_temp_end)) {
            max_start <- max(do_temp_start, uo_temp_start, na.rm = TRUE)
            max_end <- max(do_temp_end, uo_temp_end, na.rm = TRUE)

            p <- p %>%
              add_segments(
                x = hrm_start, xend = hrm_start,
                y = 0, yend = max_start,
                line = list(color = "#0000CD", width = 1.5, dash = "dot"),
                name = "HRMXa window start",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = c(hrm_start, hrm_start),
                y = c(do_temp_start, uo_temp_start),
                marker = list(size = 6, color = "#0000CD"),
                name = "HRMXa start points",
                showlegend = FALSE
              ) %>%
              add_segments(
                x = hrm_end, xend = hrm_end,
                y = 0, yend = max_end,
                line = list(color = "#0000CD", width = 1.5, dash = "dot"),
                name = "HRMXa window end",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = c(hrm_end, hrm_end),
                y = c(do_temp_end, uo_temp_end),
                marker = list(size = 6, color = "#0000CD"),
                name = "HRMXa end points",
                showlegend = FALSE
              )
          }
        } else {
          # Check if we have valid temperature data
          if (!is.na(di_temp_start) && !is.na(ui_temp_start) &&
              !is.na(di_temp_end) && !is.na(ui_temp_end)) {
            max_start <- max(di_temp_start, ui_temp_start, na.rm = TRUE)
            max_end <- max(di_temp_end, ui_temp_end, na.rm = TRUE)

            p <- p %>%
              add_segments(
                x = hrm_start, xend = hrm_start,
                y = 0, yend = max_start,
                line = list(color = "#0000CD", width = 1.5, dash = "dot"),
                name = "HRMXa window start",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = c(hrm_start, hrm_start),
                y = c(di_temp_start, ui_temp_start),
                marker = list(size = 6, color = "#0000CD"),
                name = "HRMXa start points",
                showlegend = FALSE
              ) %>%
              add_segments(
                x = hrm_end, xend = hrm_end,
                y = 0, yend = max_end,
                line = list(color = "#0000CD", width = 1.5, dash = "dot"),
                name = "HRMXa window end",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = c(hrm_end, hrm_end),
                y = c(di_temp_end, ui_temp_end),
                marker = list(size = 6, color = "#0000CD"),
                name = "HRMXa end points",
                showlegend = FALSE
              )
          }
        }
      }

      if (FALSE) {
        # HRMXb removed — method no longer supported

        # Default window times
        downstream_start <- 60
        downstream_end <- 100
        upstream_start <- 60
        upstream_end <- 100

        # Get the actual window times from vh_results for this pulse and method
        if (!is.null(vh_results)) {
          results <- vh_results()
          if (!is.null(results) && nrow(results) > 0) {
            
            cat("\n=== HRMXb Window Lookup Debug ===\n")
            cat("  Looking for pulse_id:", pulse_id, "  type:", class(pulse_id), "\n")
            cat("  Sensor position:", position, "\n")
            
            # Check for HRMXb method
            hrmxb_rows <- results[results$method == "HRMXb", ]
            cat("  Total HRMXb rows:", nrow(hrmxb_rows), "\n")
            
            if (nrow(hrmxb_rows) > 0) {
              cat("  HRMXb pulse_ids:", paste(unique(hrmxb_rows$pulse_id), collapse=", "), "\n")
              cat("  HRMXb positions:", paste(unique(hrmxb_rows$sensor_position), collapse=", "), "\n")
            }

            # Try the filter - exclude NA rows explicitly
            hrmxb_result <- results[!is.na(results$pulse_id) &
                                    !is.na(results$method) &
                                    !is.na(results$sensor_position) &
                                    results$pulse_id == pulse_id &
                                    results$method == "HRMXb" &
                                    results$sensor_position == position, ]

            cat("  Filtered rows for this pulse:", nrow(hrmxb_result), "\n")

            if (nrow(hrmxb_result) > 0 &&
                "hrmxb_downstream_start_sec" %in% names(results) &&
                "hrmxb_upstream_start_sec" %in% names(results)) {

              ds_start <- hrmxb_result$hrmxb_downstream_start_sec[1]
              ds_end <- hrmxb_result$hrmxb_downstream_end_sec[1]
              us_start <- hrmxb_result$hrmxb_upstream_start_sec[1]
              us_end <- hrmxb_result$hrmxb_upstream_end_sec[1]

              cat("  Retrieved HRMXb windows - DS:", ds_start, "-", ds_end, ", US:", us_start, "-", us_end, "\n")

              # Check if values are NA
              if (!is.na(ds_start) && !is.na(ds_end) && !is.na(us_start) && !is.na(us_end)) {
                downstream_start <- ds_start
                downstream_end <- ds_end
                upstream_start <- us_start
                upstream_end <- us_end
                cat("  ✓ Using HRMXb windows from results\n")
              } else {
                cat("  ✗ WARNING: HRMXb window values are NA - using defaults (60-100s)\n")
              }
            } else {
              if (nrow(hrmxb_result) == 0) {
                cat("  ✗ WARNING: No matching HRMXb rows found for this pulse/position\n")
              } else {
                cat("  ✗ WARNING: Window columns not found in results\n")
              }
              cat("  → Falling back to defaults (60-100s)\n")
            }
            cat("=== End Debug ===\n\n")
          }
        }

        # Get temperature values at window boundaries
        if (show_outer) {
          # Find temperatures at downstream window boundaries
          ds_start_idx <- which.min(abs(pulse_data$time_sec - downstream_start))
          do_temp_start <- if (length(ds_start_idx) > 0) pulse_data$deltaT_do[ds_start_idx] else NA
          ds_end_idx <- which.min(abs(pulse_data$time_sec - downstream_end))
          do_temp_end <- if (length(ds_end_idx) > 0) pulse_data$deltaT_do[ds_end_idx] else NA

          # Find temperatures at upstream window boundaries
          us_start_idx <- which.min(abs(pulse_data$time_sec - upstream_start))
          uo_temp_start <- if (length(us_start_idx) > 0) pulse_data$deltaT_uo[us_start_idx] else NA
          us_end_idx <- which.min(abs(pulse_data$time_sec - upstream_end))
          uo_temp_end <- if (length(us_end_idx) > 0) pulse_data$deltaT_uo[us_end_idx] else NA
        } else {
          # Find temperatures at downstream window boundaries
          ds_start_idx <- which.min(abs(pulse_data$time_sec - downstream_start))
          di_temp_start <- if (length(ds_start_idx) > 0) pulse_data$deltaT_di[ds_start_idx] else NA
          ds_end_idx <- which.min(abs(pulse_data$time_sec - downstream_end))
          di_temp_end <- if (length(ds_end_idx) > 0) pulse_data$deltaT_di[ds_end_idx] else NA

          # Find temperatures at upstream window boundaries
          us_start_idx <- which.min(abs(pulse_data$time_sec - upstream_start))
          ui_temp_start <- if (length(us_start_idx) > 0) pulse_data$deltaT_ui[us_start_idx] else NA
          us_end_idx <- which.min(abs(pulse_data$time_sec - upstream_end))
          ui_temp_end <- if (length(us_end_idx) > 0) pulse_data$deltaT_ui[us_end_idx] else NA
        }

        # Add downstream window (light red/pink)
        p <- p %>%
          add_trace(
            x = c(downstream_start, downstream_end, downstream_end, downstream_start, downstream_start),
            y = c(min_deltaT, min_deltaT, max_deltaT, max_deltaT, min_deltaT),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(255, 182, 198, 0.3)",  # Light pink
            name = sprintf("HRMXb Downstream (%d-%ds)", downstream_start, downstream_end),
            showlegend = TRUE,
            hoverinfo = "name"
          )

        # Add upstream window (light green)
        p <- p %>%
          add_trace(
            x = c(upstream_start, upstream_end, upstream_end, upstream_start, upstream_start),
            y = c(min_deltaT, min_deltaT, max_deltaT, max_deltaT, min_deltaT),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(144, 238, 144, 0.3)",  # Light green
            name = sprintf("HRMXb Upstream (%d-%ds)", upstream_start, upstream_end),
            showlegend = TRUE,
            hoverinfo = "name"
          )

        # Add vertical lines and points for downstream window (only if valid temperature data exists)
        if (show_outer) {
          # Check if we have valid temperature data
          if (!is.na(do_temp_start) && !is.na(do_temp_end) &&
              !is.na(uo_temp_start) && !is.na(uo_temp_end)) {
            p <- p %>%
              add_segments(
                x = downstream_start, xend = downstream_start,
                y = 0, yend = do_temp_start,
                line = list(color = "#DC143C", width = 1.5, dash = "dot"),
                name = "HRMXb downstream start",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = downstream_start, y = do_temp_start,
                marker = list(size = 6, color = "#DC143C"),
                name = "HRMXb downstream start point",
                showlegend = FALSE
              ) %>%
              add_segments(
                x = downstream_end, xend = downstream_end,
                y = 0, yend = do_temp_end,
                line = list(color = "#DC143C", width = 1.5, dash = "dot"),
                name = "HRMXb downstream end",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = downstream_end, y = do_temp_end,
                marker = list(size = 6, color = "#DC143C"),
                name = "HRMXb downstream end point",
                showlegend = FALSE
              ) %>%
              # Add vertical lines and points for upstream window (use UPSTREAM sensor only)
              add_segments(
                x = upstream_start, xend = upstream_start,
                y = 0, yend = uo_temp_start,
                line = list(color = "#228B22", width = 1.5, dash = "dot"),
                name = "HRMXb upstream start",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = upstream_start, y = uo_temp_start,
                marker = list(size = 6, color = "#228B22"),
                name = "HRMXb upstream start point",
                showlegend = FALSE
              ) %>%
              add_segments(
                x = upstream_end, xend = upstream_end,
                y = 0, yend = uo_temp_end,
                line = list(color = "#228B22", width = 1.5, dash = "dot"),
                name = "HRMXb upstream end",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = upstream_end, y = uo_temp_end,
                marker = list(size = 6, color = "#228B22"),
                name = "HRMXb upstream end point",
                showlegend = FALSE
              )
          }
        } else {
          # Check if we have valid temperature data
          if (!is.na(di_temp_start) && !is.na(di_temp_end) &&
              !is.na(ui_temp_start) && !is.na(ui_temp_end)) {
            p <- p %>%
              add_segments(
                x = downstream_start, xend = downstream_start,
                y = 0, yend = di_temp_start,
                line = list(color = "#DC143C", width = 1.5, dash = "dot"),
                name = "HRMXb downstream start",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = downstream_start, y = di_temp_start,
                marker = list(size = 6, color = "#DC143C"),
                name = "HRMXb downstream start point",
                showlegend = FALSE
              ) %>%
              add_segments(
                x = downstream_end, xend = downstream_end,
                y = 0, yend = di_temp_end,
                line = list(color = "#DC143C", width = 1.5, dash = "dot"),
                name = "HRMXb downstream end",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = downstream_end, y = di_temp_end,
                marker = list(size = 6, color = "#DC143C"),
                name = "HRMXb downstream end point",
                showlegend = FALSE
              ) %>%
              # Add vertical lines and points for upstream window (use UPSTREAM sensor only)
              add_segments(
                x = upstream_start, xend = upstream_start,
                y = 0, yend = ui_temp_start,
                line = list(color = "#228B22", width = 1.5, dash = "dot"),
                name = "HRMXb upstream start",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = upstream_start, y = ui_temp_start,
                marker = list(size = 6, color = "#228B22"),
                name = "HRMXb upstream start point",
                showlegend = FALSE
              ) %>%
              add_segments(
                x = upstream_end, xend = upstream_end,
                y = 0, yend = ui_temp_end,
                line = list(color = "#228B22", width = 1.5, dash = "dot"),
                name = "HRMXb upstream end",
                showlegend = FALSE
              ) %>%
              add_markers(
                x = upstream_end, y = ui_temp_end,
                marker = list(size = 6, color = "#228B22"),
                name = "HRMXb upstream end point",
                showlegend = FALSE
              )
          }
        }
      }

      if ("MHR" %in% input$show_windows) {
        # MHR finds peaks in temperature data
        # Show range between upstream and downstream peaks

        cat("\nMHR Window Debug:\n")
        if (!is.null(vh_results)) {
          results <- vh_results()
          if (!is.null(results) && nrow(results) > 0) {
            cat("  vh_results columns:", paste(names(results), collapse = ", "), "\n")
            # Check if window columns exist
            if ("hrm_window_start_sec" %in% names(results) && "hrm_window_end_sec" %in% names(results)) {
              cat("  Window columns FOUND in results\n")
            } else {
            }
          }
        }

        if (show_outer) {
          do_peak_idx <- which.max(pulse_data$deltaT_do)
          uo_peak_idx <- which.max(pulse_data$deltaT_uo)

          # Check if we have valid peak data
          if (length(do_peak_idx) > 0 && length(uo_peak_idx) > 0) {
            do_peak_time <- pulse_data$time_sec[do_peak_idx]
            uo_peak_time <- pulse_data$time_sec[uo_peak_idx]
            do_peak_temp <- pulse_data$deltaT_do[do_peak_idx]
            uo_peak_temp <- pulse_data$deltaT_uo[uo_peak_idx]

            cat("  Found peaks - DO:", do_peak_time, "s, UO:", uo_peak_time, "s\n")

            # Check if peak temperatures are valid (not NA)
            if (!is.na(do_peak_temp) && !is.na(uo_peak_temp) &&
                !is.na(do_peak_time) && !is.na(uo_peak_time)) {
              # Times are already relative to pulse injection
              # Add shaded window
              p <- p %>%
                add_trace(
                  x = c(uo_peak_time, do_peak_time, do_peak_time, uo_peak_time, uo_peak_time),
                  y = c(min_deltaT, min_deltaT, max_deltaT, max_deltaT, min_deltaT),
                  type = "scatter",
                  mode = "none",
                  fill = "toself",
                  fillcolor = "rgba(255, 127, 14, 0.15)",
                  name = sprintf("MHR Peaks (%.0f-%.0fs after pulse)", uo_peak_time, do_peak_time),
                  showlegend = TRUE,
                  hoverinfo = "name"
                ) %>%
                # Add vertical line at downstream peak with point
                add_segments(
                  x = do_peak_time, xend = do_peak_time,
                  y = 0, yend = do_peak_temp,
                  line = list(color = "#FF8C00", width = 2, dash = "dot"),
                  name = "DO peak",
                  showlegend = FALSE
                ) %>%
                add_markers(
                  x = do_peak_time, y = do_peak_temp,
                  marker = list(size = 8, color = "#FF8C00"),
                  name = "DO peak point",
                  showlegend = FALSE
                ) %>%
                # Add vertical line at upstream peak with point
                add_segments(
                  x = uo_peak_time, xend = uo_peak_time,
                  y = 0, yend = uo_peak_temp,
                  line = list(color = "#FF8C00", width = 2, dash = "dot"),
                  name = "UO peak",
                  showlegend = FALSE
                ) %>%
                add_markers(
                  x = uo_peak_time, y = uo_peak_temp,
                  marker = list(size = 8, color = "#FF8C00"),
                  name = "UO peak point",
                  showlegend = FALSE
                )
            }
          }
        } else {
          di_peak_idx <- which.max(pulse_data$deltaT_di)
          ui_peak_idx <- which.max(pulse_data$deltaT_ui)

          # Check if we have valid peak data
          if (length(di_peak_idx) > 0 && length(ui_peak_idx) > 0) {
            di_peak_time <- pulse_data$time_sec[di_peak_idx]
            ui_peak_time <- pulse_data$time_sec[ui_peak_idx]
            di_peak_temp <- pulse_data$deltaT_di[di_peak_idx]
            ui_peak_temp <- pulse_data$deltaT_ui[ui_peak_idx]

            # Check if peak temperatures are valid (not NA)
            if (!is.na(di_peak_temp) && !is.na(ui_peak_temp) &&
                !is.na(di_peak_time) && !is.na(ui_peak_time)) {
              # Times are already relative to pulse injection
              # Add shaded window
              p <- p %>%
                add_trace(
                  x = c(ui_peak_time, di_peak_time, di_peak_time, ui_peak_time, ui_peak_time),
                  y = c(min_deltaT, min_deltaT, max_deltaT, max_deltaT, min_deltaT),
                  type = "scatter",
                  mode = "none",
                  fill = "toself",
                  fillcolor = "rgba(255, 127, 14, 0.15)",
                  name = sprintf("MHR Peaks (%.0f-%.0fs after pulse)", ui_peak_time, di_peak_time),
                  showlegend = TRUE,
                  hoverinfo = "name"
                ) %>%
                # Add vertical line at downstream peak with point
                add_segments(
                  x = di_peak_time, xend = di_peak_time,
                  y = 0, yend = di_peak_temp,
                  line = list(color = "#FF8C00", width = 2, dash = "dot"),
                  name = "DI peak",
                  showlegend = FALSE
                ) %>%
                add_markers(
                  x = di_peak_time, y = di_peak_temp,
                  marker = list(size = 8, color = "#FF8C00"),
                  name = "DI peak point",
                  showlegend = FALSE
                ) %>%
                # Add vertical line at upstream peak with point
                add_segments(
                  x = ui_peak_time, xend = ui_peak_time,
                  y = 0, yend = ui_peak_temp,
                  line = list(color = "#FF8C00", width = 2, dash = "dot"),
                  name = "UI peak",
                  showlegend = FALSE
                ) %>%
                add_markers(
                  x = ui_peak_time, y = ui_peak_temp,
                  marker = list(size = 8, color = "#FF8C00"),
                  name = "UI peak point",
                  showlegend = FALSE
                )
            }
          }
        }
      }

      if ("Tmax" %in% input$show_windows) {
        # Tmax uses time-to-peak as a single point, not a window
        # Show vertical lines at downstream peak times with markers
        if (show_outer) {
          do_peak_idx <- which.max(pulse_data$deltaT_do)

          # Check if we have valid peak data
          if (length(do_peak_idx) > 0) {
            do_peak_time <- pulse_data$time_sec[do_peak_idx]
            do_peak_temp <- pulse_data$deltaT_do[do_peak_idx]

            # Check if peak is valid (not NA)
            if (!is.na(do_peak_temp) && !is.na(do_peak_time)) {
              # Time is already relative to pulse injection
              p <- p %>%
                add_segments(
                  x = do_peak_time, xend = do_peak_time,
                  y = 0, yend = do_peak_temp,
                  line = list(color = "#d62728", width = 2, dash = "dot"),
                  name = sprintf("Tmax DO (%.0fs after pulse)", do_peak_time),
                  showlegend = TRUE
                ) %>%
                add_markers(
                  x = do_peak_time, y = do_peak_temp,
                  marker = list(size = 8, color = "#d62728"),
                  name = "Tmax DO point",
                  showlegend = FALSE
                )
            }
          }
        } else {
          di_peak_idx <- which.max(pulse_data$deltaT_di)

          # Check if we have valid peak data
          if (length(di_peak_idx) > 0) {
            di_peak_time <- pulse_data$time_sec[di_peak_idx]
            di_peak_temp <- pulse_data$deltaT_di[di_peak_idx]

            # Check if peak is valid (not NA)
            if (!is.na(di_peak_temp) && !is.na(di_peak_time)) {
              # Time is already relative to pulse injection
              p <- p %>%
                add_segments(
                  x = di_peak_time, xend = di_peak_time,
                  y = 0, yend = di_peak_temp,
                  line = list(color = "#ff7f0e", width = 2, dash = "dot"),
                  name = sprintf("Tmax DI (%.0fs after pulse)", di_peak_time),
                  showlegend = TRUE
                ) %>%
                add_markers(
                  x = di_peak_time, y = di_peak_temp,
                  marker = list(size = 8, color = "#ff7f0e"),
                  name = "Tmax DI point",
                  showlegend = FALSE
                )
            }
          }
        }
      }

      # Apply standard layout
      base_layout <- get_standard_layout(
        title = paste("Pulse Temperature Trace -", format(pulse_data$datetime[1], "%Y-%m-%d %H:%M:%S")),
        xtitle = "Time relative to heat pulse injection (seconds)",
        ytitle = "\u0394T (\u00B0C)",
        uirevision = "pulse_trace_zoom"
      )

      base_layout$xaxis$range <- c(min(pulse_data$time_sec, na.rm = TRUE), max(pulse_data$time_sec, na.rm = TRUE))
      base_layout$yaxis$rangemode <- "tozero"
      base_layout$hovermode <- "x unified"

      annot_list <- list(
        list(
          x = 0,
          y = max_deltaT * 0.95,
          text = "Heat Pulse<br>Injection<br>(t = 0)",
          showarrow = FALSE,
          xanchor = "left",
          xshift = 5,
          font = list(size = 10, color = "red"),
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "red",
          borderwidth = 1,
          borderpad = 4
        )
      )

      # Add slope annotations if in Drift View
      slope_models <- attr(pulse_data, "slope_models")
      if (!is.null(slope_models) && !isTRUE(input$detrend_slope)) {
        if (show_outer) {
          annot_list[[length(annot_list) + 1]] <- list(
            x = -baseline_window_sec / 2,
            y = slope_models$do$m * (-baseline_window_sec / 2) + 0.05,
            text = sprintf("DS Drift: T = %.4ft + %.2f", slope_models$do$m, slope_models$do$c),
            showarrow = FALSE,
            font = list(size = 10, color = style_ds$color),
            bgcolor = "rgba(255,255,255,0.7)"
          )
          annot_list[[length(annot_list) + 1]] <- list(
            x = -baseline_window_sec / 2,
            y = slope_models$uo$m * (-baseline_window_sec / 2) - 0.05,
            text = sprintf("US Drift: T = %.4ft + %.2f", slope_models$uo$m, slope_models$uo$c),
            showarrow = FALSE,
            font = list(size = 10, color = style_us$color),
            bgcolor = "rgba(255,255,255,0.7)"
          )
        } else {
          annot_list[[length(annot_list) + 1]] <- list(
            x = -baseline_window_sec / 2,
            y = slope_models$di$m * (-baseline_window_sec / 2) + 0.05,
            text = sprintf("DS Drift: T = %.4ft + %.2f", slope_models$di$m, slope_models$di$c),
            showarrow = FALSE,
            font = list(size = 10, color = style_ds$color),
            bgcolor = "rgba(255,255,255,0.7)"
          )
          annot_list[[length(annot_list) + 1]] <- list(
            x = -baseline_window_sec / 2,
            y = slope_models$ui$m * (-baseline_window_sec / 2) - 0.05,
            text = sprintf("US Drift: T = %.4ft + %.2f", slope_models$ui$m, slope_models$ui$c),
            showarrow = FALSE,
            font = list(size = 10, color = style_us$color),
            bgcolor = "rgba(255,255,255,0.7)"
          )
        }
      }

      p <- p %>%
        layout(
          title = base_layout$title,
          xaxis = base_layout$xaxis,
          yaxis = base_layout$yaxis,
          showlegend = base_layout$showlegend,
          legend = base_layout$legend,
          hovermode = base_layout$hovermode,
          uirevision = base_layout$uirevision,
          plot_bgcolor = base_layout$plot_bgcolor,
          paper_bgcolor = base_layout$paper_bgcolor,
          annotations = annot_list
        ) %>%
        apply_standard_plotly_config(filename = paste0("pulse_trace_", pulse_id), add_csv_download = TRUE)

      p
    })

    # Clear selection
    observeEvent(input$clear_selection, {
      # This will be handled by parent module
      # REMOVED: Cannot modify parent reactive -       selected_pulse_id(NULL)
    })

  })
}
