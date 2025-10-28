#' Pulse Trace Viewer Module
#'
#' Shiny module for viewing individual pulse temperature traces
#'
#' @param id Module ID
#' @param heat_pulse_data Reactive containing heat_pulse_data
#' @param selected_pulse_id Reactive containing selected pulse ID from plot click
#' @return None (displays pulse trace plot)
#'

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

          h5("Calculation Windows"),
          p(class = "help-text", style = "font-size: 0.9em;",
            "Show calculation windows used by each method. Heat pulse injection is at t=0."),

          checkboxGroupInput(
            ns("show_windows"),
            NULL,
            choices = c(
              "HRM Window (60-100s after pulse)" = "HRM",
              "MHR Peaks (time to max \u0394T)" = "MHR",
              "Tmax Peaks (time to max \u0394T)" = "Tmax",
              "Baseline (Pre-pulse period, t<0)" = "baseline"
            ),
            selected = c("HRM", "baseline")
          ),

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

          plotly::plotlyOutput(ns("pulse_trace_plot"), height = "600px")
        )
      )
    )
  )
}

# Server ----
pulseTraceServer <- function(id, heat_pulse_data, selected_pulse_id) {
  moduleServer(id, function(input, output, session) {

    # Display selected pulse info
    output$pulse_info <- renderText({
      pulse_id <- selected_pulse_id()

      if (is.null(pulse_id) || is.na(pulse_id)) {
        return("No pulse selected.\n\nClick on a point in the time series plot above to view its pulse trace.")
      }

      req(heat_pulse_data())
      data <- heat_pulse_data()

      # Get pulse info
      pulse_data <- data$measurements %>%
        filter(pulse_id == !!pulse_id)

      if (nrow(pulse_data) == 0) {
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

      req(heat_pulse_data())
      data <- heat_pulse_data()

      # Get pulse measurements
      cat("\n=== PULSE TRACE DEBUG ===\n")
      cat("Looking for pulse_id:", pulse_id, "\n")
      cat("Measurements columns:", paste(names(data$measurements), collapse = ", "), "\n")

      pulse_data <- data$measurements %>%
        filter(pulse_id == !!pulse_id)

      cat("Pulse data rows:", nrow(pulse_data), "\n")

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

      # Calculate time_sec if not present
      # Time should be seconds from start of measurement series (row number - 1)
      if (!"time_sec" %in% names(pulse_data)) {
        cat("time_sec column not found, calculating from row numbers\n")
        pulse_data <- pulse_data %>%
          mutate(time_sec = row_number() - 1)
      }

      # Calculate pre-pulse baseline (typically first 30 seconds)
      pre_pulse_period <- 30

      # Adjust time_sec so heat pulse injection is at time 0
      # Pre-pulse period becomes negative (e.g., -30 to 0)
      pulse_data <- pulse_data %>%
        mutate(time_sec = time_sec - pre_pulse_period)

      baseline_indices <- pulse_data$time_sec < 0

      # Calculate baseline mean temperatures
      do_baseline <- mean(pulse_data$do[baseline_indices], na.rm = TRUE)
      di_baseline <- mean(pulse_data$di[baseline_indices], na.rm = TRUE)
      uo_baseline <- mean(pulse_data$uo[baseline_indices], na.rm = TRUE)
      ui_baseline <- mean(pulse_data$ui[baseline_indices], na.rm = TRUE)

      # Calculate temperature deltas (ΔT)
      pulse_data <- pulse_data %>%
        mutate(
          deltaT_do = do - do_baseline,
          deltaT_di = di - di_baseline,
          deltaT_uo = uo - uo_baseline,
          deltaT_ui = ui - ui_baseline
        )

      cat("Temperature delta ranges - deltaT_do:", range(pulse_data$deltaT_do, na.rm = TRUE), "\n")
      cat("Time range:", range(pulse_data$time_sec, na.rm = TRUE), "\n")

      # Create plot with temperature delta traces
      p <- plot_ly()

      # Get selected sensor position
      show_outer <- input$sensor_position == "outer"

      # Add traces based on sensor position
      if (show_outer) {
        # Downstream outer
        p <- p %>%
          add_trace(
            data = pulse_data,
            x = ~time_sec,
            y = ~deltaT_do,
            type = "scatter",
            mode = "lines+markers",
            name = "Downstream Outer",
            line = list(color = "#d62728", width = 2),
            marker = list(size = 4)
          )

        # Upstream outer
        p <- p %>%
          add_trace(
            data = pulse_data,
            x = ~time_sec,
            y = ~deltaT_uo,
            type = "scatter",
            mode = "lines+markers",
            name = "Upstream Outer",
            line = list(color = "#2ca02c", width = 2),
            marker = list(size = 4)
          )
      } else {
        # Downstream inner
        p <- p %>%
          add_trace(
            data = pulse_data,
            x = ~time_sec,
            y = ~deltaT_di,
            type = "scatter",
            mode = "lines+markers",
            name = "Downstream Inner",
            line = list(color = "#ff7f0e", width = 2),
            marker = list(size = 4)
          )

        # Upstream inner
        p <- p %>%
          add_trace(
            data = pulse_data,
            x = ~time_sec,
            y = ~deltaT_ui,
            type = "scatter",
            mode = "lines+markers",
            name = "Upstream Inner",
            line = list(color = "#1f77b4", width = 2),
            marker = list(size = 4)
          )
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
      if ("baseline" %in% input$show_windows) {
        # Baseline window (pre-pulse, now from -pre_pulse_period to 0)
        p <- p %>%
          add_trace(
            x = c(-pre_pulse_period, 0, 0, -pre_pulse_period, -pre_pulse_period),
            y = c(min_deltaT, min_deltaT, max_deltaT, max_deltaT, min_deltaT),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(128, 128, 128, 0.1)",
            name = sprintf("Baseline (-%ds to 0)", pre_pulse_period),
            showlegend = TRUE,
            hoverinfo = "name"
          )
      }

      if ("HRM" %in% input$show_windows) {
        # HRM window (typically 60-100 seconds after pulse injection)
        # Times are now relative to pulse injection (time 0)
        hrm_start <- 60
        hrm_end <- 100

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
      }

      if ("MHR" %in% input$show_windows) {
        # MHR finds peaks in temperature data
        # Show range between upstream and downstream peaks
        if (show_outer) {
          do_peak_idx <- which.max(pulse_data$deltaT_do)
          uo_peak_idx <- which.max(pulse_data$deltaT_uo)
          do_peak_time <- pulse_data$time_sec[do_peak_idx]
          uo_peak_time <- pulse_data$time_sec[uo_peak_idx]

          # Times are already relative to pulse injection
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
            )
        } else {
          di_peak_idx <- which.max(pulse_data$deltaT_di)
          ui_peak_idx <- which.max(pulse_data$deltaT_ui)
          di_peak_time <- pulse_data$time_sec[di_peak_idx]
          ui_peak_time <- pulse_data$time_sec[ui_peak_idx]

          # Times are already relative to pulse injection
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
            )
        }
      }

      if ("Tmax" %in% input$show_windows) {
        # Tmax uses time-to-peak as a single point, not a window
        # Show vertical lines at downstream peak times
        if (show_outer) {
          do_peak_idx <- which.max(pulse_data$deltaT_do)
          do_peak_time <- pulse_data$time_sec[do_peak_idx]

          # Time is already relative to pulse injection
          p <- p %>%
            add_segments(
              x = do_peak_time, xend = do_peak_time,
              y = min_deltaT, yend = max_deltaT,
              line = list(color = "#d62728", width = 2, dash = "dot"),
              name = sprintf("Tmax DO (%.0fs after pulse)", do_peak_time),
              showlegend = TRUE
            )
        } else {
          di_peak_idx <- which.max(pulse_data$deltaT_di)
          di_peak_time <- pulse_data$time_sec[di_peak_idx]

          # Time is already relative to pulse injection
          p <- p %>%
            add_segments(
              x = di_peak_time, xend = di_peak_time,
              y = min_deltaT, yend = max_deltaT,
              line = list(color = "#ff7f0e", width = 2, dash = "dot"),
              name = sprintf("Tmax DI (%.0fs after pulse)", di_peak_time),
              showlegend = TRUE
            )
        }
      }

      # Layout
      p <- p %>%
        layout(
          title = paste("Pulse ID:", pulse_id),
          xaxis = list(
            title = "Time relative to heat pulse injection (seconds)",
            showgrid = TRUE,
            range = c(min(pulse_data$time_sec, na.rm = TRUE), max(pulse_data$time_sec, na.rm = TRUE))
          ),
          yaxis = list(
            title = "\u0394T (\u00B0C)",  # Delta T (temperature change)
            showgrid = TRUE,
            rangemode = "tozero"  # Always include zero
          ),
          hovermode = "x unified",
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.2
          ),
          annotations = list(
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
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("pulse_trace_", pulse_id),
            height = 600,
            width = 1200,
            scale = 2
          )
        )

      p
    })

    # Clear selection
    observeEvent(input$clear_selection, {
      # This will be handled by parent module
      selected_pulse_id(NULL)
    })

  })
}
