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

          uiOutput(ns("show_windows_ui")),

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
pulseTraceServer <- function(id, heat_pulse_data, selected_pulse_id, vh_results = NULL) {
  moduleServer(id, function(input, output, session) {

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
          if ("HRMXa" %in% pulse_methods) {
            all_choices[["HRMXa Window"]] <- "HRMXa"
          }
          if ("HRMXb" %in% pulse_methods) {
            all_choices[["HRMXb Windows (separate up/down)"]] <- "HRMXb"
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
      # Check if show_windows input exists before using it
      show_windows <- if (!is.null(input$show_windows)) input$show_windows else character(0)

      if ("baseline" %in% show_windows) {
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

      if ("HRM" %in% show_windows) {
        # HRM window - get actual window times from results
        # Get the sensor position being displayed
        position <- if (show_outer) "outer" else "inner"

        # Get the actual window times from vh_results for this pulse and method
        if (!is.null(vh_results)) {
          results <- vh_results()
          if (!is.null(results) && nrow(results) > 0) {
            hrm_result <- results[results$pulse_id == pulse_id &
                                  results$method == "HRM" &
                                  results$sensor_position == position, ]

            if (nrow(hrm_result) > 0) {
              hrm_start <- hrm_result$calc_window_start_sec[1]
              hrm_end <- hrm_result$calc_window_end_sec[1]
            } else {
              # Fallback if no results found
              hrm_start <- 60
              hrm_end <- 100
            }
          } else {
            hrm_start <- 60
            hrm_end <- 100
          }
        } else {
          hrm_start <- 60
          hrm_end <- 100
        }

        # Get temperature values at window boundaries for both sensors
        if (show_outer) {
          # Find temperatures at window start
          do_temp_start <- pulse_data$deltaT_do[which.min(abs(pulse_data$time_sec - hrm_start))]
          uo_temp_start <- pulse_data$deltaT_uo[which.min(abs(pulse_data$time_sec - hrm_start))]
          # Find temperatures at window end
          do_temp_end <- pulse_data$deltaT_do[which.min(abs(pulse_data$time_sec - hrm_end))]
          uo_temp_end <- pulse_data$deltaT_uo[which.min(abs(pulse_data$time_sec - hrm_end))]
        } else {
          # Find temperatures at window start
          di_temp_start <- pulse_data$deltaT_di[which.min(abs(pulse_data$time_sec - hrm_start))]
          ui_temp_start <- pulse_data$deltaT_ui[which.min(abs(pulse_data$time_sec - hrm_start))]
          # Find temperatures at window end
          di_temp_end <- pulse_data$deltaT_di[which.min(abs(pulse_data$time_sec - hrm_end))]
          ui_temp_end <- pulse_data$deltaT_ui[which.min(abs(pulse_data$time_sec - hrm_end))]
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

        # Add vertical lines and points at window boundaries
        if (show_outer) {
          p <- p %>%
            add_segments(
              x = hrm_start, xend = hrm_start,
              y = 0, yend = max(do_temp_start, uo_temp_start, na.rm = TRUE),
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
              y = 0, yend = max(do_temp_end, uo_temp_end, na.rm = TRUE),
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
        } else {
          p <- p %>%
            add_segments(
              x = hrm_start, xend = hrm_start,
              y = 0, yend = max(di_temp_start, ui_temp_start, na.rm = TRUE),
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
              y = 0, yend = max(di_temp_end, ui_temp_end, na.rm = TRUE),
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

      if ("HRMXa" %in% input$show_windows) {
        # HRMXa window - get actual window times from results
        # Get the sensor position being displayed
        position <- if (show_outer) "outer" else "inner"

        # Get the actual window times from vh_results for this pulse and method
        if (!is.null(vh_results)) {
          results <- vh_results()
          if (!is.null(results) && nrow(results) > 0) {
            hrmxa_result <- results[results$pulse_id == pulse_id &
                                    results$method == "HRMXa" &
                                    results$sensor_position == position, ]

            if (nrow(hrmxa_result) > 0) {
              hrm_start <- hrmxa_result$calc_window_start_sec[1]
              hrm_end <- hrmxa_result$calc_window_end_sec[1]
            } else {
              # Fallback if no results found
              hrm_start <- 60
              hrm_end <- 100
            }
          } else {
            hrm_start <- 60
            hrm_end <- 100
          }
        } else {
          hrm_start <- 60
          hrm_end <- 100
        }

        # Get temperature values at window boundaries for both sensors
        if (show_outer) {
          do_temp_start <- pulse_data$deltaT_do[which.min(abs(pulse_data$time_sec - hrm_start))]
          uo_temp_start <- pulse_data$deltaT_uo[which.min(abs(pulse_data$time_sec - hrm_start))]
          do_temp_end <- pulse_data$deltaT_do[which.min(abs(pulse_data$time_sec - hrm_end))]
          uo_temp_end <- pulse_data$deltaT_uo[which.min(abs(pulse_data$time_sec - hrm_end))]
        } else {
          di_temp_start <- pulse_data$deltaT_di[which.min(abs(pulse_data$time_sec - hrm_start))]
          ui_temp_start <- pulse_data$deltaT_ui[which.min(abs(pulse_data$time_sec - hrm_start))]
          di_temp_end <- pulse_data$deltaT_di[which.min(abs(pulse_data$time_sec - hrm_end))]
          ui_temp_end <- pulse_data$deltaT_ui[which.min(abs(pulse_data$time_sec - hrm_end))]
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

        # Add vertical lines and points at window boundaries
        if (show_outer) {
          p <- p %>%
            add_segments(
              x = hrm_start, xend = hrm_start,
              y = 0, yend = max(do_temp_start, uo_temp_start, na.rm = TRUE),
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
              y = 0, yend = max(do_temp_end, uo_temp_end, na.rm = TRUE),
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
        } else {
          p <- p %>%
            add_segments(
              x = hrm_start, xend = hrm_start,
              y = 0, yend = max(di_temp_start, ui_temp_start, na.rm = TRUE),
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
              y = 0, yend = max(di_temp_end, ui_temp_end, na.rm = TRUE),
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

      if ("HRMXb" %in% input$show_windows) {
        # HRMXb uses two separate windows - get actual window times from results
        # Get the sensor position being displayed
        position <- if (show_outer) "outer" else "inner"

        # Get the actual window times from vh_results for this pulse and method
        if (!is.null(vh_results)) {
          results <- vh_results()
          if (!is.null(results) && nrow(results) > 0) {
            hrmxb_result <- results[results$pulse_id == pulse_id &
                                    results$method == "HRMXb" &
                                    results$sensor_position == position, ]

            if (nrow(hrmxb_result) > 0) {
              downstream_start <- hrmxb_result$downstream_window_start_sec[1]
              downstream_end <- hrmxb_result$downstream_window_end_sec[1]
              upstream_start <- hrmxb_result$upstream_window_start_sec[1]
              upstream_end <- hrmxb_result$upstream_window_end_sec[1]
            } else {
              # Fallback if no results found
              downstream_start <- 60
              downstream_end <- 100
              upstream_start <- 60
              upstream_end <- 100
            }
          } else {
            downstream_start <- 60
            downstream_end <- 100
            upstream_start <- 60
            upstream_end <- 100
          }
        } else {
          downstream_start <- 60
          downstream_end <- 100
          upstream_start <- 60
          upstream_end <- 100
        }

        # Get temperature values at window boundaries
        if (show_outer) {
          do_temp_start <- pulse_data$deltaT_do[which.min(abs(pulse_data$time_sec - downstream_start))]
          do_temp_end <- pulse_data$deltaT_do[which.min(abs(pulse_data$time_sec - downstream_end))]
          uo_temp_start <- pulse_data$deltaT_uo[which.min(abs(pulse_data$time_sec - upstream_start))]
          uo_temp_end <- pulse_data$deltaT_uo[which.min(abs(pulse_data$time_sec - upstream_end))]
        } else {
          di_temp_start <- pulse_data$deltaT_di[which.min(abs(pulse_data$time_sec - downstream_start))]
          di_temp_end <- pulse_data$deltaT_di[which.min(abs(pulse_data$time_sec - downstream_end))]
          ui_temp_start <- pulse_data$deltaT_ui[which.min(abs(pulse_data$time_sec - upstream_start))]
          ui_temp_end <- pulse_data$deltaT_ui[which.min(abs(pulse_data$time_sec - upstream_end))]
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

        # Add vertical lines and points for downstream window (use DOWNSTREAM sensor only)
        if (show_outer) {
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
        } else {
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

      if ("MHR" %in% input$show_windows) {
        # MHR finds peaks in temperature data
        # Show range between upstream and downstream peaks
        if (show_outer) {
          do_peak_idx <- which.max(pulse_data$deltaT_do)
          uo_peak_idx <- which.max(pulse_data$deltaT_uo)
          do_peak_time <- pulse_data$time_sec[do_peak_idx]
          uo_peak_time <- pulse_data$time_sec[uo_peak_idx]
          do_peak_temp <- pulse_data$deltaT_do[do_peak_idx]
          uo_peak_temp <- pulse_data$deltaT_uo[uo_peak_idx]

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
        } else {
          di_peak_idx <- which.max(pulse_data$deltaT_di)
          ui_peak_idx <- which.max(pulse_data$deltaT_ui)
          di_peak_time <- pulse_data$time_sec[di_peak_idx]
          ui_peak_time <- pulse_data$time_sec[ui_peak_idx]
          di_peak_temp <- pulse_data$deltaT_di[di_peak_idx]
          ui_peak_temp <- pulse_data$deltaT_ui[ui_peak_idx]

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

      if ("Tmax" %in% input$show_windows) {
        # Tmax uses time-to-peak as a single point, not a window
        # Show vertical lines at downstream peak times with markers
        if (show_outer) {
          do_peak_idx <- which.max(pulse_data$deltaT_do)
          do_peak_time <- pulse_data$time_sec[do_peak_idx]
          do_peak_temp <- pulse_data$deltaT_do[do_peak_idx]

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
        } else {
          di_peak_idx <- which.max(pulse_data$deltaT_di)
          di_peak_time <- pulse_data$time_sec[di_peak_idx]
          di_peak_temp <- pulse_data$deltaT_di[di_peak_idx]

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
