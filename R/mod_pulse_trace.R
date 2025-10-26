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

          h5("Calculation Windows"),
          p(class = "help-text", style = "font-size: 0.9em;",
            "Show calculation windows used by each method"),

          checkboxGroupInput(
            ns("show_windows"),
            NULL,
            choices = c(
              "HRM Window" = "HRM",
              "MHR Window" = "MHR",
              "Tmax Window" = "Tmax",
              "Baseline (Pre-pulse)" = "baseline"
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
              xaxis = list(title = "Time (seconds)"),
              yaxis = list(title = "Temperature (\u00B0C)")
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
              xaxis = list(title = "Time (seconds)"),
              yaxis = list(title = "Temperature (\u00B0C)")
            )
        )
      }

      # Calculate time_sec if not present
      if (!"time_sec" %in% names(pulse_data)) {
        cat("time_sec column not found, calculating from row numbers\n")
        # Use row number as time proxy (measurements are typically every 5 seconds)
        pulse_data <- pulse_data %>%
          mutate(time_sec = (row_number() - 1) * 5)
      }

      cat("Temperature ranges - do:", range(pulse_data$do, na.rm = TRUE), "\n")
      cat("Time range:", range(pulse_data$time_sec, na.rm = TRUE), "\n")

      # Create plot with temperature traces
      p <- plot_ly()

      # Add downstream outer trace
      p <- p %>%
        add_trace(
          data = pulse_data,
          x = ~time_sec,
          y = ~do,
          type = "scatter",
          mode = "lines+markers",
          name = "Downstream Outer",
          line = list(color = "#d62728", width = 2),
          marker = list(size = 4)
        )

      # Add downstream inner trace
      p <- p %>%
        add_trace(
          data = pulse_data,
          x = ~time_sec,
          y = ~di,
          type = "scatter",
          mode = "lines+markers",
          name = "Downstream Inner",
          line = list(color = "#ff7f0e", width = 2),
          marker = list(size = 4)
        )

      # Add upstream outer trace
      p <- p %>%
        add_trace(
          data = pulse_data,
          x = ~time_sec,
          y = ~uo,
          type = "scatter",
          mode = "lines+markers",
          name = "Upstream Outer",
          line = list(color = "#2ca02c", width = 2),
          marker = list(size = 4)
        )

      # Add upstream inner trace
      p <- p %>%
        add_trace(
          data = pulse_data,
          x = ~time_sec,
          y = ~ui,
          type = "scatter",
          mode = "lines+markers",
          name = "Upstream Inner",
          line = list(color = "#1f77b4", width = 2),
          marker = list(size = 4)
        )

      # Add calculation windows as shaded regions
      if ("baseline" %in% input$show_windows) {
        # Baseline window (pre-pulse, typically 0-30 seconds)
        p <- p %>%
          add_trace(
            x = c(0, 30, 30, 0, 0),
            y = c(min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 max(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 max(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE)),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(128, 128, 128, 0.1)",
            name = "Baseline (0-30s)",
            showlegend = TRUE,
            hoverinfo = "name"
          )
      }

      if ("HRM" %in% input$show_windows) {
        # HRM window (typically 60-100 seconds)
        p <- p %>%
          add_trace(
            x = c(60, 100, 100, 60, 60),
            y = c(min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 max(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 max(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE)),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(31, 119, 180, 0.1)",
            name = "HRM Window (60-100s)",
            showlegend = TRUE,
            hoverinfo = "name"
          )
      }

      if ("MHR" %in% input$show_windows) {
        # MHR uses max temperature, show approximate window
        p <- p %>%
          add_trace(
            x = c(40, 80, 80, 40, 40),
            y = c(min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 max(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 max(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE)),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(255, 127, 14, 0.1)",
            name = "MHR Window (40-80s)",
            showlegend = TRUE,
            hoverinfo = "name"
          )
      }

      if ("Tmax" %in% input$show_windows) {
        # Tmax looks for peak time
        p <- p %>%
          add_trace(
            x = c(30, 120, 120, 30, 30),
            y = c(min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 max(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 max(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE),
                 min(pulse_data$do, pulse_data$di, pulse_data$uo, pulse_data$ui, na.rm = TRUE)),
            type = "scatter",
            mode = "none",
            fill = "toself",
            fillcolor = "rgba(44, 160, 44, 0.1)",
            name = "Tmax Window (30-120s)",
            showlegend = TRUE,
            hoverinfo = "name"
          )
      }

      # Layout
      p <- p %>%
        layout(
          title = paste("Pulse ID:", pulse_id),
          xaxis = list(
            title = "Time Since Heat Pulse (seconds)",
            showgrid = TRUE
          ),
          yaxis = list(
            title = "Temperature (\u00B0C)",
            showgrid = TRUE
          ),
          hovermode = "x unified",
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.2
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
