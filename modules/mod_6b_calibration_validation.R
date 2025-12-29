# mod_6b_calibration_validation.R
# Module for Calibration Validation
#
# Tab 6b: Calibration Validation
# Time series plots to verify the "fit" of calibrated data against HRM baseline.
# Shows before/after calibration comparison to assess calibration quality.

# UI ----
calibrationValidationUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Controls
      column(
        width = 3,
        box(
          width = NULL,
          title = "Validation Controls",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          h5("Calibration State"),
          helpText("Compare raw vs. calibrated velocities"),
          selectInput(
            ns("calibration_state"),
            NULL,
            choices = c(
              "Raw (Before Calibration)" = "raw",
              "Calibrated (After Calibration)" = "calibrated",
              "Both (Comparison)" = "both"
            ),
            selected = "both"
          ),

          hr(),

          h5("Methods to Display"),
          helpText("Select methods to compare against HRM baseline"),
          uiOutput(ns("method_checkboxes")),

          hr(),

          h5("Sensor Position"),
          radioButtons(
            ns("sensor_position"),
            NULL,
            choices = c("Inner" = "inner", "Outer" = "outer"),
            selected = "outer"
          ),

          hr(),

          h5("Time Range"),
          p(class = "help-text", style = "font-size: 0.9em; color: #666;",
            "Select date range to display"),

          shinyWidgets::airDatepickerInput(
            ns("start_datetime"),
            "Start Date/Time:",
            value = NULL,
            timepicker = TRUE,
            dateFormat = "yyyy-MM-dd HH:mm"
          ),

          shinyWidgets::airDatepickerInput(
            ns("end_datetime"),
            "End Date/Time:",
            value = NULL,
            timepicker = TRUE,
            dateFormat = "yyyy-MM-dd HH:mm"
          ),

          actionButton(
            ns("apply_range"),
            "Apply Time Range",
            icon = icon("clock"),
            class = "btn-primary",
            style = "width: 100%; margin-top: 5px;"
          ),

          hr(),

          h5("Display Options"),
          checkboxInput(
            ns("show_vpd"),
            "Show VPD overlay (right axis)",
            value = FALSE
          ),

          checkboxInput(
            ns("show_legend"),
            "Show legend",
            value = TRUE
          )
        )
      ),

      # Right column: Plot
      column(
        width = 9,
        box(
          width = NULL,
          title = "Calibration Validation Plot",
          status = "success",
          solidHeader = TRUE,

          helpText(
            icon("info-circle"),
            "This plot shows how well the calibrated secondary methods align with HRM (the primary/baseline method).",
            "Good calibration results in similar patterns between HRM and secondary methods."
          ),

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("validation_plot"), height = "600px"),
            type = 6,
            color = "#3c8dbc"
          )
        ),

        # Statistics box (moved below plot)
        box(
          width = NULL,
          title = "Calibration Statistics",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          htmlOutput(ns("calibration_stats"))
        )
      )
    )
  )
}

# Server ----
calibrationValidationServer <- function(id,
                                         vh_raw,
                                         vh_calibrated,
                                         weather_data = reactive(NULL),
                                         code_tracker = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize datetime range from data
    observe({
      req(vh_raw())

      data <- vh_raw()
      date_range <- range(data$datetime, na.rm = TRUE)

      shinyWidgets::updateAirDateInput(
        session, "start_datetime",
        value = date_range[1]
      )

      shinyWidgets::updateAirDateInput(
        session, "end_datetime",
        value = date_range[2]
      )
    })

    # Dynamic method checkboxes
    output$method_checkboxes <- renderUI({
      req(vh_raw())

      data <- vh_raw()
      methods <- unique(data$method)
      # Remove HRM from choices (it's always shown as baseline)
      secondary_methods <- setdiff(methods, "HRM")

      checkboxGroupInput(
        ns("methods_selected"),
        NULL,
        choices = secondary_methods,
        selected = secondary_methods
      )
    })

    # Reactive: Time range
    time_range <- reactiveVal(NULL)

    observeEvent(input$apply_range, {
      time_range(c(input$start_datetime, input$end_datetime))
    })

    # Reactive: Filtered data for plotting
    plot_data <- reactive({
      req(vh_raw())
      req(input$sensor_position)
      req(input$methods_selected)

      raw_data <- vh_raw()

      # Validate raw data structure
      if (!all(c("datetime", "method", "sensor_position", "Vh_cm_hr") %in% names(raw_data))) {
        return(data.frame())  # Return empty data frame if structure is wrong
      }

      cal_data <- vh_calibrated()
      sensor <- input$sensor_position
      methods <- c("HRM", input$methods_selected)  # Always include HRM

      # Filter by sensor and methods
      raw_filtered <- raw_data %>%
        dplyr::filter(
          sensor_position == sensor,
          method %in% methods
        )

      # Apply time range if set
      if (!is.null(time_range())) {
        raw_filtered <- raw_filtered %>%
          dplyr::filter(
            datetime >= time_range()[1],
            datetime <= time_range()[2]
          )
      }

      # Add calibration state flag
      raw_filtered <- raw_filtered %>%
        dplyr::mutate(calibration_state = "raw")

      # If we have calibrated data and want to show it
      if (!is.null(cal_data) && nrow(cal_data) > 0 && input$calibration_state %in% c("calibrated", "both")) {
        cal_filtered <- cal_data %>%
          dplyr::filter(
            sensor_position == sensor,
            method %in% methods
          )

        # Apply time range
        if (!is.null(time_range())) {
          cal_filtered <- cal_filtered %>%
            dplyr::filter(
              datetime >= time_range()[1],
              datetime <= time_range()[2]
            )
        }

        cal_filtered <- cal_filtered %>%
          dplyr::mutate(calibration_state = "calibrated")

        # Combine based on selection
        if (input$calibration_state == "both") {
          plot_data <- dplyr::bind_rows(raw_filtered, cal_filtered)
        } else {
          plot_data <- cal_filtered
        }
      } else {
        plot_data <- raw_filtered
      }

      return(plot_data)
    })

    # Validation plot
    output$validation_plot <- plotly::renderPlotly({

      # Wrap in tryCatch for better error handling
      tryCatch({
        req(plot_data())

        data <- plot_data()

        # Check if calibrated data is available
        has_calibrated <- any(data$calibration_state == "calibrated")

        if (nrow(data) == 0) {
          return(
            plotly::plot_ly() %>%
              plotly::layout(
                title = "No data to display - please run calibration first",
                xaxis = list(title = "Datetime"),
                yaxis = list(title = "Velocity (cm/hr)")
              )
          )
        }

        # If user selected calibrated/both but no calibrated data exists
        if (input$calibration_state %in% c("calibrated", "both") && !has_calibrated) {
          return(
            plotly::plot_ly() %>%
              plotly::layout(
                title = "No calibrated data available - please run calibration in Tab 6a first",
                xaxis = list(title = "Datetime"),
                yaxis = list(title = "Velocity (cm/hr)")
              )
          )
        }

      # Create base plot
      p <- plotly::plot_ly()

      # Color palette for methods
      method_colors <- c(
        "HRM" = "#1f77b4",
        "MHR" = "#ff7f0e",
        "HRMXa" = "#2ca02c",
        "HRMXb" = "#d62728",
        "Tmax_Coh" = "#9467bd",
        "Tmax_Klu" = "#8c564b"
      )

      # Add traces for each method and calibration state
      for (method in unique(data$method)) {
        for (state in unique(data$calibration_state)) {
          # Skip HRM raw when showing both (HRM doesn't get calibrated, so only show once)
          if (method == "HRM" && state == "raw" && input$calibration_state == "both") {
            next
          }

          subset_data <- data %>%
            dplyr::filter(method == !!method, calibration_state == !!state)

          if (nrow(subset_data) == 0) next

          # Determine trace name
          if (method == "HRM") {
            # HRM is always the corrected baseline (not calibrated)
            trace_name <- "HRM (Corrected)"
          } else if (input$calibration_state == "both") {
            # For secondary methods, show Raw vs Calibrated
            trace_name <- paste0(method, " (",
                   ifelse(state == "raw", "Raw", "Calibrated"), ")")
          } else {
            trace_name <- method
          }

          # Determine line width: raw = thin (1), calibrated = thick (2.5)
          line_width <- if (state == "raw") {
            1.0  # Thin line for raw
          } else {
            2.5  # Thick line for calibrated
          }

          p <- p %>%
            plotly::add_trace(
              data = subset_data,
              x = ~datetime,
              y = ~Vh_cm_hr,
              type = 'scatter',
              mode = 'lines',
              name = trace_name,
              line = list(
                color = method_colors[[method]],
                width = line_width
              ),
              legendgroup = method,
              showlegend = input$show_legend
            )
        }
      }

      # Add VPD overlay if requested
      if (input$show_vpd && !is.null(weather_data()) && nrow(weather_data()) > 0) {
        weather <- weather_data()

        # Filter weather data to match time range
        if (!is.null(time_range())) {
          weather <- weather %>%
            dplyr::filter(
              datetime >= time_range()[1],
              datetime <= time_range()[2]
            )
        }

        if (nrow(weather) > 0 && "vpd_kPa" %in% names(weather)) {
          p <- p %>%
            plotly::add_trace(
              data = weather,
              x = ~datetime,
              y = ~vpd_kPa,
              type = 'scatter',
              mode = 'lines',
              name = 'VPD (kPa)',
              line = list(color = '#999999', width = 1, dash = 'dash'),
              yaxis = 'y2',
              showlegend = input$show_legend
            )
        }
      }

      # Layout
      p <- p %>%
        plotly::layout(
          title = paste0("Calibration Validation - ",
                         toupper(input$sensor_position), " Sensor"),
          xaxis = list(
            title = "Datetime",
            showgrid = TRUE
          ),
          yaxis = list(
            title = "Velocity (cm/hr)",
            showgrid = TRUE
          ),
          yaxis2 = if (input$show_vpd) {
            list(
              title = "VPD (kPa)",
              overlaying = 'y',
              side = 'right',
              showgrid = FALSE
            )
          } else {
            NULL
          },
          hovermode = 'x unified',
          legend = list(
            orientation = 'h',
            x = 0,
            y = -0.2,
            xanchor = 'left',
            yanchor = 'top'
          ),
          # Preserve zoom and pan when plot updates
          uirevision = "static"
        )

      return(p)

      }, error = function(e) {
        # Return error plot if something goes wrong
        plotly::plot_ly() %>%
          plotly::layout(
            title = paste("Error:", e$message),
            xaxis = list(title = "Datetime"),
            yaxis = list(title = "Velocity (cm/hr)")
          )
      })
    })

    # Calibration statistics
    output$calibration_stats <- renderUI({
      req(vh_raw())
      req(input$sensor_position)

      # Check if calibrated data exists
      if (is.null(vh_calibrated()) || nrow(vh_calibrated()) == 0) {
        return(HTML("<p style='color:#999;'>No calibrated data available yet. Please run calibration in Tab 6a first.</p>"))
      }

      raw_data <- vh_raw() %>%
        dplyr::filter(sensor_position == input$sensor_position)

      cal_data <- vh_calibrated() %>%
        dplyr::filter(sensor_position == input$sensor_position)

      # Calculate statistics for each method
      stats_html <- "<table style='width:100%; font-size:0.9em;'>"
      stats_html <- paste0(stats_html, "<thead><tr>",
                           "<th>Method</th>",
                           "<th>Mean Δ</th>",
                           "<th>RMSE</th>",
                           "</tr></thead><tbody>")

      for (method in setdiff(unique(raw_data$method), "HRM")) {
        raw_method <- raw_data %>%
          dplyr::filter(method == !!method) %>%
          dplyr::pull(Vh_cm_hr)

        cal_method <- cal_data %>%
          dplyr::filter(method == !!method) %>%
          dplyr::pull(Vh_cm_hr)

        if (length(raw_method) > 0 && length(cal_method) > 0) {
          delta <- mean(cal_method - raw_method, na.rm = TRUE)
          rmse <- sqrt(mean((cal_method - raw_method)^2, na.rm = TRUE))

          stats_html <- paste0(stats_html, "<tr>",
                               "<td><strong>", method, "</strong></td>",
                               "<td>", sprintf("%.2f", delta), " cm/hr</td>",
                               "<td>", sprintf("%.2f", rmse), " cm/hr</td>",
                               "</tr>")
        }
      }

      stats_html <- paste0(stats_html, "</tbody></table>")
      stats_html <- paste0(stats_html,
                           "<p style='margin-top:10px; font-size:0.85em; color:#666;'>",
                           "Mean Δ = average change after calibration<br>",
                           "RMSE = root mean squared error</p>")

      HTML(stats_html)
    })

    # Code generation
    observe({
      if (!is.null(code_tracker)) {
        # Track validation visualisation
        if (!is.null(input$calibration_state) && !is.null(input$sensor_position)) {
          code_tracker$add_step(
            step_name = "Calibration Validation",
            code = sprintf(
              "# Calibration validation - comparing %s velocities for %s sensor",
              input$calibration_state,
              input$sensor_position
            )
          )
        }
      }
    })

  })
}
