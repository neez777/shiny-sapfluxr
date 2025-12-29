# mod_7b_sdma_validation.R
# Module for sDMA Validation Visualization
#
# Tab 7b: sDMA Validation
# Interactive time series to compare HRM baseline, calibrated methods, and sDMA results

# UI ----
sdmaValidationUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Controls
      column(
        width = 3,
        box(
          width = NULL,
          title = "Plot Controls",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          h5("Methods to Display"),
          helpText("Select specific methods to show"),
          uiOutput(ns("method_checkboxes")),

          hr(),

          h5("Sensor Position"),
          checkboxGroupInput(
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
            ns("show_legend"),
            "Show legend",
            value = TRUE
          ),

          checkboxInput(
            ns("show_points"),
            "Show data points",
            value = FALSE
          ),

          hr(),

          actionButton(
            ns("reset_zoom"),
            "Reset to Full Range",
            icon = icon("refresh"),
            class = "btn-default",
            style = "width: 100%;"
          )
        )
      ),

      # Right column: Plot
      column(
        width = 9,
        box(
          width = NULL,
          title = "sDMA Validation Time Series",
          status = "success",
          solidHeader = TRUE,

          helpText(
            icon("info-circle"),
            "Compare HRM baseline, calibrated secondary methods, and sDMA results.",
            "Click-drag to zoom, double-click to reset zoom."
          ),

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("validation_plot"), height = "700px"),
            type = 6,
            color = "#3c8dbc"
          )
        )
      )
    )
  )
}

# Server ----
sdmaValidationServer <- function(id,
                                  vh_hrm_peclet,
                                  vh_calibrated,
                                  vh_sdma,
                                  code_tracker = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Time range
    time_range <- reactiveVal(NULL)

    # Initialize datetime range from data
    observe({
      req(vh_hrm_peclet())

      data <- vh_hrm_peclet()
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

    observeEvent(input$apply_range, {
      time_range(c(input$start_datetime, input$end_datetime))
    })

    # Dynamic method checkboxes based on available data
    output$method_checkboxes <- renderUI({
      req(vh_calibrated())

      # Get all unique methods from calibrated data
      methods <- unique(vh_calibrated()$method)
      methods <- methods[!is.na(methods)]

      # Add sDMA if available (exclude HRM and NA - sDMA only shows secondary methods)
      if (!is.null(vh_sdma()) && nrow(vh_sdma()) > 0) {
        sdma_sources <- unique(vh_sdma()$sdma_source)
        # Filter out HRM and NA - sDMA should only show secondary methods
        sdma_sources <- sdma_sources[!sdma_sources %in% c("HRM", "NA") & !is.na(sdma_sources)]
        if (length(sdma_sources) > 0) {
          sdma_methods <- paste0("sDMA: ", sdma_sources)
          methods <- c(methods, sdma_methods)
        }
      }

      if (length(methods) == 0) {
        return(p(style = "color: #999;", "No methods available"))
      }

      checkboxGroupInput(
        ns("methods_selected"),
        NULL,
        choices = methods,
        selected = methods
      )
    })

    # Reactive: Filtered plot data
    plot_data <- reactive({
      req(input$sensor_position)

      all_data <- list()

      # Add HRM baseline (always included)
      if (!is.null(vh_hrm_peclet())) {
        hrm_data <- vh_hrm_peclet() %>%
          dplyr::filter(sensor_position %in% input$sensor_position) %>%
          dplyr::select(datetime, pulse_id, sensor_position, Vh_cm_hr) %>%
          dplyr::mutate(
            method = "HRM (Corrected)",
            data_type = "baseline"
          )

        all_data$hrm <- hrm_data
      }

      # Add calibrated secondary methods (always included)
      if (!is.null(vh_calibrated())) {
        cal_data <- vh_calibrated() %>%
          dplyr::filter(
            method != "HRM",
            sensor_position %in% input$sensor_position
          ) %>%
          dplyr::select(datetime, pulse_id, sensor_position, method, Vh_cm_hr) %>%
          dplyr::mutate(
            method = paste0(method, " (Calibrated)"),
            data_type = "calibrated"
          )

        all_data$calibrated <- cal_data
      }

      # Add sDMA results (always included, exclude HRM and NA)
      if (!is.null(vh_sdma()) && nrow(vh_sdma()) > 0) {
        sdma_data <- vh_sdma() %>%
          dplyr::filter(
            !is.na(datetime),
            !is.na(Vh_sdma),
            sensor_position %in% input$sensor_position,
            !sdma_source %in% c("HRM", "NA"),  # Exclude HRM and NA
            !is.na(sdma_source)
          ) %>%
          dplyr::select(datetime, pulse_id, sensor_position, combination, sdma_source, Vh_sdma) %>%
          dplyr::mutate(
            method = paste0("sDMA: ", sdma_source),
            Vh_cm_hr = Vh_sdma,
            data_type = "sdma"
          ) %>%
          dplyr::select(datetime, pulse_id, sensor_position, method, Vh_cm_hr, data_type) %>%
          # Remove any duplicate datetime+method combinations (take first value)
          dplyr::distinct(datetime, method, sensor_position, .keep_all = TRUE)

        all_data$sdma <- sdma_data
      }

      if (length(all_data) == 0) {
        return(data.frame())
      }

      # Combine all data
      combined <- dplyr::bind_rows(all_data)

      # Filter by selected methods if specified
      if (!is.null(input$methods_selected) && length(input$methods_selected) > 0) {
        combined <- combined %>%
          dplyr::filter(method %in% input$methods_selected)
      }

      # Apply time range filter
      if (!is.null(time_range())) {
        combined <- combined %>%
          dplyr::filter(
            datetime >= time_range()[1],
            datetime <= time_range()[2]
          )
      }

      # CRITICAL: Sort by datetime within each method to prevent horizontal connection lines
      combined <- combined %>%
        dplyr::arrange(method, datetime)

      return(combined)
    })

    # Validation plot
    output$validation_plot <- plotly::renderPlotly({

      tryCatch({
        data <- plot_data()

        if (is.null(data) || nrow(data) == 0) {
          return(
            plotly::plot_ly() %>%
              plotly::layout(
                title = "No data to display - please check your selections",
                xaxis = list(title = "Datetime"),
                yaxis = list(title = "Velocity (cm/hr)")
              )
          )
        }

        # No downsampling - each method is added as a separate trace
        # Plotly handles ~20k points per trace efficiently

        # Color palette
        method_colors <- c(
          "HRM (Corrected)" = "#1f77b4",
          "MHR (Calibrated)" = "#ff7f0e",
          "HRMXa (Calibrated)" = "#2ca02c",
          "HRMXb (Calibrated)" = "#d62728",
          "Tmax_Coh (Calibrated)" = "#9467bd",
          "Tmax_Klu (Calibrated)" = "#8c564b",
          "sDMA: HRM" = "#1f77b4",
          "sDMA: MHR" = "#ff7f0e",
          "sDMA: HRMXa" = "#2ca02c",
          "sDMA: HRMXb" = "#d62728"
        )

        # Create plot
        p <- plotly::plot_ly()

        for (m in unique(data$method)) {
          method_data <- data %>% dplyr::filter(method == m)

          # Determine line width based on data type
          line_width <- if (grepl("sDMA", m)) {
            2.5  # Thick for sDMA
          } else if (grepl("HRM.*Corrected", m)) {
            2.0  # Medium for HRM baseline
          } else {
            1.5  # Normal for calibrated methods
          }

          # Get color
          color <- if (m %in% names(method_colors)) {
            method_colors[[m]]
          } else {
            NULL
          }

          # Determine mode
          mode <- if (input$show_points) "lines+markers" else "lines"

          p <- p %>%
            plotly::add_trace(
              data = method_data,
              x = ~datetime,
              y = ~Vh_cm_hr,
              type = "scatter",
              mode = mode,
              name = m,
              line = if (!is.null(color)) list(width = line_width, color = color) else list(width = line_width),
              marker = if (input$show_points) list(size = 4) else NULL,
              connectgaps = FALSE,  # Don't connect across gaps in time series
              hovertemplate = paste(
                "<b>Time:</b> %{x|%Y-%m-%d %H:%M}<br>",
                "<b>Velocity:</b> %{y:.2f} cm/hr<br>",
                "<b>Method:</b>", m, "<br>",
                "<extra></extra>"
              )
            )
        }

        # Layout
        p <- p %>%
          plotly::layout(
            title = "sDMA Validation: HRM Baseline vs Calibrated Methods vs sDMA Results",
            xaxis = list(
              title = "Datetime",
              showgrid = TRUE,
              gridcolor = "lightgray"
            ),
            yaxis = list(
              title = "Velocity (cm/hr)",
              showgrid = TRUE,
              gridcolor = "lightgray"
            ),
            hovermode = "closest",
            showlegend = input$show_legend,
            legend = list(
              orientation = "v",
              x = 1.02,
              y = 1,
              xanchor = "left"
            ),
            margin = list(l = 70, r = 200, t = 60, b = 60),
            uirevision = "static"  # Preserve zoom when plot updates
          )

        return(p)

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(
            title = paste("Error:", e$message),
            xaxis = list(title = "Datetime"),
            yaxis = list(title = "Velocity (cm/hr)")
          )
      })
    })

    # Reset zoom
    observeEvent(input$reset_zoom, {
      time_range(NULL)

      if (!is.null(vh_hrm_peclet())) {
        data <- vh_hrm_peclet()
        date_range <- range(data$datetime, na.rm = TRUE)

        shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
        shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
      }
    })

    # Code generation
    observe({
      if (!is.null(code_tracker)) {
        if (!is.null(input$sensor_position)) {
          code_tracker$add_step(
            step_name = "sDMA Validation Visualization",
            code = sprintf(
              "# sDMA validation plot for %s sensor(s)",
              paste(input$sensor_position, collapse = ", ")
            )
          )
        }
      }
    })

  })
}
