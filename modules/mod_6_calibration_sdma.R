# mod_calibration_sdma.R
# Module for Method Calibration and sDMA
#
# Implements method calibration and selectable dual method approach (sDMA)
# Applied AFTER spacing and wound corrections (late calibration - recommended)

# UI ----
calibrationSdmaUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Configuration
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "About Calibration & sDMA",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p("Calibration aligns secondary methods (MHR, HRMXa, etc.) with a primary method (usually HRM) using linear regression."),
          p("sDMA (Selectable Dual Method Approach) automatically switches between methods based on flow conditions (Peclet number)."),
          tags$ul(
            tags$li(strong("Low flow (Pe < threshold):"), " Uses HRM (more accurate at low flows)"),
            tags$li(strong("High flow (Pe ≥ threshold):"), " Uses calibrated secondary method")
          ),
          p(tags$small(em("Recommended: Apply AFTER spacing and wound corrections")))
        ),

        # Method selection
        box(
          width = 12,
          title = "Method Selection",
          status = "primary",
          solidHeader = TRUE,

          selectInput(
            ns("primary_method"),
            "Primary Method (Source of Truth):",
            choices = c("HRM", "MHR", "HRMXa", "HRMXb", "Tmax_Coh", "Tmax_Klu"),
            selected = "HRM"
          ),

          helpText(
            icon("info-circle"),
            " The primary method is assumed to be accurate. Secondary methods will be calibrated to match it."
          ),

          checkboxGroupInput(
            ns("secondary_methods"),
            "Secondary Methods (to calibrate):",
            choices = c("MHR", "HRMXa", "HRMXb", "Tmax_Coh", "Tmax_Klu"),
            selected = c("MHR")
          ),

          helpText(
            icon("lightbulb"),
            " Select which methods to calibrate against the primary method."
          )
        ),

        # sDMA settings
        box(
          width = 12,
          title = "sDMA Settings",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          checkboxInput(
            ns("apply_sdma"),
            "Apply sDMA method switching",
            value = TRUE
          ),

          conditionalPanel(
            condition = "input.apply_sdma",
            ns = ns,

            checkboxGroupInput(
              ns("sdma_methods"),
              "Methods for sDMA:",
              choices = c("MHR", "HRMXa", "HRMXb", "Tmax_Coh", "Tmax_Klu"),
              selected = c("MHR")
            ),

            helpText(
              icon("info-circle"),
              " Create sDMA variants that switch between HRM and each selected method."
            ),

            numericInput(
              ns("peclet_threshold"),
              "Peclet Threshold for Switching:",
              value = 1.0,
              min = 0.1,
              max = 5.0,
              step = 0.1
            ),

            helpText(
              icon("lightbulb"),
              " Pe < threshold → HRM, Pe ≥ threshold → Secondary method"
            )
          )
        ),

        # Additional settings
        box(
          width = 12,
          title = "Additional Settings",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          selectInput(
            ns("sensor_position"),
            "Sensor Position for Calibration:",
            choices = c("Outer" = "outer", "Inner" = "inner", "Both" = "both"),
            selected = "outer"
          ),

          helpText(
            icon("info-circle"),
            " Which sensor position to use for calculating calibration coefficients."
          )
        ),

        # Action button
        actionButton(
          ns("apply_calibration"),
          "Apply Calibration & sDMA",
          icon = icon("balance-scale"),
          class = "btn-primary btn-lg",
          style = "width: 100%; margin-top: 10px;"
        )
      ),

      # Right column: Results
      column(
        width = 8,

        # Results tabs
        tabBox(
          width = 12,
          title = "Results",

          # Tab 1: Calibration coefficients
          tabPanel(
            "Calibration Coefficients",
            icon = icon("table"),

            h4("Calibration Parameters"),
            p("Linear regression coefficients: Calibrated = Slope × Primary + Intercept"),

            DT::DTOutput(ns("calibration_table")),

            hr(),

            h4("Calibration Quality"),
            verbatimTextOutput(ns("calibration_summary"))
          ),

          # Tab 2: sDMA selection
          tabPanel(
            "sDMA Method Selection",
            icon = icon("exchange-alt"),

            conditionalPanel(
              condition = "input.apply_sdma == false",
              ns = ns,
              div(
                class = "alert alert-info",
                icon("info-circle"),
                " sDMA is disabled. Enable it in the settings to see method selection results."
              )
            ),

            conditionalPanel(
              condition = "input.apply_sdma == true",
              ns = ns,

              h4("Method Selection Frequency"),
              p("How often each method was selected by sDMA:"),

              plotly::plotlyOutput(ns("sdma_frequency_plot"), height = "400px"),

              hr(),

              h4("Selection by Sensor Position"),
              DT::DTOutput(ns("sdma_selection_table"))
            )
          ),

          # Tab 3: Comparison plot
          tabPanel(
            "Method Comparison",
            icon = icon("chart-line"),

            h4("Before vs After Calibration"),
            p("Comparison of methods before and after calibration for outer sensor:"),

            plotly::plotlyOutput(ns("comparison_plot"), height = "500px")
          ),

          # Tab 4: Data summary
          tabPanel(
            "Data Summary",
            icon = icon("info-circle"),

            h4("Calibrated Data Summary"),

            htmlOutput(ns("data_summary")),

            hr(),

            h4("Available Methods"),
            verbatimTextOutput(ns("methods_list"))
          )
        )
      )
    )
  )
}

# Server ----
calibrationSdmaServer <- function(id, vh_corrected, code_tracker = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store calibration result
    calibration_result <- reactiveVal(NULL)

    # Apply calibration when button clicked
    observeEvent(input$apply_calibration, {
      req(vh_corrected())

      # Validate that we have the required methods
      available_methods <- unique(vh_corrected()$method)

      if (!input$primary_method %in% available_methods) {
        showNotification(
          paste("Primary method", input$primary_method, "not found in data!"),
          type = "error",
          duration = 5
        )
        return()
      }

      if (length(input$secondary_methods) == 0) {
        showNotification(
          "Please select at least one secondary method to calibrate.",
          type = "warning",
          duration = 5
        )
        return()
      }

      missing_methods <- setdiff(input$secondary_methods, available_methods)
      if (length(missing_methods) > 0) {
        showNotification(
          paste("Methods not found in data:", paste(missing_methods, collapse = ", ")),
          type = "warning",
          duration = 5
        )
        return()
      }

      if (input$apply_sdma && length(input$sdma_methods) == 0) {
        showNotification(
          "sDMA is enabled but no methods selected. Please select methods for sDMA or disable it.",
          type = "warning",
          duration = 5
        )
        return()
      }

      # Show progress
      withProgress(message = "Applying calibration...", value = 0, {

        tryCatch({

          incProgress(0.3, detail = "Calibrating methods...")

          # Determine which sDMA methods to use
          sdma_methods_to_use <- if (input$apply_sdma) {
            input$sdma_methods
          } else {
            NULL
          }

          # Apply late calibration + sDMA
          result <- apply_late_calibration_sdma(
            vh_corrected = vh_corrected(),
            primary_method = input$primary_method,
            secondary_methods = input$secondary_methods,
            sdma_methods = sdma_methods_to_use,
            sensor_position = input$sensor_position,
            peclet_threshold = input$peclet_threshold
          )

          incProgress(0.5, detail = "Processing results...")

          # Store result
          calibration_result(result)

          # Track for code generation
          if (!is.null(code_tracker)) {
            code_tracker$add_step(
              step_name = "Calibration & sDMA",
              code = sprintf(
                'calibration_result <- apply_late_calibration_sdma(
  vh_corrected = vh_corrected,
  primary_method = "%s",
  secondary_methods = c("%s"),
  sdma_methods = %s,
  sensor_position = "%s",
  peclet_threshold = %.2f
)',
                input$primary_method,
                paste(input$secondary_methods, collapse = '", "'),
                if (input$apply_sdma && length(sdma_methods_to_use) > 0) {
                  sprintf('c("%s")', paste(sdma_methods_to_use, collapse = '", "'))
                } else {
                  "NULL"
                },
                input$sensor_position,
                input$peclet_threshold
              ),
              description = "Apply method calibration and sDMA to corrected velocities",
              params = list(
                primary = input$primary_method,
                secondary = input$secondary_methods,
                sdma = sdma_methods_to_use,
                sensor_position = input$sensor_position,
                peclet_threshold = input$peclet_threshold
              )
            )
          }

          incProgress(1.0, detail = "Complete!")

          showNotification(
            "Calibration and sDMA applied successfully!",
            type = "message",
            duration = 3
          )

        }, error = function(e) {
          showNotification(
            paste("Error applying calibration:", e$message),
            type = "error",
            duration = NULL
          )
        })
      })
    })

    # Output: Calibration coefficients table
    output$calibration_table <- DT::renderDT({
      req(calibration_result())

      cal_params <- calibration_result()$calibration_params

      if (!is.null(cal_params) && nrow(cal_params) > 0) {
        DT::datatable(
          cal_params,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 't'
          ),
          rownames = FALSE
        ) %>%
          DT::formatRound(columns = c("slope", "intercept", "r_squared", "rmse"), digits = 4)
      } else {
        DT::datatable(
          data.frame(Message = "No calibration results available"),
          options = list(dom = 't'),
          rownames = FALSE
        )
      }
    })

    # Output: Calibration summary
    output$calibration_summary <- renderPrint({
      req(calibration_result())

      cal_params <- calibration_result()$calibration_params

      if (!is.null(cal_params) && nrow(cal_params) > 0) {
        cat("Calibration Quality Summary:\n")
        cat("============================\n\n")

        for (i in seq_len(nrow(cal_params))) {
          row <- cal_params[i, ]
          cat(sprintf("Method: %s\n", row$secondary_method))
          cat(sprintf("  R² = %.4f (%.1f%% variance explained)\n",
                     row$r_squared, row$r_squared * 100))
          cat(sprintf("  RMSE = %.4f cm/hr\n", row$rmse))
          cat(sprintf("  Equation: Calibrated = %.4f × %s + %.4f\n\n",
                     row$slope, input$primary_method, row$intercept))
        }
      } else {
        cat("No calibration results available.\n")
      }
    })

    # Output: sDMA frequency plot
    output$sdma_frequency_plot <- plotly::renderPlotly({
      req(calibration_result())
      req(input$apply_sdma)

      vh_sdma <- calibration_result()$vh_with_sdma

      if (is.null(vh_sdma)) {
        return(plotly::plotly_empty())
      }

      # Filter to sDMA methods only
      sdma_data <- vh_sdma %>%
        dplyr::filter(grepl("^sDMA:", method)) %>%
        dplyr::group_by(method, selected_method, sensor_position) %>%
        dplyr::summarise(count = dplyr::n(), .groups = "drop")

      if (nrow(sdma_data) == 0) {
        return(plotly::plotly_empty())
      }

      # Create stacked bar plot
      plotly::plot_ly(
        sdma_data,
        x = ~method,
        y = ~count,
        color = ~selected_method,
        type = "bar",
        text = ~paste(selected_method, ":", count),
        hovertemplate = "%{text}<br>Sensor: %{customdata}<extra></extra>",
        customdata = ~sensor_position
      ) %>%
        plotly::layout(
          title = "sDMA Method Selection Frequency",
          xaxis = list(title = "sDMA Method"),
          yaxis = list(title = "Number of Measurements"),
          barmode = "stack",
          legend = list(title = list(text = "Selected Method"))
        )
    })

    # Output: sDMA selection table
    output$sdma_selection_table <- DT::renderDT({
      req(calibration_result())
      req(input$apply_sdma)

      vh_sdma <- calibration_result()$vh_with_sdma

      if (is.null(vh_sdma)) {
        return(DT::datatable(
          data.frame(Message = "No sDMA results available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Summarize selection by method and sensor
      sdma_summary <- vh_sdma %>%
        dplyr::filter(grepl("^sDMA:", method)) %>%
        dplyr::group_by(method, sensor_position, selected_method) %>%
        dplyr::summarise(
          count = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::group_by(method, sensor_position) %>%
        dplyr::mutate(
          percentage = count / sum(count) * 100
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(method, sensor_position, dplyr::desc(count))

      DT::datatable(
        sdma_summary,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = "percentage", digits = 1)
    })

    # Output: Comparison plot
    output$comparison_plot <- plotly::renderPlotly({
      req(calibration_result())
      req(vh_corrected())

      # Get outer sensor data only for clarity
      before <- vh_corrected() %>%
        dplyr::filter(
          sensor_position == "outer",
          method %in% c(input$primary_method, input$secondary_methods)
        ) %>%
        dplyr::mutate(status = "Before Calibration")

      after <- calibration_result()$vh_calibrated %>%
        dplyr::filter(
          sensor_position == "outer",
          method %in% c(input$primary_method, input$secondary_methods)
        ) %>%
        dplyr::mutate(status = "After Calibration")

      combined <- dplyr::bind_rows(before, after)

      if (nrow(combined) == 0) {
        return(plotly::plotly_empty())
      }

      # Sample data if too large (for performance)
      if (nrow(combined) > 5000) {
        set.seed(42)
        combined <- combined %>%
          dplyr::slice_sample(n = 5000)
      }

      # Create faceted plot
      plotly::plot_ly(
        combined,
        x = ~datetime,
        y = ~Vh_cm_hr,
        color = ~method,
        linetype = ~status,
        type = "scatter",
        mode = "lines",
        alpha = 0.7
      ) %>%
        plotly::layout(
          title = "Method Comparison: Before vs After Calibration (Outer Sensor)",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Velocity (cm/hr)"),
          hovermode = "closest"
        )
    })

    # Output: Data summary
    output$data_summary <- renderUI({
      req(calibration_result())

      vh_calibrated <- calibration_result()$vh_calibrated
      vh_sdma <- calibration_result()$vh_with_sdma

      n_calibrated <- nrow(vh_calibrated)
      n_with_sdma <- if (!is.null(vh_sdma)) nrow(vh_sdma) else n_calibrated

      methods_calibrated <- setdiff(unique(vh_calibrated$method), input$primary_method)
      methods_sdma <- if (!is.null(vh_sdma)) {
        unique(vh_sdma$method[grepl("^sDMA:", vh_sdma$method)])
      } else {
        character(0)
      }

      tagList(
        tags$dl(
          tags$dt("Primary Method:"),
          tags$dd(input$primary_method),

          tags$dt("Calibrated Methods:"),
          tags$dd(paste(methods_calibrated, collapse = ", ")),

          tags$dt("sDMA Methods Created:"),
          tags$dd(if (length(methods_sdma) > 0) paste(methods_sdma, collapse = ", ") else "None"),

          tags$dt("Measurements (calibrated only):"),
          tags$dd(format(n_calibrated, big.mark = ",")),

          tags$dt("Measurements (with sDMA):"),
          tags$dd(format(n_with_sdma, big.mark = ","))
        )
      )
    })

    # Output: Methods list
    output$methods_list <- renderPrint({
      req(calibration_result())

      vh_final <- if (!is.null(calibration_result()$vh_with_sdma)) {
        calibration_result()$vh_with_sdma
      } else {
        calibration_result()$vh_calibrated
      }

      methods <- unique(vh_final$method)

      cat("Available Methods After Calibration & sDMA:\n")
      cat("==========================================\n\n")

      for (method in sort(methods)) {
        n <- sum(vh_final$method == method)
        cat(sprintf("  - %s (%s measurements)\n", method, format(n, big.mark = ",")))
      }
    })

    # Return calibrated data for downstream use
    return(
      reactive({
        if (is.null(calibration_result())) {
          return(NULL)
        }

        # Return vh_with_sdma if available, otherwise vh_calibrated
        if (!is.null(calibration_result()$vh_with_sdma)) {
          calibration_result()$vh_with_sdma
        } else {
          calibration_result()$vh_calibrated
        }
      })
    )
  })
}
