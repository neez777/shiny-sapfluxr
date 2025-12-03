# mod_corrections_new.R
# Comprehensive Correction Module for Vh Data
#
# Implements full correction workflow:
# 1. Zero-flow offset correction (linear, universal)
# 2. Burgess spacing correction (HRM-specific, segmented)
# 3. Wound correction (final step)
#
# Supports hybrid data structure with correction tracking

# UI ----
correctionsNewUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Workflow Steps
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "Correction Workflow",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,

          p("Apply corrections in sequence to improve velocity estimates:"),
          tags$ol(
            tags$li(strong("Zero-flow offset:"), " Universal linear correction based on nighttime data"),
            tags$li(strong("Spacing correction:"), " Physics-based Burgess correction for HRM (optional)"),
            tags$li(strong("Wound correction:"), " Accounts for probe wound effects")
          ),
          hr(),
          p(class = "text-muted", tags$small(
            icon("info-circle"),
            " Each step preserves previous data. You can compare raw vs corrected values."
          ))
        ),

        # Step 1: Zero-flow offset
        box(
          width = 12,
          title = "Step 1: Zero-Flow Offset Correction",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,

          helpText("Define periods where sap flow should be zero (e.g., nighttime, dormant season)."),

          # Period definition
          fluidRow(
            column(6, dateInput(ns("zf_start_date"), "Start Date", value = NULL)),
            column(6, textInput(ns("zf_start_time"), "Time (HH:MM)", value = "22:00"))
          ),
          fluidRow(
            column(6, dateInput(ns("zf_end_date"), "End Date", value = NULL)),
            column(6, textInput(ns("zf_end_time"), "Time (HH:MM)", value = "06:00"))
          ),

          actionButton(ns("add_zf_period"), "Add Zero-Flow Period",
                      icon = icon("plus"), class = "btn-success", width = "100%"),

          hr(),
          h5("Defined Zero-Flow Periods:"),
          uiOutput(ns("zf_periods_list")),

          br(),
          actionButton(ns("clear_zf_periods"), "Clear All Periods",
                      icon = icon("trash"), class = "btn-warning btn-sm"),

          hr(),

          # Method and sensor selection
          selectInput(ns("zf_methods"), "Methods to Correct",
                     choices = c("All methods" = "ALL", "HRM" = "HRM", "MHR" = "MHR"),
                     selected = "ALL"),

          checkboxGroupInput(ns("zf_sensors"), "Sensors to Correct",
                           choices = c("Outer" = "outer", "Inner" = "inner"),
                           selected = c("outer", "inner")),

          br(),
          actionButton(ns("apply_zf_correction"), "Apply Zero-Flow Offset Correction",
                      icon = icon("play"), class = "btn-primary", width = "100%")
        ),

        # Step 2: Spacing correction (optional, HRM only)
        box(
          width = 12,
          title = "Step 2: Spacing Correction (Optional)",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          p(class = "text-info",
            icon("info-circle"),
            " Physics-based Burgess correction for HRM. Only use if offset validation passes."
          ),

          helpText("This correction is applied INSTEAD of zero-flow offset for HRM data when offset ≤ 5 cm/hr."),

          numericInput(ns("sc_probe_spacing"), "Probe Spacing (cm)",
                      value = 0.5, min = 0.1, max = 2.0, step = 0.1),

          numericInput(ns("sc_measurement_time"), "Measurement Time (sec)",
                      value = 80, min = 30, max = 150, step = 10),

          selectInput(ns("sc_sensor"), "Sensor to Correct",
                     choices = c("Outer" = "outer", "Inner" = "inner"),
                     selected = "outer"),

          br(),
          actionButton(ns("apply_spacing_correction"),
                      "Apply Spacing Correction (HRM)",
                      icon = icon("play"), class = "btn-warning", width = "100%"),

          br(), br(),
          p(class = "text-muted", tags$small(
            "Note: If validation fails (offset > max), this will fall back to linear offset."
          ))
        ),

        # Step 3: Wound correction
        box(
          width = 12,
          title = "Step 3: Wound Correction",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          helpText("Corrects for reduced sap flow around probe wound."),

          numericInput(ns("wc_wound_diameter"), "Wound Diameter (cm)",
                      value = 0.20, min = 0.05, max = 1.0, step = 0.05),

          selectInput(ns("wc_probe_spacing_preset"), "Probe Spacing",
                     choices = c("5mm (ICT standard)" = "5mm",
                               "6mm" = "6mm",
                               "10mm" = "10mm",
                               "Custom" = "custom"),
                     selected = "5mm"),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("wc_probe_spacing_preset")),
            numericInput(ns("wc_probe_spacing_custom"), "Custom Spacing (cm)",
                        value = 0.5, min = 0.1, max = 2.0, step = 0.05)
          ),

          checkboxInput(ns("wc_use_spacing_corrected"),
                       "Apply to spacing-corrected data (if available)",
                       value = TRUE),

          br(),
          actionButton(ns("apply_wound_correction"), "Apply Wound Correction",
                      icon = icon("play"), class = "btn-success", width = "100%")
        ),

        # Reset button
        box(
          width = 12,
          title = "Reset",
          status = "danger",
          solidHeader = TRUE,

          actionButton(ns("reset_all_corrections"), "Reset to Raw Data",
                      icon = icon("undo"), class = "btn-danger", width = "100%")
        )
      ),

      # Right column: Visualisation and Status
      column(
        width = 8,

        # Correction status box
        box(
          width = 12,
          title = "Current Correction Status",
          status = "success",
          solidHeader = TRUE,

          verbatimTextOutput(ns("correction_status")),

          hr(),

          h5("Available Data Columns:"),
          verbatimTextOutput(ns("available_columns"))
        ),

        # Comparison plot
        box(
          width = 12,
          title = "Correction Comparison",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,

          helpText("Compare raw vs corrected velocities."),

          fluidRow(
            column(4, selectInput(ns("plot_method"), "Method",
                                 choices = c("HRM", "MHR"), selected = "HRM")),
            column(4, selectInput(ns("plot_sensor"), "Sensor",
                                 choices = c("Outer" = "outer", "Inner" = "inner"),
                                 selected = "outer")),
            column(4, dateRangeInput(ns("plot_date_range"), "Date Range",
                                     start = NULL, end = NULL))
          ),

          plotly::plotlyOutput(ns("correction_comparison_plot"), height = "500px")
        ),

        # Offset results (if applied)
        box(
          width = 12,
          title = "Zero-Flow Offset Results",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_zf_results")),
            verbatimTextOutput(ns("zf_results_summary"))
          ),

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_zf_results")),
            p(em("No zero-flow offset correction applied yet."))
          )
        ),

        # Spacing correction results (if applied)
        box(
          width = 12,
          title = "Spacing Correction Results",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_sc_results")),
            verbatimTextOutput(ns("sc_results_summary"))
          ),

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_sc_results")),
            p(em("No spacing correction applied yet."))
          )
        ),

        # Wound correction results (if applied)
        box(
          width = 12,
          title = "Wound Correction Results",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_wc_results")),
            verbatimTextOutput(ns("wc_results_summary"))
          ),

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_wc_results")),
            p(em("No wound correction applied yet."))
          )
        )
      )
    )
  )
}

# Server ----
correctionsNewServer <- function(id, vh_results, probe_config, wood_properties) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
      vh_data = NULL,  # Current working data (gets updated after each correction)
      zf_periods = list(),  # Zero-flow periods
      zf_results = NULL,  # Zero-flow offset results
      sc_results = NULL,  # Spacing correction results
      wc_results = NULL,  # Wound correction results
      correction_history = character(0)  # Track what corrections have been applied
    )

    # Initialize vh_data from input
    observe({
      req(vh_results())
      if (is.null(rv$vh_data)) {
        rv$vh_data <- vh_results()
      }
    })

    # Initialize date inputs
    observe({
      req(vh_results())

      vh_data <- vh_results()
      if (nrow(vh_data) > 0) {
        date_range <- range(vh_data$datetime, na.rm = TRUE)

        # Set zero-flow period to first night
        updateDateInput(session, "zf_start_date", value = as.Date(date_range[1]))
        updateDateInput(session, "zf_end_date", value = as.Date(date_range[1]) + 1)

        # Set plot date range
        updateDateRangeInput(session, "plot_date_range",
                           start = as.Date(date_range[1]),
                           end = as.Date(date_range[2]))
      }
    })

    # ========================================================================
    # ZERO-FLOW OFFSET CORRECTION
    # ========================================================================

    # Add zero-flow period
    observeEvent(input$add_zf_period, {
      req(input$zf_start_date, input$zf_end_date,
          input$zf_start_time, input$zf_end_time)

      tryCatch({
        start_str <- paste(input$zf_start_date, input$zf_start_time)
        end_str <- paste(input$zf_end_date, input$zf_end_time)

        start_dt <- as.POSIXct(start_str, format = "%Y-%m-%d %H:%M")
        end_dt <- as.POSIXct(end_str, format = "%Y-%m-%d %H:%M")

        if (is.na(start_dt) || is.na(end_dt)) {
          showNotification("Invalid date/time format", type = "error")
          return()
        }

        if (end_dt <= start_dt) {
          showNotification("End must be after start", type = "error")
          return()
        }

        # Add period
        period <- list(start = start_str, end = end_str)
        rv$zf_periods <- c(rv$zf_periods, list(period))

        showNotification(
          sprintf("Added zero-flow period: %s to %s", start_str, end_str),
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        showNotification(paste("Error adding period:", e$message), type = "error")
      })
    })

    # Clear all zero-flow periods
    observeEvent(input$clear_zf_periods, {
      rv$zf_periods <- list()
      showNotification("Cleared all zero-flow periods", type = "message")
    })

    # Display zero-flow periods
    output$zf_periods_list <- renderUI({
      if (length(rv$zf_periods) == 0) {
        return(p(em("No periods defined yet.")))
      }

      period_items <- lapply(seq_along(rv$zf_periods), function(i) {
        period <- rv$zf_periods[[i]]
        div(
          style = "margin-bottom: 5px;",
          actionButton(
            session$ns(paste0("delete_zf_", i)),
            label = NULL,
            icon = icon("times"),
            class = "btn-xs btn-danger",
            style = "margin-right: 10px;"
          ),
          span(sprintf("%d. %s to %s", i, period$start, period$end))
        )
      })

      tagList(period_items)
    })

    # Handle delete buttons for zero-flow periods
    observe({
      lapply(seq_along(rv$zf_periods), function(i) {
        btn_id <- paste0("delete_zf_", i)
        observeEvent(input[[btn_id]], {
          rv$zf_periods <- rv$zf_periods[-i]
          showNotification(sprintf("Removed period %d", i), type = "message")
        }, ignoreInit = TRUE)
      })
    })

    # Apply zero-flow offset correction
    observeEvent(input$apply_zf_correction, {
      req(rv$vh_data)

      if (length(rv$zf_periods) == 0) {
        showNotification("Please define at least one zero-flow period", type = "warning")
        return()
      }

      tryCatch({
        # Determine methods
        methods <- if (input$zf_methods == "ALL") {
          NULL  # Apply to all
        } else {
          input$zf_methods
        }

        # Apply correction
        vh_corrected <- sapfluxr::apply_zero_flow_offset(
          vh_data = rv$vh_data,
          zero_periods = rv$zf_periods,
          sensors = input$zf_sensors,
          methods = methods,
          vh_col = "Vh_cm_hr",
          verbose = TRUE
        )

        # Store results
        rv$vh_data <- vh_corrected
        rv$zf_results <- attr(vh_corrected, "zero_flow_offset_results")
        rv$correction_history <- c(rv$correction_history, "zero_flow_offset")

        showNotification(
          "Zero-flow offset correction applied successfully!",
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        showNotification(
          paste("Error applying zero-flow correction:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # ========================================================================
    # SPACING CORRECTION
    # ========================================================================

    observeEvent(input$apply_spacing_correction, {
      req(rv$vh_data)

      tryCatch({
        # Apply spacing correction with validation
        vh_corrected <- sapfluxr::apply_spacing_correction_workflow(
          vh_data = rv$vh_data,
          zero_periods = rv$zf_periods,
          sensor_position = input$sc_sensor,
          probe_spacing = input$sc_probe_spacing,
          measurement_time = input$sc_measurement_time,
          verbose = TRUE
        )

        # Store results
        rv$vh_data <- vh_corrected
        rv$sc_results <- attr(vh_corrected, "spacing_correction_results")
        rv$correction_history <- c(rv$correction_history, "spacing_correction")

        showNotification(
          "Spacing correction applied successfully!",
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        showNotification(
          paste("Error applying spacing correction:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # ========================================================================
    # WOUND CORRECTION
    # ========================================================================

    observeEvent(input$apply_wound_correction, {
      req(rv$vh_data)

      tryCatch({
        # Determine probe spacing
        probe_spacing <- if (input$wc_probe_spacing_preset == "custom") {
          input$wc_probe_spacing_custom
        } else {
          input$wc_probe_spacing_preset
        }

        # Apply wound correction
        vh_corrected <- sapfluxr::apply_wound_correction(
          vh_data = rv$vh_data,
          wound_diameter = input$wc_wound_diameter,
          probe_spacing = probe_spacing,
          use_spacing_corrected = input$wc_use_spacing_corrected
        )

        # Store results
        rv$vh_data <- vh_corrected
        rv$wc_results <- attr(vh_corrected, "wound_correction_results")
        rv$correction_history <- c(rv$correction_history, "wound_correction")

        showNotification(
          "Wound correction applied successfully!",
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        showNotification(
          paste("Error applying wound correction:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # ========================================================================
    # RESET
    # ========================================================================

    observeEvent(input$reset_all_corrections, {
      rv$vh_data <- vh_results()
      rv$zf_results <- NULL
      rv$sc_results <- NULL
      rv$wc_results <- NULL
      rv$correction_history <- character(0)

      showNotification("Reset to raw data", type = "message", duration = 3)
    })

    # ========================================================================
    # OUTPUTS
    # ========================================================================

    # Correction status
    output$correction_status <- renderPrint({
      if (is.null(rv$vh_data)) {
        cat("No data loaded\n")
        return()
      }

      # Use helper function from sapfluxr
      status_label <- sapfluxr::build_correction_status_label(rv$vh_data)
      cat(status_label, "\n")
    })

    # Available columns
    output$available_columns <- renderPrint({
      req(rv$vh_data)

      vh_cols <- grep("^Vh_cm_hr", names(rv$vh_data), value = TRUE)
      cat(paste(vh_cols, collapse = ", "), "\n")
    })

    # Has results flags
    output$has_zf_results <- reactive({ !is.null(rv$zf_results) })
    output$has_sc_results <- reactive({ !is.null(rv$sc_results) })
    output$has_wc_results <- reactive({ !is.null(rv$wc_results) })

    outputOptions(output, "has_zf_results", suspendWhenHidden = FALSE)
    outputOptions(output, "has_sc_results", suspendWhenHidden = FALSE)
    outputOptions(output, "has_wc_results", suspendWhenHidden = FALSE)

    # Zero-flow results summary
    output$zf_results_summary <- renderPrint({
      req(rv$zf_results)

      cat("ZERO-FLOW OFFSET RESULTS\n")
      cat(strrep("=", 60), "\n\n")

      for (name in names(rv$zf_results)) {
        result <- rv$zf_results[[name]]
        cat(sprintf("%s:\n", name))
        cat(sprintf("  Offset: %.2f cm/hr\n", result$offset))
        cat(sprintf("  N obs: %d\n", result$n_observations))
        cat(sprintf("  SD: %.2f cm/hr\n\n", result$sd_offset))
      }
    })

    # Spacing correction results summary
    output$sc_results_summary <- renderPrint({
      req(rv$sc_results)

      cat("SPACING CORRECTION RESULTS\n")
      cat(strrep("=", 60), "\n\n")

      # Display based on results structure
      print(rv$sc_results)
    })

    # Wound correction results summary
    output$wc_results_summary <- renderPrint({
      req(rv$wc_results)

      cat("WOUND CORRECTION RESULTS\n")
      cat(strrep("=", 60), "\n\n")

      print(rv$wc_results)
    })

    # Correction comparison plot
    output$correction_comparison_plot <- plotly::renderPlotly({
      req(rv$vh_data, input$plot_method, input$plot_sensor)

      tryCatch({
        # Filter data
        vh_subset <- rv$vh_data[
          rv$vh_data$method == input$plot_method &
          rv$vh_data$sensor_position == input$plot_sensor,
        ]

        # Apply date range filter if specified
        if (!is.null(input$plot_date_range)) {
          start_date <- as.POSIXct(input$plot_date_range[1])
          end_date <- as.POSIXct(input$plot_date_range[2]) + 86400  # Add 1 day

          vh_subset <- vh_subset[
            vh_subset$datetime >= start_date & vh_subset$datetime <= end_date,
          ]
        }

        if (nrow(vh_subset) == 0) {
          return(plotly::plotly_empty())
        }

        # Use package plotting function
        sapfluxr::plot_correction_comparison(vh_subset)

      }, error = function(e) {
        plotly::plotly_empty()
      })
    })

    # Return corrected data for downstream modules
    return(reactive({
      if (is.null(rv$vh_data)) {
        vh_results()
      } else {
        rv$vh_data
      }
    }))
  })
}
