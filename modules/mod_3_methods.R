#' Method Selection and Calculation Module
#'
#' Shiny module for selecting HPV calculation methods and running calculations
#'
#' @param id Module ID
#' @param heat_pulse_data Reactive containing corrected heat_pulse_data
#' @param probe_config Reactive containing probe configuration
#' @param wood_properties Reactive containing wood properties
#' @return Reactive containing vh_results
#'

# UI ----
methodsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Method Selection
      column(
        width = 5,
        box(
          width = NULL,
          title = "Select Calculation Methods",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p("Select one or more heat pulse velocity calculation methods:"),

          selectInput(
            ns("baseline_method"),
            HTML('Pre-pulse baseline method: <span style="color: #999; cursor: help;" title="Method used to estimate the pre-pulse reference temperature. mean_30s averages the 30 seconds before the pulse (default). mean_3s uses only the last 3 seconds, better for dynamic conditions. slope_intercept fits a linear trend to correct for gradual drift."><i class="fa fa-circle-question"></i></span>'),
            choices = c(
              "30-second average (default)" = "mean_30s",
              "3-second average" = "mean_3s",
              "Slope-intercept (drift correction)" = "slope_intercept"
            ),
            selected = "mean_30s"
          ),

          hr(),

          checkboxGroupInput(
            ns("methods"),
            NULL,
            choices = c(
              "HRM - Heat Ratio Method (low/reverse flows)" = "HRM",
              "MHR - Maximum Heat Ratio (moderate to high flows)" = "MHR",
              "Tmax (Cohen) - Time-to-peak method" = "Tmax_Coh",
              "Tmax (Kluitenberg) - Time-to-peak method" = "Tmax_Klu"
            ),
            selected = c("HRM", "MHR")
          )

          # ==================================================================
          # sDMA POST-PROCESSING - REMOVED
          #
          # Post-Processing Options UI has been temporarily removed.
          # sDMA functionality will be re-implemented in a later workflow stage
          # (after wound correction, before flux density calculation).
          #
          # Complete implementation preserved in: R/04j_sdma_methods.R
          # See: SDMA_EXTRACTION_STATUS.md for extraction status
          # ==================================================================

          # REMOVED: hr(), h5("Post-Processing Options")
          # REMOVED: checkboxInput apply_sdma
          # REMOVED: conditionalPanel with sdma_secondary
          # REMOVED: uiOutput sdma_warning
#        )
      ),

      # Quality Check Configuration
 #     column(
 #       width = 6,
        box(
          width = NULL,
          title = "Quality Check Settings",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          p("Configure quality control checks applied after calculation:"),

          # Illogical Values
          div(
            style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px; border-radius: 3px;",
            h5("Illogical Values", style = "margin-top: 0;"),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  ns("qc_check_illogical"),
                  HTML('Check illogical values <span style="color: #999; cursor: help;" title="Flags physically impossible values like infinite velocities, extremely high flows that exceed maximum possible rates for the species and method, or calculations that failed due to invalid temperature ratios."><i class="fa fa-circle-question"></i></span>'),
                  value = TRUE
                ),
                checkboxInput(
                  ns("qc_flag_negative"),
                  HTML('Flag negative flows <span style="color: #999; cursor: help;" title="Marks negative velocity values as suspect. Negative flows may indicate genuine reverse flow (e.g., stem water refilling at night) or measurement errors from probe misalignment or thermal asymmetries."><i class="fa fa-circle-question"></i></span>'),
                  value = TRUE
                )
              ),
              column(
                width = 5,
                numericInput(
                  ns("qc_hard_max_vh"),
                  HTML('Absolute max velocity (cm/hr): <span style="color: #999; cursor: help;" title="Hard upper limit for biologically plausible sap velocities. Any velocity exceeding this value is automatically flagged as an error. Typical values: 100-200 cm/hr for diffuse-porous, 200-400 cm/hr for ring-porous species."><i class="fa fa-circle-question"></i></span>'),
                  value = 500,
                  min = 50,
                  max = 1000,
                  step = 50
                )
              )
            )
          ),

          # Statistical Outliers
          div(
            style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px; border-radius: 3px;",
            h5("Statistical Outliers", style = "margin-top: 0;"),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  ns("qc_detect_rate_of_change"),
                  HTML('Detect rate of change outliers <span style="color: #999; cursor: help;" title="Flags sudden jumps between consecutive measurements that exceed biologically plausible rates of change. Trees cannot instantly increase or decrease sap flow beyond certain physiological limits."><i class="fa fa-circle-question"></i></span>'),
                  value = TRUE
                ),
                checkboxInput(
                  ns("qc_detect_outliers"),
                  HTML('Detect outliers (rolling mean) <span style="color: #999; cursor: help;" title="Uses a rolling window to calculate local mean and standard deviation, then flags values that deviate too far from the local trend. Effective for catching isolated spikes or anomalies that don\'t fit the surrounding pattern."><i class="fa fa-circle-question"></i></span>'),
                  value = TRUE
                )
              ),
              column(
                width = 5,
                numericInput(
                  ns("qc_max_change_cm_hr"),
                  HTML('Max velocity change (cm/hr): <span style="color: #999; cursor: help;" title="Maximum allowed velocity change between consecutive measurements. Larger jumps are flagged as suspect. Should reflect realistic biological rates of change for your species and measurement interval."><i class="fa fa-circle-question"></i></span>'),
                  value = 4,
                  min = 0.1,
                  max = 50,
                  step = 0.5
                ),
                numericInput(
                  ns("qc_rolling_window"),
                  HTML('Rolling window half-width: <span style="color: #999; cursor: help;" title="Number of observations before and after the current point used to calculate local statistics. Larger windows produce smoother, less sensitive detection. Example: value of 5 uses 11 total points (5 before + current + 5 after)."><i class="fa fa-circle-question"></i></span>'),
                  value = 5,
                  min = 2,
                  max = 20,
                  step = 1
                ),
                numericInput(
                  ns("qc_rolling_threshold"),
                  HTML('Rolling outlier threshold (SD): <span style="color: #999; cursor: help;" title="How many standard deviations from the rolling mean before flagging as an outlier. Higher values are more lenient (fewer flags), lower values are stricter. Typical range: 2-4 SD."><i class="fa fa-circle-question"></i></span>'),
                  value = 3,
                  min = 1,
                  max = 10,
                  step = 0.5
                )
              )
            )
          ),

          p(class = "help-text", style = "margin-top: 10px; font-size: 0.9em; color: #666;",
            icon("info-circle"),
            " These settings control outlier detection sensitivity and data validation. ",
            "Lower thresholds are more strict, higher are more lenient.")
        )
      ),

#    fluidRow(
      # Calculate Button & Status
      column(
        width = 7,
        box(
          width = NULL,
          title = "Run Calculations",
          status = "success",
          solidHeader = TRUE,

          uiOutput(ns("calculation_status")),

          hr(),

          actionButton(
            ns("calculate"),
            "Calculate Heat Pulse Velocity",
            icon = icon("play-circle"),
            class = "btn-success btn-lg",
            style = "width: 100%;"
          ),

          hr(),

          uiOutput(ns("results_summary"))
        )
      ),

    # Results Table
    fluidRow(
      column(
        width = 12,
      box(
        width = 12,
        title = "Calculation Results",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        DT::dataTableOutput(ns("results_table"))
      )
      )
    )
  )
)
}

# Server ----
methodsServer <- function(id, heat_pulse_data, probe_config, wood_properties, code_tracker = TRUE) {
  moduleServer(id, function(input, output, session) {

    # Reactive to store results
    vh_results <- reactiveVal(NULL)

    # ==================================================================
    # sDMA SERVER LOGIC - REMOVED
    # ==================================================================
    # REMOVED: observeEvent(input$apply_sdma, ...) - auto-select sDMA methods
    # REMOVED: output$sdma_warning - HRM requirement warning
    # See: R/04j_sdma_methods.R for preserved implementation

    # Calculation status display
    output$calculation_status <- renderUI({
      req(heat_pulse_data())

      data <- heat_pulse_data()
      n_pulses <- data$metadata$n_pulses

      div(
        p(strong("Ready to calculate")),
        tags$ul(
          tags$li(paste("Data loaded:", data$metadata$file_name)),
          tags$li(paste("Pulses:", format(n_pulses, big.mark = ","))),
          tags$li(paste("Probe:", if (!is.null(probe_config())) probe_config()$config_name else "Default")),
          tags$li(paste("Wood:", if (!is.null(wood_properties())) wood_properties()$config_name else "Default"))
        )
      )
    })

    # Calculate button action
    observeEvent(input$calculate, {
      req(heat_pulse_data())
      req(length(input$methods) > 0)

      # Clear previous results immediately
      vh_results(NULL)

      # ==================================================================
      # sDMA VALIDATION - REMOVED
      # ==================================================================
      # REMOVED: sDMA validation check for HRM requirement

      tryCatch({
        # Get data and configs
        data <- heat_pulse_data()
        probe <- probe_config()
        wood <- wood_properties()

        # Calculate total progress steps
        n_pulses <- data$metadata$n_pulses
        n_methods <- length(input$methods)
        total_steps <- n_pulses * n_methods

        # Use Shiny's native withProgress for reliable progress reporting
        results <- NULL
        shiny::withProgress(message = "Calculating Heat Pulse Velocities", value = 0, {

          # Set up progressr handler to update Shiny progress
          progressr::handlers(progressr::handler_shiny(enable = TRUE))

          # Wrap in progressr context for compatibility
          results <- progressr::with_progress({
            sapfluxr::calc_heat_pulse_velocity(
              heat_pulse_data = data,
              methods = input$methods,
              probe_config = probe,
              wood_properties = wood,
              baseline_method = input$baseline_method,
              confirm_parameters = FALSE,
              show_progress = TRUE
            )
          })
        })

        # ==================================================================
        # sDMA PROCESSING - REMOVED
        # ==================================================================
        # REMOVED: All sDMA application logic including:
        # - Peclet number checking
        # - sDMA confirmation dialogs
        # - apply_sdma_processing() calls
        # - Method switching logic
        #
        # sDMA will be re-implemented in a later workflow stage after
        # wound correction. See R/04j_sdma_methods.R for preserved code.
        # ==================================================================

        # Apply quality control with user-configured parameters
        shiny::withProgress(message = "Applying Quality Checks", value = 0.5, {
          # Only the arguments with a corresponding UI control are passed. The
          # panel has no input for detect_missing_pulses, check_cross_sensor,
          # cross_sensor_threshold, add_rows_for_missing or max_gap_to_fill_hours,
          # so passing input$qc_* for those sent NULL into flag_vh_quality(),
          # which failed with "argument is of length zero". The error was caught
          # and the unflagged results returned, so quality control was silently
          # skipped for every run. Omitting them applies the documented defaults.
          results <- tryCatch({
            sapfluxr::flag_vh_quality(
              results,
              check_illogical = input$qc_check_illogical,
              hard_max_vh = input$qc_hard_max_vh,
              flag_negative = input$qc_flag_negative,
              detect_outliers = input$qc_detect_outliers,
              rolling_window = input$qc_rolling_window,
              rolling_threshold = input$qc_rolling_threshold,
              detect_rate_of_change = input$qc_detect_rate_of_change,
              max_change_cm_hr = input$qc_max_change_cm_hr,
              verbose = FALSE,
              return_full_report = FALSE
            )
          }, error = function(e) {
            # Degrading to unflagged results is a material loss of information,
            # so say so in the app rather than only in the console.
            message("Quality control failed: ", e$message)
            showNotification(
              paste0("Quality control could not be applied: ", e$message,
                     " Results are unflagged."),
              type = "error", duration = 10
            )
            results
          })
        })

        # Store results
        vh_results(results)

        # Track code generation
        if (!isTRUE(code_tracker)) {
          # Build quality control parameters code
          qc_params <- c()
          if (input$qc_check_illogical) {
            qc_params <- c(qc_params,
                          sprintf("  check_illogical = TRUE"),
                          sprintf("  hard_max_vh = %g", input$qc_hard_max_vh))
          }
          if (input$qc_flag_negative) {
            qc_params <- c(qc_params, "  flag_negative = TRUE")
          }
          if (input$qc_detect_outliers) {
            qc_params <- c(qc_params,
                          sprintf("  detect_outliers = TRUE"),
                          sprintf("  rolling_window = %d", input$qc_rolling_window),
                          sprintf("  rolling_threshold = %g", input$qc_rolling_threshold))
          }
          if (input$qc_detect_rate_of_change) {
            qc_params <- c(qc_params,
                          sprintf("  detect_rate_of_change = TRUE"),
                          sprintf("  max_change_cm_hr = %g", input$qc_max_change_cm_hr))
          }

          qc_code <- if (length(qc_params) > 0) {
            sprintf("\n\n# Apply quality control\nvh_results <- sapfluxr::flag_vh_quality(\n  vh_results,\n%s\n)",
                   paste(qc_params, collapse = ",\n"))
          } else {
            ""
          }

          baseline_arg <- if (input$baseline_method != "mean_30s") {
            sprintf('  baseline_method = "%s",\n', input$baseline_method)
          } else {
            ""
          }

          code_tracker$add_step(
            step_name = "Calculate Heat Pulse Velocity",
            code = sprintf(
              paste0(
                '# Calculate heat pulse velocity\n',
                'vh_results <- sapfluxr::calc_heat_pulse_velocity(\n',
                '  heat_pulse_data = heat_pulse_data,\n',
                '  methods = c(%s),\n',
                '%s',
                '  probe_config = probe_config,\n',
                '  wood_properties = wood_properties\n',
                ')%s'
              ),
              paste0('"', input$methods, '"', collapse = ", "),
              baseline_arg,
              qc_code
            ),
            description = sprintf("Calculated %s for %d measurements with quality control",
                                 paste(input$methods, collapse = ", "),
                                 nrow(results))
          )
        }

        # Show success with auto-close
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Success!",
          text = paste(format(nrow(results), big.mark = ","),
                      "velocity measurements calculated"),
          type = "success",
          timer = 3000
        )

      }, error = function(e) {
        # Show error
        notify_error(
          session = session,
          title = "Calculation Error",
          text = e$message
        )
      })
    })

    # Results summary
    output$results_summary <- renderUI({
      results <- vh_results()
      if (is.null(results)) {
        return(p("No results yet. Click Calculate to run.", style = "color: #999;"))
      }

      # Summary stats
      n_measurements <- nrow(results)
      methods_used <- unique(results$method)
      n_methods <- length(methods_used)

      div(
        style = "background-color: #E8F5E9; padding: 10px; border-radius: 3px;",
        p(strong("Calculation Complete!", style = "color: #4CAF50;")),
        tags$ul(
          tags$li(paste("Total measurements:", format(n_measurements, big.mark = ","))),
          tags$li(paste("Methods calculated:", paste(methods_used, collapse = ", "))),
          tags$li(paste("Date range:",
                       format(min(results$datetime, na.rm = TRUE), "%Y-%m-%d"),
                       "to",
                       format(max(results$datetime, na.rm = TRUE), "%Y-%m-%d")))
        ),
        p(
          icon("arrow-down"),
          " View detailed results in the table below, or proceed to ",
          strong("4. Visualise"),
          " to create plots."
        )
      )
    })

    # Results table
    output$results_table <- DT::renderDataTable({
      results <- vh_results()
      req(results)

      # Format for display — drop internal/deprecated columns
      hrmx_cols <- grep("^hrmxa|^hrmxb", names(results), value = TRUE)
      display_results <- results[, !names(results) %in% hrmx_cols, drop = FALSE]
      display_results$datetime <- format(display_results$datetime, "%Y-%m-%d %H:%M:%S")
      display_results$Vh_cm_hr <- round(display_results$Vh_cm_hr, 2)

      if ("peclet_number" %in% names(display_results)) {
        display_results$peclet_number <- round(display_results$peclet_number, 3)
      }

      DT::datatable(
        display_results,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(0, 'asc'))  # Sort by datetime
        ),
        rownames = FALSE
      )
    })

    # Return results reactive
    return(vh_results)
  })
}
