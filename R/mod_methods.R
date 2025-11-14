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

          checkboxGroupInput(
            ns("methods"),
            NULL,
            choices = c(
              "HRM - Heat Ratio Method (low/reverse flows)" = "HRM",
              "MHR - Maximum Heat Ratio (moderate to high flows)" = "MHR",
              "HRMXa - Modified HRM variant A" = "HRMXa",
              "HRMXb - Modified HRM variant B" = "HRMXb",
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

          # Missing Pulses & Gaps
          div(
            style = "background-color: #f9f9f9; padding: 10px; margin-bottom: 10px; border-radius: 3px;",
            h5("Missing Pulses & Gaps", style = "margin-top: 0;"),
            fluidRow(
              column(
                width = 5,
                checkboxInput(
                  ns("qc_detect_missing_pulses"),
                  HTML('Detect missing pulses <span style="color: #999; cursor: help;" title="Identifies gaps in the pulse sequence where expected pulses are missing from the dataset. Missing pulses can indicate logger errors, power outages, or data transmission issues."><i class="fa fa-circle-question"></i></span>'),
                  value = TRUE
                ),
                checkboxInput(
                  ns("qc_add_rows_for_missing"),
                  HTML('Add rows for missing data <span style="color: #999; cursor: help;" title="Inserts placeholder rows with NA values for detected missing pulses to maintain time sequence continuity. This ensures consistent time intervals in plots and analyses."><i class="fa fa-circle-question"></i></span>'),
                  value = TRUE
                )
              ),
              column(
                width = 5,
                numericInput(
                  ns("qc_max_gap_to_fill_hours"),
                  HTML('Max gap to fill (hours): <span style="color: #999; cursor: help;" title="Only fills gaps shorter than this duration. Longer gaps are left empty to avoid interpolating across major data outages like power failures or maintenance periods."><i class="fa fa-circle-question"></i></span>'),
                  value = 1,
                  min = 1,
                  max = 168,
                  step = 1
                )
              )
            )
          ),

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
                  ns("qc_detect_outliers"),
                  HTML('Detect outliers (rolling mean) <span style="color: #999; cursor: help;" title="Uses a rolling window to calculate local mean and standard deviation, then flags values that deviate too far from the local trend. Effective for catching isolated spikes or anomalies that don\'t fit the surrounding pattern."><i class="fa fa-circle-question"></i></span>'),
                  value = TRUE
                ),
                checkboxInput(
                  ns("qc_detect_rate_of_change"),
                  HTML('Detect rate of change outliers <span style="color: #999; cursor: help;" title="Flags sudden jumps between consecutive measurements that exceed biologically plausible rates of change. Trees cannot instantly increase or decrease sap flow beyond certain physiological limits."><i class="fa fa-circle-question"></i></span>'),
                  value = TRUE
                ),
                checkboxInput(
                  ns("qc_check_cross_sensor"),
                  HTML('Check cross-sensor anomalies <span style="color: #999; cursor: help;" title="Detects if one thermistor sensor is behaving differently from others at the same timestamp. At each time point, sensors showing values significantly different from the median across all sensors (>3x SD) are flagged as SUSPECT."><i class="fa fa-circle-question"></i></span>'),
                  value = FALSE
                )
              ),
              column(
                width = 5,
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
                ),
                numericInput(
                  ns("qc_max_change_cm_hr"),
                  HTML('Max velocity change (cm/hr): <span style="color: #999; cursor: help;" title="Maximum allowed velocity change between consecutive measurements. Larger jumps are flagged as suspect. Should reflect realistic biological rates of change for your species and measurement interval."><i class="fa fa-circle-question"></i></span>'),
                  value = 4,
                  min = 0.1,
                  max = 50,
                  step = 0.5
                ),
                numericInput(
                  ns("qc_cross_sensor_threshold"),
                  HTML('Cross-sensor threshold (SD): <span style="color: #999; cursor: help;" title="How many standard deviations from the median sensor value before flagging as anomalous. Higher values are more lenient. If one sensor consistently reads very differently from the others, it may indicate a faulty probe."><i class="fa fa-circle-question"></i></span>'),
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
}

# Server ----
methodsServer <- function(id, heat_pulse_data, probe_config, wood_properties) {
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
          results <- tryCatch({
            sapfluxr::flag_vh_quality(
              results,
              detect_missing_pulses = input$qc_detect_missing_pulses,
              check_illogical = input$qc_check_illogical,
              hard_max_vh = input$qc_hard_max_vh,
              flag_negative = input$qc_flag_negative,
              detect_outliers = input$qc_detect_outliers,
              rolling_window = input$qc_rolling_window,
              rolling_threshold = input$qc_rolling_threshold,
              detect_rate_of_change = input$qc_detect_rate_of_change,
              max_change_cm_hr = input$qc_max_change_cm_hr,
              check_cross_sensor = input$qc_check_cross_sensor,
              cross_sensor_threshold = input$qc_cross_sensor_threshold,
              add_rows_for_missing = input$qc_add_rows_for_missing,
              max_gap_to_fill_hours = input$qc_max_gap_to_fill_hours,
              verbose = FALSE,
              return_full_report = FALSE
            )
          }, error = function(e) {
            message("Quality control failed: ", e$message)
            results
          })
        })

        # Store results
        vh_results(results)

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

      # Format for display
      display_results <- results
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
