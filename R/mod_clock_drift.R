#' Clock Drift Correction Module
#'
#' Shiny module for correcting clock drift in heat pulse data
#' Assumes first pulse time is correct (synced at start)
#'
#' @param id Module ID
#' @param heat_pulse_data Reactive containing heat_pulse_data object
#' @return Reactive containing corrected data (or NULL if no correction applied)
#'

# UI ----
clockDriftUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Collapsible panel using wellPanel
    wellPanel(
      style = "background-color: #f9f9f9; border: 1px solid #ddd;",

      h4(
        icon("clock"),
        "Correct Clock Drift",
        span(
          style = "float: right; font-size: 0.8em; font-style: italic; color: #666;",
          "Optional"
        )
      ),

      p(class = "help-text",
        "Correct clock drift assuming the first pulse time was correct (synced at data collection start)."),

      # Show data range
      uiOutput(ns("data_range_info")),

      hr(),

      fluidRow(
        column(
          width = 6,
          h5("Device Time (at collection end)"),
          p(class = "help-text", "What time did the logger show?"),
          dateInput(
            ns("device_time"),
            "Date:",
            value = NULL
          ),
          textInput(
            ns("device_time_time"),
            "Time (HH:MM:SS):",
            value = "",
            placeholder = "12:00:00"
          )
        ),
        column(
          width = 6,
          h5("Actual Time (at collection end)"),
          p(class = "help-text", "What was the actual time?"),
          dateInput(
            ns("actual_time"),
            "Date:",
            value = Sys.Date()
          ),
          textInput(
            ns("actual_time_time"),
            "Time (HH:MM:SS):",
            value = format(Sys.time(), "%H:%M:%S"),
            placeholder = "12:00:00"
          )
        )
      ),

      hr(),

      # Warning message
      uiOutput(ns("drift_warning")),

      # Apply button
      div(
        style = "text-align: center;",
        actionButton(
          ns("apply_correction"),
          "Apply Clock Drift Correction",
          icon = icon("check"),
          class = "btn-primary"
        )
      ),

      # Correction status
      uiOutput(ns("correction_status"))
    )
  )
}

# Server ----
clockDriftServer <- function(id, heat_pulse_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive to store corrected data
    corrected_data <- reactiveVal(NULL)

    # Reactive for first and last pulse times
    pulse_times <- reactive({
      req(heat_pulse_data())
      data <- heat_pulse_data()

      if (nrow(data$measurements) == 0) {
        return(NULL)
      }

      list(
        first = min(data$measurements$datetime, na.rm = TRUE),
        last = max(data$measurements$datetime, na.rm = TRUE)
      )
    })

    # Update device time inputs when data changes
    observe({
      req(pulse_times())
      times <- pulse_times()

      # Set device time to last pulse time
      updateDateInput(session, "device_time", value = as.Date(times$last))
      updateTextInput(session, "device_time_time", value = format(times$last, "%H:%M:%S"))
    })

    # Display data range info
    output$data_range_info <- renderUI({
      req(pulse_times())
      times <- pulse_times()

      div(
        style = "padding: 10px; background-color: #E3F2FD; border-radius: 3px; margin-top: 10px;",
        p(
          strong("Data Range:"),
          br(),
          "First pulse: ", format(times$first, "%Y-%m-%d %H:%M:%S"),
          " (assumed correct)",
          br(),
          "Last pulse: ", format(times$last, "%Y-%m-%d %H:%M:%S")
        )
      )
    })

    # Check for large time difference and show warning
    output$drift_warning <- renderUI({
      req(input$device_time, input$device_time_time, pulse_times())

      # Parse device time
      tryCatch({
        time_pattern <- "^([0-1]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"
        if (!grepl(time_pattern, input$device_time_time)) {
          return(NULL)
        }

        device_time <- as.POSIXct(paste(input$device_time, input$device_time_time))
        last_pulse <- pulse_times()$last

        # Calculate difference in minutes
        diff_mins <- abs(as.numeric(difftime(device_time, last_pulse, units = "mins")))

        if (diff_mins > 30) {
          div(
            style = "padding: 10px; background-color: #FFF3CD; border-left: 4px solid #FF9800; margin-bottom: 10px;",
            p(
              icon("exclamation-triangle"),
              strong(" Warning:"),
              sprintf(" Large time difference detected (%.1f minutes between last pulse and entered device time).", diff_mins),
              br(),
              "This is unusual. Please verify your times are correct."
            )
          )
        }
      }, error = function(e) NULL)
    })

    # Apply clock drift correction
    observeEvent(input$apply_correction, {
      req(heat_pulse_data(), pulse_times())

      tryCatch({
        # Validate time input
        time_pattern <- "^([0-1]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"

        if (!grepl(time_pattern, input$device_time_time)) {
          stop("Device time must be in HH:MM:SS format")
        }
        if (!grepl(time_pattern, input$actual_time_time)) {
          stop("Actual time must be in HH:MM:SS format")
        }

        # Get first pulse time (assumed correct)
        first_pulse <- pulse_times()$first

        # Parse device and actual times (end of collection)
        device_time_end <- as.POSIXct(paste(input$device_time, input$device_time_time))
        actual_time_end <- as.POSIXct(paste(input$actual_time, input$actual_time_time))

        # Create observed time vectors: first pulse (no drift) + end time (with drift)
        observed_device <- c(first_pulse, device_time_end)
        observed_actual <- c(first_pulse, actual_time_end)

        # Show loading message
        notify_progress(
          title = "Applying Clock Drift Correction...",
          text = "Please wait while timestamps are adjusted."
        )

        # Get original data
        data <- heat_pulse_data()

        # Apply correction to measurements
        data$measurements <- sapfluxr::fix_clock_drift(
          data = data$measurements,
          device_time_col = "datetime",
          observed_device_time = observed_device,
          observed_actual_time = observed_actual
        )

        # Apply correction to diagnostics if present
        if (!is.null(data$diagnostics) && nrow(data$diagnostics) > 0) {
          data$diagnostics <- sapfluxr::fix_clock_drift(
            data = data$diagnostics,
            device_time_col = "datetime",
            observed_device_time = observed_device,
            observed_actual_time = observed_actual
          )
        }

        # Store corrected data
        corrected_data(data)

        # Close loading notification
        close_notify(session)

        # Show success message with auto-close
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Success!",
          text = "Clock drift correction applied successfully",
          type = "success",
          timer = 3000
        )

      }, error = function(e) {
        # Close loading notification
        close_notify(session)

        # Show error message
        notify_error(
          session = session,
          title = "Correction Error",
          text = e$message
        )

        corrected_data(NULL)
      })
    })

    # Correction status UI
    output$correction_status <- renderUI({
      req(corrected_data())

      div(
        style = "margin-top: 15px; padding: 10px; background-color: #E8F5E9; border-left: 4px solid #4CAF50;",
        p(
          icon("check-circle", class = "success-message"),
          strong("Clock drift correction applied"),
          style = "margin: 0; color: #4CAF50;"
        ),
        p(
          class = "help-text",
          style = "margin: 5px 0 0 0;",
          "Timestamps have been corrected. The corrected data will be used for all calculations."
        )
      )
    })

    # Return reactive containing corrected data
    return(corrected_data)
  })
}
