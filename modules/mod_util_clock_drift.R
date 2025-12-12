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
}

# Data Trimming UI (separate function)
dataTrimUI <- function(id) {
  ns <- NS(id)

  tagList(
    p(class = "help-text",
      "Remove first and/or last day if they contain less than 23 hours of data. ",
      "This is recommended for daily aggregation analyses."),

    # Apply button
    div(
      style = "text-align: center; margin-top: 10px;",
      actionButton(
        ns("apply_trimming"),
        "Apply Data Trimming",
        icon = icon("scissors"),
        class = "btn-primary"
      )
    ),

    # Trimming status
    uiOutput(ns("trimming_status"))
  )
}

# Server ----
clockDriftServer <- function(id, heat_pulse_data) {
  moduleServer(id, function(input, output, session) {

    # Reactive to store corrected data (from clock drift)
    corrected_data <- reactiveVal(NULL)

    # Reactive to store trimmed data (after clock drift correction)
    trimmed_data <- reactiveVal(NULL)

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

      # Set actual time to last pulse time as well (more likely to be within 30 mins)
      updateDateInput(session, "actual_time", value = as.Date(times$last))
      updateTextInput(session, "actual_time_time", value = format(times$last, "%H:%M:%S"))
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

    # Placeholder for drift warning (only shown when Apply button is clicked)
    output$drift_warning <- renderUI({
      NULL
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
        last_pulse <- pulse_times()$last

        # Parse device and actual times (end of collection)
        device_time_end <- as.POSIXct(paste(input$device_time, input$device_time_time))
        actual_time_end <- as.POSIXct(paste(input$actual_time, input$actual_time_time))

        # Check for large time difference between device time and last pulse
        diff_mins <- abs(as.numeric(difftime(device_time_end, last_pulse, units = "mins")))

        if (diff_mins > 30) {
          notify_warning(
            session = session,
            title = "Large Time Difference",
            text = sprintf("Warning: %.1f minutes difference between last pulse and entered device time. Please verify your times are correct.", diff_mins),
            timer = 5000
          )
        }

        # Show loading message
        notify_progress(
          title = "Applying Clock Drift Correction...",
          text = "Please wait while timestamps are adjusted."
        )

        # Get original data
        data <- heat_pulse_data()

        # Apply correction to entire heat_pulse_data object
        # fix_clock_drift now handles heat_pulse_data objects directly
        data <- sapfluxr::fix_clock_drift(
          data = data,
          device_time_col = "datetime",
          observed_device_time = device_time_end,
          observed_actual_time = actual_time_end
        )

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

    # Apply data trimming
    observeEvent(input$apply_trimming, {
      # Get the data to trim (use corrected data if available, otherwise original)
      data_to_trim <- if (!is.null(corrected_data())) {
        corrected_data()
      } else {
        req(heat_pulse_data())
        heat_pulse_data()
      }

      tryCatch({
        # Show loading message
        notify_progress(
          title = "Trimming Incomplete Days...",
          text = "Please wait while incomplete days are removed."
        )

        # Apply trimming using sapfluxr function
        trimmed <- sapfluxr:::trim_incomplete_days(
          measurements = data_to_trim$measurements,
          diagnostics = data_to_trim$diagnostics,
          verbose = TRUE
        )

        # Update the heat_pulse_data object with trimmed data
        data_trimmed <- data_to_trim
        data_trimmed$measurements <- trimmed$measurements
        data_trimmed$diagnostics <- trimmed$diagnostics

        # Update metadata to reflect trimming
        data_trimmed$metadata$n_pulses <- nrow(trimmed$diagnostics)

        # Store trimmed data
        trimmed_data(data_trimmed)

        # Close loading notification
        close_notify(session)

        # Calculate how much was trimmed
        n_measurements_removed <- nrow(data_to_trim$measurements) - nrow(trimmed$measurements)
        n_pulses_removed <- nrow(data_to_trim$diagnostics) - nrow(trimmed$diagnostics)

        # Show success message
        trim_msg <- if (n_measurements_removed > 0) {
          sprintf("Trimmed %s measurements (%s pulses) from incomplete days",
                  format(n_measurements_removed, big.mark = ","),
                  format(n_pulses_removed, big.mark = ","))
        } else {
          "No trimming needed - first and last days are complete (≥ 23 hours)"
        }

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Data Trimming Complete!",
          text = trim_msg,
          type = "success",
          timer = 4000
        )

      }, error = function(e) {
        # Close loading notification
        close_notify(session)

        # Show error message
        notify_error(
          session = session,
          title = "Trimming Error",
          text = e$message
        )

        trimmed_data(NULL)
      })
    })

    # Trimming status UI
    output$trimming_status <- renderUI({
      req(trimmed_data())

      # Calculate trimming stats
      data_before <- if (!is.null(corrected_data())) corrected_data() else heat_pulse_data()
      data_after <- trimmed_data()

      n_measurements_removed <- nrow(data_before$measurements) - nrow(data_after$measurements)

      div(
        style = "margin-top: 15px; padding: 10px; background-color: #E8F5E9; border-left: 4px solid #4CAF50;",
        p(
          icon("check-circle", class = "success-message"),
          strong("Data trimming applied"),
          style = "margin: 0; color: #4CAF50;"
        ),
        p(
          class = "help-text",
          style = "margin: 5px 0 0 0;",
          if (n_measurements_removed > 0) {
            sprintf("Removed %s measurements from incomplete days. Trimmed data will be used for all calculations.",
                    format(n_measurements_removed, big.mark = ","))
          } else {
            "No measurements removed - first and last days were complete."
          }
        )
      )
    })

    # Return reactive containing final processed data
    # Priority: trimmed_data > corrected_data > NULL
    final_data <- reactive({
      if (!is.null(trimmed_data())) {
        trimmed_data()
      } else if (!is.null(corrected_data())) {
        corrected_data()
      } else {
        NULL
      }
    })

    return(final_data)
  })
}
