#' Data Upload Module
#'
#' Shiny module for uploading heat pulse data files
#'
#' @param id Module ID
#' @return Reactive containing heat_pulse_data object
#'

# UI ----
dataUploadUI <- function(id) {
  ns <- NS(id)

  tagList(
    # File upload area
    div(
      class = "file-upload-area",
      fileInput(
        ns("file"),
        label = NULL,
        accept = c(".txt", ".csv", ".json", ".dat"),
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      p(class = "help-text",
        "Supported formats: ICT JSON, CSV, Legacy text files (.txt, .csv, .json, .dat)")
    ),

    # Upload status
    uiOutput(ns("upload_status")),

    # Validation results
    uiOutput(ns("validation_results"))
  )
}

# Server ----
dataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive to store uploaded data
    heat_pulse_data <- reactiveVal(NULL)

    # Observe file upload
    observeEvent(input$file, {
      req(input$file)

      # Clear previous data immediately when new file is selected
      heat_pulse_data(NULL)

      tryCatch({
        # Debug: print file info
        cat("Original filename:", input$file$name, "\n")
        cat("Temp file path:", input$file$datapath, "\n")
        cat("File size:", input$file$size, "bytes\n")

        # Show loading notification
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Loading File...",
          text = "Please wait while your data is being imported and validated.",
          type = "info",
          showConfirmButton = FALSE
        )

        # Read and validate data
        # Suppress progressr to prevent blue toast notifications
        data <- progressr::without_progress({
          sapfluxr::read_heat_pulse_data(input$file$datapath)
        })

        # Override temp filename with original filename
        data$metadata$file_name <- input$file$name

        # Close loading notification
        shinyWidgets::closeSweetAlert(session = session)

        # Debug: check what we got
        cat("Original filename:", input$file$name, "\n")
        cat("Pulse count (n_pulses):", data$metadata$n_pulses, "\n")
        cat("Format:", data$metadata$format, "\n")
        cat("Measurements rows:", nrow(data$measurements), "\n")
        cat("Has gap_detection:", !is.null(data$gap_detection), "\n")
        if (!is.null(data$gap_detection)) {
          cat("Gap detection summary:\n")
          cat("  n_actual:", data$gap_detection$summary$n_actual, "\n")
          cat("  n_missing:", data$gap_detection$summary$n_missing, "\n")
          cat("  n_filled:", data$gap_detection$summary$n_filled, "\n")
          cat("  n_expected:", data$gap_detection$summary$n_expected, "\n")
          cat("  completeness_pct:", data$gap_detection$summary$completeness_pct, "\n")
        }
        cat("==================================\n\n")

        # Store data
        heat_pulse_data(data)

        # Show success message with auto-close
        pulse_msg <- if (!is.null(data$metadata$n_pulses)) {
          paste("Successfully loaded", data$metadata$n_pulses, "pulses")
        } else {
          "File loaded (pulse count: NULL - check console for debug info)"
        }

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "File Loaded!",
          text = pulse_msg,
          type = "success",
          timer = 3000
        )

      }, error = function(e) {
        # Error occurred during file reading
        cat("\n=== ERROR in file reading ===\n")
        cat("Error:", e$message, "\n")
        cat("Call stack:\n")
        print(sys.calls())
        cat("=============================\n\n")

        # Show error message with more detail
        error_text <- paste0(
          "Error: ", e$message, "\n\n",
          "This might be a Unicode/regex issue. ",
          "Please ensure sapfluxr is up to date.\n\n",
          "Try running in R console:\n",
          "devtools::load_all('E:/R/project/sapfluxr')"
        )

        shinyWidgets::sendSweetAlert(
          session = session,
          title = "File Read Error",
          text = error_text,
          type = "error",
          html = TRUE
        )

        heat_pulse_data(NULL)
      })
    })

    # Upload status UI
    output$upload_status <- renderUI({
      req(heat_pulse_data())

      data <- heat_pulse_data()

      # Debug: check reactive data
      cat("Pulse count from reactive:", data$metadata$n_pulses, "\n")
      cat("Is NULL?:", is.null(data$metadata$n_pulses), "\n")
      cat("Class:", class(data$metadata$n_pulses), "\n")
      cat("=====================================\n\n")

      # Handle NULL pulse count gracefully
      pulse_display <- if (!is.null(data$metadata$n_pulses)) {
        format(data$metadata$n_pulses, big.mark = ",")
      } else {
        "NULL (see console debug)"
      }

      tagList(
        hr(),
        h4(icon("check-circle"), "File Loaded Successfully",
           style = "color: #4CAF50;"),

        tags$table(
          style = "width: 100%; margin-top: 10px;",
          tags$tr(
            tags$td(strong("Filename:")),
            tags$td(data$metadata$file_name)
          ),
          tags$tr(
            tags$td(strong("Format:")),
            tags$td(data$metadata$format)
          ),
          tags$tr(
            tags$td(strong("Pulses:")),
            tags$td(pulse_display)
          ),
          tags$tr(
            tags$td(strong("Date Range:")),
            tags$td(get_date_range_text(data$measurements))
          ),
          tags$tr(
            tags$td(strong("Duration:")),
            tags$td(get_duration_text(data$measurements))
          )
        )
      )
    })

    # Validation results UI
    output$validation_results <- renderUI({
      req(heat_pulse_data())

      data <- heat_pulse_data()
      validation <- data$validation

      # Determine validation status from the valid field and presence of issues/warnings
      n_issues <- length(validation$issues)
      n_warnings <- length(validation$warnings)

      # Calculate status
      status <- if (!is.null(validation$valid) && validation$valid) {
        if (n_warnings > 0) {
          "WARNING"
        } else {
          "OK"
        }
      } else {
        "ERROR"
      }

      # Determine status colour and icon
      status_info <- switch(
        status,
        "OK" = list(colour = "#4CAF50", icon = "check-circle", label = "Passed"),
        "WARNING" = list(colour = "#FF9800", icon = "exclamation-triangle", label = "Warnings"),
        "ERROR" = list(colour = "#F44336", icon = "times-circle", label = "Failed"),
        list(colour = "#666", icon = "question-circle", label = status)
      )

      div(
        hr(),
        h4(icon(status_info$icon),
           paste("Validation:", status_info$label),
           style = paste0("color: ", status_info$colour, ";")),

        # Show summary if validation passed or has warnings (not errors)
        if ((status == "OK" || status == "WARNING") && !is.null(validation$summary)) {
          cat("n_expected_pulses:", validation$summary$n_expected_pulses, "\n")
          cat("overall_completeness:", validation$summary$overall_completeness, "\n")
          cat("Names in summary:", paste(names(validation$summary), collapse = ", "), "\n")
          cat("================================\n\n")

          div(
            style = "margin-top: 10px; padding: 10px; background-color: #E8F5E9; border-radius: 3px;",
            p(strong("Data Quality Summary:")),
            tags$ul(
              # Show pulse completeness if available (accounts for missing pulses)
              if (!is.null(validation$summary$pulse_completeness)) {
                tags$li(paste("Pulse Completeness:",
                             round(validation$summary$pulse_completeness * 100, 2), "%",
                             sprintf("(%d of %d expected pulses)",
                                    validation$summary$n_actual_pulses,
                                    validation$summary$n_expected_pulses)))
              } else {
                tags$li(paste("Overall Completeness:",
                             round(validation$summary$overall_completeness * 100, 2), "%"))
              },
              if (!is.null(validation$summary$n_missing_pulses) && validation$summary$n_missing_pulses > 0) {
                tags$li(
                  style = "color: #FF6F00;",
                  paste("Missing Pulses:", validation$summary$n_missing_pulses,
                       "gap(s) detected in pulse sequence")
                )
              },
              tags$li(paste("Sensor Completeness:")),
              tags$ul(
                lapply(names(validation$summary$data_completeness), function(sensor) {
                  tags$li(paste(toupper(sensor), ":",
                               round(validation$summary$data_completeness[sensor] * 100, 2), "%"))
                })
              ),
              tags$li(paste("Total Records:", format(validation$summary$n_measurements, big.mark = ",")))
            )
          )
        },

        # Show warnings if present
        if (n_warnings > 0) {
          div(
            class = "warning-message",
            style = "margin-top: 10px;",
            h5(icon("exclamation-triangle"), "Warnings:"),
            tags$ul(
              lapply(validation$warnings, function(w) {
                tags$li(w)
              })
            )
          )
        },

        # Show issues if present
        if (n_issues > 0) {
          div(
            class = "error-message",
            style = "margin-top: 10px;",
            h5(icon("times-circle"), "Issues:"),
            tags$ul(
              lapply(validation$issues, function(i) {
                tags$li(i)
              })
            )
          )
        }
      )
    })

    # Return reactive containing data
    return(heat_pulse_data)
  })
}
