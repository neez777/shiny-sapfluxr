#' Weather Data Upload Module
#'
#' Shiny module for uploading and processing weather data files with VPD calculation
#'
#' @param id Module ID
#' @param heat_pulse_data Reactive containing heat_pulse_data object for date trimming
#' @return List of reactives: weather_data, weather_vpd, daily_vpd
#'

# UI ----
weatherUploadUI <- function(id) {
  ns <- NS(id)

  tagList(
    # File upload area
    div(
      class = "file-upload-area",
      fileInput(
        ns("file"),
        label = NULL,
        accept = c(".csv", ".txt"),
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      p(class = "help-text",
        "CSV format with datetime, temperature, and relative humidity columns")
    ),

    # Column specification (optional)
    div(
      id = ns("col_spec_section"),
      style = "display: none;",
      h5("Column Specification (Optional)"),
      p(class = "text-muted",
        "Leave blank for automatic detection"),
      fluidRow(
        column(6,
          textInput(ns("datetime_col"), "Datetime Column:",
                   placeholder = "Auto-detect")
        ),
        column(6,
          textInput(ns("temp_col"), "Temperature Column:",
                   placeholder = "Auto-detect")
        )
      ),
      fluidRow(
        column(6,
          textInput(ns("rh_col"), "Humidity Column:",
                   placeholder = "Auto-detect")
        ),
        column(6,
          textInput(ns("pressure_col"), "Pressure Column (optional):",
                   placeholder = "Auto-detect")
        )
      ),
      actionButton(ns("reprocess"), "Reprocess with Custom Columns",
                  icon = icon("sync"))
    ),

    # Upload status
    uiOutput(ns("upload_status")),

    # VPD calculation controls
    conditionalPanel(
      condition = "output.weather_loaded",
      ns = ns,
      wellPanel(
        h5("VPD Calculation"),
        checkboxInput(ns("auto_trim"),
                     "Automatically trim weather data to match heat pulse data dates",
                     value = TRUE),
        conditionalPanel(
          condition = "input.auto_trim == false",
          ns = ns,
          dateRangeInput(ns("date_range"), "Date Range:",
                        start = NULL, end = NULL)
        ),
        actionButton(ns("calc_vpd"), "Calculate VPD",
                    icon = icon("calculator"),
                    class = "btn-primary")
      )
    ),

    # VPD status and summary
    uiOutput(ns("vpd_status"))
  )
}

# Server ----
weatherUploadServer <- function(id, heat_pulse_data = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      weather_raw = NULL,
      weather_vpd = NULL,
      daily_vpd = NULL,
      file_uploaded = FALSE
    )

    # Observe file upload
    observeEvent(input$file, {
      req(input$file)

      # Clear previous data
      rv$weather_raw <- NULL
      rv$weather_vpd <- NULL
      rv$daily_vpd <- NULL
      rv$file_uploaded <- FALSE

      tryCatch({
        # Show loading notification
        showNotification("Loading weather data...",
                        type = "message",
                        duration = NULL,
                        id = "weather_load")

        # Get column specifications if provided
        datetime_col <- if (nzchar(input$datetime_col)) input$datetime_col else NULL
        temp_col <- if (nzchar(input$temp_col)) input$temp_col else NULL
        rh_col <- if (nzchar(input$rh_col)) input$rh_col else NULL
        pressure_col <- if (nzchar(input$pressure_col)) input$pressure_col else NULL

        # Read weather data
        weather <- sapfluxr::read_weather_data(
          input$file$datapath,
          datetime_col = datetime_col,
          temp_col = temp_col,
          rh_col = rh_col,
          pressure_col = pressure_col,
          confirm = FALSE
        )

        # Override filename
        attr(weather, "source_file") <- input$file$name

        # Store data
        rv$weather_raw <- weather
        rv$file_uploaded <- TRUE

        # Remove loading notification
        removeNotification("weather_load")

        # Show success
        showNotification(
          paste("Weather data loaded:", nrow(weather), "records"),
          type = "message",
          duration = 5
        )

        # Show column specification section
        shinyjs::show("col_spec_section")

      }, error = function(e) {
        removeNotification("weather_load")
        showNotification(
          paste("Error loading weather data:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Reprocess with custom columns
    observeEvent(input$reprocess, {
      req(input$file)
      req(rv$file_uploaded)

      tryCatch({
        showNotification("Reprocessing weather data...",
                        type = "message",
                        duration = NULL,
                        id = "weather_reprocess")

        # Get column specifications
        datetime_col <- if (nzchar(input$datetime_col)) input$datetime_col else NULL
        temp_col <- if (nzchar(input$temp_col)) input$temp_col else NULL
        rh_col <- if (nzchar(input$rh_col)) input$rh_col else NULL
        pressure_col <- if (nzchar(input$pressure_col)) input$pressure_col else NULL

        # Read weather data
        weather <- sapfluxr::read_weather_data(
          input$file$datapath,
          datetime_col = datetime_col,
          temp_col = temp_col,
          rh_col = rh_col,
          pressure_col = pressure_col,
          confirm = FALSE
        )

        # Override filename
        attr(weather, "source_file") <- input$file$name

        # Store data
        rv$weather_raw <- weather

        # Remove loading notification
        removeNotification("weather_reprocess")

        # Show success
        showNotification(
          "Weather data reprocessed successfully",
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        removeNotification("weather_reprocess")
        showNotification(
          paste("Error reprocessing:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Calculate VPD
    observeEvent(input$calc_vpd, {
      req(rv$weather_raw)

      tryCatch({
        showNotification("Calculating VPD...",
                        type = "message",
                        duration = NULL,
                        id = "vpd_calc")

        weather <- rv$weather_raw

        # Trim to heat pulse data dates if requested
        if (input$auto_trim && !is.null(heat_pulse_data())) {
          hp_dates <- range(heat_pulse_data()$measurements$datetime)
          weather <- weather %>%
            filter(datetime >= hp_dates[1] & datetime <= hp_dates[2])

          message("Weather data trimmed to match heat pulse data: ",
                 format(hp_dates[1]), " to ", format(hp_dates[2]))
        } else if (!input$auto_trim && !is.null(input$date_range)) {
          # Manual date range
          weather <- weather %>%
            filter(datetime >= input$date_range[1] & datetime <= input$date_range[2])
        }

        # Calculate VPD
        weather_vpd <- sapfluxr::calc_vpd(weather)

        # Calculate daily VPD minima
        daily_vpd <- sapfluxr::calculate_daily_vpd_minima(weather_vpd)

        # Store results
        rv$weather_vpd <- weather_vpd
        rv$daily_vpd <- daily_vpd

        # Remove loading notification
        removeNotification("vpd_calc")

        # Show success
        showNotification(
          paste("VPD calculated successfully:", nrow(weather_vpd), "records,",
               nrow(daily_vpd), "days"),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        removeNotification("vpd_calc")
        showNotification(
          paste("Error calculating VPD:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Upload status output
    output$upload_status <- renderUI({
      if (rv$file_uploaded && !is.null(rv$weather_raw)) {
        weather <- rv$weather_raw

        date_range <- range(weather$datetime)
        n_records <- nrow(weather)

        # Get column mapping
        col_mapping <- attr(weather, "column_mapping")

        # Get validation issues
        validation_issues <- attr(weather, "validation_issues")

        tagList(
          div(class = "alert alert-success",
            icon("check-circle"),
            strong("Weather data loaded successfully"),
            br(),
            sprintf("%s records from %s to %s",
                   format(n_records, big.mark = ","),
                   format(date_range[1]),
                   format(date_range[2]))
          ),

          # Column mapping
          div(class = "well well-sm",
            h5("Column Mapping"),
            tags$ul(
              tags$li(strong("Datetime: "), col_mapping$datetime),
              tags$li(strong("Temperature: "), col_mapping$temperature),
              tags$li(strong("Humidity: "), col_mapping$humidity),
              if (!is.null(col_mapping$pressure))
                tags$li(strong("Pressure: "), col_mapping$pressure)
            )
          ),

          # Validation issues
          if (!is.null(validation_issues) && length(validation_issues) > 0) {
            div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              strong("Validation Warnings:"),
              tags$ul(
                lapply(validation_issues, function(issue) tags$li(issue))
              )
            )
          }
        )
      }
    })

    # VPD status output
    output$vpd_status <- renderUI({
      if (!is.null(rv$weather_vpd) && !is.null(rv$daily_vpd)) {

        vpd_range <- range(rv$weather_vpd$vpd_kpa, na.rm = TRUE)
        daily_min_range <- range(rv$daily_vpd$min_vpd, na.rm = TRUE)

        div(class = "alert alert-info",
          icon("chart-line"),
          strong("VPD Calculated"),
          br(),
          sprintf("Range: %.3f - %.3f kPa", vpd_range[1], vpd_range[2]),
          br(),
          sprintf("Daily minimum range: %.3f - %.3f kPa",
                 daily_min_range[1], daily_min_range[2]),
          br(),
          sprintf("%d days available for VPD-based changepoint detection",
                 nrow(rv$daily_vpd))
        )
      }
    })

    # Weather loaded flag for conditional panel
    output$weather_loaded <- reactive({
      !is.null(rv$weather_raw)
    })
    outputOptions(output, "weather_loaded", suspendWhenHidden = FALSE)

    # Return list of reactives
    return(list(
      weather_data = reactive(rv$weather_raw),
      weather_vpd = reactive(rv$weather_vpd),
      daily_vpd = reactive(rv$daily_vpd)
    ))
  })
}
