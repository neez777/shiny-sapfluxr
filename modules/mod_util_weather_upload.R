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
        "CSV format with datetime, temperature, and relative humidity columns"),
      tags$div(
        class = "demo-only",
        style = "margin-top: 10px;",
        tags$span(class = "help-text", "No data of your own? "),
        actionButton(
          ns("load_example"),
          label = "Load Example Weather",
          icon = icon("flask"),
          class = "btn-info btn-sm"
        )
      )
    ),

    # Column specification (optional)
    div(
      id = ns("col_spec_section"),
      style = "display: none;",
      h5("Column Specification (Optional)"),
      p(class = "text-muted",
        "Select the correct columns from the dropdowns below."),
      fluidRow(
        column(6,
          selectInput(ns("datetime_col"), "Datetime Column:",
                   choices = character(0))
        ),
        column(6,
          selectInput(ns("temp_col"), "Temperature Column:",
                   choices = character(0))
        )
      ),
      fluidRow(
        column(6,
          selectInput(ns("rh_col"), "Humidity Column:",
                   choices = character(0))
        ),
        column(6,
          selectInput(ns("pressure_col"), "Pressure Column (optional):",
                   choices = character(0))
        )
      ),
      actionButton(ns("reprocess"), "Update Column Mapping",
                  icon = icon("sync"))
    ),

    # Upload status
    uiOutput(ns("upload_status")),

    # VPD options (VPD is calculated automatically on upload)
    conditionalPanel(
      condition = "output.weather_loaded",
      ns = ns,
      wellPanel(
        h5("VPD Options"),
        p(class = "help-text",
          icon("info-circle"),
          " Vapour pressure deficit is calculated automatically when weather data is loaded."),
        checkboxInput(ns("auto_trim"),
                     "Automatically trim weather data to match heat pulse data dates",
                     value = TRUE),
        conditionalPanel(
          condition = "input.auto_trim == false",
          ns = ns,
          dateRangeInput(ns("date_range"), "Date Range:",
                        start = NULL, end = NULL)
        )
      )
    ),

    # VPD status and summary
    uiOutput(ns("vpd_status"))
  )
}

# Server ----
weatherUploadServer <- function(id, heat_pulse_data = reactive(NULL),
                                code_tracker = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Quote a column name for code generation, or "NULL" when unset (auto-detected).
    quote_col <- function(x) {
      if (is.null(x) || !nzchar(x)) "NULL" else sprintf('"%s"', x)
    }

    # Reactive values
    rv <- reactiveValues(
      weather_raw = NULL,
      weather_vpd = NULL,
      daily_vpd = NULL,
      file_uploaded = FALSE
    )

    # Shared weather import routine used by both the upload and example loader.
    # code_path_expr is the R expression (as text) used to refer to the source file
    # in the generated reproducibility script.
    load_weather <- function(path, display_name, code_path_expr) {

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

        # Get column specifications if provided (Handle selectInputs)
        # On first load these will be empty, so auto-detect runs.
        # On 'Update Mapping' click, they will have values.
        datetime_col <- if (nzchar(input$datetime_col %||% "")) input$datetime_col else NULL
        temp_col <- if (nzchar(input$temp_col %||% "")) input$temp_col else NULL
        rh_col <- if (nzchar(input$rh_col %||% "")) input$rh_col else NULL
        pressure_col <- if (nzchar(input$pressure_col %||% "")) input$pressure_col else NULL

        # Read weather data
        weather <- sapfluxr::read_weather_data(
          path,
          datetime_col = datetime_col,
          temp_col = temp_col,
          rh_col = rh_col,
          pressure_col = pressure_col,
          confirm = FALSE
        )

        # Record source name
        attr(weather, "source_file") <- display_name

        # Store data
        rv$weather_raw <- weather
        rv$file_uploaded <- TRUE

        # Track code generation — record the file path and column mapping used
        if (!isTRUE(code_tracker)) {
          mapping <- attr(weather, "column_mapping")
          code_tracker$add_step(
            step_name = "Upload Weather Data",
            code = sprintf(
              paste0(
                "# Load weather data (temperature + relative humidity)\n",
                "weather <- sapfluxr::read_weather_data(\n",
                "  path = %s,\n",
                "  datetime_col = %s,\n",
                "  temp_col = %s,\n",
                "  rh_col = %s,\n",
                "  pressure_col = %s,\n",
                "  confirm = FALSE\n)"
              ),
              code_path_expr,
              quote_col(mapping$datetime),
              quote_col(mapping$temperature),
              quote_col(mapping$humidity),
              quote_col(mapping$pressure)
            ),
            description = sprintf("Imported %s (%d records)",
                                  display_name, nrow(weather))
          )
        }

        # Populate Dropdowns with RAW column names from the file
        # We get these from the attributes of the successfully read weather object
        col_mapping <- attr(weather, "column_mapping")
        raw_col_names <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE))

        updateSelectInput(session, "datetime_col", choices = raw_col_names, selected = col_mapping$datetime)
        updateSelectInput(session, "temp_col", choices = raw_col_names, selected = col_mapping$temperature)
        updateSelectInput(session, "rh_col", choices = raw_col_names, selected = col_mapping$humidity)
        updateSelectInput(session, "pressure_col",
                          choices = c("None" = "", raw_col_names),
                          selected = col_mapping$pressure %||% "")

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

        # Automatically calculate VPD (and daily minima) for the freshly loaded data
        compute_vpd()

      }, error = function(e) {
        removeNotification("weather_load")
        showNotification(
          paste("Error loading weather data:", e$message),
          type = "error",
          duration = 10
        )
      })
    }

    # Compute VPD and daily VPD minima from the currently loaded weather data,
    # applying any requested date trimming. Runs automatically on upload and
    # whenever the trim controls or the heat pulse data change, so the user
    # never has to press a button.
    compute_vpd <- function() {
      weather <- rv$weather_raw
      if (is.null(weather)) return(invisible(NULL))

      tryCatch({
        # Trim to heat pulse data dates if requested and available
        trim_code <- ""
        if (isTRUE(input$auto_trim) && !is.null(heat_pulse_data())) {
          hp_dates <- range(heat_pulse_data()$measurements$datetime)
          weather <- weather %>%
            filter(datetime >= hp_dates[1] & datetime <= hp_dates[2])
          trim_code <- paste0(
            "# Trim weather to the heat pulse data date range\n",
            "hp_dates <- range(heat_pulse_data$measurements$datetime)\n",
            "weather <- dplyr::filter(weather, ",
            "datetime >= hp_dates[1], datetime <= hp_dates[2])\n\n"
          )
        } else if (!isTRUE(input$auto_trim) && !is.null(input$date_range)) {
          weather <- weather %>%
            filter(datetime >= input$date_range[1] & datetime <= input$date_range[2])
          trim_code <- sprintf(
            paste0(
              "# Trim weather to the chosen date range\n",
              "weather <- dplyr::filter(weather, ",
              "datetime >= as.POSIXct(\"%s\"), datetime <= as.POSIXct(\"%s\"))\n\n"
            ),
            input$date_range[1], input$date_range[2]
          )
        }

        # Calculate VPD and daily VPD minima
        weather_vpd <- sapfluxr::calc_vpd(weather)
        daily_vpd <- sapfluxr::calculate_daily_vpd_minima(weather_vpd)

        rv$weather_vpd <- weather_vpd
        rv$daily_vpd <- daily_vpd

        # Track code generation as its own upsertable step
        if (!isTRUE(code_tracker)) {
          code_tracker$add_step(
            step_name = "Calculate VPD",
            code = paste0(
              trim_code,
              "# Calculate vapour pressure deficit and daily minima\n",
              "weather_vpd <- sapfluxr::calc_vpd(weather)\n",
              "daily_vpd  <- sapfluxr::calculate_daily_vpd_minima(weather_vpd)"
            ),
            description = sprintf("Calculated VPD (%d records, %d days)",
                                  nrow(weather_vpd), nrow(daily_vpd))
          )
        }

      }, error = function(e) {
        showNotification(
          paste("Error calculating VPD:", e$message),
          type = "error",
          duration = 10
        )
      })
    }

    # Observe file upload
    observeEvent(input$file, {
      req(input$file)
      load_weather(
        input$file$datapath, input$file$name,
        code_path_expr = sprintf('"%s"', input$file$name)
      )
    })

    # Observe "Load Example Weather" button
    observeEvent(input$load_example, {
      example_path <- system.file(
        "extdata", "Sample_Meteorological_Data.txt", package = "sapfluxr"
      )
      if (!nzchar(example_path)) {
        showNotification(
          "Bundled example weather file not found. Is sapfluxr installed?",
          type = "error", duration = 10
        )
        return()
      }
      load_weather(
        example_path, "Sample_Meteorological_Data.txt",
        code_path_expr =
          'system.file("extdata", "Sample_Meteorological_Data.txt", package = "sapfluxr")'
      )
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

        # Recompute VPD for the reprocessed data
        compute_vpd()

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

    # Recompute VPD automatically when the trim options change
    observeEvent(list(input$auto_trim, input$date_range), {
      if (!is.null(rv$weather_raw)) compute_vpd()
    }, ignoreInit = TRUE)

    # Recompute VPD when heat pulse data loads or changes (auto-trim depends on it)
    observeEvent(heat_pulse_data(), {
      if (!is.null(rv$weather_raw) && isTRUE(input$auto_trim)) compute_vpd()
    }, ignoreInit = TRUE)

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
