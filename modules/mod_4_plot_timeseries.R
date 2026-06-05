#' Time Series Visualisation Module
#'
#' Shiny module for interactive plotly time series of HPV results
#'
#' @param id Module ID
#' @param vh_results Reactive containing vh_results tibble
#' @return None (displays plot)
#'

# UI ----
plotTimeseriesUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Plot Controls
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
          uiOutput(ns("method_checkboxes")),

          hr(),

          h5("Sensor Position"),
          helpText("Tip: Select only one sensor for faster plot rendering with large datasets"),
          checkboxGroupInput(
            ns("sensor_position"),
            NULL,
            choices = c("Inner" = "inner", "Outer" = "outer"),
            selected = "outer"  # Default to outer sensor only for performance
          ),

          hr(),

          h5("Quality Flags to Display"),
          uiOutput(ns("flag_checkboxes")),

          hr(),

          h5("Time Range"),
          p(class = "help-text", style = "font-size: 0.9em; color: #666;",
            "Set the date/time range to display. Updates automatically when you use the range slider."),

          shinyWidgets::airDatepickerInput(
            ns("start_datetime"),
            "Start Date/Time:",
            value = NULL,
            timepicker = TRUE,
            dateFormat = "yyyy-mm-dd",
            timepickerOpts = shinyWidgets::timepickerOptions(timeFormat = "HH:mm")
          ),

          shinyWidgets::airDatepickerInput(
            ns("end_datetime"),
            "End Date/Time:",
            value = NULL,
            timepicker = TRUE,
            dateFormat = "yyyy-mm-dd",
            timepickerOpts = shinyWidgets::timepickerOptions(timeFormat = "HH:mm")
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
            ns("show_quality_flags"),
            "Show quality flag markers",
            value = FALSE
          ),

          checkboxInput(
            ns("show_points"),
            "Show data points",
            value = FALSE
          ),

          checkboxInput(
            ns("show_interpolated"),
            "Highlight interpolated points",
            value = FALSE
          ),

          checkboxInput(
            ns("show_vpd"),
            "Show VPD (right axis)",
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
        ),

        box(
          width = NULL,
          title = "Data Cleaning & Filtering",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          h5("Interpolation Settings"),

          selectInput(
            ns("interp_method"),
            "Interpolation method:",
            choices = c(
              "Linear" = "linear",
              "Moving Average" = "moving_average"
            ),
            selected = "linear"
          ),

          conditionalPanel(
            condition = "input.interp_method == 'linear'",
            ns = ns,
            p(style = "font-size: 0.9em; color: #666; margin-bottom: 10px;",
              "Creates a straight line between valid points. For single missing points, this is the simple average of before/after values.")
          ),

          conditionalPanel(
            condition = "input.interp_method == 'moving_average'",
            ns = ns,
            p(style = "font-size: 0.9em; color: #666; margin-bottom: 10px;",
              "Uses weighted average of surrounding valid points. Window size is automatically determined based on gap size. Good for noisy data.")
          ),

          numericInput(
            ns("max_gap_hours"),
            "Max gap to fill (hours):",
            value = 1,
            min = 0.5,
            max = 24,
            step = 0.5
          ),

          conditionalPanel(
            condition = "input.max_gap_hours > 3",
            ns = ns,
            div(
              style = "color: #ff9800; margin-bottom: 10px; padding: 5px; background-color: #fff3e0; border-radius: 3px;",
              icon("exclamation-triangle"),
              " Warning: Gaps > 3 hours are not recommended"
            )
          ),

          hr(),

          h5("Quality Flags to Interpolate"),
          checkboxGroupInput(
            ns("flags_to_interpolate"),
            NULL,
            choices = c(
              "Missing data" = "DATA_MISSING",
              "Statistical outliers" = "DATA_OUTLIER",
              "Illogical values" = "DATA_ILLOGICAL"
            ),
            selected = c("DATA_MISSING", "DATA_OUTLIER", "DATA_ILLOGICAL")
          ),

          hr(),

          h5("Filter by Method/Sensor (optional)"),
          p(class = "help-text", style = "font-size: 0.85em; color: #666;",
            "Leave all unchecked to apply to all methods/sensors"),

          uiOutput(ns("methods_to_clean_checkboxes")),

          checkboxGroupInput(
            ns("sensors_to_clean"),
            "Sensors:",
            choices = c("Inner" = "inner", "Outer" = "outer"),
            selected = c()
          ),

          hr()

          # ==================================================================
          # sDMA HANDLING - REMOVED
          # ==================================================================
          # REMOVED: h5("sDMA Handling")
          # REMOVED: radioButtons handle_multi_method
          #
          # sDMA will be re-implemented in a later workflow stage.
          # See R/04j_sdma_methods.R for preserved implementation.
          # ==================================================================

          ,hr(),

          actionButton(
            ns("preview_cleaning"),
            "Preview Changes",
            icon = icon("search"),
            class = "btn-info",
            style = "width: 100%; margin-bottom: 5px;"
          ),

          actionButton(
            ns("apply_cleaning"),
            "Apply Cleaning",
            icon = icon("broom"),
            class = "btn-warning",
            style = "width: 100%; margin-bottom: 10px;"
          ),

          uiOutput(ns("cleaning_summary_ui"))
        ),

      ),

      # Time Series Plot
      column(
        width = 9,
        box(
          width = NULL,
          title = "Heat Pulse Velocity Time Series",
          status = "success",
          solidHeader = TRUE,

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("timeseries_plot"), height = "600px"),
            type = 6,
            color = "#3c8dbc"
          )
        )
,

        box(
          width = NULL,
          title = "Plot Information & Quality Control",
          status = "info",
          collapsible = TRUE,
          collapsed = FALSE,
          solidHeader = TRUE,

          fluidRow(
            column(
              width = 6,
              h5("Plot Information"),
              uiOutput(ns("plot_info"))
            ),
            column(
              width = 6,
              h5("Quality Control Summary"),
              uiOutput(ns("qc_summary"))
            )
          )
        )
      )
    )
  )
}

# Server ----
plotTimeseriesServer <- function(id, vh_results, daily_vpd = reactive(NULL), weather_vpd = reactive(NULL), plot_settings = reactive(NULL), rv = NULL) {
  moduleServer(id, function(input, output, session) {

    # Store current axis ranges
    current_xrange <- reactiveVal(NULL)
    current_yrange <- reactiveVal(NULL)

    # Store selected pulse ID from click
    selected_pulse_id <- reactiveVal(NULL)

    # Initialize date/time range inputs with full data range
    observe({
      req(vh_results())
      data <- vh_results()

      # Get full date range from data
      date_range <- range(data$datetime, na.rm = TRUE)

      # Update date/time inputs (only if not already set)
      if (is.null(input$start_datetime)) {
        shinyWidgets::updateAirDateInput(
          session = session,
          inputId = "start_datetime",
          value = date_range[1]
        )
      }

      if (is.null(input$end_datetime)) {
        shinyWidgets::updateAirDateInput(
          session = session,
          inputId = "end_datetime",
          value = date_range[2]
        )
      }
    })

    # Available methods from data
    available_methods <- reactive({
      unique(vh_results()$method)
    })

    # Dynamic method checkboxes
    output$method_checkboxes <- renderUI({
      methods <- available_methods()
      req(length(methods) > 0)

      # Default only first method checked (to avoid long loading times with large datasets)
      checkboxGroupInput(
        session$ns("methods"),
        NULL,
        choices = setNames(methods, methods),
        selected = methods[1]
      )
    })

    # Available quality flags from data
    available_flags <- reactive({
      req(vh_results())
      if (!"quality_flag" %in% names(vh_results())) {
        return(c("OK"))
      }
      unique(vh_results()$quality_flag)
    })

    # Dynamic quality flag checkboxes
    output$flag_checkboxes <- renderUI({
      flags <- available_flags()
      req(length(flags) > 0)

      # Create friendly labels and descriptions for flags
      flag_info <- list(
        "OK" = list(
          label = "OK (No issues)",
          desc = "Data passed all quality checks with no issues detected"
        ),
        "DATA_OUTLIER" = list(
          label = "Outliers",
          desc = "Statistical outlier detected by rolling mean (deviates >3 SD from local mean) or rate of change (>4 cm/hr change between consecutive measurements)"
        ),
        "DATA_SUSPECT" = list(
          label = "Suspect",
          desc = "Negative flow detected or sensor shows values significantly different from other sensors at the same timestamp (cross-sensor anomaly)"
        ),
        "DATA_MISSING" = list(
          label = "Missing",
          desc = "No pulse recorded at expected timestamp, indicating hardware/logging gap"
        ),
        "DATA_ILLOGICAL" = list(
          label = "Illogical",
          desc = "Exceeds maximum velocity threshold (absolute max or species-specific max)"
        ),
        "CALC_FAILED" = list(
          label = "Calc Failed",
          desc = "Calculation returned NA (e.g., Tmax couldn't find temperature peak)"
        ),
        "CALC_INFINITE" = list(
          label = "Calc Infinite",
          desc = "Calculation returned Inf (division by zero or mathematical error)"
        ),
        "CALC_EXTREME" = list(
          label = "Calc Extreme",
          desc = "Result outside physically realistic range (|Vh| > 200 or Vh < -50 cm/hr)"
        )
      )

      # Create checkbox inputs with tooltips
      checkbox_list <- lapply(flags, function(flag) {
        info <- flag_info[[flag]]
        label_text <- if (!is.null(info)) info$label else flag
        desc_text <- if (!is.null(info)) info$desc else ""

        div(
          style = "margin-bottom: 5px;",
          checkboxInput(
            session$ns(paste0("qflag_", flag)),
            HTML(paste0(
              label_text,
              if (desc_text != "") {
                paste0(' <span style="color: #999; cursor: help;" title="', desc_text,
                       '"><i class="fa fa-circle-question"></i></span>')
              } else {
                ""
              }
            )),
            value = TRUE
          )
        )
      })

      # Combine checkboxes and observe changes
      tagList(
        checkbox_list,
        # Hidden reactive to aggregate selections
        tags$script(HTML(sprintf("
          $(document).ready(function() {
            $('input[id^=\"%s\"]').on('change', function() {
              var selected = [];
              $('input[id^=\"%s\"]:checked').each(function() {
                var flag = $(this).attr('id').replace('%s', '').replace('qflag_', '');
                selected.push(flag);
              });
              Shiny.setInputValue('%s', selected);
            });
            // Trigger initial value
            $('input[id^=\"%s\"]').first().trigger('change');
          });
        ", session$ns("qflag_"), session$ns("qflag_"), session$ns(""),
           session$ns("quality_flags"), session$ns("qflag_"))))
      )
    })

    # Flag colors (consistent with scientific QC standards)
    flag_colors <- c(
      "OK" = "#2ca02c",              # Green
      "DATA_SUSPECT" = "#ff7f0e",    # Orange
      "DATA_OUTLIER" = "#d62728",    # Red
      "DATA_MISSING" = "#7f7f7f",    # Gray
      "DATA_ILLOGICAL" = "#8b0000",  # Dark red
      "CALC_FAILED" = "#9467bd",     # Purple
      "CALC_INFINITE" = "#e377c2",   # Pink
      "CALC_EXTREME" = "#bcbd22",    # Yellow-green
      "INTERPOLATED" = "#2ca02c",    # Green (for interpolated points)
      "LARGE_GAP" = "#666666"        # Dark gray
    )

    # === Data Cleaning Logic ===

    # Reactive to store cleaned data
    cleaned_data <- reactiveVal(NULL)
    cleaning_preview <- reactiveVal(NULL)

    # Track if cleaning has been applied
    cleaning_applied <- reactive({
      !is.null(cleaned_data())
    })

    # Output for conditional panel
    output$cleaning_applied <- reactive({
      cleaning_applied()
    })
    outputOptions(output, "cleaning_applied", suspendWhenHidden = FALSE)

    # Dynamic method checkboxes for cleaning filter
    output$methods_to_clean_checkboxes <- renderUI({
      methods <- available_methods()
      req(length(methods) > 0)

      checkboxGroupInput(
        session$ns("methods_to_clean"),
        "Methods:",
        choices = setNames(methods, methods),
        selected = c()  # Empty = apply to all
      )
    })

    # Choose data source: cleaned (if applied) or original
    data_for_plot <- reactive({
      if (cleaning_applied()) cleaned_data() else vh_results()
    })

    # Filtered data based on selections
    filtered_data <- reactive({
      req(data_for_plot())
      req(input$methods)
      req(input$sensor_position)

      data <- data_for_plot()

      # Filter by method
      data <- data %>%
        dplyr::filter(method %in% input$methods)

      # Filter by sensor position
      data <- data %>%
        dplyr::filter(sensor_position %in% input$sensor_position)

      # Filter by quality flags (if quality_flags input exists and has selections)
      if (!is.null(input$quality_flags) && length(input$quality_flags) > 0) {
        if ("quality_flag" %in% names(data)) {
          data <- data %>%
            dplyr::filter(quality_flag %in% input$quality_flags)
        }
      }

      data
    })

    # Plot information summary
    output$plot_info <- renderUI({
      data <- filtered_data()
      req(nrow(data) > 0)

      n_points <- nrow(data)
      date_range <- range(data$datetime, na.rm = TRUE)
      methods <- unique(data$method)
      positions <- unique(data$sensor_position)

      # Quality flag summary
      flag_summary <- data %>%
        count(quality_flag) %>%
        dplyr::arrange(desc(n))

      tagList(
        p(strong("Displayed Data:")),
        tags$ul(
          tags$li(paste("Points:", format(n_points, big.mark = ","))),
          tags$li(paste("Methods:", paste(methods, collapse = ", "))),
          tags$li(paste("Positions:", paste(positions, collapse = ", "))),
          tags$li(paste("Date range:",
                       format(date_range[1], "%Y-%m-%d %H:%M"),
                       "to",
                       format(date_range[2], "%Y-%m-%d %H:%M")))
        ),

        if (nrow(flag_summary) > 0) {
          tagList(
            p(strong("Quality Flags:")),
            tags$ul(
              lapply(seq_len(nrow(flag_summary)), function(i) {
                tags$li(paste(flag_summary$quality_flag[i], ":",
                             format(flag_summary$n[i], big.mark = ",")))
              })
            )
          )
        }
      )
    })

    # Quality Control Summary
    output$qc_summary <- renderUI({
      req(vh_results())

      # Use filtered data so summary reflects current method/sensor selection
      data <- filtered_data()
      req(nrow(data) > 0)

      # Check if quality_flag column exists
      if (!"quality_flag" %in% names(data)) {
        return(p("No quality control data available."))
      }

      # Calculate flag counts based on filtered data (selected methods/sensors)
      flag_counts <- data %>%
        count(quality_flag) %>%
        dplyr::arrange(quality_flag)

      total_points <- sum(flag_counts$n)

      # Create colored list items
      flag_items <- lapply(seq_len(nrow(flag_counts)), function(i) {
        flag <- flag_counts$quality_flag[i]
        count <- flag_counts$n[i]
        pct <- round(count / total_points * 100, 1)

        # Get color for this flag
        color <- if (flag %in% names(flag_colors)) {
          flag_colors[[flag]]
        } else {
          "#333333"  # Default gray
        }

        # Create friendly label
        flag_label <- switch(flag,
          "OK" = "OK (No issues)",
          "DATA_OUTLIER" = "Outliers",
          "DATA_SUSPECT" = "Suspect",
          "DATA_MISSING" = "Missing",
          "DATA_ILLOGICAL" = "Illogical",
          "CALC_FAILED" = "Calc Failed",
          "CALC_INFINITE" = "Calc Infinite",
          "CALC_EXTREME" = "Calc Extreme",
          flag  # Default to flag name
        )

        tags$li(
          style = paste0("color: ", color, "; font-weight: bold;"),
          paste0(flag_label, ": ", format(count, big.mark = ","),
                 " (", pct, "%)")
        )
      })

      tagList(
        p(strong(paste("Filtered Data:", format(total_points, big.mark = ","), "measurements"))),
        tags$ul(style = "list-style-type: none; padding-left: 10px;",
          flag_items
        ),
        p(style = "font-size: 0.85em; color: #666; margin-top: 10px;",
          "Summary reflects currently selected methods and sensor positions. ",
          "Use the 'Quality Flags to Display' filter to show/hide specific flags on the plot.")
      )
    })

    # Preview cleaning changes
    observeEvent(input$preview_cleaning, {
      req(vh_results())

      # Filter data if method/sensor selections made
      data_to_clean <- vh_results()

      if (length(input$methods_to_clean) > 0) {
        data_to_clean <- data_to_clean %>%
          dplyr::filter(method %in% input$methods_to_clean)
      }

      if (length(input$sensors_to_clean) > 0) {
        data_to_clean <- data_to_clean %>%
          dplyr::filter(sensor_position %in% input$sensors_to_clean)
      }

      # Preview changes
      tryCatch({
        preview <- sapfluxr::preview_interpolation_changes(
          data_to_clean,
          flags_to_interpolate = input$flags_to_interpolate,
          max_gap_hours = input$max_gap_hours,
          group_by_method = TRUE,
          group_by_sensor = TRUE
        )

        cleaning_preview(preview)

        # Show preview summary
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Interpolation Preview",
          text = HTML(sprintf("
            <p><strong>Summary:</strong></p>
            <ul>
              <li>Total rows: %s</li>
              <li>Will interpolate: %s values</li>
              <li>Large gaps (not filled): %s values</li>
            </ul>
            <p style='margin-top: 10px;'>Review the Cleaning Summary below for details.</p>
          ",
          format(preview$n_total, big.mark = ","),
          format(preview$n_interpolated, big.mark = ","),
          format(preview$n_large_gaps, big.mark = ",")
          )),
          type = "info",
          html = TRUE
        )

      }, error = function(e) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Preview Error",
          text = paste("Error previewing changes:", e$message),
          type = "error"
        )
      })
    })

    # Apply cleaning
    observeEvent(input$apply_cleaning, {
      req(vh_results())

      # Show loading message
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Applying Cleaning...",
        text = "Please wait while data is being cleaned and interpolated.",
        type = "info",
        showConfirmButton = FALSE
      )

      tryCatch({
        # IMPORTANT: Filter data BEFORE interpolation to improve performance
        # Only process the methods and sensors the user has selected
        data_to_clean <- vh_results()

        # Filter by selected methods (if any specified)
        if (!is.null(input$methods_to_clean) && length(input$methods_to_clean) > 0) {
          data_to_clean <- data_to_clean %>%
            dplyr::filter(method %in% input$methods_to_clean)
        }

        # Filter by selected sensors (if any specified)
        if (!is.null(input$sensors_to_clean) && length(input$sensors_to_clean) > 0) {
          data_to_clean <- data_to_clean %>%
            dplyr::filter(sensor_position %in% input$sensors_to_clean)
        }

        cat("\n")
        cat("=======================================================================\n")
        cat("CLEANING BUTTON DIAGNOSTIC\n")
        cat("=======================================================================\n")
        cat("Full dataset:", nrow(vh_results()), "rows\n")
        cat("Filtered for cleaning:", nrow(data_to_clean), "rows\n\n")

        # Apply interpolation to filtered subset
        cat("Applying interpolation (C++ optimised)...\n")
        timing_clean <- system.time({
          cleaned <- sapfluxr::filter_and_interpolate_vh(
            vh_flagged = data_to_clean,
            flags_to_interpolate = input$flags_to_interpolate,
            interpolation_method = input$interp_method,
            max_gap_hours = input$max_gap_hours,
            keep_original_values = TRUE,
            keep_interpolated_flag = TRUE,
            group_by_method = TRUE,
            group_by_sensor = TRUE,
            handle_multi_method = input$handle_multi_method,
            verbose = FALSE
          )
        })
        cat(sprintf("\nInterpolation time: %.3f seconds\n", timing_clean["elapsed"]))

        # Store cleaned data and propagate to shared session state
        cat("Updating reactive data...\n")
        timing_reactive <- system.time({
          cleaned_data(cleaned)
          if (!is.null(rv)) rv$vh_results <- cleaned
        })
        cat(sprintf("Reactive update time: %.3f seconds\n", timing_reactive["elapsed"]))

        total_time <- timing_clean["elapsed"] + timing_reactive["elapsed"]
        cat(sprintf("\nTOTAL TIME: %.3f seconds\n", total_time))
        cat("=======================================================================\n\n")

        # Close loading message
        shinyWidgets::closeSweetAlert(session = session)

        # Show success message
        n_interpolated <- sum(cleaned$is_interpolated, na.rm = TRUE)
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Cleaning Applied!",
          text = sprintf("Successfully interpolated %s values.",
                        format(n_interpolated, big.mark = ",")),
          type = "success",
          timer = 3000
        )

      }, error = function(e) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Cleaning Error",
          text = paste("Error applying cleaning:", e$message),
          type = "error"
        )
      })
    })

    # Cleaning summary output
    output$cleaning_summary_ui <- renderUI({
      preview <- cleaning_preview()
      if (is.null(preview)) return(NULL)

      div(
        style = "margin-top: 15px; padding: 10px; background-color: #f9f9f9; border-radius: 3px;",
        h5("Preview Summary", style = "margin-top: 0;"),

        if (nrow(preview$summary_table) > 0) {
          tagList(
            tags$table(
              style = "width: 100%; font-size: 0.9em;",
              tags$thead(
                tags$tr(
                  tags$th("Flag Type"),
                  tags$th("Count"),
                  tags$th("Interpolate")
                )
              ),
              tags$tbody(
                lapply(seq_len(nrow(preview$summary_table)), function(i) {
                  row <- preview$summary_table[i, ]
                  tags$tr(
                    tags$td(row$flag_type),
                    tags$td(row$original_count),
                    tags$td(row$will_interpolate)
                  )
                })
              )
            )
          )
        },

        if (nrow(preview$gap_report) > 0) {
          tagList(
            tags$hr(),
            p(strong(sprintf("Gaps Detected: %d", nrow(preview$gap_report)))),
            tags$table(
              style = "width: 100%; font-size: 0.85em;",
              tags$thead(
                tags$tr(
                  tags$th("Group"),
                  tags$th("Duration"),
                  tags$th("Action")
                )
              ),
              tags$tbody(
                lapply(seq_len(min(5, nrow(preview$gap_report))), function(i) {
                  gap <- preview$gap_report[i, ]
                  tags$tr(
                    tags$td(gap$group, style = "font-size: 0.8em;"),
                    tags$td(sprintf("%.1f hr", gap$duration_hours)),
                    tags$td(gap$action,
                            style = paste0("color: ", if(gap$action == "INTERPOLATE") "#2ca02c" else "#ff7f0e"))
                  )
                })
              )
            ),
            if (nrow(preview$gap_report) > 5) {
              p(style = "font-size: 0.8em; color: #666; margin-top: 5px;",
                sprintf("... and %d more gap(s)", nrow(preview$gap_report) - 5))
            }
          )
        }
      )
    })

    # Quality flag markers data
    quality_markers <- reactive({
      req(vh_results())

      data <- vh_results()

      # Determine which Vh column exists (prefer current best estimate)
      vh_col <- if ("Vs_cm_hr" %in% names(data)) {
        "Vs_cm_hr"
      } else if ("Vh_cm_hr_sc" %in% names(data)) {
        "Vh_cm_hr_sc"
      } else if ("Vh_cm_hr_zf" %in% names(data)) {
        "Vh_cm_hr_zf"
      } else {
        "Vh_cm_hr"
      }

      # Get non-OK quality flags — keep the original column name so renderPlotly
      # can access it via the same vh_col cascade (renaming to Vh_cm_hr breaks the
      # Vs_cm_hr reference in the flag-marker traces).
      markers <- data %>%
        dplyr::filter(quality_flag != "OK") %>%
        dplyr::select(datetime, pulse_id, quality_flag, method, sensor_position, !!sym(vh_col)) %>%
        distinct()

      markers
    })

    # Main time series plot - Integrated render (handles base lines with persistent zoom)
    output$timeseries_plot <- plotly::renderPlotly({
      tryCatch({
        style_config <- plot_settings()
        data <- filtered_data()
        req(nrow(data) > 0)

        # Determine which Vh column to use (prefer current best estimate)
        is_corrected <- FALSE
        vh_col <- if ("Vs_cm_hr" %in% names(data)) {
          is_corrected <- TRUE
          "Vs_cm_hr"
        } else if ("Vh_cm_hr_sc" %in% names(data)) {
          is_corrected <- TRUE
          "Vh_cm_hr_sc"
        } else if ("Vh_cm_hr_zf" %in% names(data)) {
          is_corrected <- TRUE
          "Vh_cm_hr_zf"
        } else {
          "Vh_cm_hr"
        }

        # Create base plot with source for event tracking
        p <- plotly::plot_ly(source = "timeseries")

        # Add trace for each method
        for (method in unique(data$method)) {
          method_data <- data %>% dplyr::filter(method == !!method)

          # Separate by sensor position if both selected
          if (length(input$sensor_position) > 1) {
            for (pos in unique(method_data$sensor_position)) {
              pos_data <- method_data %>% dplyr::filter(sensor_position == !!pos)
              trace_name <- paste0(method, " (", pos, ")")
              
              style <- get_plot_style(method = method, sensor = pos, is_corrected = is_corrected, config = style_config)

              p <- p %>%
                plotly::add_trace(
                  data = pos_data,
                  x = ~datetime,
                  y = as.formula(paste0("~", vh_col)),
                  customdata = ~pulse_id,
                  type = "scatter",
                  mode = if (input$show_points) "lines+markers" else "lines",
                  name = trace_name,
                  line = style,
                  marker = if (input$show_points) list(size = 4, color = style$color) else NULL,
                  legendgroup = trace_name,
                  showlegend = TRUE,
                  hovertemplate = paste0(
                    "<b>", trace_name, "</b><br>",
                    "Time: %{x}<br>",
                    "Vh: %{y:.2f} cm/hr<br>",
                    "<extra></extra>"
                  )
                )
            }
          } else {
            # Single sensor position
            pos <- input$sensor_position[1]
            style <- get_plot_style(method = method, sensor = pos, is_corrected = is_corrected, config = style_config)
            
            p <- p %>%
              plotly::add_trace(
                data = method_data,
                x = ~datetime,
                y = as.formula(paste0("~", vh_col)),
                type = "scatter",
                customdata = ~pulse_id,
                mode = if (input$show_points) "lines+markers" else "lines",
                name = method,
                line = style,
                marker = if (input$show_points) list(size = 4, color = style$color) else NULL,
                legendgroup = method,
                showlegend = TRUE,
                hovertemplate = paste0(
                  "<b>", method, "</b><br>",
                  "Time: %{x}<br>",
                  "Vh: %{y:.2f} cm/hr<br>",
                  "<extra></extra>"
                )
              )
          }
        }

        # Apply standard layout
        base_layout <- get_standard_layout(
          title = "",
          xtitle = "Date/Time",
          ytitle = "Heat Pulse Velocity (cm/hr)",
          uirevision = "timeseries_main"
        )
        
        # Explicit Zoom Range tracking to enforce persistence across redraws
        if (!is.null(current_xrange())) {
          base_layout$xaxis$range <- current_xrange()
          base_layout$xaxis$autorange <- FALSE
        }

        p <- p %>% plotly::layout(
          xaxis = base_layout$xaxis,
          yaxis = base_layout$yaxis,
          yaxis2 = base_layout$yaxis2,
          plot_bgcolor = base_layout$plot_bgcolor,
          paper_bgcolor = base_layout$paper_bgcolor,
          showlegend = TRUE,
          legend = base_layout$legend,
          hovermode = base_layout$hovermode,
          uirevision = base_layout$uirevision,
          margin = base_layout$margin
        ) %>%
          apply_standard_plotly_config(filename = "heat_pulse_velocity_timeseries", add_csv_download = TRUE) %>%
          plotly::event_register("plotly_relayout") %>%
          plotly::event_register("plotly_click")

        return(p)

      }, error = function(e) {
        # Return empty plot with error message
        plotly::plot_ly(source = "timeseries") %>%
          plotly::layout(
            title = list(text = paste("Plot Error:", e$message), font = list(color = "red")),
            xaxis = list(title = "Time"),
            yaxis = list(title = "Vh (cm/hr)", fixedrange = TRUE),
            uirevision = "timeseries_main"
          ) %>%
          plotly::event_register("plotly_relayout") %>%
          plotly::event_register("plotly_click")
      })
    })

    # Use plotlyProxy to handle dynamic overlays (VPD, Flags, Interpolation)
    observe({
      req(vh_results())
      data <- filtered_data()
      style_config <- plot_settings()
      
      # Determine base line count (one for each method/sensor)
      n_base <- length(input$methods) * length(input$sensor_position)
      
      # Step 1: Clear all previous overlays (Traces at indices n_base..END)
      tryCatch({
        for (i in 1:100) { 
          plotly::plotlyProxy("timeseries_plot", session) %>%
            plotly::plotlyProxyInvoke("deleteTraces", list(n_base))
        }
      }, error = function(e) {})

      # Determine column names for markers
      vh_col <- if ("Vs_cm_hr" %in% names(data)) "Vs_cm_hr" else 
                if ("Vh_cm_hr_sc" %in% names(data)) "Vh_cm_hr_sc" else "Vh_cm_hr"
      is_corrected <- (vh_col != "Vh_cm_hr")

      # Step 2: Add Quality Flag Markers
      if (isTRUE(input$show_quality_flags)) {
        markers <- quality_markers()
        if (!is.null(markers) && nrow(markers) > 0) {
          # Filter markers to visible methods/positions
          markers <- markers %>%
            dplyr::filter(method %in% input$methods, sensor_position %in% input$sensor_position)
          
          # Also filter by selected flags
          if (!is.null(input$quality_flags) && length(input$quality_flags) > 0) {
            markers <- markers %>% dplyr::filter(quality_flag %in% input$quality_flags)
          }

          if (nrow(markers) > 0) {
            flag_shapes <- c("DATA_OUTLIER" = "x", "DATA_SUSPECT" = "diamond", "DATA_MISSING" = "triangle-up",
                           "DATA_ILLOGICAL" = "square", "CALC_FAILED" = "circle-open")
            
            for (flag in unique(markers$quality_flag)) {
              flag_data <- markers %>% dplyr::filter(quality_flag == !!flag)
              shape <- flag_shapes[[flag]] %||% "circle"
              col <- flag_colors[[flag]] %||% "#999999"
              y_val <- if (flag == "DATA_MISSING") rep(0, nrow(flag_data)) else flag_data[[vh_col]]

              plotly::plotlyProxy("timeseries_plot", session) %>%
                plotly::plotlyProxyInvoke("addTraces", list(
                  x = flag_data$datetime, y = y_val,
                  type = "scatter", mode = "markers", name = flag,
                  marker = list(symbol = shape, size = 8, color = col, line = list(width = 1, color = "white")),
                  hovertemplate = paste0("<b>", flag, "</b><br>Time: %{x}<br>Vel: %{y:.2f}<extra></extra>")
                ))
            }
          }
        }
      }

      # Step 3: Add Interpolation Markers
      if (isTRUE(input$show_interpolated) && "is_interpolated" %in% names(data)) {
        interp_data <- data %>% dplyr::filter(is_interpolated == TRUE)
        if (nrow(interp_data) > 0) {
          for (m in unique(interp_data$method)) {
            m_interp <- interp_data %>% dplyr::filter(method == !!m)
            style <- get_plot_style(method = m, sensor = "outer", is_corrected = is_corrected, config = style_config)
            
            plotly::plotlyProxy("timeseries_plot", session) %>%
              plotly::plotlyProxyInvoke("addTraces", list(
                x = m_interp$datetime, y = m_interp[[vh_col]],
                type = "scatter", mode = "markers", name = paste(m, "(Interp)"),
                marker = list(symbol = "circle-open", size = 8, color = style$color, line = list(width = 2, color = style$color)),
                hovertemplate = paste0("<b>Interpolated</b><br>Method: ", m, "<br>Time: %{x}<br>Vel: %{y:.2f}<extra></extra>")
              ))
          }
        }
      }

      # Step 4: Add VPD Trace (Right Axis)
      if (isTRUE(input$show_vpd) && !is.null(weather_vpd())) {
        vpd <- weather_vpd()
        # Filter to current data range
        dr <- range(data$datetime, na.rm = TRUE)
        vpd_f <- vpd %>% dplyr::filter(datetime >= dr[1], datetime <= dr[2])
        
        if (nrow(vpd_f) > 0) {
          # 1. Determine Velocity axis range explicitly
          vh_vals <- data[[vh_col]]
          vh_max_data <- max(vh_vals, na.rm = TRUE)
          vh_min_data <- min(vh_vals, na.rm = TRUE)
          
          # Add 5% padding
          vh_pad <- (vh_max_data - vh_min_data) * 0.05
          if (vh_pad == 0) vh_pad <- 1
          
          v_min <- min(0, vh_min_data - vh_pad)
          v_max <- max(0, vh_max_data + vh_pad)
          if (v_max <= 0) v_max <- 1 # Prevent division by zero
          
          # 2. Determine VPD axis range proportional to Velocity
          vpd_max_data <- max(vpd_f$vpd_kpa, na.rm = TRUE)
          p_max <- vpd_max_data * 1.1 # 10% headroom
          
          # Zero alignment: p_min / p_max = v_min / v_max
          p_min <- (v_min / v_max) * p_max

          plotly::plotlyProxy("timeseries_plot", session) %>%
            plotly::plotlyProxyInvoke("addTraces", list(
              x = vpd_f$datetime, y = vpd_f$vpd_kpa,
              type = "scatter", mode = "lines", name = "VPD (kPa)",
              line = get_plot_style(is_vpd = TRUE, config = style_config),
              yaxis = "y2", hovertemplate = "<b>VPD</b><br>Time: %{x}<br>VPD: %{y:.2f} kPa<extra></extra>"
            )) %>%
            plotly::plotlyProxyInvoke("relayout", list(
              # Important: We must set BOTH ranges to force alignment
              yaxis = list(title = "Heat Pulse Velocity (cm/hr)", range = c(v_min, v_max), autorange = FALSE,
                           showline = TRUE, linecolor = "black", showgrid = FALSE,
                           zeroline = TRUE, zerolinecolor = "black", zerolinewidth = 0.5),
              yaxis2 = list(title = "VPD (kPa)", overlaying = "y", side = "right", range = c(p_min, p_max),
                            showgrid = FALSE, zeroline = TRUE, zerolinecolor = "black", zerolinewidth = 0.5,
                            showline = TRUE, linecolor = "black", fixedrange = TRUE),
              "margin.r" = 80
            ))
        }
      } else {
        # Ensure axis reset and restore standard theme (black lines, no grid)
        plotly::plotlyProxy("timeseries_plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", list(
            yaxis = list(
              title = "Heat Pulse Velocity (cm/hr)", 
              autorange = TRUE,
              showline = TRUE, 
              linecolor = "black", 
              showgrid = FALSE,
              zeroline = TRUE, 
              zerolinecolor = "black", 
              zerolinewidth = 0.5
            ),
            yaxis2 = NULL, 
            "margin.r" = 40
          ))
      }
    }) %>% bindEvent(input$show_quality_flags, input$quality_flags, input$show_interpolated, input$show_vpd, input$methods, input$sensor_position, cleaning_applied())

    # Capture current plot zoom/ranges when user interacts with range slider
    # Use debouncing to prevent excessive updates while dragging
    relayout_debounced <- debounce(reactive({
      event_data("plotly_relayout", source = "timeseries")
    }), 500)  # Wait 500ms after user stops dragging

    observeEvent(relayout_debounced(), {
      relayout_data <- relayout_debounced()

      if (!is.null(relayout_data)) {
        # Filter out non-relevant relayout events (autosize, axis additions, etc.)
        # We only care about X-axis range changes for zoom persistence
        is_zoom_event <- any(c("xaxis.range", "xaxis.range[0]", "xaxis.autorange") %in% names(relayout_data))

        if (!is_zoom_event) return()

        # Handle X-axis range update
        if (!is.null(relayout_data$xaxis.range) && length(relayout_data$xaxis.range) == 2) {
          xrange <- relayout_data$xaxis.range
          
          # Update date inputs
          shinyWidgets::updateAirDateInput(session, "start_datetime", value = as.POSIXct(xrange[1], tz = "UTC"))
          shinyWidgets::updateAirDateInput(session, "end_datetime", value = as.POSIXct(xrange[2], tz = "UTC"))

          # Store current range
          current_xrange(xrange)

        } else if (!is.null(relayout_data$`xaxis.range[0]`)) {
          xrange <- c(relayout_data$`xaxis.range[0]`, relayout_data$`xaxis.range[1]`)

          # Update date inputs
          shinyWidgets::updateAirDateInput(session, "start_datetime", value = as.POSIXct(xrange[1], tz = "UTC"))
          shinyWidgets::updateAirDateInput(session, "end_datetime", value = as.POSIXct(xrange[2], tz = "UTC"))

          # Store current range
          current_xrange(xrange)
        }

        # Handle autorange reset
        if (isTRUE(relayout_data$`xaxis.autorange`)) {
          current_xrange(NULL)
          req(vh_results())
          date_range <- range(vh_results()$datetime, na.rm = TRUE)
          shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
          shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
        }
      }
    })

    # Apply time range when button clicked or when filters change
    apply_time_range <- function() {
      req(input$start_datetime, input$end_datetime)

      cat("\n=== APPLYING TIME RANGE ===\n")
      cat("Start datetime:", as.character(input$start_datetime), "\n")
      cat("End datetime:", as.character(input$end_datetime), "\n")

      # Convert POSIXct to ISO date strings for plotly
      start_str <- format(input$start_datetime, "%Y-%m-%d %H:%M:%S")
      end_str <- format(input$end_datetime, "%Y-%m-%d %H:%M:%S")

      cat("Formatted start:", start_str, "\n")
      cat("Formatted end:", end_str, "\n")

      # Update plot range using plotlyProxy
      plotly::plotlyProxy("timeseries_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list(
          "xaxis.range" = list(start_str, end_str)
        ))

      # Store current range (as ISO strings)
      current_xrange(c(start_str, end_str))

      cat("Plot range applied\n")
    }

    # Apply range when button clicked
    observeEvent(input$apply_range, {
      cat("Apply range button clicked\n")
      apply_time_range()
    })

    # Reset zoom button - reset to full data range
    observeEvent(input$reset_zoom, {
      req(vh_results())
      date_range <- range(vh_results()$datetime, na.rm = TRUE)

      # Update date/time inputs to full range
      shinyWidgets::updateAirDateInput(
        session = session,
        inputId = "start_datetime",
        value = date_range[1]
      )

      shinyWidgets::updateAirDateInput(
        session = session,
        inputId = "end_datetime",
        value = date_range[2]
      )

      # Reset plot to autorange
      current_xrange(NULL)
      current_yrange(NULL)

      plotly::plotlyProxy("timeseries_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list(
          "xaxis.autorange" = TRUE,
          "yaxis.autorange" = TRUE
        ))
    })

    # Capture click events on plot points
    observeEvent(event_data("plotly_click", source = "timeseries"), {
      click_data <- event_data("plotly_click", source = "timeseries")

      if (!is.null(click_data)) {
        cat("\n=== PLOT CLICK EVENT ===\n")
        cat("Click data:", names(click_data), "\n")
        cat("Point number:", click_data$pointNumber, "\n")
        cat("Curve number:", click_data$curveNumber, "\n")

        # Get the clicked point's data
        # The customdata field should contain pulse_id if we set it
        req(vh_results())
        data <- filtered_data()

        # Get curve number and point number
        curve_num <- click_data$curveNumber + 1  # plotly is 0-indexed
        point_num <- click_data$pointNumber + 1  # plotly is 0-indexed

        cat("Adjusted curve:", curve_num, "point:", point_num, "\n")

        # Extract pulse_id directly from customdata (no timezone issues!)
        # customdata contains pulse_id from the plotted data
        if (!is.null(click_data$customdata)) {
          pulse_id <- click_data$customdata
          cat("✓ Extracted pulse_id from customdata:", pulse_id, "\n")

          selected_pulse_id(pulse_id)

          # Show notification
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Pulse Selected",
            text = paste("Viewing Pulse ID:", pulse_id),
            type = "info",
            timer = 2000
          )
        } else {
          cat("WARNING: No customdata in click event - pulse_id not available\n")
          cat("  Make sure add_trace() includes customdata = ~pulse_id\n")
        }
      }
    })


    # Return the selected pulse ID reactive
    return(selected_pulse_id)
  })
}
