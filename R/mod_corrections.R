# mod_corrections.R
# Module for Changepoint-Based Spacing Correction
#
# Implements segment-based Burgess et al. (2001) spacing correction
# using changepoint detection to identify baseline shifts

# UI ----
correctionsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Configuration and Controls
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "About Changepoint-Based Correction",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p(class = "text-info", strong("Note:"), " View Tab 4 (Visualise Raw HPV) first to identify data quality before correction."),
          hr(),
          p("This approach detects baseline shifts in daily minimum velocities caused by probe movement (tree swelling/shrinkage)."),
          tags$ul(
            tags$li(strong("Step 1:"), " Detect changepoints that divide data into segments"),
            tags$li(strong("Step 2:"), " Apply separate Burgess corrections per segment"),
            tags$li(strong("Step 3:"), " Review segment-specific results")
          ),
          p(tags$small(em("Based on Burgess et al. (2001) with PELT changepoint detection")))
        ),

        # Changepoint definition with tabbed interface
        box(
          width = 12,
          title = "Define Zero-Flow Changepoints",
          status = "primary",
          solidHeader = TRUE,

          helpText("Changepoints mark dates where probe alignment shifts, dividing data into segments for separate calibration."),

          # Tabbed interface for Manual vs Auto-detect
          tabsetPanel(
            id = ns("changepoint_tabs"),
            type = "tabs",

            # Tab 1: Manual Definition
            tabPanel(
              "Manual",
              br(),

              helpText("Add a changepoint at a specific date/time where you know the baseline shifted."),

              fluidRow(
                column(6, dateInput(ns("changepoint_date"), "Changepoint Date", value = NULL)),
                column(6, textInput(ns("changepoint_time"), "Time (HH:MM)", value = "00:00"))
              ),

              actionButton(ns("add_changepoint"), "Add Changepoint", icon = icon("plus"),
                          class = "btn-success", width = "100%")
            ),

            # Tab 2: Auto-Detect
            tabPanel(
              "Auto-Detect",
              br(),

              helpText(
                icon("info-circle"),
                " Automatically detect baseline shifts in daily minimum velocities using PELT changepoint detection."
              ),

              div(
                style = "margin-bottom: 10px;",
                tags$label(
                  "Penalty Type",
                  HTML('<span style="color: #999; cursor: help; margin-left: 5px;" title="MBIC (Modified Bayesian Information Criterion): Most conservative, fewest changepoints. BIC (Bayesian Information Criterion): Moderate number of changepoints. Manual: Set custom penalty value (0-100), higher values = fewer changepoints."><i class="fa fa-circle-question"></i></span>')
                ),
                selectInput(
                  ns("penalty_type"),
                  NULL,
                  choices = c(
                    "MBIC (Conservative)" = "MBIC",
                    "BIC (Moderate)" = "BIC",
                    "Manual (Custom)" = "Manual"
                  ),
                  selected = "MBIC"
                )
              ),

              conditionalPanel(
                condition = sprintf("input['%s'] == 'Manual'", ns("penalty_type")),
                sliderInput(
                  ns("penalty_value"),
                  HTML('Penalty Value <span style="color: #999; cursor: help;" title="Higher values produce fewer changepoints (more conservative). Lower values produce more changepoints (more sensitive to small shifts). Range: 0-100."><i class="fa fa-circle-question"></i></span>'),
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 5
                )
              ),

              fluidRow(
                column(
                  6,
                  numericInput(
                    ns("min_segment_days"),
                    HTML('Min Segment Days <span style="color: #999; cursor: help;" title="Minimum number of days required for a segment. Shorter segments indicate spurious changepoints and will be merged with adjacent segments. Typical: 7-14 days."><i class="fa fa-circle-question"></i></span>'),
                    value = 7,
                    min = 1,
                    max = 30,
                    step = 1
                  ),
                  selectInput(
                    ns("detect_method_filter"),
                    HTML('Method <span style="color: #999; cursor: help;" title="HPV calculation method to use for daily minima. HRM (Heat Ratio Method) is recommended as it is validated by Burgess for low flows."><i class="fa fa-circle-question"></i></span>'),
                    choices = c("HRM" = "HRM", "MHR" = "MHR"),
                    selected = "HRM"
                  )
                ),
                column(
                  6,
                  selectInput(
                    ns("detect_sensor_position"),
                    HTML('Sensor Position <span style="color: #999; cursor: help;" title="Which sensor to use for changepoint detection. Outer is recommended as it is more reliably in sapwood and less affected by heartwood interference."><i class="fa fa-circle-question"></i></span>'),
                    choices = c("Outer" = "outer", "Inner" = "inner"),
                    selected = "outer"
                  ),
                  checkboxInput(
                    ns("merge_short_segments"),
                    HTML('Merge Short Segments <span style="color: #999; cursor: help;" title="Automatically merge segments shorter than Min Segment Days with adjacent segments. Recommended to avoid spurious changepoints from outliers."><i class="fa fa-circle-question"></i></span>'),
                    value = TRUE
                  )
                )
              ),

              actionButton(
                ns("detect_changepoints"),
                "Detect Changepoints",
                icon = icon("search"),
                class = "btn-info",
                width = "100%"
              ),

              br(), br(),

              # Results display
              conditionalPanel(
                condition = sprintf("output['%s']", ns("changepoints_detected")),
                h5("Detected Changepoints:"),
                helpText(icon("info-circle"), " Detected changepoints shown as ", tags$span(style = "color: orange;", "orange dotted lines"), " on plot. Click ", tags$code("[+]"), " to add individually or use button below to add all."),
                uiOutput(ns("detected_changepoints_list")),
                br(),
                actionButton(
                  ns("add_detected_changepoints"),
                  "Add All Detected Changepoints",
                  icon = icon("check"),
                  class = "btn-success",
                  width = "100%"
                )
              )
            )
          ),

          # Current changepoints display (shared between tabs)
          hr(),
          h5("Current Changepoints:"),
          helpText("Confirmed changepoints shown as ", tags$span(style = "color: red;", "red dashed lines"), " with baselines. Click ", tags$code("[X]"), " to remove."),
          uiOutput(ns("changepoint_list")),
          br(),
          actionButton(ns("clear_changepoints"), "Clear All Changepoints", icon = icon("trash"),
                      class = "btn-warning", width = "100%")
        ),

        # Analysis settings
        box(
          width = 12,
          title = "Analysis Settings",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,

          selectInput(
            ns("sensor_to_correct"),
            "Sensor to Correct",
            choices = c("Outer" = "outer", "Inner" = "inner"),
            selected = "outer"
          ),

          numericInput(
            ns("k_assumed"),
            "Assumed k (cm²/s)",
            value = 0.0025,
            min = 0.001,
            max = 0.005,
            step = 0.0001
          ),

          numericInput(
            ns("probe_spacing"),
            "Probe Spacing (cm)",
            value = 0.5,
            min = 0.1,
            max = 2.0,
            step = 0.1
          )
        ),

        # Action button
        box(
          width = 12,
          title = "Run Analysis",
          status = "success",
          solidHeader = TRUE,

          actionButton(
            ns("run_correction"),
            "Apply Segment-Based Spacing Correction",
            icon = icon("play"),
            class = "btn-primary",
            width = "100%"
          )
        )
      ),

      # Right column: Visualisation and Results
      column(
        width = 8,

        # Interactive changepoint plot
        box(
          width = 12,
          title = "Daily Minimum Velocities with Changepoints",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,

          helpText("Visualise daily minimum velocities and changepoints. Click on the plot to add a changepoint at that date."),

          checkboxInput(
            ns("show_original_data"),
            "Overlay original data points (all timestamps)",
            value = FALSE
          ),

          plotly::plotlyOutput(ns("plot_changepoints"), height = "500px"),

          br(),
          p(class = "text-muted", tags$small(
            icon("info-circle"),
            " Tip: Click any date on the plot to populate the Manual tab for adding a changepoint."
          ))
        ),

        # Spacing Correction Results
        box(
          width = 12,
          title = "Spacing Correction Results",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_correction_results")),
            p(em("No results yet. Define changepoints and run spacing correction."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_correction_results")),

            tabsetPanel(
              id = ns("results_tabs"),

              tabPanel(
                "Segment Results",
                br(),
                helpText("Each segment between changepoints gets separate Burgess correction coefficients."),
                verbatimTextOutput(ns("segment_results_table"))
              ),

              tabPanel(
                "Summary",
                br(),
                verbatimTextOutput(ns("correction_summary"))
              )
            )
          )
        ),

        # Correction Status
        box(
          width = 12,
          title = "Active Correction Status",
          status = "success",
          solidHeader = TRUE,

          p("The most recent correction is automatically applied to downstream analyses:"),

          verbatimTextOutput(ns("correction_status")),

          hr(),

          actionButton(
            ns("reset_corrections"),
            "Reset to Uncorrected Data",
            icon = icon("undo"),
            class = "btn-warning",
            width = "100%"
          )
        )
      )
    )
  )
}

# Server ----
correctionsServer <- function(id, vh_results, heat_pulse_data, probe_config, wood_properties, calc_methods) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
      changepoints = list(),  # List of POSIXct dates
      detected_result = NULL,  # Changepoint detection result
      correction_result = NULL,  # Spacing correction result
      corrected_vh = NULL,
      correction_applied = FALSE
    )

    # Initialize settings from configuration
    observe({
      req(vh_results())

      vh_data <- vh_results()
      if (!is.null(vh_data) && nrow(vh_data) > 0) {
        # Set initial date to midpoint of data
        date_range <- range(vh_data$datetime, na.rm = TRUE)
        mid_date <- as.Date(date_range[1]) + as.numeric(diff(as.Date(date_range))) / 2

        updateDateInput(session, "changepoint_date", value = mid_date)
      }
    })

    # Initialize analysis settings from actual config
    observe({
      # Get k from wood properties if available
      if (!is.null(wood_properties())) {
        wood <- wood_properties()
        if (!is.null(wood$thermal_diffusivity)) {
          updateNumericInput(session, "k_assumed", value = wood$thermal_diffusivity)
        }
      }

      # Get probe spacing from probe config if available
      if (!is.null(probe_config())) {
        probe <- probe_config()
        if (!is.null(probe$probe_spacing)) {
          updateNumericInput(session, "probe_spacing", value = probe$probe_spacing)
        }
      }
    })

    # ==================================================================
    # MANUAL CHANGEPOINT ENTRY
    # ==================================================================

    observeEvent(input$add_changepoint, {
      req(input$changepoint_date, input$changepoint_time)

      tryCatch({
        cp_str <- paste(input$changepoint_date, input$changepoint_time)
        cp_dt <- as.POSIXct(cp_str, format = "%Y-%m-%d %H:%M")

        if (is.na(cp_dt)) {
          showNotification("Invalid date/time format", type = "error")
          return()
        }

        # Add to list (keep as POSIXct)
        rv$changepoints <- c(rv$changepoints, list(cp_dt))

        # Sort changepoints chronologically
        rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

        showNotification(
          sprintf("Added changepoint: %s", format(cp_dt, "%Y-%m-%d %H:%M")),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        showNotification(paste("Error adding changepoint:", e$message), type = "error")
      })
    })

    # Clear all changepoints
    observeEvent(input$clear_changepoints, {
      rv$changepoints <- list()
      rv$detected_result <- NULL
      showNotification("All changepoints cleared", type = "message", duration = 5)
    })

    # Display changepoint list with delete buttons
    output$changepoint_list <- renderUI({
      if (length(rv$changepoints) == 0) {
        return(p(em("No changepoints defined yet.")))
      }

      # Create list of changepoints with delete buttons
      cp_items <- lapply(seq_along(rv$changepoints), function(i) {
        cp <- rv$changepoints[[i]]
        cp_text <- format(cp, "%Y-%m-%d %H:%M")

        # Get baseline value if we have detection results
        baseline_text <- ""
        if (!is.null(rv$detected_result) && !is.null(rv$detected_result$segments)) {
          segments <- rv$detected_result$segments
          # Find which segment this changepoint ends
          seg_idx <- which(segments$end_date == as.Date(cp))
          if (length(seg_idx) > 0 && "baseline_value" %in% names(segments)) {
            baseline <- segments$baseline_value[seg_idx[1]]
            baseline_text <- sprintf(" (baseline: %.2f cm/hr)", baseline)
          }
        }

        div(
          style = "margin-bottom: 5px;",
          actionButton(
            session$ns(paste0("delete_cp_", i)),
            label = NULL,
            icon = icon("times"),
            class = "btn-xs btn-danger",
            style = "margin-right: 10px;"
          ),
          span(paste0(i, ". ", cp_text, baseline_text))
        )
      })

      tagList(cp_items)
    })

    # Handle delete button clicks
    observe({
      # Create observers for each delete button
      lapply(seq_along(rv$changepoints), function(i) {
        btn_id <- paste0("delete_cp_", i)
        observeEvent(input[[btn_id]], {
          rv$changepoints <- rv$changepoints[-i]
          showNotification(
            sprintf("Removed changepoint %d", i),
            type = "message",
            duration = 5
          )
        }, ignoreInit = TRUE)
      })
    })

    # ==================================================================
    # AUTO-DETECT CHANGEPOINTS
    # ==================================================================

    observeEvent(input$detect_changepoints, {
      req(vh_results())

      # Show waiter
      waiter <- waiter::Waiter$new(
        html = waiter::spin_fading_circles(),
        color = waiter::transparent(0.5)
      )
      waiter$show()

      # Ensure waiter is always hidden, even if code returns early or errors
      on.exit(waiter$hide(), add = TRUE)

      tryCatch({
        vh_data <- vh_results()

        # Step 1: Calculate daily minima
        daily_min <- sapfluxr::calculate_daily_minima(
          vh_data = vh_data,
          sensor_position = input$detect_sensor_position,
          method = input$detect_method_filter,
          vh_col = "Vh_cm_hr"
        )

        if (nrow(daily_min) < 10) {
          showNotification(
            "Not enough data for changepoint detection (need at least 10 days)",
            type = "error",
            duration = 5
          )
          return()
        }

        # Step 2: Detect changepoints
        cpt_result <- sapfluxr::detect_changepoints(
          daily_min = daily_min,
          penalty = input$penalty_type,
          penalty_value = if (input$penalty_type == "Manual") input$penalty_value else NULL,
          detection_type = "mean",
          min_segment_days = input$min_segment_days,
          merge_short_segments = input$merge_short_segments
        )

        # Step 3: Extract segment baselines
        segments <- sapfluxr::extract_segment_baselines(cpt_result)

        cpt_result$segments <- segments

        rv$detected_result <- cpt_result

        if (length(cpt_result$changepoints) == 0) {
          showNotification(
            "No changepoints detected with current settings. Try reducing penalty or minimum segment days.",
            type = "warning",
            duration = 5
          )
        } else {
          showNotification(
            sprintf("Detected %d changepoint(s), creating %d segment(s)",
                   length(cpt_result$changepoints),
                   cpt_result$parameters$n_segments),
            type = "message",
            duration = 5
          )
        }

      }, error = function(e) {
        showNotification(
          paste("Error detecting changepoints:", e$message),
          type = "error",
          duration = 10
        )
        rv$detected_result <- NULL
      })
    })

    # Show/hide detected changepoints output
    output$changepoints_detected <- reactive({
      !is.null(rv$detected_result) && length(rv$detected_result$changepoints) > 0
    })
    outputOptions(output, "changepoints_detected", suspendWhenHidden = FALSE)

    # Display detected changepoints with individual add buttons
    output$detected_changepoints_list <- renderUI({
      req(rv$detected_result)

      cpts <- rv$detected_result$changepoints
      segments <- rv$detected_result$segments

      if (length(cpts) == 0) {
        return(p(em("No changepoints detected.")))
      }

      # Create header
      header <- tags$p(strong(sprintf("Detected %d changepoint(s):", length(cpts))))

      # Create list of detected changepoints with individual add buttons
      cpt_items <- lapply(seq_along(cpts), function(i) {
        cp_date <- format(cpts[i], "%Y-%m-%d")

        # Get baseline info
        seg <- segments[segments$end_date == cpts[i], ]
        baseline_text <- ""
        if (nrow(seg) > 0 && "baseline_value" %in% names(seg)) {
          baseline <- seg$baseline_value[1]
          baseline_text <- sprintf(" (baseline: %.2f cm/hr, %d days)",
                                  baseline, seg$n_days[1])
        }

        div(
          style = "margin-bottom: 5px;",
          actionButton(
            session$ns(paste0("add_detected_cp_", i)),
            label = NULL,
            icon = icon("plus"),
            class = "btn-xs btn-success",
            style = "margin-right: 10px;"
          ),
          span(paste0(i, ". ", cp_date, baseline_text))
        )
      })

      # Combine header, items, and summary
      tagList(
        header,
        tagList(cpt_items),
        tags$p(
          tags$small(
            sprintf("This creates %d segment(s). Click [+] to add individual changepoints or use 'Add All' below.",
                   rv$detected_result$parameters$n_segments)
          )
        )
      )
    })

    # Handle individual add button clicks for detected changepoints
    observe({
      req(rv$detected_result)

      cpts <- rv$detected_result$changepoints

      # Create observers for each individual add button
      lapply(seq_along(cpts), function(i) {
        btn_id <- paste0("add_detected_cp_", i)
        observeEvent(input[[btn_id]], {
          # Convert Date to POSIXct at midnight
          cp_date <- cpts[i]
          cp_posix <- as.POSIXct(paste(cp_date, "00:00:00"), format = "%Y-%m-%d %H:%M:%S")

          # Add to changepoints list
          rv$changepoints <- c(rv$changepoints, list(cp_posix))

          # Sort chronologically
          rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

          showNotification(
            sprintf("Added changepoint: %s", format(cp_date, "%Y-%m-%d")),
            type = "message",
            duration = 3
          )
        }, ignoreInit = TRUE)
      })
    })

    # Add all detected changepoints to the list
    observeEvent(input$add_detected_changepoints, {
      req(rv$detected_result)

      cpts <- rv$detected_result$changepoints

      if (length(cpts) == 0) {
        showNotification("No changepoints to add", type = "warning")
        return()
      }

      # Convert Date to POSIXct at midnight
      cpts_posix <- lapply(cpts, function(d) {
        as.POSIXct(paste(d, "00:00:00"), format = "%Y-%m-%d %H:%M:%S")
      })

      # Add to existing changepoints
      rv$changepoints <- c(rv$changepoints, cpts_posix)

      # Sort chronologically
      rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

      showNotification(
        sprintf("Added %d changepoint(s) to the list", length(cpts)),
        type = "message",
        duration = 5
      )

      # Clear detected results after adding
      # rv$detected_result <- NULL  # Keep results for plot
    })

    # ==================================================================
    # INTERACTIVE CHANGEPOINT PLOT
    # ==================================================================

    output$plot_changepoints <- plotly::renderPlotly({
      req(vh_results())

      # Add explicit dependency on changepoints to trigger re-render
      rv$changepoints

      tryCatch({
        vh_data <- vh_results()

        # Use detection settings or defaults
        sensor <- if (!is.null(input$detect_sensor_position)) {
          input$detect_sensor_position
        } else {
          "outer"
        }

        method <- if (!is.null(input$detect_method_filter)) {
          input$detect_method_filter
        } else {
          "HRM"
        }

        # Calculate daily minima for plotting
        daily_min <- sapfluxr::calculate_daily_minima(
          vh_data = vh_data,
          sensor_position = sensor,
          method = method,
          vh_col = "Vh_cm_hr"
        )

        if (nrow(daily_min) == 0) {
          return(plotly::plotly_empty())
        }

        # Convert current changepoints to dates for plotting
        cpts_dates <- if (length(rv$changepoints) > 0) {
          # Convert list of POSIXct to vector of Dates
          # Using do.call to combine list elements into a vector
          as.Date(do.call(c, rv$changepoints))
        } else {
          NULL
        }

        # Get segments if available
        segments <- if (!is.null(rv$detected_result)) {
          rv$detected_result$segments
        } else {
          NULL
        }

        # Filter vh_data to same sensor/method for overlay
        vh_filtered <- vh_data[
          vh_data$sensor_position == sensor &
          vh_data$method == method,
        ]

        # Get proposed changepoints from auto-detect (not yet confirmed)
        proposed_cpts <- if (!is.null(rv$detected_result)) {
          rv$detected_result$changepoints
        } else {
          NULL
        }

        # Debug: Log what we're passing to the plot
        n_confirmed <- if (!is.null(cpts_dates)) length(cpts_dates) else 0
        n_proposed <- if (!is.null(proposed_cpts)) length(proposed_cpts) else 0

        message(sprintf("Plot rendering: %d confirmed changepoints, %d proposed changepoints",
                       n_confirmed, n_proposed))

        # Create interactive plot
        # NOTE: segments = NULL so they will be auto-generated from confirmed changepoints only
        sapfluxr::plot_changepoints_interactive(
          daily_min = daily_min,
          changepoints = cpts_dates,  # Confirmed changepoints (red lines + baselines)
          segments = NULL,  # Let plot auto-generate from confirmed changepoints
          proposed_changepoints = proposed_cpts,  # Detected but not confirmed (orange lines, no baselines)
          vh_data = vh_filtered,
          title = sprintf("Daily Minimum Velocities - %s Sensor (%s)",
                         toupper(sensor), method),
          show_baseline_values = TRUE,
          show_original_data = isTRUE(input$show_original_data)
        )

      }, error = function(e) {
        # Show error to user
        showNotification(
          paste("Error rendering changepoint plot:", e$message),
          type = "error",
          duration = 10
        )
        # Return empty plot on error
        plotly::plotly_empty()
      })
    })

    # Handle plot click events to populate manual changepoint date
    observeEvent(plotly::event_data("plotly_click", source = "changepoint_plot"), {
      click_data <- plotly::event_data("plotly_click", source = "changepoint_plot")

      if (!is.null(click_data)) {
        # Extract the x value (date)
        clicked_date <- as.Date(click_data$x)

        if (!is.na(clicked_date)) {
          # Update the manual tab date input
          updateDateInput(session, "changepoint_date", value = clicked_date)

          showNotification(
            sprintf("Date %s populated in Manual tab. Click 'Add Changepoint' to confirm.",
                   format(clicked_date, "%Y-%m-%d")),
            type = "message",
            duration = 3
          )
        }
      }
    })

    # ==================================================================
    # SPACING CORRECTION
    # ==================================================================

    observeEvent(input$run_correction, {
      req(vh_results())

      vh_data <- vh_results()

      # Show waiter
      waiter <- waiter::Waiter$new(
        html = waiter::spin_fading_circles(),
        color = waiter::transparent(0.5)
      )
      waiter$show()

      # Ensure waiter is always hidden
      on.exit(waiter$hide(), add = TRUE)

      tryCatch({
        # Convert changepoints to Date objects
        # rv$changepoints is a list of POSIXct, convert each to Date
        changepoint_dates <- if (length(rv$changepoints) > 0) {
          vapply(rv$changepoints, as.Date, FUN.VALUE = as.Date("2000-01-01"))
        } else {
          NULL
        }

        # Run segment-based spacing correction
        result <- sapfluxr::apply_spacing_correction_per_segment(
          vh_data = vh_data,
          changepoints = changepoint_dates,
          sensor_position = input$sensor_to_correct,
          method = "HRM",
          k_assumed = input$k_assumed,
          probe_spacing = input$probe_spacing,
          measurement_time = 80,
          verbose = FALSE
        )

        rv$correction_result <- result

        # Automatically apply corrections
        rv$corrected_vh <- result$vh_corrected
        rv$correction_applied <- TRUE

        n_segments <- result$metadata$n_segments
        showNotification(
          sprintf("Spacing correction completed! Applied %d segment-specific corrections.", n_segments),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        showNotification(
          paste("Error in spacing correction:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Has results flag
    output$has_correction_results <- reactive({
      !is.null(rv$correction_result)
    })
    outputOptions(output, "has_correction_results", suspendWhenHidden = FALSE)

    # Segment results table
    output$segment_results_table <- renderPrint({
      req(rv$correction_result)

      results <- rv$correction_result$segment_results

      if (length(results) == 0) {
        cat("No segment results available.\n")
        return()
      }

      cat("SEGMENT-BY-SEGMENT CORRECTION RESULTS\n")
      cat(strrep("=", 72), "\n\n")

      for (i in seq_along(results)) {
        seg <- results[[i]]

        cat(sprintf("Segment %d of %d\n", seg$segment_id, length(results)))
        cat(strrep("-", 72), "\n")
        cat(sprintf("Period: %s to %s\n",
                   format(seg$start_datetime, "%Y-%m-%d %H:%M"),
                   format(seg$end_datetime, "%Y-%m-%d %H:%M")))
        cat(sprintf("Observations: %d\n", seg$n_observations))
        cat(sprintf("Baseline (Zero Vh): %.1f cm/hr\n", seg$zero_vh))
        cat(sprintf("Mean Vh: %.2f cm/hr\n", seg$mean_vh))
        cat(sprintf("SD: %.2f | CV: %.3f\n", seg$sd_vh, seg$cv))
        cat(sprintf("Correction: %s\n", seg$correction_formula))
        cat(sprintf("Severity: %s\n", toupper(seg$severity)))
        cat("\n")
      }

      cat(strrep("=", 72), "\n")
    })

    # Correction summary
    output$correction_summary <- renderPrint({
      req(rv$correction_result)

      result <- rv$correction_result
      metadata <- result$metadata

      cat("SPACING CORRECTION SUMMARY\n")
      cat(strrep("=", 72), "\n\n")

      cat(sprintf("Sensor: %s\n", toupper(metadata$sensor_position)))
      cat(sprintf("Method: %s\n", metadata$method))
      cat(sprintf("k assumed: %.4f cm²/s\n", metadata$k_assumed))
      cat(sprintf("Probe spacing: %.1f cm\n", metadata$probe_spacing))
      cat(sprintf("Number of segments: %d\n", metadata$n_segments))
      cat(sprintf("Changepoints: %s\n",
                 if (length(metadata$changepoints) == 0) {
                   "None (single segment)"
                 } else {
                   paste(metadata$changepoints, collapse = ", ")
                 }))
      cat(sprintf("Date applied: %s\n", format(metadata$date_applied, "%Y-%m-%d %H:%M:%S")))
      cat(sprintf("Approach: %s\n", metadata$approach))

      cat("\n")
      cat(strrep("=", 72), "\n")
    })

    # Reset to uncorrected data
    observeEvent(input$reset_corrections, {
      rv$corrected_vh <- NULL
      rv$correction_applied <- FALSE
      showNotification(
        "Reset to uncorrected data.",
        type = "message",
        duration = 3
      )
    })

    # Correction status
    output$correction_status <- renderPrint({
      if (!rv$correction_applied || is.null(rv$correction_result)) {
        cat("✗ NO CORRECTIONS APPLIED\n")
        cat("──────────────────────────────────────\n")
        cat("Data shows original uncorrected velocities\n")

      } else {
        cat("✓ SEGMENT-BASED CORRECTIONS ACTIVE\n")
        cat("──────────────────────────────────────\n")

        metadata <- rv$correction_result$metadata

        cat(sprintf("Sensor: %s\n", toupper(metadata$sensor_position)))
        cat(sprintf("k used: %.4f cm²/s (assumed)\n", metadata$k_assumed))
        cat(sprintf("Segments: %d\n", metadata$n_segments))

        if (metadata$n_segments > 1) {
          cat(sprintf("Changepoints: %d\n", length(metadata$changepoints)))
        }

        n_corrected <- sum(rv$correction_result$vh_corrected$spacing_correction_applied, na.rm = TRUE)
        cat(sprintf("Data points corrected: %d\n", n_corrected))
      }
    })

    # Return corrected data for use in other modules
    return(reactive({
      if (is.null(rv$corrected_vh)) {
        vh_results()  # Return original if no correction applied
      } else {
        rv$corrected_vh  # Return corrected data
      }
    }))
  })
}
