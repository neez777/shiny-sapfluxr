# mod_wound_correction.R
# Module for Wound Correction with Temporal Tracking
#
# Implements wound expansion correction using Burgess et al. (2001) approach
# with support for multiple reinstallation dates

# UI ----
woundCorrectionUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Configuration
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "About Wound Correction",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p("Wound correction accounts for the wound created by probe installation, which expands over time as wound tissue forms."),
          tags$ul(
            tags$li(strong("Initial Installation:"), " First date of data with initial wound size (drill bit + wound tissue)"),
            tags$li(strong("Reinstallations:"), " Add dates when probe was removed and reinstalled with measured wound diameter"),
            tags$li(strong("Temporal Tracking:"), " Wound diameter is interpolated linearly between dates")
          ),
          p(tags$small(em("Based on Burgess et al. (2001) and ICT International Appendix 23.1")))
        ),

        # Initial installation info
        box(
          width = 12,
          title = "Initial Installation",
          status = "primary",
          solidHeader = TRUE,

          helpText(
            icon("info-circle"),
            " Initial installation date is automatically set to the first date of your data.",
            " Initial wound diameter is calculated from drill bit size and wound tissue addition."
          ),

          htmlOutput(ns("initial_install_info")),

          hr(),

          h5("Wood Properties Configuration:"),
          helpText("Drill bit diameter and wound tissue addition are loaded from your wood properties YAML file."),
          verbatimTextOutput(ns("wound_config_display"))
        ),

        # Reinstallation management
        box(
          width = 12,
          title = "Manage Reinstallations",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          helpText(
            icon("redo"),
            " Add dates when the probe was removed and reinstalled.",
            " The wound diameter will reset to the initial size at each reinstallation."
          ),

          fluidRow(
            column(6, dateInput(ns("reinstall_date"), "Reinstallation Date", value = NULL)),
            column(6, textInput(ns("reinstall_time"), "Time (HH:MM)", value = "00:00"))
          ),

          numericInput(
            ns("reinstall_measured_diameter"),
            "Measured Diameter at Removal (mm)",
            value = NULL,
            min = 1.5,
            step = 0.1
          ),

          actionButton(
            ns("add_reinstallation"),
            "Add Reinstallation",
            icon = icon("plus"),
            class = "btn-success",
            width = "100%"
          ),

          hr(),

          h5("Current Reinstallations:"),
          helpText("Click ", tags$code("[X]"), " to remove a reinstallation."),
          uiOutput(ns("reinstallation_list")),

          br(),

          actionButton(
            ns("clear_reinstallations"),
            "Clear All Reinstallations",
            icon = icon("trash"),
            class = "btn-warning",
            width = "100%"
          )
        ),

        # Final Measurement (Growth Rate)
        box(
          width = 12,
          title = "Final Measurement (Growth Rate)",
          status = "info",
          solidHeader = TRUE,

          helpText(
            "Define the final wound diameter at the end of the experiment (or a specific date).",
            " This determines the daily growth rate applied to all installation periods."
          ),

          fluidRow(
            column(6, dateInput(ns("final_measurement_date"), "Final Measurement Date", value = NULL)),
            column(6, numericInput(ns("final_measurement_diameter"), "Final Diameter (mm)", value = NULL, min = 1.5, step = 0.1))
          )
        ),

        # Apply wound correction
        box(
          width = 12,
          title = "Apply Wound Correction",
          status = "success",
          solidHeader = TRUE,

          helpText(
            "Apply wound correction to spacing-corrected velocity data.",
            " Correction uses temporal wound diameter tracking if reinstallations are defined."
          ),

          selectInput(
            ns("probe_spacing"),
            "Probe Spacing:",
            choices = c("5mm" = "5mm", "6mm" = "6mm"),
            selected = "5mm"
          ),

          actionButton(
            ns("apply_wound_correction"),
            "Apply Wound Correction",
            icon = icon("medkit"),
            class = "btn-primary",
            width = "100%"
          )
        )
      ),

      # Right column: Visualisation and Results
      column(
        width = 8,

        # Wound diameter timeline plot
        box(
          width = 12,
          title = "Temporal Wound Diameter",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,

          helpText("Visualize how wound diameter changes over time based on initial installation and reinstallation dates."),

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_temporal_wound_tracking")),
            p(em("No temporal wound tracking defined. Set final measurement date and diameter to see wound growth over time."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_temporal_wound_tracking")),
            plotly::plotlyOutput(ns("wound_diameter_plot"), height = "400px")
          )
        ),

        # Wound correction results
        box(
          width = 12,
          title = "Wound Correction Results",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_wound_results")),
            p(em("No results yet. Apply wound correction to see results."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_wound_results")),

            tabsetPanel(
              id = ns("wound_results_tabs"),

              tabPanel(
                "Before/After Comparison",
                br(),

                fluidRow(
                  column(4,
                    selectInput(
                      ns("plot_sensor_position"),
                      "Sensor Position:",
                      choices = c("Outer" = "outer", "Inner" = "inner"),
                      selected = "outer"
                    )
                  ),
                  column(4,
                    checkboxInput(
                      ns("show_raw_data"),
                      "Show Raw Data",
                      value = FALSE
                    )
                  ),
                  column(4,
                    checkboxInput(
                      ns("show_spacing_corrected"),
                      "Show Spacing Corrected",
                      value = FALSE
                    )
                  )
                ),

                plotly::plotlyOutput(ns("wound_correction_comparison"), height = "500px")
              ),

              tabPanel(
                "Correction Coefficients",
                br(),
                helpText("Wound correction coefficients (B) applied over time."),
                verbatimTextOutput(ns("wound_coefficients_table"))
              ),

              tabPanel(
                "Summary",
                br(),
                verbatimTextOutput(ns("wound_correction_summary"))
              )
            )
          )
        ),

        # Active wound correction status
        box(
          width = 12,
          title = "Active Wound Correction Status",
          status = "success",
          solidHeader = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_wound_results")),
            p(em("No wound correction applied."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_wound_results")),

            p("Wound correction is active and will be used in downstream analyses:"),
            verbatimTextOutput(ns("wound_status")),

            hr(),

            actionButton(
              ns("reset_wound_correction"),
              "Remove Wound Correction",
              icon = icon("undo"),
              class = "btn-warning",
              width = "100%"
            )
          )
        )
      )
    )
  )
}

# Server ----
woundCorrectionServer <- function(id,
                                   vh_data = reactive(NULL),
                                   wood_properties = reactive(NULL),
                                   probe_config = reactive(NULL),
                                   code_tracker = NULL) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
      reinstallations = data.frame(
        datetime = as.POSIXct(character()),
        measured_diameter_mm = numeric(),
        stringsAsFactors = FALSE
      ),
      wound_corrected_data = NULL,
      wound_correction_result = NULL,
      initial_date = NULL,
      initial_wound_mm = NULL
    )

    # Calculate initial installation info when data loads
    observe({
      req(vh_data())
      req(wood_properties())

      # Get first date from data
      rv$initial_date <- min(vh_data()$datetime, na.rm = TRUE)

      # Calculate initial wound diameter from wood properties
      wood <- wood_properties()
      if (inherits(wood, "WoodProperties")) {
        drill_mm <- wood$wound_correction$drill_bit_diameter_mm
        wound_add_mm <- wood$wound_correction$wound_addition_mm
        if (!is.null(drill_mm) && !is.null(wound_add_mm)) {
          rv$initial_wound_mm <- drill_mm + (2 * wound_add_mm)
        }

        # Auto-populate final measurement inputs from wood properties if available
        if (!is.null(wood$wound_correction$final_date)) {
          updateDateInput(session, "final_measurement_date", value = wood$wound_correction$final_date)
        }
        if (!is.null(wood$wound_correction$final_diameter_mm)) {
          updateNumericInput(session, "final_measurement_diameter", value = wood$wound_correction$final_diameter_mm)
        }
      }
    })

    # Display initial installation info
    output$initial_install_info <- renderUI({
      req(rv$initial_date)
      req(rv$initial_wound_mm)

      tagList(
        tags$table(
          class = "table table-condensed",
          tags$tr(
            tags$td(strong("Installation Date:")),
            tags$td(format(rv$initial_date, "%Y-%m-%d %H:%M"))
          ),
          tags$tr(
            tags$td(strong("Initial Wound Diameter:")),
            tags$td(sprintf("%.2f mm", rv$initial_wound_mm))
          )
        )
      )
    })

    # Display wound configuration from wood properties
    output$wound_config_display <- renderText({
      req(wood_properties())

      wood <- wood_properties()
      if (inherits(wood, "WoodProperties")) {
        wc <- wood$wound_correction
        sprintf("Drill Bit: %.1f mm\nWound Addition: %.1f mm per side\nInitial Wound: %.2f mm\nFinal Diam (Config): %s",
                wc$drill_bit_diameter_mm,
                wc$wound_addition_mm,
                wc$drill_bit_diameter_mm + (2 * wc$wound_addition_mm),
                if(is.null(wc$final_diameter_mm)) "Not set" else paste(wc$final_diameter_mm, "mm"))
      } else {
        "Wood properties not loaded or invalid format"
      }
    })

    # Add reinstallation
    observeEvent(input$add_reinstallation, {
      req(input$reinstall_date)
      req(input$reinstall_time)

      # Validate measured diameter
      if (is.null(input$reinstall_measured_diameter) || is.na(input$reinstall_measured_diameter)) {
        showNotification("Please enter the measured diameter at removal", type = "error")
        return()
      }

      if (input$reinstall_measured_diameter < 1.5) {
        showNotification("Measured diameter must be at least 1.5 mm", type = "error")
        return()
      }

      # Parse datetime
      datetime_str <- paste(input$reinstall_date, input$reinstall_time)
      datetime <- tryCatch({
        as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M")
      }, error = function(e) {
        showNotification("Invalid date/time format", type = "error")
        return(NULL)
      })

      req(datetime)

      # Safety check: Initialize reinstallations if needed
      if (is.null(rv$reinstallations) || !is.data.frame(rv$reinstallations)) {
        rv$reinstallations <- data.frame(
          datetime = as.POSIXct(character()),
          measured_diameter_mm = numeric(),
          stringsAsFactors = FALSE
        )
      }

      # Check if datetime already exists
      if (datetime %in% rv$reinstallations$datetime) {
        showNotification("Reinstallation date already exists", type = "warning")
        return()
      }

      # Check if datetime is after initial date
      if (!is.null(rv$initial_date) && datetime <= rv$initial_date) {
        showNotification("Reinstallation date must be after initial installation", type = "error")
        return()
      }

      # Add to reinstallations with measured diameter
      new_reinstall <- data.frame(
        datetime = datetime,
        measured_diameter_mm = input$reinstall_measured_diameter,
        stringsAsFactors = FALSE
      )

      # Use bind_rows for safety
      rv$reinstallations <- dplyr::bind_rows(rv$reinstallations, new_reinstall)
      rv$reinstallations <- rv$reinstallations[order(rv$reinstallations$datetime), ]

      showNotification("Reinstallation added successfully", type = "message")

      # Clear inputs
      updateDateInput(session, "reinstall_date", value = NULL)
      updateTextInput(session, "reinstall_time", value = "00:00")
      updateNumericInput(session, "reinstall_measured_diameter", value = NULL)
    })

    # Clear reinstallations
    observeEvent(input$clear_reinstallations, {
      rv$reinstallations <- data.frame(
        datetime = as.POSIXct(character()),
        measured_diameter_mm = numeric(),
        stringsAsFactors = FALSE
      )
      showNotification("All reinstallations cleared", type = "message")
    })

    # Render reinstallation list
    output$reinstallation_list <- renderUI({
      # Safety checks
      if (is.null(rv$reinstallations)) return(NULL)
      if (!is.data.frame(rv$reinstallations)) return(NULL)

      n_rows <- nrow(rv$reinstallations)
      if (is.null(n_rows) || length(n_rows) == 0 || n_rows == 0) {
        return(p(em("No reinstallations defined yet.")))
      }

      reinstall_items <- lapply(seq_len(n_rows), function(i) {
        reinstall <- rv$reinstallations[i, ]
        diameter_text <- if (!is.null(reinstall$measured_diameter_mm) && !is.na(reinstall$measured_diameter_mm)) {
          sprintf(" (%.1f mm)", reinstall$measured_diameter_mm)
        } else {
          ""
        }
        div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 5px; border-bottom: 1px solid #eee;",
          span(sprintf("%s%s", format(reinstall$datetime, "%Y-%m-%d %H:%M"), diameter_text)),
          actionButton(
            session$ns(paste0("remove_reinstall_", i)),
            label = NULL,
            icon = icon("times"),
            class = "btn-xs btn-danger",
            style = "padding: 2px 6px;"
          )
        )
      })

      tagList(reinstall_items)
    })

    # Handle individual reinstallation removal (dynamic)
    observe({
      # Safety checks
      if (is.null(rv$reinstallations)) return()
      if (!is.data.frame(rv$reinstallations)) return()

      n_rows <- nrow(rv$reinstallations)
      if (is.null(n_rows) || length(n_rows) == 0 || n_rows == 0) return()

      lapply(seq_len(n_rows), function(i) {
        observeEvent(input[[paste0("remove_reinstall_", i)]], {
          rv$reinstallations <- rv$reinstallations[-i, , drop = FALSE]
          showNotification("Reinstallation removed", type = "message")
        }, ignoreInit = TRUE)
      })
    })

    # Flag for reinstallations
    output$has_reinstallations <- reactive({
      if (is.null(rv$reinstallations) || !is.data.frame(rv$reinstallations)) {
        return(FALSE)
      }
      n_rows <- nrow(rv$reinstallations)
      !is.null(n_rows) && length(n_rows) > 0 && n_rows > 0
    })
    outputOptions(output, "has_reinstallations", suspendWhenHidden = FALSE)

    # Flag for temporal wound tracking (reinstallations OR final date/diameter defined)
    output$has_temporal_wound_tracking <- reactive({
      # Check if has reinstallations
      has_reinstalls <- if (is.null(rv$reinstallations) || !is.data.frame(rv$reinstallations)) {
        FALSE
      } else {
        n_rows <- nrow(rv$reinstallations)
        !is.null(n_rows) && length(n_rows) > 0 && n_rows > 0
      }

      # Check if has final measurements for Model A temporal tracking
      has_final_measurements <- !is.null(input$final_measurement_date) &&
                                !is.null(input$final_measurement_diameter)

      # Return TRUE if either condition is met
      has_reinstalls || has_final_measurements
    })
    outputOptions(output, "has_temporal_wound_tracking", suspendWhenHidden = FALSE)

    # Apply wound correction
    observeEvent(input$apply_wound_correction, {
      req(vh_data())
      req(wood_properties())

      withProgress(message = "Applying wound correction...", {

        # Update wood properties with temporal wound data
        wood <- wood_properties()

        # Set initial installation date
        wood$wound_correction$initial_date <- rv$initial_date

        # Use final date/diameter from UI inputs (or YAML defaults if inputs empty)
        # Prioritize UI inputs if provided
        if (!is.null(input$final_measurement_date)) {
          wood$wound_correction$final_date <- input$final_measurement_date
        }
        if (!is.null(input$final_measurement_diameter)) {
          wood$wound_correction$final_diameter_mm <- input$final_measurement_diameter
        }

        # Set reinstall dates and measured diameters if present
        if (!is.null(rv$reinstallations) && is.data.frame(rv$reinstallations) && nrow(rv$reinstallations) > 0) {
          wood$wound_correction$reinstall_dates <- rv$reinstallations$datetime
          # Pass measured diameters if available
          if ("measured_diameter_mm" %in% names(rv$reinstallations)) {
            wood$wound_correction$reinstall_measured_diameters <- rv$reinstallations$measured_diameter_mm
          }
        } else {
          wood$wound_correction$reinstall_dates <- NULL
          wood$wound_correction$reinstall_measured_diameters <- NULL
        }

        tryCatch({
          # Apply wound correction
          result <- sapfluxr::apply_wound_correction(
            vh_data = vh_data(),
            probe_spacing = input$probe_spacing,
            wood_properties = wood,
            use_spacing_corrected = TRUE,
            confirm_parameters = FALSE
          )

          rv$wound_correction_result <- result
          rv$wound_corrected_data <- result  # Direct assignment - apply_wound_correction returns data frame

          # Track wound correction
          if (!is.null(code_tracker)) {
            n_reinstalls <- 0
            if (!is.null(rv$reinstallations) && is.data.frame(rv$reinstallations)) {
              n_reinstalls <- nrow(rv$reinstallations)
            }

            code_tracker$add_step(
              step_name = "Apply Wound Correction",
              code = sprintf(
                'vh_corrected <- apply_wound_correction(
  vh_data = vh_data,
  probe_spacing = "%s",
  wood_properties = wood_properties
)',
                input$probe_spacing
              ),
              description = sprintf("Wound correction applied%s",
                                   if (n_reinstalls > 0)
                                     sprintf(" with %d reinstallation%s",
                                            n_reinstalls,
                                            if (n_reinstalls > 1) "s" else "")
                                   else "")
            )
          }

          showNotification("Wound correction applied successfully!", type = "message")

        }, error = function(e) {
          showNotification(paste("Error applying wound correction:", e$message),
                         type = "error", duration = 10)
        })
      })
    })

    # Reset wound correction
    observeEvent(input$reset_wound_correction, {
      rv$wound_corrected_data <- NULL
      rv$wound_correction_result <- NULL
      showNotification("Wound correction removed", type = "message")
    })

    # Flag for wound results
    output$has_wound_results <- reactive({
      !is.null(rv$wound_corrected_data)
    })
    outputOptions(output, "has_wound_results", suspendWhenHidden = FALSE)

    # Wound correction summary
    output$wound_correction_summary <- renderText({
      req(rv$wound_corrected_data)

      before <- vh_data()
      after <- rv$wound_corrected_data

      # Calculate statistics from the corrected data frame
      wound_range <- range(after$wound_diameter_cm, na.rm = TRUE)
      B_range <- range(after$wound_correction_factor, na.rm = TRUE)
      mean_correction <- mean(after$Vh_cm_hr - before$Vh_cm_hr, na.rm = TRUE)

      sprintf(
        paste0(
          "Wound Correction Applied

",
          "Data Points: %d
",
          "Probe Spacing: %s
",
          "Wound Diameter Range: %.2f - %.2f mm
",
          "Correction Coefficient Range: %.3f - %.3f
",
          "Mean Correction: %.2f cm/hr
"
        ),
        nrow(after),
        input$probe_spacing,
        wound_range[1] * 10, wound_range[2] * 10,
        B_range[1], B_range[2],
        mean_correction
      )
    })

    # Wound status
    output$wound_status <- renderText({
      req(rv$wound_corrected_data)

      n_reinstalls <- 0
      if (!is.null(rv$reinstallations) && is.data.frame(rv$reinstallations)) {
        n_reinstalls <- nrow(rv$reinstallations)
      }

      sprintf(
        paste0(
          "Wound-corrected data active (%d records)
",
          "Applied: %s
",
          "Reinstallations: %d"
        ),
        nrow(rv$wound_corrected_data),
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        n_reinstalls
      )
    })

    # Wound diameter timeline plot
    output$wound_diameter_plot <- plotly::renderPlotly({
      req(rv$initial_date)
      req(rv$initial_wound_mm)
      # Require at least one reinstallation OR a final date to show something meaningful
      # (If just initial date, it's a flat line, which is fine too)

      # Construct temporary wood config for calculation
      temp_wood <- list(
        drill_bit_diameter_mm = (rv$initial_wound_mm - 0.6), # Approximation: assume 0.6mm total wound
        wound_addition_mm = 0.3,
        initial_date = rv$initial_date,
        final_date = input$final_measurement_date, # Use UI input
        final_diameter_mm = input$final_measurement_diameter, # Use UI input
        reinstall_dates = if(!is.null(rv$reinstallations) && is.data.frame(rv$reinstallations) && nrow(rv$reinstallations) > 0) rv$reinstallations$datetime else NULL,
        reinstall_measured_diameters = if(!is.null(rv$reinstallations) && is.data.frame(rv$reinstallations) && nrow(rv$reinstallations) > 0 && "measured_diameter_mm" %in% names(rv$reinstallations)) rv$reinstallations$measured_diameter_mm else NULL
      )

      # If drill bit not known perfectly from rv (since we only store initial_wound),
      # try to get from wood_properties again for accuracy
      if (!is.null(wood_properties()) && inherits(wood_properties(), "WoodProperties")) {
        temp_wood$drill_bit_diameter_mm <- wood_properties()$wound_correction$drill_bit_diameter_mm
        temp_wood$wound_addition_mm <- wood_properties()$wound_correction$wound_addition_mm
      }

      # Determine timeline range
      start_time <- rv$initial_date
      end_time <- if (!is.null(input$final_measurement_date)) {
        as.POSIXct(input$final_measurement_date)
      } else {
        # Default to today or last reinstall
        max(Sys.time(), max(rv$reinstallations$datetime, na.rm=TRUE))
      }

      # Generate daily timestamps
      timestamps <- seq(from = start_time, to = end_time, by = "1 day")

      # Calculate diameters using package function
      # This ensures plot matches actual logic (sawtooth)
      # Pass as list structure mimicking WoodProperties wound_correction
      diameters_cm <- tryCatch({
        sapfluxr::calc_wound_diameter(timestamps, temp_wood)
      }, error = function(e) {
        showNotification(
          paste("Warning: Could not calculate wound diameter trajectory.",
                "Showing flat line at initial wound diameter.",
                "Error:", e$message),
          type = "warning",
          duration = 10
        )
        return(rep(rv$initial_wound_mm/10, length(timestamps)))
      })

      diameters_mm <- diameters_cm * 10

      # Create data frame for plotting
      timeline_df <- data.frame(
        datetime = timestamps,
        wound_diameter_mm = diameters_mm,
        stringsAsFactors = FALSE
      )

      # Plot
      fig <- plotly::plot_ly(
        data = timeline_df,
        x = ~datetime,
        y = ~wound_diameter_mm,
        type = "scatter",
        mode = "lines", # Just lines for the continuous growth
        name = "Wound Diameter",
        line = list(color = "darkred", width = 2),
        hovertemplate = paste(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>Wound Diameter:</b> %{y:.2f} mm<br>",
          "<extra></extra>"
        )
      )

      # Add markers for reinstallations
      if (!is.null(rv$reinstallations) && is.data.frame(rv$reinstallations) && nrow(rv$reinstallations) > 0) {
        # Show measured diameter at each reinstallation if available
        reinstall_y_values <- if ("measured_diameter_mm" %in% names(rv$reinstallations) &&
                                   !all(is.na(rv$reinstallations$measured_diameter_mm))) {
          rv$reinstallations$measured_diameter_mm
        } else {
          rep(rv$initial_wound_mm, nrow(rv$reinstallations))
        }

        fig <- fig %>% plotly::add_trace(
          x = rv$reinstallations$datetime,
          y = reinstall_y_values,
          type = "scatter",
          mode = "markers",
          name = "Reinstallation",
          marker = list(symbol = "x", size = 10, color = "black"),
          hovertemplate = paste(
            "<b>Reinstallation</b><br>",
            "<b>Date:</b> %{x|%Y-%m-%d}<br>",
            "<b>Diameter:</b> %{y:.2f} mm<br>",
            "<extra></extra>"
          )
        )
      }

      fig <- fig %>%
        plotly::layout(
          title = "Wound Diameter Over Time (Sawtooth Model)",
          xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "lightgray"),
          yaxis = list(title = "Wound Diameter (mm)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          margin = list(l = 60, r = 40, t = 60, b = 60),
          showlegend = TRUE
        )

      return(fig)
    })

    # Pre-filtered data for wound correction plot - returns both before and after datasets
    wound_plot_data <- reactive({
      req(rv$wound_corrected_data)
      req(vh_data())  # This is the "before" (input to wound correction)
      req(input$plot_sensor_position)

      sensor <- input$plot_sensor_position
      method <- "HRM"  # Wound correction is only applied to HRM

      # Before = spacing corrected data (input to wound correction)
      before <- vh_data()
      cat("\n=== WOUND PLOT DATA DEBUG ===\n")
      cat("vh_data() total rows:", nrow(before), "\n")
      cat("Unique sensors in vh_data():", paste(unique(before$sensor_position), collapse = ", "), "\n")
      cat("Unique methods in vh_data():", paste(unique(before$method), collapse = ", "), "\n")

      before <- before[before$sensor_position == sensor & before$method == method, ]
      cat("Before rows after sensor+method filter (", sensor, ",", method, "):", nrow(before), "\n")

      # After = wound corrected data (output of wound correction)
      after <- rv$wound_corrected_data
      cat("wound_corrected_data total rows:", nrow(after), "\n")
      cat("Unique sensors in wound_corrected_data:", paste(unique(after$sensor_position), collapse = ", "), "\n")
      cat("Unique methods in wound_corrected_data:", paste(unique(after$method), collapse = ", "), "\n")

      after <- after[after$sensor_position == sensor & after$method == method, ]
      cat("After rows after sensor+method filter (", sensor, ",", method, "):", nrow(after), "\n")

      # CRITICAL: Sort by datetime to ensure lines connect properly
      before <- before[order(before$datetime), ]
      after <- after[order(after$datetime), ]

      # Systematic sampling for performance while maintaining line continuity
      # Plotly gets slow with >5000 points, so downsample for visualisation
      max_plot_points <- 5000

      if (nrow(after) > max_plot_points) {
        sample_idx <- seq(1, nrow(after), length.out = max_plot_points)
        after <- after[sample_idx, ]
        cat("Downsampled after:", nrow(after), "rows (from original for plotting)\n")
      }

      if (nrow(before) > max_plot_points) {
        sample_idx <- seq(1, nrow(before), length.out = max_plot_points)
        before <- before[sample_idx, ]
        cat("Downsampled before:", nrow(before), "rows (from original for plotting)\n")
      }

      list(before = before, after = after)
    })

    # Before/after comparison plot - interactive plotly
    output$wound_correction_comparison <- plotly::renderPlotly({
      req(wound_plot_data())

      data_list <- wound_plot_data()
      before <- data_list$before
      after <- data_list$after
      sensor <- input$plot_sensor_position
      method <- "HRM"  # Wound correction is only applied to HRM

      # Debug output
      cat("\n=== WOUND CORRECTION PLOT DEBUG ===\n")
      cat("Before rows:", nrow(before), "\n")
      cat("After rows:", nrow(after), "\n")
      cat("Before columns:", paste(names(before), collapse = ", "), "\n")
      cat("After columns:", paste(names(after), collapse = ", "), "\n")
      cat("Sensor:", sensor, "\n")
      cat("Method:", method, "\n")
      cat("Show raw data:", isTRUE(input$show_raw_data), "\n")
      cat("Show spacing corrected:", isTRUE(input$show_spacing_corrected), "\n")

      if (nrow(after) == 0) {
        # Return empty plot if no data
        return(plotly::plot_ly() %>%
                 plotly::layout(
                   title = paste("No data for", sensor, "sensor"),
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Velocity (cm/hr)")
                 ))
      }

      # Determine which wound corrected column to use
      wc_col <- if ("Vh_cm_hr_wc" %in% names(after) &&
                    sum(!is.na(after$Vh_cm_hr_wc)) > 0) {
        "Vh_cm_hr_wc"
      } else if ("Vh_cm_hr_sc_wc" %in% names(after)) {
        "Vh_cm_hr_sc_wc"
      } else {
        cat("ERROR: No wound corrected velocity column found!\n")
        cat("Available columns in 'after':", paste(names(after), collapse = ", "), "\n")
        stop("No wound corrected velocity column found.")
      }

      cat("Using wound corrected column:", wc_col, "\n")
      cat("Non-NA values:", sum(!is.na(after[[wc_col]])), "\n")

      # Start with Wound Corrected line (always shown)
      p <- plotly::plot_ly(
        data = after,
        x = ~datetime,
        y = as.formula(paste0("~", wc_col)),
        type = "scatter",
        mode = "lines",
        name = "Wound Corrected",
        line = list(color = "darkgreen", width = 1.5),
        hovertemplate = paste(
          "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
          "<b>Wound Corrected:</b> %{y:.2f} cm/hr<br>",
          "<extra></extra>"
        )
      )

      # Add Raw Data overlay if requested
      if (isTRUE(input$show_raw_data)) {
        raw_col <- if ("Vh_cm_hr_raw" %in% names(before)) "Vh_cm_hr_raw" else "Vh_cm_hr"

        p <- p %>%
          plotly::add_trace(
            data = before,
            x = ~datetime,
            y = as.formula(paste0("~", raw_col)),
            type = "scatter",
            mode = "lines",
            name = "Raw Data",
            line = list(color = "red", width = 1),
            hovertemplate = paste(
              "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
              "<b>Raw:</b> %{y:.2f} cm/hr<br>",
              "<extra></extra>"
            )
          )
      }

      # Add Spacing Corrected overlay if requested
      if (isTRUE(input$show_spacing_corrected)) {
        sc_col <- if ("Vh_cm_hr_sc" %in% names(before)) "Vh_cm_hr_sc" else "Vh_cm_hr"

        p <- p %>%
          plotly::add_trace(
            data = before,
            x = ~datetime,
            y = as.formula(paste0("~", sc_col)),
            type = "scatter",
            mode = "lines",
            name = "Spacing Corrected",
            line = list(color = "steelblue", width = 1.2),
            hovertemplate = paste(
              "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
              "<b>Spacing Corrected:</b> %{y:.2f} cm/hr<br>",
              "<extra></extra>"
            )
          )
      }

      # Layout
      p <- p %>%
        plotly::layout(
          title = sprintf("Wound Correction: Before vs After (%s, %s)", toupper(sensor), method),
          xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "lightgray"),
          yaxis = list(title = "Velocity (cm/hr)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          legend = list(x = 0.02, y = 0.98),
          margin = list(l = 60, r = 40, t = 60, b = 60)
        )

      return(p)
    })

    # Wound coefficients table
    output$wound_coefficients_table <- renderText({
      req(rv$wound_corrected_data)

      data <- rv$wound_corrected_data

      # Sample if too many rows
      if (nrow(data) > 100) {
        sample_idx <- seq(1, nrow(data), length.out = 100)
        sample_data <- data[sample_idx, ]
      } else {
        sample_data <- data
      }

      # Build header
      table_text <- sprintf(
        "%-20s | %10s | %10s
%s
",
        "Datetime",
        "Wound (mm)",
        "Coeff (B)",
        paste(rep("-", 50), collapse = "")
      )

      # Build rows
      for (i in 1:nrow(sample_data)) {
        table_text <- paste0(
          table_text,
          sprintf(
            "%-20s | %10.2f | %10.3f
",
            format(sample_data$datetime[i], "%Y-%m-%d %H:%M"),
            sample_data$wound_diameter_cm[i] * 10,
            sample_data$wound_correction_factor[i]
          )
        )
      }

      return(table_text)
    })

    # Return values for downstream modules
    return(list(
      wound_corrected_data = reactive(rv$wound_corrected_data),
      has_wound_correction = reactive(!is.null(rv$wound_corrected_data)),
      reinstallations = reactive(rv$reinstallations)
    ))
  })
}
