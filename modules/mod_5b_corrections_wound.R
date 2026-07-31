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

        # Heartwood warning (dynamic - shown only if inner sensor is in heartwood)
        uiOutput(ns("heartwood_warning")),

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

          radioButtons(
            ns("wound_method"),
            "Correction Method:",
            choices = c("Linear" = "linear", "Polynomial" = "polynomial"),
            selected = "linear",
            inline = TRUE
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

          helpText("Visualise how wound diameter changes over time based on initial installation and reinstallation dates."),

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_temporal_wound_tracking")),
            p(em("No temporal wound tracking defined. Set final measurement date and diameter to see wound growth over time."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_temporal_wound_tracking")),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("wound_diameter_plot"), height = "400px"),
              type = 6,
              color = "#3c8dbc"
            )
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
                      "Show Raw Data Overlay",
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

                fluidRow(
                  column(4,
                    shinyWidgets::airDatepickerInput(
                      ns("wc_start_datetime"),
                      "Start Date/Time:",
                      value = NULL,
                      timepicker = TRUE,
                      dateFormat = "yyyy-MM-dd HH:mm"
                    )
                  ),
                  column(4,
                    shinyWidgets::airDatepickerInput(
                      ns("wc_end_datetime"),
                      "End Date/Time:",
                      value = NULL,
                      timepicker = TRUE,
                      dateFormat = "yyyy-MM-dd HH:mm"
                    )
                  ),
                  column(2,
                    br(),
                    actionButton(
                      ns("wc_apply_range"),
                      "Apply",
                      icon = icon("clock"),
                      class = "btn-primary btn-sm",
                      style = "width: 100%; margin-top: 8px;"
                    )
                  ),
                  column(2,
                    br(),
                    actionButton(
                      ns("wc_reset_zoom"),
                      "Reset",
                      icon = icon("refresh"),
                      class = "btn-default btn-sm",
                      style = "width: 100%; margin-top: 8px;"
                    )
                  )
                ),

                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("wound_correction_comparison"), height = "500px"),
                  type = 6,
                  color = "#3c8dbc"
                )
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
                                   code_tracker = NULL,
                                   plot_settings = reactive(NULL)) {
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

    # TRUE only while the comparison plot holds its Raw and Spacing Corrected
    # overlay traces, so the visibility toggle never addresses a trace index the
    # current render did not create.
    overlays_rendered <- reactiveVal(FALSE)

    # Heartwood warning output
    output$heartwood_warning <- renderUI({
      req(wood_properties(), probe_config())

      # Validate probe/tree configuration to get tissue information
      validation <- validate_probe_tree_config(
        probe_config = probe_config(),
        wood_properties = wood_properties()
      )

      # Check if inner sensor is in heartwood
      if (!is.null(validation$inner_tissue) && validation$inner_tissue == "heartwood") {
        box(
          width = 12,
          title = NULL,
          status = "warning",
          solidHeader = FALSE,

          div(
            style = "padding: 10px;",
            p(
              icon("exclamation-triangle", class = "fa-lg"),
              strong(" Attention:"),
              " Based on your sapwood depth and probe geometry, the",
              strong(" Inner Sensor"),
              " is located in the heartwood."
            ),
            tags$ul(
              tags$li("Heartwood records zero flow and is not active in sap transport"),
              tags$li("Inner sensor data does not require wound correction"),
              tags$li("Can be used as a continuous zero-flow reference")
            )
          )
        )
      } else {
        NULL  # No warning if not in heartwood
      }
    })

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
          # Derive the wound coefficient table (5mm/6mm) from the configured probe
          # (Tab 2) rather than a separate dropdown. Probe spacing is in cm.
          pc <- probe_config()
          spacing_cm <- tryCatch(
            if (!is.null(pc)) pc$probe_spacing else NULL,
            error = function(e) NULL
          )
          wound_table <- if (!is.null(spacing_cm) && !is.na(spacing_cm) &&
                             round(spacing_cm * 10) >= 6) "6mm" else "5mm"

          # Apply wound correction
          result <- sapfluxr::apply_wound_correction(
            vh_data = vh_data(),
            probe_spacing = wound_table,
            method = input$wound_method,
            wood_properties = wood,
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
  method = "%s",
  wood_properties = wood_properties
)',
                wound_table,
                input$wound_method
              ),
              description = sprintf("Wound correction applied (%s method)%s",
                                   input$wound_method,
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

      # Probe-spacing label derived from the configured probe (5mm/6mm table)
      probe_label <- {
        pc <- probe_config()
        sc <- tryCatch(if (!is.null(pc)) pc$probe_spacing else NULL, error = function(e) NULL)
        if (!is.null(sc) && !is.na(sc) && round(sc * 10) >= 6) "6mm" else "5mm"
      }

      sprintf(
        paste0(
          "Wound Correction Applied\n",
          strrep("-", 40), "\n",
          "Data Points: %d\n",
          "Method: %s\n",
          "Probe Spacing: %s\n",
          "Wound Diameter Range: %.2f - %.2f mm\n",
          "Correction Coefficient Range: %.3f - %.3f\n",
          "Mean Correction: %.2f cm/hr\n"
        ),
        nrow(after),
        input$wound_method,
        probe_label,
        wound_range[1] * 10, wound_range[2] * 10,
        B_range[1], B_range[2],
        mean_correction
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

      # Generate a sequence of points for a crisp sawtooth geometry
      # We need points at start/end and at EVERY reinstallation moment (Peak and Base)
      
      reinstalls <- if(!is.null(rv$reinstallations) && is.data.frame(rv$reinstallations) && nrow(rv$reinstallations) > 0) rv$reinstallations$datetime else NULL
      
      # 1. Start with daily timestamps for the growth lines
      daily_ts <- seq(from = start_time, to = end_time, by = "1 day")
      
      # 2. Add reinstallation moments (TWICE: just before and just after)
      # T-1s captures the peak reached just before removal
      # T+1s captures the reset to base size
      reinstall_peaks <- if(!is.null(reinstalls)) reinstalls - 1 else as.POSIXct(character())
      reinstall_bases <- if(!is.null(reinstalls)) reinstalls + 1 else as.POSIXct(character())
      
      # 3. Combine and sort
      timestamps_list <- list(start_time, daily_ts, reinstall_peaks, reinstall_bases, end_time)
      # Ensure all are valid by removing NULLs or empty vectors if any, though do.call("c") handles empty well
      timestamps <- do.call("c", timestamps_list)
      timestamps <- sort(unique(timestamps))

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
      
      # IMPORTANT: Ensure strictly sorted to avoid ghost lines crossing back/forth
      timeline_df <- timeline_df[order(timeline_df$datetime), ]
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
          inherit = FALSE,
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

      # Apply standard layout
      base_layout <- get_standard_layout(
        title = "Wound Diameter Over Time",
        xtitle = "Date",
        ytitle = "Wound Diameter (mm)",
        uirevision = "wound_diameter_zoom"
      )
      
      # Tweak legend and margin to avoid overlap with x-axis title
      base_layout$legend$y <- -0.25
      base_layout$margin$b <- 120
      base_layout$yaxis$fixedrange <- FALSE

      fig <- fig %>%
        plotly::layout(
          title = base_layout$title,
          xaxis = base_layout$xaxis,
          yaxis = base_layout$yaxis,
          legend = base_layout$legend,
          hovermode = base_layout$hovermode,
          margin = base_layout$margin,
          uirevision = base_layout$uirevision,
          plot_bgcolor = base_layout$plot_bgcolor,
          paper_bgcolor = base_layout$paper_bgcolor,
          showlegend = TRUE
        ) %>%
        apply_standard_plotly_config(filename = "wound_diameter_plot", add_csv_download = TRUE)

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
      # Plotly gets slow with >30000 points, so downsample for visualisation
      max_plot_points <- 30000

      # Integer indices -- seq() with length.out returns doubles, which tibbles
      # reject when subsetting rows.
      if (nrow(after) > max_plot_points) {
        sample_idx <- unique(as.integer(round(seq(1, nrow(after), length.out = max_plot_points))))
        after <- after[sample_idx, ]
        cat("Downsampled after:", nrow(after), "rows (from original for plotting)\n")
      }

      if (nrow(before) > max_plot_points) {
        sample_idx <- unique(as.integer(round(seq(1, nrow(before), length.out = max_plot_points))))
        before <- before[sample_idx, ]
        cat("Downsampled before:", nrow(before), "rows (from original for plotting)\n")
      }

      list(before = before, after = after)
    })

    # Before/after comparison plot - interactive plotly
    output$wound_correction_comparison <- plotly::renderPlotly({
      req(wound_plot_data())

      # Cleared until the full plot below is built, so the toggle observer never
      # restyles traces that this render did not create.
      overlays_rendered(FALSE)

      data_list <- wound_plot_data()
      before <- data_list$before
      after <- data_list$after
      sensor <- input$plot_sensor_position
      method <- "HRM"  # Wound correction is only applied to HRM
      
      if (nrow(after) == 0) {
        # Return empty plot if no data
        return(plotly::plot_ly(source = "wound_comparison") %>%
                 plotly::layout(
                   title = paste("No data for", sensor, "sensor"),
                   xaxis = list(title = "Date"),
                   yaxis = list(title = "Velocity (cm/hr)", fixedrange = TRUE),
                   uirevision = "wound_comparison_zoom"
                 ) %>%
                 plotly::event_register("plotly_relayout"))
      }

      # Determine which wound corrected column to use
      wc_col <- if ("Vh_cm_hr_wc" %in% names(after) &&
                    sum(!is.na(after$Vh_cm_hr_wc)) > 0) {
        "Vh_cm_hr_wc"
      } else if ("Vh_cm_hr_sc_wc" %in% names(after)) {
        "Vh_cm_hr_sc_wc"
      } else {
        stop("No wound corrected velocity column found.")
      }

      # Get styling config
      style_config <- plot_settings()
      
      # Start with Wound Corrected line (always shown as base layer)
      p <- plotly::plot_ly(
        source = "wound_comparison",
        data = after,
        x = ~datetime,
        y = as.formula(paste0("~", wc_col)),
        type = "scatter",
        mode = "lines",
        name = "Wound Corrected",
        line = list(color = "#2ca02c", width = 1.5),
        hovertemplate = paste(
          "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
          "<b>Wound Corrected:</b> %{y:.2f} cm/hr<br>",
          "<extra></extra>"
        )
      )

      # Apply standard layout
      base_layout <- get_standard_layout(
        title = sprintf("Wound Correction: Before vs After (%s, %s)", toupper(sensor), method),
        xtitle = "Date",
        ytitle = "Velocity (cm/hr)",
        uirevision = "wound_comparison_zoom"
      )
      
      # Both overlays are rendered up-front, as traces 1 and 2, hidden unless
      # ticked. They must always exist: removing a trace index that is not
      # present raises a plotly.js error in the browser which cannot be caught in
      # R and stalls Shiny's client message queue for the rest of the session.
      # The observer below therefore restyles them rather than adding and
      # removing them. isolate() keeps the checkboxes from forcing a re-render.
      sc_col <- if ("Vh_cm_hr_sc" %in% names(before)) "Vh_cm_hr_sc" else "Vh_cm_hr"

      p <- p %>%
        plotly::add_trace(
          data = before,
          x = ~datetime,
          y = ~Vh_cm_hr,
          type = "scatter",
          mode = "lines",
          name = "Raw Data",
          line = list(color = "#d62728", width = 1.0),
          visible = isTRUE(shiny::isolate(input$show_raw_data)),
          hovertemplate = paste(
            "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
            "<b>Raw:</b> %{y:.2f} cm/hr<br>",
            "<extra></extra>"
          )
        ) %>%
        plotly::add_trace(
          data = before,
          x = ~datetime,
          y = as.formula(paste0("~", sc_col)),
          type = "scatter",
          mode = "lines",
          name = "Spacing Corrected",
          line = list(color = "#ff7f0e", width = 1.2),
          visible = isTRUE(shiny::isolate(input$show_spacing_corrected)),
          hovertemplate = paste(
            "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
            "<b>Spacing Corrected:</b> %{y:.2f} cm/hr<br>",
            "<extra></extra>"
          )
        )

      p <- p %>%
        plotly::layout(
          title = base_layout$title,
          xaxis = base_layout$xaxis,
          yaxis = base_layout$yaxis,
          showlegend = TRUE,
          legend = base_layout$legend,
          hovermode = base_layout$hovermode,
          uirevision = base_layout$uirevision,
          plot_bgcolor = base_layout$plot_bgcolor,
          paper_bgcolor = base_layout$paper_bgcolor,
          margin = base_layout$margin
        ) %>%
        apply_standard_plotly_config(filename = "wound_correction_comparison", add_csv_download = TRUE) %>%
        plotly::event_register("plotly_relayout")

      # Only now do the overlay traces exist client-side.
      overlays_rendered(TRUE)

      return(p)
    })

    # Toggle the Raw and Spacing Corrected overlays via restyle, which preserves
    # zoom. Traces 1 and 2 are created by the renderer above, so this cannot raise
    # the invalid-index error that add/remove by index used to cause. A sensor or
    # data change re-renders the plot and sets visibility there, so neither is
    # bound here.
    observe({
      # The renderer returns a trace-less placeholder when there is no data for
      # the selected sensor; restyling then would target traces that do not exist.
      if (!isTRUE(isolate(overlays_rendered()))) return()

      plotly::plotlyProxy("wound_correction_comparison", session) %>%
        plotly::plotlyProxyInvoke(
          "restyle", list(visible = isTRUE(input$show_raw_data)), list(1)
        )

      plotly::plotlyProxy("wound_correction_comparison", session) %>%
        plotly::plotlyProxyInvoke(
          "restyle", list(visible = isTRUE(input$show_spacing_corrected)), list(2)
        )
    }) %>% bindEvent(input$show_raw_data, input$show_spacing_corrected, ignoreInit = TRUE)

    # Wound coefficients table
    output$wound_coefficients_table <- renderText({
      req(rv$wound_corrected_data)

      data <- rv$wound_corrected_data

      # Sample if too many rows. Integer indices -- seq() with length.out returns
      # doubles, which tibbles reject when subsetting rows.
      if (nrow(data) > 100) {
        sample_idx <- unique(as.integer(round(seq(1, nrow(data), length.out = 100))))
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

    # Initialise date/time range inputs when wound corrected data becomes available
    observe({
      req(rv$wound_corrected_data)
      date_range <- range(rv$wound_corrected_data$datetime, na.rm = TRUE)
      if (is.null(input$wc_start_datetime)) {
        shinyWidgets::updateAirDateInput(session, "wc_start_datetime", value = date_range[1])
      }
      if (is.null(input$wc_end_datetime)) {
        shinyWidgets::updateAirDateInput(session, "wc_end_datetime", value = date_range[2])
      }
    })

    # Update datetime inputs when user zooms the wound comparison plot
    wc_relayout_debounced <- debounce(reactive({
      event_data("plotly_relayout", source = "wound_comparison")
    }), 500)

    observeEvent(wc_relayout_debounced(), {
      rd <- wc_relayout_debounced()
      if (is.null(rd)) return()

      if (!is.null(rd$xaxis.range) && length(rd$xaxis.range) == 2) {
        shinyWidgets::updateAirDateInput(session, "wc_start_datetime",
          value = as.POSIXct(rd$xaxis.range[1], tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "wc_end_datetime",
          value = as.POSIXct(rd$xaxis.range[2], tz = "UTC"))
      } else if (!is.null(rd$`xaxis.range[0]`)) {
        shinyWidgets::updateAirDateInput(session, "wc_start_datetime",
          value = as.POSIXct(rd$`xaxis.range[0]`, tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "wc_end_datetime",
          value = as.POSIXct(rd$`xaxis.range[1]`, tz = "UTC"))
      } else if (isTRUE(rd$`xaxis.autorange`)) {
        req(rv$wound_corrected_data)
        date_range <- range(rv$wound_corrected_data$datetime, na.rm = TRUE)
        shinyWidgets::updateAirDateInput(session, "wc_start_datetime", value = date_range[1])
        shinyWidgets::updateAirDateInput(session, "wc_end_datetime", value = date_range[2])
      }
    })

    # Apply user-entered time range to wound comparison plot
    observeEvent(input$wc_apply_range, {
      req(input$wc_start_datetime, input$wc_end_datetime)
      plotly::plotlyProxy("wound_correction_comparison", session) %>%
        plotly::plotlyProxyInvoke("relayout", list(
          "xaxis.range" = list(
            format(input$wc_start_datetime, "%Y-%m-%d %H:%M:%S"),
            format(input$wc_end_datetime, "%Y-%m-%d %H:%M:%S")
          )
        ))
    })

    # Reset wound comparison plot to full range
    observeEvent(input$wc_reset_zoom, {
      req(rv$wound_corrected_data)
      date_range <- range(rv$wound_corrected_data$datetime, na.rm = TRUE)
      shinyWidgets::updateAirDateInput(session, "wc_start_datetime", value = date_range[1])
      shinyWidgets::updateAirDateInput(session, "wc_end_datetime", value = date_range[2])
      plotly::plotlyProxy("wound_correction_comparison", session) %>%
        plotly::plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE))
    })

    # Return values for downstream modules
    return(list(
      wound_corrected_data = reactive(rv$wound_corrected_data),
      has_wound_correction = reactive(!is.null(rv$wound_corrected_data)),
      reinstallations = reactive(rv$reinstallations)
    ))
  })
}
