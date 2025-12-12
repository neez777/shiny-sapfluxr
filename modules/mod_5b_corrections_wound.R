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

          helpText(
            icon("redo"),
            " Add reinstallation dates if the probe was removed and reinstalled.",
            " You must provide the wound diameter measured before reinstallation."
          ),

          fluidRow(
            column(6, dateInput(ns("reinstall_date"), "Reinstallation Date", value = NULL)),
            column(6, textInput(ns("reinstall_time"), "Time (HH:MM)", value = "00:00"))
          ),

          numericInput(
            ns("wound_diameter_mm"),
            HTML('Wound Diameter Before Reinstall (mm) <span style="color: #999; cursor: help;" title="Measured wound diameter just before probe reinstallation. This becomes the final diameter for the previous installation period. After reinstallation, diameter resets to initial size."><i class="fa fa-circle-question"></i></span>'),
            value = NULL,
            min = 1.5,
            max = 10,
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

      # Right column: Visualization and Results
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
            condition = sprintf("!output['%s']", ns("has_reinstallations")),
            p(em("No reinstallations defined. Using static initial wound diameter."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_reinstallations")),
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
                "Summary",
                br(),
                verbatimTextOutput(ns("wound_correction_summary"))
              ),

              tabPanel(
                "Before/After Comparison",
                br(),
                plotly::plotlyOutput(ns("wound_correction_comparison"), height = "500px")
              ),

              tabPanel(
                "Correction Coefficients",
                br(),
                helpText("Wound correction coefficients (B) applied over time."),
                verbatimTextOutput(ns("wound_coefficients_table"))
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
                                   probe_config = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
      reinstallations = data.frame(
        datetime = as.POSIXct(character()),
        wound_diameter_mm = numeric(),
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
        sprintf("Drill Bit: %.1f mm\nWound Addition: %.1f mm per side\nInitial Wound: %.2f mm",
                wc$drill_bit_diameter_mm,
                wc$wound_addition_mm,
                wc$drill_bit_diameter_mm + (2 * wc$wound_addition_mm))
      } else {
        "Wood properties not loaded or invalid format"
      }
    })

    # Add reinstallation
    observeEvent(input$add_reinstallation, {
      req(input$reinstall_date)
      req(input$reinstall_time)
      req(input$wound_diameter_mm)

      # Parse datetime
      datetime_str <- paste(input$reinstall_date, input$reinstall_time)
      datetime <- tryCatch({
        as.POSIXct(datetime_str, format = "%Y-%m-%d %H:%M")
      }, error = function(e) {
        showNotification("Invalid date/time format", type = "error")
        return(NULL)
      })

      req(datetime)

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

      # Add to reinstallations
      new_reinstall <- data.frame(
        datetime = datetime,
        wound_diameter_mm = input$wound_diameter_mm,
        stringsAsFactors = FALSE
      )

      rv$reinstallations <- rbind(rv$reinstallations, new_reinstall)
      rv$reinstallations <- rv$reinstallations[order(rv$reinstallations$datetime), ]

      showNotification("Reinstallation added successfully", type = "message")

      # Clear inputs
      updateDateInput(session, "reinstall_date", value = NULL)
      updateTextInput(session, "reinstall_time", value = "00:00")
      updateNumericInput(session, "wound_diameter_mm", value = NULL)
    })

    # Clear reinstallations
    observeEvent(input$clear_reinstallations, {
      rv$reinstallations <- rv$reinstallations[0, ]
      showNotification("All reinstallations cleared", type = "message")
    })

    # Render reinstallation list
    output$reinstallation_list <- renderUI({
      if (nrow(rv$reinstallations) == 0) {
        return(p(em("No reinstallations defined yet.")))
      }

      reinstall_items <- lapply(1:nrow(rv$reinstallations), function(i) {
        reinstall <- rv$reinstallations[i, ]
        div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 5px; border-bottom: 1px solid #eee;",
          span(sprintf("%s — Wound: %.2f mm",
                       format(reinstall$datetime, "%Y-%m-%d %H:%M"),
                       reinstall$wound_diameter_mm)),
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
      lapply(1:nrow(rv$reinstallations), function(i) {
        observeEvent(input[[paste0("remove_reinstall_", i)]], {
          rv$reinstallations <- rv$reinstallations[-i, ]
          showNotification("Reinstallation removed", type = "message")
        }, ignoreInit = TRUE)
      })
    })

    # Flag for reinstallations
    output$has_reinstallations <- reactive({
      nrow(rv$reinstallations) > 0
    })
    outputOptions(output, "has_reinstallations", suspendWhenHidden = FALSE)

    # Apply wound correction
    observeEvent(input$apply_wound_correction, {
      req(vh_data())
      req(wood_properties())

      withProgress(message = "Applying wound correction...", {

        # Update wood properties with temporal wound data
        wood <- wood_properties()

        # Set initial installation date
        wood$wound_correction$initial_date <- rv$initial_date

        # Use final date/diameter from either:
        # 1. User-entered reinstallations (last entry if multiple)
        # 2. Or values already in wood_properties YAML
        if (nrow(rv$reinstallations) > 0) {
          # Use the LAST reinstallation as final measurement
          # (sapfluxr currently supports one final_date/diameter for linear interpolation)
          last_reinstall <- rv$reinstallations[nrow(rv$reinstallations), ]
          wood$wound_correction$final_date <- last_reinstall$reinstall_datetime
          wood$wound_correction$final_diameter_mm <- last_reinstall$wound_diameter_mm
        }
        # else: use final_date/final_diameter_mm from YAML (if set)

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
          rv$wound_corrected_data <- result$vh_corrected

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
      req(rv$wound_correction_result)

      result <- rv$wound_correction_result

      sprintf(
        "Wound Correction Applied\n\n" +
        "Data Points: %d\n" +
        "Probe Spacing: %s\n" +
        "Wound Diameter Range: %.2f - %.2f mm\n" +
        "Correction Coefficient Range: %.3f - %.3f\n" +
        "Mean Correction: %.2f cm/hr\n",
        nrow(result$vh_corrected),
        input$probe_spacing,
        min(result$wound_diameter_cm * 10, na.rm = TRUE),
        max(result$wound_diameter_cm * 10, na.rm = TRUE),
        min(result$B, na.rm = TRUE),
        max(result$B, na.rm = TRUE),
        mean(result$vh_corrected$Vh_cm_hr - vh_data()$Vh_cm_hr, na.rm = TRUE)
      )
    })

    # Wound status
    output$wound_status <- renderText({
      req(rv$wound_corrected_data)

      sprintf(
        "Wound-corrected data active (%d records)\n" +
        "Applied: %s\n" +
        "Reinstallations: %d",
        nrow(rv$wound_corrected_data),
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        nrow(rv$reinstallations)
      )
    })

    # Wound diameter timeline plot
    output$wound_diameter_plot <- plotly::renderPlotly({
      req(rv$initial_date)
      req(rv$initial_wound_mm)
      req(nrow(rv$reinstallations) > 0)

      # Create timeline data
      dates <- c(rv$initial_date, rv$reinstallations$datetime)
      diameters <- c(rv$initial_wound_mm, rv$reinstallations$wound_diameter_mm)

      # Create data frame for plotting
      timeline_df <- data.frame(
        datetime = dates,
        wound_diameter_mm = diameters,
        stringsAsFactors = FALSE
      )

      # Plot
      fig <- plotly::plot_ly(
        data = timeline_df,
        x = ~datetime,
        y = ~wound_diameter_mm,
        type = "scatter",
        mode = "lines+markers",
        name = "Wound Diameter",
        line = list(color = "darkred", width = 2),
        marker = list(size = 8, color = "darkred"),
        hovertemplate = paste(
          "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
          "<b>Wound Diameter:</b> %{y:.2f} mm<br>",
          "<extra></extra>"
        )
      )

      fig <- fig %>%
        plotly::layout(
          title = "Wound Diameter Over Time",
          xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "lightgray"),
          yaxis = list(title = "Wound Diameter (mm)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          margin = list(l = 60, r = 40, t = 60, b = 60)
        )

      return(fig)
    })

    # Before/after comparison plot
    output$wound_correction_comparison <- plotly::renderPlotly({
      req(rv$wound_corrected_data)
      req(vh_data())

      # Prepare data
      before <- vh_data()
      after <- rv$wound_corrected_data

      # Merge by datetime for aligned comparison
      merged <- merge(before, after, by = "datetime", suffixes = c("_before", "_after"))

      # Sample if too many points (for performance)
      if (nrow(merged) > 5000) {
        sample_idx <- seq(1, nrow(merged), length.out = 5000)
        merged <- merged[sample_idx, ]
      }

      # Create plot
      fig <- plotly::plot_ly()

      fig <- fig %>%
        plotly::add_trace(
          data = merged,
          x = ~datetime,
          y = ~Vh_cm_hr_before,
          type = "scatter",
          mode = "lines",
          name = "Before Correction",
          line = list(color = "steelblue", width = 1.5),
          hovertemplate = paste(
            "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
            "<b>Before:</b> %{y:.3f} cm/hr<br>",
            "<extra></extra>"
          )
        )

      fig <- fig %>%
        plotly::add_trace(
          data = merged,
          x = ~datetime,
          y = ~Vh_cm_hr_after,
          type = "scatter",
          mode = "lines",
          name = "After Correction",
          line = list(color = "darkgreen", width = 1.5),
          hovertemplate = paste(
            "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
            "<b>After:</b> %{y:.3f} cm/hr<br>",
            "<extra></extra>"
          )
        )

      fig <- fig %>%
        plotly::layout(
          title = "Wound Correction: Before vs After",
          xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "lightgray"),
          yaxis = list(title = "Velocity (cm/hr)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          legend = list(x = 0.02, y = 0.98),
          margin = list(l = 60, r = 40, t = 60, b = 60)
        )

      return(fig)
    })

    # Wound coefficients table
    output$wound_coefficients_table <- renderText({
      req(rv$wound_correction_result)

      result <- rv$wound_correction_result

      # Sample coefficients over time (show every nth row to get ~100 samples)
      if (nrow(result) > 100) {
        sample_idx <- seq(1, nrow(result), length.out = 100)
        sample_data <- result[sample_idx, ]
      } else {
        sample_data <- result
      }

      # Format as table
      table_text <- sprintf(
        "%-20s | %10s | %10s\n%s\n",
        "Datetime",
        "Wound (mm)",
        "Coeff (B)",
        paste(rep("-", 50), collapse = "")
      )

      for (i in 1:nrow(sample_data)) {
        row_data <- sample_data[i, ]
        table_text <- paste0(
          table_text,
          sprintf(
            "%-20s | %10.2f | %10.3f\n",
            format(row_data$datetime, "%Y-%m-%d %H:%M"),
            row_data$wound_diameter_cm * 10,  # Convert cm to mm
            row_data$B
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
