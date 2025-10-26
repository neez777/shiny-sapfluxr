#' Method Selection and Calculation Module
#'
#' Shiny module for selecting HPV calculation methods and running calculations
#'
#' @param id Module ID
#' @param heat_pulse_data Reactive containing corrected heat_pulse_data
#' @param probe_config Reactive containing probe configuration
#' @param wood_properties Reactive containing wood properties
#' @return Reactive containing vh_results
#'

# UI ----
methodsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Method Selection
      column(
        width = 6,
        box(
          width = NULL,
          title = "Select Calculation Methods",
          status = "primary",
          solidHeader = TRUE,

          p("Select one or more heat pulse velocity calculation methods:"),

          checkboxGroupInput(
            ns("methods"),
            NULL,
            choices = c(
              "HRM - Heat Ratio Method (low/reverse flows)" = "HRM",
              "MHR - Maximum Heat Ratio (moderate to high flows)" = "MHR",
              "HRMXa - Modified HRM variant A" = "HRMXa",
              "HRMXb - Modified HRM variant B" = "HRMXb",
              "Tmax (Cohen) - Time-to-peak method" = "Tmax_Coh",
              "Tmax (Kluitenberg) - Time-to-peak method" = "Tmax_Klu"
            ),
            selected = c("HRM", "MHR")
          ),

          hr(),

          h5("Post-Processing Options"),
          checkboxInput(
            ns("apply_sdma"),
            "Apply sDMA (Selectable Dual Method Approach)",
            value = FALSE
          ),

          conditionalPanel(
            condition = sprintf("input['%s']", ns("apply_sdma")),
            p(class = "help-text",
              icon("info-circle"),
              " sDMA requires HRM and at least one other method. It automatically switches between HRM (low flows) and your selected secondary methods (high flows) based on Péclet number."),

            checkboxGroupInput(
              ns("sdma_secondary"),
              "Secondary Methods for sDMA:",
              choices = c(
                "MHR" = "MHR",
                "Tmax (Cohen)" = "Tmax_Coh",
                "Tmax (Kluitenberg)" = "Tmax_Klu",
                "HRMXa" = "HRMXa",
                "HRMXb" = "HRMXb"
              ),
              selected = "MHR"
            )
          ),

          # Warning if HRM not selected but sDMA is
          uiOutput(ns("sdma_warning"))
        )
      ),

      # Calculate Button & Status
      column(
        width = 6,
        box(
          width = NULL,
          title = "Run Calculations",
          status = "success",
          solidHeader = TRUE,

          uiOutput(ns("calculation_status")),

          hr(),

          actionButton(
            ns("calculate"),
            "Calculate Heat Pulse Velocity",
            icon = icon("play-circle"),
            class = "btn-success btn-lg",
            style = "width: 100%;"
          ),

          hr(),

          uiOutput(ns("results_summary"))
        )
      )
    ),

    # Results Table
    fluidRow(
      box(
        width = 12,
        title = "Calculation Results",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        DT::dataTableOutput(ns("results_table"))
      )
    )
  )
}

# Server ----
methodsServer <- function(id, heat_pulse_data, probe_config, wood_properties) {
  moduleServer(id, function(input, output, session) {

    # Reactive to store results
    vh_results <- reactiveVal(NULL)

    # Check if HRM is selected when sDMA is enabled
    output$sdma_warning <- renderUI({
      if (input$apply_sdma && !"HRM" %in% input$methods) {
        div(
          style = "padding: 10px; background-color: #FFF3CD; border-left: 4px solid #FF9800; margin-top: 10px;",
          p(
            icon("exclamation-triangle"),
            strong(" Warning:"),
            " sDMA requires HRM to be selected. Please check HRM above."
          )
        )
      }
    })

    # Calculation status display
    output$calculation_status <- renderUI({
      req(heat_pulse_data())

      data <- heat_pulse_data()
      n_pulses <- data$metadata$n_pulses

      div(
        p(strong("Ready to calculate")),
        tags$ul(
          tags$li(paste("Data loaded:", data$metadata$file_name)),
          tags$li(paste("Pulses:", format(n_pulses, big.mark = ","))),
          tags$li(paste("Probe:", if (!is.null(probe_config())) probe_config()$config_name else "Default")),
          tags$li(paste("Wood:", if (!is.null(wood_properties())) wood_properties()$config_name else "Default"))
        )
      )
    })

    # Calculate button action
    observeEvent(input$calculate, {
      req(heat_pulse_data())
      req(length(input$methods) > 0)

      # Validate sDMA requirements
      if (input$apply_sdma && !"HRM" %in% input$methods) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "sDMA Validation Error",
          text = "sDMA requires HRM to be selected. Please check HRM in the methods above.",
          type = "warning",
          timer = 3000
        )
        return()
      }

      tryCatch({
        # Get data and configs
        data <- heat_pulse_data()
        probe <- probe_config()
        wood <- wood_properties()

        # Calculate expected steps
        n_methods <- length(input$methods)
        n_pulses <- data$metadata$n_pulses
        total_steps <- n_pulses * n_methods
        if (input$apply_sdma) total_steps <- total_steps + n_pulses

        # Show progress bar
        shinyWidgets::progressSweetAlert(
          session = session,
          id = "calc_progress",
          title = "Calculating Heat Pulse Velocities",
          display_pct = TRUE,
          value = 0
        )

        # Call sapfluxr calculation
        # Suppress progressr to prevent blue toast notifications
        results <- progressr::without_progress({
          sapfluxr::calc_heat_pulse_velocity(
            heat_pulse_data = data,
            methods = input$methods,
            probe_config = probe,
            wood_properties = wood,
            confirm_parameters = FALSE  # Don't ask for confirmation in Shiny
          )
        })

        # Update progress to 80% after main calculation
        shinyWidgets::updateProgressBar(
          session = session,
          id = "calc_progress",
          value = 80,
          title = "Processing Results..."
        )

        # Apply sDMA if requested
        if (input$apply_sdma) {
          shinyWidgets::updateProgressBar(
            session = session,
            id = "calc_progress",
            value = 85,
            title = "Applying sDMA Processing..."
          )

          # Suppress progressr for sDMA too
          results <- progressr::without_progress({
            sapfluxr::apply_sdma_processing(
              vh_results = results,
              secondary_method = input$sdma_secondary,
              show_progress = FALSE  # Don't show console progress
            )
          })
        }

        # Complete progress
        shinyWidgets::updateProgressBar(
          session = session,
          id = "calc_progress",
          value = 100,
          title = "Complete!"
        )

        Sys.sleep(0.5)  # Brief pause to show 100%

        # Close progress bar
        shinyWidgets::closeSweetAlert(session = session)

        # Store results
        vh_results(results)

        # Show success with auto-close using shinyWidgets
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Success!",
          text = paste(format(nrow(results), big.mark = ","),
                      "velocity measurements calculated"),
          type = "success",
          timer = 3000  # Auto-close after 3 seconds
        )

      }, error = function(e) {
        # Close progress bar/alert
        shinyWidgets::closeSweetAlert(session = session)

        # Show error
        notify_error(
          session = session,
          title = "Calculation Error",
          text = e$message
        )
      })
    })

    # Results summary
    output$results_summary <- renderUI({
      results <- vh_results()
      if (is.null(results)) {
        return(p("No results yet. Click Calculate to run.", style = "color: #999;"))
      }

      # Summary stats
      n_measurements <- nrow(results)
      methods_used <- unique(results$method)
      n_methods <- length(methods_used)

      div(
        style = "background-color: #E8F5E9; padding: 10px; border-radius: 3px;",
        p(strong("Calculation Complete!", style = "color: #4CAF50;")),
        tags$ul(
          tags$li(paste("Total measurements:", format(n_measurements, big.mark = ","))),
          tags$li(paste("Methods calculated:", paste(methods_used, collapse = ", "))),
          tags$li(paste("Date range:",
                       format(min(results$datetime, na.rm = TRUE), "%Y-%m-%d"),
                       "to",
                       format(max(results$datetime, na.rm = TRUE), "%Y-%m-%d")))
        ),
        p(
          icon("arrow-down"),
          " View detailed results in the table below, or proceed to ",
          strong("5. Visualise"),
          " to create plots."
        )
      )
    })

    # Results table
    output$results_table <- DT::renderDataTable({
      results <- vh_results()
      req(results)

      # Format for display
      display_results <- results
      display_results$datetime <- format(display_results$datetime, "%Y-%m-%d %H:%M:%S")
      display_results$Vh_cm_hr <- round(display_results$Vh_cm_hr, 2)

      if ("peclet_number" %in% names(display_results)) {
        display_results$peclet_number <- round(display_results$peclet_number, 3)
      }

      DT::datatable(
        display_results,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(0, 'asc'))  # Sort by datetime
        ),
        rownames = FALSE
      )
    })

    # Return results reactive
    return(vh_results)
  })
}
