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

        # Calculate total progress steps
        n_pulses <- data$metadata$n_pulses
        n_methods <- length(input$methods)
        total_steps <- n_pulses * n_methods

        # Use Shiny's native withProgress for reliable progress reporting
        results <- NULL
        shiny::withProgress(message = "Calculating Heat Pulse Velocities", value = 0, {

          # Set up progressr handler to update Shiny progress
          progressr::handlers(progressr::handler_shiny(enable = TRUE))

          # Wrap in progressr context for compatibility
          results <- progressr::with_progress({
            sapfluxr::calc_heat_pulse_velocity(
              heat_pulse_data = data,
              methods = input$methods,
              probe_config = probe,
              wood_properties = wood,
              confirm_parameters = FALSE,
              show_progress = TRUE
            )
          })
        })

        # Check if sDMA should be applied
        should_prompt_sdma <- FALSE
        if (input$apply_sdma) {
          # Check Peclet numbers before running sDMA
          hrm_data <- results[results$method == "HRM", ]
          max_peclet <- max(hrm_data$peclet_number, na.rm = TRUE)

          # If all Peclet numbers are <= 1, prompt user
          if (!is.na(max_peclet) && max_peclet <= 1.0) {
            should_prompt_sdma <- TRUE

            # Show confirmation dialog
            shinyWidgets::confirmSweetAlert(
              session = session,
              inputId = "confirm_sdma",
              title = "sDMA Not Required",
              text = paste0(
                "All Peclet numbers are ≤ 1.0 (maximum: ", round(max_peclet, 3), ").\n\n",
                "This means HRM is valid for all measurements and sDMA would never ",
                "switch to the secondary method. The results would be identical to HRM.\n\n",
                "Do you still want to calculate sDMA?"
              ),
              type = "warning",
              btn_labels = c("Skip sDMA", "Calculate Anyway"),
              btn_colors = c("#3085d6", "#d33")
            )

            # Wait for user response
            observeEvent(input$confirm_sdma, {
              tryCatch({
                if (input$confirm_sdma) {
                  # User chose to calculate anyway
                  results_temp <- NULL
                  shiny::withProgress(message = "Applying sDMA Processing", value = 0, {
                    progressr::handlers(progressr::handler_shiny(enable = TRUE))
                    results_temp <- progressr::with_progress({
                      sapfluxr::apply_sdma_processing(
                        vh_results = results,
                        secondary_method = input$sdma_secondary,
                        skip_low_peclet = FALSE,  # User confirmed
                        show_progress = TRUE
                      )
                    })
                  })

                  # Finalize with sDMA results
                  finalize_results(results_temp)
                } else {
                  # User chose to skip - proceed with quality control on original results
                  # Apply quality control
                  qc_results <- tryCatch({
                    sapfluxr::flag_vh_quality(
                      results,
                      verbose = FALSE,
                      return_full_report = FALSE
                    )
                  }, error = function(e) {
                    # If flag_vh_quality fails, just return original results
                    message("Quality control failed: ", e$message)
                    results
                  })

                  # Store results
                  vh_results(qc_results)

                  # Show success
                  shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Success!",
                    text = paste(format(nrow(qc_results), big.mark = ","),
                                "velocity measurements calculated (sDMA skipped)"),
                    type = "success",
                    timer = 3000
                  )
                }
              }, error = function(e) {
                # Show error
                notify_error(
                  session = session,
                  title = "Processing Error",
                  text = paste("Error during processing:", e$message)
                )
              })
            }, once = TRUE)

            return()  # Exit early to wait for confirmation
          } else {
            # Peclet numbers > 1 exist, proceed normally
            shiny::withProgress(message = "Applying sDMA Processing", value = 0, {
              progressr::handlers(progressr::handler_shiny(enable = TRUE))
              results <- progressr::with_progress({
                sapfluxr::apply_sdma_processing(
                  vh_results = results,
                  secondary_method = input$sdma_secondary,
                  skip_low_peclet = FALSE,  # Auto-skip not needed
                  show_progress = TRUE
                )
              })
            })
          }
        }

        # Define quality control and finalization function
        finalize_results <- function(final_results) {
          # Apply quality control
          final_results <- tryCatch({
            sapfluxr::flag_vh_quality(
              final_results,
              verbose = FALSE,
              return_full_report = FALSE  # Just get data frame, not full report
            )
          }, error = function(e) {
            message("Quality control failed: ", e$message)
            final_results
          })

          # Store results
          vh_results(final_results)

          # Show success with auto-close using shinyWidgets
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Success!",
            text = paste(format(nrow(final_results), big.mark = ","),
                        "velocity measurements calculated"),
            type = "success",
            timer = 3000  # Auto-close after 3 seconds
          )
        }

        # If sDMA prompt is needed, handle it asynchronously
        if (should_prompt_sdma) {
          # Handler for confirm dialog is set up above
          # Results will be finalized in the observe() block
        } else if (input$apply_sdma) {
          # Peclet numbers > 1 exist, proceed with sDMA normally
          # (this code was already in the else block above)
          # Then finalize
          finalize_results(results)
        } else {
          # No sDMA requested, proceed directly to finalization
          finalize_results(results)
        }

      }, error = function(e) {
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
