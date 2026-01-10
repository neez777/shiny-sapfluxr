# mod_8_flux_density.R
# Module for Sap Flux Density Conversion
#
# Tab 8: Flux Density
# Converts corrected heat pulse velocity to sap flux density using
# wood-specific conversion factors (sapwood area integration)

# UI ----
fluxDensityUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Configuration
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "About Sap Flux Density Conversion",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p("Sap flux density (Jv) represents the volume of sap flowing through a unit area of sapwood per unit time."),
          tags$ul(
            tags$li(strong("Formula:"), " Jv = Z × Vh"),
            tags$li(strong("Z factor:"), " Wood-specific conversion factor calculated from wood properties"),
            tags$li(strong("Units:"), " cm³/cm²/hr (equivalent to cm/hr sap velocity)"),
            tags$li(strong("Sensors:"), " Conversion applied to all sensor positions (inner + outer) for radial integration")
          ),
          p(tags$small(em("Based on Burgess et al. (2001) after Barrett et al. (1995)"))),

          hr(),

          h5("Recommended Workflow:"),
          tags$ol(
            tags$li("Calculate raw heat pulse velocity (Vh)"),
            tags$li("Apply spacing correction"),
            tags$li("Apply wound correction (optional)"),
            tags$li("Convert to sap flux density (Jv) - all sensors"),
            tags$li("Integrate across sapwood area → tree-level water use")
          )
        ),

        # Conversion configuration
        box(
          width = 12,
          title = "Conversion Settings",
          status = "primary",
          solidHeader = TRUE,

          helpText(
            icon("info-circle"),
            " Select which method(s) to convert to flux density. sDMA methods combine multiple measurement approaches."
          ),

          h5("Select Methods:"),
          uiOutput(ns("method_checkboxes")),

          hr(),

          helpText(
            icon("info-circle"),
            " Flux density will be calculated for all available sensor positions (inner and outer).",
            " Both sensors are required for radial integration across the sapwood."
          ),

          # Data availability status
          htmlOutput(ns("data_availability_status")),

          hr(),

          h5("Wood Properties:"),
          helpText("Z factor (sap flux conversion factor) is calculated from your wood properties."),
          verbatimTextOutput(ns("z_factor_display")),

          hr(),

          actionButton(
            ns("convert_to_flux"),
            "1. Convert to Sap Flux Density (Jv)",
            icon = icon("exchange-alt"),
            class = "btn-primary",
            width = "100%"
          )
        ),

        # Tree dimensions for integration
        box(
          width = 12,
          title = "Tree Water Use Integration",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          helpText(
            icon("tree"),
            " Integrate flux density across sapwood area to calculate whole-tree water use (Q)."
          ),

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("Convert to flux density first."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            h5("Tree Dimensions:"),

            fluidRow(
              column(6,
                numericInput(
                  ns("dbh_cm"),
                  "DBH (cm):",
                  value = 30,
                  min = 1,
                  max = 200,
                  step = 0.1
                )
              ),
              column(6,
                numericInput(
                  ns("sapwood_depth_cm"),
                  "Sapwood Depth (cm):",
                  value = 3.0,
                  min = 0.1,
                  max = 50,
                  step = 0.1
                )
              )
            ),

            numericInput(
              ns("bark_thickness_cm"),
              "Bark Thickness (cm, optional):",
              value = 0,
              min = 0,
              max = 10,
              step = 0.1
            ),

            hr(),

            selectInput(
              ns("integration_method"),
              "Integration Method:",
              choices = c(
                "Weighted Average" = "weighted_average",
                "Uniform" = "uniform"
              ),
              selected = "weighted_average"
            ),

            helpText("Weighted average accounts for radial variation in sap flux density."),

            hr(),

            actionButton(
              ns("calculate_tree_water_use"),
              "2. Calculate Tree Water Use (Q)",
              icon = icon("tint"),
              class = "btn-warning",
              width = "100%"
            )
          )
        ),

        # Export options
        box(
          width = 12,
          title = "Export Flux Density Data",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No flux density data available yet."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            helpText("Export sap flux density data to CSV for further analysis."),

            selectInput(
              ns("export_format"),
              "Export Format:",
              choices = c(
                "Wide format (one column per sensor/method)" = "wide",
                "Long format (tidy data)" = "long"
              ),
              selected = "wide"
            ),

            downloadButton(
              ns("download_flux_data"),
              "Download CSV",
              class = "btn-success",
              style = "width: 100%;"
            )
          )
        )
      ),

      # Right column: Results Summary
      column(
        width = 8,

        # Step 1: Flux Density Conversion Summary
        box(
          width = 12,
          title = "1. Flux Density Conversion Summary",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No flux density data yet. Click 'Convert to Sap Flux Density (Jv)' to begin."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            verbatimTextOutput(ns("flux_statistics")),

            hr(),

            p(
              icon("arrow-right"),
              " Flux density conversion is complete.",
              strong(" Proceed to calculate tree water use (Q) below,"),
              " or go to ",
              strong("Tab 8b: Flux Density Validation"),
              " to explore interactive plots."
            ),

            hr(),

            actionButton(
              ns("reset_flux"),
              "Clear Flux Conversion",
              icon = icon("undo"),
              class = "btn-warning btn-sm",
              style = "width: 100%;"
            )
          )
        ),

        # Step 2: Tree Water Use Summary
        box(
          width = 12,
          title = "2. Tree Water Use Summary",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_tree_water_use")),
            p(em("No tree water use data yet. Convert flux density first, then click 'Calculate Tree Water Use (Q)'."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_tree_water_use")),

            verbatimTextOutput(ns("tree_water_use_summary")),

            hr(),

            p(
              icon("check-circle"),
              " Tree water use calculation is complete.",
              " Go to ",
              strong("Tab 8b: Flux Density Validation"),
              " to view interactive plots,",
              " or proceed to ",
              strong("Tab 9: Aggregation"),
              " for temporal summaries."
            )
          )
        ),

        # Active Status Box
        box(
          width = 12,
          title = "Active Conversion Status",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No conversions applied yet."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),
            verbatimTextOutput(ns("flux_status"))
          )
        )
      )
    )
  )
}

# Server ----
fluxDensityServer <- function(id,
                               vh_raw = reactive(NULL),
                               vh_spacing_corrected = reactive(NULL),
                               vh_wound_corrected = reactive(NULL),
                               vh_sdma = reactive(NULL),
                               wood_properties = reactive(NULL),
                               code_tracker = TRUE) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
      flux_data = NULL,
      velocity_source_used = NULL,
      methods_used = NULL,
      sensor_position_used = NULL,
      conversion_timestamp = NULL,
      tree_water_use_data = NULL,
      tree_dimensions = NULL,
      integration_timestamp = NULL
    )

    # Reactive: Consolidate all available velocity data
    all_velocity_data <- reactive({
      # Combine ALL available datasets (not just highest priority)
      # This allows users to select from corrected, sDMA, wound, etc.

      all_datasets <- list()

      # Add sDMA data if available
      if (!is.null(vh_sdma()) && nrow(vh_sdma()) > 0) {
        sdma_data <- vh_sdma()
        sdma_data$data_source <- "sDMA"
        all_datasets$sdma <- sdma_data
      }

      # Add wound corrected data if available
      if (!is.null(vh_wound_corrected()) && nrow(vh_wound_corrected()) > 0) {
        wound_data <- vh_wound_corrected()
        wound_data$data_source <- "wound_corrected"
        all_datasets$wound <- wound_data
      }

      # Add spacing corrected data if available
      if (!is.null(vh_spacing_corrected()) && nrow(vh_spacing_corrected()) > 0) {
        spacing_data <- vh_spacing_corrected()
        spacing_data$data_source <- "spacing_corrected"
        all_datasets$spacing <- spacing_data
      }

      # Add raw data if available
      if (!is.null(vh_raw()) && nrow(vh_raw()) > 0) {
        raw_data <- vh_raw()
        raw_data$data_source <- "raw"
        all_datasets$raw <- raw_data
      }

      if (length(all_datasets) == 0) {
        return(NULL)
      }

      # Return list of all available datasets
      return(all_datasets)
    })

    # Dynamic method checkboxes based on available data
    output$method_checkboxes <- renderUI({
      datasets <- all_velocity_data()

      if (is.null(datasets) || length(datasets) == 0) {
        return(p(style = "color: #999;", "No velocity data available"))
      }

      # Extract available methods from all datasets
      all_method_choices <- list()

      for (source_name in names(datasets)) {
        data <- datasets[[source_name]]
        data_source_label <- data$data_source[1]

        if ("combination" %in% names(data) && "method" %in% names(data)) {
          # sDMA data - extract secondary method from combination
          # Format: "outer_MHR" or "inner_MHR" -> extract "MHR"
          # We want to show just "sDMA: MHR" (not split by sensor or source)

          # Extract unique secondary methods from combination strings
          combinations <- unique(data$combination)
          secondary_methods <- unique(gsub("^(outer|inner)_", "", combinations))

          for (sec_method in secondary_methods) {
            method_label <- paste0("sDMA: ", sec_method)
            method_value <- paste0("sdma:", sec_method)
            # For checkboxGroupInput: names are labels (what user sees), values are what gets returned
            all_method_choices[[method_label]] <- method_value
          }

        } else if ("method" %in% names(data)) {
          # Regular data - use method column
          methods <- unique(data$method)
          methods <- methods[!is.na(methods)]

          for (m in methods) {
            # Create unique identifier including source
            if (data_source_label == "spacing_corrected") {
              method_label <- paste0(m, " (Spacing Corrected)")
              method_value <- paste0("corrected:", m)
            } else if (data_source_label == "wound_corrected") {
              method_label <- paste0(m, " (Spacing and Wound Corrected)")
              method_value <- paste0("wound:", m)
            } else {
              method_label <- paste0(m, " (", data_source_label, ")")
              method_value <- paste0(data_source_label, ":", m)
            }
            # For checkboxGroupInput: names are labels (what user sees), values are what gets returned
            all_method_choices[[method_label]] <- method_value
          }
        }
      }

      if (length(all_method_choices) == 0) {
        return(p(style = "color: #999;", "No method information available"))
      }

      checkboxGroupInput(
        session$ns("methods_selected"),
        NULL,
        choices = all_method_choices,
        selected = all_method_choices[[1]]  # Select first value, not first label
      )
    })

    # Update tree dimension inputs from wood properties
    observe({
      req(wood_properties())

      wood <- wood_properties()

      # Extract tree measurements if available
      if (inherits(wood, "WoodProperties") && !is.null(wood$tree_measurements)) {
        tree_meas <- wood$tree_measurements

        # Update DBH if available
        if (!is.null(tree_meas$dbh) && !is.na(tree_meas$dbh)) {
          updateNumericInput(session, "dbh_cm", value = tree_meas$dbh)
        }

        # Update sapwood depth if available
        if (!is.null(tree_meas$sapwood_depth) && !is.na(tree_meas$sapwood_depth)) {
          updateNumericInput(session, "sapwood_depth_cm", value = tree_meas$sapwood_depth)
        }

        # Update bark thickness if available
        if (!is.null(tree_meas$bark_thickness) && !is.na(tree_meas$bark_thickness)) {
          updateNumericInput(session, "bark_thickness_cm", value = tree_meas$bark_thickness)
        }
      }
    })

    # Display Z factor from wood properties
    output$z_factor_display <- renderText({
      req(wood_properties())

      wood <- wood_properties()
      if (inherits(wood, "WoodProperties")) {
        Z <- wood$derived_properties$sap_flux_conversion_factor
        if (!is.null(Z) && !is.na(Z)) {
          sprintf(
            paste0(
              "Z factor: %.4f\n\n",
              "Interpretation:\n",
              "Jv = %.4f × Vh\n\n",
              "For every 1 cm/hr of heat pulse velocity,\n",
              "sap flux density is %.4f cm³/cm²/hr"
            ),
            Z, Z, Z
          )
        } else {
          "Z factor not calculated.\nRun calculate_wood_properties() first."
        }
      } else {
        "Wood properties not loaded."
      }
    })

    # Data availability status
    output$data_availability_status <- renderUI({
      has_raw <- !is.null(vh_raw()) && nrow(vh_raw()) > 0
      has_spacing <- !is.null(vh_spacing_corrected()) && nrow(vh_spacing_corrected()) > 0
      has_wound <- !is.null(vh_wound_corrected()) && nrow(vh_wound_corrected()) > 0
      has_sdma <- !is.null(vh_sdma()) && nrow(vh_sdma()) > 0

      # List all available sources
      available_sources <- c()
      if (has_sdma) available_sources <- c(available_sources, "sDMA")
      if (has_wound) available_sources <- c(available_sources, "Wound-Corrected")
      if (has_spacing) available_sources <- c(available_sources, "Spacing-Corrected")
      if (has_raw) available_sources <- c(available_sources, "Raw HPV")

      data_source <- if (length(available_sources) > 0) {
        paste(available_sources, collapse = ", ")
      } else {
        "None"
      }

      status_items <- tagList(
        h5("Available Data Sources:"),
        p(strong(data_source), style = "color: #3c8dbc; font-size: 1.1em;"),

        h5("Data Pipeline Status:"),
        tags$ul(
          tags$li(
            if (has_raw) icon("check", class = "text-success") else icon("times", class = "text-danger"),
            " Raw HPV"
          ),
          tags$li(
            if (has_spacing) icon("check", class = "text-success") else icon("times", class = "text-danger"),
            " Spacing-Corrected"
          ),
          tags$li(
            if (has_wound) icon("check", class = "text-success") else icon("times", class = "text-danger"),
            " Wound-Corrected/Calibrated"
          ),
          tags$li(
            if (has_sdma) icon("check", class = "text-success") else icon("times", class = "text-danger"),
            " sDMA Applied"
          )
        )
      )

      return(status_items)
    })

    # Convert to flux density
    observeEvent(input$convert_to_flux, {
      req(wood_properties())
      req(input$methods_selected)

      withProgress(message = "Converting to sap flux density...", {

        tryCatch({
          # Get all available datasets
          all_datasets <- all_velocity_data()

          if (is.null(all_datasets) || length(all_datasets) == 0) {
            showNotification("No velocity data available", type = "error")
            return()
          }

          # Collect data for each selected method
          vh_data_list <- list()

          for (method_selection in input$methods_selected) {
            # Parse method selection (format: "source:method" or "sdma:secondary_method")
            parts <- strsplit(method_selection, ":")[[1]]

            if (parts[1] == "sdma") {
              # sDMA data: format is "sdma:secondary_method" (e.g., "sdma:MHR")
              # Get ALL sensors for this sDMA variant
              secondary_method <- parts[2]

              sdma_data <- all_datasets$sdma
              if (!is.null(sdma_data)) {
                # Filter by secondary method (matches combinations like "outer_MHR", "inner_MHR")
                # Don't filter by sdma_source - that's internal to sDMA
                filtered <- sdma_data %>%
                  dplyr::filter(
                    grepl(paste0("_", secondary_method, "$"), combination)
                  ) %>%
                  dplyr::mutate(
                    Vh_for_conversion = Vh_sdma,
                    method_label = paste0("sDMA: ", secondary_method)
                  )

                if (nrow(filtered) > 0) {
                  vh_data_list[[method_selection]] <- filtered
                }
              }

            } else {
              # Regular data: format is "source:method"
              source_type <- parts[1]
              method_name <- parts[2]

              # Find corresponding dataset
              data <- if (source_type == "corrected") {
                all_datasets$spacing
              } else if (source_type == "wound") {
                all_datasets$wound
              } else if (source_type == "raw") {
                all_datasets$raw
              } else {
                all_datasets[[source_type]]
              }

              if (!is.null(data) && "method" %in% names(data)) {
                # Get ALL sensors (don't filter by sensor_position)
                filtered <- data %>%
                  dplyr::filter(
                    method == method_name
                  ) %>%
                  dplyr::mutate(
                    Vh_for_conversion = Vh_cm_hr,
                    method_label = if (source_type == "corrected") {
                      paste0(method_name, " (Spacing Corrected)")
                    } else if (source_type == "wound") {
                      paste0(method_name, " (Spacing and Wound Corrected)")
                    } else {
                      paste0(method_name, " (", source_type, ")")
                    }
                  )

                if (nrow(filtered) > 0) {
                  vh_data_list[[method_selection]] <- filtered
                }
              }
            }
          }

          if (length(vh_data_list) == 0) {
            showNotification("No data for selected methods", type = "warning")
            return()
          }

          # Combine all selected data
          vh_data <- dplyr::bind_rows(vh_data_list)

          if (nrow(vh_data) == 0) {
            showNotification("No data after filtering", type = "warning")
            return()
          }

          # Apply flux density conversion
          wood <- wood_properties()

          # Convert using sapfluxr function
          flux_data <- vh_data
          flux_data$Jv_cm3_cm2_hr <- sapfluxr::calc_sap_flux_density(
            Vh = vh_data$Vh_for_conversion,
            wood_properties = wood
          )

          # Get sensor positions in the data
          sensors_used <- unique(flux_data$sensor_position)

          rv$flux_data <- flux_data
          rv$velocity_source_used <- paste(unique(flux_data$data_source), collapse = ", ")
          rv$methods_used <- input$methods_selected
          rv$sensor_position_used <- paste(sensors_used, collapse = ", ")
          rv$conversion_timestamp <- Sys.time()

          # Code generation
          if (!isTRUE(code_tracker)) {
            methods_str <- paste0('"', input$methods_selected, '"', collapse = ", ")
            code_tracker$add_step(
              step_name = "Convert to Sap Flux Density",
              code = sprintf(
                paste0(
                  '# Convert heat pulse velocity to sap flux density\n',
                  'flux_data <- sapfluxr::convert_vh_to_flux_density(\n',
                  '  vh_data = velocity_data,  # Use corrected velocity data\n',
                  '  wood_properties = wood_properties,\n',
                  '  methods = c(%s)\n',
                  ')'
                ),
                methods_str
              ),
              description = sprintf("Converted %d method(s) to flux density", length(input$methods_selected))
            )
          }

          showNotification(
            sprintf("Flux density conversion successful! %d method(s), %s sensor(s), %d records",
                    length(input$methods_selected), paste(sensors_used, collapse = "+"), nrow(flux_data)),
            type = "message"
          )

        }, error = function(e) {
          showNotification(
            paste("Error converting to flux density:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # Calculate tree water use (Q)
    observeEvent(input$calculate_tree_water_use, {
      req(rv$flux_data)
      req(input$dbh_cm, input$sapwood_depth_cm)

      withProgress(message = "Calculating tree water use...", {

        tryCatch({
          # Add tree dimensions as columns to flux data
          flux_with_dims <- rv$flux_data
          flux_with_dims$dbh <- input$dbh_cm
          flux_with_dims$sapwood_depth <- input$sapwood_depth_cm
          flux_with_dims$bark_thickness <- input$bark_thickness_cm

          # Apply sap flux integration across sapwood area
          # This integrates flux density from all sensor positions (inner + outer)
          # using the selected integration method (weighted average or uniform)
          tree_water_use_data <- sapfluxr::apply_sap_flux_integration(
            flux_data = flux_with_dims,
            dbh_col = "dbh",
            sapwood_depth_col = "sapwood_depth",
            bark_thickness_col = "bark_thickness",
            method = input$integration_method
          )

          # Store results
          rv$tree_water_use_data <- tree_water_use_data
          rv$tree_dimensions <- list(
            dbh = input$dbh_cm,
            sapwood_depth = input$sapwood_depth_cm,
            bark_thickness = input$bark_thickness_cm
          )
          rv$integration_timestamp <- Sys.time()

          # Code generation
          if (!isTRUE(code_tracker)) {
            # Calculate sapwood area
            r_outer <- (input$dbh_cm - 2 * input$bark_thickness_cm) / 2
            r_inner <- r_outer - input$sapwood_depth_cm
            sapwood_area <- pi * (r_outer^2 - r_inner^2)

            rv$tree_dimensions$sapwood_area <- sapwood_area
            rv$tree_dimensions$integration_method <- input$integration_method

            code_tracker$add_step(
              step_name = "Calculate Tree Water Use",
              code = sprintf(
                paste0(
                  '# Calculate tree-level water use from flux density\n',
                  'tree_water_use <- sapfluxr::apply_sap_flux_integration(\n',
                  '  flux_data = flux_data,\n',
                  '  dbh = %.2f,  # cm\n',
                  '  sapwood_depth = %.2f,  # cm\n',
                  '  bark_thickness = %.2f,  # cm\n',
                  '  method = "%s"  # Integration method\n',
                  ')\n',
                  '# Sapwood area: %.2f cm²'
                ),
                input$dbh_cm,
                input$sapwood_depth_cm,
                input$bark_thickness_cm,
                input$integration_method,
                sapwood_area
              ),
              description = sprintf("Integrated flux across sapwood area (DBH: %.1f cm, Sapwood: %.1f cm, Area: %.1f cm²)",
                                   input$dbh_cm, input$sapwood_depth_cm, sapwood_area)
            )
          }

          showNotification(
            sprintf("Tree water use calculated! DBH: %.1f cm, Sapwood depth: %.1f cm",
                    input$dbh_cm, input$sapwood_depth_cm),
            type = "message"
          )

        }, error = function(e) {
          showNotification(
            paste("Error calculating tree water use:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # Reset flux conversion
    observeEvent(input$reset_flux, {
      rv$flux_data <- NULL
      rv$velocity_source_used <- NULL
      rv$conversion_timestamp <- NULL
      rv$tree_water_use_data <- NULL
      rv$tree_dimensions <- NULL
      rv$integration_timestamp <- NULL
      showNotification("Flux conversion cleared", type = "message")
    })

    # Flag for flux results
    output$has_flux_results <- reactive({
      !is.null(rv$flux_data)
    })
    outputOptions(output, "has_flux_results", suspendWhenHidden = FALSE)

    # Flag for tree water use
    output$has_tree_water_use <- reactive({
      !is.null(rv$tree_water_use_data)
    })
    outputOptions(output, "has_tree_water_use", suspendWhenHidden = FALSE)

    # Flux statistics
    output$flux_statistics <- renderText({
      req(rv$flux_data)

      flux <- rv$flux_data

      # Get method breakdown
      if ("method_label" %in% names(flux)) {
        method_summary <- flux %>%
          dplyr::group_by(method_label, sensor_position) %>%
          dplyr::summarise(
            n = dplyr::n(),
            mean_jv = mean(Jv_cm3_cm2_hr, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::arrange(method_label, sensor_position)

        method_text <- paste0(
          "\nMethod Breakdown:\n",
          paste(
            sprintf("  %s (%s): %d points, mean = %.3f cm³/cm²/hr",
                    method_summary$method_label,
                    toupper(method_summary$sensor_position),
                    method_summary$n,
                    method_summary$mean_jv),
            collapse = "\n"
          ),
          "\n"
        )
      } else {
        method_text <- ""
      }

      sprintf(
        paste0(
          "Flux Density Conversion Summary\n\n",
          "Source: %s\n",
          "Converted: %s\n\n",
          "Data Points: %d\n",
          "Date Range: %s to %s\n",
          "%s",
          "\nOverall Sap Flux Density (Jv) Statistics:\n",
          "  Mean: %.3f cm³/cm²/hr\n",
          "  Median: %.3f cm³/cm²/hr\n",
          "  Min: %.3f cm³/cm²/hr\n",
          "  Max: %.3f cm³/cm²/hr\n",
          "  SD: %.3f cm³/cm²/hr"
        ),
        rv$velocity_source_used,
        format(rv$conversion_timestamp, "%Y-%m-%d %H:%M:%S"),
        nrow(flux),
        format(min(flux$datetime), "%Y-%m-%d"),
        format(max(flux$datetime), "%Y-%m-%d"),
        method_text,
        mean(flux$Jv_cm3_cm2_hr, na.rm = TRUE),
        median(flux$Jv_cm3_cm2_hr, na.rm = TRUE),
        min(flux$Jv_cm3_cm2_hr, na.rm = TRUE),
        max(flux$Jv_cm3_cm2_hr, na.rm = TRUE),
        sd(flux$Jv_cm3_cm2_hr, na.rm = TRUE)
      )
    })

    # Flux status
    output$flux_status <- renderText({
      req(rv$flux_data)

      sprintf(
        paste0(
          "Flux density data active (%d records)\n",
          "Source: %s\n",
          "Converted: %s"
        ),
        nrow(rv$flux_data),
        rv$velocity_source_used,
        format(rv$conversion_timestamp, "%Y-%m-%d %H:%M:%S")
      )
    })

    # Tree water use summary
    output$tree_water_use_summary <- renderText({
      req(rv$tree_water_use_data)

      q_data <- rv$tree_water_use_data

      # Get method breakdown if available
      if ("method_label" %in% names(q_data)) {
        method_summary <- q_data %>%
          dplyr::group_by(method_label) %>%
          dplyr::summarise(
            n = dplyr::n(),
            mean_Q = mean(Q_L_hr, na.rm = TRUE),
            total_daily = sum(Q_L_day, na.rm = TRUE),
            .groups = "drop"
          )

        method_text <- paste0(
          "\nMethod Breakdown:\n",
          paste(
            sprintf("  %s: %d points, mean = %.2f L/hr, total = %.1f L/day",
                    method_summary$method_label,
                    method_summary$n,
                    method_summary$mean_Q,
                    method_summary$total_daily),
            collapse = "\n"
          ),
          "\n"
        )
      } else {
        method_text <- ""
      }

      sprintf(
        paste0(
          "Tree Water Use Summary\n\n",
          "Tree Dimensions:\n",
          "  DBH: %.2f cm\n",
          "  Sapwood Depth: %.2f cm\n",
          "  Sapwood Area: %.2f cm²\n\n",
          "Integration Method: %s\n",
          "Calculated: %s\n\n",
          "Data Points: %d\n",
          "Date Range: %s to %s\n",
          "%s",
          "\nOverall Water Use Statistics:\n",
          "  Mean: %.3f L/hr\n",
          "  Median: %.3f L/hr\n",
          "  Total Daily: %.2f L/day\n",
          "  Min: %.3f L/hr\n",
          "  Max: %.3f L/hr"
        ),
        rv$tree_dimensions$dbh,
        rv$tree_dimensions$sapwood_depth,
        rv$tree_dimensions$sapwood_area,
        rv$tree_dimensions$integration_method,
        format(rv$integration_timestamp, "%Y-%m-%d %H:%M:%S"),
        nrow(q_data),
        format(min(q_data$datetime), "%Y-%m-%d"),
        format(max(q_data$datetime), "%Y-%m-%d"),
        method_text,
        mean(q_data$Q_L_hr, na.rm = TRUE),
        median(q_data$Q_L_hr, na.rm = TRUE),
        sum(q_data$Q_L_day, na.rm = TRUE),
        min(q_data$Q_L_hr, na.rm = TRUE),
        max(q_data$Q_L_hr, na.rm = TRUE)
      )
    })

    # ==================================================================
    # PLOT OUTPUTS REMOVED
    # All visualization moved to mod_8b_flux_validation.R
    # ==================================================================

    # REMOVED: flux_timeseries_plot - moved to mod_8b
    # REMOVED: velocity_vs_flux_plot - moved to mod_8b
    # REMOVED: daily_flux_plot - moved to mod_8b
    # REMOVED: tree_water_use_plot_hourly - moved to mod_8b
    # REMOVED: tree_water_use_plot_daily - moved to mod_8b

    # Download flux data
    output$download_flux_data <- downloadHandler(
      filename = function() {
        paste0("sap_flux_density_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(rv$flux_data)

        if (input$export_format == "wide") {
          # Wide format: keep as is
          write.csv(rv$flux_data, file, row.names = FALSE)
        } else {
          # Long format: reshape
          # This would require tidyr::pivot_longer, implement if needed
          write.csv(rv$flux_data, file, row.names = FALSE)
        }
      }
    )

    # Return values for downstream modules
    return(list(
      flux_data = reactive(rv$flux_data),
      has_flux_data = reactive(!is.null(rv$flux_data)),
      velocity_source = reactive(rv$velocity_source_used),
      tree_water_use_data = reactive(rv$tree_water_use_data),
      has_tree_water_use = reactive(!is.null(rv$tree_water_use_data)),
      tree_dimensions = reactive(rv$tree_dimensions)
    ))
  })
}
