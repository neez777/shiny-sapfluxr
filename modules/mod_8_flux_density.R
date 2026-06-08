# mod_8_flux_density.R
# Module for Sap Flux Density Conversion (Jv)
#
# Step 8: Sap Flux Density
# Converts corrected heat pulse velocity to sap flux density using
# wood-specific conversion factors, with an inline time-series plot.
# Radial integration / tree water use lives in mod_8b_radial_integration.R.

# UI ----
fluxDensityUI <- function(id) {
  ns <- NS(id)

  tagList(
    # ---- Row 1: Conversion controls + summary ----
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
            tags$li("Integrate across sapwood area → tree-level water use (Step 9)")
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
            "Convert to Sap Flux Density (Jv)",
            icon = icon("exchange-alt"),
            class = "btn-primary",
            width = "100%"
          )
        )
      ),

      # Right column: Results (tabbed — Summary | Time Series)
      column(
        width = 8,

        box(
          width = 12,
          title = "Sap Flux Density Results",
          status = "primary",
          solidHeader = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No flux density data yet. Click 'Convert to Sap Flux Density (Jv)' to begin."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            tabsetPanel(
              id = ns("flux_results_tabs"),

              tabPanel(
                "Time Series",
                br(),

                # ---- Plot controls ----
                fluidRow(
                  column(4,
                    h5("Methods to Display"),
                    uiOutput(ns("plot_method_checkboxes"))
                  ),
                  column(4,
                    h5("Sensor Position"),
                    checkboxGroupInput(
                      ns("plot_sensor_position"),
                      NULL,
                      choices = c("Inner" = "inner", "Outer" = "outer"),
                      selected = c("inner", "outer")
                    )
                  ),
                  column(4,
                    h5("Display"),
                    checkboxInput(
                      ns("show_points"),
                      "Show data points",
                      value = FALSE
                    )
                  )
                ),

                fluidRow(
                  column(4,
                    shinyWidgets::airDatepickerInput(
                      ns("start_datetime"),
                      "Start Date/Time:",
                      value = NULL,
                      timepicker = TRUE,
                      dateFormat = "yyyy-MM-dd HH:mm"
                    )
                  ),
                  column(4,
                    shinyWidgets::airDatepickerInput(
                      ns("end_datetime"),
                      "End Date/Time:",
                      value = NULL,
                      timepicker = TRUE,
                      dateFormat = "yyyy-MM-dd HH:mm"
                    )
                  ),
                  column(2,
                    br(),
                    actionButton(
                      ns("apply_range"),
                      "Apply",
                      icon = icon("clock"),
                      class = "btn-primary btn-sm",
                      style = "width: 100%; margin-top: 8px;"
                    )
                  ),
                  column(2,
                    br(),
                    actionButton(
                      ns("reset_zoom"),
                      "Reset",
                      icon = icon("refresh"),
                      class = "btn-default btn-sm",
                      style = "width: 100%; margin-top: 8px;"
                    )
                  )
                ),

                helpText(
                  icon("info-circle"),
                  "Sap flux density over time for all selected methods.",
                  "Click-drag to zoom, double-click to reset zoom."
                ),

                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("flux_timeseries_plot"), height = "500px"),
                  type = 6,
                  color = "#3c8dbc"
                )
              ),

              tabPanel(
                "Summary",
                br(),

                verbatimTextOutput(ns("flux_statistics")),

                hr(),

                p(
                  icon("arrow-right"),
                  " Flux density conversion is complete.",
                  strong(" Proceed to Step 9: Radial Integration"),
                  " to integrate across the sapwood and calculate whole-tree water use."
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
            )
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
                               code_tracker = TRUE,
                               plot_settings = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      flux_data = NULL,
      velocity_source_used = NULL,
      methods_used = NULL,
      sensor_position_used = NULL,
      conversion_timestamp = NULL
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
          combinations <- unique(data$combination)
          secondary_methods <- unique(gsub("^(outer|inner)_", "", combinations))

          for (sec_method in secondary_methods) {
            method_label <- paste0("sDMA: ", sec_method)
            method_value <- paste0("sdma:", sec_method)
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
              secondary_method <- parts[2]

              sdma_data <- all_datasets$sdma
              if (!is.null(sdma_data)) {
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
                vel_col <- if ("Vs_cm_hr" %in% names(data)) "Vs_cm_hr" else "Vh_cm_hr"
                # Get ALL sensors (don't filter by sensor_position)
                filtered <- data %>%
                  dplyr::filter(
                    method == method_name
                  ) %>%
                  dplyr::mutate(
                    Vh_for_conversion = .data[[vel_col]],
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
            code_tracker$add_step(
              step_name = "Convert to Sap Flux Density",
              code = paste0(
                  '# Convert heat pulse velocity to sap flux density\n',
                  '# velocity_data$Vs_cm_hr is the current best velocity estimate\n',
                  'flux_data <- velocity_data\n',
                  'flux_data$Jv_cm3_cm2_hr <- sapfluxr::calc_sap_flux_density(\n',
                  '  Vh             = velocity_data$Vs_cm_hr,\n',
                  '  wood_properties = wood_properties\n',
                  ')'
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

    # Reset flux conversion
    observeEvent(input$reset_flux, {
      rv$flux_data <- NULL
      rv$velocity_source_used <- NULL
      rv$conversion_timestamp <- NULL
      showNotification("Flux conversion cleared", type = "message")
    })

    # Flag for flux results
    output$has_flux_results <- reactive({
      !is.null(rv$flux_data)
    })
    outputOptions(output, "has_flux_results", suspendWhenHidden = FALSE)

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

    # ==================================================================
    # INLINE TIME-SERIES PLOT
    # ==================================================================

    # Reactive: Time range
    time_range <- reactiveVal(NULL)

    # Initialise datetime range from data
    observe({
      req(rv$flux_data)

      data <- rv$flux_data
      date_range <- range(data$datetime, na.rm = TRUE)

      shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
      shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
    })

    # Dynamic method checkboxes for the plot
    output$plot_method_checkboxes <- renderUI({
      req(rv$flux_data)

      data <- rv$flux_data
      methods <- c()

      if ("method_label" %in% names(data)) {
        methods <- unique(data$method_label)
        methods <- methods[!is.na(methods)]
      } else if ("method" %in% names(data)) {
        methods <- unique(data$method)
        methods <- methods[!is.na(methods)]
      } else if ("combination" %in% names(data)) {
        methods <- unique(data$combination)
        methods <- methods[!is.na(methods)]
      }

      if (length(methods) == 0) {
        return(p(style = "color: #999;", "No methods available"))
      }

      checkboxGroupInput(
        ns("plot_methods_selected"),
        NULL,
        choices = methods,
        selected = methods
      )
    })

    # Reactive: Filtered plot data
    plot_data_flux <- reactive({
      req(rv$flux_data)
      req(input$plot_sensor_position)

      data <- rv$flux_data

      # Filter by sensor position
      if ("sensor_position" %in% names(data)) {
        data <- data %>%
          dplyr::filter(sensor_position %in% input$plot_sensor_position)
      }

      # Filter by selected methods if specified
      if (!is.null(input$plot_methods_selected) && length(input$plot_methods_selected) > 0) {
        if ("method_label" %in% names(data)) {
          data <- data %>% dplyr::filter(method_label %in% input$plot_methods_selected)
        } else if ("method" %in% names(data)) {
          data <- data %>% dplyr::filter(method %in% input$plot_methods_selected)
        } else if ("combination" %in% names(data)) {
          data <- data %>% dplyr::filter(combination %in% input$plot_methods_selected)
        }
      }

      # Apply time range filter
      if (!is.null(time_range())) {
        data <- data %>%
          dplyr::filter(datetime >= time_range()[1], datetime <= time_range()[2])
      }

      # Sort to prevent diagonal connection lines
      if ("method_label" %in% names(data) && "sensor_position" %in% names(data)) {
        data <- data %>% dplyr::arrange(method_label, sensor_position, datetime)
      } else if ("method" %in% names(data)) {
        data <- data %>% dplyr::arrange(method, datetime)
      } else {
        data <- data %>% dplyr::arrange(datetime)
      }

      return(data)
    })

    # Flux timeseries plot
    output$flux_timeseries_plot <- plotly::renderPlotly({
      tryCatch({
        flux <- plot_data_flux()
        style_config <- plot_settings()

        if (is.null(flux) || nrow(flux) == 0) {
          return(
            plotly::plot_ly() %>%
              plotly::layout(
                title = "No data to display - please check your selections",
                xaxis = list(title = "Datetime"),
                yaxis = list(title = "Sap Flux Density (cm³/cm²/hr)"),
                uirevision = "flux_timeseries_zoom"
              )
          )
        }

        # Create base plot
        p <- plotly::plot_ly(source = "flux_timeseries_plot")

        # Determine mode
        mode <- if (input$show_points) "lines+markers" else "lines"

        # Determine which methods and sensors are present
        methods <- unique(flux$method_label %||% flux$method %||% "Sap Flux")
        sensors <- if ("sensor_position" %in% names(flux)) unique(flux$sensor_position) else "outer"

        # Add traces using loop for consistent styling
        for (m in methods) {
          for (s in sensors) {
            trace_data <- flux
            if ("method_label" %in% names(flux)) {
              trace_data <- trace_data %>% dplyr::filter(method_label == m)
            } else if ("method" %in% names(flux)) {
              trace_data <- trace_data %>% dplyr::filter(method == m)
            }

            if ("sensor_position" %in% names(flux)) {
              trace_data <- trace_data %>% dplyr::filter(sensor_position == s)
            }

            if (nrow(trace_data) == 0) next

            trace_name <- if (length(sensors) > 1) {
              paste0(m, " (", toupper(s), ")")
            } else {
              m
            }

            # Get style
            style_m <- m
            if (grepl("HRM", style_m)) style_m <- "HRM"
            else if (grepl("MHR", style_m)) style_m <- "MHR"
            else if (grepl("Tmax_Coh", style_m)) style_m <- "Tmax_Coh"
            else if (grepl("Tmax_Klu", style_m)) style_m <- "Tmax_Klu"
            else if (grepl("sDMA", style_m)) style_m <- "sDMA"

            style <- get_plot_style(method = style_m, sensor = s, is_corrected = TRUE, config = style_config)

            p <- p %>%
              plotly::add_trace(
                data = trace_data,
                x = ~datetime,
                y = ~Jv_cm3_cm2_hr,
                type = "scatter",
                mode = mode,
                name = trace_name,
                line = style,
                marker = if (input$show_points) list(size = 4, color = style$color) else NULL,
                legendgroup = trace_name,
                showlegend = TRUE,
                hovertemplate = paste(
                  "<b>", trace_name, "</b><br>",
                  "Date: %{x|%Y-%m-%d %H:%M}<br>",
                  "Jv: %{y:.2f} cm³/cm²/hr<br>",
                  "<extra></extra>"
                )
              )
          }
        }

        # Apply standard layout
        base_layout <- get_standard_layout(
          title = "Sap Flux Density Time Series",
          xtitle = "Date",
          ytitle = "Sap Flux Density (cm³/cm²/hr)",
          uirevision = "flux_timeseries_zoom"
        )

        # Force zoom range persistence
        if (!is.null(time_range())) {
          base_layout$xaxis$range <- time_range()
          base_layout$xaxis$autorange <- FALSE
        }

        p <- p %>%
          plotly::layout(
            title = list(text = base_layout$title, x = 0.5, xanchor = "center"),
            xaxis = base_layout$xaxis,
            yaxis = base_layout$yaxis,
            hovermode = base_layout$hovermode,
            showlegend = TRUE,
            legend = base_layout$legend,
            margin = base_layout$margin,
            plot_bgcolor = base_layout$plot_bgcolor,
            paper_bgcolor = base_layout$paper_bgcolor,
            uirevision = base_layout$uirevision
          ) %>%
          apply_standard_plotly_config(filename = "flux_timeseries_plot", add_csv_download = TRUE) %>%
          plotly::event_register("plotly_relayout")

        return(p)

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(
            title = list(text = paste("Error:", e$message), x = 0.5),
            xaxis = list(title = "Datetime"),
            yaxis = list(title = "Sap Flux Density (cm³/cm²/hr)"),
            uirevision = "flux_timeseries_zoom"
          )
      })
    })

    # Update datetime inputs when user zooms the plot
    relayout_debounced <- debounce(reactive({
      event_data("plotly_relayout", source = "flux_timeseries_plot")
    }), 500)

    observeEvent(relayout_debounced(), {
      rd <- relayout_debounced()
      if (is.null(rd)) return()

      if (!is.null(rd$xaxis.range) && length(rd$xaxis.range) == 2) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$xaxis.range[1], tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$xaxis.range[2], tz = "UTC"))
        time_range(c(rd$xaxis.range[1], rd$xaxis.range[2]))

      } else if (!is.null(rd$`xaxis.range[0]`)) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$`xaxis.range[0]`, tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$`xaxis.range[1]`, tz = "UTC"))
        time_range(c(rd$`xaxis.range[0]`, rd$`xaxis.range[1]`))

      } else if (isTRUE(rd$`xaxis.autorange`)) {
        req(rv$flux_data)
        date_range <- range(rv$flux_data$datetime, na.rm = TRUE)
        shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
        shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
        time_range(NULL)
      }
    })

    # Apply manual range
    observeEvent(input$apply_range, {
      req(input$start_datetime, input$end_datetime)

      t_range <- c(
        format(input$start_datetime, "%Y-%m-%d %H:%M:%S"),
        format(input$end_datetime, "%Y-%m-%d %H:%M:%S")
      )

      time_range(t_range)

      plotly::plotlyProxy("flux_timeseries_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list("xaxis.range" = t_range))
    })

    # Reset zoom
    observeEvent(input$reset_zoom, {
      time_range(NULL)

      if (!is.null(rv$flux_data)) {
        date_range <- range(rv$flux_data$datetime, na.rm = TRUE)

        shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
        shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])

        plotly::plotlyProxy("flux_timeseries_plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE))
      }
    })

    # Return values for downstream modules
    return(list(
      flux_data = reactive(rv$flux_data),
      has_flux_data = reactive(!is.null(rv$flux_data)),
      velocity_source = reactive(rv$velocity_source_used)
    ))
  })
}
