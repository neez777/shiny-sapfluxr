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
            tags$li(strong("Units:"), " cm³/cm²/hr (equivalent to cm/hr sap velocity)")
          ),
          p(tags$small(em("Based on Burgess et al. (2001) after Barrett et al. (1995)"))),

          hr(),

          h5("Recommended Workflow:"),
          tags$ol(
            tags$li("Calculate raw heat pulse velocity (Vh)"),
            tags$li("Apply spacing correction"),
            tags$li("Apply wound correction (optional)"),
            tags$li("Convert to sap flux density (Jv)"),
            tags$li("Scale to tree-level water use")
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

          h5("Select Sensor Position:"),
          radioButtons(
            ns("sensor_position"),
            NULL,
            choices = c("Outer" = "outer", "Inner" = "inner"),
            selected = "outer",
            inline = TRUE
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

      # Right column: Results and Visualisation
      column(
        width = 8,

        # Tree water use time series plot (Q)
        box(
          width = 12,
          title = "Tree Water Use Time Series (Q)",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_tree_water_use")),
            p(em("No tree water use data yet. Calculate Q to see results."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_tree_water_use")),

            helpText("Whole-tree water use integrated across sapwood area. Hover for details, click-drag to zoom."),

            tabsetPanel(
              tabPanel(
                "Hourly (L/hr)",
                br(),
                plotly::plotlyOutput(ns("tree_water_use_plot_hourly"), height = "400px")
              ),
              tabPanel(
                "Daily (L/day)",
                br(),
                plotly::plotlyOutput(ns("tree_water_use_plot_daily"), height = "400px")
              )
            )
          )
        ),

        # Flux density time series plot (Jv)
        box(
          width = 12,
          title = "Sap Flux Density Time Series (Jv)",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No flux density data yet. Convert velocity data to see results."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            helpText("Interactive plot of sap flux density over time for all selected methods. Hover for details, click-drag to zoom."),

            plotly::plotlyOutput(ns("flux_timeseries_plot"), height = "500px")
          )
        ),

        # Velocity vs Flux comparison
        box(
          width = 12,
          title = "Velocity vs Flux Density Comparison",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No flux density data yet."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            helpText("Compare heat pulse velocity (Vh) with sap flux density (Jv) to visualize the conversion."),

            plotly::plotlyOutput(ns("velocity_vs_flux_plot"), height = "400px")
          )
        ),

        # Conversion summary and statistics
        box(
          width = 12,
          title = "Conversion Summary",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No results yet."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            tabsetPanel(
              id = ns("summary_tabs"),

              tabPanel(
                "Statistics",
                br(),
                verbatimTextOutput(ns("flux_statistics"))
              ),

              tabPanel(
                "Daily Totals",
                br(),
                helpText("Daily sap flux totals integrated over 24-hour periods."),
                plotly::plotlyOutput(ns("daily_flux_plot"), height = "400px")
              )
            )
          )
        ),

        # Active flux conversion status
        box(
          width = 12,
          title = "Active Conversion Status",
          status = "success",
          solidHeader = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No flux density conversion applied."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            p("Flux density conversion is active:"),
            verbatimTextOutput(ns("flux_status")),

            hr(),

            actionButton(
              ns("reset_flux"),
              "Clear Flux Conversion",
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
fluxDensityServer <- function(id,
                               vh_raw = reactive(NULL),
                               vh_spacing_corrected = reactive(NULL),
                               vh_wound_corrected = reactive(NULL),
                               vh_sdma = reactive(NULL),
                               wood_properties = reactive(NULL)) {
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
      # Priority order: sDMA > wound > spacing > raw
      if (!is.null(vh_sdma()) && nrow(vh_sdma()) > 0) {
        data <- vh_sdma()
        # Add data_source column if not present
        if (!"data_source" %in% names(data)) {
          data$data_source <- "sDMA"
        }
        return(data)
      } else if (!is.null(vh_wound_corrected()) && nrow(vh_wound_corrected()) > 0) {
        data <- vh_wound_corrected()
        if (!"data_source" %in% names(data)) {
          data$data_source <- "wound"
        }
        return(data)
      } else if (!is.null(vh_spacing_corrected()) && nrow(vh_spacing_corrected()) > 0) {
        data <- vh_spacing_corrected()
        if (!"data_source" %in% names(data)) {
          data$data_source <- "spacing"
        }
        return(data)
      } else if (!is.null(vh_raw()) && nrow(vh_raw()) > 0) {
        data <- vh_raw()
        if (!"data_source" %in% names(data)) {
          data$data_source <- "raw"
        }
        return(data)
      }
      return(NULL)
    })

    # Dynamic method checkboxes based on available data
    output$method_checkboxes <- renderUI({
      data <- all_velocity_data()

      if (is.null(data)) {
        return(p(style = "color: #999;", "No velocity data available"))
      }

      # Extract available methods
      if ("combination" %in% names(data) && "sdma_source" %in% names(data)) {
        # sDMA data - create method options from combination and sdma_source
        # Filter out HRM and NA - sDMA should only show secondary methods
        method_options <- data %>%
          dplyr::distinct(combination, sdma_source) %>%
          dplyr::filter(!sdma_source %in% c("HRM", "NA"), !is.na(sdma_source)) %>%
          dplyr::mutate(
            method_label = paste0("sDMA: ", sdma_source, " (", combination, ")")
          ) %>%
          dplyr::pull(method_label)

        method_values <- data %>%
          dplyr::distinct(combination, sdma_source) %>%
          dplyr::filter(!sdma_source %in% c("HRM", "NA"), !is.na(sdma_source)) %>%
          dplyr::mutate(
            method_value = paste0(combination, ":", sdma_source)
          ) %>%
          dplyr::pull(method_value)

        names(method_values) <- method_options
      } else if ("method" %in% names(data)) {
        # Regular data - use method column
        methods <- unique(data$method)
        methods <- methods[!is.na(methods)]
        method_values <- methods
        names(method_values) <- methods
      } else {
        return(p(style = "color: #999;", "No method information available"))
      }

      checkboxGroupInput(
        session$ns("methods_selected"),
        NULL,
        choices = method_values,
        selected = method_values[1]
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

      data_source <- if (has_sdma) {
        "sDMA"
      } else if (has_wound) {
        "Wound-Corrected"
      } else if (has_spacing) {
        "Spacing-Corrected"
      } else if (has_raw) {
        "Raw HPV"
      } else {
        "None"
      }

      status_items <- tagList(
        h5("Active Data Source:"),
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
      req(input$sensor_position)

      withProgress(message = "Converting to sap flux density...", {

        tryCatch({
          # Get the consolidated velocity data
          all_data <- all_velocity_data()

          if (is.null(all_data) || nrow(all_data) == 0) {
            showNotification("No velocity data available", type = "error")
            return()
          }

          # Filter by selected sensor position
          vh_data <- all_data %>%
            dplyr::filter(sensor_position == input$sensor_position)

          # Filter by selected methods
          if ("combination" %in% names(vh_data) && "sdma_source" %in% names(vh_data)) {
            # sDMA data - filter by combination:sdma_source
            selected_filters <- input$methods_selected

            # Parse the method selection (format: "combination:sdma_source")
            filter_list <- lapply(selected_filters, function(x) {
              parts <- strsplit(x, ":")[[1]]
              list(combination = parts[1], sdma_source = parts[2])
            })

            # Apply filters
            vh_data <- vh_data %>%
              dplyr::filter(
                purrr::map2_lgl(combination, sdma_source, function(comb, source) {
                  any(purrr::map_lgl(filter_list, function(f) {
                    f$combination == comb && f$sdma_source == source
                  }))
                })
              )

            # Use Vh_sdma column for sDMA data
            velocity_col <- "Vh_sdma"
          } else if ("method" %in% names(vh_data)) {
            # Regular data - filter by method
            vh_data <- vh_data %>%
              dplyr::filter(method %in% input$methods_selected)

            velocity_col <- "Vh_cm_hr"
          } else {
            showNotification("Data structure not recognised", type = "error")
            return()
          }

          if (nrow(vh_data) == 0) {
            showNotification("No data for selected methods and sensor position", type = "warning")
            return()
          }

          # Apply flux density conversion
          wood <- wood_properties()

          # Convert using sapfluxr function
          flux_data <- vh_data
          flux_data$Jv_cm3_cm2_hr <- sapfluxr::calc_sap_flux_density(
            Vh = vh_data[[velocity_col]],
            wood_properties = wood
          )

          rv$flux_data <- flux_data
          rv$velocity_source_used <- flux_data$data_source[1]  # Get from data
          rv$methods_used <- input$methods_selected
          rv$sensor_position_used <- input$sensor_position
          rv$conversion_timestamp <- Sys.time()

          showNotification(
            sprintf("Flux density conversion successful! %d methods, %s sensor, %d records",
                    length(input$methods_selected), input$sensor_position, nrow(flux_data)),
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

          # Apply sap flux integration
          wood <- wood_properties()
          tree_water_use_data <- sapfluxr::apply_sap_flux_integration(
            flux_data = flux_with_dims,
            wood_properties = wood,
            integration_method = input$integration_method,
            verbose = FALSE
          )

          # Store results
          rv$tree_water_use_data <- tree_water_use_data
          rv$tree_dimensions <- list(
            dbh = input$dbh_cm,
            sapwood_depth = input$sapwood_depth_cm,
            bark_thickness = input$bark_thickness_cm
          )
          rv$integration_timestamp <- Sys.time()

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

      sprintf(
        "Flux Density Conversion Summary\n\n" +
        "Source: %s\n" +
        "Converted: %s\n\n" +
        "Data Points: %d\n" +
        "Date Range: %s to %s\n\n" +
        "Sap Flux Density (Jv) Statistics:\n" +
        "  Mean: %.3f cm³/cm²/hr\n" +
        "  Median: %.3f cm³/cm²/hr\n" +
        "  Min: %.3f cm³/cm²/hr\n" +
        "  Max: %.3f cm³/cm²/hr\n" +
        "  SD: %.3f cm³/cm²/hr",
        rv$velocity_source_used,
        format(rv$conversion_timestamp, "%Y-%m-%d %H:%M:%S"),
        nrow(flux),
        format(min(flux$datetime), "%Y-%m-%d"),
        format(max(flux$datetime), "%Y-%m-%d"),
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
        "Flux density data active (%d records)\n" +
        "Source: %s\n" +
        "Converted: %s",
        nrow(rv$flux_data),
        rv$velocity_source_used,
        format(rv$conversion_timestamp, "%Y-%m-%d %H:%M:%S")
      )
    })

    # Flux timeseries plot
    output$flux_timeseries_plot <- plotly::renderPlotly({
      req(rv$flux_data)

      flux <- rv$flux_data

      # Sample if too many points
      if (nrow(flux) > 10000) {
        sample_idx <- seq(1, nrow(flux), length.out = 10000)
        flux <- flux[sample_idx, ]
      }

      # Create plot - handle both sDMA and regular data
      if ("combination" %in% names(flux) && "sdma_source" %in% names(flux)) {
        # sDMA data - color by combination:sdma_source
        flux <- flux %>%
          dplyr::mutate(
            trace_name = paste0(combination, ": ", sdma_source)
          )

        fig <- plotly::plot_ly(
          data = flux,
          x = ~datetime,
          y = ~Jv_cm3_cm2_hr,
          color = ~trace_name,
          type = "scatter",
          mode = "lines",
          line = list(width = 1.5),
          hovertemplate = paste(
            "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
            "<b>Jv:</b> %{y:.3f} cm³/cm²/hr<br>",
            "<b>Method:</b> %{customdata}<br>",
            "<extra></extra>"
          ),
          customdata = ~trace_name
        )
      } else if ("method" %in% names(flux)) {
        # Regular data - color by method
        fig <- plotly::plot_ly(
          data = flux,
          x = ~datetime,
          y = ~Jv_cm3_cm2_hr,
          color = ~method,
          type = "scatter",
          mode = "lines",
          line = list(width = 1.5),
          hovertemplate = paste(
            "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
            "<b>Jv:</b> %{y:.3f} cm³/cm²/hr<br>",
            "<b>Method:</b> %{customdata}<br>",
            "<extra></extra>"
          ),
          customdata = ~method
        )
      } else {
        # Fallback - single trace
        fig <- plotly::plot_ly(
          data = flux,
          x = ~datetime,
          y = ~Jv_cm3_cm2_hr,
          type = "scatter",
          mode = "lines",
          name = "Sap Flux Density",
          line = list(color = "darkgreen", width = 1.5),
          hovertemplate = paste(
            "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
            "<b>Jv:</b> %{y:.3f} cm³/cm²/hr<br>",
            "<extra></extra>"
          )
        )
      }

      fig <- fig %>%
        plotly::layout(
          title = paste("Sap Flux Density -", rv$sensor_position_used, "Sensor"),
          xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "lightgray"),
          yaxis = list(title = "Sap Flux Density (cm³/cm²/hr)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          legend = list(orientation = "h", y = -0.2),
          margin = list(l = 70, r = 40, t = 60, b = 80)
        )

      return(fig)
    })

    # Velocity vs Flux comparison plot
    output$velocity_vs_flux_plot <- plotly::renderPlotly({
      req(rv$flux_data)

      flux <- rv$flux_data

      # Sample if too many points
      if (nrow(flux) > 5000) {
        sample_idx <- seq(1, nrow(flux), length.out = 5000)
        flux <- flux[sample_idx, ]
      }

      # Calculate Z factor
      Z <- mean(flux$Jv_cm3_cm2_hr / flux$Vh_cm_hr, na.rm = TRUE)

      # Create scatter plot
      fig <- plotly::plot_ly(
        data = flux,
        x = ~Vh_cm_hr,
        y = ~Jv_cm3_cm2_hr,
        type = "scatter",
        mode = "markers",
        marker = list(
          color = "steelblue",
          size = 4,
          opacity = 0.6
        ),
        hovertemplate = paste(
          "<b>Vh:</b> %{x:.3f} cm/hr<br>",
          "<b>Jv:</b> %{y:.3f} cm³/cm²/hr<br>",
          "<extra></extra>"
        )
      )

      # Add reference line (Jv = Z × Vh)
      vh_range <- range(flux$Vh_cm_hr, na.rm = TRUE)
      fig <- fig %>%
        plotly::add_trace(
          x = vh_range,
          y = vh_range * Z,
          type = "scatter",
          mode = "lines",
          name = sprintf("Jv = %.4f × Vh", Z),
          line = list(color = "darkred", width = 2, dash = "dash"),
          hoverinfo = "skip"
        )

      fig <- fig %>%
        plotly::layout(
          title = "Velocity vs Flux Density",
          xaxis = list(title = "Heat Pulse Velocity (cm/hr)", showgrid = TRUE, gridcolor = "lightgray"),
          yaxis = list(title = "Sap Flux Density (cm³/cm²/hr)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          showlegend = TRUE,
          legend = list(x = 0.02, y = 0.98),
          margin = list(l = 70, r = 40, t = 60, b = 60)
        )

      return(fig)
    })

    # Daily flux totals plot
    output$daily_flux_plot <- plotly::renderPlotly({
      req(rv$flux_data)

      flux <- rv$flux_data

      # Calculate daily totals
      flux$date <- as.Date(flux$datetime)
      daily_totals <- aggregate(Jv_cm3_cm2_hr ~ date, data = flux, FUN = sum, na.rm = TRUE)

      fig <- plotly::plot_ly(
        data = daily_totals,
        x = ~date,
        y = ~Jv_cm3_cm2_hr,
        type = "bar",
        marker = list(color = "darkgreen"),
        hovertemplate = paste(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>Daily Total:</b> %{y:.2f} cm³/cm²/day<br>",
          "<extra></extra>"
        )
      )

      fig <- fig %>%
        plotly::layout(
          title = "Daily Sap Flux Totals",
          xaxis = list(title = "Date", showgrid = FALSE),
          yaxis = list(title = "Daily Flux (cm³/cm²/day)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          margin = list(l = 70, r = 40, t = 60, b = 60)
        )

      return(fig)
    })

    # Tree water use - Hourly plot (L/hr)
    output$tree_water_use_plot_hourly <- plotly::renderPlotly({
      req(rv$tree_water_use_data)

      q_data <- rv$tree_water_use_data

      # Sample if too many points
      if (nrow(q_data) > 5000) {
        sample_idx <- seq(1, nrow(q_data), length.out = 5000)
        q_data <- q_data[sample_idx, ]
      }

      fig <- plotly::plot_ly(
        data = q_data,
        x = ~datetime,
        y = ~Q_L_hr,
        type = "scatter",
        mode = "lines",
        name = "Tree Water Use",
        line = list(color = "darkblue", width = 1.5),
        hovertemplate = paste(
          "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
          "<b>Q:</b> %{y:.3f} L/hr<br>",
          "<extra></extra>"
        )
      )

      fig <- fig %>%
        plotly::layout(
          title = sprintf("Tree Water Use (DBH: %.1f cm, Sapwood: %.1f cm)",
                         rv$tree_dimensions$dbh, rv$tree_dimensions$sapwood_depth),
          xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "lightgray"),
          yaxis = list(title = "Water Use (L/hr)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          margin = list(l = 70, r = 40, t = 60, b = 60)
        )

      return(fig)
    })

    # Tree water use - Daily plot (L/day)
    output$tree_water_use_plot_daily <- plotly::renderPlotly({
      req(rv$tree_water_use_data)

      q_data <- rv$tree_water_use_data

      # Calculate daily totals
      q_data$date <- as.Date(q_data$datetime)
      daily_totals <- aggregate(Q_L_day ~ date, data = q_data, FUN = mean, na.rm = TRUE)

      fig <- plotly::plot_ly(
        data = daily_totals,
        x = ~date,
        y = ~Q_L_day,
        type = "bar",
        marker = list(color = "darkblue"),
        hovertemplate = paste(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>Daily Total:</b> %{y:.2f} L/day<br>",
          "<extra></extra>"
        )
      )

      fig <- fig %>%
        plotly::layout(
          title = sprintf("Daily Tree Water Use (DBH: %.1f cm)",
                         rv$tree_dimensions$dbh),
          xaxis = list(title = "Date", showgrid = FALSE),
          yaxis = list(title = "Daily Water Use (L/day)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          margin = list(l = 70, r = 40, t = 60, b = 60)
        )

      return(fig)
    })

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
