# mod_flux_density.R
# Module for Sap Flux Density Conversion
#
# Converts corrected heat pulse velocity to sap flux density using
# wood-specific conversion factors from Burgess et al. (2001)

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
            " Select which velocity data to convert. Use the most corrected version available."
          ),

          radioButtons(
            ns("velocity_source"),
            "Velocity Data Source:",
            choices = c(
              "Wound-Corrected (recommended)" = "wound",
              "Spacing-Corrected" = "spacing",
              "Raw HPV (uncorrected)" = "raw"
            ),
            selected = "wound"
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
            "Convert to Sap Flux Density",
            icon = icon("exchange-alt"),
            class = "btn-primary",
            width = "100%"
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

      # Right column: Results and Visualization
      column(
        width = 8,

        # Flux density time series plot
        box(
          width = 12,
          title = "Sap Flux Density Time Series",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_results")),
            p(em("No flux density data yet. Convert velocity data to see results."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_results")),

            helpText("Interactive plot of sap flux density over time. Hover for details, click-drag to zoom."),

            fluidRow(
              column(6,
                selectInput(
                  ns("plot_sensor"),
                  "Sensor:",
                  choices = c("Outer" = "outer", "Inner" = "inner"),
                  selected = "outer"
                )
              ),
              column(6,
                selectInput(
                  ns("plot_method"),
                  "Method:",
                  choices = character(0),  # Populated dynamically
                  selected = NULL
                )
              )
            ),

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
                               wood_properties = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
      flux_data = NULL,
      velocity_source_used = NULL,
      conversion_timestamp = NULL
    )

    # Display Z factor from wood properties
    output$z_factor_display <- renderText({
      req(wood_properties())

      wood <- wood_properties()
      if (inherits(wood, "WoodProperties")) {
        Z <- wood$derived_properties$sap_flux_conversion_factor
        if (!is.null(Z) && !is.na(Z)) {
          sprintf(
            "Z factor: %.4f\n\n" +
            "Interpretation:\n" +
            "Jv = %.4f × Vh\n\n" +
            "For every 1 cm/hr of heat pulse velocity,\n" +
            "sap flux density is %.4f cm³/cm²/hr",
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
      has_raw <- !is.null(vh_raw())
      has_spacing <- !is.null(vh_spacing_corrected())
      has_wound <- !is.null(vh_wound_corrected())

      status_items <- tagList(
        h5("Available Data:"),
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
            " Wound-Corrected"
          )
        )
      )

      return(status_items)
    })

    # Convert to flux density
    observeEvent(input$convert_to_flux, {
      req(wood_properties())

      # Select velocity data source
      vh_data <- NULL
      source_name <- NULL

      if (input$velocity_source == "wound" && !is.null(vh_wound_corrected())) {
        vh_data <- vh_wound_corrected()
        source_name <- "Wound-Corrected"
      } else if (input$velocity_source == "spacing" && !is.null(vh_spacing_corrected())) {
        vh_data <- vh_spacing_corrected()
        source_name <- "Spacing-Corrected"
      } else if (!is.null(vh_raw())) {
        vh_data <- vh_raw()
        source_name <- "Raw HPV"
      }

      if (is.null(vh_data)) {
        showNotification("No velocity data available for selected source", type = "error")
        return()
      }

      withProgress(message = "Converting to sap flux density...", {

        tryCatch({
          # Apply flux density conversion
          wood <- wood_properties()

          # Convert using sapfluxr function
          flux_data <- vh_data
          flux_data$Jv_cm3_cm2_hr <- sapfluxr::calc_sap_flux_density(
            Vh = vh_data$Vh_cm_hr,
            wood_properties = wood
          )

          rv$flux_data <- flux_data
          rv$velocity_source_used <- source_name
          rv$conversion_timestamp <- Sys.time()

          showNotification("Flux density conversion successful!", type = "message")

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
      if (nrow(flux) > 5000) {
        sample_idx <- seq(1, nrow(flux), length.out = 5000)
        flux <- flux[sample_idx, ]
      }

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

      fig <- fig %>%
        plotly::layout(
          title = paste("Sap Flux Density -", rv$velocity_source_used),
          xaxis = list(title = "Date", showgrid = TRUE, gridcolor = "lightgray"),
          yaxis = list(title = "Sap Flux Density (cm³/cm²/hr)", showgrid = TRUE, gridcolor = "lightgray"),
          hovermode = "closest",
          margin = list(l = 70, r = 40, t = 60, b = 60)
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
      velocity_source = reactive(rv$velocity_source_used)
    ))
  })
}
