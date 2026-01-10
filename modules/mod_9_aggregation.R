# mod_9_aggregation.R
# Module for Temporal Aggregation and Tree Water Use
#
# Tab 9: Visualise (Aggregated)
# Daily/Hourly Flux Density plots and Whole Tree Water Use calculations

# UI ----
aggregationUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Controls
      column(
        width = 3,
        box(
          width = NULL,
          title = "Aggregation Settings",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          h5("Temporal Aggregation"),
          selectInput(
            ns("aggregation_period"),
            "Aggregation Period:",
            choices = c(
              "Hourly" = "hourly",
              "Daily" = "daily",
              "Weekly" = "weekly"
            ),
            selected = "daily"
          ),

          hr(),

          h5("Aggregation Function"),
          selectInput(
            ns("aggregation_function"),
            "Summary Function:",
            choices = c(
              "Mean" = "mean",
              "Sum" = "sum",
              "Median" = "median",
              "Maximum" = "max",
              "Minimum" = "min"
            ),
            selected = "mean"
          ),

          hr(),

          h5("Plot Type"),
          radioButtons(
            ns("plot_type"),
            NULL,
            choices = c(
              "Time Series" = "timeseries",
              "Bar Chart" = "bar",
              "Heatmap" = "heatmap"
            ),
            selected = "timeseries"
          ),

          hr(),

          actionButton(
            ns("calculate_aggregation"),
            "Calculate Aggregation",
            icon = icon("calculator"),
            class = "btn-primary btn-block"
          )
        ),

        # Summary statistics box
        box(
          width = NULL,
          title = "Summary Statistics",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          htmlOutput(ns("summary_stats"))
        )
      ),

      # Right column: Plots
      column(
        width = 9,

        # Aggregated flux density plot
        box(
          width = NULL,
          title = "Aggregated Flux Density",
          status = "primary",
          solidHeader = TRUE,

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("aggregation_plot"), height = "500px"),
            type = 6,
            color = "#3c8dbc"
          )
        ),

        # Data table
        box(
          width = NULL,
          title = "Aggregated Data",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          DT::DTOutput(ns("aggregated_table"))
        )
      )
    )
  )
}

# Server ----
aggregationServer <- function(id,
                               flux_density_data,
                               code_tracker = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Aggregated data
    # Re-calculates when button clicked OR when aggregation settings change
    aggregated_data <- reactive({
      # Require button to be clicked at least once
      req(input$calculate_aggregation > 0)
      req(flux_density_data())

      # Make reactive to these inputs so plot updates when changed
      req(input$aggregation_period)
      req(input$aggregation_function)

      data <- flux_density_data()

      # Determine aggregation period
      if (input$aggregation_period == "hourly") {
        data <- data %>%
          dplyr::mutate(period = lubridate::floor_date(datetime, "hour"))
      } else if (input$aggregation_period == "daily") {
        data <- data %>%
          dplyr::mutate(period = lubridate::floor_date(datetime, "day"))
      } else if (input$aggregation_period == "weekly") {
        data <- data %>%
          dplyr::mutate(period = lubridate::floor_date(datetime, "week"))
      }

      # Apply aggregation function
      agg_func <- switch(input$aggregation_function,
                         "mean" = mean,
                         "sum" = sum,
                         "median" = median,
                         "max" = max,
                         "min" = min,
                         mean)

      # Aggregate across all sensors and methods
      # Group by period and method_label if available
      if ("method_label" %in% names(data)) {
        aggregated <- data %>%
          dplyr::group_by(period, method_label) %>%
          dplyr::summarise(
            flux_density = agg_func(Jv_cm3_cm2_hr, na.rm = TRUE),
            n_points = dplyr::n(),
            .groups = "drop"
          )
      } else {
        aggregated <- data %>%
          dplyr::group_by(period) %>%
          dplyr::summarise(
            flux_density = agg_func(Jv_cm3_cm2_hr, na.rm = TRUE),
            n_points = dplyr::n(),
            .groups = "drop"
          )
      }

      return(aggregated)
    })

    # Aggregation plot
    output$aggregation_plot <- plotly::renderPlotly({
      req(aggregated_data())

      data <- aggregated_data()

      if (nrow(data) == 0) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              title = "No data to display",
              xaxis = list(title = "Time Period"),
              yaxis = list(title = "Flux Density (g/cm²/hr)")
            )
        )
      }

      # Create plot based on plot type
      # Color by method_label if available
      has_method_label <- "method_label" %in% names(data)

      if (input$plot_type == "timeseries") {
        if (has_method_label) {
          p <- plotly::plot_ly(data,
                               x = ~period,
                               y = ~flux_density,
                               color = ~method_label,
                               type = 'scatter',
                               mode = 'lines+markers')
        } else {
          p <- plotly::plot_ly(data,
                               x = ~period,
                               y = ~flux_density,
                               type = 'scatter',
                               mode = 'lines+markers',
                               marker = list(color = 'steelblue'))
        }

        p <- p %>%
          plotly::layout(
            title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                           tools::toTitleCase(input$aggregation_function),
                           " Flux Density"),
            xaxis = list(title = "Time Period"),
            yaxis = list(title = "Flux Density (cm³/cm²/hr)"),
            hovermode = 'x unified',
            legend = list(orientation = "h", y = -0.15)
          )
      } else if (input$plot_type == "bar") {
        if (has_method_label) {
          p <- plotly::plot_ly(data,
                               x = ~period,
                               y = ~flux_density,
                               color = ~method_label,
                               type = 'bar')
        } else {
          p <- plotly::plot_ly(data,
                               x = ~period,
                               y = ~flux_density,
                               type = 'bar',
                               marker = list(color = 'steelblue'))
        }

        p <- p %>%
          plotly::layout(
            title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                           tools::toTitleCase(input$aggregation_function),
                           " Flux Density"),
            xaxis = list(title = "Time Period"),
            yaxis = list(title = "Flux Density (cm³/cm²/hr)"),
            barmode = 'group',
            legend = list(orientation = "h", y = -0.15)
          )
      } else {
        # Heatmap - use method if available, otherwise just show flux_density over time
        if (has_method_label) {
          p <- plotly::plot_ly(
            data,
            x = ~period,
            y = ~method_label,
            z = ~flux_density,
            type = 'heatmap',
            colorscale = 'Viridis'
          ) %>%
            plotly::layout(
              title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                             "Flux Density Heatmap"),
              xaxis = list(title = "Time Period"),
              yaxis = list(title = "Method")
            )
        } else {
          # Simplified heatmap for single method
          p <- plotly::plot_ly(
            data,
            x = ~period,
            y = 1,
            z = ~flux_density,
            type = 'heatmap',
            colorscale = 'Viridis'
          ) %>%
            plotly::layout(
              title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                             "Flux Density Over Time"),
              xaxis = list(title = "Time Period"),
              yaxis = list(title = "", showticklabels = FALSE)
            )
        }
      }

      return(p)
    })

    # Summary statistics
    output$summary_stats <- renderUI({
      req(aggregated_data())

      data <- aggregated_data()

      stats_html <- "<table style='width:100%; font-size:0.9em;'>"
      stats_html <- paste0(stats_html, "<thead><tr>",
                           "<th>Statistic</th>",
                           "<th>Value</th>",
                           "</tr></thead><tbody>")

      # Calculate summary statistics
      total_periods <- nrow(data)
      mean_flux <- mean(data$flux_density, na.rm = TRUE)
      median_flux <- median(data$flux_density, na.rm = TRUE)
      min_flux <- min(data$flux_density, na.rm = TRUE)
      max_flux <- max(data$flux_density, na.rm = TRUE)

      stats_html <- paste0(stats_html,
                           "<tr><td><strong>Total Periods</strong></td><td>", total_periods, "</td></tr>",
                           "<tr><td><strong>Mean Flux Density</strong></td><td>",
                           sprintf("%.3f g/cm²/hr", mean_flux), "</td></tr>",
                           "<tr><td><strong>Median Flux Density</strong></td><td>",
                           sprintf("%.3f g/cm²/hr", median_flux), "</td></tr>",
                           "<tr><td><strong>Min Flux Density</strong></td><td>",
                           sprintf("%.3f g/cm²/hr", min_flux), "</td></tr>",
                           "<tr><td><strong>Max Flux Density</strong></td><td>",
                           sprintf("%.3f g/cm²/hr", max_flux), "</td></tr>")

      stats_html <- paste0(stats_html, "</tbody></table>")

      HTML(stats_html)
    })

    # Data table
    output$aggregated_table <- DT::renderDT({
      req(aggregated_data())

      data <- aggregated_data()

      DT::datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("flux_density"), digits = 4)
    })

    # Code generation
    observe({
      if (!isTRUE(code_tracker)) {
        if (!is.null(input$aggregation_period) && !is.null(input$aggregation_function)) {
          code_tracker$add_step(
            step_name = "Temporal Aggregation",
            code = sprintf(
              "# Aggregate flux density data (%s %s)",
              input$aggregation_period,
              input$aggregation_function
            )
          )
        }
      }
    })

  })
}
