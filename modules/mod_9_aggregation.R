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

          h5("Data Type"),
          selectInput(
            ns("data_type"),
            "Data to Aggregate:",
            choices = c(
              "Flux Density" = "flux_density",
              "Tree Water Use" = "water_use"
            ),
            selected = "flux_density"
          ),

          hr(),

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

        # Aggregated data plot
        box(
          width = NULL,
          title = "Aggregated Data",
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
                               tree_water_use_data = NULL,
                               code_tracker = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Aggregated data
    # Re-calculates when button clicked OR when aggregation settings change
    aggregated_data <- reactive({
      # Require button to be clicked at least once
      req(input$calculate_aggregation > 0)
      req(input$data_type)

      # Make reactive to these inputs so plot updates when changed
      req(input$aggregation_period)
      req(input$aggregation_function)

      # Get appropriate data based on data_type
      if (input$data_type == "flux_density") {
        req(flux_density_data())
        data <- flux_density_data()
        value_col <- "Jv_cm3_cm2_hr"
      } else {
        # water_use
        req(tree_water_use_data())
        data <- tree_water_use_data()
        value_col <- "Q_L_hr"
      }

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
            aggregated_value = agg_func(.data[[value_col]], na.rm = TRUE),
            n_points = dplyr::n(),
            .groups = "drop"
          )
      } else {
        aggregated <- data %>%
          dplyr::group_by(period) %>%
          dplyr::summarise(
            aggregated_value = agg_func(.data[[value_col]], na.rm = TRUE),
            n_points = dplyr::n(),
            .groups = "drop"
          )
      }

      # Add data type attribute for plotting
      attr(aggregated, "data_type") <- input$data_type
      attr(aggregated, "value_col") <- value_col

      return(aggregated)
    })

    # Aggregation plot
    output$aggregation_plot <- plotly::renderPlotly({
      req(aggregated_data())

      data <- aggregated_data()
      data_type <- attr(data, "data_type")

      # Set labels based on data type
      if (data_type == "water_use") {
        data_label <- "Water Use"
        y_label <- "Water Use (L/hr)"
      } else {
        data_label <- "Flux Density"
        y_label <- "Flux Density (cm³/cm²/hr)"
      }

      if (nrow(data) == 0) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              title = "No data to display",
              xaxis = list(title = "Time Period"),
              yaxis = list(title = y_label),
              uirevision = "aggregation_zoom"
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
                               y = ~aggregated_value,
                               color = ~method_label,
                               type = 'scatter',
                               mode = 'lines+markers')
        } else {
          p <- plotly::plot_ly(data,
                               x = ~period,
                               y = ~aggregated_value,
                               type = 'scatter',
                               mode = 'lines+markers',
                               marker = list(color = 'steelblue'))
        }

        p <- p %>%
          plotly::layout(
            title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                           tools::toTitleCase(input$aggregation_function),
                           " ", data_label),
            xaxis = list(title = "Time Period"),
            yaxis = list(title = y_label),
            hovermode = 'x unified',
            legend = list(orientation = "h", y = -0.15),
            uirevision = "aggregation_zoom"
          )
      } else if (input$plot_type == "bar") {
        if (has_method_label) {
          p <- plotly::plot_ly(data,
                               x = ~period,
                               y = ~aggregated_value,
                               color = ~method_label,
                               type = 'bar')
        } else {
          p <- plotly::plot_ly(data,
                               x = ~period,
                               y = ~aggregated_value,
                               type = 'bar',
                               marker = list(color = 'steelblue'))
        }

        p <- p %>%
          plotly::layout(
            title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                           tools::toTitleCase(input$aggregation_function),
                           " ", data_label),
            xaxis = list(title = "Time Period"),
            yaxis = list(title = y_label),
            barmode = 'group',
            legend = list(orientation = "h", y = -0.15),
            uirevision = "aggregation_zoom"
          )
      } else {
        # Heatmap - use method if available, otherwise just show aggregated_value over time
        if (has_method_label) {
          p <- plotly::plot_ly(
            data,
            x = ~period,
            y = ~method_label,
            z = ~aggregated_value,
            type = 'heatmap',
            colorscale = 'Viridis'
          ) %>%
            plotly::layout(
              title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                             data_label, " Heatmap"),
              xaxis = list(title = "Time Period"),
              yaxis = list(title = "Method"),
              uirevision = "aggregation_zoom"
            )
        } else {
          # Simplified heatmap for single method
          p <- plotly::plot_ly(
            data,
            x = ~period,
            y = 1,
            z = ~aggregated_value,
            type = 'heatmap',
            colorscale = 'Viridis'
          ) %>%
            plotly::layout(
              title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                             data_label, " Over Time"),
              xaxis = list(title = "Time Period"),
              yaxis = list(title = "", showticklabels = FALSE),
              uirevision = "aggregation_zoom"
            )
        }
      }

      return(p)
    })

    # Summary statistics
    output$summary_stats <- renderUI({
      req(aggregated_data())

      data <- aggregated_data()
      data_type <- attr(data, "data_type")

      # Set labels and units based on data type
      if (data_type == "water_use") {
        data_label <- "Water Use"
        unit_label <- "L/hr"
      } else {
        data_label <- "Flux Density"
        unit_label <- "cm³/cm²/hr"
      }

      stats_html <- "<table style='width:100%; font-size:0.9em;'>"
      stats_html <- paste0(stats_html, "<thead><tr>",
                           "<th>Statistic</th>",
                           "<th>Value</th>",
                           "</tr></thead><tbody>")

      # Calculate summary statistics
      total_periods <- nrow(data)
      mean_val <- mean(data$aggregated_value, na.rm = TRUE)
      median_val <- median(data$aggregated_value, na.rm = TRUE)
      min_val <- min(data$aggregated_value, na.rm = TRUE)
      max_val <- max(data$aggregated_value, na.rm = TRUE)

      stats_html <- paste0(stats_html,
                           "<tr><td><strong>Total Periods</strong></td><td>", total_periods, "</td></tr>",
                           "<tr><td><strong>Mean ", data_label, "</strong></td><td>",
                           sprintf("%.3f %s", mean_val, unit_label), "</td></tr>",
                           "<tr><td><strong>Median ", data_label, "</strong></td><td>",
                           sprintf("%.3f %s", median_val, unit_label), "</td></tr>",
                           "<tr><td><strong>Min ", data_label, "</strong></td><td>",
                           sprintf("%.3f %s", min_val, unit_label), "</td></tr>",
                           "<tr><td><strong>Max ", data_label, "</strong></td><td>",
                           sprintf("%.3f %s", max_val, unit_label), "</td></tr>")

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
        DT::formatRound(columns = c("aggregated_value"), digits = 4)
    })

    # Code generation
    observe({
      if (!isTRUE(code_tracker)) {
        if (!is.null(input$aggregation_period) &&
            !is.null(input$aggregation_function) &&
            !is.null(input$data_type)) {

          data_label <- if (input$data_type == "water_use") "water use" else "flux density"

          code_tracker$add_step(
            step_name = "Temporal Aggregation",
            code = sprintf(
              "# Aggregate %s data (%s %s)",
              data_label,
              input$aggregation_period,
              input$aggregation_function
            )
          )
        }
      }
    })

  })
}
