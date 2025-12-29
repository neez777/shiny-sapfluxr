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

          h5("Sensor Position"),
          checkboxGroupInput(
            ns("sensor_position"),
            NULL,
            choices = c("Inner" = "inner", "Outer" = "outer"),
            selected = c("inner", "outer")
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

        # Tree water use box
        box(
          width = NULL,
          title = "Whole Tree Water Use",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          helpText("Calculate tree-level water use from flux density and sapwood area"),

          numericInput(
            ns("sapwood_area"),
            "Sapwood Area (cm²):",
            value = 100,
            min = 1,
            max = 10000,
            step = 10
          ),

          helpText("Or calculate from DBH and sapwood depth:"),

          numericInput(
            ns("dbh"),
            "DBH (cm):",
            value = NULL,
            min = 1,
            max = 200,
            step = 1
          ),

          numericInput(
            ns("sapwood_depth"),
            "Sapwood Depth (cm):",
            value = NULL,
            min = 0.1,
            max = 50,
            step = 0.5
          ),

          actionButton(
            ns("calculate_sapwood_area"),
            "Calculate from DBH",
            icon = icon("tree"),
            class = "btn-success btn-block"
          ),

          hr(),

          actionButton(
            ns("calculate_water_use"),
            "Calculate Tree Water Use",
            icon = icon("droplet"),
            class = "btn-success btn-block"
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

        # Tree water use plot
        box(
          width = NULL,
          title = "Whole Tree Water Use",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("water_use_plot"), height = "500px"),
            type = 6,
            color = "#00a65a"
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
                               code_tracker = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Aggregated data
    aggregated_data <- eventReactive(input$calculate_aggregation, {
      req(flux_density_data())

      data <- flux_density_data()

      # Filter by sensor position
      if (!is.null(input$sensor_position) && length(input$sensor_position) > 0) {
        data <- data %>%
          dplyr::filter(sensor_position %in% input$sensor_position)
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

      # Aggregate
      aggregated <- data %>%
        dplyr::group_by(period, sensor_position) %>%
        dplyr::summarise(
          flux_density = agg_func(flux_density, na.rm = TRUE),
          n_points = dplyr::n(),
          .groups = "drop"
        )

      return(aggregated)
    })

    # Reactive: Sapwood area calculation
    observeEvent(input$calculate_sapwood_area, {
      req(input$dbh, input$sapwood_depth)

      # Calculate sapwood area from DBH and sapwood depth
      # Sapwood area = π × [(DBH/2)² - (DBH/2 - sapwood_depth)²]
      r_outer <- input$dbh / 2
      r_inner <- r_outer - input$sapwood_depth

      sapwood_area <- pi * (r_outer^2 - r_inner^2)

      updateNumericInput(session, "sapwood_area", value = round(sapwood_area, 2))

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Sapwood Area Calculated",
        text = sprintf("Sapwood area: %.2f cm²", sapwood_area),
        type = "success"
      )
    })

    # Reactive: Tree water use data
    water_use_data <- eventReactive(input$calculate_water_use, {
      req(aggregated_data(), input$sapwood_area)

      data <- aggregated_data()
      sapwood_area <- input$sapwood_area

      # Calculate water use (L/period)
      # Flux density is in g/cm²/hr
      # Water use = flux_density × sapwood_area × time_period / 1000 (to convert g to L)

      time_multiplier <- switch(input$aggregation_period,
                                "hourly" = 1,
                                "daily" = 24,
                                "weekly" = 24 * 7,
                                1)

      water_use <- data %>%
        dplyr::mutate(
          water_use_L = (flux_density * sapwood_area * time_multiplier) / 1000
        )

      return(water_use)
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
      if (input$plot_type == "timeseries") {
        p <- plotly::plot_ly(data,
                             x = ~period,
                             y = ~flux_density,
                             color = ~sensor_position,
                             type = 'scatter',
                             mode = 'lines+markers') %>%
          plotly::layout(
            title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                           tools::toTitleCase(input$aggregation_function),
                           " Flux Density"),
            xaxis = list(title = "Time Period"),
            yaxis = list(title = "Flux Density (g/cm²/hr)"),
            hovermode = 'x unified'
          )
      } else if (input$plot_type == "bar") {
        p <- plotly::plot_ly(data,
                             x = ~period,
                             y = ~flux_density,
                             color = ~sensor_position,
                             type = 'bar') %>%
          plotly::layout(
            title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                           tools::toTitleCase(input$aggregation_function),
                           " Flux Density"),
            xaxis = list(title = "Time Period"),
            yaxis = list(title = "Flux Density (g/cm²/hr)"),
            barmode = 'group'
          )
      } else {
        # Heatmap
        p <- plotly::plot_ly(
          data,
          x = ~period,
          y = ~sensor_position,
          z = ~flux_density,
          type = 'heatmap',
          colorscale = 'Viridis'
        ) %>%
          plotly::layout(
            title = paste0(tools::toTitleCase(input$aggregation_period), " ",
                           "Flux Density Heatmap"),
            xaxis = list(title = "Time Period"),
            yaxis = list(title = "Sensor Position")
          )
      }

      return(p)
    })

    # Water use plot
    output$water_use_plot <- plotly::renderPlotly({
      req(water_use_data())

      data <- water_use_data()

      if (nrow(data) == 0) {
        return(
          plotly::plot_ly() %>%
            plotly::layout(
              title = "No data to display",
              xaxis = list(title = "Time Period"),
              yaxis = list(title = "Water Use (L)")
            )
        )
      }

      p <- plotly::plot_ly(data,
                           x = ~period,
                           y = ~water_use_L,
                           color = ~sensor_position,
                           type = 'scatter',
                           mode = 'lines+markers',
                           fill = 'tozeroy') %>%
        plotly::layout(
          title = paste0("Whole Tree Water Use (",
                         tools::toTitleCase(input$aggregation_period), ")"),
          xaxis = list(title = "Time Period"),
          yaxis = list(title = paste0("Water Use (L/",
                                       input$aggregation_period, ")")),
          hovermode = 'x unified'
        )

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

      # If water use is calculated, add those stats
      if (!is.null(water_use_data()) && nrow(water_use_data()) > 0) {
        wu_data <- water_use_data()
        total_water_use <- sum(wu_data$water_use_L, na.rm = TRUE)
        mean_water_use <- mean(wu_data$water_use_L, na.rm = TRUE)

        stats_html <- paste0(stats_html,
                             "<tr><td colspan='2'><hr></td></tr>",
                             "<tr><td><strong>Total Water Use</strong></td><td>",
                             sprintf("%.2f L", total_water_use), "</td></tr>",
                             "<tr><td><strong>Mean Water Use</strong></td><td>",
                             sprintf("%.2f L/%s", mean_water_use, input$aggregation_period),
                             "</td></tr>")
      }

      stats_html <- paste0(stats_html, "</tbody></table>")

      HTML(stats_html)
    })

    # Data table
    output$aggregated_table <- DT::renderDT({
      req(aggregated_data())

      data <- aggregated_data()

      if (!is.null(water_use_data()) && nrow(water_use_data()) > 0) {
        data <- water_use_data()
      }

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
      if (!is.null(code_tracker)) {
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

        if (!is.null(input$calculate_water_use) && input$calculate_water_use > 0) {
          code_tracker$add_step(
            step_name = "Tree Water Use",
            code = sprintf(
              "# Calculate whole tree water use (sapwood area: %.2f cm²)",
              input$sapwood_area
            )
          )
        }
      }
    })

  })
}
