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
              "Weekly" = "weekly",
              "Monthly" = "monthly"
            ),
            selected = "daily"
          ),

          hr(),

          h5("Plot Type"),
          radioButtons(
            ns("plot_type"),
            NULL,
            choices = c(
              "Time Series" = "timeseries",
              "Bar Chart" = "bar"
            ),
            selected = "timeseries"
          ),

          conditionalPanel(
            condition = sprintf("input['%s'] == 'timeseries' || input['%s'] == 'bar'",
                               ns("plot_type"), ns("plot_type")),
            checkboxInput(
              ns("show_cumulative"),
              "Show Cumulative",
              value = FALSE
            )
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
                               code_tracker = NULL,
                               plot_settings = reactive(list())) {
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
      
      # We now default to 'sum' for all temporal aggregations
      # as it represents the physical total (integral) for the period.
      agg_func_name <- "sum"

      # Get appropriate data based on data_type
      if (input$data_type == "flux_density") {
        req(flux_density_data())
        data <- flux_density_data()
        value_col <- "Jv_cm3_cm2_hr"
      } else {
        # water_use
        req(tree_water_use_data())
        data <- tree_water_use_data()
        value_col <- "Q_total_L_hr"
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
      } else if (input$aggregation_period == "monthly") {
        data <- data %>%
          dplyr::mutate(period = lubridate::floor_date(datetime, "month"))
      }

      # Detect measurement interval (hours) from unique timestamps
      # This is critical for 'sum' aggregation (Total volume = sum(rate) * delta_t)
      unique_dts <- sort(unique(data$datetime))
      delta_t <- 1 # Default fallback
      if (length(unique_dts) > 1) {
        dt_diffs <- as.numeric(difftime(unique_dts[-1], unique_dts[-length(unique_dts)], units = "hours"))
        # Use median to be robust to gaps
        delta_t <- median(dt_diffs[dt_diffs > 0], na.rm = TRUE)
      }

      # Apply aggregation function (Integrated sum)
      agg_func <- function(x, na.rm = TRUE) sum(x, na.rm = na.rm) * delta_t

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

      # Add metadata for plotting
      attr(aggregated, "data_type") <- input$data_type
      attr(aggregated, "value_col") <- value_col
      attr(aggregated, "agg_func") <- agg_func_name
      attr(aggregated, "agg_period") <- input$aggregation_period
      attr(aggregated, "delta_t") <- delta_t

      return(aggregated)
    })

    # Aggregation plot
    output$aggregation_plot <- plotly::renderPlotly({
      req(aggregated_data())

      data <- aggregated_data()
      data_type <- attr(data, "data_type")
      agg_func_name <- attr(data, "agg_func")
      agg_period_name <- attr(data, "agg_period")

      # Set labels based on data type and aggregation function
      if (data_type == "water_use") {
        data_label <- "Water Use"
        if (agg_func_name == "sum") {
          unit <- switch(agg_period_name, 
                         "hourly" = "L/hr", 
                         "daily" = "L/day", 
                         "weekly" = "L/week", 
                         "monthly" = "L/month",
                         "L")
          y_label <- paste0("Total Water Use (", unit, ")")
        } else {
          y_label <- "Water Use (L/hr)"
        }
      } else {
        data_label <- "Flux Density"
        if (agg_func_name == "sum") {
          unit <- switch(agg_period_name, 
                         "hourly" = "cm/hr", 
                         "daily" = "cm/day", 
                         "weekly" = "cm/week", 
                         "monthly" = "cm/month",
                         "cm")
          y_label <- paste0("Total Flux Density (", unit, ")")
        } else {
          y_label <- "Flux Density (cm³/cm²/hr)"
        }
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

      # Apply cumulative transform if requested
      show_cumulative <- isTRUE(input$show_cumulative) && input$plot_type %in% c("timeseries", "bar")
      if (show_cumulative) {
        if ("method_label" %in% names(data)) {
          data <- data %>%
            dplyr::group_by(method_label) %>%
            dplyr::arrange(period) %>%
            dplyr::mutate(aggregated_value = cumsum(aggregated_value)) %>%
            dplyr::ungroup()
        } else {
          data <- data %>%
            dplyr::arrange(period) %>%
            dplyr::mutate(aggregated_value = cumsum(aggregated_value))
        }
        
        # Override y_label for cumulative
        if (data_type == "water_use") {
          y_label <- "Cumulative Water Use (L)"
        } else {
          y_label <- "Cumulative Flux (cm)"
        }
      }

      # Create plot based on plot type
      # Color by method_label if available
      has_method_label <- "method_label" %in% names(data)

      # Apply standard layout
      base_layout <- get_standard_layout(
        title = paste0(tools::toTitleCase(input$aggregation_period), " Total ",
                       data_label),
        xtitle = "Time Period",
        ytitle = y_label,
        uirevision = "aggregation_zoom"
      )
      
      style_config <- plot_settings()

      if (input$plot_type == "timeseries") {
        p <- plotly::plot_ly()
        
        if (has_method_label) {
          methods <- unique(data$method_label)
          for (m in methods) {
            method_data <- data %>% dplyr::filter(method_label == m)
            
            base_method <- m
            if (grepl("HRM", m)) base_method <- "HRM"
            else if (grepl("MHR", m)) base_method <- "MHR"
            else if (grepl("Tmax_Coh", m)) base_method <- "Tmax_Coh"
            else if (grepl("Tmax_Klu", m)) base_method <- "Tmax_Klu"
            
            style <- get_plot_style(method = base_method, sensor = "outer", is_corrected = TRUE, config = style_config)
            
            p <- p %>%
              plotly::add_trace(
                data = method_data,
                x = ~period,
                y = ~aggregated_value,
                name = m,
                type = 'scatter',
                mode = 'lines+markers',
                line = style,
                marker = list(size = 6, color = style$color)
              )
          }
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
            xaxis = base_layout$xaxis,
            yaxis = base_layout$yaxis,
            hovermode = 'x unified',
            showlegend = base_layout$showlegend,
            legend = base_layout$legend,
            plot_bgcolor = base_layout$plot_bgcolor,
            paper_bgcolor = base_layout$paper_bgcolor,
            uirevision = base_layout$uirevision,
            margin = base_layout$margin
          ) %>%
          plotly::event_register("plotly_relayout")
      } else {
        # Bar Chart
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
            xaxis = base_layout$xaxis,
            yaxis = base_layout$yaxis,
            barmode = 'group',
            showlegend = base_layout$showlegend,
            legend = base_layout$legend,
            plot_bgcolor = base_layout$plot_bgcolor,
            paper_bgcolor = base_layout$paper_bgcolor,
            uirevision = base_layout$uirevision,
            margin = base_layout$margin
          ) %>%
          plotly::event_register("plotly_relayout")
      }

      p <- p %>% apply_standard_plotly_config(filename = "aggregated_data_plot", add_csv_download = TRUE)

      return(p)
    })

    # Summary statistics
    output$summary_stats <- renderUI({
      req(aggregated_data())

      data <- aggregated_data()
      data_type <- attr(data, "data_type")
      agg_func_name <- attr(data, "agg_func")
      agg_period_name <- attr(data, "agg_period")

      # Set labels and units based on data type and aggregation function
      if (data_type == "water_use") {
        data_label <- "Water Use"
        if (agg_func_name == "sum") {
          unit_label <- switch(agg_period_name, 
                               "hourly" = "L/hr", 
                               "daily" = "L/day", 
                               "weekly" = "L/week", 
                               "monthly" = "L/month",
                               "L")
        } else {
          unit_label <- "L/hr"
        }
      } else {
        data_label <- "Flux Density"
        if (agg_func_name == "sum") {
          unit_label <- switch(agg_period_name, 
                               "hourly" = "cm/hr", 
                               "daily" = "cm/day", 
                               "weekly" = "cm/week", 
                               "monthly" = "cm/month",
                               "cm")
        } else {
          unit_label <- "cm³/cm²/hr"
        }
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
                           "<tr><td><strong>Average Total (per period)</strong></td><td>",
                           sprintf("%.3f %s", mean_val, unit_label), "</td></tr>",
                           "<tr><td><strong>Median Total</strong></td><td>",
                           sprintf("%.3f %s", median_val, unit_label), "</td></tr>",
                           "<tr><td><strong>Min Total</strong></td><td>",
                           sprintf("%.3f %s", min_val, unit_label), "</td></tr>",
                           "<tr><td><strong>Max Total</strong></td><td>",
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
      if (!is.null(code_tracker) && !is.logical(code_tracker)) {
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
