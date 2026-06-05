# mod_8b_flux_validation.R
# Module for Flux Density and Water Use Validation Visualisation
#
# Tab 8b: Flux Density & Water Use Validation
# Interactive time series to explore flux density and tree water use results

# UI ----
fluxValidationUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Controls
      column(
        width = 3,
        box(
          width = NULL,
          title = "Plot Controls",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          h5("Methods to Display"),
          helpText("Select specific methods to show"),
          uiOutput(ns("method_checkboxes")),

          hr(),

          h5("Sensor Position"),
          checkboxGroupInput(
            ns("sensor_position"),
            NULL,
            choices = c("Inner" = "inner", "Outer" = "outer"),
            selected = c("inner", "outer")
          ),

          hr(),

          h5("Time Range"),
          p(class = "help-text", style = "font-size: 0.9em; color: #666;",
            "Select date range to display"),

          shinyWidgets::airDatepickerInput(
            ns("start_datetime"),
            "Start Date/Time:",
            value = NULL,
            timepicker = TRUE,
            dateFormat = "yyyy-MM-dd HH:mm"
          ),

          shinyWidgets::airDatepickerInput(
            ns("end_datetime"),
            "End Date/Time:",
            value = NULL,
            timepicker = TRUE,
            dateFormat = "yyyy-MM-dd HH:mm"
          ),

          actionButton(
            ns("apply_range"),
            "Apply Time Range",
            icon = icon("clock"),
            class = "btn-primary",
            style = "width: 100%; margin-top: 5px;"
          ),

          hr(),

          h5("Display Options"),
          checkboxInput(
            ns("show_points"),
            "Show data points",
            value = FALSE
          ),

          hr(),

          actionButton(
            ns("reset_zoom"),
            "Reset to Full Range",
            icon = icon("refresh"),
            class = "btn-default",
            style = "width: 100%;"
          )
        )
      ),

      # Right column: Plots
      column(
        width = 9,

        # Flux density time series
        box(
          width = NULL,
          title = "Sap Flux Density Time Series (Jv)",
          status = "primary",
          solidHeader = TRUE,

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

        # Tree water use - Hourly
        box(
          width = NULL,
          title = "Tree Water Use - Hourly (L/hr)",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          helpText(
            icon("info-circle"),
            "Whole-tree water use integrated across sapwood area.",
            "Click-drag to zoom, double-click to reset zoom."
          ),

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("tree_water_use_plot_hourly"), height = "500px"),
            type = 6,
            color = "#00a65a"
          )
        ),

        # Tree water use - Daily
        box(
          width = NULL,
          title = "Tree Water Use - Daily (L/day)",
          status = "success",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,

          helpText(
            icon("info-circle"),
            "Daily totals of tree water use.",
            "Grouped bars show multiple methods side-by-side."
          ),

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("tree_water_use_plot_daily"), height = "500px"),
            type = 6,
            color = "#00a65a"
          )
        )
      )
    )
  )
}

# Server ----
fluxValidationServer <- function(id,
                                  flux_data,
                                  tree_water_use_data,
                                  tree_dimensions,
                                  code_tracker = NULL,
                                  plot_settings = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Time range
    time_range <- reactiveVal(NULL)

    # Initialize datetime range from data
    observe({
      req(flux_data())

      data <- flux_data()
      date_range <- range(data$datetime, na.rm = TRUE)

      shinyWidgets::updateAirDateInput(
        session, "start_datetime",
        value = date_range[1]
      )

      shinyWidgets::updateAirDateInput(
        session, "end_datetime",
        value = date_range[2]
      )
    })

    observeEvent(input$apply_range, {
      time_range(c(input$start_datetime, input$end_datetime))
    })

    # Dynamic method checkboxes based on available data
    output$method_checkboxes <- renderUI({
      req(flux_data())

      data <- flux_data()

      # Get all unique methods from method_label if available
      methods <- c()

      if ("method_label" %in% names(data)) {
        methods <- unique(data$method_label)
        methods <- methods[!is.na(methods)]
      } else if ("method" %in% names(data)) {
        methods <- unique(data$method)
        methods <- methods[!is.na(methods)]
      } else if ("combination" %in% names(data)) {
        # sDMA data
        methods <- unique(data$combination)
        methods <- methods[!is.na(methods)]
      }

      if (length(methods) == 0) {
        return(p(style = "color: #999;", "No methods available"))
      }

      checkboxGroupInput(
        ns("methods_selected"),
        NULL,
        choices = methods,
        selected = methods
      )
    })

    # Reactive: Filtered plot data (flux density)
    plot_data_flux <- reactive({
      req(flux_data())
      req(input$sensor_position)

      data <- flux_data()

      # Filter by sensor position
      if ("sensor_position" %in% names(data)) {
        data <- data %>%
          dplyr::filter(sensor_position %in% input$sensor_position)
      }

      # Filter by selected methods if specified
      if (!is.null(input$methods_selected) && length(input$methods_selected) > 0) {
        if ("method_label" %in% names(data)) {
          data <- data %>%
            dplyr::filter(method_label %in% input$methods_selected)
        } else if ("method" %in% names(data)) {
          data <- data %>%
            dplyr::filter(method %in% input$methods_selected)
        } else if ("combination" %in% names(data)) {
          data <- data %>%
            dplyr::filter(combination %in% input$methods_selected)
        }
      }

      # Apply time range filter
      if (!is.null(time_range())) {
        data <- data %>%
          dplyr::filter(
            datetime >= time_range()[1],
            datetime <= time_range()[2]
          )
      }

      # Sort by method and datetime to prevent diagonal connection lines
      if ("method_label" %in% names(data) && "sensor_position" %in% names(data)) {
        data <- data %>%
          dplyr::arrange(method_label, sensor_position, datetime)
      } else if ("method" %in% names(data)) {
        data <- data %>%
          dplyr::arrange(method, datetime)
      } else {
        data <- data %>%
          dplyr::arrange(datetime)
      }

      return(data)
    })

    # Reactive: Filtered plot data (tree water use)
    plot_data_water_use <- reactive({
      req(tree_water_use_data())

      data <- tree_water_use_data()

      # Filter by selected methods if specified and method_label exists
      if (!is.null(input$methods_selected) && length(input$methods_selected) > 0) {
        if ("method_label" %in% names(data)) {
          data <- data %>%
            dplyr::filter(method_label %in% input$methods_selected)
        }
      }

      # Apply time range filter
      if (!is.null(time_range())) {
        data <- data %>%
          dplyr::filter(
            datetime >= time_range()[1],
            datetime <= time_range()[2]
          )
      }

      # Sort by method and datetime
      if ("method_label" %in% names(data)) {
        data <- data %>%
          dplyr::arrange(method_label, datetime)
      } else {
        data <- data %>%
          dplyr::arrange(datetime)
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
            # Filter data for this trace
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
            
            # Determine trace name
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

    # Tree water use - Hourly plot
    output$tree_water_use_plot_hourly <- plotly::renderPlotly({
      tryCatch({
        q_data <- plot_data_water_use()
        style_config <- plot_settings()

        if (is.null(q_data) || nrow(q_data) == 0) {
          return(
            plotly::plot_ly() %>%
              plotly::layout(
                title = "No tree water use data available",
                xaxis = list(title = "Datetime"),
                yaxis = list(title = "Water Use (L/hr)"),
                uirevision = "water_use_hourly_zoom"
              )
          )
        }

        # Sample if too many points
        if (nrow(q_data) > 30000) { # Increased sample limit
          sample_idx <- seq(1, nrow(q_data), length.out = 30000)
          q_data <- q_data[sample_idx, ]
        }

        # Determine mode
        mode <- if (input$show_points) "lines+markers" else "lines"

        # Create Plotly object
        fig <- plotly::plot_ly(source = "water_use_hourly_plot")

        # Add traces manually for consistent styling
        methods <- unique(q_data$method_label %||% "Tree Water Use")
        
        for (m in methods) {
          trace_data <- q_data %>% dplyr::filter(method_label == m)
          if (nrow(trace_data) == 0) next
          
          # Get style
          style_m <- m
          if (grepl("HRM", style_m)) style_m <- "HRM"
          else if (grepl("MHR", style_m)) style_m <- "MHR"
          else if (grepl("Tmax_Coh", style_m)) style_m <- "Tmax_Coh"
          else if (grepl("Tmax_Klu", style_m)) style_m <- "Tmax_Klu"
          else if (grepl("sDMA", style_m)) style_m <- "sDMA"
          
          style <- get_plot_style(method = style_m, sensor = "outer", is_corrected = TRUE, config = style_config)

          fig <- fig %>%
            plotly::add_trace(
              data = trace_data,
              x = ~datetime,
              y = ~Q_total_L_hr,
              type = "scatter",
              mode = mode,
              name = m,
              line = style,
              marker = if (input$show_points) list(size = 4, color = style$color) else NULL,
              legendgroup = m,
              showlegend = TRUE,
              hovertemplate = paste(
                "<b>", m, "</b><br>",
                "Date: %{x|%Y-%m-%d %H:%M}<br>",
                "Q: %{y:.3f} L/hr<br>",
                "<extra></extra>"
              )
            )
        }

        # Get tree dimensions for title
        dims <- tree_dimensions()
        title_text <- if (!is.null(dims) && !is.null(dims$dbh)) {
          sprintf("Tree Water Use (DBH: %.1f cm, Sapwood: %.1f cm)",
                  dims$dbh, dims$sapwood_thickness)
        } else {
          "Tree Water Use"
        }

        # Apply standard layout
        base_layout <- get_standard_layout(
          title = title_text,
          xtitle = "Date",
          ytitle = "Water Use (L/hr)",
          uirevision = "water_use_hourly_zoom"
        )
        
        # Force zoom range persistence
        if (!is.null(time_range())) {
          base_layout$xaxis$range <- time_range()
          base_layout$xaxis$autorange <- FALSE
        }
        
        fig <- fig %>%
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
          apply_standard_plotly_config(filename = "water_use_hourly_plot", add_csv_download = TRUE) %>%
          plotly::event_register("plotly_relayout")

        return(fig)

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(
            title = list(text = paste("Error:", e$message), x = 0.5),
            xaxis = list(title = "Datetime"),
            yaxis = list(title = "Water Use (L/hr)"),
            uirevision = "water_use_hourly_zoom"
          )
      })
    })

    # Tree water use - Daily plot
    output$tree_water_use_plot_daily <- plotly::renderPlotly({
      tryCatch({
        q_data <- plot_data_water_use()
        style_config <- plot_settings()

        if (is.null(q_data) || nrow(q_data) == 0) {
          return(
            plotly::plot_ly() %>%
              plotly::layout(
                title = "No tree water use data available",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Daily Water Use (L/day)"),
                uirevision = "water_use_daily_zoom"
              )
          )
        }

        # Calculate true daily totals (rate × interval, not mean of rate×24)
        group_cols_present <- intersect(c("method_label", "method", "pulse_id"), names(q_data))
        daily_totals <- sapfluxr::aggregate_daily_flux(
          q_data,
          group_cols = if (length(group_cols_present) > 0) group_cols_present else NULL
        )
        # Ensure method_label is present for trace loop
        if (!"method_label" %in% names(daily_totals)) {
          daily_totals$method_label <- "Tree Water Use"
        }

        # Create Plotly object
        fig <- plotly::plot_ly(source = "water_use_daily_plot")

        for (m in unique(daily_totals$method_label)) {
          trace_data <- daily_totals %>% dplyr::filter(method_label == m)
          
          # Get style
          style_m <- m
          if (grepl("HRM", style_m)) style_m <- "HRM"
          else if (grepl("MHR", style_m)) style_m <- "MHR"
          else if (grepl("Tmax_Coh", style_m)) style_m <- "Tmax_Coh"
          else if (grepl("Tmax_Klu", style_m)) style_m <- "Tmax_Klu"
          else if (grepl("sDMA", style_m)) style_m <- "sDMA"
          
          style <- get_plot_style(method = style_m, sensor = "outer", is_corrected = TRUE, config = style_config)

          fig <- fig %>%
            plotly::add_trace(
              data = trace_data,
              x = ~date,
              y = ~Q_total_L_day,
              name = m,
              type = "bar",
              marker = list(color = style$color),
              legendgroup = m,
              showlegend = TRUE,
              hovertemplate = paste(
                "<b>", m, "</b><br>",
                "Date: %{x|%Y-%m-%d}<br>",
                "Daily Total: %{y:.2f} L/day<br>",
                "<extra></extra>"
              )
            )
        }

        # Get tree dimensions for title
        dims <- tree_dimensions()
        title_text <- if (!is.null(dims) && !is.null(dims$dbh)) {
          sprintf("Daily Tree Water Use (DBH: %.1f cm)", dims$dbh)
        } else {
          "Daily Tree Water Use"
        }

        # Apply standard layout
        base_layout <- get_standard_layout(
          title = title_text,
          xtitle = "Date",
          ytitle = "Daily Water Use (L/day)",
          uirevision = "water_use_daily_zoom"
        )
        
        fig <- fig %>%
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
            uirevision = base_layout$uirevision,
            barmode = "group"
          ) %>%
          apply_standard_plotly_config(filename = "water_use_daily_plot", add_csv_download = TRUE)

        return(fig)

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(
            title = list(text = paste("Error:", e$message), x = 0.5),
            xaxis = list(title = "Date"),
            yaxis = list(title = "Daily Water Use (L/day)"),
            uirevision = "water_use_daily_zoom"
          )
      })
    })

    # Update datetime inputs when user zooms any of the validation plots
    relayout_debounced <- debounce(reactive({
      # Listen to both timeseries plots
      r1 <- event_data("plotly_relayout", source = "flux_timeseries_plot")
      r2 <- event_data("plotly_relayout", source = "water_use_hourly_plot")
      if (!is.null(r1)) return(r1)
      return(r2)
    }), 500)

    observeEvent(relayout_debounced(), {
      rd <- relayout_debounced()
      if (is.null(rd)) return()

      if (!is.null(rd$xaxis.range) && length(rd$xaxis.range) == 2) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$xaxis.range[1], tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$xaxis.range[2], tz = "UTC"))
        
        # Save range for persistence
        time_range(c(rd$xaxis.range[1], rd$xaxis.range[2]))
        
      } else if (!is.null(rd$`xaxis.range[0]`)) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$`xaxis.range[0]`, tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$`xaxis.range[1]`, tz = "UTC"))
          
        # Save range
        time_range(c(rd$`xaxis.range[0]`, rd$`xaxis.range[1]`))
        
      } else if (isTRUE(rd$`xaxis.autorange`)) {
        req(flux_data())
        date_range <- range(flux_data()$datetime, na.rm = TRUE)
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
      
      plotly::plotlyProxy("water_use_hourly_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list("xaxis.range" = t_range))
    })

    # Reset zoom
    observeEvent(input$reset_zoom, {
      time_range(NULL)

      if (!is.null(flux_data())) {
        data <- flux_data()
        date_range <- range(data$datetime, na.rm = TRUE)

        shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
        shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
        
        plotly::plotlyProxy("flux_timeseries_plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE))
        
        plotly::plotlyProxy("water_use_hourly_plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE))
      }
    })

    # Code generation
    observe({
      if (!is.null(code_tracker) && !is.logical(code_tracker)) {
        if (!is.null(input$sensor_position)) {
          code_tracker$add_step(
            step_name = "Flux Density Validation Visualisation",
            code = sprintf(
              "# Flux density validation plot for %s sensor(s)",
              paste(input$sensor_position, collapse = ", ")
            )
          )
        }
      }
    })

  })
}
