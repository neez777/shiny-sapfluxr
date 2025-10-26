#' Time Series Visualization Module
#'
#' Shiny module for interactive plotly time series of HPV results
#'
#' @param id Module ID
#' @param vh_results Reactive containing vh_results tibble
#' @return None (displays plot)
#'

# UI ----
plotTimeseriesUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Plot Controls
      column(
        width = 3,
        box(
          width = NULL,
          title = "Plot Controls",
          status = "primary",
          solidHeader = TRUE,

          h5("Methods to Display"),
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

          h5("Quality Flags to Display"),
          uiOutput(ns("flag_checkboxes")),

          hr(),

          h5("Time Range"),
          p(class = "help-text", style = "font-size: 0.9em; color: #666;",
            "Set the date/time range to display. Updates automatically when you use the range slider."),

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
            ns("show_quality_flags"),
            "Show quality flag markers",
            value = TRUE
          ),

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
        ),

        box(
          width = NULL,
          title = "Plot Information",
          status = "info",
          collapsible = TRUE,
          collapsed = TRUE,

          uiOutput(ns("plot_info"))
        ),

        box(
          width = NULL,
          title = "Quality Control Summary",
          status = "warning",
          solidHeader = TRUE,

          uiOutput(ns("qc_summary"))
        )
      ),

      # Time Series Plot
      column(
        width = 9,
        box(
          width = NULL,
          title = "Heat Pulse Velocity Time Series",
          status = "success",
          solidHeader = TRUE,

          plotly::plotlyOutput(ns("timeseries_plot"), height = "600px")
        )
      )
    )
  )
}

# Server ----
plotTimeseriesServer <- function(id, vh_results) {
  moduleServer(id, function(input, output, session) {

    # Store current axis ranges
    current_xrange <- reactiveVal(NULL)
    current_yrange <- reactiveVal(NULL)

    # Store selected pulse ID from click
    selected_pulse_id <- reactiveVal(NULL)

    # Initialize date/time range inputs with full data range
    observe({
      req(vh_results())
      data <- vh_results()

      # Get full date range from data
      date_range <- range(data$datetime, na.rm = TRUE)

      # Update date/time inputs (only if not already set)
      if (is.null(input$start_datetime)) {
        shinyWidgets::updateAirDateInput(
          session = session,
          inputId = "start_datetime",
          value = date_range[1]
        )
      }

      if (is.null(input$end_datetime)) {
        shinyWidgets::updateAirDateInput(
          session = session,
          inputId = "end_datetime",
          value = date_range[2]
        )
      }
    })

    # Available methods from data
    available_methods <- reactive({
      req(vh_results())
      unique(vh_results()$method)
    })

    # Dynamic method checkboxes
    output$method_checkboxes <- renderUI({
      methods <- available_methods()
      req(length(methods) > 0)

      # Default all checked
      checkboxGroupInput(
        session$ns("methods"),
        NULL,
        choices = setNames(methods, methods),
        selected = methods
      )
    })

    # Available quality flags from data
    available_flags <- reactive({
      req(vh_results())
      if (!"quality_flag" %in% names(vh_results())) {
        return(c("OK"))
      }
      unique(vh_results()$quality_flag)
    })

    # Dynamic quality flag checkboxes
    output$flag_checkboxes <- renderUI({
      flags <- available_flags()
      req(length(flags) > 0)

      # Create friendly labels for flags
      flag_labels <- c(
        "OK" = "OK (No issues)",
        "DATA_OUTLIER" = "Outliers",
        "DATA_SUSPECT" = "Suspect",
        "DATA_MISSING" = "Missing",
        "DATA_ILLOGICAL" = "Illogical",
        "CALC_FAILED" = "Calc Failed",
        "CALC_INFINITE" = "Calc Infinite",
        "CALC_EXTREME" = "Calc Extreme"
      )

      # Only show flags that exist in data
      choices <- flags
      names(choices) <- sapply(flags, function(f) {
        if (f %in% names(flag_labels)) flag_labels[[f]] else f
      })

      # Default: show all flags
      checkboxGroupInput(
        session$ns("quality_flags"),
        NULL,
        choices = choices,
        selected = flags
      )
    })

    # Flag colors (consistent with scientific QC standards)
    flag_colors <- c(
      "OK" = "#2ca02c",              # Green
      "DATA_SUSPECT" = "#ff7f0e",    # Orange
      "DATA_OUTLIER" = "#d62728",    # Red
      "DATA_MISSING" = "#7f7f7f",    # Gray
      "DATA_ILLOGICAL" = "#8b0000",  # Dark red
      "CALC_FAILED" = "#9467bd",     # Purple
      "CALC_INFINITE" = "#e377c2",   # Pink
      "CALC_EXTREME" = "#bcbd22"     # Yellow-green
    )

    # Filtered data based on selections
    filtered_data <- reactive({
      req(vh_results())
      req(input$methods)
      req(input$sensor_position)

      data <- vh_results()

      # Filter by method
      data <- data %>%
        filter(method %in% input$methods)

      # Filter by sensor position
      data <- data %>%
        filter(sensor_position %in% input$sensor_position)

      # Filter by quality flags (if quality_flags input exists and has selections)
      if (!is.null(input$quality_flags) && length(input$quality_flags) > 0) {
        if ("quality_flag" %in% names(data)) {
          data <- data %>%
            filter(quality_flag %in% input$quality_flags)
        }
      }

      data
    })

    # Plot information summary
    output$plot_info <- renderUI({
      data <- filtered_data()
      req(nrow(data) > 0)

      n_points <- nrow(data)
      date_range <- range(data$datetime, na.rm = TRUE)
      methods <- unique(data$method)
      positions <- unique(data$sensor_position)

      # Quality flag summary
      flag_summary <- data %>%
        count(quality_flag) %>%
        arrange(desc(n))

      tagList(
        p(strong("Displayed Data:")),
        tags$ul(
          tags$li(paste("Points:", format(n_points, big.mark = ","))),
          tags$li(paste("Methods:", paste(methods, collapse = ", "))),
          tags$li(paste("Positions:", paste(positions, collapse = ", "))),
          tags$li(paste("Date range:",
                       format(date_range[1], "%Y-%m-%d %H:%M"),
                       "to",
                       format(date_range[2], "%Y-%m-%d %H:%M")))
        ),

        if (nrow(flag_summary) > 0) {
          tagList(
            p(strong("Quality Flags:")),
            tags$ul(
              lapply(seq_len(nrow(flag_summary)), function(i) {
                tags$li(paste(flag_summary$quality_flag[i], ":",
                             format(flag_summary$n[i], big.mark = ",")))
              })
            )
          )
        }
      )
    })

    # Quality Control Summary
    output$qc_summary <- renderUI({
      req(vh_results())
      data <- vh_results()

      # Check if quality_flag column exists
      if (!"quality_flag" %in% names(data)) {
        return(p("No quality control data available."))
      }

      # Calculate flag counts across ALL data (not just filtered)
      flag_counts <- data %>%
        count(quality_flag) %>%
        arrange(quality_flag)

      total_points <- sum(flag_counts$n)

      # Create colored list items
      flag_items <- lapply(seq_len(nrow(flag_counts)), function(i) {
        flag <- flag_counts$quality_flag[i]
        count <- flag_counts$n[i]
        pct <- round(count / total_points * 100, 1)

        # Get color for this flag
        color <- if (flag %in% names(flag_colors)) {
          flag_colors[[flag]]
        } else {
          "#333333"  # Default gray
        }

        # Create friendly label
        flag_label <- switch(flag,
          "OK" = "OK (No issues)",
          "DATA_OUTLIER" = "Outliers",
          "DATA_SUSPECT" = "Suspect",
          "DATA_MISSING" = "Missing",
          "DATA_ILLOGICAL" = "Illogical",
          "CALC_FAILED" = "Calc Failed",
          "CALC_INFINITE" = "Calc Infinite",
          "CALC_EXTREME" = "Calc Extreme",
          flag  # Default to flag name
        )

        tags$li(
          style = paste0("color: ", color, "; font-weight: bold;"),
          paste0(flag_label, ": ", format(count, big.mark = ","),
                 " (", pct, "%)")
        )
      })

      tagList(
        p(strong(paste("Total Measurements:", format(total_points, big.mark = ",")))),
        tags$ul(style = "list-style-type: none; padding-left: 10px;",
          flag_items
        ),
        p(style = "font-size: 0.85em; color: #666; margin-top: 10px;",
          "Use the 'Quality Flags to Display' filter above to show/hide specific flags on the plot.")
      )
    })

    # Method colours
    method_colours <- reactive({
      tryCatch({
        methods <- available_methods()
        req(length(methods) > 0)

        cat("\n=== METHOD COLOURS DEBUG ===\n")
        cat("Methods to color:", paste(methods, collapse = ", "), "\n")

        # Define base colour palette
        base_colours <- c(
          "HRM" = "#1f77b4",      # Blue
          "MHR" = "#ff7f0e",      # Orange
          "HRMXa" = "#2ca02c",    # Green
          "HRMXb" = "#d62728",    # Red
          "Tmax_Coh" = "#9467bd", # Purple
          "Tmax_Klu" = "#8c564b", # Brown
          "sDMA:HRM" = "#e377c2", # Pink
          "sDMA:MHR" = "#7f7f7f", # Gray
          "sDMA:Tmax_Coh" = "#bcbd22", # Yellow-green
          "sDMA:Tmax_Klu" = "#17becf",  # Cyan
          "sDMA:HRMXa" = "#17a589"  # Teal
        )

        # Generate colors for all methods
        colours <- character(length(methods))
        names(colours) <- methods

        for (i in seq_along(methods)) {
          m <- methods[i]
          if (m %in% names(base_colours)) {
            colours[i] <- base_colours[[m]]
            cat("  ", m, "-> predefined:", colours[i], "\n")
          } else {
            # For unknown methods, generate a color
            set.seed(sum(as.integer(charToRaw(m))))
            colours[i] <- sprintf("#%06X", sample(0:16777215, 1))
            cat("  ", m, "-> generated:", colours[i], "\n")
          }
        }

        cat("Total colors:", length(colours), "\n")
        colours

      }, error = function(e) {
        cat("ERROR in method_colours:", e$message, "\n")
        print(e)
        return(c("HRM" = "#1f77b4"))  # Return at least one color
      })
    })

    # Quality flag markers data
    quality_markers <- reactive({
      req(vh_results())

      data <- vh_results()

      # Get non-OK quality flags
      markers <- data %>%
        filter(quality_flag != "OK") %>%
        select(datetime, quality_flag, method, sensor_position, Vh_cm_hr) %>%
        distinct()

      markers
    })

    # Main time series plot
    output$timeseries_plot <- plotly::renderPlotly({
      tryCatch({
        data <- filtered_data()
        req(nrow(data) > 0)

        cat("\n=== TIME SERIES PLOT DEBUG ===\n")
        cat("Data rows:", nrow(data), "\n")
        cat("Data columns:", paste(names(data), collapse = ", "), "\n")
        cat("Unique methods in data:", paste(unique(data$method), collapse = ", "), "\n")

        # Get method colours
        colours <- method_colours()
        cat("Colors generated:", length(colours), "\n")
        cat("Color names:", paste(names(colours), collapse = ", "), "\n")
        req(!is.null(colours), length(colours) > 0)

        # Create base plot with source for event tracking
        p <- plot_ly(source = "timeseries")

        # Add trace for each method
        for (method in unique(data$method)) {
          cat("Processing method:", method, "\n")
          method_data <- data %>% filter(method == !!method)
          cat("  Rows for this method:", nrow(method_data), "\n")

          # Make sure color exists for this method
          if (!method %in% names(colours)) {
            cat("  ERROR: No color for method:", method, "\n")
            next
          }

          # Get color safely
          method_color <- tryCatch(
            colours[[method]],
            error = function(e) {
              cat("  ERROR getting color for", method, ":", e$message, "\n")
              "#999999"  # Default gray
            }
          )
          cat("  Using color:", method_color, "\n")

        # Separate by sensor position if both selected
        if (length(input$sensor_position) > 1) {
          for (pos in unique(method_data$sensor_position)) {
            pos_data <- method_data %>% filter(sensor_position == !!pos)

            trace_name <- paste0(method, " (", pos, ")")
            line_dash <- if (pos == "inner") "solid" else "dash"

            p <- p %>%
              add_trace(
                data = pos_data,
                x = ~datetime,
                y = ~Vh_cm_hr,
                type = "scatter",
                mode = if (input$show_points) "lines+markers" else "lines",
                name = trace_name,
                line = list(
                  color = method_color,
                  dash = line_dash,
                  width = 2
                ),
                marker = list(size = 4),
                hovertemplate = paste0(
                  "<b>", trace_name, "</b><br>",
                  "Time: %{x}<br>",
                  "Vh: %{y:.2f} cm/hr<br>",
                  "<extra></extra>"
                )
              )
          }
        } else {
          # Single sensor position
          p <- p %>%
            add_trace(
              data = method_data,
              x = ~datetime,
              y = ~Vh_cm_hr,
              type = "scatter",
              mode = if (input$show_points) "lines+markers" else "lines",
              name = method,
              line = list(
                color = method_color,
                width = 2
              ),
              marker = list(size = 4),
              hovertemplate = paste0(
                "<b>", method, "</b><br>",
                "Time: %{x}<br>",
                "Vh: %{y:.2f} cm/hr<br>",
                "<extra></extra>"
              )
            )
        }
      }

      # Add quality flag markers if enabled
      if (input$show_quality_flags) {
        markers <- quality_markers()

        if (nrow(markers) > 0) {
          # Filter markers to displayed methods/positions and selected flags
          markers <- markers %>%
            filter(method %in% input$methods,
                   sensor_position %in% input$sensor_position)

          # Also filter by selected quality flags if input exists
          if (!is.null(input$quality_flags) && length(input$quality_flags) > 0) {
            markers <- markers %>%
              filter(quality_flag %in% input$quality_flags)
          }

          if (nrow(markers) > 0) {
            # Define marker shapes by quality flag
            flag_shapes <- c(
              "DATA_OUTLIER" = "x",
              "DATA_SUSPECT" = "diamond",
              "DATA_MISSING" = "square",
              "DATA_ILLOGICAL" = "triangle-up",
              "CALC_FAILED" = "circle-open",
              "CALC_INFINITE" = "star",
              "CALC_EXTREME" = "triangle-down"
            )

            for (flag in unique(markers$quality_flag)) {
              flag_data <- markers %>% filter(quality_flag == !!flag)

              # Get shape and color using our flag_colors definition
              flag_shape <- if (flag %in% names(flag_shapes)) {
                flag_shapes[[flag]]
              } else {
                "circle"  # Default shape
              }

              flag_color <- if (flag %in% names(flag_colors)) {
                flag_colors[[flag]]
              } else {
                "#999999"  # Default gray
              }

              # Friendly label
              flag_label <- switch(flag,
                "DATA_OUTLIER" = "Outlier",
                "DATA_SUSPECT" = "Suspect",
                "DATA_MISSING" = "Missing",
                "DATA_ILLOGICAL" = "Illogical",
                "CALC_FAILED" = "Calc Failed",
                "CALC_INFINITE" = "Calc Infinite",
                "CALC_EXTREME" = "Calc Extreme",
                flag
              )

              p <- p %>%
                add_trace(
                  data = flag_data,
                  x = ~datetime,
                  y = ~Vh_cm_hr,
                  type = "scatter",
                  mode = "markers",
                  name = flag_label,
                  marker = list(
                    symbol = flag_shape,
                    size = 8,
                    color = flag_color,
                    line = list(width = 1, color = "white")
                  ),
                  hovertemplate = paste0(
                    "<b>", flag_label, "</b><br>",
                    "Time: %{x}<br>",
                    "Vh: %{y:.2f} cm/hr<br>",
                    "<extra></extra>"
                  )
                )
            }
          }
        }
      }

      # Layout with range slider
      p <- p %>%
        layout(
          xaxis = list(
            title = "Date/Time",
            rangeslider = list(
              visible = TRUE,
              thickness = 0.1  # Height of range slider as fraction of plot height
            ),
            rangeselector = list(
              buttons = list(
                list(count = 1, label = "1d", step = "day", stepmode = "backward"),
                list(count = 7, label = "1w", step = "day", stepmode = "backward"),
                list(count = 1, label = "1m", step = "month", stepmode = "backward"),
                list(count = 3, label = "3m", step = "month", stepmode = "backward"),
                list(step = "all", label = "All")
              )
            )
          ),
          yaxis = list(
            title = "Heat Pulse Velocity (cm/hr)",
            rangemode = "tozero"
          ),
          hovermode = "closest",
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.25,
            xanchor = "left",
            yanchor = "top"
          ),
          margin = list(b = 150)  # Increased for range slider
        ) %>%
        config(
          displayModeBar = TRUE,
          displaylogo = FALSE,
          modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
          toImageButtonOptions = list(
            format = "png",
            filename = "heat_pulse_velocity_timeseries",
            height = 600,
            width = 1200,
            scale = 2
          )
        ) %>%
        event_register("plotly_relayout") %>%
        event_register("plotly_click")

        p

      }, error = function(e) {
        # Show error in plot area
        cat("Error in time series plot:", e$message, "\n")
        cat("Available methods:", paste(available_methods(), collapse = ", "), "\n")

        # Return empty plot with error message
        plot_ly() %>%
          layout(
            title = list(
              text = paste("Plot Error:", e$message),
              font = list(color = "red")
            ),
            xaxis = list(title = "Time"),
            yaxis = list(title = "Vh (cm/hr)")
          )
      })
    })

    # Capture current plot zoom/ranges when user interacts with range slider
    observeEvent(event_data("plotly_relayout", source = "timeseries"), {
      relayout_data <- event_data("plotly_relayout", source = "timeseries")

      if (!is.null(relayout_data)) {
        cat("\n=== PLOTLY RELAYOUT EVENT ===\n")
        cat("Event data fields:", paste(names(relayout_data), collapse = ", "), "\n")

        # Print all values for debugging
        for (name in names(relayout_data)) {
          cat("  ", name, "=", relayout_data[[name]], "\n")
        }

        # Check if this is a range update
        # Plotly can send either "xaxis.range" (a vector) or "xaxis.range[0]" and "xaxis.range[1]"
        if (!is.null(relayout_data$xaxis.range) && length(relayout_data$xaxis.range) == 2) {
          # Single field with vector [start, end]
          xrange <- relayout_data$xaxis.range

          cat("xaxis.range vector:", xrange[1], "to", xrange[2], "\n")

          # Parse dates - plotly sends ISO strings
          start_date <- as.POSIXct(xrange[1], tz = "UTC")
          end_date <- as.POSIXct(xrange[2], tz = "UTC")

          cat("Converted start_date:", as.character(start_date), "\n")
          cat("Converted end_date:", as.character(end_date), "\n")

          # Update date inputs
          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "start_datetime",
            value = start_date
          )

          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "end_datetime",
            value = end_date
          )

          # Store current range
          current_xrange(xrange)

          cat("Date inputs updated\n")

        } else if (!is.null(relayout_data$`xaxis.range[0]`)) {
          # Separate fields for start and end
          xrange <- c(relayout_data$`xaxis.range[0]`, relayout_data$`xaxis.range[1]`)

          cat("xaxis.range[0]:", relayout_data$`xaxis.range[0]`, "\n")
          cat("xaxis.range[1]:", relayout_data$`xaxis.range[1]`, "\n")

          # Parse dates
          start_date <- as.POSIXct(xrange[1], tz = "UTC")
          end_date <- as.POSIXct(xrange[2], tz = "UTC")

          cat("Converted start_date:", as.character(start_date), "\n")
          cat("Converted end_date:", as.character(end_date), "\n")

          # Update date inputs
          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "start_datetime",
            value = start_date
          )

          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "end_datetime",
            value = end_date
          )

          # Store current range
          current_xrange(xrange)

          cat("Date inputs updated\n")
        }

        if (!is.null(relayout_data$`yaxis.range[0]`)) {
          current_yrange(c(relayout_data$`yaxis.range[0]`, relayout_data$`yaxis.range[1]`))
        }

        # Handle autorange reset
        if (!is.null(relayout_data$`xaxis.autorange`) && relayout_data$`xaxis.autorange`) {
          current_xrange(NULL)

          # Reset date/time inputs to full range
          req(vh_results())
          date_range <- range(vh_results()$datetime, na.rm = TRUE)

          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "start_datetime",
            value = date_range[1]
          )

          shinyWidgets::updateAirDateInput(
            session = session,
            inputId = "end_datetime",
            value = date_range[2]
          )
        }

        if (!is.null(relayout_data$`yaxis.autorange`) && relayout_data$`yaxis.autorange`) {
          current_yrange(NULL)
        }
      }
    })

    # Apply time range when button clicked or when filters change
    apply_time_range <- function() {
      req(input$start_datetime, input$end_datetime)

      cat("\n=== APPLYING TIME RANGE ===\n")
      cat("Start datetime:", as.character(input$start_datetime), "\n")
      cat("End datetime:", as.character(input$end_datetime), "\n")

      # Convert POSIXct to ISO date strings for plotly
      start_str <- format(input$start_datetime, "%Y-%m-%d %H:%M:%S")
      end_str <- format(input$end_datetime, "%Y-%m-%d %H:%M:%S")

      cat("Formatted start:", start_str, "\n")
      cat("Formatted end:", end_str, "\n")

      # Update plot range using plotlyProxy
      plotly::plotlyProxy("timeseries_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list(
          "xaxis.range" = list(start_str, end_str)
        ))

      # Store current range (as ISO strings)
      current_xrange(c(start_str, end_str))

      cat("Plot range applied\n")
    }

    # Apply range when button clicked
    observeEvent(input$apply_range, {
      cat("Apply range button clicked\n")
      apply_time_range()
    })

    # Preserve time range when filters change (methods, sensor position, etc.)
    # Use priority = -1 to run AFTER the plot has rendered
    observeEvent(list(input$methods, input$sensor_position, input$show_points, input$show_quality_flags), {
      # Only apply if user has set a custom range (date/time inputs are set)
      if (!is.null(input$start_datetime) && !is.null(input$end_datetime)) {
        cat("\n=== FILTER CHANGED - PRESERVING ZOOM ===\n")
        apply_time_range()
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE, priority = -1)

    # Reset zoom button - reset to full data range
    observeEvent(input$reset_zoom, {
      req(vh_results())
      date_range <- range(vh_results()$datetime, na.rm = TRUE)

      # Update date/time inputs to full range
      shinyWidgets::updateAirDateInput(
        session = session,
        inputId = "start_datetime",
        value = date_range[1]
      )

      shinyWidgets::updateAirDateInput(
        session = session,
        inputId = "end_datetime",
        value = date_range[2]
      )

      # Reset plot to autorange
      current_xrange(NULL)
      current_yrange(NULL)

      plotly::plotlyProxy("timeseries_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list(
          "xaxis.autorange" = TRUE,
          "yaxis.autorange" = TRUE
        ))
    })

    # Capture click events on plot points
    observeEvent(event_data("plotly_click", source = "timeseries"), {
      click_data <- event_data("plotly_click", source = "timeseries")

      if (!is.null(click_data)) {
        cat("\n=== PLOT CLICK EVENT ===\n")
        cat("Click data:", names(click_data), "\n")
        cat("Point number:", click_data$pointNumber, "\n")
        cat("Curve number:", click_data$curveNumber, "\n")

        # Get the clicked point's data
        # The customdata field should contain pulse_id if we set it
        req(vh_results())
        data <- filtered_data()

        # Get curve number and point number
        curve_num <- click_data$curveNumber + 1  # plotly is 0-indexed
        point_num <- click_data$pointNumber + 1  # plotly is 0-indexed

        cat("Adjusted curve:", curve_num, "point:", point_num, "\n")

        # Try to extract pulse_id from the clicked point
        # We need to figure out which row in filtered_data this corresponds to
        # For now, use the x value (datetime) to find the pulse
        clicked_datetime <- click_data$x

        cat("Clicked datetime:", clicked_datetime, "\n")

        # Find pulse_id for this datetime
        matching_row <- data %>%
          filter(abs(as.numeric(difftime(datetime, clicked_datetime, units = "secs"))) < 1) %>%
          slice(1)

        if (nrow(matching_row) > 0) {
          pulse_id <- matching_row$pulse_id[1]
          cat("Found pulse_id:", pulse_id, "\n")
          selected_pulse_id(pulse_id)

          # Show notification
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Pulse Selected",
            text = paste("Viewing Pulse ID:", pulse_id),
            type = "info",
            timer = 2000
          )
        } else {
          cat("No matching pulse found\n")
        }
      }
    })

    # Return the selected pulse ID reactive
    return(selected_pulse_id)

  })
}
