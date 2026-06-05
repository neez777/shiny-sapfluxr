# mod_7b_sdma_validation.R
# Module for sDMA Validation Visualisation
#
# Tab 7b: sDMA Validation
# Interactive time series to compare HRM baseline, calibrated methods, and sDMA results

# UI ----
sdmaValidationUI <- function(id) {
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
            selected = "outer"
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
          checkboxInput(
            ns("show_peclet"),
            "Show Peclet Trace & Threshold",
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

      # Right column: Plot
      column(
        width = 9,
        box(
          width = NULL,
          title = "sDMA Validation",
          status = "success",
          solidHeader = TRUE,

          helpText(
            icon("info-circle"),
            "Compare HRM baseline, calibrated secondary methods, and sDMA results.",
            "Combined option shows color-coded switching segments."
          ),

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("validation_plot"), height = "700px"),
            type = 6,
            color = "#3c8dbc"
          )
        )
      )
    )
  )
}

# Server ----
sdmaValidationServer <- function(id,
                                   vh_hrm_peclet,
                                   vh_calibrated,
                                   vh_sdma,
                                   sdma_threshold = reactive(1.0),
                                   code_tracker = NULL,
                                   plot_settings = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Time range
    time_range <- reactiveVal(NULL)

    # Initialize datetime range from data
    observe({
      req(vh_hrm_peclet())

      data <- vh_hrm_peclet()
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

    # Dynamic method checkboxes based on available data
    output$method_checkboxes <- renderUI({
      req(vh_calibrated())

      methods <- c()

      # HRM (from corrected data)
      if (!is.null(vh_hrm_peclet())) {
        methods <- c(methods, "HRM (Corrected)")
      }

      # Calibrated secondary methods
      cal_methods <- unique(vh_calibrated()$method)
      cal_methods <- cal_methods[!is.na(cal_methods) & cal_methods != "HRM"]
      if (length(cal_methods) > 0) {
        methods <- c(methods, paste0(cal_methods, " (Calibrated)"))
      }

      # sDMA combinations
      sdma_methods <- character(0)
      if (!is.null(vh_sdma()) && nrow(vh_sdma()) > 0) {
        sdma_methods <- unique(vh_sdma()$method)
        sdma_methods <- sdma_methods[!is.na(sdma_methods)]
        if (length(sdma_methods) > 0) {
          methods <- c(methods, sdma_methods)
        }
        # Add the special Combined option
        methods <- c(methods, "Method Combined sDMA")
      }

      if (length(methods) == 0) {
        return(p(style = "color: #999;", "No methods available"))
      }
      
      # Default selection: Only sDMA methods if they exist, else HRM
      selected_initial <- if (length(sdma_methods) > 0) sdma_methods else c("HRM (Corrected)")

      checkboxGroupInput(
        ns("methods_selected"),
        NULL,
        choices = methods,
        selected = selected_initial
      )
    })

    # Validation plot - Integrated render (handles all traces with persistent zoom)
    output$validation_plot <- plotly::renderPlotly({
      req(vh_hrm_peclet())
      req(input$sensor_position)
      
      # Data dependencies
      hrm_full <- vh_hrm_peclet()
      cal_full <- vh_calibrated()
      sdma_full <- vh_sdma()
      
      sensor_pos <- input$sensor_position
      selected_methods <- input$methods_selected
      style_config <- plot_settings()
      show_points <- isTRUE(input$show_points)
      mode <- if (show_points) "lines+markers" else "lines"

      tryCatch({
        # 1. Prepare Data structure
        all_data <- list()

        # HRM Baseline
        # Note: Recalculated Peclet data from Tab 7a uses Pe_corrected
        vh_col_hrm <- if ("Vs_cm_hr" %in% names(hrm_full)) "Vs_cm_hr" else "Vh_cm_hr"
        hrm_data <- hrm_full %>%
          dplyr::filter(method == "HRM", sensor_position %in% sensor_pos) %>%
          dplyr::mutate(
            plot_method = "HRM (Corrected)",
            plot_velocity = !!rlang::sym(vh_col_hrm)
          )
        all_data$hrm <- hrm_data

        # Calibrated Methods
        if (!is.null(cal_full) && nrow(cal_full) > 0) {
          vh_col_cal <- if ("Vs_cm_hr" %in% names(cal_full)) "Vs_cm_hr" else "Vh_cm_hr"
          cal_data <- cal_full %>%
            dplyr::filter(method != "HRM", sensor_position %in% sensor_pos) %>%
            dplyr::mutate(
              plot_method = paste0(method, " (Calibrated)"),
              plot_velocity = !!rlang::sym(vh_col_cal)
            )
          all_data$calibrated <- cal_data
        }

        # sDMA Results (Standard)
        if (!is.null(sdma_full) && nrow(sdma_full) > 0) {
          sdma_data <- sdma_full %>%
            dplyr::filter(sensor_position %in% sensor_pos, !is.na(Vh_sdma)) %>%
            dplyr::mutate(
              plot_method = method,
              plot_velocity = Vh_sdma
            )
          all_data$sdma <- sdma_data
        }

        # Combine base data for standard traces
        combined_base <- dplyr::bind_rows(all_data)
        
        # Filter standard traces (excluding special virtual methods)
        standard_selected <- selected_methods[selected_methods != "Method Combined sDMA"]
        combined <- combined_base %>% dplyr::filter(plot_method %in% standard_selected)

        # 2. Create Plot
        p <- plotly::plot_ly(source = "validation_plot")

        # Stable sorting for standard traces
        available_plot_methods <- sort(unique(combined$plot_method))
        
        for (m in available_plot_methods) {
          for (s in sensor_pos) {
            subset_data <- combined %>% 
              dplyr::filter(plot_method == m, sensor_position == s) %>%
              dplyr::arrange(datetime)
            
            if (nrow(subset_data) == 0) next

            style_m <- m
            if (grepl(" \\(Calibrated\\)", style_m)) {
              style_m <- gsub(" \\(Calibrated\\)", "", style_m)
            } else if (grepl(" \\(Corrected\\)", style_m)) {
              style_m <- gsub(" \\(Corrected\\)", "", style_m)
            }

            trace_name <- if (length(sensor_pos) > 1) paste0(m, " (", toupper(s), ")") else m

            p <- p %>%
              plotly::add_trace(
                data = subset_data,
                x = ~datetime,
                y = ~plot_velocity,
                type = "scatter",
                mode = mode,
                name = trace_name,
                line = get_plot_style(method = style_m, sensor = s, is_corrected = TRUE, config = style_config),
                marker = if (show_points) list(size = 4) else NULL,
                connectgaps = FALSE,
                showlegend = TRUE,
                hovertemplate = paste0("<b>", trace_name, "</b><br>Time: %{x}<br>Vel: %{y:.2f}<extra></extra>")
              )
          }
        }

        # 3. Add "Method Combined sDMA" Trace Logic (Fast NA masking)
        if ("Method Combined sDMA" %in% selected_methods && !is.null(sdma_full) && nrow(sdma_full) > 0) {
          for (s in sensor_pos) {
            # Filter sDMA results for this sensor
            combo_full <- sdma_full %>% 
              dplyr::filter(sensor_position == s, !is.na(Vh_sdma)) %>%
              dplyr::arrange(datetime)
            
            if (nrow(combo_full) == 0) next
            
            sources <- unique(combo_full$sdma_source)
            
            for (src in sources) {
              # Logical mask of where the source is active
              is_src <- (combo_full$sdma_source == src)
              
              # Shift mask to find adjacent points to prevent visual gaps
              next_is_src <- c(is_src[-1], FALSE)
              prev_is_src <- c(FALSE, is_src[-nrow(combo_full)])
              
              # Keep point if it belongs to this source, OR it is the handover point
              keep_mask <- is_src | next_is_src | prev_is_src
              
              segment_data <- combo_full
              # Mask non-relevant points with NA to break the line efficiently
              segment_data$Vh_sdma[!keep_mask] <- NA
              
              # Skip if trace would be entirely empty
              if (sum(!is.na(segment_data$Vh_sdma)) == 0) next
              
              # Style for the source method
              style_m <- src
              src_style <- get_plot_style(method = style_m, sensor = s, is_corrected = TRUE, config = style_config)
              src_style$width <- 3.0 # Thicker for Combined
              
              trace_name <- sprintf("sDMA:%s (via %s)", toupper(s), src)
              
              p <- p %>%
                plotly::add_trace(
                  data = segment_data,
                  x = ~datetime,
                  y = ~Vh_sdma,
                  type = "scatter",
                  mode = mode,
                  name = trace_name,
                  line = src_style,
                  marker = if (show_points) list(size = 4) else NULL,
                  connectgaps = FALSE,
                  showlegend = TRUE,
                  hovertemplate = paste0("<b>sDMA Segment</b><br>Source: ", src, "<br>Time: %{x}<br>Vel: %{y:.2f}<extra></extra>")
                )
            }
          }
        }

        # 4. Handle Peclet Trace & Dual Axis
        pe_trace_active <- isTRUE(input$show_peclet) && !is.null(hrm_full) && "Pe_corrected" %in% names(hrm_full)
        
        if (pe_trace_active) {
          for (s in sensor_pos) {
            # Get Peclet data from HRM Recalc (derived via vh_hrm_peclet)
            pe_data <- hrm_full %>% 
              dplyr::filter(sensor_position == s, method == "HRM", !is.na(Pe_corrected)) %>%
              dplyr::arrange(datetime)
            
            if (nrow(pe_data) == 0) next
            
            p <- p %>%
              plotly::add_trace(
                data = pe_data,
                x = ~datetime,
                y = ~Pe_corrected,
                type = "scatter",
                mode = "lines",
                name = paste0("Peclet (", toupper(s), ")"),
                line = list(color = "#666666", width = 1.0, dash = "dot"),
                yaxis = "y2",
                showlegend = TRUE,
                hovertemplate = "<b>Peclet Number</b><br>Time: %{x}<br>Pe: %{y:.3f}<extra></extra>"
              )
          }
          
          # Add Threshold Line (Horizontal)
          threshold <- sdma_threshold()
          p <- p %>%
            plotly::add_segments(
              x = min(hrm_full$datetime, na.rm = TRUE),
              xend = max(hrm_full$datetime, na.rm = TRUE),
              y = threshold, yend = threshold,
              yaxis = "y2",
              name = paste0("Threshold (Pe=", threshold, ")"),
              line = list(color = "red", width = 1.5, dash = "dash"),
              hovertemplate = paste0("<b>sDMA Threshold</b><br>Value: ", threshold, "<extra></extra>")
            )
        }

        # 5. Standard Layout with Dual Axis Synchronization
        base_layout <- get_standard_layout(
          title = "sDMA Validation",
          xtitle = "Datetime",
          ytitle = "Velocity (cm/hr)",
          uirevision = "sdma_validation_zoom"
        )
        
        # Explicit Zoom Range tracking to enforce persistence across redraws
        if (!is.null(time_range())) {
          base_layout$xaxis$range <- time_range()
          base_layout$xaxis$autorange <- FALSE
        }
        
        # Dual Axis Math (Zero-Alignment & Pe=1 Alignment)
        if (pe_trace_active) {
          # 1. Determine v_pe1 (Velocity where Peclet = 1)
          # Pe_corrected = plot_velocity / v_pe1 -> v_pe1 = plot_velocity / Pe_corrected
          pe_ratios <- hrm_full$plot_velocity / hrm_full$Pe_corrected
          pe_ratios <- pe_ratios[is.finite(pe_ratios) & pe_ratios > 0]
          v_pe1 <- if (length(pe_ratios) > 0) median(pe_ratios, na.rm = TRUE) else 18.0
          
          # 2. Determine Velocity axis range explicitly so we can scale Y2 to match
          vel_max <- max(combined_base$plot_velocity, na.rm = TRUE)
          vel_min <- min(combined_base$plot_velocity, na.rm = TRUE)
          if (is.infinite(vel_max)) vel_max <- 50
          if (is.infinite(vel_min)) vel_min <- -5
          
          vel_pad <- (vel_max - vel_min) * 0.05
          if (vel_pad == 0) vel_pad <- 1
          
          vel_range <- c(vel_min - vel_pad, vel_max + vel_pad)
          
          # Only override Y axis if user hasn't explicitly zoomed the Y axis?
          # For dual axis synchronization, we must lock the primary Y-axis range
          base_layout$yaxis$range <- vel_range
          base_layout$yaxis$autorange <- FALSE
          
          # 3. Set Peclet axis range directly proportional
          pe_range <- vel_range / v_pe1
          
          base_layout$yaxis2 <- list(
            title = "Peclet Number",
            overlaying = "y",
            side = "right",
            range = pe_range,
            showgrid = FALSE,
            zeroline = TRUE,
            zerolinecolor = "black",
            zerolinewidth = 0.5,
            fixedrange = FALSE,
            showline = TRUE,
            linecolor = "black"
          )
          base_layout$margin$r <- 80
        }

        p <- p %>%
          plotly::layout(
            title = list(text = base_layout$title, x = 0.5, xanchor = "center"),
            xaxis = base_layout$xaxis,
            yaxis = base_layout$yaxis,
            yaxis2 = base_layout$yaxis2,
            showlegend = TRUE,
            legend = base_layout$legend,
            hovermode = base_layout$hovermode,
            uirevision = base_layout$uirevision,
            plot_bgcolor = base_layout$plot_bgcolor,
            paper_bgcolor = base_layout$paper_bgcolor,
            margin = base_layout$margin
          ) %>%
          apply_standard_plotly_config(filename = "sdma_validation_plot", add_csv_download = TRUE) %>%
          plotly::event_register("plotly_relayout")

        return(p)

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = list(text = paste("Error:", e$message), x = 0.5), 
                         uirevision = "sdma_validation_zoom")
      })
    })

    # Update datetime inputs AND explicitly track time_range() to prevent zoom reset
    relayout_debounced <- debounce(reactive({
      event_data("plotly_relayout", source = "validation_plot")
    }), 500)

    observeEvent(relayout_debounced(), {
      rd <- relayout_debounced()
      if (is.null(rd)) return()

      if (!is.null(rd$xaxis.range) && length(rd$xaxis.range) == 2) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$xaxis.range[1], tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$xaxis.range[2], tz = "UTC"))
        
        # Explicitly save zoom state so renderPlotly won't reset it
        time_range(c(rd$xaxis.range[1], rd$xaxis.range[2]))
        
      } else if (!is.null(rd$`xaxis.range[0]`)) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$`xaxis.range[0]`, tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$`xaxis.range[1]`, tz = "UTC"))
          
        # Explicitly save zoom state
        time_range(c(rd$`xaxis.range[0]`, rd$`xaxis.range[1]`))
        
      } else if (isTRUE(rd$`xaxis.autorange`)) {
        req(vh_hrm_peclet())
        date_range <- range(vh_hrm_peclet()$datetime, na.rm = TRUE)
        shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
        shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
        
        # Clear zoom state to allow auto-ranging
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
      
      plotly::plotlyProxy("validation_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list(
          "xaxis.range" = t_range
        ))
    })

    # Reset zoom
    observeEvent(input$reset_zoom, {
      req(vh_hrm_peclet())
      date_range <- range(vh_hrm_peclet()$datetime, na.rm = TRUE)
      shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
      shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
      
      time_range(NULL)
      
      plotly::plotlyProxy("validation_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE))
    })

    # Code generation
    observe({
      if (!is.null(code_tracker)) {
        if (!is.null(input$sensor_position)) {
          code_tracker$add_step(
            step_name = "sDMA Validation Visualisation",
            code = sprintf(
              "# sDMA validation plot for %s sensor(s)",
              paste(input$sensor_position, collapse = ", ")
            )
          )
        }
      }
    })

  })
}
