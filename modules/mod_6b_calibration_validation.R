# mod_6b_calibration_validation.R
# Module for Calibration Validation
#
# Tab 6b: Calibration Validation
# Time series plots to verify the "fit" of calibrated data against HRM baseline.
# Shows before/after calibration comparison for secondary methods.

# UI ----
calibrationValidationUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Configuration
      column(
        width = 3,
        box(
          width = 12,
          title = "Plot Controls",
          status = "primary",
          solidHeader = TRUE,

          selectInput(
            ns("sensor_position"),
            "Sensor Position:",
            choices = c("Outer" = "outer", "Inner" = "inner"),
            selected = "outer"
          ),

          hr(),

          h5("Calibration States"),
          helpText("Toggle Raw and Calibrated traces"),
          checkboxGroupInput(
            ns("calibration_states"),
            NULL,
            choices = c(
              "Raw (Before Calibration)" = "raw",
              "Calibrated (After Calibration)" = "calibrated"
            ),
            selected = c("raw", "calibrated")
          ),

          hr(),

          h5("Methods to Show"),
          helpText("HRM is always shown as baseline"),
          uiOutput(ns("method_checkboxes"))
        ),

        box(
          width = 12,
          title = "Time Range",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,

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

          fluidRow(
            column(6, actionButton(ns("apply_range"), "Apply", class = "btn-primary btn-block")),
            column(6, actionButton(ns("reset_zoom"), "Reset Zoom", class = "btn-default btn-block"))
          )
        )
      ),

      # Right column: Plot and Results
      column(
        width = 9,
        box(
          width = 12,
          title = "Calibration Validation",
          status = "primary",
          solidHeader = TRUE,
          
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("validation_plot"), height = "600px"),
            type = 6,
            color = "#3c8dbc"
          )
        )
      )
    )
  )
}

# Server ----
calibrationValidationServer <- function(id,
                                         vh_raw = reactive(NULL),
                                         vh_calibrated = reactive(NULL),
                                         weather_data = reactive(NULL),
                                         code_tracker = NULL,
                                         plot_settings = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # Internal state
    time_range <- reactiveVal(NULL)

    # Initialise date/time range inputs when raw data becomes available
    observe({
      req(vh_raw())

      data <- vh_raw()
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

    # Dynamic method checkboxes
    output$method_checkboxes <- renderUI({
      req(vh_raw())

      # Get available secondary methods
      methods <- unique(vh_raw()$method)
      secondary_methods <- methods[methods != "HRM"]

      if (length(secondary_methods) == 0) {
        return(p(em("No secondary methods available.")))
      }

      checkboxGroupInput(
        session$ns("methods_selected"),
        NULL,
        choices = secondary_methods,
        selected = secondary_methods
      )
    })

    observeEvent(input$apply_range, {
      time_range(c(input$start_datetime, input$end_datetime))
    })

    # Calibration Validation Plot - Stable Base Layer (HRM)
    output$validation_plot <- plotly::renderPlotly({
      req(vh_raw())
      req(input$sensor_position)

      raw_data <- vh_raw()
      sensor <- input$sensor_position

      tryCatch({
        # Prepare HRM Baseline (always shown as base)
        hrm_base <- raw_data %>%
          dplyr::filter(sensor_position == sensor, method == "HRM")

        if (nrow(hrm_base) == 0) {
          return(
            plotly::plot_ly() %>%
              plotly::layout(
                title = paste("No HRM data for", sensor, "sensor"),
                xaxis = list(title = "Datetime"),
                yaxis = list(title = "Velocity (cm/hr)")
              )
          )
        }

        # Get Style
        style_config <- plot_settings()
        hrm_style <- get_plot_style(method = "HRM", sensor = sensor, is_corrected = TRUE, config = style_config)

        # Determine HRM column (prioritize Wound Corrected > Spacing Corrected > Raw)
        hrm_col <- if ("Vh_cm_hr_wc" %in% names(hrm_base)) "Vh_cm_hr_wc" else 
                   if ("Vh_cm_hr_sc" %in% names(hrm_base)) "Vh_cm_hr_sc" else "Vh_cm_hr"
        
        # Create Plot with HRM only
        p <- plotly::plot_ly(source = "validation_plot") %>%
          plotly::add_trace(
            data = hrm_base,
            x = ~datetime,
            y = as.formula(paste0("~", hrm_col)),
            type = "scatter",
            mode = "lines",
            name = "HRM (Corrected)",
            line = hrm_style,
            hovertemplate = paste0("<b>HRM (Baseline)</b><br>Time: %{x}<br>Vel: %{y:.2f} (", hrm_col, ")<extra></extra>")
          )

        # Standard Layout with uirevision
        base_layout <- get_standard_layout(
          title = sprintf("Calibration Validation: %s Sensor", toupper(sensor)),
          xtitle = "Datetime",
          ytitle = "Velocity (cm/hr)",
          uirevision = "calibration_validation_zoom"
        )

        p <- p %>%
          plotly::layout(
            title = base_layout$title,
            xaxis = base_layout$xaxis,
            yaxis = base_layout$yaxis,
            showlegend = TRUE,
            legend = base_layout$legend,
            hovermode = base_layout$hovermode,
            uirevision = base_layout$uirevision,
            plot_bgcolor = base_layout$plot_bgcolor,
            paper_bgcolor = base_layout$paper_bgcolor,
            margin = base_layout$margin
          ) %>%
          apply_standard_plotly_config(filename = "calibration_validation", add_csv_download = TRUE) %>%
          plotly::event_register("plotly_relayout")

        return(p)

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message), uirevision = "calibration_validation_zoom")
      })
    })

    # Handle dynamic overlays (Raw/Calibrated/Methods) using plotlyProxy
    observe({
      req(vh_raw())
      req(input$sensor_position)
      
      # Dependencies
      raw_data <- vh_raw()
      cal_data <- vh_calibrated()
      sensor <- input$sensor_position
      methods <- input$methods_selected
      states <- input$calibration_states
      
      # 1. Clear all overlays (everything except the base HRM trace at index 0)
      tryCatch({
        for (i in 1:20) { # Max possible methods/states
          plotly::plotlyProxy("validation_plot", session) %>%
            plotly::plotlyProxyInvoke("deleteTraces", list(1))
        }
      }, error = function(e) {})

      # 2. Add back selected traces
      style_config <- plot_settings()
      
      for (m in methods) {
        # Raw State
        if ("raw" %in% states) {
          m_raw <- raw_data %>% dplyr::filter(sensor_position == sensor, method == m)
          if (nrow(m_raw) > 0) {
            style <- get_plot_style(method = m, sensor = sensor, is_corrected = FALSE, config = style_config)
            plotly::plotlyProxy("validation_plot", session) %>%
              plotly::plotlyProxyInvoke("addTraces", list(
                x = m_raw$datetime, y = m_raw$Vh_cm_hr,
                type = "scatter", mode = "lines",
                name = paste0(m, " (Raw)"), line = style,
                hovertemplate = paste0("<b>", m, " (Raw)</b><br>Time: %{x}<br>Vel: %{y:.2f}<extra></extra>")
              ))
          }
        }
        
        # Calibrated State
        if ("calibrated" %in% states && !is.null(cal_data) && nrow(cal_data) > 0) {
          m_cal <- cal_data %>% dplyr::filter(sensor_position == sensor, method == m)
          if (nrow(m_cal) > 0) {
            style <- get_plot_style(method = m, sensor = sensor, is_corrected = TRUE, config = style_config)
            y_col <- if ("Vs_cm_hr" %in% names(m_cal)) "Vs_cm_hr" else "Vh_cm_hr"
            plotly::plotlyProxy("validation_plot", session) %>%
              plotly::plotlyProxyInvoke("addTraces", list(
                x = m_cal$datetime, y = m_cal[[y_col]],
                type = "scatter", mode = "lines",
                name = paste0(m, " (Calibrated)"), line = style,
                hovertemplate = paste0("<b>", m, " (Cal)</b><br>Time: %{x}<br>Vel: %{y:.2f}<extra></extra>")
              ))
          }
        }
      }
    }) %>% bindEvent(input$sensor_position, input$methods_selected, input$calibration_states, vh_calibrated())

    # Update datetime inputs when user zooms the validation plot
    val_relayout_debounced <- debounce(reactive({
      event_data("plotly_relayout", source = "validation_plot")
    }), 500)

    observeEvent(val_relayout_debounced(), {
      rd <- val_relayout_debounced()
      if (is.null(rd)) return()

      if (!is.null(rd$xaxis.range) && length(rd$xaxis.range) == 2) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$xaxis.range[1], tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$xaxis.range[2], tz = "UTC"))
      } else if (!is.null(rd$`xaxis.range[0]`)) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$`xaxis.range[0]`, tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$`xaxis.range[1]`, tz = "UTC"))
      } else if (isTRUE(rd$`xaxis.autorange`)) {
        req(vh_raw())
        date_range <- range(vh_raw()$datetime, na.rm = TRUE)
        shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
        shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
      }
    })

    observeEvent(input$apply_range, {
      req(input$start_datetime, input$end_datetime)
      plotly::plotlyProxy("validation_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list(
          "xaxis.range" = list(
            format(input$start_datetime, "%Y-%m-%d %H:%M:%S"),
            format(input$end_datetime, "%Y-%m-%d %H:%M:%S")
          )
        ))
    })

    # Reset zoom
    observeEvent(input$reset_zoom, {
      req(vh_raw())
      date_range <- range(vh_raw()$datetime, na.rm = TRUE)
      shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
      shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])

      plotly::plotlyProxy("validation_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE))
    })

    # Code generation
    observe({
      if (!is.null(code_tracker)) {
        # Track validation visualisation
        if (!is.null(input$calibration_states) && !is.null(input$sensor_position)) {
          code_tracker$add_step(
            step_name = "Calibration Validation",
            code = sprintf(
              "# Calibration validation - comparing %s velocities for %s sensor",
              paste(input$calibration_states, collapse = " and "),
              input$sensor_position
            )
          )
        }
      }
    })

  })
}
