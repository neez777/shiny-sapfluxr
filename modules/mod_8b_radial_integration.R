# mod_8b_radial_integration.R
# Module for Radial Integration → Tree Water Use (Q)
#
# Step 9: Radial Integration
# Integrates sap flux density across the sapwood area to calculate whole-tree
# water use, with inline hourly and daily time-series plots.
# Sap flux density conversion (Jv) lives in mod_8_flux_density.R.

# UI ----
radialIntegrationUI <- function(id) {
  ns <- NS(id)

  tagList(
    # ---- Row 1: Integration controls + summary ----
    fluidRow(
      # Left column: Configuration
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "About Radial Integration",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p("Radial integration scales sap flux density (Jv) across the sapwood area to estimate whole-tree water use (Q)."),
          tags$ul(
            tags$li(strong("Inputs:"), " Flux density at each sensor position (inner + outer)"),
            tags$li(strong("Geometry:"), " DBH, sapwood thickness, and bark thickness define the sapwood annulus"),
            tags$li(strong("Output:"), " Whole-tree water use Q (L/hr, L/day)")
          ),
          p(tags$small(em("Linear decay after Pausch et al. (2000)")))
        ),

        # Tree dimensions for integration
        box(
          width = 12,
          title = "Tree Water Use Integration",
          status = "warning",
          solidHeader = TRUE,

          helpText(
            icon("tree"),
            " Integrate flux density across sapwood area to calculate whole-tree water use (Q)."
          ),

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_flux_data")),
            p(em("Convert to flux density first (Step 8: Sap Flux Density)."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_flux_data")),

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
                  ns("sapwood_thickness_cm"),
                  "Sapwood Thickness (cm):",
                  value = 3.0,
                  min = 0.1,
                  max = 50,
                  step = 0.1
                )
              )
            ),

            numericInput(
              ns("bark_thickness_dbh_cm"),
              "Bark Thickness at DBH (cm):",
              value = 0,
              min = 0,
              max = 10,
              step = 0.1
            ),

            numericInput(
              ns("bark_thickness_probe_cm"),
              "Bark Thickness at Probe (cm, after shaving):",
              value = 0,
              min = 0,
              max = 10,
              step = 0.1
            ),

            hr(),

            tags$label(
              "Radial Integration Method:",
              tags$span(
                tabindex = "0",
                style = "margin-left: 6px; color: #3c8dbc; cursor: help;",
                title = paste(
                  "Linear decay (Pausch et al. 2000): sap flux declines linearly",
                  "from the adjacent sensor value to zero across an unmeasured",
                  "annulus, giving a mean of Jv / 2.\n\n",
                  "Constant velocity (nearest-neighbour): the adjacent sensor",
                  "value is applied unchanged across the unmeasured annulus."
                ),
                icon("circle-question")
              )
            ),
            selectInput(
              ns("integration_method"),
              label = NULL,
              choices = c(
                "Linear decay (Pausch et al. 2000)" = "linear_decay",
                "Constant velocity (nearest-neighbour)" = "constant_velocity"
              ),
              selected = "linear_decay"
            ),

            helpText(
              icon("info-circle"),
              " The method only affects sensorless annuli (where the sapwood",
              " extends past the deepest sensor or no inner sensor is fitted)."
            ),

            hr(),

            actionButton(
              ns("calculate_tree_water_use"),
              "Calculate Tree Water Use (Q)",
              icon = icon("tint"),
              class = "btn-warning",
              width = "100%"
            )
          )
        )
      ),

      # Right column: Results (tabbed — Summary | Water Use Plots)
      column(
        width = 8,

        box(
          width = 12,
          title = "Tree Water Use Results",
          status = "success",
          solidHeader = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_tree_water_use")),
            p(em("No tree water use data yet. Convert flux density first (Step 8), then click 'Calculate Tree Water Use (Q)'."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_tree_water_use")),

            tabsetPanel(
              id = ns("radial_results_tabs"),

              tabPanel(
                "Water Use Plots",
                br(),

                # ---- Plot controls ----
                fluidRow(
                  column(6,
                    h5("Methods to Display"),
                    uiOutput(ns("method_checkboxes"))
                  ),
                  column(6,
                    h5("Display"),
                    checkboxInput(
                      ns("show_points"),
                      "Show data points",
                      value = FALSE
                    )
                  )
                ),

                fluidRow(
                  column(4,
                    shinyWidgets::airDatepickerInput(
                      ns("start_datetime"),
                      "Start Date/Time:",
                      value = NULL,
                      timepicker = TRUE,
                      dateFormat = "yyyy-MM-dd HH:mm"
                    )
                  ),
                  column(4,
                    shinyWidgets::airDatepickerInput(
                      ns("end_datetime"),
                      "End Date/Time:",
                      value = NULL,
                      timepicker = TRUE,
                      dateFormat = "yyyy-MM-dd HH:mm"
                    )
                  ),
                  column(2,
                    br(),
                    actionButton(
                      ns("apply_range"),
                      "Apply",
                      icon = icon("clock"),
                      class = "btn-primary btn-sm",
                      style = "width: 100%; margin-top: 8px;"
                    )
                  ),
                  column(2,
                    br(),
                    actionButton(
                      ns("reset_zoom"),
                      "Reset",
                      icon = icon("refresh"),
                      class = "btn-default btn-sm",
                      style = "width: 100%; margin-top: 8px;"
                    )
                  )
                ),

                hr(),

                h5("Hourly (L/hr)"),
                helpText(
                  icon("info-circle"),
                  "Whole-tree water use integrated across sapwood area.",
                  "Click-drag to zoom, double-click to reset zoom."
                ),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("tree_water_use_plot_hourly"), height = "450px"),
                  type = 6,
                  color = "#00a65a"
                ),

                hr(),

                h5("Daily (L/day)"),
                helpText(
                  icon("info-circle"),
                  "Daily totals of tree water use.",
                  "Grouped bars show multiple methods side-by-side."
                ),
                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("tree_water_use_plot_daily"), height = "450px"),
                  type = 6,
                  color = "#00a65a"
                )
              ),

              tabPanel(
                "Summary",
                br(),

                verbatimTextOutput(ns("tree_water_use_summary")),

                hr(),

                p(
                  icon("check-circle"),
                  " Tree water use calculation is complete.",
                  " Proceed to ",
                  strong("Step 10: Temporal Aggregation"),
                  " for temporal summaries."
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
radialIntegrationServer <- function(id,
                                    flux_data = reactive(NULL),
                                    wood_properties = reactive(NULL),
                                    code_tracker = NULL,
                                    plot_settings = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      tree_water_use_data = NULL,
      tree_dimensions = NULL,
      integration_timestamp = NULL
    )

    # Flag: flux density data available (gates the integration controls)
    output$has_flux_data <- reactive({
      !is.null(flux_data())
    })
    outputOptions(output, "has_flux_data", suspendWhenHidden = FALSE)

    # Flag: tree water use computed
    output$has_tree_water_use <- reactive({
      !is.null(rv$tree_water_use_data)
    })
    outputOptions(output, "has_tree_water_use", suspendWhenHidden = FALSE)

    # Update tree dimension inputs from wood properties
    observe({
      req(wood_properties())

      wood <- wood_properties()

      if (inherits(wood, "WoodProperties") && !is.null(wood$tree_measurements)) {
        tree_meas <- wood$tree_measurements

        if (!is.null(tree_meas$dbh) && !is.na(tree_meas$dbh)) {
          updateNumericInput(session, "dbh_cm", value = tree_meas$dbh)
        }
        if (!is.null(tree_meas$sapwood_thickness) && !is.na(tree_meas$sapwood_thickness)) {
          updateNumericInput(session, "sapwood_thickness_cm", value = tree_meas$sapwood_thickness)
        }
        if (!is.null(tree_meas$bark_thickness_dbh) && !is.na(tree_meas$bark_thickness_dbh)) {
          updateNumericInput(session, "bark_thickness_dbh_cm", value = tree_meas$bark_thickness_dbh)
        }
        if (!is.null(tree_meas$bark_thickness_probe) && !is.na(tree_meas$bark_thickness_probe)) {
          updateNumericInput(session, "bark_thickness_probe_cm", value = tree_meas$bark_thickness_probe)
        }
      }
    })

    # Calculate tree water use (Q)
    observeEvent(input$calculate_tree_water_use, {
      req(flux_data())
      req(input$dbh_cm, input$sapwood_thickness_cm)

      withProgress(message = "Calculating tree water use...", {

        tryCatch({
          # Add tree dimensions as columns to flux data
          flux_with_dims <- flux_data()
          flux_with_dims$dbh <- input$dbh_cm
          flux_with_dims$sapwood_thickness <- input$sapwood_thickness_cm
          flux_with_dims$bark_thickness_dbh <- input$bark_thickness_dbh_cm
          flux_with_dims$bark_thickness_probe <- input$bark_thickness_probe_cm

          # Apply sap flux integration across sapwood area
          tree_water_use_data <- sapfluxr::apply_sap_flux_integration(
            flux_data = flux_with_dims,
            dbh_col = "dbh",
            sapwood_thickness_col = "sapwood_thickness",
            bark_thickness_dbh_col = "bark_thickness_dbh",
            bark_thickness_probe_col = "bark_thickness_probe",
            method = input$integration_method
          )

          # Calculate sapwood area for reporting
          r_outer <- (input$dbh_cm / 2) - input$bark_thickness_dbh_cm
          r_inner <- r_outer - input$sapwood_thickness_cm
          sapwood_area <- pi * (r_outer^2 - r_inner^2)

          # Store results
          rv$tree_water_use_data <- tree_water_use_data
          rv$tree_dimensions <- list(
            dbh = input$dbh_cm,
            sapwood_thickness = input$sapwood_thickness_cm,
            bark_thickness_dbh = input$bark_thickness_dbh_cm,
            bark_thickness_probe = input$bark_thickness_probe_cm,
            sapwood_area = sapwood_area,
            integration_method = input$integration_method
          )
          rv$integration_timestamp <- Sys.time()

          # Code generation
          if (!is.null(code_tracker) && !is.logical(code_tracker)) {
            code_tracker$add_step(
              step_name = "Calculate Tree Water Use",
              code = sprintf(
                paste0(
                  '# Add tree dimensions as columns, then integrate across sapwood\n',
                  'flux_data$dbh                  <- %.2f  # cm\n',
                  'flux_data$sapwood_thickness        <- %.2f  # cm\n',
                  'flux_data$bark_thickness_dbh   <- %.2f  # cm (full bark at DBH)\n',
                  'flux_data$bark_thickness_probe <- %.2f  # cm (remaining bark at probe site)\n',
                  'tree_water_use <- sapfluxr::apply_sap_flux_integration(\n',
                  '  flux_data              = flux_data,\n',
                  '  bark_thickness_dbh_col = "bark_thickness_dbh",\n',
                  '  bark_thickness_probe_col = "bark_thickness_probe",\n',
                  '  method                 = "%s"\n',
                  ')\n',
                  '# Sapwood area: %.2f cm²'
                ),
                input$dbh_cm,
                input$sapwood_thickness_cm,
                input$bark_thickness_dbh_cm,
                input$bark_thickness_probe_cm,
                input$integration_method,
                sapwood_area
              ),
              description = sprintf("Integrated flux across sapwood area (DBH: %.1f cm, Sapwood: %.1f cm, Area: %.1f cm²)",
                                   input$dbh_cm, input$sapwood_thickness_cm, sapwood_area)
            )
          }

          showNotification(
            sprintf("Tree water use calculated! DBH: %.1f cm, Sapwood depth: %.1f cm",
                    input$dbh_cm, input$sapwood_thickness_cm),
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

    # Tree water use summary
    output$tree_water_use_summary <- renderText({
      req(rv$tree_water_use_data)

      q_data <- rv$tree_water_use_data

      # Get method breakdown if available
      if ("method_label" %in% names(q_data)) {
        method_summary <- q_data %>%
          dplyr::group_by(method_label) %>%
          dplyr::summarise(
            n = dplyr::n(),
            mean_Q = mean(Q_total_L_hr, na.rm = TRUE),
            mean_daily = mean(Q_total_L_hr, na.rm = TRUE) * 24,  # Mean instantaneous rate × 24 h
            .groups = "drop"
          )

        method_text <- paste0(
          "\nMethod Breakdown:\n",
          paste(
            sprintf("  %s: %d points, mean = %.2f L/hr, mean daily = %.1f L/day",
                    method_summary$method_label,
                    method_summary$n,
                    method_summary$mean_Q,
                    method_summary$mean_daily),
            collapse = "\n"
          ),
          "\n"
        )
      } else {
        method_text <- ""
      }

      sprintf(
        paste0(
          "Tree Water Use Summary\n\n",
          "Tree Dimensions:\n",
          "  DBH: %.2f cm\n",
          "  Sapwood Thickness: %.2f cm\n",
          "  Sapwood Area: %.2f cm²\n\n",
          "Integration Method: %s\n",
          "Calculated: %s\n\n",
          "Data Points: %d\n",
          "Date Range: %s to %s\n",
          "%s",
          "\nOverall Water Use Statistics:\n",
          "  Mean: %.3f L/hr\n",
          "  Median: %.3f L/hr\n",
          "  Mean Daily Water Use: %.2f L/day\n",
          "  Min: %.3f L/hr\n",
          "  Max: %.3f L/hr"
        ),
        rv$tree_dimensions$dbh,
        rv$tree_dimensions$sapwood_thickness,
        rv$tree_dimensions$sapwood_area,
        rv$tree_dimensions$integration_method,
        format(rv$integration_timestamp, "%Y-%m-%d %H:%M:%S"),
        nrow(q_data),
        format(min(q_data$datetime), "%Y-%m-%d"),
        format(max(q_data$datetime), "%Y-%m-%d"),
        method_text,
        mean(q_data$Q_total_L_hr, na.rm = TRUE),
        median(q_data$Q_total_L_hr, na.rm = TRUE),
        mean(q_data$Q_total_L_hr, na.rm = TRUE) * 24,
        min(q_data$Q_total_L_hr, na.rm = TRUE),
        max(q_data$Q_total_L_hr, na.rm = TRUE)
      )
    })

    # ==================================================================
    # INLINE WATER-USE PLOTS
    # ==================================================================

    # Reactive: Time range
    time_range <- reactiveVal(NULL)

    # Initialise datetime range from data
    observe({
      req(rv$tree_water_use_data)

      data <- rv$tree_water_use_data
      date_range <- range(data$datetime, na.rm = TRUE)

      shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
      shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])
    })

    # Dynamic method checkboxes based on available data
    output$method_checkboxes <- renderUI({
      req(rv$tree_water_use_data)

      data <- rv$tree_water_use_data
      methods <- c()

      if ("method_label" %in% names(data)) {
        methods <- unique(data$method_label)
        methods <- methods[!is.na(methods)]
      } else if ("method" %in% names(data)) {
        methods <- unique(data$method)
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

    # Reactive: Filtered plot data (tree water use)
    plot_data_water_use <- reactive({
      req(rv$tree_water_use_data)

      data <- rv$tree_water_use_data

      # Filter by selected methods if specified and method_label exists
      if (!is.null(input$methods_selected) && length(input$methods_selected) > 0) {
        if ("method_label" %in% names(data)) {
          data <- data %>% dplyr::filter(method_label %in% input$methods_selected)
        }
      }

      # Apply time range filter
      if (!is.null(time_range())) {
        data <- data %>%
          dplyr::filter(datetime >= time_range()[1], datetime <= time_range()[2])
      }

      # Sort by method and datetime
      if ("method_label" %in% names(data)) {
        data <- data %>% dplyr::arrange(method_label, datetime)
      } else {
        data <- data %>% dplyr::arrange(datetime)
      }

      return(data)
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
        if (nrow(q_data) > 30000) {
          sample_idx <- seq(1, nrow(q_data), length.out = 30000)
          q_data <- q_data[sample_idx, ]
        }

        mode <- if (input$show_points) "lines+markers" else "lines"

        fig <- plotly::plot_ly(source = "water_use_hourly_plot")

        methods <- unique(q_data$method_label %||% "Tree Water Use")

        for (m in methods) {
          trace_data <- q_data %>% dplyr::filter(method_label == m)
          if (nrow(trace_data) == 0) next

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
        dims <- rv$tree_dimensions
        title_text <- if (!is.null(dims) && !is.null(dims$dbh)) {
          sprintf("Tree Water Use (DBH: %.1f cm, Sapwood: %.1f cm)",
                  dims$dbh, dims$sapwood_thickness)
        } else {
          "Tree Water Use"
        }

        base_layout <- get_standard_layout(
          title = title_text,
          xtitle = "Date",
          ytitle = "Water Use (L/hr)",
          uirevision = "water_use_hourly_zoom"
        )

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
        if (!"method_label" %in% names(daily_totals)) {
          daily_totals$method_label <- "Tree Water Use"
        }

        fig <- plotly::plot_ly(source = "water_use_daily_plot")

        for (m in unique(daily_totals$method_label)) {
          trace_data <- daily_totals %>% dplyr::filter(method_label == m)

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

        dims <- rv$tree_dimensions
        title_text <- if (!is.null(dims) && !is.null(dims$dbh)) {
          sprintf("Daily Tree Water Use (DBH: %.1f cm)", dims$dbh)
        } else {
          "Daily Tree Water Use"
        }

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

    # Update datetime inputs when user zooms the hourly plot
    relayout_debounced <- debounce(reactive({
      event_data("plotly_relayout", source = "water_use_hourly_plot")
    }), 500)

    observeEvent(relayout_debounced(), {
      rd <- relayout_debounced()
      if (is.null(rd)) return()

      if (!is.null(rd$xaxis.range) && length(rd$xaxis.range) == 2) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$xaxis.range[1], tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$xaxis.range[2], tz = "UTC"))
        time_range(c(rd$xaxis.range[1], rd$xaxis.range[2]))

      } else if (!is.null(rd$`xaxis.range[0]`)) {
        shinyWidgets::updateAirDateInput(session, "start_datetime",
          value = as.POSIXct(rd$`xaxis.range[0]`, tz = "UTC"))
        shinyWidgets::updateAirDateInput(session, "end_datetime",
          value = as.POSIXct(rd$`xaxis.range[1]`, tz = "UTC"))
        time_range(c(rd$`xaxis.range[0]`, rd$`xaxis.range[1]`))

      } else if (isTRUE(rd$`xaxis.autorange`)) {
        req(rv$tree_water_use_data)
        date_range <- range(rv$tree_water_use_data$datetime, na.rm = TRUE)
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

      plotly::plotlyProxy("water_use_hourly_plot", session) %>%
        plotly::plotlyProxyInvoke("relayout", list("xaxis.range" = t_range))
    })

    # Reset zoom
    observeEvent(input$reset_zoom, {
      time_range(NULL)

      if (!is.null(rv$tree_water_use_data)) {
        date_range <- range(rv$tree_water_use_data$datetime, na.rm = TRUE)

        shinyWidgets::updateAirDateInput(session, "start_datetime", value = date_range[1])
        shinyWidgets::updateAirDateInput(session, "end_datetime", value = date_range[2])

        plotly::plotlyProxy("water_use_hourly_plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", list("xaxis.autorange" = TRUE))
      }
    })

    # Return values for downstream modules
    return(list(
      tree_water_use_data = reactive(rv$tree_water_use_data),
      has_tree_water_use = reactive(!is.null(rv$tree_water_use_data)),
      tree_dimensions = reactive(rv$tree_dimensions)
    ))
  })
}
