# mod_7_sdma.R
# Module for sDMA (Selectable Dual Method Approach)
#
# Tab 7: Selectable DMA (sDMA)
# Applies method switching logic based on recalculated Peclet numbers using
# calibrated secondary methods and newly recalculated Peclet numbers.

# UI ----
sdmaUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Configuration
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "About sDMA",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p("sDMA (Selectable Dual Method Approach) automatically switches between methods based on flow conditions."),
          tags$ul(
            tags$li(strong("Step 1:"), " Select secondary method and Péclet threshold"),
            tags$li(strong("Step 2:"), " Apply sDMA switching logic")
          ),
          hr(),
          p(tags$small(em("Peclet number determines the theoretical validity limit of HRM. When Pe ≥ threshold, sDMA switches to the secondary method.")))
        ),

        # Heartwood warning (conditionally displayed)
        uiOutput(ns("heartwood_warning")),

        # Calibration requirement warning (conditionally displayed)
        uiOutput(ns("calibration_warning")),

        # sDMA settings
        box(
          width = 12,
          title = "sDMA Configuration",
          status = "primary",
          solidHeader = TRUE,

          h5("Sensor Positions:"),
          checkboxGroupInput(
            ns("sensor_positions"),
            NULL,
            choices = c("Outer" = "outer", "Inner" = "inner"),
            selected = "outer",
            inline = TRUE
          ),

          hr(),

          h5("Secondary Methods:"),
          helpText(icon("info-circle"), " Select one or more methods to apply sDMA switching."),
          uiOutput(ns("secondary_method_checkboxes")),

          hr(),

          numericInput(
            ns("peclet_threshold"),
            "Peclet Threshold:",
            value = 1.0,
            min = 0.1, max = 5.0, step = 0.1
          ),

          helpText(icon("info-circle"), " When Pe ≥ threshold, use secondary method. Otherwise use HRM."),

          hr(),

          actionButton(
            ns("apply_sdma"),
            "Apply sDMA Switching",
            icon = icon("exchange-alt"),
            class = "btn-primary btn-block"
          ),

          helpText(
            style = "margin-top: 10px; color: #666;",
            icon("lightbulb"),
            " sDMA will be applied to all selected sensor/method combinations."
          )
        )
      ),

      # Right column: Results
      column(
        width = 8,

        box(
          width = 12,
          title = "sDMA Results",
          status = "success",
          solidHeader = TRUE,

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_sdma_result")),

            tabsetPanel(
              id = ns("results_tabs"),

              tabPanel(
                "Method Usage Summary",
                br(),
                shinycssloaders::withSpinner(
                  plotOutput(ns("method_usage_plot"), height = "450px"),
                  type = 6,
                  color = "#3c8dbc"
                ),
                hr(),
                verbatimTextOutput(ns("sdma_summary"))
              ),

              tabPanel(
                "Peclet vs Velocity",
                br(),
                p("Visualise switching points: HRM below threshold, secondary method above."),                shinycssloaders::withSpinner(
                  plotOutput(ns("peclet_scatter_plot"), height = "600px"),
                  type = 6,
                  color = "#3c8dbc"
                )
              )

#              tabPanel(
#                "Time Series",
#                br(),
#                p("View the combined sDMA velocity time series."),
#                plotly::plotlyOutput(ns("sdma_timeseries"), height = "400px")
#              ),

#              tabPanel(
#                "Data Table",
#                br(),
#                DT::DTOutput(ns("sdma_table"))
#              )
            )
          ),

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_sdma_result")),
            div(
              style = "text-align: center; padding: 50px;",
              icon("info-circle", style = "font-size: 48px; color: #999;"),
              p(style = "color: #999; margin-top: 20px;", "No sDMA results yet. Follow the steps on the left to apply sDMA switching.")
            )
          )
        )
      )
    )
  )
}

# Server ----
sdmaServer <- function(id, vh_calibrated, primary_method = reactive("HRM"), probe_config = reactive(NULL), wood_properties = reactive(NULL), method_thresholds = NULL, code_tracker = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      vh_with_peclet = NULL,     # After Peclet recalculation
      vh_sdma = NULL,            # Combined sDMA results for all combinations
      sdma_results = list()      # Store individual results for each sensor/method combination
    )

    # Dynamic secondary method checkboxes based on calibrated data
    output$secondary_method_checkboxes <- renderUI({
      req(vh_calibrated())

      data <- vh_calibrated()

      # Get available methods from method column (transformed data is in long format)
      methods <- unique(data$method)
      methods <- methods[!is.na(methods)]

      # Remove HRM to get secondary methods
      secondary_methods <- setdiff(methods, "HRM")

      if (length(secondary_methods) > 0) {
        checkboxGroupInput(
          ns("secondary_methods"),
          NULL,
          choices = secondary_methods,
          selected = secondary_methods[1],
          inline = TRUE
        )
      } else {
        p(style = "color: #999;", "No secondary methods available")
      }
    })

    # Heartwood warning output
    output$heartwood_warning <- renderUI({
      req(wood_properties(), probe_config())

      # Validate probe/tree configuration to get tissue information
      validation <- validate_probe_tree_config(
        probe_config = probe_config(),
        wood_properties = wood_properties()
      )

      # Check if inner sensor is in heartwood
      if (!is.null(validation$inner_tissue) && validation$inner_tissue == "heartwood") {
        box(
          width = 12,
          title = NULL,
          status = "warning",
          solidHeader = FALSE,

          div(
            style = "padding: 10px;",
            p(
              icon("exclamation-triangle", class = "fa-lg"),
              strong(" Attention:"),
              " Based on your sapwood depth and probe geometry, the",
              strong(" Inner Sensor"),
              " is located in the heartwood."
            ),
            tags$ul(
              tags$li("Heartwood records zero flow and is not active in sap transport"),
              tags$li("Inner sensor data does not require spacing correction or calibration"),
              tags$li("Can be used as a continuous zero-flow reference")
            )
          )
        )
      } else {
        NULL  # No warning if not in heartwood
      }
    })

    # Calibration requirement warning output
    output$calibration_warning <- renderUI({
      # Check if calibration data is available
      vh_data <- vh_calibrated()

      # If no calibration data or it's empty, show warning
      if (is.null(vh_data) || (is.data.frame(vh_data) && nrow(vh_data) == 0)) {
        box(
          width = 12,
          title = NULL,
          status = "danger",
          solidHeader = FALSE,

          div(
            style = "padding: 10px;",
            p(
              icon("exclamation-circle", class = "fa-lg"),
              strong(" Calibration Required"),
              " - sDMA requires calibrated velocity data."
            ),
            tags$ul(
              tags$li(strong("sDMA cannot proceed"), " without calibrated secondary methods (MHR, Tmax, etc.)"),
              tags$li("Please complete ", strong("Method Calibration (Tab 6a)"), " first"),
              tags$li("Method calibration creates a unified dataset with all methods available for switching"),
              tags$li(em("Note: If you only need HRM data, you can skip sDMA and proceed directly to flux conversion"))
            )
          )
        )
      } else {
        # Check if secondary methods exist (not just HRM)
        available_methods <- unique(vh_data$method)
        secondary_methods <- setdiff(available_methods, "HRM")

        if (length(secondary_methods) == 0) {
          box(
            width = 12,
            title = NULL,
            status = "warning",
            solidHeader = FALSE,

            div(
              style = "padding: 10px;",
              p(
                icon("exclamation-triangle", class = "fa-lg"),
                strong(" No Secondary Methods Available")
              ),
              tags$ul(
                tags$li("Only HRM data is available in the calibrated dataset"),
                tags$li("sDMA requires at least one secondary method (MHR, Tmax, etc.)"),
                tags$li("Please ensure secondary methods were calculated and calibrated in Tab 6a"),
                tags$li(em("If you only need HRM, you can skip sDMA and proceed to flux conversion"))
              )
            )
          )
        } else {
          NULL  # Calibration data exists and has secondary methods
        }
      }
    })

    # Apply sDMA Switching (Multiple Combinations)
    observeEvent(input$apply_sdma, {
      req(vh_calibrated(), probe_config(), wood_properties())
      req(input$secondary_methods)
      req(input$sensor_positions)

      withProgress(message = "Applying sDMA switching...", value = 0, {
        tryCatch({
          # Get all combinations
          sensors <- input$sensor_positions
          methods <- input$secondary_methods
          n_combinations <- length(sensors) * length(methods)

          if (n_combinations == 0) {
            stop("Please select at least one sensor position and one secondary method")
          }

          incProgress(0.1, detail = paste("Processing", n_combinations, "combinations..."))

          incProgress(0.2, detail = "Applying sDMA switching...")

          # Pass calibrated data directly — apply_sdma_processing() computes the
          # Péclet number automatically from probe_config / wood_properties.
          vh_sdma_result <- sapfluxr::apply_sdma_processing(
            vh_results       = vh_calibrated(),
            secondary_method = methods,
            probe_config     = probe_config(),
            wood_properties  = wood_properties(),
            peclet_threshold = input$peclet_threshold,
            skip_low_peclet  = FALSE,
            show_progress    = FALSE
          )

          # Populate rv$vh_with_peclet for the validation module (needs Pe_corrected column).
          rv$vh_with_peclet <- vh_sdma_result %>%
            dplyr::filter(method == "HRM") %>%
            dplyr::rename(Pe_corrected = peclet_number)

          # Extract sDMA rows for selected sensors and add combination column.
          rv$vh_sdma <- vh_sdma_result %>%
            dplyr::filter(grepl("^sDMA:", method), sensor_position %in% sensors) %>%
            dplyr::mutate(
              combination = paste0(sensor_position, "_",
                                 gsub("^sDMA:", "", method)),
              sdma_source = selected_method,
              Vh_sdma = Vh_cm_hr
            )

          # Store individual results for compatibility
          all_results <- split(rv$vh_sdma, rv$vh_sdma$combination)

          # === DIAGNOSTIC OUTPUT ===
          cat("\n")
          cat(strrep("=", 70), "\n")
          cat("sDMA DIAGNOSTIC INFORMATION\n")
          cat(strrep("=", 70), "\n")
          cat("Total sDMA rows:", nrow(rv$vh_sdma), "\n")
          cat("Rows with NA in Vh_sdma:", sum(is.na(rv$vh_sdma$Vh_sdma)), "\n")
          cat("Rows with valid Vh_sdma:", sum(!is.na(rv$vh_sdma$Vh_sdma)), "\n")
          cat("\nBreakdown by combination:\n")
          for (combo in unique(rv$vh_sdma$combination)) {
            combo_data <- rv$vh_sdma[rv$vh_sdma$combination == combo, ]
            n_total <- nrow(combo_data)
            n_na <- sum(is.na(combo_data$Vh_sdma))
            n_valid <- sum(!is.na(combo_data$Vh_sdma))
            cat(sprintf("  %s: %d total, %d valid, %d NA (%.1f%% NA)\n",
                        combo, n_total, n_valid, n_na, 100*n_na/n_total))
          }
          cat("\nBreakdown by sdma_source:\n")
          source_table <- table(rv$vh_sdma$sdma_source, useNA = "ifany")
          print(source_table)
          cat(strrep("=", 70), "\n\n")
          # === END DIAGNOSTIC ===

          incProgress(0.2, detail = "Done!")

          # Track code
          if (!is.null(code_tracker)) {
            code_tracker$add_step(
              step_name = "Apply sDMA Switching",
              code = sprintf(
                '# Apply sDMA switching — Péclet number computed automatically.
# Sensors: %s
# Methods: %s
# Peclet threshold: %.2f

vh_sdma_result <- sapfluxr::apply_sdma_processing(
  vh_results       = vh_calibrated,
  secondary_method = c(%s),
  probe_config     = probe_config,
  wood_properties  = wood_properties,
  peclet_threshold = %.2f,
  skip_low_peclet  = FALSE,
  show_progress    = TRUE
)

vh_sdma <- vh_sdma_result %%>%%
  dplyr::filter(grepl("^sDMA:", method))',
                paste(sensors, collapse = ", "),
                paste(methods, collapse = ", "),
                input$peclet_threshold,
                paste0('"', paste(methods, collapse = '", "'), '"'),
                input$peclet_threshold
              ),
              description = sprintf(
                "Apply sDMA switching to %d combinations (Pe = %.1f)",
                n_combinations,
                input$peclet_threshold
              )
            )
          }

          showNotification(
            paste0("sDMA applied successfully to ", n_combinations, " sensor/method combination(s)!"),
            type = "message",
            duration = 3
          )

        }, error = function(e) {
          showNotification(
            paste("Error applying sDMA:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # Outputs ----

    # Flags
    output$has_sdma_result <- reactive({
      !is.null(rv$vh_sdma)
    })
    outputOptions(output, "has_sdma_result", suspendWhenHidden = FALSE)

    # sDMA summary
    output$sdma_summary <- renderPrint({
      req(rv$vh_sdma)

      data <- rv$vh_sdma

      cat("sDMA SWITCHING SUMMARY\n")
      cat(strrep("=", 60), "\n")
      cat("Peclet threshold:", input$peclet_threshold, "\n")
      cat("Combinations processed:", length(unique(data$combination)), "\n\n")

      # Overall method usage
      cat("Overall Method Usage:\n")
      cat(strrep("-", 60), "\n")
      counts <- table(data$sdma_source)
      for (method in names(counts)) {
        pct <- 100 * counts[method] / sum(counts)
        cat(sprintf("  %s: %d points (%.1f%%)\n", method, counts[method], pct))
      }
      cat("\n")

      # Breakdown by combination
      if (length(unique(data$combination)) > 1) {
        cat("Breakdown by Sensor/Method Combination:\n")
        cat(strrep("-", 60), "\n")
        for (combo in unique(data$combination)) {
          combo_data <- data %>% dplyr::filter(combination == combo)
          cat(sprintf("\n%s:\n", combo))
          combo_counts <- table(combo_data$sdma_source)
          for (method in names(combo_counts)) {
            pct <- 100 * combo_counts[method] / sum(combo_counts)
            cat(sprintf("  %s: %d points (%.1f%%)\n", method, combo_counts[method], pct))
          }
        }
      }
      cat("\n")
    })

    # Method usage plot
    output$method_usage_plot <- renderPlot({
      req(rv$vh_sdma)

      data <- rv$vh_sdma
      n_combinations <- length(unique(data$combination))

      # Use project standard sizes
      title_size <- 16
      axis_title_size <- 12
      tick_size <- 11

      if (n_combinations == 1) {
        # Single combination - simple bar chart
        usage_summary <- data %>%
          dplyr::group_by(sdma_source) %>%
          dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
          dplyr::mutate(
            percentage = 100 * count / sum(count),
            label = sprintf("%s\n%d (%.1f%%)", sdma_source, count, percentage)
          )

        p <- ggplot2::ggplot(usage_summary, ggplot2::aes(x = sdma_source, y = count, fill = sdma_source)) +
          ggplot2::geom_col(width = 0.6) +
          ggplot2::geom_text(ggplot2::aes(label = label), vjust = -0.5, size = 4.5, fontface = "bold") +
          ggplot2::labs(
            title = "Method Selection Frequency",
            subtitle = paste("sDMA Switching:", unique(data$combination)),
            x = "Method Used",
            y = "Number of Measurements"
          ) +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) + # Extra room at top
          ggplot2::theme_classic() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = title_size, hjust = 0.5), # Centred
            plot.subtitle = ggplot2::element_text(size = axis_title_size, hjust = 0.5), # Centred
            axis.title = ggplot2::element_text(size = axis_title_size, face = "bold"),
            axis.text = ggplot2::element_text(size = tick_size, color = "black"),
            legend.position = "none",
            plot.margin = ggplot2::margin(20, 20, 20, 20)
          )
      } else {
        # Multiple combinations - grouped bar chart
        usage_summary <- data %>%
          dplyr::group_by(combination, sdma_source) %>%
          dplyr::summarise(count = dplyr::n(), .groups = "drop")

        p <- ggplot2::ggplot(usage_summary, ggplot2::aes(x = combination, y = count, fill = sdma_source)) +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::labs(
            title = "Method Selection Frequency by Combination",
            subtitle = paste(n_combinations, "Sensor/Method Combinations"),
            x = "Sensor/Method Combination",
            y = "Number of Measurements",
            fill = "Method Used"
          ) +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) + # Extra room at top
          ggplot2::theme_classic() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = title_size, hjust = 0.5), # Centred
            plot.subtitle = ggplot2::element_text(size = axis_title_size, hjust = 0.5), # Centred
            axis.title = ggplot2::element_text(size = axis_title_size, face = "bold"),
            axis.text = ggplot2::element_text(size = tick_size, color = "black"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.margin = ggplot2::margin(20, 20, 20, 20)
          )
      }
      return(p)
    })

    # Peclet scatter plot
    output$peclet_scatter_plot <- renderPlot({
      req(rv$vh_sdma)

      data <- rv$vh_sdma %>%
        dplyr::filter(!is.na(peclet_number), !is.na(Vh_sdma))

      if (nrow(data) == 0) {
        return(NULL)
      }

      n_combinations <- length(unique(data$combination))

      # Use project standard sizes
      title_size <- 16
      axis_title_size <- 12
      tick_size <- 11

      p <- ggplot2::ggplot(data, ggplot2::aes(x = peclet_number, y = Vh_sdma, color = sdma_source)) +
        ggplot2::geom_point(alpha = 0.5, size = 2) +
        ggplot2::geom_vline(xintercept = input$peclet_threshold, linetype = "dashed",
                           color = "red", linewidth = 1) +
        ggplot2::annotate("text", x = input$peclet_threshold, y = max(data$Vh_sdma, na.rm = TRUE),
                         label = sprintf("Pe = %.1f", input$peclet_threshold),
                         hjust = -0.1, vjust = 1, color = "red", size = 4.5, fontface = "bold") +
        ggplot2::labs(
          title = "sDMA Switching Behaviour",
          subtitle = if (n_combinations == 1) {
            paste("Peclet Threshold =", input$peclet_threshold, "-", unique(data$combination))
          } else {
            paste("Peclet Threshold =", input$peclet_threshold, "-", n_combinations, "Combinations")
          },
          x = "Peclet Number (Corrected)",
          y = "Velocity (cm/hr)",
          color = "Method Used"
        ) +
        ggplot2::scale_color_brewer(palette = "Set1") +
        ggplot2::theme_classic() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = title_size, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = axis_title_size, hjust = 0.5),
          axis.title = ggplot2::element_text(size = axis_title_size, face = "bold"),
          axis.text = ggplot2::element_text(size = tick_size, color = "black"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(face = "bold"),
          plot.margin = ggplot2::margin(20, 20, 20, 20),
          strip.text = ggplot2::element_text(face = "bold", size = tick_size) # For faceted combos
        )

      # Add faceting if multiple combinations
      if (n_combinations > 1) {
        p <- p + ggplot2::facet_wrap(~combination, ncol = 2)
      }

      return(p)
    })

    # Time series plot - comparison of HRM, calibrated secondary, and sDMA result
    output$sdma_timeseries <- plotly::renderPlotly({

      tryCatch({
        # Need at least vh_with_peclet (HRM) and vh_calibrated (secondary methods)
        req(rv$vh_with_peclet)
        req(vh_calibrated())

        # Get HRM data (corrected, with recalculated Peclet)
        hrm_data <- rv$vh_with_peclet %>%
          dplyr::select(datetime, pulse_id, sensor_position, Vh_cm_hr) %>%
          dplyr::mutate(
            method = "HRM (Corrected)",
            data_type = "baseline"
          )

        # Get calibrated secondary methods
        calibrated_data <- vh_calibrated() %>%
          dplyr::filter(method != "HRM") %>%
          dplyr::select(datetime, pulse_id, sensor_position, method, Vh_cm_hr) %>%
          dplyr::mutate(
            method = paste0(method, " (Calibrated)"),
            data_type = "calibrated"
          )

        # Combine baseline and calibrated
        combined_data <- dplyr::bind_rows(hrm_data, calibrated_data)

        # Add sDMA results if available
        if (!is.null(rv$vh_sdma) && nrow(rv$vh_sdma) > 0) {
          sdma_data <- rv$vh_sdma %>%
            dplyr::filter(!is.na(datetime), !is.na(Vh_sdma)) %>%
            dplyr::select(datetime, pulse_id, sensor_position, combination, sdma_source, Vh_sdma) %>%
            dplyr::mutate(
              method = paste0("sDMA: ", sdma_source, " (", combination, ")"),
              Vh_cm_hr = Vh_sdma,
              data_type = "sdma"
            ) %>%
            dplyr::select(datetime, pulse_id, sensor_position, method, Vh_cm_hr, data_type)

          combined_data <- dplyr::bind_rows(combined_data, sdma_data)
        }

        if (nrow(combined_data) == 0) {
          return(plotly::plot_ly() %>%
                   plotly::layout(
                     title = "No data available for plotting",
                     uirevision = "sdma_timeseries_zoom"
                   ))
        }

        # Downsample if too many points. Coerce to integer indices -- seq() with
        # length.out returns doubles, which tibbles reject when subsetting rows.
        if (nrow(combined_data) > 10000) {
          sample_idx <- unique(as.integer(round(seq(1, nrow(combined_data), length.out = 10000))))
          combined_data <- combined_data[sample_idx, ]
        }

        # Color palette
        method_colors <- c(
          "HRM (Corrected)" = "#1f77b4",
          "MHR (Calibrated)" = "#ff7f0e",
          "Tmax_Coh (Calibrated)" = "#9467bd",
          "Tmax_Klu (Calibrated)" = "#8c564b"
        )

        # Create plot with separate traces for each method
        p <- plotly::plot_ly()

        for (m in unique(combined_data$method)) {
          method_data <- combined_data %>% dplyr::filter(method == m)

          # Determine line style based on data type
          line_style <- if (grepl("sDMA", m)) {
            list(width = 2.5)  # Thick for sDMA
          } else if (grepl("Corrected", m) && grepl("HRM", m)) {
            list(width = 2)  # Medium for HRM baseline
          } else {
            list(width = 1.5)  # Normal for calibrated methods
          }

          # Get color
          color <- if (m %in% names(method_colors)) {
            method_colors[[m]]
          } else {
            NULL  # Let plotly auto-assign
          }

          p <- p %>%
            plotly::add_trace(
              data = method_data,
              x = ~datetime,
              y = ~Vh_cm_hr,
              type = "scatter",
              mode = "lines",
              name = m,
              line = if (!is.null(color)) c(line_style, list(color = color)) else line_style,
              hovertemplate = paste(
                "<b>Time:</b> %{x|%Y-%m-%d %H:%M}<br>",
                "<b>Velocity:</b> %{y:.2f} cm/hr<br>",
                "<b>Method:</b>", m, "<br>",
                "<extra></extra>"
              )
            )
        }

        # Apply standard layout
        base_layout <- get_standard_layout(
          title = "sDMA Comparison: HRM Baseline, Calibrated Methods, and sDMA Results",
          xtitle = "Datetime",
          ytitle = "Velocity (cm/hr)",
          uirevision = "sdma_timeseries_zoom"
        )
        
        p <- p %>%
          plotly::layout(
            title = base_layout$title,
            xaxis = base_layout$xaxis,
            yaxis = base_layout$yaxis,
            hovermode = base_layout$hovermode,
            showlegend = base_layout$showlegend,
            legend = base_layout$legend,
            margin = base_layout$margin,
            plot_bgcolor = base_layout$plot_bgcolor,
            paper_bgcolor = base_layout$paper_bgcolor,
            uirevision = base_layout$uirevision
          )

        return(p)

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(
            title = paste("Error creating plot:", e$message),
            xaxis = list(title = "Datetime"),
            yaxis = list(title = "Velocity (cm/hr)"),
            uirevision = "sdma_timeseries_zoom"
          )
      })
    })

    # Data table
    output$sdma_table <- DT::renderDT({
      req(rv$vh_sdma)

      data <- rv$vh_sdma %>%
        dplyr::select(datetime, pulse_id, combination, sensor_position,
                     peclet_number, Vh_sdma, sdma_source, sdma_trigger) %>%
        dplyr::arrange(dplyr::desc(datetime))

      DT::datatable(
        data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        filter = 'top'  # Add filters for combination and other columns
      ) %>%
        DT::formatRound(columns = c("peclet_number", "Vh_sdma"), digits = 3)
    })

    # Return values for downstream modules
    return(list(
      vh_sdma = reactive({
        rv$vh_sdma
      }),
      vh_with_peclet = reactive({
        rv$vh_with_peclet
      })
    ))
  })
}
