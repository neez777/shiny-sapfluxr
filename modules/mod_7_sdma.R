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
            tags$li(strong("Step 1:"), " Recalculate Peclet numbers using calibrated HRM velocities"),
            tags$li(strong("Step 2:"), " Select secondary method and Peclet threshold"),
            tags$li(strong("Step 3:"), " Apply sDMA switching logic")
          ),
          hr(),
          p(tags$small(em("Peclet number determines the theoretical validity limit of HRM. When Pe ≥ threshold, sDMA switches to the secondary method.")))
        ),

        # Peclet recalculation
        box(
          width = 12,
          title = "Step 1: Recalculate Peclet Number",
          status = "warning",
          solidHeader = TRUE,

          p("Recalculate Peclet numbers using the latest calibrated HRM velocities."),
          helpText(icon("info-circle"), " This ensures switching thresholds reflect wound and spacing corrections."),

          actionButton(
            ns("recalculate_peclet"),
            "Recalculate Peclet Number",
            icon = icon("calculator"),
            class = "btn-warning btn-block"
          ),

          hr(),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_peclet_recalc")),
            div(
              style = "background: #f0f9ff; padding: 10px; border-radius: 4px; margin-top: 10px;",
              icon("check-circle", style = "color: green;"),
              strong(" Peclet Recalculated"),
              br(),
              verbatimTextOutput(ns("peclet_summary"))
            )
          )
        ),

        # sDMA settings
        box(
          width = 12,
          title = "Step 2 & 3: sDMA Configuration",
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
                plotOutput(ns("method_usage_plot"), height = "350px"),
                hr(),
                verbatimTextOutput(ns("sdma_summary"))
              ),

              tabPanel(
                "Peclet vs Velocity",
                br(),
                p("Visualize switching points: HRM below threshold, secondary method above."),
                plotOutput(ns("peclet_scatter_plot"), height = "400px")
              ),

              tabPanel(
                "Time Series",
                br(),
                p("View the combined sDMA velocity time series."),
                plotly::plotlyOutput(ns("sdma_timeseries"), height = "400px")
              ),

              tabPanel(
                "Data Table",
                br(),
                DT::DTOutput(ns("sdma_table"))
              )
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

    # Step 1: Recalculate Peclet Number
    observeEvent(input$recalculate_peclet, {
      req(vh_calibrated(), probe_config(), wood_properties())

      withProgress(message = "Recalculating Peclet numbers...", value = 0, {
        tryCatch({
          incProgress(0.2, detail = "Validating configuration...")

          # Validate configurations (ProbeConfiguration now has probe_spacing active binding)
          probe <- probe_config()
          wood <- wood_properties()

          if (is.null(probe)) {
            stop("Probe configuration is NULL. Please ensure probe configuration is loaded.")
          }

          if (is.null(wood)) {
            stop("Wood properties is NULL. Please ensure wood properties are loaded.")
          }

          incProgress(0.2, detail = "Preparing data...")

          # Get calibrated data (transformed format - long)
          vh_data <- vh_calibrated()

          # Filter to HRM rows only
          hrm_data <- vh_data %>%
            dplyr::filter(method == "HRM")

          if (nrow(hrm_data) == 0) {
            stop("No HRM data found in calibrated results")
          }

          incProgress(0.3, detail = "Recalculating Peclet...")

          # Recalculate Peclet using the function from 04d_apply_calibration.R
          # This function uses the corrected velocity column (auto-detected or specified)
          hrm_with_peclet <- sapfluxr::recalculate_peclet(
            vh_results = hrm_data,
            probe_config = probe_config(),
            wood_properties = wood_properties(),
            velocity_col = "Vh_cm_hr",  # Use the velocity column from transformed data
            peclet_col = "Pe_corrected"
          )

          # Store result
          rv$vh_with_peclet <- hrm_with_peclet

          incProgress(0.4, detail = "Done!")

          # Track code
          if (!is.null(code_tracker)) {
            code_tracker$add_step(
              step_name = "Recalculate Peclet Number",
              code = 'vh_with_peclet <- sapfluxr::recalculate_peclet(
  vh_results = hrm_data,
  probe_config = probe_config,
  wood_properties = wood_properties,
  velocity_col = "Vh_cm_hr",
  peclet_col = "Pe_corrected"
)',
              description = "Recalculate Peclet numbers using calibrated velocities"
            )
          }

          showNotification(
            "Peclet numbers recalculated successfully!",
            type = "message",
            duration = 3
          )

        }, error = function(e) {
          showNotification(
            paste("Error recalculating Peclet:", e$message),
            type = "error",
            duration = 10
          )
        })
      })
    })

    # Step 3: Apply sDMA Switching (Multiple Combinations)
    observeEvent(input$apply_sdma, {
      req(rv$vh_with_peclet)
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

          # Store individual results
          all_results <- list()
          result_counter <- 0

          # Prepare data for parked sDMA function
          # Step 1: Get HRM data with recalculated Peclet numbers
          hrm_data <- rv$vh_with_peclet %>%
            dplyr::select(datetime, pulse_id, sensor_position, method,
                         Vh_cm_hr, hrm_peclet_number = Pe_corrected)

          # Step 2: Get calibrated secondary methods
          vh_calibrated_data <- vh_calibrated() %>%
            dplyr::filter(
              sensor_position %in% sensors,
              method %in% methods
            ) %>%
            dplyr::select(datetime, pulse_id, sensor_position, method, Vh_cm_hr)

          # Step 3: Combine HRM and secondary methods into one dataset
          vh_combined <- dplyr::bind_rows(hrm_data, vh_calibrated_data)

          # Step 4: Use parked sDMA function (vectorized, no loops!)
          incProgress(0.3, detail = "Applying sDMA switching...")

          vh_sdma_result <- sapfluxr::apply_sdma_processing(
            vh_results = vh_combined,
            secondary_method = methods,
            peclet_threshold = input$peclet_threshold,
            skip_low_peclet = FALSE,
            show_progress = FALSE
          )

          # Step 5: Extract only sDMA results and add combination column
          rv$vh_sdma <- vh_sdma_result %>%
            dplyr::filter(grepl("^sDMA:", method)) %>%
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
                '# Apply sDMA switching using parked function
# Sensors: %s
# Methods: %s
# Peclet threshold: %.2f

# Combine HRM with recalculated Peclet and calibrated secondary methods
vh_combined <- dplyr::bind_rows(
  hrm_data,
  vh_calibrated_data
)

# Apply sDMA switching (vectorized, no loops!)
vh_sdma_result <- sapfluxr::apply_sdma_processing(
  vh_results = vh_combined,
  secondary_method = c(%s),
  peclet_threshold = %.2f,
  skip_low_peclet = FALSE,
  show_progress = TRUE
)

# Extract sDMA results
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
    output$has_peclet_recalc <- reactive({
      !is.null(rv$vh_with_peclet)
    })
    outputOptions(output, "has_peclet_recalc", suspendWhenHidden = FALSE)

    output$has_sdma_result <- reactive({
      !is.null(rv$vh_sdma)
    })
    outputOptions(output, "has_sdma_result", suspendWhenHidden = FALSE)

    # Peclet summary
    output$peclet_summary <- renderText({
      req(rv$vh_with_peclet)

      data <- rv$vh_with_peclet
      pe_range <- range(data$Pe_corrected, na.rm = TRUE)
      n_points <- sum(!is.na(data$Pe_corrected))

      paste0(
        "Recalculated: ", n_points, " points\n",
        "Pe range: [", round(pe_range[1], 3), ", ", round(pe_range[2], 3), "]"
      )
    })

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

      if (n_combinations == 1) {
        # Single combination - simple bar chart
        usage_summary <- data %>%
          dplyr::group_by(sdma_source) %>%
          dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
          dplyr::mutate(
            percentage = 100 * count / sum(count),
            label = sprintf("%s\n%d (%.1f%%)", sdma_source, count, percentage)
          )

        ggplot2::ggplot(usage_summary, ggplot2::aes(x = sdma_source, y = count, fill = sdma_source)) +
          ggplot2::geom_col() +
          ggplot2::geom_text(ggplot2::aes(label = label), vjust = -0.5, size = 4) +
          ggplot2::labs(
            title = "Method Selection Frequency",
            subtitle = paste("sDMA switching:", unique(data$combination)),
            x = "Method Used",
            y = "Number of Measurements"
          ) +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          ggplot2::theme_classic() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 14),
            plot.subtitle = ggplot2::element_text(size = 11),
            legend.position = "none"
          )
      } else {
        # Multiple combinations - grouped bar chart
        usage_summary <- data %>%
          dplyr::group_by(combination, sdma_source) %>%
          dplyr::summarise(count = dplyr::n(), .groups = "drop")

        ggplot2::ggplot(usage_summary, ggplot2::aes(x = combination, y = count, fill = sdma_source)) +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::labs(
            title = "Method Selection Frequency by Combination",
            subtitle = paste(n_combinations, "sensor/method combinations"),
            x = "Sensor/Method Combination",
            y = "Number of Measurements",
            fill = "Method Used"
          ) +
          ggplot2::scale_fill_brewer(palette = "Set2") +
          ggplot2::theme_classic() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 14),
            plot.subtitle = ggplot2::element_text(size = 11),
            legend.position = "bottom",
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
          )
      }
    })

    # Peclet scatter plot
    output$peclet_scatter_plot <- renderPlot({
      req(rv$vh_sdma)

      data <- rv$vh_sdma %>%
        dplyr::filter(!is.na(hrm_peclet_number), !is.na(Vh_sdma))

      if (nrow(data) == 0) {
        return(NULL)
      }

      n_combinations <- length(unique(data$combination))

      p <- ggplot2::ggplot(data, ggplot2::aes(x = hrm_peclet_number, y = Vh_sdma, color = sdma_source)) +
        ggplot2::geom_point(alpha = 0.5, size = 2) +
        ggplot2::geom_vline(xintercept = input$peclet_threshold, linetype = "dashed",
                           color = "red", linewidth = 1) +
        ggplot2::annotate("text", x = input$peclet_threshold, y = max(data$Vh_sdma, na.rm = TRUE),
                         label = sprintf("Pe = %.1f", input$peclet_threshold),
                         hjust = -0.1, vjust = 1, color = "red", size = 4) +
        ggplot2::labs(
          title = "sDMA Switching Behaviour",
          subtitle = if (n_combinations == 1) {
            paste("Peclet threshold =", input$peclet_threshold, "-", unique(data$combination))
          } else {
            paste("Peclet threshold =", input$peclet_threshold, "-", n_combinations, "combinations")
          },
          x = "Peclet Number (Corrected)",
          y = "Velocity (cm/hr)",
          color = "Method Used"
        ) +
        ggplot2::scale_color_brewer(palette = "Set1") +
        ggplot2::theme_classic() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14),
          plot.subtitle = ggplot2::element_text(size = 11),
          legend.position = "bottom"
        )

      # Add faceting if multiple combinations
      if (n_combinations > 1) {
        p <- p + ggplot2::facet_wrap(~combination, ncol = 2)
      }

      p
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
                   plotly::layout(title = "No data available for plotting"))
        }

        # Downsample if too many points
        if (nrow(combined_data) > 10000) {
          combined_data <- combined_data[seq(1, nrow(combined_data), length.out = 10000), ]
        }

        # Color palette
        method_colors <- c(
          "HRM (Corrected)" = "#1f77b4",
          "MHR (Calibrated)" = "#ff7f0e",
          "HRMXa (Calibrated)" = "#2ca02c",
          "HRMXb (Calibrated)" = "#d62728",
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

        p <- p %>%
          plotly::layout(
            title = "sDMA Comparison: HRM Baseline, Calibrated Methods, and sDMA Results",
            xaxis = list(title = "Datetime", showgrid = TRUE, gridcolor = "lightgray"),
            yaxis = list(title = "Velocity (cm/hr)", showgrid = TRUE, gridcolor = "lightgray"),
            hovermode = "closest",
            legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left"),
            margin = list(l = 70, r = 200, t = 60, b = 60)
          )

        return(p)

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(
            title = paste("Error creating plot:", e$message),
            xaxis = list(title = "Datetime"),
            yaxis = list(title = "Velocity (cm/hr)")
          )
      })
    })

    # Data table
    output$sdma_table <- DT::renderDT({
      req(rv$vh_sdma)

      data <- rv$vh_sdma %>%
        dplyr::select(datetime, pulse_id, combination, sensor_position,
                     hrm_peclet_number, Vh_sdma, sdma_source, sdma_trigger) %>%
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
        DT::formatRound(columns = c("hrm_peclet_number", "Vh_sdma"), digits = 3)
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
