# mod_6a_calibration.R
# Module for Method Calibration
#
# Implements linear regression calibration of secondary methods against a primary method.
# Part 1 of the Calibration & sDMA workflow.
#
# NEW WORKFLOW:
# 1. R² optimisation calculated after wound correction (or on-demand)
# 2. User reviews diagnostic plots
# 3. User sets thresholds (auto-detect or manual per method)
# 4. User applies calibration

# UI ----
calibrationUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Configuration and Threshold Settings
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "About Calibration",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p("Review the diagnostic plots on the right to evaluate calibration quality for each method."),
          p("The segmented regression plot shows the breakpoint where methods diverge."),
          p("You can use auto-detected thresholds or set manual values per method."),
          p(tags$small(em("Recommended: Apply AFTER spacing and wound corrections")))
        ),  # Close Info box

        # Heartwood warning (dynamic - shown only if inner sensor is in heartwood)
        uiOutput(ns("heartwood_warning")),

        # Sensor position selection
        box(
          width = 12,
          title = "Configuration",
          status = "primary",
          solidHeader = TRUE,

          selectInput(
            ns("sensor_position"),
            "Sensor Position:",
            choices = c("Outer" = "outer", "Inner" = "inner"),
            selected = "outer"
          ),

          helpText(
            icon("info-circle"),
            " Primary method is HRM. All secondary methods will be calibrated against HRM."
          )
        ),  # Close Configuration box

        # Per-method threshold settings (dynamic)
        box(
          width = 12,
          title = "Threshold Settings",
          status = "warning",
          solidHeader = TRUE,

          helpText(
            "Review the segmented regression plots, then set thresholds for each method below.",
            "The auto-detected value is the statistically identified breakpoint."
          ),

          uiOutput(ns("threshold_controls"))
        ),  # Close Threshold Settings box

        # Apply calibration button
        box(
          width = 12,
          title = "Apply Calibration",
          status = "success",
          solidHeader = TRUE,

          actionButton(
            ns("apply_calibration"),
            "Apply Calibration with Selected Thresholds",
            icon = icon("check"),
            class = "btn-success btn-block"
          ),

          hr(),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_calibration_result")),
            p(strong("Calibration Applied"), style = "color: green;"),
            verbatimTextOutput(ns("calibration_status"))
          )
        )
      ),

      # Right column: Diagnostic Plots
      column(
        width = 8,

        box(
          width = 12,
          title = "Calibration Diagnostics - Review Before Applying",
          status = "primary",
          solidHeader = TRUE,

          helpText(
            icon("chart-line"),
            strong(" Review these plots to understand calibration quality and threshold selection."),
            br(),
            "LEFT: Segmented regression - shows breakpoint where methods diverge",
            br(),
            "RIGHT: Residuals plot - shows pattern and fit quality"
          ),

          # Dynamic diagnostic plots
          uiOutput(ns("diagnostic_plots"))
        )
      )
    )
  )
}

# Server ----
calibrationServer <- function(id, vh_corrected, wood_properties, probe_config = reactive(NULL), code_tracker = NULL, active_tab = NULL, wound_module = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      r2_optimization_results = list(),  # rv$r2_optimization_results[[sensor_position]][[method]]
      vh_calibrated = NULL,              # rv$vh_calibrated - unified dataset (both sensors)
      method_thresholds = list()         # rv$method_thresholds$outer[[method]], rv$method_thresholds$inner[[method]]
    )

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

    # =========================================================================
    # PROACTIVE BACKGROUND CALCULATION
    # Triggers AFTER wound correction plot is generated
    # This ensures wound-corrected data is used for calibration
    # =========================================================================

    # Track calculation status
    calculation_status <- reactiveVal("idle")
    calculated_sensors <- reactiveVal(character(0))

    # Background calculation - triggers when vh_corrected data changes OR when user navigates to calibration tab
    # This happens AFTER wound correction is applied (if used)
    # or AFTER spacing correction (if wound correction skipped)
    observe({
      cat("\n[DEBUG] Background calc observe() triggered\n")
      req(vh_corrected())
      cat("[DEBUG] vh_corrected() is available\n")

      # CRITICAL: Check if this is wound-corrected data or just spacing-corrected
      # Wound-corrected data has the "Vh_cm_hr_wc" column
      data_cols <- names(vh_corrected())
      has_wound_correction <- "Vh_cm_hr_wc" %in% data_cols
      cat(sprintf("[DEBUG] has_wound_correction: %s\n", has_wound_correction))

      # If wound correction hasn't been applied yet, wait for it UNLESS:
      # 1. User navigates to calibration tab (they're skipping wound correction)
      # 2. No wound module exists
      if (!has_wound_correction) {
        # Check if wound module exists AND user hasn't navigated to calibration yet
        if (!is.null(wound_module) && !is.null(active_tab)) {
          # If user is on calibration tab, proceed anyway (they're skipping wound correction)
          if (active_tab() != "calibration") {
            cat("Waiting for wound correction before starting calibration calculations...\n")
            cat("(Navigate to Method Calibration tab to skip wound correction)\n")
            return()
          } else {
            cat("User navigated to calibration - proceeding with spacing-corrected data\n")
          }
        }
      }

      # Skip if already started calculating
      if (calculation_status() != "idle") {
        return()
      }

      cat("Starting background calibration with",
          ifelse(has_wound_correction, "wound-corrected", "spacing-corrected"),
          "data\n")
      # Detect available methods
      available_methods <- unique(vh_corrected()$method)
      secondary_methods <- setdiff(available_methods, "HRM")

      if (length(secondary_methods) == 0) {
        return()
      }

      # Calculate BOTH sensors proactively
      for (sensor in c("outer", "inner")) {
        # Skip if already calculated
        if (sensor %in% calculated_sensors()) {
          next
        }

        # Check if this sensor has data
        sensor_data <- vh_corrected()[vh_corrected()$sensor_position == sensor, ]
        if (nrow(sensor_data) == 0) {
          next
        }

        calculation_status(paste("Calculating", sensor, "sensor"))

        cat("\n")
        cat(strrep("=", 72), "\n")
        cat("BACKGROUND CALIBRATION:", toupper(sensor), "SENSOR\n")
        cat(strrep("=", 72), "\n")
        cat("Starting at:", format(Sys.time(), "%H:%M:%S"), "\n")
        cat("Methods to analyse:", paste(secondary_methods, collapse = ", "), "\n")
        cat("\n")

        # Initialize results for this sensor
        if (is.null(rv$r2_optimization_results[[sensor]])) {
          rv$r2_optimization_results[[sensor]] <- list()
        }

        # Calculate each method and store immediately
        for (i in seq_along(secondary_methods)) {
          method <- secondary_methods[i]

          cat(sprintf("[%d/%d] %s vs HRM... ", i, length(secondary_methods), method))
          method_start <- Sys.time()

          tryCatch({
            result <- sapfluxr::compare_methods_segmented(
              vh_corrected = vh_corrected(),
              primary_method = "HRM",
              secondary_method = method,
              sensor_position = sensor,
              create_plots = TRUE,
              verbose = FALSE
            )

            # Store immediately for incremental rendering
            rv$r2_optimization_results[[sensor]][[method]] <- result

            method_elapsed <- as.numeric(difftime(Sys.time(), method_start, units = "secs"))
            cat(sprintf("✓ %.1fs (BP: %.1f cm/hr, R²: %.3f)\n",
                        method_elapsed, result$breakpoint, result$r_squared))

          }, error = function(e) {
            cat(sprintf("✗ Failed: %s\n", e$message))
          })
        }

        # Mark this sensor as calculated
        calculated_sensors(c(calculated_sensors(), sensor))

        cat("\n")
        cat("Completed at:", format(Sys.time(), "%H:%M:%S"), "\n")
        cat(sprintf("%s sensor: %d/%d methods successful\n",
                    toupper(sensor),
                    length(rv$r2_optimization_results[[sensor]]),
                    length(secondary_methods)))
        cat(strrep("=", 72), "\n")
        cat("\n")
      }

      calculation_status("complete")
    })

    # =========================================================================
    # Dynamic threshold controls - Grouped by METHOD, showing both sensors
    # =========================================================================
    output$threshold_controls <- renderUI({
      req(rv$r2_optimization_results)

      # Get all methods from outer sensor (should be same for both)
      outer_results <- rv$r2_optimization_results[["outer"]]
      if (is.null(outer_results) || length(outer_results) == 0) {
        return(p(em("No calibration results available")))
      }

      methods <- names(outer_results)

      # Create controls grouped by method
      method_controls <- lapply(methods, function(method) {

        # Create rows for each sensor
        sensor_rows <- lapply(c("outer", "inner"), function(sensor) {
          sensor_results <- rv$r2_optimization_results[[sensor]]
          if (is.null(sensor_results) || is.null(sensor_results[[method]])) {
            return(NULL)
          }

          r2_data <- sensor_results[[method]]
          optimal_threshold <- if (!is.null(r2_data$breakpoint) && !is.na(r2_data$breakpoint)) {
            r2_data$breakpoint
          } else if (!is.null(r2_data$optimal_threshold)) {
            r2_data$optimal_threshold
          } else {
            10
          }

          r2_value <- if (!is.null(r2_data$r_squared)) r2_data$r_squared else NA

          fluidRow(
            style = "margin-bottom: 5px; padding: 3px;",
            column(2, div(style = "padding-top: 5px; font-size: 0.9em;", toupper(sensor))),
            column(3, div(style = "padding-top: 5px; font-size: 0.85em;",
                         sprintf("%.1f cm/hr", optimal_threshold),
                         if (!is.na(r2_value)) sprintf(" (R²=%.2f)", r2_value) else "")),
            column(4, radioButtons(
              ns(paste0("threshold_mode_", sensor, "_", method)),
              NULL,
              choices = c("Auto" = "auto", "Manual" = "manual"),
              selected = "auto",
              inline = TRUE
            )),
            column(3, conditionalPanel(
              condition = sprintf("input['%s'] == 'manual'", ns(paste0("threshold_mode_", sensor, "_", method))),
              numericInput(
                ns(paste0("threshold_value_", sensor, "_", method)),
                NULL,
                value = round(optimal_threshold, 1),
                min = 0,
                max = 20,
                step = 0.5
              )
            ))
          )
        })

        tagList(
          h5(method, style = "margin-top: 15px; margin-bottom: 8px; color: #333; border-bottom: 1px solid #ddd; padding-bottom: 5px;"),
          div(style = "background: #f9f9f9; border-radius: 4px; padding: 8px;",
              do.call(tagList, sensor_rows)
          )
        )
      })

      do.call(tagList, c(
        list(p(style = "font-size: 0.9em; margin-bottom: 10px;",
               "Set thresholds for each method (both sensors shown):")),
        method_controls
      ))
    })

    # =========================================================================
    # Dynamic diagnostic plots (sensor-specific, filtered)
    # =========================================================================
    output$diagnostic_plots <- renderUI({
      req(input$sensor_position)
      sensor <- input$sensor_position

      # Access sensor-specific results
      sensor_results <- rv$r2_optimization_results[[sensor]]
      req(sensor_results)

      methods <- names(sensor_results)
      if (length(methods) == 0) {
        return(p(em("Calculating diagnostics...")))
      }

      plots <- lapply(methods, function(method) {
        tagList(
          h4(sprintf("%s vs HRM - %s sensor", method, sensor), style = "margin-top: 20px;"),

          # Diagnostic warning if U-shaped pattern detected
          conditionalPanel(
            condition = sprintf("output['%s']", ns(paste0("has_pattern_warning_", sensor, "_", method))),
            div(
              class = "alert alert-warning",
              style = "margin-bottom: 15px;",
              icon("exclamation-triangle"),
              strong(" Non-linear pattern detected: "),
              textOutput(ns(paste0("pattern_warning_", sensor, "_", method)), inline = TRUE)
            )
          ),

          # Only show: Segmented Regression and Residuals Plot
          fluidRow(
            column(
              width = 6,
              shinycssloaders::withSpinner(
                plotOutput(ns(paste0("r2_plot_", sensor, "_", method)), height = "350px"),
                type = 6,
                color = "#3c8dbc"
              )
            ),
            column(
              width = 6,
              shinycssloaders::withSpinner(
                plotOutput(ns(paste0("residuals_plot_", sensor, "_", method)), height = "350px"),
                type = 6,
                color = "#3c8dbc"
              )
            )
          ),
          hr()
        )
      })

      do.call(tagList, plots)
    })

    # Generate plot outputs dynamically (sensor-specific, filtered to essential plots)
    observe({
      req(rv$r2_optimization_results)

      # Iterate through all sensor positions that have results
      lapply(names(rv$r2_optimization_results), function(sensor) {
        sensor_results <- rv$r2_optimization_results[[sensor]]

        # Iterate through all methods for this sensor
        lapply(names(sensor_results), function(method) {
          r2_data <- sensor_results[[method]]

          # Pattern warning text
          output[[paste0("pattern_warning_", sensor, "_", method)]] <- renderText({
            if (!is.null(r2_data$residual_diagnostics) &&
                isTRUE(r2_data$residual_diagnostics$pattern_detected)) {
              r2_data$residual_diagnostics$pattern_type
            } else if (!is.null(r2_data$residual_pattern_warning)) {
              r2_data$residual_pattern_warning
            } else {
              ""
            }
          })

          # Flag for conditional panel
          output[[paste0("has_pattern_warning_", sensor, "_", method)]] <- reactive({
            !is.null(r2_data$residual_diagnostics) &&
              isTRUE(r2_data$residual_diagnostics$pattern_detected)
          })
          outputOptions(output, paste0("has_pattern_warning_", sensor, "_", method), suspendWhenHidden = FALSE)

          # Segmented regression plot - reactive to threshold changes with full recalculation
          local({
            sensor_local <- sensor
            method_local <- method
            r2_data_local <- r2_data

            output[[paste0("r2_plot_", sensor_local, "_", method_local)]] <- renderPlot({
              # Get current threshold (auto or manual)
              mode_input <- input[[paste0("threshold_mode_", sensor_local, "_", method_local)]]
              if (is.null(mode_input)) mode_input <- "auto"

              # AUTO MODE: Use original plot
              if (mode_input == "auto") {
                base_plot <- if (!is.null(r2_data_local$plots$segmented_plot)) {
                  r2_data_local$plots$segmented_plot
                } else if (!is.null(r2_data_local$plots$r_squared_plot)) {
                  r2_data_local$plots$r_squared_plot
                } else {
                  NULL
                }

                if (is.null(base_plot)) {
                  plot.new()
                  text(0.5, 0.5, "Segmented regression plot not available", cex = 1.2)
                  return(invisible(NULL))
                }

                return(base_plot)
              }

              # MANUAL MODE: Recalculate with fixed breakpoint
              manual_threshold <- input[[paste0("threshold_value_", sensor_local, "_", method_local)]]
              if (is.null(manual_threshold)) {
                return(NULL)
              }

              # Get merged data
              merged_data <- r2_data_local$merged_data
              if (is.null(merged_data) || nrow(merged_data) < 50) {
                plot.new()
                text(0.5, 0.5, "Insufficient data for manual calibration", cex = 1.2)
                return(invisible(NULL))
              }

              # Fit piecewise linear regression at manual threshold
              # Segment 1: Primary <= manual_threshold
              segment1 <- merged_data[merged_data$Vh_cm_hr_primary <= manual_threshold, ]
              # Segment 2: Primary > manual_threshold
              segment2 <- merged_data[merged_data$Vh_cm_hr_primary > manual_threshold, ]

              # Fit linear models for each segment if enough points
              fitted_values <- rep(NA_real_, nrow(merged_data))

              if (nrow(segment1) >= 10) {
                lm1 <- lm(Vh_cm_hr_secondary ~ Vh_cm_hr_primary, data = segment1)
                slope1 <- coef(lm1)[2]
                intercept1 <- coef(lm1)[1]
                # Predict for segment 1
                idx1 <- merged_data$Vh_cm_hr_primary <= manual_threshold
                fitted_values[idx1] <- predict(lm1, newdata = merged_data[idx1, ])
              } else {
                slope1 <- NA
                intercept1 <- NA
              }

              if (nrow(segment2) >= 10) {
                lm2 <- lm(Vh_cm_hr_secondary ~ Vh_cm_hr_primary, data = segment2)
                slope2 <- coef(lm2)[2]
                intercept2 <- coef(lm2)[1]
                # Predict for segment 2
                idx2 <- merged_data$Vh_cm_hr_primary > manual_threshold
                fitted_values[idx2] <- predict(lm2, newdata = merged_data[idx2, ])
              } else {
                slope2 <- NA
                intercept2 <- NA
              }

              # Calculate R² for piecewise model
              ss_res <- sum((merged_data$Vh_cm_hr_secondary - fitted_values)^2, na.rm = TRUE)
              ss_tot <- sum((merged_data$Vh_cm_hr_secondary - mean(merged_data$Vh_cm_hr_secondary))^2)
              r2_manual <- 1 - (ss_res / ss_tot)

              # Create plot data with fitted values
              plot_data <- merged_data
              plot_data$fitted <- fitted_values

              # Create segmented regression plot
              primary_method <- r2_data_local$primary_method
              secondary_method <- r2_data_local$secondary_method

              p <- ggplot2::ggplot(plot_data,
                                   ggplot2::aes(x = Vh_cm_hr_primary, y = Vh_cm_hr_secondary)) +
                # Data points
                ggplot2::geom_point(alpha = 0.3, color = "grey40", size = 1.5) +

                # Fitted piecewise line
                ggplot2::geom_line(ggplot2::aes(y = fitted), color = "red", linewidth = 1.2, na.rm = TRUE) +

                # 1:1 reference line
                ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                                     color = "blue", alpha = 0.5) +

                # Manual breakpoint line
                ggplot2::geom_vline(xintercept = manual_threshold, color = "darkgreen",
                                    linetype = "dashed", linewidth = 1) +

                # Annotation with statistics
                ggplot2::annotate("text",
                                 x = manual_threshold,
                                 y = max(plot_data$Vh_cm_hr_secondary, na.rm = TRUE),
                                 label = sprintf("Manual: %.1f cm/hr", manual_threshold),
                                 hjust = -0.1, vjust = 1, color = "darkgreen", size = 4, fontface = "bold") +

                ggplot2::annotate("text",
                                 x = Inf, y = -Inf,
                                 label = sprintf(
                                   "R² = %.3f\nSlope before = %.3f\nSlope after = %.3f",
                                   r2_manual,
                                   ifelse(is.na(slope1), NA, slope1),
                                   ifelse(is.na(slope2), NA, slope2)
                                 ),
                                 hjust = 1.1, vjust = -0.3, size = 3.5, color = "black") +

                ggplot2::labs(
                  title = paste("Method Comparison:", secondary_method, "vs", primary_method),
                  subtitle = sprintf("Manual Breakpoint at %.1f cm/hr (R² = %.3f)",
                                    manual_threshold, r2_manual),
                  x = paste(primary_method, "Velocity (cm/hr)"),
                  y = paste(secondary_method, "Velocity (cm/hr)")
                ) +

                ggplot2::theme_classic() +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(face = "bold", size = 14),
                  plot.subtitle = ggplot2::element_text(size = 11),
                  legend.position = "bottom"
                )

              p
            })
          })

          # Residuals plot - reactive to threshold changes
          local({
            sensor_local <- sensor
            method_local <- method
            r2_data_local <- r2_data

            output[[paste0("residuals_plot_", sensor_local, "_", method_local)]] <- renderPlot({
              # Get current threshold mode
              mode_input <- input[[paste0("threshold_mode_", sensor_local, "_", method_local)]]
              if (is.null(mode_input)) mode_input <- "auto"

              # AUTO MODE: Use original residuals plot
              if (mode_input == "auto") {
                if (!is.null(r2_data_local$plots$residuals_plot)) {
                  return(r2_data_local$plots$residuals_plot)
                } else {
                  plot.new()
                  text(0.5, 0.5, "Residuals plot not available", cex = 1.2)
                  return(invisible(NULL))
                }
              }

              # MANUAL MODE: Recalculate residuals for manual threshold
              manual_threshold <- input[[paste0("threshold_value_", sensor_local, "_", method_local)]]
              if (is.null(manual_threshold)) {
                return(NULL)
              }

              # Get merged data
              merged_data <- r2_data_local$merged_data
              if (is.null(merged_data) || nrow(merged_data) < 50) {
                plot.new()
                text(0.5, 0.5, "Insufficient data for residuals plot", cex = 1.2)
                return(invisible(NULL))
              }

              # Fit piecewise linear regression at manual threshold (same as main plot)
              segment1 <- merged_data[merged_data$Vh_cm_hr_primary <= manual_threshold, ]
              segment2 <- merged_data[merged_data$Vh_cm_hr_primary > manual_threshold, ]

              # Fit linear models and calculate residuals
              fitted_values <- rep(NA_real_, nrow(merged_data))

              if (nrow(segment1) >= 10) {
                lm1 <- lm(Vh_cm_hr_secondary ~ Vh_cm_hr_primary, data = segment1)
                idx1 <- merged_data$Vh_cm_hr_primary <= manual_threshold
                fitted_values[idx1] <- predict(lm1, newdata = merged_data[idx1, ])
              }

              if (nrow(segment2) >= 10) {
                lm2 <- lm(Vh_cm_hr_secondary ~ Vh_cm_hr_primary, data = segment2)
                idx2 <- merged_data$Vh_cm_hr_primary > manual_threshold
                fitted_values[idx2] <- predict(lm2, newdata = merged_data[idx2, ])
              }

              # Calculate residuals
              residuals <- merged_data$Vh_cm_hr_secondary - fitted_values

              # Create residuals plot data
              plot_data <- data.frame(
                primary = merged_data$Vh_cm_hr_primary,
                residuals = residuals,
                fitted = fitted_values
              )

              # Remove NA values
              plot_data <- plot_data[!is.na(plot_data$residuals), ]

              # Create residuals plot
              primary_method <- r2_data_local$primary_method
              secondary_method <- r2_data_local$secondary_method

              p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = primary, y = residuals)) +
                ggplot2::geom_point(alpha = 0.3, color = "grey40", size = 1.5) +
                ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
                ggplot2::geom_vline(xintercept = manual_threshold, color = "darkgreen",
                                    linetype = "dashed", linewidth = 1) +
                ggplot2::geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.2) +
                ggplot2::annotate("text",
                                 x = manual_threshold,
                                 y = max(plot_data$residuals, na.rm = TRUE),
                                 label = sprintf("Manual: %.1f cm/hr", manual_threshold),
                                 hjust = -0.1, vjust = 1, color = "darkgreen", size = 4, fontface = "bold") +
                ggplot2::labs(
                  title = "Residuals Plot",
                  subtitle = sprintf("Manual Breakpoint at %.1f cm/hr", manual_threshold),
                  x = paste(primary_method, "Velocity (cm/hr)"),
                  y = "Residuals (cm/hr)"
                ) +
                ggplot2::theme_classic() +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(face = "bold", size = 14),
                  plot.subtitle = ggplot2::element_text(size = 11),
                  legend.position = "bottom"
                )

              p
            })
          })
        })
      })
    })

    # =========================================================================
    # NOTE: Live threshold updates removed - caused UI blocking
    # Manual thresholds are applied when user clicks "Apply Calibration"
    # =========================================================================

    # =========================================================================
    # Apply calibration - BOTH SENSORS with unified dataset
    # =========================================================================
    observeEvent(input$apply_calibration, {
      req(vh_corrected())
      req(rv$r2_optimization_results)

      # Collect thresholds for BOTH sensors
      thresholds_outer <- list()
      thresholds_inner <- list()

      withProgress(message = "Collecting thresholds...", value = 0, {

        # Process OUTER sensor thresholds
        if (!is.null(rv$r2_optimization_results[["outer"]])) {
          outer_methods <- names(rv$r2_optimization_results[["outer"]])

          for (method in outer_methods) {
            mode <- input[[paste0("threshold_mode_outer_", method)]]
            if (is.null(mode)) mode <- "auto"

            if (mode == "auto") {
              result <- rv$r2_optimization_results[["outer"]][[method]]
              if (!is.null(result$breakpoint) && !is.na(result$breakpoint)) {
                thresholds_outer[[method]] <- result$breakpoint
              } else if (!is.null(result$optimal_threshold)) {
                thresholds_outer[[method]] <- result$optimal_threshold
              }
            } else {
              manual_val <- input[[paste0("threshold_value_outer_", method)]]
              if (!is.null(manual_val) && !is.na(manual_val)) {
                thresholds_outer[[method]] <- manual_val
              }
            }
          }
        }

        # Process INNER sensor thresholds
        if (!is.null(rv$r2_optimization_results[["inner"]])) {
          inner_methods <- names(rv$r2_optimization_results[["inner"]])

          for (method in inner_methods) {
            mode <- input[[paste0("threshold_mode_inner_", method)]]
            if (is.null(mode)) mode <- "auto"

            if (mode == "auto") {
              result <- rv$r2_optimization_results[["inner"]][[method]]
              if (!is.null(result$breakpoint) && !is.na(result$breakpoint)) {
                thresholds_inner[[method]] <- result$breakpoint
              } else if (!is.null(result$optimal_threshold)) {
                thresholds_inner[[method]] <- result$optimal_threshold
              }
            } else {
              manual_val <- input[[paste0("threshold_value_inner_", method)]]
              if (!is.null(manual_val) && !is.na(manual_val)) {
                thresholds_inner[[method]] <- manual_val
              }
            }
          }
        }

        incProgress(0.3, detail = "Applying calibration...")

        tryCatch({
          # STEP 1: Transform methods (for validation plot - keeps long format)
          # Create actual method_calibration objects from thresholds
          all_calibrations <- list()

          # Process OUTER sensor calibrations
          if (length(thresholds_outer) > 0) {
            for (method in names(thresholds_outer)) {
              threshold_val <- thresholds_outer[[method]]

              # Create calibration object using the threshold
              calib_obj <- sapfluxr::calibrate_method_to_primary(
                vh_corrected = vh_corrected(),
                primary_method = "HRM",
                secondary_method = method,
                sensor_position = "outer",
                threshold_velocity = threshold_val,
                verbose = FALSE
              )

              # Store in the format expected by transform_multiple_methods
              all_calibrations[[paste0("outer_", method)]] <- list(
                optimal_calibration = calib_obj,
                optimal_threshold = threshold_val
              )
            }
          }

          # Process INNER sensor calibrations
          if (length(thresholds_inner) > 0) {
            for (method in names(thresholds_inner)) {
              threshold_val <- thresholds_inner[[method]]

              # Create calibration object using the threshold
              calib_obj <- sapfluxr::calibrate_method_to_primary(
                vh_corrected = vh_corrected(),
                primary_method = "HRM",
                secondary_method = method,
                sensor_position = "inner",
                threshold_velocity = threshold_val,
                verbose = FALSE
              )

              # Store in the format expected by transform_multiple_methods
              all_calibrations[[paste0("inner_", method)]] <- list(
                optimal_calibration = calib_obj,
                optimal_threshold = threshold_val
              )
            }
          }

          # Transform velocities (keeps long format for validation and sDMA)
          vh_transformed <- vh_corrected()
          if (length(all_calibrations) > 0) {
            vh_transformed <- sapfluxr::transform_multiple_methods(
              vh_corrected = vh_transformed,
              calibrations = all_calibrations,
              verbose = FALSE
            )
          }

          # Store results
          # For sDMA, we need all methods available in long format
          # So we use vh_transformed for both validation and downstream
          rv$vh_transformed <- vh_transformed  # Long format for validation plots
          rv$vh_calibrated <- vh_transformed   # Long format for sDMA (needs all methods)
          rv$method_thresholds <- list(
            outer = thresholds_outer,
            inner = thresholds_inner
          )

          # Track code
          if (!is.null(code_tracker)) {
            # Format thresholds for both sensors
            threshold_text_outer <- if (length(thresholds_outer) > 0) {
              sapply(names(thresholds_outer), function(m) {
                sprintf("Outer %s: %.1f cm/hr", m, thresholds_outer[[m]])
              })
            } else {
              character(0)
            }

            threshold_text_inner <- if (length(thresholds_inner) > 0) {
              sapply(names(thresholds_inner), function(m) {
                sprintf("Inner %s: %.1f cm/hr", m, thresholds_inner[[m]])
              })
            } else {
              character(0)
            }

            threshold_text_all <- c(threshold_text_outer, threshold_text_inner)

            # Build calibration code for each method
            calib_code_lines <- character(0)

            for (method in names(thresholds_outer)) {
              calib_code_lines <- c(calib_code_lines, sprintf(
                '  outer_%s = sapfluxr::calibrate_method_to_primary(vh_corrected, "HRM", "%s", "outer", %.1f)',
                method, method, thresholds_outer[[method]]
              ))
            }

            for (method in names(thresholds_inner)) {
              calib_code_lines <- c(calib_code_lines, sprintf(
                '  inner_%s = sapfluxr::calibrate_method_to_primary(vh_corrected, "HRM", "%s", "inner", %.1f)',
                method, method, thresholds_inner[[method]]
              ))
            }

            code_tracker$add_step(
              step_name = "Method Calibration",
              code = sprintf(
                '# Create calibration objects for each method
calibrations <- list(
%s
)

# Transform methods using calibrations
vh_calibrated <- sapfluxr::transform_multiple_methods(
  vh_corrected = vh_corrected,
  calibrations = calibrations,
  verbose = FALSE
)',
                paste(calib_code_lines, collapse = ",\n")
              ),
              description = paste0(
                "Transform secondary methods to align with HRM scale\n",
                if (length(threshold_text_all) > 0) {
                  paste("Thresholds:\n", paste(threshold_text_all, collapse = "\n"))
                } else {
                  "No thresholds set"
                }
              )
            )
          }

          incProgress(1.0, detail = "Done!")

          # Count methods calibrated for each sensor
          n_outer <- length(thresholds_outer)
          n_inner <- length(thresholds_inner)
          n_total <- nrow(vh_transformed)

          showNotification(
            paste0(
              "Calibration applied successfully!\n",
              "Outer sensor: ", n_outer, " method(s)\n",
              "Inner sensor: ", n_inner, " method(s)\n",
              "Total calibrated points: ", n_total
            ),
            type = "message",
            duration = 5
          )

        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error", duration = 10)
        })
      })
    })

    # =========================================================================
    # Outputs (sensor-specific)
    # =========================================================================

    # Flag for calibration result
    output$has_calibration_result <- reactive({
      !is.null(rv$vh_calibrated)
    })
    outputOptions(output, "has_calibration_result", suspendWhenHidden = FALSE)

    # Calibration status
    output$calibration_status <- renderText({
      req(rv$vh_calibrated)
      req(rv$method_thresholds)

      # Get thresholds for both sensors
      thresholds_outer <- rv$method_thresholds$outer
      thresholds_inner <- rv$method_thresholds$inner

      # Format outer sensor info
      outer_text <- if (length(thresholds_outer) > 0) {
        threshold_list <- sapply(names(thresholds_outer), function(m) {
          sprintf("  %s: %.1f cm/hr", m, thresholds_outer[[m]])
        })
        paste0(
          "Outer sensor:\n",
          paste(threshold_list, collapse = "\n")
        )
      } else {
        "Outer sensor: No thresholds set"
      }

      # Format inner sensor info
      inner_text <- if (length(thresholds_inner) > 0) {
        threshold_list <- sapply(names(thresholds_inner), function(m) {
          sprintf("  %s: %.1f cm/hr", m, thresholds_inner[[m]])
        })
        paste0(
          "Inner sensor:\n",
          paste(threshold_list, collapse = "\n")
        )
      } else {
        "Inner sensor: No thresholds set"
      }

      # Get calibration summary
      n_points <- nrow(rv$vh_calibrated)
      methods_available <- unique(rv$vh_calibrated$method)

      paste0(
        "Calibration Applied\n",
        strrep("=", 40), "\n",
        "Total calibrated points: ", n_points, "\n",
        "Methods available: ", paste(methods_available, collapse = ", "), "\n\n",
        "Thresholds:\n",
        outer_text, "\n\n",
        inner_text
      )
    })

    # =========================================================================
    # Return values for downstream modules
    # =========================================================================
    return(list(
      vh_calibrated = reactive({
        rv$vh_calibrated
      }),
      vh_transformed = reactive({
        rv$vh_transformed
      }),
      method_thresholds = reactive({
        rv$method_thresholds
      }),
      primary_method = reactive("HRM")
    ))
  })
}
