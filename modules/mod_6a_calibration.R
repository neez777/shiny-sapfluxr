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

          checkboxInput(
            ns("enhanced_regression"),
            "Enhanced regression (auto-detect non-linearity)",
            value = FALSE
          ),

          helpText(
            icon("info-circle"),
            " Primary method is HRM. All secondary methods will be calibrated against HRM.",
            br(),
            tags$small(em("Enhanced regression tests whether a quadratic model better describes the post-breakpoint relationship and adds a third plot when detected."))
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

          uiOutput(ns("plot_help_text")),

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

    # Reset cached results when enhanced_regression toggle changes so recalculation runs
    observeEvent(input$enhanced_regression, {
      rv$r2_optimization_results <- list()
      calculated_sensors(character(0))
      calculation_status("idle")
    }, ignoreInit = TRUE)

    # Background calculation - triggers when vh_corrected data changes OR when user navigates to calibration tab
    observe({
      req(vh_corrected())

      # CRITICAL: Check if this is wound-corrected data or just spacing-corrected
      data_cols <- names(vh_corrected())
      has_wound_correction <- "Vh_cm_hr_wc" %in% data_cols

      # If wound correction hasn't been applied yet, wait for it UNLESS skipping
      if (!has_wound_correction) {
        if (!is.null(wound_module) && !is.null(active_tab)) {
          if (active_tab() != "calibration") {
            return()
          }
        }
      }

      # Skip if already started calculating
      if (calculation_status() != "idle") {
        return()
      }

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
            use_enhanced <- isTRUE(isolate(input$enhanced_regression))
            
            # Determine correct velocity column (Vs_cm_hr if available, else Vh_cm_hr)
            vh_col <- if ("Vs_cm_hr" %in% names(vh_corrected())) "Vs_cm_hr" else "Vh_cm_hr"
            
            result <- if (use_enhanced) {
              sapfluxr::compare_methods_enhanced(
                vh_corrected = vh_corrected(),
                primary_method = "HRM",
                secondary_method = method,
                sensor_position = sensor,
                try_quadratic = TRUE,
                velocity_col = vh_col,
                verbose = FALSE
              )
            } else {
              sapfluxr::compare_methods_segmented(
                vh_corrected = vh_corrected(),
                primary_method = "HRM",
                secondary_method = method,
                sensor_position = sensor,
                create_plots = TRUE,
                velocity_col = vh_col,
                verbose = FALSE
              )
            }

            # Store immediately for incremental rendering
            rv$r2_optimization_results[[sensor]][[method]] <- result

            quad_tag <- if (!is.null(result$recommended_model) && result$recommended_model == "quadratic") " [quadratic]" else ""
            method_elapsed <- as.numeric(difftime(Sys.time(), method_start, units = "secs"))
            cat(sprintf("✓ %.1fs (BP: %.1f cm/hr, R²: %.3f%s)\n",
                        method_elapsed, result$breakpoint, result$r_squared, quad_tag))

          }, error = function(e) {
            cat(sprintf("✗ Failed: %s\n", e$message))
            showNotification(paste("Calibration calculation failed for", method, ":", e$message), type = "error", duration = 15)
            # Assign a dummy error result so UI doesn't crash
            rv$r2_optimization_results[[sensor]][[method]] <- list(
              error = e$message,
              plots = list(segmented_plot = NULL, residuals_plot = NULL)
            )
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
    # RESTORED: Detailed Threshold Settings UI
    # =========================================================================
    output$threshold_controls <- renderUI({
      req(rv$r2_optimization_results)
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
    # Help text
    # =========================================================================
    output$plot_help_text <- renderUI({
      if (isTRUE(input$enhanced_regression)) {
        helpText(
          icon("chart-line"),
          strong(" Review these plots to understand calibration quality and threshold selection."),
          br(),
          "LEFT: Segmented regression  |  CENTRE: Residuals  |  RIGHT: Quadratic model (shown only when non-linearity is detected)"
        )
      } else {
        helpText(
          icon("chart-line"),
          strong(" Review these plots to understand calibration quality and threshold selection."),
          br(),
          "LEFT: Segmented regression - shows breakpoint where methods diverge",
          br(),
          "RIGHT: Residuals plot - shows pattern and fit quality"
        )
      }
    })

    # =========================================================================
    # RESTORED: Three-column layout for diagnostic plots
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
        r2_data <- sensor_results[[method]]
        has_quadratic <- !is.null(r2_data$recommended_model) &&
                         r2_data$recommended_model == "quadratic" &&
                         !is.null(r2_data$plots$quadratic_plot)

        tagList(
          h4(sprintf("%s vs HRM - %s sensor", method, sensor), style = "margin-top: 20px;"),

          if (has_quadratic) {
            # Three-column layout: segmented | residuals | quadratic
            fluidRow(
              column(
                width = 4,
                shinycssloaders::withSpinner(
                  plotOutput(ns(paste0("r2_plot_", sensor, "_", method)), height = "320px"),
                  type = 6, color = "#3c8dbc"
                )
              ),
              column(
                width = 4,
                shinycssloaders::withSpinner(
                  plotOutput(ns(paste0("residuals_plot_", sensor, "_", method)), height = "320px"),
                  type = 6, color = "#3c8dbc"
                )
              ),
              column(
                width = 4,
                div(
                  class = "alert alert-info",
                  style = "padding: 6px 10px; margin-bottom: 8px;",
                  icon("star"),
                  strong(" Quadratic model recommended"),
                  br(),
                  tags$small(sprintf(
                    "R² improvement: +%.4f",
                    r2_data$quadratic_model$r_squared - r2_data$r_squared
                  ))
                ),
                shinycssloaders::withSpinner(
                  plotOutput(ns(paste0("quadratic_plot_", sensor, "_", method)), height = "280px"),
                  type = 6, color = "#3c8dbc"
                )
              )
            )
          } else {
            # Standard two-column layout
            fluidRow(
              column(
                width = 6,
                shinycssloaders::withSpinner(
                  plotOutput(ns(paste0("r2_plot_", sensor, "_", method)), height = "350px"),
                  type = 6, color = "#3c8dbc"
                )
              ),
              column(
                width = 6,
                shinycssloaders::withSpinner(
                  plotOutput(ns(paste0("residuals_plot_", sensor, "_", method)), height = "350px"),
                  type = 6, color = "#3c8dbc"
                )
              )
            )
          },
          hr()
        )
      })

      do.call(tagList, plots)
    })

    # Generate plot outputs dynamically
    observe({
      req(rv$r2_optimization_results)

      # Iterate through all sensor positions that have results
      lapply(names(rv$r2_optimization_results), function(sensor) {
        sensor_results <- rv$r2_optimization_results[[sensor]]

        # Iterate through all methods for this sensor
        lapply(names(sensor_results), function(method) {
          r2_data <- sensor_results[[method]]

          # Segmented regression plot - reactive to threshold changes
          local({
            sensor_local <- sensor
            method_local <- method
            r2_data_local <- r2_data

            output[[paste0("r2_plot_", sensor_local, "_", method_local)]] <- renderPlot({
              # Get current threshold mode
              mode_input <- input[[paste0("threshold_mode_", sensor_local, "_", method_local)]]
              if (is.null(mode_input)) mode_input <- "auto"

              # AUTO MODE: Use original plot from package
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

              # MANUAL MODE: Recalculate visuals with fixed breakpoint
              # CONSISTENT AXES: X=Primary (HRM), Y=Secondary (MHR)
              
              # 1. Get manual threshold value
              manual_threshold <- input[[paste0("threshold_value_", sensor_local, "_", method_local)]]
              if (is.null(manual_threshold)) return(NULL)

              # 2. Get merged data and FLATTEN
              merged_data <- r2_data_local$merged_data
              if (is.null(merged_data) || nrow(merged_data) < 50) return(NULL)
              
              df <- as.data.frame(merged_data)
              # Positional Standard: 1=pulse_id, 2=Primary, 3=Secondary
              if (ncol(df) >= 3) names(df)[1:3] <- c("id", "primary", "secondary")
              df$primary <- as.numeric(unlist(df$primary))
              df$secondary <- as.numeric(unlist(df$secondary))

              # 3. Fit manual segments for VISUAL ONLY
              # X=Primary, Y=Secondary (Matches package segmented_plot)
              fitted <- rep(NA_real_, nrow(df))
              
              idx1 <- df$primary <= manual_threshold
              if(sum(idx1) >= 5) {
                 lm1 <- lm(secondary ~ primary, data=df[idx1,])
                 fitted[idx1] <- predict(lm1, newdata=df[idx1,])
              }
              idx2 <- df$primary > manual_threshold
              if(sum(idx2) >= 5) {
                 lm2 <- lm(secondary ~ primary, data=df[idx2,])
                 fitted[idx2] <- predict(lm2, newdata=df[idx2,])
              }
              
              # Calculate R2 for the manual visual fit
              ss_res <- sum((df$secondary - fitted)^2, na.rm = TRUE)
              ss_tot <- sum((df$secondary - mean(df$secondary, na.rm = TRUE))^2, na.rm = TRUE)
              r2_manual <- 1 - (ss_res / ss_tot)
              
              ggplot2::ggplot(df, ggplot2::aes(x=primary, y=secondary)) + 
                ggplot2::geom_point(alpha=0.3, color="grey40") + 
                ggplot2::geom_line(ggplot2::aes(y=fitted), color="red", linewidth = 1.2, na.rm = TRUE) +
                ggplot2::geom_vline(xintercept=manual_threshold, color="darkgreen", linetype="dashed", linewidth = 1) +
                ggplot2::geom_abline(slope=1, intercept=0, linetype="dashed", color="blue", alpha=0.5) +
                ggplot2::annotate("text", x = manual_threshold, y = max(df$secondary, na.rm = TRUE),
                        label = sprintf("Manual: %.1f cm/hr", manual_threshold),
                        hjust = -0.1, vjust = 1, color = "darkgreen", fontface = "bold") +
                ggplot2::annotate("text", x = Inf, y = -Inf,
                        label = sprintf("R² = %.3f", r2_manual),
                        hjust = 1.1, vjust = -0.3, size = 4) +
                ggplot2::labs(title=paste("Manual Calibration:", method_local, "vs HRM"),
                     subtitle="Consistency check: Primary (HRM) on X-axis",
                     x="HRM Velocity (cm/hr)", y=paste(method_local, "Velocity (cm/hr)")) +
                ggplot2::theme_classic()
            })

            # Residuals plot - reactive to threshold changes
            output[[paste0("residuals_plot_", sensor_local, "_", method_local)]] <- renderPlot({
              mode <- input[[paste0("threshold_mode_", sensor_local, "_", method_local)]]
              if (is.null(mode)) mode <- "auto"

              # AUTO MODE
              if (mode == "auto") {
                if (is.null(r2_data_local$plots$residuals_plot)) {
                  plot.new()
                  text(0.5, 0.5, "Residuals plot not available", cex = 1.2)
                  return(invisible(NULL))
                }
                return(r2_data_local$plots$residuals_plot)
              }

              # MANUAL MODE
              manual_threshold <- input[[paste0("threshold_value_", sensor_local, "_", method_local)]]
              if (is.null(manual_threshold)) return(NULL)

              merged_data <- r2_data_local$merged_data
              if (is.null(merged_data)) return(NULL)
              
              df <- as.data.frame(merged_data)
              if (ncol(df) >= 3) names(df)[1:3] <- c("id", "primary", "secondary")
              df$primary <- as.numeric(unlist(df$primary))
              df$secondary <- as.numeric(unlist(df$secondary))

              fitted <- rep(NA_real_, nrow(df))
              idx1 <- df$primary <= manual_threshold
              if(sum(idx1) >= 5) fitted[idx1] <- predict(lm(secondary ~ primary, data=df[idx1,]), newdata=df[idx1,])
              idx2 <- df$primary > manual_threshold
              if(sum(idx2) >= 5) fitted[idx2] <- predict(lm(secondary ~ primary, data=df[idx2,]), newdata=df[idx2,])
              
              df$res <- df$secondary - fitted
              
              ggplot2::ggplot(df[!is.na(df$res),], ggplot2::aes(x=primary, y=res)) + 
                ggplot2::geom_point(alpha=0.3, color="grey40") + 
                ggplot2::geom_hline(yintercept=0, color="red", linetype="dashed") +
                ggplot2::geom_vline(xintercept=manual_threshold, color="darkgreen", linetype="dashed", alpha=0.5) +
                ggplot2::labs(title="Manual Residuals", x="HRM Velocity (cm/hr)", y="Residuals (cm/hr)") + 
                ggplot2::theme_classic()
            })

            # Quadratic plot
            if (!is.null(r2_data_local$plots$quadratic_plot)) {
              output[[paste0("quadratic_plot_", sensor_local, "_", method_local)]] <- renderPlot({
                r2_data_local$plots$quadratic_plot
              })
            }
          })
        })
      })
    })

    # Apply Calibration
    observeEvent(input$apply_calibration, {
      req(vh_corrected(), rv$r2_optimization_results)
      all_calibrations <- list()
      
      # Show progress
      withProgress(message = "Applying Calibration", value = 0, {
        sensor_positions <- c("outer", "inner")
        total_steps <- length(sensor_positions)
        
        for (idx in seq_along(sensor_positions)) {
          sensor <- sensor_positions[idx]
          incProgress(1/total_steps, detail = paste("Processing", sensor, "sensor"))
          
          methods <- names(rv$r2_optimization_results[[sensor]])
          if (is.null(methods)) next
          
          for (method in methods) {
            # Check if this method already has an error from the optimization phase
            if (!is.null(rv$r2_optimization_results[[sensor]][[method]]$error)) {
              cat(sprintf("Skipping calibration for %s (%s) due to previous error\n", method, sensor))
              next
            }
            
            mode <- input[[paste0("threshold_mode_", sensor, "_", method)]]
            if (is.null(mode)) mode <- "auto"
            
            val <- if (mode == "manual") {
              input[[paste0("threshold_value_", sensor, "_", method)]]
            } else {
              res <- rv$r2_optimization_results[[sensor]][[method]]
              if (!is.null(res$breakpoint) && !is.na(res$breakpoint)) res$breakpoint else 10
            }
            
            if (!is.null(val) && !is.na(val)) {
              tryCatch({
                vh_col <- if ("Vs_cm_hr" %in% names(vh_corrected())) "Vs_cm_hr" else "Vh_cm_hr"
                
                calib_obj <- sapfluxr::calibrate_method_to_primary(
                  vh_corrected = vh_corrected(),
                  primary_method = "HRM",
                  secondary_method = method,
                  sensor_position = sensor,
                  threshold_velocity = val,
                  velocity_col = vh_col,
                  verbose = FALSE
                )
                
                all_calibrations[[paste0(sensor, "_", method)]] <- list(
                  optimal_calibration = calib_obj,
                  optimal_threshold = val
                )
              }, error = function(e) {
                cat(sprintf("✗ Calibration failed for %s (%s): %s\n", method, sensor, e$message))
                showNotification(
                  paste("Calibration failed for", method, "(", sensor, "):", e$message),
                  type = "warning",
                  duration = 10
                )
              })
            }
          }
        }
      })
      
      if (length(all_calibrations) == 0) {
        showNotification("No methods were successfully calibrated. Check thresholds and data availability.", type = "error")
        return()
      }
      
      tryCatch({
        # Final transformation using the current best estimate column
        vh_col <- if ("Vs_cm_hr" %in% names(vh_corrected())) "Vs_cm_hr" else "Vh_cm_hr"
        
        vh_transformed <- sapfluxr::transform_multiple_methods(
          vh_corrected = vh_corrected(),
          calibrations = all_calibrations,
          velocity_col = vh_col,
          verbose = FALSE
        )
        
        rv$vh_calibrated <- vh_transformed
        rv$vh_transformed <- vh_transformed
        
        rv$method_thresholds <- list(
          outer = sapply(names(all_calibrations)[grepl("^outer_", names(all_calibrations))], function(n) all_calibrations[[n]]$optimal_threshold, simplify=FALSE),
          inner = sapply(names(all_calibrations)[grepl("^inner_", names(all_calibrations))], function(n) all_calibrations[[n]]$optimal_threshold, simplify=FALSE)
        )
        
        showNotification("Calibration applied successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error applying calibration:", e$message), type = "error", duration = 10)
      })
    })

    # Flag for calibration result
    output$has_calibration_result <- reactive({ !is.null(rv$vh_calibrated) })
    outputOptions(output, "has_calibration_result", suspendWhenHidden = FALSE)

    # Calibration status
    output$calibration_status <- renderText({
      req(rv$vh_calibrated, rv$method_thresholds)
      "Calibration Applied. Review Tab 6b (Validation) for results."
    })

    return(list(
      vh_calibrated = reactive(rv$vh_calibrated),
      vh_transformed = reactive(rv$vh_transformed),
      primary_method = reactive("HRM")
    ))
  })
}
