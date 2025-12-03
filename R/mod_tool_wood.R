#' Wood Properties Tool Module
#'
#' Standalone tool for creating/editing wood properties YAML files
#'
#' @param id Module ID
#' @return None (standalone page)
#'

# UI ----
toolWoodUI <- function(id) {
  ns <- NS(id)

  tagList(
    p(class = "help-text",
      strong("Tip: "),
      "Save your configuration with a descriptive name matching your heat pulse data file for easy recognition ",
      "(e.g., if your data file is ", code("oak_tree_2024.txt"), ", name your config ",
      code("oak_tree_2024_wood.yaml"), ")."),

    fluidRow(
      box(
        width = 4,
        title = "Load Existing Configuration (Optional)",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        p(class = "help-text", "Upload an existing wood properties YAML file to edit."),

        fileInput(
          ns("upload_yaml"),
          label = NULL,
          accept = c(".yaml", ".yml"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),

        uiOutput(ns("upload_status"))
      ),

      box(
        width = 8,
        title = "Configuration Status",
        status = "warning",
        solidHeader = TRUE,

        uiOutput(ns("validation_summary"))
      )
    ),

    fluidRow(
      box(
        width = 12,
        title = "Wood Properties Configuration",
        status = "primary",
        solidHeader = TRUE,

        tabsetPanel(
          id = ns("wood_tabs"),

          # Wood Measurements Tab
          tabPanel(
            "Wood Measurements",
            br(),

            # Calculate button (visible on all tabs)
            div(
              style = "text-align: center; margin-bottom: 20px;",
              actionButton(ns("calculate_wood_props"),
                          "Calculate Derived Properties",
                          icon = icon("calculator"),
                          class = "btn-success btn-lg")
            ),

            # Method selector
            radioButtons(
              ns("wood_input_method"),
              "Input Method:",
              choices = c(
                "Method 1: Weight & Volume" = "method1",
                "Method 2: Dual Density (RECOMMENDED)" = "method2"
              ),
              selected = "method1"
            ),

            # Help text
            uiOutput(ns("method_help_text")),

            # Method 1 inputs
            conditionalPanel(
              condition = sprintf("input['%s'] == 'method1'", ns("wood_input_method")),

              p(class = "help-text",
                "Measure fresh weight immediately after sampling, then oven-dry at 105°C."),

              numericInput(ns("fresh_weight_g"),
                          "Fresh Weight (g):",
                          value = NULL, min = 0.01, max = 100, step = 0.0001),

              numericInput(ns("dry_weight_g"),
                          "Dry Weight (g):",
                          value = NULL, min = 0.01, max = 100, step = 0.0001),

              numericInput(ns("fresh_volume_cm3"),
                          "Fresh Volume (cm³):",
                          value = NULL, min = 0.01, max = 100, step = 0.0001)
            ),

            # Method 2 inputs
            conditionalPanel(
              condition = sprintf("input['%s'] == 'method2'", ns("wood_input_method")),

              h5("Dual Density Measurements (RECOMMENDED):"),
              p(class = "help-text",
                "Measure BOTH densities on the same sample. Volume cancels out!"),

              numericInput(ns("density_dry_kg_m3"),
                          "Dry Wood Density (kg/m³):",
                          value = NULL, min = 200, max = 1000, step = 10),

              numericInput(ns("density_fresh_kg_m3"),
                          "Fresh Wood Density (kg/m³):",
                          value = NULL, min = 400, max = 1400, step = 10)
            )
          ),

          # Wood Constants Tab
          tabPanel(
            "Wood Constants",
            br(),

            # Calculate button
            div(
              style = "text-align: center; margin-bottom: 20px;",
              actionButton(ns("calculate_wood_props_2"),
                          "Calculate Derived Properties",
                          icon = icon("calculator"),
                          class = "btn-success btn-lg")
            ),

            p(class = "help-text",
              "Physical constants for wood and sap. Defaults from Burgess et al. (2001)."),

            h5("Basic Properties:"),
            textInput(ns("species"), "Species:", value = "unknown"),

            textInput(ns("config_name"), "Configuration Name:", value = "Custom Wood Properties"),

            selectInput(ns("wood_type"), "Wood Type:",
                       choices = c("Softwood" = "softwood",
                                  "Hardwood" = "hardwood",
                                  "Unknown" = "unknown"),
                       selected = "softwood"),

            hr(),

            h5("Thermal Constants:"),

            fluidRow(
              column(
                width = 6,
                numericInput(ns("thermal_diffusivity_default_cm2_s"),
                            "Default Thermal Diffusivity (cm²/s):",
                            value = 0.0025, min = 0.001, max = 0.01, step = 0.0001),

                numericInput(ns("c_sap_J_kg_K"),
                            "Sap Specific Heat (J/(kg·K)):",
                            value = 4186, min = 4000, max = 4400, step = 10),

                numericInput(ns("rho_cell_wall_kg_m3"),
                            "Cell Wall Density (kg/m³):",
                            value = 1530, min = 1400, max = 1600, step = 10)
              ),
              column(
                width = 6,
                numericInput(ns("rho_sap_kg_m3"),
                            "Sap Density (kg/m³):",
                            value = 1000, min = 900, max = 1100, step = 10),

                numericInput(ns("K_sap_W_m_K"),
                            "Sap Thermal Conductivity (W/(m·K)):",
                            value = 0.5984, min = 0.5, max = 0.7, step = 0.01),

                numericInput(ns("c_dry_wood_J_kg_K"),
                            "Dry Wood Specific Heat (J/(kg·K)):",
                            value = 1200, min = 1000, max = 1400, step = 10)
              )
            )
          ),

          # Wound Correction Tab
          tabPanel(
            "Wound Correction",
            br(),

            # Calculate button
            div(
              style = "text-align: center; margin-bottom: 20px;",
              actionButton(ns("calculate_wood_props_3"),
                          "Calculate Derived Properties",
                          icon = icon("calculator"),
                          class = "btn-success btn-lg")
            ),

            h5("Initial Wound Configuration:"),
            p(class = "help-text",
              "The wound around the probe affects the measured sapwood area."),

            numericInput(ns("drill_bit_diameter_mm"),
                        "Drill Bit Diameter (mm):",
                        value = 2.0, min = 1.0, max = 5.0, step = 0.1),

            numericInput(ns("wound_addition_mm"),
                        "Wound Addition per Side (mm):",
                        value = 0.3, min = 0.0, max = 1.0, step = 0.05),

            uiOutput(ns("initial_wound_display")),

            hr(),

            h5("Temporal Wound Tracking (Optional):"),
            checkboxInput(ns("enable_temporal_wound"),
                         "Enable temporal wound expansion tracking",
                         value = FALSE),

            conditionalPanel(
              condition = sprintf("input['%s'] == true", ns("enable_temporal_wound")),

              dateInput(ns("wound_initial_date"),
                       "Installation Date:",
                       value = NULL),

              dateInput(ns("wound_final_date"),
                       "Final Measurement Date:",
                       value = NULL),

              numericInput(ns("wound_final_diameter_mm"),
                          "Final Wound Diameter (mm):",
                          value = NULL, min = 2.0, max = 10.0, step = 0.1),

              uiOutput(ns("wound_growth_summary"))
            ),

            p(class = "text-muted",
              "Initial wound = drill bit + 2 × wound addition. Default: 2.0 + 2(0.3) = 2.6 mm")
          ),

          # Derived Properties Tab
          tabPanel(
            "Derived Properties",
            br(),

            # Calculate button
            div(
              style = "text-align: center; margin-bottom: 20px;",
              actionButton(ns("calculate_wood_props_4"),
                          "Calculate Derived Properties",
                          icon = icon("calculator"),
                          class = "btn-success btn-lg")
            ),

            p(class = "help-text",
              "Calculate wood properties based on your measurements. Requires Method 1 or Method 2 measurements."),

            uiOutput(ns("derived_properties_display"))
          ),

          # Tree Measurements Tab
          tabPanel(
            "Tree Measurements",
            br(),

            # Calculate button
            div(
              style = "text-align: center; margin-bottom: 20px;",
              actionButton(ns("calculate_wood_props_5"),
                          "Calculate Derived Properties",
                          icon = icon("calculator"),
                          class = "btn-success btn-lg")
            ),

            p(class = "help-text", "Optional - tree-specific measurements for scaling calculations."),

            numericInput(ns("dbh"), "DBH - Diameter at Breast Height (cm):",
                        value = NULL, min = 1, max = 300, step = 0.1),

            numericInput(ns("bark_thickness"), "Bark Thickness (cm):",
                        value = NULL, min = 0.1, max = 5, step = 0.1),

            numericInput(ns("sapwood_depth"), "Sapwood Depth (cm):",
                        value = NULL, min = 0.1, max = 50, step = 0.1),

            numericInput(ns("sapwood_area"), "Sapwood Area (cm²):",
                        value = NULL, min = 1, max = 10000, step = 1)
          ),

          # Quality Thresholds Tab
          tabPanel(
            "Quality Thresholds",
            br(),

            # Calculate button
            div(
              style = "text-align: center; margin-bottom: 20px;",
              actionButton(ns("calculate_wood_props_6"),
                          "Calculate Derived Properties",
                          icon = icon("calculator"),
                          class = "btn-success btn-lg")
            ),

            p(class = "help-text", "Set acceptable ranges for quality control."),

            numericInput(ns("max_velocity"), "Maximum Velocity (cm/hr):",
                        value = 200, min = 50, max = 500, step = 10),

            numericInput(ns("min_velocity"), "Minimum Velocity (cm/hr):",
                        value = -50, min = -100, max = 0, step = 5),

            sliderInput(ns("temp_range"), "Temperature Range (°C):",
                       min = -20, max = 80, value = c(-10, 60), step = 1)
          )
        )
      )
    ),

    fluidRow(
      box(
        width = 12,
        title = "Export Configuration",
        status = "success",
        solidHeader = TRUE,

        p(class = "help-text",
          "Download your wood properties configuration as a YAML file. ",
          "Derived properties will be automatically calculated if measurements are provided."),

        textInput(
          ns("filename"),
          "Filename:",
          value = "custom_wood_properties.yaml",
          placeholder = "my_wood_properties.yaml"
        ),

        div(
          style = "text-align: center; margin-top: 20px;",
          downloadButton(
            ns("download_yaml"),
            "Download YAML Configuration",
            icon = icon("download"),
            class = "btn-success btn-lg"
          )
        )
      )
    )
  )
}

# Server ----
toolWoodServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive to store current configuration
    current_config <- reactiveVal(NULL)

    # Reactive to store uploaded filename
    uploaded_filename <- reactiveVal(NULL)

    # Upload YAML and parse
    observeEvent(input$upload_yaml, {
      req(input$upload_yaml)

      tryCatch({
        # Store uploaded filename
        original_name <- input$upload_yaml$name
        uploaded_filename(original_name)

        # Update filename to <original>_edited.yaml
        new_filename <- sub("\\.ya?ml$", "_edited.yaml", original_name, ignore.case = TRUE)
        updateTextInput(session, "filename", value = new_filename)

        config <- sapfluxr::load_wood_properties(input$upload_yaml$datapath)
        current_config(config)

        # Populate UI fields from loaded config
        # Wood measurements
        if (!is.null(config$wood_measurements)) {
          if (!is.null(config$wood_measurements$fresh_weight_g)) {
            updateNumericInput(session, "fresh_weight_g", value = config$wood_measurements$fresh_weight_g)
          }
          if (!is.null(config$wood_measurements$dry_weight_g)) {
            updateNumericInput(session, "dry_weight_g", value = config$wood_measurements$dry_weight_g)
          }
          if (!is.null(config$wood_measurements$fresh_volume_cm3)) {
            updateNumericInput(session, "fresh_volume_cm3", value = config$wood_measurements$fresh_volume_cm3)
          }
          if (!is.null(config$wood_measurements$density_dry_kg_m3)) {
            updateNumericInput(session, "density_dry_kg_m3", value = config$wood_measurements$density_dry_kg_m3)
          }
          if (!is.null(config$wood_measurements$density_fresh_kg_m3)) {
            updateNumericInput(session, "density_fresh_kg_m3", value = config$wood_measurements$density_fresh_kg_m3)
          }
        }

        # Wood constants
        if (!is.null(config$wood_constants)) {
          if (!is.null(config$wood_constants$thermal_diffusivity_default_cm2_s)) {
            updateNumericInput(session, "thermal_diffusivity_default_cm2_s",
                              value = config$wood_constants$thermal_diffusivity_default_cm2_s)
          }
          if (!is.null(config$wood_constants$rho_sap_kg_m3)) {
            updateNumericInput(session, "rho_sap_kg_m3", value = config$wood_constants$rho_sap_kg_m3)
          }
          if (!is.null(config$wood_constants$c_sap_J_kg_K)) {
            updateNumericInput(session, "c_sap_J_kg_K", value = config$wood_constants$c_sap_J_kg_K)
          }
          if (!is.null(config$wood_constants$K_sap_W_m_K)) {
            updateNumericInput(session, "K_sap_W_m_K", value = config$wood_constants$K_sap_W_m_K)
          }
          if (!is.null(config$wood_constants$rho_cell_wall_kg_m3)) {
            updateNumericInput(session, "rho_cell_wall_kg_m3", value = config$wood_constants$rho_cell_wall_kg_m3)
          }
          if (!is.null(config$wood_constants$c_dry_wood_J_kg_K)) {
            updateNumericInput(session, "c_dry_wood_J_kg_K", value = config$wood_constants$c_dry_wood_J_kg_K)
          }
        }

        # Wood property
        if (!is.null(config$wood_property)) {
          if (!is.null(config$wood_property$species)) {
            updateTextInput(session, "species", value = config$wood_property$species)
          }
          if (!is.null(config$wood_property$wood_type)) {
            updateSelectInput(session, "wood_type", selected = config$wood_property$wood_type)
          }
        }

        # Config name
        if (!is.null(config$config_name)) {
          updateTextInput(session, "config_name", value = config$config_name)
        }

        # Wound correction
        if (!is.null(config$wound_correction)) {
          if (!is.null(config$wound_correction$drill_bit_diameter_mm)) {
            updateNumericInput(session, "drill_bit_diameter_mm",
                              value = config$wound_correction$drill_bit_diameter_mm)
          }
          if (!is.null(config$wound_correction$wound_addition_mm)) {
            updateNumericInput(session, "wound_addition_mm",
                              value = config$wound_correction$wound_addition_mm)
          }
          if (!is.null(config$wound_correction$initial_date)) {
            updateDateInput(session, "wound_initial_date",
                           value = as.Date(config$wound_correction$initial_date))
            updateCheckboxInput(session, "enable_temporal_wound", value = TRUE)
          }
          if (!is.null(config$wound_correction$final_date)) {
            updateDateInput(session, "wound_final_date",
                           value = as.Date(config$wound_correction$final_date))
          }
          if (!is.null(config$wound_correction$final_diameter_mm)) {
            updateNumericInput(session, "wound_final_diameter_mm",
                              value = config$wound_correction$final_diameter_mm)
          }
        }

        # Tree measurements
        if (!is.null(config$tree_measurements)) {
          if (!is.null(config$tree_measurements$dbh)) {
            updateNumericInput(session, "dbh", value = config$tree_measurements$dbh)
          }
          if (!is.null(config$tree_measurements$bark_thickness)) {
            updateNumericInput(session, "bark_thickness", value = config$tree_measurements$bark_thickness)
          }
          if (!is.null(config$tree_measurements$sapwood_depth)) {
            updateNumericInput(session, "sapwood_depth", value = config$tree_measurements$sapwood_depth)
          }
          if (!is.null(config$tree_measurements$sapwood_area)) {
            updateNumericInput(session, "sapwood_area", value = config$tree_measurements$sapwood_area)
          }
        }

        # Quality thresholds
        if (!is.null(config$quality_thresholds)) {
          if (!is.null(config$quality_thresholds$max_velocity_cm_hr)) {
            updateNumericInput(session, "max_velocity", value = config$quality_thresholds$max_velocity_cm_hr)
          }
          if (!is.null(config$quality_thresholds$min_velocity_cm_hr)) {
            updateNumericInput(session, "min_velocity", value = config$quality_thresholds$min_velocity_cm_hr)
          }
          if (!is.null(config$quality_thresholds$temperature_range)) {
            updateSliderInput(session, "temp_range", value = config$quality_thresholds$temperature_range)
          }
        }

      }, error = function(e) {
        showNotification(
          paste("Error loading YAML:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # Upload status
    output$upload_status <- renderUI({
      req(input$upload_yaml)

      div(
        class = "alert alert-success",
        icon("check-circle"),
        " Configuration loaded successfully"
      )
    })

    # Method help text
    output$method_help_text <- renderUI({
      if (input$wood_input_method == "method1") {
        div(
          class = "alert alert-info",
          style = "margin-bottom: 15px;",
          tags$strong(icon("flask"), " Method 1: Weight & Volume"),
          tags$p(style = "margin-top: 10px; margin-bottom: 5px;",
            "Directly measure fresh weight, dry weight, and volume from wood samples."),
          tags$ul(
            tags$li("Most intuitive approach"),
            tags$li("Fresh weight must be measured quickly after sampling"),
            tags$li("Requires oven access for drying to constant weight")
          )
        )
      } else {
        div(
          class = "alert alert-success",
          style = "margin-bottom: 15px;",
          tags$strong(icon("check-circle"), " Method 2: Dual Density (RECOMMENDED)"),
          tags$p(style = "margin-top: 10px; margin-bottom: 5px;",
            "Measure both dry and fresh density on the same sample."),
          tags$ul(
            tags$li(tags$strong("Easier workflow:"), " no fresh weight timing pressure"),
            tags$li(tags$strong("Same results:"), " back-calculates moisture content from density ratio"),
            tags$li(tags$strong("Complete:"), " calculates ALL properties including Z factor"),
            tags$li(tags$strong("Simpler:"), " volume measurement only once")
          )
        )
      }
    })

    # Initial wound display
    output$initial_wound_display <- renderUI({
      req(input$drill_bit_diameter_mm, input$wound_addition_mm)

      initial_wound <- input$drill_bit_diameter_mm + 2 * input$wound_addition_mm

      div(
        class = "alert alert-secondary",
        style = "margin-top: 15px;",
        tags$strong("Calculated Initial Wound Diameter:"),
        tags$p(style = "font-size: 1.2em; margin-top: 10px; margin-bottom: 0;",
          sprintf("%.2f mm", initial_wound)),
        tags$p(class = "text-muted", style = "font-size: 0.9em; margin-top: 5px;",
          sprintf("(%.1f mm drill + 2 × %.2f mm wound addition)",
                  input$drill_bit_diameter_mm, input$wound_addition_mm))
      )
    })

    # Wound growth summary
    output$wound_growth_summary <- renderUI({
      req(input$wound_initial_date, input$wound_final_date,
          input$wound_final_diameter_mm, input$drill_bit_diameter_mm,
          input$wound_addition_mm)

      initial_wound <- input$drill_bit_diameter_mm + 2 * input$wound_addition_mm
      final_wound <- input$wound_final_diameter_mm

      days_elapsed <- as.numeric(difftime(input$wound_final_date,
                                         input$wound_initial_date,
                                         units = "days"))

      if (days_elapsed <= 0) {
        return(div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          " Final date must be after initial date"
        ))
      }

      wound_expansion <- final_wound - initial_wound
      growth_rate_per_day <- wound_expansion / days_elapsed
      growth_rate_per_year <- growth_rate_per_day * 365.25

      div(
        class = "alert alert-info",
        style = "margin-top: 15px;",
        tags$strong("Wound Expansion Summary:"),
        tags$table(
          class = "table table-sm",
          style = "margin-top: 10px; background-color: white;",
          tags$tr(
            tags$td("Initial wound:"),
            tags$td(tags$strong(sprintf("%.2f mm", initial_wound)))
          ),
          tags$tr(
            tags$td("Final wound:"),
            tags$td(tags$strong(sprintf("%.2f mm", final_wound)))
          ),
          tags$tr(
            tags$td("Expansion:"),
            tags$td(tags$strong(sprintf("%.2f mm", wound_expansion)))
          ),
          tags$tr(
            tags$td("Days elapsed:"),
            tags$td(tags$strong(sprintf("%.0f days", days_elapsed)))
          ),
          tags$tr(
            tags$td("Growth rate:"),
            tags$td(tags$strong(sprintf("%.4f mm/day (%.3f mm/year)",
                                       growth_rate_per_day, growth_rate_per_year)))
          )
        )
      )
    })

    # Function to calculate derived properties (shared by all buttons)
    calculate_derived <- function() {
      tryCatch({
        # Create wood properties object
        config <- sapfluxr::WoodProperties$new(
          config_name = input$config_name
        )

        # Set wood_property
        config$wood_property <- list(
          species = input$species,
          wood_type = input$wood_type,
          temperature = 20,
          comments = "Created via Wood Properties Tool"
        )

        # Populate wood_constants
        config$wood_constants <- list(
          thermal_diffusivity_default_cm2_s = input$thermal_diffusivity_default_cm2_s,
          rho_sap_kg_m3 = input$rho_sap_kg_m3,
          c_sap_J_kg_K = input$c_sap_J_kg_K,
          K_sap_W_m_K = input$K_sap_W_m_K,
          rho_cell_wall_kg_m3 = input$rho_cell_wall_kg_m3,
          c_dry_wood_J_kg_K = input$c_dry_wood_J_kg_K
        )

        # Populate wood_measurements
        config$wood_measurements <- list()
        if (!is.null(input$wood_input_method) && input$wood_input_method == "method1") {
          if (!is.null(input$fresh_weight_g)) {
            config$wood_measurements$fresh_weight_g <- input$fresh_weight_g
          }
          if (!is.null(input$dry_weight_g)) {
            config$wood_measurements$dry_weight_g <- input$dry_weight_g
          }
          if (!is.null(input$fresh_volume_cm3)) {
            config$wood_measurements$fresh_volume_cm3 <- input$fresh_volume_cm3
          }
        } else {
          if (!is.null(input$density_dry_kg_m3)) {
            config$wood_measurements$density_dry_kg_m3 <- input$density_dry_kg_m3
          }
          if (!is.null(input$density_fresh_kg_m3)) {
            config$wood_measurements$density_fresh_kg_m3 <- input$density_fresh_kg_m3
          }
        }

        # Calculate derived properties
        config <- sapfluxr::derive_wood_properties(config)

        # Store configuration
        current_config(config)

        showNotification(
          "Derived properties calculated successfully!",
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        showNotification(
          paste("Error calculating properties:", e$message),
          type = "error",
          duration = 10
        )
      })
    }

    # All calculate buttons trigger the same function
    observeEvent(input$calculate_wood_props, { calculate_derived() })
    observeEvent(input$calculate_wood_props_2, { calculate_derived() })
    observeEvent(input$calculate_wood_props_3, { calculate_derived() })
    observeEvent(input$calculate_wood_props_4, { calculate_derived() })
    observeEvent(input$calculate_wood_props_5, { calculate_derived() })
    observeEvent(input$calculate_wood_props_6, { calculate_derived() })

    # Auto-calculate sapwood area when DBH, bark thickness, and sapwood depth are provided
    observe({
      req(input$dbh, input$bark_thickness, input$sapwood_depth)

      # Calculate sapwood area
      # Sapwood area = π * (R_sapwood² - R_heartwood²)
      # R_sapwood = DBH/2 - bark_thickness
      # R_heartwood = R_sapwood - sapwood_depth

      R_sapwood <- (input$dbh / 2) - input$bark_thickness
      R_heartwood <- R_sapwood - input$sapwood_depth

      if (R_heartwood < 0) R_heartwood <- 0  # No heartwood

      sapwood_area <- pi * (R_sapwood^2 - R_heartwood^2)

      # Update the sapwood area input
      updateNumericInput(session, "sapwood_area", value = round(sapwood_area, 2))
    })

    # Display derived properties
    output$derived_properties_display <- renderUI({
      config <- current_config()

      if (is.null(config) || is.null(config$derived_properties)) {
        return(div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " No derived properties calculated yet. Click the button above to calculate."
        ))
      }

      dp <- config$derived_properties

      div(
        class = "alert alert-success",
        style = "margin-top: 15px;",
        tags$strong(icon("check-circle"), " Derived Properties Calculated:"),
        tags$table(
          class = "table table-sm",
          style = "margin-top: 10px; background-color: white;",
          tags$tr(
            tags$td("Moisture Content (kg/kg):"),
            tags$td(tags$strong(if (!is.null(dp$mc_kg_kg)) sprintf("%.4f", dp$mc_kg_kg) else "N/A"))
          ),
          tags$tr(
            tags$td("Dry Density (kg/m³):"),
            tags$td(tags$strong(if (!is.null(dp$rho_dw_kg_m3)) sprintf("%.1f", dp$rho_dw_kg_m3) else "N/A"))
          ),
          tags$tr(
            tags$td("Fresh Density (kg/m³):"),
            tags$td(tags$strong(if (!is.null(dp$rho_fw_kg_m3)) sprintf("%.1f", dp$rho_fw_kg_m3) else "N/A"))
          ),
          tags$tr(
            tags$td("Thermal Diffusivity (cm²/s):"),
            tags$td(tags$strong(if (!is.null(dp$thermal_diffusivity_actual_cm2_s))
                               sprintf("%.6f", dp$thermal_diffusivity_actual_cm2_s) else "N/A"))
          ),
          tags$tr(
            tags$td("Sap Flux Conversion Factor (Z):"),
            tags$td(tags$strong(if (!is.null(dp$sap_flux_conversion_factor))
                               sprintf("%.6f", dp$sap_flux_conversion_factor) else "N/A"))
          ),
          tags$tr(
            tags$td("Thermal Diffusivity Correction (Y):"),
            tags$td(tags$strong(if (!is.null(dp$thermal_diffusivity_correction_factor))
                               sprintf("%.6f", dp$thermal_diffusivity_correction_factor) else "N/A"))
          )
        )
      )
    })

    # Validation summary
    output$validation_summary <- renderUI({
      # Check if measurements provided
      has_method1 <- !is.null(input$fresh_weight_g) && !is.null(input$dry_weight_g) &&
                     !is.null(input$fresh_volume_cm3) &&
                     !is.na(input$fresh_weight_g) && !is.na(input$dry_weight_g) &&
                     !is.na(input$fresh_volume_cm3)

      has_method2 <- !is.null(input$density_dry_kg_m3) && !is.null(input$density_fresh_kg_m3) &&
                     !is.na(input$density_dry_kg_m3) && !is.na(input$density_fresh_kg_m3)

      has_measurements <- has_method1 || has_method2

      config <- current_config()
      has_derived <- !is.null(config) && !is.null(config$derived_properties) &&
                    !all(sapply(config$derived_properties, is.null))

      # Validation messages
      messages <- list()
      status <- "warning"

      if (has_measurements) {
        messages <- c(messages, list(tags$li(icon("check", style = "color: green;"),
                                             " Wood measurements provided")))
      } else {
        messages <- c(messages, list(tags$li(icon("exclamation-triangle", style = "color: orange;"),
                                             " No wood measurements (optional but recommended)")))
      }

      if (has_derived) {
        messages <- c(messages, list(tags$li(icon("check", style = "color: green;"),
                                             " Derived properties calculated")))
        status <- "success"
      } else if (has_measurements) {
        messages <- c(messages, list(tags$li(icon("info-circle", style = "color: blue;"),
                                             " Measurements provided - click 'Calculate Derived Properties'")))
      }

      if (!is.null(input$species) && input$species != "") {
        messages <- c(messages, list(tags$li(icon("check", style = "color: green;"),
                                             " Species specified")))
      }

      div(
        class = paste0("alert alert-", status),
        tags$strong("Configuration Status:"),
        tags$ul(style = "margin-top: 10px; margin-bottom: 0;", messages)
      )
    })

    # Download handler
    output$download_yaml <- downloadHandler(
      filename = function() {
        input$filename
      },
      content = function(file) {
        tryCatch({
          # Create wood properties object from current inputs
          config <- sapfluxr::WoodProperties$new(
            config_name = input$config_name
          )

          # Set wood_property
          config$wood_property <- list(
            species = input$species,
            wood_type = input$wood_type,
            temperature = 20,
            comments = "Created via Wood Properties Tool"
          )

          # Populate wood_constants
          config$wood_constants <- list(
            thermal_diffusivity_default_cm2_s = input$thermal_diffusivity_default_cm2_s,
            rho_sap_kg_m3 = input$rho_sap_kg_m3,
            c_sap_J_kg_K = input$c_sap_J_kg_K,
            K_sap_W_m_K = input$K_sap_W_m_K,
            rho_cell_wall_kg_m3 = input$rho_cell_wall_kg_m3,
            c_dry_wood_J_kg_K = input$c_dry_wood_J_kg_K
          )

          # Populate wood_measurements
          config$wood_measurements <- list()
          if (!is.null(input$wood_input_method) && input$wood_input_method == "method1") {
            if (!is.null(input$fresh_weight_g) && !is.na(input$fresh_weight_g)) {
              config$wood_measurements$fresh_weight_g <- input$fresh_weight_g
            }
            if (!is.null(input$dry_weight_g) && !is.na(input$dry_weight_g)) {
              config$wood_measurements$dry_weight_g <- input$dry_weight_g
            }
            if (!is.null(input$fresh_volume_cm3) && !is.na(input$fresh_volume_cm3)) {
              config$wood_measurements$fresh_volume_cm3 <- input$fresh_volume_cm3
            }
          } else {
            if (!is.null(input$density_dry_kg_m3) && !is.na(input$density_dry_kg_m3)) {
              config$wood_measurements$density_dry_kg_m3 <- input$density_dry_kg_m3
            }
            if (!is.null(input$density_fresh_kg_m3) && !is.na(input$density_fresh_kg_m3)) {
              config$wood_measurements$density_fresh_kg_m3 <- input$density_fresh_kg_m3
            }
          }

          # Populate wound_correction
          config$wound_correction <- list(
            drill_bit_diameter_mm = input$drill_bit_diameter_mm,
            wound_addition_mm = input$wound_addition_mm
          )

          if (!is.null(input$enable_temporal_wound) && input$enable_temporal_wound) {
            if (!is.null(input$wound_initial_date) && !is.na(input$wound_initial_date)) {
              config$wound_correction$initial_date <- as.character(input$wound_initial_date)
            }
            if (!is.null(input$wound_final_date) && !is.na(input$wound_final_date)) {
              config$wound_correction$final_date <- as.character(input$wound_final_date)
            }
            if (!is.null(input$wound_final_diameter_mm) && !is.na(input$wound_final_diameter_mm)) {
              config$wound_correction$final_diameter_mm <- input$wound_final_diameter_mm
            }
          }

          # Populate tree_measurements
          config$tree_measurements <- list()
          if (!is.null(input$dbh) && !is.na(input$dbh)) {
            config$tree_measurements$dbh <- input$dbh
          }
          if (!is.null(input$bark_thickness) && !is.na(input$bark_thickness)) {
            config$tree_measurements$bark_thickness <- input$bark_thickness
          }
          if (!is.null(input$sapwood_depth) && !is.na(input$sapwood_depth)) {
            config$tree_measurements$sapwood_depth <- input$sapwood_depth
          }
          if (!is.null(input$sapwood_area) && !is.na(input$sapwood_area)) {
            config$tree_measurements$sapwood_area <- input$sapwood_area
          }

          # Populate quality_thresholds
          config$quality_thresholds <- list(
            max_velocity_cm_hr = input$max_velocity,
            min_velocity_cm_hr = input$min_velocity,
            temperature_range = input$temp_range
          )

          # Auto-calculate derived properties if measurements present
          has_measurements <- (!is.null(config$wood_measurements$density_dry_kg_m3) &&
                              !is.na(config$wood_measurements$density_dry_kg_m3) &&
                              !is.null(config$wood_measurements$density_fresh_kg_m3) &&
                              !is.na(config$wood_measurements$density_fresh_kg_m3)) ||
                             (!is.null(config$wood_measurements$fresh_weight_g) &&
                              !is.na(config$wood_measurements$fresh_weight_g) &&
                              !is.null(config$wood_measurements$dry_weight_g) &&
                              !is.na(config$wood_measurements$dry_weight_g) &&
                              !is.null(config$wood_measurements$fresh_volume_cm3) &&
                              !is.na(config$wood_measurements$fresh_volume_cm3))

          if (has_measurements) {
            config <- sapfluxr::derive_wood_properties(config)
          }

          # Create YAML data structure manually
          yaml_data <- list(
            metadata = list(
              config_name = input$config_name,
              description = paste("Wood properties for", input$species),
              version = "1.0",
              created_date = as.character(Sys.Date())
            ),
            wood_property = list(
              species = input$species,
              wood_type = input$wood_type,
              temperature = 20,
              comments = "Created via Wood Properties Tool"
            ),
            wood_constants = list(
              thermal_diffusivity_default_cm2_s = input$thermal_diffusivity_default_cm2_s,
              rho_sap_kg_m3 = input$rho_sap_kg_m3,
              c_sap_J_kg_K = input$c_sap_J_kg_K,
              K_sap_W_m_K = input$K_sap_W_m_K,
              rho_cell_wall_kg_m3 = input$rho_cell_wall_kg_m3,
              c_dry_wood_J_kg_K = input$c_dry_wood_J_kg_K
            ),
            wood_measurements = config$wood_measurements,
            wound_correction = config$wound_correction,
            tree_measurements = config$tree_measurements,
            quality_thresholds = config$quality_thresholds
          )

          # Add derived properties if they exist
          if (!is.null(config$derived_properties) &&
              !all(sapply(config$derived_properties, is.null))) {
            yaml_data$derived_properties <- config$derived_properties
          }

          # Write YAML file
          yaml::write_yaml(yaml_data, file)

          showNotification(
            "YAML configuration downloaded successfully!",
            type = "message",
            duration = 3
          )

        }, error = function(e) {
          showNotification(
            paste("Error generating YAML:", e$message),
            type = "error",
            duration = 10
          )
        })
      }
    )

  })
}
