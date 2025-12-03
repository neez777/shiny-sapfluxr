#' Configuration Module
#'
#' Shiny module for probe and wood property configuration
#' Supports YAML selection, file upload, and manual entry with derived value calculation
#'
#' @param id Module ID
#' @param heat_pulse_data Reactive containing heat_pulse_data object (optional, for context)
#' @return List of reactives: probe_config, wood_properties
#'

# UI ----
configUI <- function(id) {
  ns <- NS(id)

  tagList(
    # JavaScript for adding/removing CSS classes
    tags$script(HTML("
      Shiny.addCustomMessageHandler('addClass', function(message) {
        $('#' + message.id).addClass(message.class);
      });
      Shiny.addCustomMessageHandler('removeClass', function(message) {
        $('#' + message.id).removeClass(message.class);
      });
    ")),

    fluidRow(
      # Probe Configuration
      column(
        width = 6,
        box(
          width = NULL,
          title = "Probe Configuration",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          # Mode selector
          radioButtons(
            ns("probe_mode"),
            "Configuration Source:",
            choices = c(
              "Default" = "builtin",
              "Upload YAML" = "upload",
              "Manual Entry" = "manual"
            ),
            selected = "builtin"
          ),

          # Built-in configurations selector
          conditionalPanel(
            condition = sprintf("input['%s'] == 'builtin'", ns("probe_mode")),
            selectInput(
              ns("probe_yaml_builtin"),
              "Built-in configurations:",
              choices = NULL  # Will be populated in server
            )
          ),

          # Upload YAML
          conditionalPanel(
            condition = sprintf("input['%s'] == 'upload'", ns("probe_mode")),
            fileInput(
              ns("probe_yaml_upload"),
              "Upload Probe YAML:",
              accept = c(".yaml", ".yml")
            )
          ),

          # Manual entry
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("probe_mode")),
            uiOutput(ns("probe_manual_ui"))
          ),

          hr(),
          uiOutput(ns("probe_summary"))
        )
      ),

      # Wood Properties Configuration
      column(
        width = 6,
        box(
          width = NULL,
          title = "Wood Properties",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          # Mode selector
          radioButtons(
            ns("wood_mode"),
            "Configuration Source:",
            choices = c(
              "Default" = "builtin",
              "Upload YAML" = "upload",
              "Manual Entry" = "manual"
            ),
            selected = "builtin"
          ),

          # Built-in configurations selector
          conditionalPanel(
            condition = sprintf("input['%s'] == 'builtin'", ns("wood_mode")),
            selectInput(
              ns("wood_yaml_builtin"),
              "Built-in configurations:",
              choices = NULL  # Will be populated in server
            )
          ),

          # Upload YAML
          conditionalPanel(
            condition = sprintf("input['%s'] == 'upload'", ns("wood_mode")),
            fileInput(
              ns("wood_yaml_upload"),
              "Upload Wood Properties YAML:",
              accept = c(".yaml", ".yml")
            )
          ),

          # Manual entry
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("wood_mode")),
            uiOutput(ns("wood_manual_ui"))
          ),

          hr(),
          uiOutput(ns("wood_summary"))
        )
      )
    ),

    # Probe Visualisation
    fluidRow(
      box(
        width = 12,
        title = "Probe Configuration Visualisation",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = FALSE,

        p(
          "Visual representation of probe placement relative to tree anatomy.",
          style = "margin-bottom: 15px;"
        ),

        fluidRow(
          column(
            width = 6,
            plotOutput(ns("probe_vertical_plot"), height = "400px")
          ),
          column(
            width = 6,
            plotOutput(ns("probe_radial_plot"), height = "400px")
          )
        ),

        hr(),

        # Configuration assessment feedback
        uiOutput(ns("config_assessment"))
      )
    )
  )
}

# Server ----
configServer <- function(id, heat_pulse_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    probe_config <- reactiveVal(NULL)
    wood_properties <- reactiveVal(NULL)

    # Get available YAML files from sapfluxr
    available_probe_yamls <- reactive({
      yaml_dir <- system.file("configurations", package = "sapfluxr")
      if (yaml_dir == "") return(NULL)

      files <- list.files(yaml_dir, pattern = "^probe_.*\\.yaml$", full.names = TRUE)

      # Filter out asymmetrical probe configurations
      files <- files[!grepl("asymmetrical", basename(files), ignore.case = TRUE)]

      file_names <- basename(files)
      file_names <- gsub("^probe_", "", file_names)
      file_names <- gsub("\\.yaml$", "", file_names)
      file_names <- gsub("_", " ", file_names)
      file_names <- tools::toTitleCase(file_names)
      names(files) <- file_names

      files
    })

    available_wood_yamls <- reactive({
      yaml_dir <- system.file("configurations", package = "sapfluxr")
      if (yaml_dir == "") return(NULL)

      files <- list.files(yaml_dir, pattern = "^wood_.*\\.yaml$", full.names = TRUE)
      file_names <- basename(files)
      file_names <- gsub("^wood_", "", file_names)
      file_names <- gsub("\\.yaml$", "", file_names)
      file_names <- gsub("_", " ", file_names)
      file_names <- tools::toTitleCase(file_names)
      names(files) <- file_names

      files
    })

    # Populate YAML dropdowns
    observe({
      yamls <- available_probe_yamls()
      if (!is.null(yamls)) {
        # Find symmetrical but NOT asymmetrical
        sym_idx <- grep("^probe_symmetrical\\.yaml$", basename(yamls))
        if (length(sym_idx) == 0) {
          # Fallback: find first file with "symmetrical" that doesn't have "asymmetrical"
          sym_idx <- grep("symmetrical", yamls, ignore.case = TRUE)
          sym_idx <- sym_idx[!grepl("asymmetrical", yamls[sym_idx], ignore.case = TRUE)]
        }
        default_selection <- if (length(sym_idx) > 0) yamls[sym_idx[1]] else yamls[1]

        updateSelectInput(session, "probe_yaml_builtin",
                         choices = yamls,
                         selected = default_selection)
      }
    })

    observe({
      yamls <- available_wood_yamls()
      if (!is.null(yamls)) {
        updateSelectInput(session, "wood_yaml_builtin",
                         choices = yamls,
                         selected = yamls[grep("generic", yamls, ignore.case = TRUE)[1]])
      }
    })

    # Load probe configuration based on mode
    observe({
      if (input$probe_mode == "builtin") {
        req(input$probe_yaml_builtin)

        tryCatch({
          config <- sapfluxr::load_probe_config(input$probe_yaml_builtin)
          probe_config(config)
        }, error = function(e) {
          notify_error(
            session = session,
            title = "Probe Config Error",
            text = e$message
          )
        })

      } else if (input$probe_mode == "upload") {
        req(input$probe_yaml_upload)

        tryCatch({
          config <- sapfluxr::load_probe_config(input$probe_yaml_upload$datapath)
          probe_config(config)

          notify_success(
            session = session,
            title = "Success!",
            text = "Probe configuration loaded successfully"
          )
        }, error = function(e) {
          notify_error(
            session = session,
            title = "YAML Load Error",
            text = e$message
          )
          probe_config(NULL)
        })
      }
      # Manual mode handled separately
    })

    # Load wood properties based on mode
    observe({
      if (input$wood_mode == "builtin") {
        req(input$wood_yaml_builtin)

        tryCatch({
          config <- sapfluxr::load_wood_properties(input$wood_yaml_builtin)

          # Auto-calculate derived properties if not present
          has_derived <- !is.null(config$derived_properties) &&
                        !all(sapply(config$derived_properties, is.null))

          if (!has_derived) {
            # Check if we have measurements to calculate from
            has_measurements <- (!is.null(config$density_dry_kg_m3) && !is.na(config$density_dry_kg_m3) &&
                                !is.null(config$density_fresh_kg_m3) && !is.na(config$density_fresh_kg_m3)) ||
                               (!is.null(config$fresh_weight_g) && !is.na(config$fresh_weight_g) &&
                                !is.null(config$dry_weight_g) && !is.na(config$dry_weight_g) &&
                                !is.null(config$fresh_volume_cm3) && !is.na(config$fresh_volume_cm3))

            if (has_measurements) {
              # Calculate derived properties
              sapfluxr::calculate_wood_properties(config)
            }
          }

          wood_properties(config)
        }, error = function(e) {
          notify_error(session, "Error loading wood properties:", e$message)
        })

      } else if (input$wood_mode == "upload") {
        req(input$wood_yaml_upload)

        tryCatch({
          config <- sapfluxr::load_wood_properties(input$wood_yaml_upload$datapath)

          # Auto-calculate derived properties if not present
          has_derived <- !is.null(config$derived_properties) &&
                        !all(sapply(config$derived_properties, is.null))

          if (!has_derived) {
            # Check if we have measurements to calculate from
            has_measurements <- (!is.null(config$density_dry_kg_m3) && !is.na(config$density_dry_kg_m3) &&
                                !is.null(config$density_fresh_kg_m3) && !is.na(config$density_fresh_kg_m3)) ||
                               (!is.null(config$fresh_weight_g) && !is.na(config$fresh_weight_g) &&
                                !is.null(config$dry_weight_g) && !is.na(config$dry_weight_g) &&
                                !is.null(config$fresh_volume_cm3) && !is.na(config$fresh_volume_cm3))

            if (has_measurements) {
              # Calculate derived properties
              sapfluxr::calculate_wood_properties(config)
            }
          }

          wood_properties(config)

          notify_success(session, "Success!", "Wood properties loaded successfully")
        }, error = function(e) {
          notify_error(session, "Error loading uploaded YAML:", e$message)
          wood_properties(NULL)
        })
      }
      # Manual mode handled separately
    })

    # Populate manual entry fields when switching to manual mode with loaded config
    observeEvent(input$wood_mode, {
      # Only update when switching to manual mode
      req(input$wood_mode == "manual")

      # Get current wood properties if any
      config <- isolate(wood_properties())
      if (!is.null(config)) {
        # Update thermal properties
        if (!is.null(config$wood_constants$thermal_diffusivity_default_cm2_s)) {
          updateNumericInput(session, "thermal_diffusivity", value = config$wood_constants$thermal_diffusivity_default_cm2_s)
        }
        if (!is.null(config$wood_constants$K_sap_W_m_K)) {
          updateNumericInput(session, "K_sap_W_m_K", value = config$wood_constants$K_sap_W_m_K)
        }
        if (!is.null(config$wood_constants$c_sap_J_kg_K)) {
          updateNumericInput(session, "c_sap_J_kg_K", value = config$wood_constants$c_sap_J_kg_K)
        }
      if (!is.null(config$wood_constants$rho_sap_kg_m3) && !is.na(config$wood_constants$rho_sap_kg_m3)) {
        updateNumericInput(session, "rho_sap_kg_m3", value = config$wood_constants$rho_sap_kg_m3)
      }
      if (!is.null(config$wood_constants$rho_cell_wall_kg_m3) && !is.na(config$wood_constants$rho_cell_wall_kg_m3)) {
        updateNumericInput(session, "rho_cell_wall_kg_m3", value = config$wood_constants$rho_cell_wall_kg_m3)
      }
      if (!is.null(config$wood_constants$c_dry_wood_J_kg_K) && !is.na(config$wood_constants$c_dry_wood_J_kg_K)) {
        updateNumericInput(session, "c_dry_wood_J_kg_K", value = config$wood_constants$c_dry_wood_J_kg_K)
      }
      if (!is.null(config$wood_constants$rho_sap_kg_m3) && !is.na(config$wood_constants$rho_sap_kg_m3)) {
        updateNumericInput(session, "rho_sap_kg_m3", value = config$wood_constants$rho_sap_kg_m3)
      }
      if (!is.null(config$wood_constants$rho_cell_wall_kg_m3) && !is.na(config$wood_constants$rho_cell_wall_kg_m3)) {
        updateNumericInput(session, "rho_cell_wall_kg_m3", value = config$wood_constants$rho_cell_wall_kg_m3)
      }
      if (!is.null(config$wood_constants$c_dry_wood_J_kg_K) && !is.na(config$wood_constants$c_dry_wood_J_kg_K)) {
        updateNumericInput(session, "c_dry_wood_J_kg_K", value = config$wood_constants$c_dry_wood_J_kg_K)
      }
      if (!is.null(config$wood_constants$rho_sap_kg_m3) && !is.na(config$wood_constants$rho_sap_kg_m3)) {
        updateNumericInput(session, "rho_sap_kg_m3", value = config$wood_constants$rho_sap_kg_m3)
      }
      if (!is.null(config$wood_constants$rho_cell_wall_kg_m3) && !is.na(config$wood_constants$rho_cell_wall_kg_m3)) {
        updateNumericInput(session, "rho_cell_wall_kg_m3", value = config$wood_constants$rho_cell_wall_kg_m3)
      }
      if (!is.null(config$wood_constants$c_dry_wood_J_kg_K) && !is.na(config$wood_constants$c_dry_wood_J_kg_K)) {
        updateNumericInput(session, "c_dry_wood_J_kg_K", value = config$wood_constants$c_dry_wood_J_kg_K)
      }

        # Update physical properties
        if (!is.null(config$wood_property$species)) {
          updateTextInput(session, "species", value = config$wood_property$species)
        }
        if (!is.null(config$wood_property$wood_type)) {
          updateSelectInput(session, "wood_type", selected = config$wood_property$wood_type)
        }
        if (!is.null(config$wood_measurements$density_dry_kg_m3) && !is.na(config$wood_measurements$density_dry_kg_m3)) {
          updateNumericInput(session, "dry_density", value = config$wood_measurements$density_dry_kg_m3)
        }
        if (!is.null(config$wood_measurements$density_fresh_kg_m3) && !is.na(config$wood_measurements$density_fresh_kg_m3)) {
          updateNumericInput(session, "fresh_density", value = config$wood_measurements$density_fresh_kg_m3)
        }
        if (!is.null(config$derived_properties$mc_kg_kg) && !is.na(config$derived_properties$mc_kg_kg)) {
          updateNumericInput(session, "moisture_content", value = config$derived_properties$mc_kg_kg)
        }

        # Update tree measurements
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

        # Update quality thresholds
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
      }
    })

    # Also update manual fields when wood_properties() changes while in manual mode
    observe({
      req(input$wood_mode == "manual")
      config <- wood_properties()
      req(!is.null(config))

      # Update all fields when properties change
      if (!is.null(config$wood_constants$thermal_diffusivity_default_cm2_s) && !is.na(config$wood_constants$thermal_diffusivity_default_cm2_s)) {
        updateNumericInput(session, "thermal_diffusivity", value = config$wood_constants$thermal_diffusivity_default_cm2_s)
      }
      if (!is.null(config$wood_constants$K_sap_W_m_K) && !is.na(config$wood_constants$K_sap_W_m_K)) {
        updateNumericInput(session, "K_sap_W_m_K", value = config$wood_constants$K_sap_W_m_K)
      }
      if (!is.null(config$wood_constants$c_sap_J_kg_K) && !is.na(config$wood_constants$c_sap_J_kg_K)) {
        updateNumericInput(session, "c_sap_J_kg_K", value = config$wood_constants$c_sap_J_kg_K)
      }
      if (!is.null(config$wood_constants$rho_sap_kg_m3) && !is.na(config$wood_constants$rho_sap_kg_m3)) {
        updateNumericInput(session, "rho_sap_kg_m3", value = config$wood_constants$rho_sap_kg_m3)
      }
      if (!is.null(config$wood_constants$rho_cell_wall_kg_m3) && !is.na(config$wood_constants$rho_cell_wall_kg_m3)) {
        updateNumericInput(session, "rho_cell_wall_kg_m3", value = config$wood_constants$rho_cell_wall_kg_m3)
      }
      if (!is.null(config$wood_constants$c_dry_wood_J_kg_K) && !is.na(config$wood_constants$c_dry_wood_J_kg_K)) {
        updateNumericInput(session, "c_dry_wood_J_kg_K", value = config$wood_constants$c_dry_wood_J_kg_K)
      }
      if (!is.null(config$wood_constants$rho_sap_kg_m3) && !is.na(config$wood_constants$rho_sap_kg_m3)) {
        updateNumericInput(session, "rho_sap_kg_m3", value = config$wood_constants$rho_sap_kg_m3)
      }
      if (!is.null(config$wood_constants$rho_cell_wall_kg_m3) && !is.na(config$wood_constants$rho_cell_wall_kg_m3)) {
        updateNumericInput(session, "rho_cell_wall_kg_m3", value = config$wood_constants$rho_cell_wall_kg_m3)
      }
      if (!is.null(config$wood_constants$c_dry_wood_J_kg_K) && !is.na(config$wood_constants$c_dry_wood_J_kg_K)) {
        updateNumericInput(session, "c_dry_wood_J_kg_K", value = config$wood_constants$c_dry_wood_J_kg_K)
      }
      if (!is.null(config$wood_measurements$density_dry_kg_m3) && !is.na(config$wood_measurements$density_dry_kg_m3)) {
        updateNumericInput(session, "dry_density", value = config$wood_measurements$density_dry_kg_m3)
      }
      if (!is.null(config$wood_measurements$density_fresh_kg_m3) && !is.na(config$wood_measurements$density_fresh_kg_m3)) {
        updateNumericInput(session, "fresh_density", value = config$wood_measurements$density_fresh_kg_m3)
      }
      if (!is.null(config$derived_properties$mc_kg_kg) && !is.na(config$derived_properties$mc_kg_kg)) {
        updateNumericInput(session, "moisture_content", value = config$derived_properties$mc_kg_kg)
      }
      if (!is.null(config$wood_property$species)) {
        updateTextInput(session, "species", value = config$wood_property$species)
      }
      if (!is.null(config$wood_property$wood_type)) {
        updateSelectInput(session, "wood_type", selected = config$wood_property$wood_type)
      }

      # Update tree measurements
      if (!is.null(config$tree_measurements)) {
        if (!is.null(config$tree_measurements$dbh) && !is.na(config$tree_measurements$dbh)) {
          updateNumericInput(session, "dbh", value = config$tree_measurements$dbh)
        }
        if (!is.null(config$tree_measurements$bark_thickness) && !is.na(config$tree_measurements$bark_thickness)) {
          updateNumericInput(session, "bark_thickness", value = config$tree_measurements$bark_thickness)
        }
        if (!is.null(config$tree_measurements$sapwood_depth) && !is.na(config$tree_measurements$sapwood_depth)) {
          updateNumericInput(session, "sapwood_depth", value = config$tree_measurements$sapwood_depth)
        }
        if (!is.null(config$tree_measurements$sapwood_area) && !is.na(config$tree_measurements$sapwood_area)) {
          updateNumericInput(session, "sapwood_area", value = config$tree_measurements$sapwood_area)
        }
      }

      # Update quality thresholds
      if (!is.null(config$quality_thresholds)) {
        if (!is.null(config$quality_thresholds$max_velocity_cm_hr) && !is.na(config$quality_thresholds$max_velocity_cm_hr)) {
          updateNumericInput(session, "max_velocity", value = config$quality_thresholds$max_velocity_cm_hr)
        }
        if (!is.null(config$quality_thresholds$min_velocity_cm_hr) && !is.na(config$quality_thresholds$min_velocity_cm_hr)) {
          updateNumericInput(session, "min_velocity", value = config$quality_thresholds$min_velocity_cm_hr)
        }
        if (!is.null(config$quality_thresholds$temperature_range)) {
          updateSliderInput(session, "temp_range", value = config$quality_thresholds$temperature_range)
        }
      }
    })

    # Probe manual entry UI
    output$probe_manual_ui <- renderUI({
      ns <- session$ns

      tagList(
        tabsetPanel(
          id = ns("probe_tabs"),

          # Metadata Tab
          tabPanel(
            "Metadata",
            br(),
            textInput(ns("probe_config_name"), "Configuration Name:",
                     value = "Custom Probe Configuration"),

            textInput(ns("probe_description"), "Description:",
                     value = "User-defined probe configuration"),

            textInput(ns("probe_manufacturer"), "Manufacturer:",
                     value = "ICT"),

            textInput(ns("probe_model"), "Model:",
                     value = "SFM1")
          ),

          # Probe Layout Tab
          tabPanel(
            "Probe Layout",
            br(),
            p(class = "help-text", "Define the spatial arrangement of thermistor sensors relative to the heater."),

            numericInput(ns("heater_position"), "Heater Position (mm):",
                        value = 0, min = 0, max = 10, step = 0.1),

            numericInput(ns("upstream_distance"), "Upstream Distance (mm):",
                        value = 5, min = 1, max = 20, step = 0.5),

            numericInput(ns("downstream_distance"), "Downstream Distance (mm):",
                        value = 5, min = 1, max = 20, step = 0.5),

            hr(),
            h6(strong("Probe Insertion"), style = "margin-top: 15px;"),
            p(class = "help-text", "If probe is not fully inserted (e.g., due to external spacer for thin bark/sapwood), enter spacer thickness."),

            numericInput(ns("spacer_thickness"), "Spacer Thickness (mm):",
                        value = 0, min = 0, max = 10, step = 0.5)
          ),

          # Probe Configuration Tab
          tabPanel(
            "Probe Configuration",
            br(),
            p(class = "help-text", "Physical dimensions and sensor positions."),

            numericInput(ns("probe_diameter"), "Probe Diameter (mm):",
                        value = 1.27, min = 0.5, max = 5, step = 0.01),

            numericInput(ns("probe_length"), "Probe Length (mm):",
                        value = 35, min = 10, max = 100, step = 1),

            numericInput(ns("needle_diameter"), "Needle Diameter (mm):",
                        value = 1.27, min = 0.5, max = 5, step = 0.01),

            numericInput(ns("inner_sensor"), "Inner Sensor (distance from tip, mm):",
                        value = 7.5, min = 1, max = 50, step = 0.5),

            numericInput(ns("outer_sensor"), "Outer Sensor (distance from tip, mm):",
                        value = 22.5, min = 1, max = 50, step = 0.5),

            numericInput(ns("heat_pulse_duration"), "Heat Pulse Duration (s):",
                        value = 2, min = 0.5, max = 10, step = 0.5)
          ),

          # Methods Tab
          tabPanel(
            "Methods",
            br(),
            p(class = "help-text", "Select calculation methods compatible with this probe configuration."),

            h5("Compatible Methods"),
            checkboxGroupInput(ns("compatible_methods"), NULL,
                             choices = c("HRM", "MHR", "Tmax_Coh", "Tmax_Klu", "HRMx"),
                             selected = c("HRM", "MHR", "Tmax_Coh", "Tmax_Klu", "HRMx")),

            br(),
            h5("Recommended Methods"),
            p(class = "help-text", "Select the subset of methods recommended for this configuration."),
            checkboxGroupInput(ns("recommended_methods"), NULL,
                             choices = c("HRM", "MHR", "Tmax_Coh", "Tmax_Klu", "HRMx"),
                             selected = c("HRM", "Tmax_Coh", "MHR"))
          )
        ),

        br(),
        actionButton(ns("apply_probe_manual"), "Apply Manual Configuration", class = "btn-primary")
      )
    })

    # Wood manual entry UI - UPDATED VERSION
    output$wood_manual_ui <- renderUI({
      ns <- session$ns

      tagList(
        tabsetPanel(
          id = ns("wood_tabs"),

          # Wood Measurements Tab (NEW - Method 1 or 2)
          tabPanel(
            "Wood Measurements",
            br(),

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

          # Wood Constants Tab (UPDATED - replaces Thermal Properties)
          tabPanel(
            "Wood Constants",
            br(),
            p(class = "help-text",
              "Physical constants for wood and sap. Defaults from Burgess et al. (2001)."),

            h5("Basic Properties:"),
            textInput(ns("species"), "Species:", value = "unknown"),

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

          # Wound Correction Tab (NEW)
          tabPanel(
            "Wound Correction",
            br(),

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

          # Derived Properties Tab (NEW)
          tabPanel(
            "Derived Properties",
            br(),

            actionButton(ns("calculate_wood_props"),
                        "Calculate Derived Properties",
                        icon = icon("calculator"),
                        class = "btn-success",
                        style = "width: 100%; margin-bottom: 20px;"),

            p(class = "help-text",
              "Calculate wood properties based on your measurements. Requires Method 1 or Method 2 measurements."),

            uiOutput(ns("derived_properties_display"))
          ),

          # Tree Measurements Tab (UNCHANGED)
          tabPanel(
            "Tree Measurements",
            br(),
            p(class = "help-text", "Optional - tree-specific measurements for scaling calculations."),

            numericInput(ns("dbh"), "DBH - Diameter at Breast Height (cm):",
                        value = 20, min = 1, max = 300, step = 0.1),

            numericInput(ns("bark_thickness"), "Bark Thickness (cm):",
                        value = 0.5, min = 0.1, max = 5, step = 0.1),

            numericInput(ns("sapwood_depth"), "Sapwood Depth (cm):",
                        value = 3.0, min = 0.1, max = 50, step = 0.1),

            div(
              id = ns("sapwood_area_container"),
              numericInput(ns("sapwood_area"), "Sapwood Area (cm²):",
                          value = NULL, min = 1, max = 10000, step = 1)
            ),
            tags$style(HTML(sprintf("
              #%s.calculated-value input {
                color: #2196F3 !important;
                font-weight: bold !important;
              }
              #%s.calculated-value label::after {
                content: ' (calculated)';
                color: #2196F3;
                font-weight: normal;
                font-size: 0.9em;
                font-style: italic;
              }
            ", ns("sapwood_area_container"), ns("sapwood_area_container"))))
          ),

          # Quality Thresholds Tab (UNCHANGED)
          tabPanel(
            "Quality Thresholds",
            br(),
            p(class = "help-text", "Set acceptable ranges for quality control."),

            numericInput(ns("max_velocity"), "Maximum Velocity (cm/hr):",
                        value = 200, min = 50, max = 500, step = 10),

            numericInput(ns("min_velocity"), "Minimum Velocity (cm/hr):",
                        value = -50, min = -100, max = 0, step = 5),

            sliderInput(ns("temp_range"), "Temperature Range (°C):",
                       min = -20, max = 80, value = c(-10, 60), step = 1)
          )
        ),

        hr(),
        actionButton(ns("apply_wood_manual"), "Apply Manual Configuration", class = "btn-primary", style = "width: 100%;")
      )
    })

    # Calculate and display derived thermal values
    output$derived_thermal_values <- renderUI({
      # Get current values
      diff <- input$thermal_diffusivity
      cond <- input$thermal_conductivity
      cap <- input$volumetric_heat_capacity

      derived <- list()

      # Calculate missing value if 2 of 3 are provided
      # Formula: diffusivity = conductivity / capacity
      # Therefore: conductivity = diffusivity * capacity
      #            capacity = conductivity / diffusivity

      if (!is.null(diff) && !is.null(cond) && is.null(cap)) {
        # Calculate capacity
        cap_calc <- cond / (diff * 10000)  # Convert diff from cm²/s to m²/s
        derived$volumetric_heat_capacity <- cap_calc
      } else if (!is.null(diff) && is.null(cond) && !is.null(cap)) {
        # Calculate conductivity
        cond_calc <- diff * 10000 * cap  # Convert diff to m²/s
        derived$thermal_conductivity <- cond_calc
      } else if (is.null(diff) && !is.null(cond) && !is.null(cap)) {
        # Calculate diffusivity
        diff_calc <- cond / cap / 10000  # Result in cm²/s
        derived$thermal_diffusivity <- diff_calc
      }

      if (length(derived) > 0) {
        div(
          style = "margin-top: 15px; padding: 10px; background-color: #E3F2FD; border-radius: 3px;",
          p(strong("Derived Values:"), class = "derived-value"),
          tags$ul(
            lapply(names(derived), function(name) {
              tags$li(
                span(paste0(gsub("_", " ", tools::toTitleCase(name)), ":")),
                span(sprintf(" %.4g", derived[[name]]), class = "derived-value")
              )
            })
          )
        )
      }
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
            tags$li("Requires oven access for drying to constant weight"),
            tags$li("Volume measured by water displacement")
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

    # Auto-populate wound dates when temporal tracking is enabled
    observeEvent(input$enable_temporal_wound, {
      if (input$enable_temporal_wound) {
        # Get the current data (either corrected or original)
        data <- heat_pulse_data()

        if (!is.null(data) && !is.null(data$measurements) && nrow(data$measurements) > 0) {
          # Extract first and last dates from measurements
          first_date <- as.Date(min(data$measurements$datetime, na.rm = TRUE))
          last_date <- as.Date(max(data$measurements$datetime, na.rm = TRUE))

          # Update date inputs
          updateDateInput(session, "wound_initial_date", value = first_date)
          updateDateInput(session, "wound_final_date", value = last_date)
        }
      }
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
            tags$td("Total expansion:"),
            tags$td(tags$strong(sprintf("%.2f mm over %d days", wound_expansion, round(days_elapsed))))
          ),
          tags$tr(
            tags$td("Growth rate:"),
            tags$td(tags$strong(sprintf("%.4f mm/day (%.3f mm/year)",
                                       growth_rate_per_day, growth_rate_per_year)))
          )
        )
      )
    })

    # Calculate wood properties when requested
    wood_properties_calculated <- eventReactive(input$calculate_wood_props, {
      req(wood_properties())

      wood <- wood_properties()

      # Populate measurements based on method
      if (input$wood_input_method == "method1") {
        req(input$fresh_weight_g, input$dry_weight_g, input$fresh_volume_cm3)

        wood$wood_measurements$fresh_weight_g <- input$fresh_weight_g
        wood$wood_measurements$dry_weight_g <- input$dry_weight_g
        wood$wood_measurements$fresh_volume_cm3 <- input$fresh_volume_cm3
        wood$wood_measurements$density_dry_kg_m3 <- NULL
        wood$wood_measurements$density_fresh_kg_m3 <- NULL

      } else {
        req(input$density_dry_kg_m3, input$density_fresh_kg_m3)

        wood$wood_measurements$density_dry_kg_m3 <- input$density_dry_kg_m3
        wood$wood_measurements$density_fresh_kg_m3 <- input$density_fresh_kg_m3
        wood$wood_measurements$fresh_weight_g <- NULL
        wood$wood_measurements$dry_weight_g <- NULL
        wood$wood_measurements$fresh_volume_cm3 <- NULL
      }

      # Populate wound correction
      wood$wound_correction$drill_bit_diameter_mm <- input$drill_bit_diameter_mm
      wood$wound_correction$wound_addition_mm <- input$wound_addition_mm

      if (input$enable_temporal_wound &&
          !is.null(input$wound_initial_date) &&
          !is.null(input$wound_final_date) &&
          !is.null(input$wound_final_diameter_mm)) {
        wood$wound_correction$initial_date <- as.character(input$wound_initial_date)
        wood$wound_correction$final_date <- as.character(input$wound_final_date)
        wood$wound_correction$final_diameter_mm <- input$wound_final_diameter_mm
      } else {
        wood$wound_correction$initial_date <- NULL
        wood$wound_correction$final_date <- NULL
        wood$wound_correction$final_diameter_mm <- NULL
      }

      # Calculate derived properties
      tryCatch({
        wood_calc <- sapfluxr::calculate_wood_properties(wood)

        showNotification(
          "Wood properties calculated successfully!",
          type = "message",
          duration = 3
        )

        wood_calc
      }, error = function(e) {
        showNotification(
          paste("Error calculating wood properties:", e$message),
          type = "error",
          duration = 10
        )
        NULL
      })
    })

    # Derived properties display
    output$derived_properties_display <- renderUI({
      # Check if calculate button has been pressed
      if (input$calculate_wood_props == 0) {
        return(div(
          class = "alert alert-info",
          icon("info-circle"),
          " Click 'Calculate Derived Properties' button above to compute wood properties from your measurements."
        ))
      }

      wood_calc <- wood_properties_calculated()

      if (is.null(wood_calc)) {
        return(div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          " Calculation failed. Check error message above and ensure all required measurements are provided."
        ))
      }

      deriv <- wood_calc$derived_properties

      if (is.null(deriv) || all(sapply(deriv, is.null))) {
        return(div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " No derived properties calculated. Ensure you have provided all required measurements."
        ))
      }

      tagList(
        div(class = "alert alert-success",
            icon("check-circle"),
            " Properties calculated successfully"),

        h5("Key Conversion Factors:"),
        tags$table(
          class = "table table-bordered",
          tags$tr(
            tags$td(tags$strong("Thermal Diffusivity Correction (Y):")),
            tags$td(
              if (!is.null(deriv$thermal_diffusivity_correction_factor) &&
                  !is.na(deriv$thermal_diffusivity_correction_factor)) {
                sprintf("%.4f", deriv$thermal_diffusivity_correction_factor)
              } else {
                "1.0 (default)"
              }
            )
          ),
          tags$tr(
            tags$td(tags$strong("Sap Flux Conversion Factor (Z):")),
            tags$td(
              if (!is.null(deriv$sap_flux_conversion_factor) &&
                  !is.na(deriv$sap_flux_conversion_factor)) {
                tags$span(style = "color: green; font-weight: bold;",
                         sprintf("%.4f", deriv$sap_flux_conversion_factor))
              } else {
                HTML("<span class='text-danger'>NA - requires wood measurements</span>")
              }
            )
          )
        ),

        hr(),

        h5("Calculated Properties:"),
        tags$table(
          class = "table table-sm table-striped",
          tags$tr(
            tags$td("Moisture Content:"),
            tags$td(if (!is.null(deriv$mc_kg_kg) && !is.na(deriv$mc_kg_kg)) {
              sprintf("%.4f kg/kg (%.1f%%)", deriv$mc_kg_kg, deriv$mc_kg_kg * 100)
            } else "NA")
          ),
          tags$tr(
            tags$td("Dry Wood Density:"),
            tags$td(if (!is.null(deriv$rho_dw_kg_m3) && !is.na(deriv$rho_dw_kg_m3)) {
              sprintf("%.1f kg/m³", deriv$rho_dw_kg_m3)
            } else "NA")
          ),
          tags$tr(
            tags$td("Fresh Wood Density:"),
            tags$td(if (!is.null(deriv$rho_fw_kg_m3) && !is.na(deriv$rho_fw_kg_m3)) {
              sprintf("%.1f kg/m³", deriv$rho_fw_kg_m3)
            } else "NA")
          ),
          tags$tr(
            tags$td("Specific Gravity:"),
            tags$td(if (!is.null(deriv$specific_gravity) && !is.na(deriv$specific_gravity)) {
              sprintf("%.4f", deriv$specific_gravity)
            } else "NA")
          ),
          tags$tr(
            tags$td("Actual Thermal Diffusivity:"),
            tags$td(if (!is.null(deriv$thermal_diffusivity_actual_cm2_s) &&
                       !is.na(deriv$thermal_diffusivity_actual_cm2_s)) {
              sprintf("%.6f cm²/s", deriv$thermal_diffusivity_actual_cm2_s)
            } else "NA")
          )
        )
      )
    })

    # Calculate and update sapwood area when DBH or sapwood_depth changes
    observe({
      req(input$wood_mode == "manual")

      dbh <- input$dbh
      sapwood_depth <- input$sapwood_depth

      if (!is.null(dbh) && !is.null(sapwood_depth) && dbh > 0 && sapwood_depth > 0) {
        # Calculate sapwood area
        # Outer radius = DBH/2, inner radius = DBH/2 - sapwood_depth
        outer_r <- dbh / 2
        inner_r <- max(0, outer_r - sapwood_depth)
        sapwood_area <- pi * (outer_r^2 - inner_r^2)

        # Update the sapwood_area input field
        updateNumericInput(session, "sapwood_area", value = round(sapwood_area, 1))

        # Add calculated-value class to the container for styling using JavaScript
        session$sendCustomMessage(
          type = "addClass",
          message = list(id = session$ns("sapwood_area_container"), class = "calculated-value")
        )
      } else {
        # Remove calculated-value class if inputs are invalid
        session$sendCustomMessage(
          type = "removeClass",
          message = list(id = session$ns("sapwood_area_container"), class = "calculated-value")
        )
      }
    })

    # Apply manual probe configuration
    observeEvent(input$apply_probe_manual, {
      # Create probe config from manual inputs
      tryCatch({
        # Convert distances from mm to cm for sensor positions
        x_upstream <- input$upstream_distance / 10
        x_downstream <- input$downstream_distance / 10

        # Create sensor positions list
        sensor_positions <- list(
          upstream_inner = -x_upstream,
          downstream_inner = x_downstream,
          upstream_outer = -x_upstream,
          downstream_outer = x_downstream
        )

        # Determine config type
        config_type <- ifelse(
          input$upstream_distance == input$downstream_distance,
          "symmetric",
          "asymmetric"
        )

        # Create yaml_data structure for compatibility with probe_summary
        yaml_data <- list(
          metadata = list(
            config_name = input$probe_config_name,
            description = input$probe_description
          ),
          probe = list(
            heater_position = input$heater_position,
            upstream_distance = input$upstream_distance,
            downstream_distance = input$downstream_distance,
            diameter = input$probe_diameter,
            length = input$probe_length,
            needle_diameter = input$needle_diameter,
            inner_sensor = input$inner_sensor,
            outer_sensor = input$outer_sensor,
            spacer_thickness = input$spacer_thickness,
            manufacturer = input$probe_manufacturer,
            model = input$probe_model,
            heat_pulse_duration = input$heat_pulse_duration
          ),
          methods = list(
            compatible = input$compatible_methods,
            recommended = input$recommended_methods,
            priority_order = input$recommended_methods
          )
        )

        # Create ProbeConfiguration object
        config <- sapfluxr::ProbeConfiguration$new(
          config_name = input$probe_config_name,
          config_type = config_type,
          heater_position = input$heater_position,
          sensor_positions = sensor_positions,
          probe_diameter = input$probe_diameter,
          heat_pulse_duration = input$heat_pulse_duration,
          thermal_diffusivity = NULL,
          compatible_methods = input$compatible_methods,
          method_priorities = input$recommended_methods,
          required_parameters = list(
            x = mean(c(x_upstream, x_downstream)),
            heat_pulse_duration = input$heat_pulse_duration
          ),
          yaml_source = "manual_entry",
          yaml_data = yaml_data
        )

        probe_config(config)

        notify_success(session, "Success!", "Manual probe configuration applied")

      }, error = function(e) {
        notify_error(session, "Error creating configuration:", e$message)
      })
    })

    # Apply manual wood configuration
    observeEvent(input$apply_wood_manual, {
      # Create wood properties from manual inputs
      # This would call sapfluxr's WoodProperties R6 class
      tryCatch({
        # Get all values
        # Create wood properties object with new structure
        config <- sapfluxr::WoodProperties$new(
          config_name = "Custom Manual Entry"
        )

        # Set wood_property (species, wood_type, etc.)
        config$wood_property <- list(
          species = input$species,
          wood_type = input$wood_type,
          temperature = 20,
          comments = "Manual entry via Shiny app"
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

        # Populate wood_measurements based on selected method
        config$wood_measurements <- list()

        if (!is.null(input$wood_input_method) && input$wood_input_method == "method1") {
          # Method 1: Weight & Volume
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
          # Method 2: Dual Density
          if (!is.null(input$density_dry_kg_m3)) {
            config$wood_measurements$density_dry_kg_m3 <- input$density_dry_kg_m3
          }
          if (!is.null(input$density_fresh_kg_m3)) {
            config$wood_measurements$density_fresh_kg_m3 <- input$density_fresh_kg_m3
          }
        }

        # Populate wound_correction
        config$wound_correction <- list(
          drill_bit_diameter_mm = input$drill_bit_diameter_mm,
          wound_addition_mm = input$wound_addition_mm
        )

        if (!is.null(input$enable_temporal_wound) && input$enable_temporal_wound) {
          if (!is.null(input$wound_initial_date)) {
            config$wound_correction$initial_date <- as.character(input$wound_initial_date)
          }
          if (!is.null(input$wound_final_date)) {
            config$wound_correction$final_date <- as.character(input$wound_final_date)
          }
          if (!is.null(input$wound_final_diameter_mm)) {
            config$wound_correction$final_diameter_mm <- input$wound_final_diameter_mm
          }
        }

        # Populate tree_measurements
        config$tree_measurements <- list(
          dbh = input$dbh,
          bark_thickness = input$bark_thickness,
          sapwood_depth = input$sapwood_depth,
          sapwood_area = input$sapwood_area
        )

        # Populate quality_thresholds
        config$quality_thresholds <- list(
          max_velocity_cm_hr = input$max_velocity,
          min_velocity_cm_hr = input$min_velocity,
          temperature_range = input$temp_range
        )

        # Auto-calculate derived properties if measurements are present
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
          # Calculate derived properties
          sapfluxr::calculate_wood_properties(config)
        }

        wood_properties(config)

        # Notify with derived properties status
        if (has_measurements) {
          notify_success(session, "Success!", "Manual wood configuration applied and derived properties calculated")
        } else {
          notify_success(session, "Success!", "Manual wood configuration applied (no measurements provided for derived properties)")
        }

      }, error = function(e) {
        notify_error(session, "Error creating configuration:", e$message)
      })
    })

    # Probe summary
    output$probe_summary <- renderUI({
      config <- probe_config()
      if (is.null(config)) {
        return(p("No configuration loaded", style = "color: #999;"))
      }

      ns <- session$ns

      # Extract upstream and downstream distances from sensor positions (in cm, convert to mm)
      upstream_mm <- abs(config$sensor_positions$upstream_inner * 10)
      downstream_mm <- abs(config$sensor_positions$downstream_inner * 10)

      # Get probe dimensions from yaml_data
      probe_diameter <- if (!is.null(config$yaml_data$probe$diameter)) {
        config$yaml_data$probe$diameter
      } else if (!is.null(config$probe_diameter)) {
        config$probe_diameter
      } else {
        NA
      }

      probe_length <- if (!is.null(config$yaml_data$probe$length)) {
        config$yaml_data$probe$length
      } else {
        NA
      }

      inner_sensor <- if (!is.null(config$yaml_data$probe$inner_sensor)) {
        config$yaml_data$probe$inner_sensor
      } else {
        NA
      }

      outer_sensor <- if (!is.null(config$yaml_data$probe$outer_sensor)) {
        config$yaml_data$probe$outer_sensor
      } else {
        NA
      }

      spacer_thickness <- if (!is.null(config$yaml_data$probe$spacer_thickness)) {
        config$yaml_data$probe$spacer_thickness
      } else {
        0
      }

      heat_pulse_duration <- if (!is.null(config$heat_pulse_duration)) {
        config$heat_pulse_duration
      } else if (!is.null(config$yaml_data$probe$heat_pulse_duration)) {
        config$yaml_data$probe$heat_pulse_duration
      } else {
        NA
      }

      manufacturer <- if (!is.null(config$yaml_data$probe$manufacturer)) {
        config$yaml_data$probe$manufacturer
      } else {
        "Unknown"
      }

      model <- if (!is.null(config$yaml_data$probe$model)) {
        config$yaml_data$probe$model
      } else {
        "Unknown"
      }

      # Get description
      description <- if (!is.null(config$yaml_data$metadata$description)) {
        config$yaml_data$metadata$description
      } else {
        NULL
      }

      # Extract recommended methods from yaml_data or method_priorities
      recommended_methods <- if (!is.null(config$yaml_data$methods$recommended)) {
        paste(config$yaml_data$methods$recommended, collapse = ", ")
      } else if (!is.null(config$method_priorities)) {
        paste(head(config$method_priorities, 3), collapse = ", ")
      } else {
        "Not specified"
      }

      # Extract compatible methods
      compatible_methods <- if (!is.null(config$yaml_data$methods$compatible)) {
        paste(config$yaml_data$methods$compatible, collapse = ", ")
      } else if (!is.null(config$compatible_methods)) {
        paste(config$compatible_methods, collapse = ", ")
      } else {
        "Not specified"
      }

      div(
        style = "background-color: #f9f9f9; padding: 10px; border-radius: 3px;",
        h5(strong(config$config_name), style = "margin-top: 0; margin-bottom: 5px;"),
        if (!is.null(description)) {
          p(style = "font-size: 0.9em; color: #666; margin-bottom: 5px;", description)
        },
        p(style = "font-size: 0.9em; color: #666; margin-bottom: 10px;",
          paste(manufacturer, model, sep = " - ")),

        # Two-column layout using fluidRow
        fluidRow(
          column(
            width = 6,
            h6(strong("Probe Geometry"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              if (!is.na(probe_diameter)) tags$li(paste("Diameter:", probe_diameter, "mm")),
              if (!is.na(probe_length)) tags$li(paste("Length:", probe_length, "mm")),
              tags$li(paste("Upstream:", upstream_mm, "mm")),
              tags$li(paste("Downstream:", downstream_mm, "mm")),
              tags$li(paste("Spacer thickness:", spacer_thickness, "mm"))
            )
          ),
          column(
            width = 6,
            h6(strong("Sensor Configuration"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              if (!is.na(inner_sensor)) tags$li(paste("Inner sensor:", inner_sensor, "mm from tip")),
              if (!is.na(outer_sensor)) tags$li(paste("Outer sensor:", outer_sensor, "mm from tip")),
              if (!is.na(heat_pulse_duration)) tags$li(paste("Pulse duration:", heat_pulse_duration, "s"))
            ),
            h6(strong("Methods"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              tags$li(paste("Compatible:", compatible_methods)),
              tags$li(paste("Recommended:", recommended_methods))
            )
          )
        ),

        hr(),
        downloadButton(
          ns("download_probe_yaml"),
          "Export to YAML",
          icon = icon("download"),
          class = "btn-sm btn-primary",
          style = "width: 100%;"
        )
      )
    })

    # Wood summary
    output$wood_summary <- renderUI({
      config <- wood_properties()
      if (is.null(config)) {
        return(p("No configuration loaded", style = "color: #999;"))
      }

      ns <- session$ns

      div(
        style = "background-color: #f9f9f9; padding: 10px; border-radius: 3px;",
        h5(strong(config$config_name), style = "margin-top: 0;"),

        # Display derived properties status
        {
          has_derived <- !is.null(config$derived_properties) &&
                         !all(sapply(config$derived_properties, is.null))

          if (has_derived) {
            div(
              class = "alert alert-success",
              style = "margin-bottom: 10px;",
              icon("check-circle"),
              tags$strong(" Derived properties calculated"),
              if (!is.null(config$derived_properties$sap_flux_conversion_factor) &&
                  !is.na(config$derived_properties$sap_flux_conversion_factor)) {
                tags$span(style = "margin-left: 10px;",
                         sprintf("(Z factor: %.4f)", config$derived_properties$sap_flux_conversion_factor))
              }
            )
          } else {
            div(
              class = "alert alert-warning",
              style = "margin-bottom: 10px;",
              icon("exclamation-triangle"),
              " Derived properties not calculated - use Manual Entry mode to calculate"
            )
          }
        },

        # Two-column layout using fluidRow
        fluidRow(
          column(
            width = 6,
            h6(strong("Species & Thermal Properties"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              tags$li(paste("Species:", if (!is.null(config$wood_property$species)) config$wood_property$species else "Unknown")),
              tags$li(paste("Wood type:", if (!is.null(config$wood_property$wood_type)) config$wood_property$wood_type else "Unknown")),
              tags$li(paste("Thermal diffusivity:", config$wood_constants$thermal_diffusivity_default_cm2_s, "cm²/s")),
              if (!is.null(config$wood_constants$K_sap_W_m_K)) {
                tags$li(paste("Thermal conductivity:", config$wood_constants$K_sap_W_m_K, "W/(m·K)"))
              },
              if (!is.null(config$wood_constants$c_sap_J_kg_K)) {
                tags$li(paste("Heat capacity:", format(config$wood_constants$c_sap_J_kg_K, scientific = FALSE), "J/(m³·K)"))
              }
            ),

            h6(strong("Physical Properties"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              if (!is.null(config$wood_measurements$density_dry_kg_m3)) {
                tags$li(paste("Dry density:", config$wood_measurements$density_dry_kg_m3, "kg/m³"))
              },
              if (!is.null(config$wood_measurements$density_fresh_kg_m3)) {
                tags$li(paste("Fresh density:", config$wood_measurements$density_fresh_kg_m3, "kg/m³"))
              },
              if (!is.null(config$derived_properties$mc_kg_kg)) {
                tags$li(paste("Moisture content:", sprintf("%.2f%%", config$derived_properties$mc_kg_kg * 100)))
              }
            )
          ),
          column(
            width = 6,
            h6(strong("Tree Measurements"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              if (!is.null(config$tree_measurements$dbh)) {
                tags$li(paste("DBH:", config$tree_measurements$dbh, "cm"))
              },
              if (!is.null(config$tree_measurements$bark_thickness)) {
                tags$li(paste("Bark thickness:", config$tree_measurements$bark_thickness, "cm"))
              },
              if (!is.null(config$tree_measurements$sapwood_depth)) {
                tags$li(paste("Sapwood depth:", config$tree_measurements$sapwood_depth, "cm"))
              },
              if (!is.null(config$tree_measurements$sapwood_area)) {
                tags$li(paste("Sapwood area:", round(config$tree_measurements$sapwood_area, 1), "cm²"))
              }
            ),

            h6(strong("Quality Thresholds"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              if (!is.null(config$quality_thresholds$max_velocity_cm_hr)) {
                tags$li(paste("Max velocity:", config$quality_thresholds$max_velocity_cm_hr, "cm/hr"))
              },
              if (!is.null(config$quality_thresholds$min_velocity_cm_hr)) {
                tags$li(paste("Min velocity:", config$quality_thresholds$min_velocity_cm_hr, "cm/hr"))
              }
            )
          )
        ),

        hr(),
        downloadButton(
          ns("download_wood_yaml"),
          "Export to YAML",
          icon = icon("download"),
          class = "btn-sm btn-primary",
          style = "width: 100%;"
        )
      )
    })

    # Probe Visualisation ----

    # Get current probe config for visualization (includes manual entry)
    current_probe_config <- reactive({
      if (input$probe_mode == "manual") {
        # Return a minimal list structure with values from manual inputs
        # This won't be a full R6 object, so validation function will use fallbacks
        req(input$upstream_distance, input$downstream_distance)

        list(
          sensor_positions = list(
            upstream_inner = -input$upstream_distance / 10,    # Convert mm to cm, negative for upstream
            downstream_inner = input$downstream_distance / 10,
            upstream_outer = -input$upstream_distance / 10,
            downstream_outer = input$downstream_distance / 10
          ),
          yaml_data = list(
            probe = list(
              diameter = if (!is.null(input$probe_diameter)) input$probe_diameter else 1.27,
              length = if (!is.null(input$probe_length)) input$probe_length else 35,
              inner_sensor = if (!is.null(input$inner_sensor)) input$inner_sensor else 7.5,
              outer_sensor = if (!is.null(input$outer_sensor)) input$outer_sensor else 22.5,
              spacer_thickness = if (!is.null(input$spacer_thickness)) input$spacer_thickness else 0
            )
          )
        )
      } else {
        # Use the stored configuration (works for both builtin and upload modes)
        req(probe_config())
        probe_config()
      }
    })

    # Get current wood properties for visualization (includes manual entry)
    current_wood_properties <- reactive({
      if (input$wood_mode == "manual") {
        # Return a minimal list structure with values from manual inputs
        req(input$dbh, input$bark_thickness, input$sapwood_depth)

        list(
          tree_measurements = list(
            dbh = input$dbh,
            bark_thickness = input$bark_thickness,
            sapwood_depth = input$sapwood_depth
          )
        )
      } else {
        # Use the stored configuration
        req(wood_properties())
        wood_properties()
      }
    })

    # Validate probe and tree configuration
    validation_data <- reactive({
      req(current_probe_config(), current_wood_properties())

      validate_probe_tree_config(
        probe_config = current_probe_config(),
        wood_properties = current_wood_properties()
      )
    })

    # Render vertical view
    output$probe_vertical_plot <- renderPlot({
      req(validation_data())

      plot_probe_vertical(validation_data())
    })

    # Render radial view
    output$probe_radial_plot <- renderPlot({
      req(validation_data())

      plot_probe_radial(validation_data())
    })

    # Configuration assessment feedback
    output$config_assessment <- renderUI({
      req(validation_data())

      val <- validation_data()

      # Status indicators for each sensor based on tissue type
      outer_status <- if (val$outer_tissue == "sapwood") {
        div(
          style = "color: green; font-weight: bold; margin: 5px 0;",
          icon("check-circle"), " Outer sensor is in SAPWOOD"
        )
      } else if (val$outer_tissue == "bark") {
        div(
          style = "color: red; font-weight: bold; margin: 5px 0;",
          icon("times-circle"), " Outer sensor is in BARK"
        )
      } else {
        div(
          style = "color: red; font-weight: bold; margin: 5px 0;",
          icon("times-circle"), " Outer sensor is in HEARTWOOD"
        )
      }

      inner_status <- if (val$inner_tissue == "sapwood") {
        div(
          style = "color: green; font-weight: bold; margin: 5px 0;",
          icon("check-circle"), " Inner sensor is in SAPWOOD"
        )
      } else if (val$inner_tissue == "bark") {
        div(
          style = "color: red; font-weight: bold; margin: 5px 0;",
          icon("times-circle"), " Inner sensor is in BARK"
        )
      } else {
        div(
          style = "color: red; font-weight: bold; margin: 5px 0;",
          icon("times-circle"), " Inner sensor is in HEARTWOOD"
        )
      }

      # Summary information
      div(
        style = "background-color: #f9f9f9; padding: 15px; border-radius: 5px;",
        h4("Configuration Assessment", style = "margin-top: 0;"),

        div(
          style = "margin: 10px 0;",
          outer_status,
          inner_status
        ),

        hr(),

        div(
          style = "font-size: 0.9em;",
          tags$ul(
            tags$li(paste("Tree radius:", round(val$radius, 2), "cm")),
            tags$li(paste("Bark depth:", round(val$bark_depth, 2), "cm")),
            tags$li(paste("Sapwood depth:", round(val$sapwood_depth, 2), "cm")),
            tags$li(paste("Sapwood/heartwood boundary:", round(val$sapwood_boundary, 2), "cm from bark surface")),
            tags$li(paste("Outer sensor depth:", round(val$outer_sensor_depth, 2), "cm from bark surface")),
            tags$li(paste("Inner sensor depth:", round(val$inner_sensor_depth, 2), "cm from bark surface"))
          )
        )
      )
    })

    # Download handlers ----

    # Download probe configuration as YAML
    output$download_probe_yaml <- downloadHandler(
      filename = function() {
        "my_probe_config.yaml"
      },
      content = function(file) {
        config <- probe_config()
        if (is.null(config)) {
          stop("No probe configuration to export")
        }

        # Use yaml_data if available (from loaded YAML or manual entry)
        if (!is.null(config$yaml_data)) {
          yaml_content <- config$yaml_data
        } else {
          # Construct YAML structure from config object
          yaml_content <- list(
            metadata = list(
              config_name = config$config_name,
              description = paste("Exported on", Sys.Date()),
              created_date = as.character(Sys.Date()),
              version = "1.0"
            ),
            probe = list(
              heater_position = 0,
              upstream_distance = abs(config$sensor_positions$upstream_inner * 10),  # Convert cm to mm
              downstream_distance = abs(config$sensor_positions$downstream_inner * 10),
              diameter = config$probe_diameter,
              spacer_thickness = 0,  # Default to 0 if not specified
              heat_pulse_duration = config$heat_pulse_duration,
              manufacturer = "Custom",
              model = "Custom"
            ),
            methods = list(
              compatible = config$compatible_methods,
              recommended = config$method_priorities,
              priority_order = config$method_priorities
            )
          )
        }

        # Write to YAML
        yaml::write_yaml(yaml_content, file)

        notify_success(
          session = session,
          title = "Export Complete",
          text = "Probe configuration exported successfully"
        )
      }
    )

    # Download wood properties as YAML
    output$download_wood_yaml <- downloadHandler(
      filename = function() {
        # Try to get filename from heat_pulse_data
        if (!is.null(heat_pulse_data) && !is.null(heat_pulse_data())) {
          data <- heat_pulse_data()
          if (!is.null(data$metadata$file_name)) {
            # Extract base filename without extension
            base_name <- tools::file_path_sans_ext(data$metadata$file_name)
            return(paste0(base_name, ".yaml"))
          }
        }
        # Fallback to generic name
        "my_wood_properties.yaml"
      },
      content = function(file) {
        config <- wood_properties()
        if (is.null(config)) {
          stop("No wood properties to export")
        }

        # Construct YAML structure
        yaml_content <- list(
          metadata = list(
            config_name = config$config_name,
            description = paste("Exported on", Sys.Date()),
            created_date = as.character(Sys.Date()),
            version = "1.0"
          ),
          species = list(
            common_name = if (!is.null(config$wood_property$species)) config$wood_property$species else "Unknown",
            wood_type = if (!is.null(config$wood_property$wood_type)) config$wood_property$wood_type else "unknown"
          ),
          thermal_properties = list(
            thermal_diffusivity = list(
              value = config$wood_constants$thermal_diffusivity_default_cm2_s,
              units = "cm²/s"
            ),
            thermal_conductivity = if (!is.null(config$wood_constants$K_sap_W_m_K)) {
              list(value = config$wood_constants$K_sap_W_m_K, units = "W/(m·K)")
            } else NULL,
            volumetric_heat_capacity = if (!is.null(config$wood_constants$c_sap_J_kg_K)) {
              list(value = config$wood_constants$c_sap_J_kg_K, units = "J/(m³·K)")
            } else NULL
          ),
          physical_properties = list(
            dry_density = if (!is.null(config$wood_measurements$density_dry_kg_m3)) {
              list(value = config$wood_measurements$density_dry_kg_m3, units = "kg/m³")
            } else NULL,
            fresh_density = if (!is.null(config$wood_measurements$density_fresh_kg_m3)) {
              list(value = config$wood_measurements$density_fresh_kg_m3, units = "kg/m³")
            } else NULL,
            moisture_content = if (!is.null(config$derived_properties$mc_kg_kg)) {
              list(value = config$derived_properties$mc_kg_kg, units = "fraction")
            } else NULL
          ),
          tree_measurements = if (!is.null(config$tree_measurements)) {
            list(
              dbh = if (!is.null(config$tree_measurements$dbh)) {
                list(value = config$tree_measurements$dbh, units = "cm")
              } else NULL,
              bark_thickness = if (!is.null(config$tree_measurements$bark_thickness)) {
                list(value = config$tree_measurements$bark_thickness, units = "cm")
              } else NULL,
              sapwood_depth = if (!is.null(config$tree_measurements$sapwood_depth)) {
                list(value = config$tree_measurements$sapwood_depth, units = "cm")
              } else NULL,
              sapwood_area = if (!is.null(config$tree_measurements$sapwood_area)) {
                list(value = config$tree_measurements$sapwood_area, units = "cm²")
              } else NULL
            )
          } else NULL,
          quality_thresholds = if (!is.null(config$quality_thresholds)) {
            list(
              velocity = list(
                max_cm_hr = config$quality_thresholds$max_velocity_cm_hr,
                min_cm_hr = config$quality_thresholds$min_velocity_cm_hr
              ),
              temperature = if (!is.null(config$quality_thresholds$temperature_range)) {
                list(
                  min_degC = config$quality_thresholds$temperature_range[1],
                  max_degC = config$quality_thresholds$temperature_range[2]
                )
              } else NULL
            )
          } else NULL
        )

        # Remove NULL entries
        yaml_content <- remove_nulls(yaml_content)

        # Write to YAML
        yaml::write_yaml(yaml_content, file)

        notify_success(
          session = session,
          title = "Export Complete",
          text = "Wood properties exported successfully"
        )
      }
    )

    # Helper function to remove NULL entries recursively
    remove_nulls <- function(x) {
      if (is.list(x)) {
        x <- x[!sapply(x, is.null)]
        lapply(x, remove_nulls)
      } else {
        x
      }
    }

    # Return reactive configurations
    return(list(
      probe_config = probe_config,
      wood_properties = wood_properties
    ))
  })
}
