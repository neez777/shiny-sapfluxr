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
          wood_properties(config)
        }, error = function(e) {
          notify_error(session, "Error loading wood properties:", e$message)
        })

      } else if (input$wood_mode == "upload") {
        req(input$wood_yaml_upload)

        tryCatch({
          config <- sapfluxr::load_wood_properties(input$wood_yaml_upload$datapath)
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
        if (!is.null(config$thermal_diffusivity)) {
          updateNumericInput(session, "thermal_diffusivity", value = config$thermal_diffusivity)
        }
        if (!is.null(config$thermal_conductivity)) {
          updateNumericInput(session, "thermal_conductivity", value = config$thermal_conductivity)
        }
        if (!is.null(config$volumetric_heat_capacity)) {
          updateNumericInput(session, "volumetric_heat_capacity", value = config$volumetric_heat_capacity)
        }

        # Update physical properties
        if (!is.null(config$species)) {
          updateTextInput(session, "species", value = config$species)
        }
        if (!is.null(config$wood_type)) {
          updateSelectInput(session, "wood_type", selected = config$wood_type)
        }
        if (!is.null(config$dry_density) && !is.na(config$dry_density)) {
          updateNumericInput(session, "dry_density", value = config$dry_density)
        }
        if (!is.null(config$fresh_density) && !is.na(config$fresh_density)) {
          updateNumericInput(session, "fresh_density", value = config$fresh_density)
        }
        if (!is.null(config$moisture_content) && !is.na(config$moisture_content)) {
          updateNumericInput(session, "moisture_content", value = config$moisture_content)
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
      if (!is.null(config$thermal_diffusivity) && !is.na(config$thermal_diffusivity)) {
        updateNumericInput(session, "thermal_diffusivity", value = config$thermal_diffusivity)
      }
      if (!is.null(config$thermal_conductivity) && !is.na(config$thermal_conductivity)) {
        updateNumericInput(session, "thermal_conductivity", value = config$thermal_conductivity)
      }
      if (!is.null(config$volumetric_heat_capacity) && !is.na(config$volumetric_heat_capacity)) {
        updateNumericInput(session, "volumetric_heat_capacity", value = config$volumetric_heat_capacity)
      }
      if (!is.null(config$dry_density) && !is.na(config$dry_density)) {
        updateNumericInput(session, "dry_density", value = config$dry_density)
      }
      if (!is.null(config$fresh_density) && !is.na(config$fresh_density)) {
        updateNumericInput(session, "fresh_density", value = config$fresh_density)
      }
      if (!is.null(config$moisture_content) && !is.na(config$moisture_content)) {
        updateNumericInput(session, "moisture_content", value = config$moisture_content)
      }
      if (!is.null(config$species)) {
        updateTextInput(session, "species", value = config$species)
      }
      if (!is.null(config$wood_type)) {
        updateSelectInput(session, "wood_type", selected = config$wood_type)
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

    # Wood manual entry UI
    output$wood_manual_ui <- renderUI({
      ns <- session$ns

      tagList(
        tabsetPanel(
          id = ns("wood_tabs"),

          # Thermal Properties Tab
          tabPanel(
            "Thermal Properties",
            br(),
            p(class = "help-text", "Enter at least thermal diffusivity. Other values can be calculated if 2 of 3 are provided."),

            numericInput(ns("thermal_diffusivity"), "Thermal Diffusivity (cm²/s):",
                        value = 0.0025, min = 0.001, max = 0.01, step = 0.0001),

            numericInput(ns("thermal_conductivity"), "Thermal Conductivity (W/(m·K)):",
                        value = NULL, min = 0.1, max = 1, step = 0.01),

            numericInput(ns("volumetric_heat_capacity"), "Volumetric Heat Capacity (J/(m³·K)):",
                        value = NULL, min = 1e6, max = 3e6, step = 1e4),

            uiOutput(ns("derived_thermal_values"))
          ),

          # Physical Properties Tab
          tabPanel(
            "Physical Properties",
            br(),
            textInput(ns("species"), "Species:", value = "unknown"),

            selectInput(ns("wood_type"), "Wood Type:",
                       choices = c("Softwood" = "softwood",
                                  "Hardwood" = "hardwood",
                                  "Unknown" = "unknown"),
                       selected = "softwood"),

            numericInput(ns("dry_density"), "Dry Density (kg/m³):",
                        value = 400, min = 200, max = 1000, step = 10),

            numericInput(ns("fresh_density"), "Fresh Density (kg/m³):",
                        value = 350, min = 200, max = 1000, step = 10),

            numericInput(ns("moisture_content"), "Moisture Content (%):",
                        value = 30, min = 0, max = 200, step = 1)
          ),

          # Tree Measurements Tab
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

          # Quality Thresholds Tab
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
        config <- sapfluxr::WoodProperties$new(
          config_name = "Custom Manual Entry",
          thermal_diffusivity = input$thermal_diffusivity,
          thermal_conductivity = input$thermal_conductivity,
          volumetric_heat_capacity = input$volumetric_heat_capacity,
          dry_density = input$dry_density,
          fresh_density = input$fresh_density,
          moisture_content = input$moisture_content,
          species = input$species,
          wood_type = input$wood_type,
          tree_measurements = list(
            dbh = input$dbh,
            sapwood_depth = input$sapwood_depth,
            sapwood_area = input$sapwood_area
          ),
          quality_thresholds = list(
            max_velocity_cm_hr = input$max_velocity,
            min_velocity_cm_hr = input$min_velocity,
            temperature_range = input$temp_range
          )
        )

        wood_properties(config)

        notify_success(session, "Success!", "Manual wood configuration applied")

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

        # Two-column layout using fluidRow
        fluidRow(
          column(
            width = 6,
            h6(strong("Species & Thermal Properties"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              tags$li(paste("Species:", if (!is.null(config$species)) config$species else "Unknown")),
              tags$li(paste("Wood type:", if (!is.null(config$wood_type)) config$wood_type else "Unknown")),
              tags$li(paste("Thermal diffusivity:", config$thermal_diffusivity, "cm²/s")),
              if (!is.null(config$thermal_conductivity)) {
                tags$li(paste("Thermal conductivity:", config$thermal_conductivity, "W/(m·K)"))
              },
              if (!is.null(config$volumetric_heat_capacity)) {
                tags$li(paste("Heat capacity:", format(config$volumetric_heat_capacity, scientific = FALSE), "J/(m³·K)"))
              }
            ),

            h6(strong("Physical Properties"), style = "margin-top: 10px;"),
            tags$ul(
              style = "font-size: 0.9em; padding-left: 20px;",
              if (!is.null(config$dry_density)) {
                tags$li(paste("Dry density:", config$dry_density, "kg/m³"))
              },
              if (!is.null(config$fresh_density)) {
                tags$li(paste("Fresh density:", config$fresh_density, "kg/m³"))
              },
              if (!is.null(config$moisture_content)) {
                tags$li(paste("Moisture:", config$moisture_content, "%"))
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
            common_name = if (!is.null(config$species)) config$species else "Unknown",
            wood_type = if (!is.null(config$wood_type)) config$wood_type else "unknown"
          ),
          thermal_properties = list(
            thermal_diffusivity = list(
              value = config$thermal_diffusivity,
              units = "cm²/s"
            ),
            thermal_conductivity = if (!is.null(config$thermal_conductivity)) {
              list(value = config$thermal_conductivity, units = "W/(m·K)")
            } else NULL,
            volumetric_heat_capacity = if (!is.null(config$volumetric_heat_capacity)) {
              list(value = config$volumetric_heat_capacity, units = "J/(m³·K)")
            } else NULL
          ),
          physical_properties = list(
            dry_density = if (!is.null(config$dry_density)) {
              list(value = config$dry_density, units = "kg/m³")
            } else NULL,
            fresh_density = if (!is.null(config$fresh_density)) {
              list(value = config$fresh_density, units = "kg/m³")
            } else NULL,
            moisture_content = if (!is.null(config$moisture_content)) {
              list(value = config$moisture_content, units = "fraction")
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
