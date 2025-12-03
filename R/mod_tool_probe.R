#' Probe Configuration Tool Module
#'
#' Standalone tool for creating/editing probe configuration YAML files
#'
#' @param id Module ID
#' @return None (standalone page)
#'

# UI ----
toolProbeUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Probe Configuration Builder",
        status = "primary",
        solidHeader = TRUE,

        p(class = "help-text",
          "Create or edit probe configuration YAML files for use in sap flow analysis workflows. ",
          "Upload an existing YAML to edit, or create a new configuration from scratch.")
      )
    ),

    fluidRow(
      box(
        width = 4,
        title = "Load Existing Configuration (Optional)",
        status = "info",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        p(class = "help-text", "Upload an existing probe configuration YAML file to edit."),

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
        title = "Probe Configuration",
        status = "primary",
        solidHeader = TRUE,

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
                        value = 0, min = 0, max = 10, step = 0.5),

            uiOutput(ns("sensor_positions_display"))
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
          "Download your probe configuration as a YAML file."),

        textInput(
          ns("filename"),
          "Filename:",
          value = "custom_probe_config.yaml",
          placeholder = "my_probe_config.yaml"
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
toolProbeServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive to store current configuration
    current_config <- reactiveVal(NULL)

    # Upload YAML and parse
    observeEvent(input$upload_yaml, {
      req(input$upload_yaml)

      tryCatch({
        config <- sapfluxr::load_probe_config(input$upload_yaml$datapath)
        current_config(config)

        # Populate UI fields from loaded config
        # Metadata
        if (!is.null(config$config_name)) {
          updateTextInput(session, "probe_config_name", value = config$config_name)
        }
        if (!is.null(config$yaml_data$metadata$description)) {
          updateTextInput(session, "probe_description", value = config$yaml_data$metadata$description)
        }

        # Probe dimensions
        if (!is.null(config$yaml_data$probe$manufacturer)) {
          updateTextInput(session, "probe_manufacturer", value = config$yaml_data$probe$manufacturer)
        }
        if (!is.null(config$yaml_data$probe$model)) {
          updateTextInput(session, "probe_model", value = config$yaml_data$probe$model)
        }
        if (!is.null(config$yaml_data$probe$diameter)) {
          updateNumericInput(session, "probe_diameter", value = config$yaml_data$probe$diameter)
        }
        if (!is.null(config$yaml_data$probe$length)) {
          updateNumericInput(session, "probe_length", value = config$yaml_data$probe$length)
        }
        if (!is.null(config$yaml_data$probe$needle_diameter)) {
          updateNumericInput(session, "needle_diameter", value = config$yaml_data$probe$needle_diameter)
        }
        if (!is.null(config$yaml_data$probe$inner_sensor)) {
          updateNumericInput(session, "inner_sensor", value = config$yaml_data$probe$inner_sensor)
        }
        if (!is.null(config$yaml_data$probe$outer_sensor)) {
          updateNumericInput(session, "outer_sensor", value = config$yaml_data$probe$outer_sensor)
        }

        # Heat pulse
        if (!is.null(config$heat_pulse_duration)) {
          updateNumericInput(session, "heat_pulse_duration", value = config$heat_pulse_duration)
        }

        # Sensor positions (from yaml_data)
        if (!is.null(config$yaml_data$sensors)) {
          if (!is.null(config$yaml_data$sensors$heater_position)) {
            updateNumericInput(session, "heater_position", value = config$yaml_data$sensors$heater_position)
          }
          if (!is.null(config$yaml_data$sensors$upstream_distance)) {
            updateNumericInput(session, "upstream_distance", value = config$yaml_data$sensors$upstream_distance)
          }
          if (!is.null(config$yaml_data$sensors$downstream_distance)) {
            updateNumericInput(session, "downstream_distance", value = config$yaml_data$sensors$downstream_distance)
          }
          if (!is.null(config$yaml_data$sensors$spacer_thickness)) {
            updateNumericInput(session, "spacer_thickness", value = config$yaml_data$sensors$spacer_thickness)
          }
        }

        # Methods
        if (!is.null(config$compatible_methods)) {
          updateCheckboxGroupInput(session, "compatible_methods", selected = config$compatible_methods)
        }
        if (!is.null(config$method_priorities)) {
          updateCheckboxGroupInput(session, "recommended_methods", selected = config$method_priorities)
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

    # Sensor positions display
    output$sensor_positions_display <- renderUI({
      req(input$upstream_distance, input$downstream_distance)

      # Convert mm to cm for display
      x_upstream <- -input$upstream_distance / 10  # Negative because upstream
      x_downstream <- input$downstream_distance / 10

      # Apply spacer offset if present
      spacer_cm <- input$spacer_thickness / 10
      x_upstream_adj <- x_upstream - spacer_cm
      x_downstream_adj <- x_downstream - spacer_cm

      div(
        class = "alert alert-info",
        style = "margin-top: 15px;",
        tags$strong("Calculated Sensor Positions (cm):"),
        tags$table(
          class = "table table-sm",
          style = "margin-top: 10px; background-color: white;",
          tags$tr(
            tags$td("Upstream (inner):"),
            tags$td(tags$strong(sprintf("%.2f cm", x_upstream)))
          ),
          tags$tr(
            tags$td("Downstream (inner):"),
            tags$td(tags$strong(sprintf("%.2f cm", x_downstream)))
          ),
          if (spacer_cm > 0) {
            tagList(
              tags$tr(
                tags$td(colspan = 2, tags$em("With spacer adjustment:"))
              ),
              tags$tr(
                tags$td("Upstream (adjusted):"),
                tags$td(tags$strong(sprintf("%.2f cm", x_upstream_adj)))
              ),
              tags$tr(
                tags$td("Downstream (adjusted):"),
                tags$td(tags$strong(sprintf("%.2f cm", x_downstream_adj)))
              )
            )
          }
        )
      )
    })

    # Validation summary
    output$validation_summary <- renderUI({
      # Validation checks
      messages <- list()
      status <- "success"

      # Check required fields
      if (!is.null(input$probe_config_name) && input$probe_config_name != "") {
        messages <- c(messages, list(tags$li(icon("check", style = "color: green;"),
                                             " Configuration name specified")))
      } else {
        messages <- c(messages, list(tags$li(icon("exclamation-triangle", style = "color: orange;"),
                                             " Missing configuration name")))
        status <- "warning"
      }

      if (!is.null(input$upstream_distance) && !is.null(input$downstream_distance)) {
        if (abs(input$upstream_distance - input$downstream_distance) < 0.1) {
          messages <- c(messages, list(tags$li(icon("check", style = "color: green;"),
                                               " Symmetrical probe (upstream = downstream)")))
        } else {
          messages <- c(messages, list(tags$li(icon("info-circle", style = "color: blue;"),
                                               " Asymmetrical probe configuration")))
        }
      }

      if (!is.null(input$compatible_methods) && length(input$compatible_methods) > 0) {
        messages <- c(messages, list(tags$li(icon("check", style = "color: green;"),
                                             sprintf(" %d compatible methods selected", length(input$compatible_methods)))))
      } else {
        messages <- c(messages, list(tags$li(icon("exclamation-triangle", style = "color: red;"),
                                             " No compatible methods selected (required!)")))
        status <- "danger"
      }

      if (!is.null(input$recommended_methods) && length(input$recommended_methods) > 0) {
        messages <- c(messages, list(tags$li(icon("check", style = "color: green;"),
                                             sprintf(" %d recommended methods selected", length(input$recommended_methods)))))
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
          # Validate required fields
          if (is.null(input$compatible_methods) || length(input$compatible_methods) == 0) {
            showNotification(
              "Error: At least one compatible method must be selected!",
              type = "error",
              duration = 10
            )
            return(NULL)
          }

          # Calculate sensor positions in cm
          x_upstream <- -input$upstream_distance / 10
          x_downstream <- input$downstream_distance / 10

          # Determine config type
          config_type <- if (abs(input$upstream_distance - input$downstream_distance) < 0.1) {
            "symmetrical"
          } else {
            "asymmetrical"
          }

          # Create sensor positions list
          sensor_positions <- list(
            upstream_inner = x_upstream,
            upstream_outer = x_upstream,  # Same as inner for standard probes
            downstream_inner = x_downstream,
            downstream_outer = x_downstream  # Same as inner for standard probes
          )

          # Create YAML data structure
          yaml_data <- list(
            metadata = list(
              config_name = input$probe_config_name,
              description = input$probe_description,
              config_type = config_type,
              created_date = as.character(Sys.Date()),
              version = "1.0"
            ),
            probe = list(
              manufacturer = input$probe_manufacturer,
              model = input$probe_model,
              diameter = input$probe_diameter,
              length = input$probe_length,
              needle_diameter = input$needle_diameter,
              inner_sensor = input$inner_sensor,
              outer_sensor = input$outer_sensor
            ),
            sensors = list(
              heater_position = input$heater_position,
              upstream_distance = input$upstream_distance,
              downstream_distance = input$downstream_distance,
              spacer_thickness = input$spacer_thickness,
              positions_cm = sensor_positions
            ),
            heat_pulse = list(
              duration = input$heat_pulse_duration
            ),
            methods = list(
              compatible = input$compatible_methods,
              recommended = if (!is.null(input$recommended_methods)) input$recommended_methods else input$compatible_methods,
              priority_order = if (!is.null(input$recommended_methods)) input$recommended_methods else input$compatible_methods
            )
          )

          # Write YAML file directly
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
