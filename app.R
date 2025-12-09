#
# Shiny Sap Flow Analyser
# Interactive interface for heat pulse velocity data analysis
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(yaml)
library(lubridate)
library(ggplot2)
library(waiter)
library(fresh)

# Load sapfluxr package
library(sapfluxr)

# Source modules
source("modules/notify_helper.R")
source("modules/mod_data_upload.R")
source("modules/mod_clock_drift.R")
source("modules/mod_config.R")
source("modules/mod_methods.R")
source("modules/mod_corrections.R")
source("modules/mod_plot_timeseries.R")
source("modules/mod_pulse_trace.R")
source("modules/mod_tool_probe.R")
source("modules/mod_tool_wood.R")
source("modules/utils.R")

# Increase file upload size limit
# Default is 5MB - we need to handle large sap flow data files (100s of MB)
options(shiny.maxRequestSize = 1000 * 1024^2)  # 1000 MB = 1 GB

# Disable fancy tibble printing to avoid Unicode/regex errors on Windows
options(
  pillar.bold = FALSE,
  pillar.subtle = FALSE,
  pillar.neg = FALSE,
  pillar.use_width = FALSE,
  pillar.min_chars = 3,
  crayon.enabled = FALSE,
  cli.unicode = FALSE
)

# Disable progressr handlers to prevent blue toast notifications
if (requireNamespace("progressr", quietly = TRUE)) {
  progressr::handlers("void")
}

# ---- DEFINE SAPFLUXR THEME (Professional/Flatly Look) ----
sapfluxr_theme <- create_theme(
  adminlte_color(
    light_blue = "#6C757D", # Dark Slate (Replaces standard Blue)
    aqua = "#6C757D",       # Teal (Replaces standard Cyan)
    green = "#a0af6f",      # Teal (Replaces standard bright Green)
    red = "#DC3545",        # Muted Red
    yellow = "#78909c"      # Muted Orange
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#263238",    # Dark Slate sidebar
    dark_hover_bg = "#37474f", # Teal highlight on hover
    dark_color = "#E4E6EB"  # Light grey text
  ),
  adminlte_global(
    content_bg = "#F8F9FA",
    box_bg = "#E4E6EB",
    info_box_bg = "#ffffff"
  )
)

# UI ----
ui <- tagList(
  tags$head(
    tags$title("sapfluxr Dashboard")  # This sets the browser tab title
  ),
  tags$footer(
    id = "fixed-logo-footer",
    tags$div(
      style = "padding: 5px; text-align: center;",
      tags$a(href = "https://github.com/neez777/sapfluxr", target = "_blank",
             tags$img(src = "sapfluxr.png", height = "60px", style = "margin: 5px;")),
      tags$a(href = "https://github.com/neez777/shiny-sapfluxr", target = "_blank",
             tags$img(src = "shiny_sapfluxr.png", height = "60px", style = "margin: 5px;"))
    )
  ),
  dashboardPage(
#  skin = "green",

  ## Header ----
  dashboardHeader(
    title = tags$a(
      href = "https://github.com/neez777/sapfluxr",
      # target = "_blank",
      # tags$img(src = "sapfluxr.png", height = "40px", style = "margin-top: -5px; margin-right: 5px;"),
      "sapfluxr Dashboard"
    ),
    titleWidth = 250
  ),

  ## Sidebar ----
  dashboardSidebar(
    width = 250,
    tags$div(
      class = "sidebar-content-wrapper", # Class for the scrollable part (you can remove this class if you like)

      sidebarMenu(
        id = "sidebar",
        menuItem("1. Data Upload", tabName = "upload", icon = icon("upload")),
        menuItem("2. Configuration", tabName = "config", icon = icon("cog")),
        menuItem("3. Calculations", tabName = "methods", icon = icon("calculator")),
        menuItem("4. Visualise (Raw HPV)", tabName = "visualise_raw", icon = icon("chart-line")),
        menuItem("5. Corrections", icon = icon("adjust"),
          menuSubItem("Spacing Correction", tabName = "corrections", icon = icon("ruler-horizontal")),
          menuSubItem("Wound Correction", tabName = "wound_correction", icon = icon("bandage"))
        ),
        menuItem("6. Visualise (Corrected)", tabName = "visualise_corrected", icon = icon("chart-area")),
        tags$hr(style = "margin: 10px 0; border-color: #555;"),
        menuItem("Tools", icon = icon("wrench"),
          menuSubItem("Probe Configuration", tabName = "tool_probe", icon = icon("ruler")),
          menuSubItem("Wood Properties", tabName = "tool_wood", icon = icon("tree"))
        ),
        tags$hr(style = "margin: 10px 0; border-color: #555;")
      ),
      div(style = "padding: 0 15px 15px 15px; font-size: 0.8em; color: #666;",
          p(strong("About")),
          p("Interactive interface for processing heat pulse velocity data from ICT SFM1x sensors."),
          p("Built on ", code("sapfluxr"), " package."),
          hr(),
          p(strong("Version:"), " 0.1.0"),
          p(a("Report Issues",
              href = "https://github.com/neez777/sapfluxr/issues",
              target = "_blank"))
      )
    )
  ),

  ## Body ----
  dashboardBody(

    use_theme(sapfluxr_theme),

    # Initialize waiter
    waiter::use_waiter(),

    # Load custom CSS and favicon
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "icon", type = "image/png", href = "sapfluxr.png"),

      # Background Image CSS (Inlined for Reverse Proxy Compatibility)
      tags$style(HTML(
        ".content-wrapper {
          position: relative;
          z-index: 0;
        }
        .content-wrapper::before {
          content: \"\";
          position: fixed;
          bottom: 0px;
          right: 0px;
          width: 702px;
          height: 473px;
          background-image: url('sapfluxr_bg.png');
          background-repeat: no-repeat;
          background-position: bottom right;
          background-size: contain;
          opacity: 0.10;
          z-index: 0;
          pointer-events: none;
        }
      ")),

      # JavaScript to auto-close Shiny notifications (blue toasts) after 5 seconds
      tags$script(HTML(
        "// Simple approach: every 500ms, check for and auto-close old notifications
        setInterval(function() {
          // Find all shiny notification elements
          var notifications = document.querySelectorAll('.shiny-notification');

          notifications.forEach(function(notification) {
            // Skip if this is a progress notification (has a progress bar inside)
            if (notification.querySelector('.shiny-progress') ||
                notification.querySelector('.progress')) {
              return; // Don't auto-close progress bars
            }

            // If this notification doesn't have a timestamp, add one
            if (!notification.dataset.addedAt) {
              notification.dataset.addedAt = Date.now();
              console.log('New notification found, will close in 5 seconds');
            } else {
              // Check if it's been 5 seconds since it was added
              var age = Date.now() - parseInt(notification.dataset.addedAt);
              if (age >= 5000) {
                console.log('Removing notification after 5 seconds');
                $(notification).fadeOut(300, function() {
                  $(this).remove();
                });
              }
            }
          });
        }, 500);

        console.log('Notification auto-close script loaded (polling method)');
      "))
    ),

    tabItems(

      # Tab 1: Data Upload ----
      tabItem(
        tabName = "upload",
        h2("Data Upload"),

        fluidRow(
          column(
            width = 7,
            box(
              width = NULL,
              title = "Upload Heat Pulse Data",
              status = "primary",
              solidHeader = TRUE,

              dataUploadUI("data_upload")
            ),
            box(
              width = NULL,
              title = "Upload Weather Data (Optional)",
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,

              p(class = "help-text",
                "Weather data upload functionality will be implemented soon."),
              p(class = "text-muted",
                "This will allow you to upload meteorological data (temperature, VPD, solar radiation) ",
                "for correlation analysis with sap flow measurements.")
            )
          ),
          column(
            width = 5,
            box(
              width = NULL,
              title = "Clock Drift Correction (Optional)",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,

              clockDriftUI("clock_drift")
            ),
            box(
              width = NULL,
              title = "Trim Incomplete Days (Optional)",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,

              dataTrimUI("clock_drift")
            )
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Data Summary",
            solidHeader = TRUE,
            status = "info",

            verbatimTextOutput("data_summary")
          )
        )
      ),

      # Tab 2: Configuration ----
      tabItem(
        tabName = "config",
        h2("Probe & Wood Configuration"),

        configUI("config")
      ),

      # Tab 3: Calculations ----
      tabItem(
        tabName = "methods",
        h2("Heat Pulse Velocity Calculations"),
        methodsUI("methods")
      ),

      # Tab 4: Visualise Raw ----
      tabItem(
        tabName = "visualise_raw",
        h2("Interactive Visualisation - Raw (Uncorrected) HPV"),
        p(class = "text-muted", "View raw heat pulse velocity calculations to identify outliers, missing data, and quality issues before applying corrections."),

        plotTimeseriesUI("plot_timeseries_raw"),

        hr(),

        pulseTraceUI("pulse_trace_raw")
      ),

      # Tab 5: Corrections ----
      tabItem(
        tabName = "corrections",
        h2("Spacing Correction & Thermal Diffusivity Calibration"),

        correctionsUI("corrections")
      ),

      # Tab 6: Visualise Corrected ----
      tabItem(
        tabName = "visualise_corrected",
        h2("Interactive Visualisation - Corrected HPV"),
        p(class = "text-muted", "View corrected heat pulse velocity data after applying spacing corrections and/or wound corrections."),

        plotTimeseriesUI("plot_timeseries_corrected"),

        hr(),

        pulseTraceUI("pulse_trace_corrected")
      ),

      # Tool: Probe Configuration ----
      tabItem(
        tabName = "tool_probe",
        h2("Probe Configuration Builder"),
        p(class = "text-muted", "Create or edit probe configuration YAML files for use in sap flow analysis workflows."),

        toolProbeUI("tool_probe")
      ),

      # Tool: Wood Properties ----
      tabItem(
        tabName = "tool_wood",
        h2("Wood Properties Builder"),
        p(class = "text-muted", "Create or edit wood properties YAML files for use in sap flow analysis workflows."),

        toolWoodUI("tool_wood")
      )
    )
  )
)
)

# Server ----
server <- function(input, output, session) {

  # Reactive values to store data across modules
  rv <- reactiveValues(
    heat_pulse_data = NULL,
    corrected_data = NULL,
    probe_config = NULL,
    wood_properties = NULL,
    selected_methods = NULL,
    vh_results = NULL
  )

  # Module: Data Upload
  uploaded_data <- dataUploadServer("data_upload")

  # Store uploaded data
  observe({
    req(uploaded_data())
    rv$heat_pulse_data <- uploaded_data()
  })

  # Module: Clock Drift Correction
  corrected_data <- clockDriftServer("clock_drift", uploaded_data)

  # Store corrected data (or use original if no correction)
  observe({
    if (!is.null(corrected_data())) {
      rv$corrected_data <- corrected_data()
    } else if (!is.null(rv$heat_pulse_data)) {
      rv$corrected_data <- rv$heat_pulse_data
    }
  })

  # Module: Configuration
  configs <- configServer("config", reactive(rv$corrected_data))

  # Store configurations
  observe({
    rv$probe_config <- configs$probe_config()
  })

  observe({
    rv$wood_properties <- configs$wood_properties()
  })

  # Module: Methods & Calculation
  vh_results <- methodsServer(
    "methods",
    heat_pulse_data = reactive(rv$corrected_data),
    probe_config = configs$probe_config,
    wood_properties = configs$wood_properties
  )

  # Store results and pre-compute splits for visualization
  observe({
    req(vh_results())

    cat("\n")
    cat("=======================================================================\n")
    cat("PRE-COMPUTING DATA SPLITS FOR VISUALIZATION\n")
    cat("=======================================================================\n")

    rv$vh_results <- vh_results()
    vh_data <- vh_results()

    # Pre-split data by method × sensor_position for faster Tab 4 rendering
    # This happens in the background while user reviews results on Tab 3
    timing <- system.time({
      # Create splits
      splits <- vh_data %>%
        group_by(method, sensor_position) %>%
        group_split(.keep = TRUE)

      # Create lookup table for quick access
      lookup <- vh_data %>%
        group_by(method, sensor_position) %>%
        group_keys()

      # Store both
      rv$vh_splits <- splits
      rv$vh_lookup <- lookup
    })

    cat(sprintf("Created %d data splits in %.3f seconds\n", length(splits), timing["elapsed"]))
    cat("\nSplits breakdown:\n")
    for (i in seq_along(splits)) {
      method <- lookup$method[i]
      sensor <- lookup$sensor_position[i]
      n_rows <- nrow(splits[[i]])
      cat(sprintf("  %s × %s: %s rows\n",
                  method, sensor, format(n_rows, big.mark = ",")))
    }
    cat("=======================================================================\n\n")
    cat("Data ready for visualization! Navigate to Tab 4 when ready.\n\n")
  })

  # Module: Visualise Raw (Tab 4) - Always shows uncorrected data
  selected_pulse_id_raw <- plotTimeseriesServer("plot_timeseries_raw", vh_results)
  pulseTraceServer("pulse_trace_raw", reactive(rv$corrected_data), selected_pulse_id_raw, vh_results)

  # Module: Corrections (Tab 5) - Spacing Correction & k Estimation
  # Store calculation methods for Phase 3 recalculation
  observe({
    req(vh_results())
    rv$calc_methods <- unique(vh_results()$method)
  })

  corrected_vh <- correctionsServer(
    "corrections",
    vh_results = vh_results,
    heat_pulse_data = reactive(rv$corrected_data),
    probe_config = configs$probe_config,
    wood_properties = configs$wood_properties,
    calc_methods = reactive(rv$calc_methods)
  )

  # Module: Visualise Corrected (Tab 6) - Shows corrected data
  selected_pulse_id_corrected <- plotTimeseriesServer("plot_timeseries_corrected", corrected_vh)
  pulseTraceServer("pulse_trace_corrected", reactive(rv$corrected_data), selected_pulse_id_corrected, corrected_vh)

  # Data Summary Output
  output$data_summary <- renderPrint({
    req(rv$corrected_data)

    data <- rv$corrected_data

    cat("=== Heat Pulse Data Summary ===\n\n")

    # Metadata
    cat("File:", data$metadata$file_name, "\n")
    cat("Format:", data$metadata$format, "\n")
    cat("Imported:", format(data$metadata$import_time), "\n")
    cat("Total Pulses:", data$metadata$n_pulses, "\n\n")

    # Date range
    if (nrow(data$measurements) > 0) {
      date_range <- range(data$measurements$datetime, na.rm = TRUE)
      cat("Date Range:", format(date_range[1]), "to", format(date_range[2]), "\n")
      cat("Duration:",
          round(difftime(date_range[2], date_range[1], units = "days"), 1),
          "days\n\n")
    }

    # Validation
    cat("=== Validation Results ===\n")
    cat("Valid:", data$validation$valid, "\n")
    cat("Issues:", length(data$validation$issues), "\n")
    cat("Warnings:", length(data$validation$warnings), "\n")

    if (length(data$validation$warnings) > 0) {
      cat("\nWarnings:\n")
      for (w in data$validation$warnings) {
        cat(" -", w, "\n")
      }
    }

    if (length(data$validation$issues) > 0) {
      cat("\nIssues:\n")
      for (i in data$validation$issues) {
        cat(" -", i, "\n")
      }
    }

    # Show summary if available
    if (!is.null(data$validation$summary)) {
      cat("\n=== Data Quality Summary ===\n")

      # Show pulse completeness if available (accounts for missing pulses)
      if (!is.null(data$validation$summary$pulse_completeness)) {
        cat("Pulse Completeness:", round(data$validation$summary$pulse_completeness * 100, 2), "%\n")
        cat("  Actual pulses:", data$validation$summary$n_actual_pulses, "\n")
        cat("  Expected pulses:", data$validation$summary$n_expected_pulses, "\n")
        cat("  Missing pulses:", data$validation$summary$n_missing_pulses, "\n")
        if (data$validation$summary$n_missing_pulses > 0) {
          cat("  ** ", data$validation$summary$n_missing_pulses, " gap(s) detected in pulse sequence **\n")
        }
      } else {
        cat("Overall Completeness:", round(data$validation$summary$overall_completeness * 100, 2), "%\n")
      }

      if (!is.null(data$validation$summary$data_completeness)) {
        cat("\nSensor Completeness:\n")
        for (sensor in names(data$validation$summary$data_completeness)) {
          cat("  ", toupper(sensor), ":", round(data$validation$summary$data_completeness[sensor] * 100, 2), "%\n")
        }
      }

      cat("\nTotal Records:", format(data$validation$summary$n_measurements, big.mark = ","), "\n")
      cat("============================\n")
    }

    # Preview measurements (as data.frame to avoid tibble formatting)
    cat("\n=== Measurement Preview ===\n")
    print(as.data.frame(head(data$measurements, 10)), row.names = FALSE)
  })

  # Module: Tool - Probe Configuration
  toolProbeServer("tool_probe")

  # Module: Tool - Wood Properties
  toolWoodServer("tool_wood")

}

# Run App ----
shinyApp(ui, server)
