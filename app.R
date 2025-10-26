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

# Load sapfluxr package
library(sapfluxr)

# Source modules
source("R/notify_helper.R")
source("R/mod_data_upload.R")
source("R/mod_clock_drift.R")
source("R/mod_config.R")
source("R/mod_methods.R")
source("R/mod_plot_timeseries.R")
source("R/utils.R")

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

# UI ----
ui <- dashboardPage(
  skin = "blue",

  ## Header ----
  dashboardHeader(
    title = tags$a(
      href = "https://github.com/neez777/sapfluxr",
      target = "_blank",
      tags$img(src = "sapfluxR.png", height = "40px", style = "margin-top: -10px; margin-right: 10px;"),
      "Sap Flow Analyser"
    ),
    titleWidth = 300
  ),

  ## Sidebar ----
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",

      # Step 1: Data Upload
      menuItem("1. Data Upload",
               tabName = "upload",
               icon = icon("upload")),

      # Step 2: Configuration
      menuItem("2. Configuration",
               tabName = "config",
               icon = icon("cog")),

      # Step 3: Calculations
      menuItem("3. Calculations",
               tabName = "methods",
               icon = icon("calculator")),

      # Step 4: Visualise
      menuItem("4. Visualise",
               tabName = "visualise",
               icon = icon("chart-line")),

      # Step 5: Export
      menuItem("5. Export",
               tabName = "export",
               icon = icon("download"))
    ),

    # App info in sidebar
    hr(),
    div(style = "padding: 15px; font-size: 0.8em; color: #666;",
        p(strong("About")),
        p("Interactive interface for processing heat pulse velocity data from ICT SFM1x sensors."),
        p("Built on ", code("sapfluxr"), " package."),
        hr(),
        p(strong("Version:"), " 0.1.0"),
        p(a("Report Issues",
            href = "https://github.com/neez777/sapfluxr/issues",
            target = "_blank"))
    )
  ),

  ## Body ----
  dashboardBody(

    # Initialize waiter
    waiter::use_waiter(),

    # Load custom CSS and favicon
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "icon", type = "image/png", href = "sapfluxR.png")
    ),

    tabItems(

      # Tab 1: Data Upload ----
      tabItem(
        tabName = "upload",
        h2("Data Upload"),

        fluidRow(
          box(
            width = 12,
            title = "Upload Heat Pulse Data",
            status = "primary",
            solidHeader = TRUE,

            dataUploadUI("data_upload"),

            hr(),

            h4("Clock Drift Correction (Optional)"),
            clockDriftUI("clock_drift")
          )
        ),

        fluidRow(
          box(
            width = 12,
            title = "Data Summary",
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

      # Tab 4: Visualise ----
      tabItem(
        tabName = "visualise",
        h2("Interactive Visualisation"),

        plotTimeseriesUI("plot_timeseries"),

        fluidRow(
          box(
            width = 12,
            title = "Pulse Trace Viewer",
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,

            p("Pulse trace viewer will be implemented in Session 5."),
            p("Click on time series plot to view individual pulse temperature traces.")
          )
        )
      ),

      # Tab 5: Export ----
      tabItem(
        tabName = "export",
        h2("Export Results"),

        fluidRow(
          box(
            width = 12,
            title = "Download Options",
            status = "primary",
            solidHeader = TRUE,

            p("Export functionality will be implemented in Session 6."),
            p("Export formats:"),
            tags$ul(
              tags$li(strong("Plots:"), " PNG, PDF, TIF (publication quality), HTML (interactive)"),
              tags$li(strong("Data:"), " Results as CSV"),
              tags$li(strong("Configuration:"), " Save as YAML")
            )
          )
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

  # Store results
  observe({
    rv$vh_results <- vh_results()
  })

  # Module: Plot Time Series
  plotTimeseriesServer("plot_timeseries", vh_results)

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
      cat("\nData Completeness:\n")
      cat("  Overall:", round(data$validation$summary$overall_completeness * 100, 1), "%\n")
      if (!is.null(data$validation$summary$data_completeness)) {
        for (sensor in names(data$validation$summary$data_completeness)) {
          cat("  ", sensor, ":", round(data$validation$summary$data_completeness[sensor] * 100, 1), "%\n")
        }
      }
    }

    # Preview measurements (as data.frame to avoid tibble formatting)
    cat("\n=== Measurement Preview ===\n")
    print(as.data.frame(head(data$measurements, 10)), row.names = FALSE)
  })

}

# Run App ----
shinyApp(ui, server)
