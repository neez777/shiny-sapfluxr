#
# Shiny Sap Flow Analyser
# Interactive interface for heat pulse velocity data analysis
#

# This app dir is also an R package (has DESCRIPTION/NAMESPACE), so shiny warns
# that it might auto-source an R/ subdirectory. There is no R/ dir to source, so
# disable the autoload to silence the (false-alarm) "Loading R/ subdirectory" warning.
options(shiny.autoload.r = FALSE)

# ---- STARTUP ENVIRONMENT CHECKS ----
# These run before any library() call so that an unmet requirement reports itself
# clearly, rather than surfacing later as an obscure failure on a single page.
# Both checks exist because of real incidents: the app ran to completion on
# R 4.3.3 but stalled on the zero-flow changepoint page, and a fresh install
# revealed its missing packages one crash at a time.
local({
  min_r <- "4.4.0"
  if (getRversion() < min_r) {
    stop(
      "sapfluxr Shiny requires R >= ", min_r, ", but this is R ", getRversion(), ".\n",
      "The app relies on the base R null-coalescing operator `%||%`, introduced\n",
      "in R 4.4.0. On older R the app starts but fails partway through analysis.\n",
      "Please upgrade R.",
      call. = FALSE
    )
  }

  required <- c(
    "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "shinycssloaders",
    "fresh", "plotly", "DT", "waiter", "leaflet", "htmlwidgets",
    "dplyr", "tidyr", "purrr", "readr", "yaml", "lubridate", "ggplot2",
    "scales", "rlang", "progressr", "suncalc", "R6", "lutz", "zip",
    "sapfluxr"
  )
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0) {
    from_github <- "sapfluxr" %in% missing
    cran <- setdiff(missing, "sapfluxr")
    msg <- paste0(
      "The sapfluxr Shiny app needs ", length(missing),
      " package(s) that are not installed:\n  ",
      paste(missing, collapse = ", "), "\n\nInstall them with:\n"
    )
    if (length(cran) > 0) {
      msg <- paste0(msg, '  install.packages(c("',
                    paste(cran, collapse = '", "'), '"))\n')
    }
    if (from_github) {
      msg <- paste0(msg, '  remotes::install_github("neez777/sapfluxr")\n')
    }
    stop(msg, call. = FALSE)
  }
})

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
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

# Load sapfluxr package (use library() to avoid namespace conflicts with shinydashboard::box)
# During development, install first with: devtools::install("E:/R/project/sapfluxr")
library(sapfluxr)

# Source modules
source("modules/notify_helper.R")
source("modules/mod_1_data_upload.R")
source("modules/mod_util_weather_upload.R")
source("modules/mod_util_clock_drift.R")
source("modules/mod_2_config.R")
source("modules/mod_3_methods.R")
source("modules/mod_5a_corrections_spacing.R")
source("modules/mod_4_plot_timeseries.R")
source("modules/mod_4_pulse_trace.R")
source("modules/mod_tools_probe.R")
source("modules/mod_tools_wood.R")
source("modules/utils.R")
source("modules/mod_5b_corrections_wound.R")
source("modules/mod_6a_calibration.R")
source("modules/mod_6b_calibration_validation.R")
source("modules/mod_7_sdma.R")
source("modules/mod_7b_sdma_validation.R")
source("modules/mod_8_flux_density.R")
source("modules/mod_8b_radial_integration.R")
source("modules/mod_9_aggregation.R")
source("modules/mod_util_code_generation.R")
source("modules/mod_util_data_download.R")
source("modules/mod_tools_settings.R")

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
    titleWidth = 250,

    # Demo mode toggle (top-right). When on, example/sample data controls become
    # visible and the header turns amber (see body.demo-mode rules in custom.css).
    tags$li(
      class = "dropdown",
      style = "padding: 10px 15px;",
      tags$div(
        style = "display: flex; align-items: center; gap: 8px;",
        tags$span(style = "color: #fff; font-weight: 600;", "Demo mode"),
        shinyWidgets::materialSwitch(
          inputId = "demo_mode",
          label = NULL,
          value = FALSE,
          status = "warning",
          inline = TRUE
        )
      )
    )
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
        menuItem("6. Method Calibration", icon = icon("ruler-combined"),
          menuSubItem("Calibration Parameters", tabName = "calibration", icon = icon("sliders-h")),
          menuSubItem("Calibration Validation", tabName = "calibration_validation", icon = icon("check-circle"))
        ),
        menuItem("7. Selectable DMA", icon = icon("exchange-alt"),
          menuSubItem("sDMA Calculation", tabName = "sdma", icon = icon("calculator")),
          menuSubItem("sDMA Validation", tabName = "sdma_validation", icon = icon("chart-line"))
        ),
        menuItem("8. Sap Flux Density", tabName = "flux_density", icon = icon("tint")),
        menuItem("9. Radial Integration", tabName = "radial_integration", icon = icon("tree")),
        menuItem("10. Temporal Aggregation", tabName = "aggregation", icon = icon("chart-bar")),
        tags$hr(style = "margin: 10px 0; border-color: #555;"),
        menuItem("Tools", icon = icon("wrench"),
          menuSubItem("Probe Configuration", tabName = "tool_probe", icon = icon("ruler")),
          menuSubItem("Wood Properties", tabName = "tool_wood", icon = icon("tree")),
          menuSubItem("Code Generator", tabName = "code_generation", icon = icon("code")),
          menuSubItem("Data Export", tabName = "data_export", icon = icon("download"))
        ),
        menuItem("Settings", tabName = "settings", icon = icon("cog")),
        tags$li(
          tags$a(
            href = "#",
            onclick = "Shiny.setInputValue('start_over', Math.random()); return false;",
            icon("redo"),
            tags$span(" Start Over")
          )
        ),
        tags$hr(style = "margin: 10px 0; border-color: #555;")
      ),
      div(style = "padding: 0 15px 15px 15px; font-size: 0.8em; color: #666;",
          p(strong("About")),
          p("Interactive interface for processing heat pulse velocity data from ICT SFM1x sensors."),
          p("Built on ", code("sapfluxr"), " package."),
          hr(),
          # Read the Shiny app version dynamically from the local DESCRIPTION file
          p(strong("shiny-sapfluxr:"), read.dcf("DESCRIPTION", fields = "Version")[1]),
          # Read the loaded sapfluxr package version
          p(strong("sapfluxr:"), as.character(packageVersion("sapfluxr"))),
          p(a("Report Issues",
              href = "https://github.com/neez777/sapfluxr/issues",
              target = "_blank"))
      )
    )
  ),

  ## Body ----
  dashboardBody(

    use_theme(sapfluxr_theme),

    # Initialize shinyjs
    shinyjs::useShinyjs(),

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
      ")),

      # Stage completion tick helper — updates sidebar icons in-place without re-rendering the menu
      tags$script(HTML("
        function updateStageIndicator(tabName, complete) {
          var link = $('.sidebar-menu a[data-value=\"' + tabName + '\"]').first();
          if (!link.length) return;
          link.find('.stage-tick').remove();
          var cls = complete ? 'stage-tick complete' : 'stage-tick incomplete';
          link.append('<span class=\"pull-right-container\"><i class=\"fa fa-check-circle ' + cls + '\"></i></span>');
        }
        Shiny.addCustomMessageHandler('initStageTicks', function(x){});
      ")),

      # Demo mode label — injected next to the sidebar (hamburger) toggle.
      # Visibility is driven purely by the body.demo-mode class (see custom.css).
      tags$script(HTML("
        $(document).on('shiny:connected', function() {
          if ($('.demo-mode-label').length === 0) {
            $('<span class=\"demo-mode-label\">Demo mode enabled</span>')
              .insertAfter('.main-header .sidebar-toggle');
          }
        });
      "))
    ),

    tabItems(

      # Tab 1: Data Upload ----
      tabItem(
        tabName = "upload",
        h2("Data Upload"),

        fluidRow(
          column(
            width = 6,
            box(
              width = NULL,
              title = "Upload Heat Pulse Data",
              status = "primary",
              solidHeader = TRUE,

              dataUploadUI("data_upload")
            )
          ),
          column(
            width = 6,
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
            ),
            box(
              width = NULL,
              title = "Upload Weather Data (Optional)",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = TRUE,

              weatherUploadUI("weather_upload")
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
        h2("Spacing Correction"),

        correctionsUI("corrections")
      ),

      # Tab 5b: Wound Correction ----
      tabItem(
        tabName = "wound_correction",
        h2("Wound Correction"),
        p(class = "text-muted", "Apply wound correction for probe reinstallations."),

        woundCorrectionUI("wound_correction")
      ),

      # Tab 6a: Calibration ----
      tabItem(
        tabName = "calibration",
        h2("Method Calibration"),
        p(class = "text-muted", "Calibrate secondary methods to a primary method using linear regression."),

        calibrationUI("calibration")
      ),

      # Tab 6b: Calibration Validation ----
      tabItem(
        tabName = "calibration_validation",
        h2("Calibration Validation"),
        p(class = "text-muted", "Verify the quality of method calibration by comparing raw vs. calibrated velocities against the HRM baseline."),

        calibrationValidationUI("calibration_validation")
      ),

      # Tab 7a: sDMA Calculation ----
      tabItem(
        tabName = "sdma",
        h2("Selectable DMA (sDMA) Method Switching"),
        p(class = "text-muted", "Apply Selectable Dual Method Approach (sDMA) to switch between calibrated methods based on recalculated Peclet numbers and flow conditions."),

        sdmaUI("sdma")
      ),

      # Tab 7b: sDMA Validation ----
      tabItem(
        tabName = "sdma_validation",
        h2("sDMA Validation - Interactive Time Series"),
        p(class = "text-muted", "Compare HRM baseline, calibrated secondary methods, and sDMA results in an interactive time series plot."),

        sdmaValidationUI("sdma_validation")
      ),

      # Tab 8: Sap Flux Density ----
      tabItem(
        tabName = "flux_density",
        h2("Sap Flux Density (Jv)"),
        p(class = "text-muted", "Convert corrected heat pulse velocity (Vh) to sap flux density (Jv), with an inline time-series plot."),

        fluxDensityUI("flux_density")
      ),

      # Tab 9: Radial Integration ----
      tabItem(
        tabName = "radial_integration",
        h2("Radial Integration — Tree Water Use"),
        p(class = "text-muted", "Integrate sap flux density across the sapwood area for whole-tree water use (Q), with inline hourly and daily plots."),

        radialIntegrationUI("radial_integration")
      ),

      # Tab 10: Temporal Aggregation ----
      tabItem(
        tabName = "aggregation",
        h2("Temporal Aggregation"),
        p(class = "text-muted", "Aggregate flux density data to daily/weekly/hourly summaries for temporal analysis."),

        aggregationUI("aggregation")
      ),

      # Additional: Visualise Corrected ----
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
      ),

      # Tool: Code Generation ----
      tabItem(
        tabName = "code_generation",
        h2("Reproducible Code Generation"),
        p(class = "text-muted", "Generate executable R scripts that reproduce your Shiny analysis workflow using sapfluxr functions."),

        codeGenerationUI("code_generation")
      ),

      # Tool: Data Export ----
      tabItem(
        tabName = "data_export",
        h2("Data Export"),
        p(class = "text-muted", "Download any dataset produced in the pipeline as CSV or R binary (.rds / .rda) at any time."),

        dataDownloadUI("data_export")
      ),

      # Tool: Settings ----
      tabItem(
        tabName = "settings",
        h2("Application Settings"),
        p(class = "text-muted", "Configure global application settings and plot visual styles."),

        settingsUI("settings")
      )
    )
  )
)
)

# Server ----
server <- function(input, output, session) {

  # Initialize code tracker FIRST (other modules will use this)
  code_tracker <- codeGenerationServer("code_generation")

  # Demo mode: toggle the body.demo-mode class, which reveals the example/sample data
  # controls (.demo-only) and the amber header styling. CSS lives in www/custom.css.
  observeEvent(input$demo_mode, {
    shinyjs::toggleClass(
      selector = "body", class = "demo-mode",
      condition = isTRUE(input$demo_mode)
    )
  }, ignoreNULL = FALSE)

  # Pipeline stage order and sidebar tabName mapping
  STAGE_ORDER <- c("upload", "config", "methods", "corrections", "wound",
                   "calibration", "sdma", "flux", "radial", "aggregation")
  STAGE_TAB <- c(
    upload = "upload", config = "config", methods = "methods",
    corrections = "corrections", wound = "wound_correction",
    calibration = "calibration", sdma = "sdma", flux = "flux_density",
    radial = "radial_integration", aggregation = "aggregation"
  )

  # Prevent config tick firing before the user has visited the tab
  config_visited <- reactiveVal(FALSE)
  observeEvent(input$sidebar, {
    if (isTRUE(input$sidebar == "config")) config_visited(TRUE)
  }, ignoreNULL = TRUE)

  # Reactive values to store data across modules
  rv <- reactiveValues(
    heat_pulse_data = NULL,
    corrected_data = NULL,
    probe_config = NULL,
    wood_properties = NULL,
    selected_methods = NULL,
    vh_results = NULL,
    weather_data = NULL,
    weather_vpd = NULL,
    daily_vpd = NULL,
    flux_data = NULL,
    stage_complete = list(
      upload = FALSE, config = FALSE, methods = FALSE, corrections = FALSE,
      wound = FALSE, calibration = FALSE, sdma = FALSE, flux = FALSE,
      radial = FALSE, aggregation = FALSE
    )
  )

  # Marks a stage complete and cascades resets of all downstream ticks + script lines.
  # All rv$stage_complete access is isolated: assigning rv$stage_complete[[x]] reads the
  # list before writing it, so without isolate() every caller would take a reactive
  # dependency on the list AND write to it — an infinite loop where upstream observers
  # reset downstream stages that downstream observers immediately re-assert.
  mark_stage_done <- function(stage) {
    isolate({
      idx <- match(stage, STAGE_ORDER)
      rv$stage_complete[[stage]] <- TRUE
      if (idx < length(STAGE_ORDER)) {
        for (s in STAGE_ORDER[(idx + 1):length(STAGE_ORDER)]) {
          rv$stage_complete[[s]] <- FALSE
        }
      }
    })
    code_tracker$remove_steps_after(stage)
  }

  # Initialize plot settings
  plot_settings_rv <- reactiveVal(NULL)

  # Load plot settings from YAML
  observe({
    config_path <- "inst/configurations/plot_settings.yaml"
    loaded_settings <- NULL
    if (file.exists(config_path)) {
      tryCatch({
        loaded_settings <- yaml::read_yaml(config_path)
      }, error = function(e) {
        message("Error reading plot settings: ", e$message)
      })
    }

    # If loading failed or file doesn't exist, use defaults
    if (is.null(loaded_settings)) {
      default_method <- function(outer, inner) {
        list(outer = outer, inner = inner, raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid")
      }
      
      loaded_settings <- list(
        methods = list(
          HRM = default_method("#1f77b4", "#aec7e8"),
          MHR = default_method("#ff7f0e", "#ffbb78"),
          Tmax_Klu = default_method("#2ca02c", "#98df8a"),
          Tmax_Coh = default_method("#d62728", "#ff9896"),
          sDMA = default_method("#9467bd", "#c5b0d5")
        ),
        special_traces = list(
          vpd = list(
            color = "#000000",
            width = 1.0,
            dash = "dash"
          ),
          peclet = list(
            color = "#666666",
            width = 1.0,
            dash = "dot"
          )
        )
      )
    }
    plot_settings_rv(loaded_settings)
  })

  # Module: Settings (Tab: settings)
  settingsServer("settings", plot_settings_rv)

  # Module: Data Upload
  uploaded_data <- dataUploadServer("data_upload", code_tracker = code_tracker)

  # Store uploaded data
  observe({
    req(uploaded_data())
    rv$heat_pulse_data <- uploaded_data()
  })

  # Module: Weather Upload
  weather_outputs <- weatherUploadServer("weather_upload",
                                         heat_pulse_data = reactive(rv$corrected_data),
                                         code_tracker = code_tracker)

  # Store weather data
  observe({
    rv$weather_data <- weather_outputs$weather_data()
    rv$weather_vpd <- weather_outputs$weather_vpd()
    rv$daily_vpd <- weather_outputs$daily_vpd()
  })

  # Module: Clock Drift Correction
  corrected_data <- clockDriftServer("clock_drift", uploaded_data,
                                     code_tracker = code_tracker)

  # Store corrected data (or use original if no correction)
  observe({
    if (!is.null(corrected_data())) {
      rv$corrected_data <- corrected_data()
    } else if (!is.null(rv$heat_pulse_data)) {
      rv$corrected_data <- rv$heat_pulse_data
    }
  })

  # Module: Configuration
  configs <- configServer("config", reactive(rv$corrected_data), code_tracker = code_tracker)

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
    wood_properties = configs$wood_properties,
    code_tracker = code_tracker
  )

  # Store results for downstream stages and visualisation.
  # (The old per-method×sensor pre-splitting was removed — Tab 4 now updates its
  # plotly traces incrementally, so rv$vh_splits/rv$vh_lookup are no longer needed.)
  observe({
    req(vh_results())
    rv$vh_results <- vh_results()
  })

  # Module: Visualise Raw (Tab 4) - Always shows uncorrected data
  selected_pulse_id_raw <- plotTimeseriesServer("plot_timeseries_raw", vh_results, reactive(rv$daily_vpd), reactive(rv$weather_vpd), plot_settings = plot_settings_rv, rv = rv)
  pulseTraceServer("pulse_trace_raw", reactive(rv$corrected_data), selected_pulse_id_raw, vh_results, plot_settings = plot_settings_rv)

  # Module: Corrections (Tab 5) - Spacing Correction & k Estimation
  # Store calculation methods for Phase 3 recalculation
  observe({
    req(vh_results())
    rv$calc_methods <- unique(vh_results()$method)
  })

  corrections_module <- correctionsServer(
    "corrections",
    vh_results = reactive(rv$vh_results),
    heat_pulse_data = reactive(rv$corrected_data),
    probe_config = configs$probe_config,
    wood_properties = configs$wood_properties,
    calc_methods = reactive(rv$calc_methods),
    daily_vpd = reactive(rv$daily_vpd),
    weather_vpd = reactive(rv$weather_vpd),
    code_tracker = code_tracker,
    plot_settings = plot_settings_rv
  )
  # Passthrough corrected-velocity reactive (original data until a correction is committed)
  corrected_vh <- corrections_module$vh
  # Module: Wound Correction (Tab 5b) - Apply wound corrections
  wound_module <- woundCorrectionServer(
    "wound_correction",
    vh_data = corrected_vh,
    wood_properties = configs$wood_properties,
    probe_config = configs$probe_config,
    code_tracker = code_tracker,
    plot_settings = plot_settings_rv
  )

  # Module: Calibration (Tab 6a)
  calibration_results <- calibrationServer(
    "calibration",
    vh_corrected = reactive({
      wound_data <- wound_module$wound_corrected_data()
      if (!is.null(wound_data)) wound_data else corrected_vh()
    }),
    wood_properties = configs$wood_properties,
    probe_config = configs$probe_config,
    code_tracker = code_tracker,
    active_tab = reactive(input$sidebar),
    wound_module = wound_module  # Pass wound module to detect when correction is applied
  )

  # Module: Calibration Validation (Tab 6b)
  calibrationValidationServer(
    "calibration_validation",
    vh_raw = reactive({
      # Get raw data before calibration
      wound_data <- wound_module$wound_corrected_data()
      if (!is.null(wound_data)) wound_data else corrected_vh()
    }),
    vh_calibrated = calibration_results$vh_transformed,  # Use transformed (long format) data
    weather_data = reactive(rv$weather_data),
    code_tracker = code_tracker,
    plot_settings = plot_settings_rv
  )

  # Module: sDMA (Tab 7a)
  sdma_results <- sdmaServer(
    "sdma",
    vh_calibrated = calibration_results$vh_transformed,  # Use transformed (long format)
    primary_method = calibration_results$primary_method,
    probe_config = reactive(rv$probe_config),
    wood_properties = reactive(rv$wood_properties),
    code_tracker = code_tracker
  )

  # Module: sDMA Validation (Tab 7b)
  sdmaValidationServer(
    "sdma_validation",
    vh_hrm_peclet = sdma_results$vh_with_peclet,
    vh_calibrated = calibration_results$vh_transformed,
    vh_sdma = sdma_results$vh_sdma,
    sdma_threshold = reactive(input[["sdma-peclet_threshold"]] %||% 1.0),
    code_tracker = code_tracker,
    plot_settings = plot_settings_rv
  )

  # Consolidate results for downstream use
  # If sDMA applied, use it. Else if calibration applied, use it. Else use wound/spacing corrected.
  vh_calibrated_sdma <- reactive({
    if (!is.null(sdma_results$vh_sdma())) {
      return(sdma_results$vh_sdma())
    }
    calibration_results$vh_calibrated()
  })

  # Module: Sap Flux Density (Tab 8)
  flux_results <- fluxDensityServer(
    "flux_density",
    vh_raw = reactive(rv$vh_results),
    vh_spacing_corrected = reactive(corrected_vh()),
    vh_wound_corrected = reactive({
      # Use calibrated data if available (but not sDMA), otherwise wound corrected
      if (!is.null(calibration_results$vh_transformed()) && is.null(sdma_results$vh_sdma())) {
        return(calibration_results$vh_transformed())
      }
      wound_module$wound_corrected_data()
    }),
    vh_sdma = reactive(sdma_results$vh_sdma()),
    wood_properties = reactive(rv$wood_properties),
    code_tracker = code_tracker,
    plot_settings = plot_settings_rv
  )

  # Store flux data
  observe({
    rv$flux_data <- flux_results$flux_data()
  })

  # Module: Radial Integration → Tree Water Use (Tab 9)
  radial_results <- radialIntegrationServer(
    "radial_integration",
    flux_data = flux_results$flux_data,
    wood_properties = reactive(rv$wood_properties),
    code_tracker = code_tracker,
    plot_settings = plot_settings_rv
  )

  # Module: Aggregation (Tab 10)
  agg_out <- aggregationServer(
    "aggregation",
    flux_density_data = flux_results$flux_data,
    tree_water_use_data = radial_results$tree_water_use_data,
    code_tracker = code_tracker,
    plot_settings = plot_settings_rv
  )

  # Module: Data Export (Tools) — surfaces every pipeline dataset for download
  # at any time. The reactive is lazy, so unfinished stages simply read NULL and
  # are filtered out by the module's availability logic.
  dataDownloadServer(
    "data_export",
    datasets = reactive({
      list(
        "Raw heat-pulse measurements"    = tryCatch(rv$heat_pulse_data$measurements, error = function(e) NULL),
        "Aligned/corrected measurements" = tryCatch(rv$corrected_data$measurements, error = function(e) NULL),
        "Heat-pulse velocity (Vh)"       = rv$vh_results,
        "Sap flux density (Jv)"          = rv$flux_data,
        "Tree water use (Q)"             = tryCatch(radial_results$tree_water_use_data(), error = function(e) NULL),
        "Temporal aggregation"           = tryCatch(agg_out$aggregated(), error = function(e) NULL),
        "Weather"                        = rv$weather_data
      )
    })
  )

  # ---- Stage completion observers ----
  # Upload uses a plain reactiveValues field — observeEvent works fine here.
  observeEvent(rv$heat_pulse_data, mark_stage_done("upload"), ignoreNULL = TRUE)

  # Config: gated by config_visited so defaults loaded on startup don't auto-tick.
  observe({
    req(config_visited())
    if (!is.null(rv$probe_config) && !is.null(rv$wood_properties)) mark_stage_done("config")
  })

  # All other stages return eventReactives that throw a silent error (not NULL) before
  # their trigger button is clicked. observeEvent + ignoreNULL silently fails in that case,
  # so we use observe + tryCatch to handle the error and re-establish the reactive dependency.
  observe({
    result <- tryCatch(vh_results(), error = function(e) NULL)
    req(!is.null(result))
    mark_stage_done("methods")
  })
  # Corrections ticks only on an actual committed correction (not the passthrough),
  # and reverts to grey if the user resets the correction.
  observe({
    if (isTRUE(corrections_module$applied())) {
      mark_stage_done("corrections")
    } else {
      isolate(rv$stage_complete[["corrections"]] <- FALSE)
    }
  })
  observe({
    result <- tryCatch(wound_module$wound_corrected_data(), error = function(e) NULL)
    req(!is.null(result))
    mark_stage_done("wound")
  })
  observe({
    result <- tryCatch(calibration_results$vh_transformed(), error = function(e) NULL)
    req(!is.null(result))
    mark_stage_done("calibration")
  })
  observe({
    result <- tryCatch(sdma_results$vh_sdma(), error = function(e) NULL)
    req(!is.null(result))
    mark_stage_done("sdma")
  })
  observe({
    result <- tryCatch(flux_results$flux_data(), error = function(e) NULL)
    req(!is.null(result))
    mark_stage_done("flux")
  })
  observe({
    result <- tryCatch(radial_results$tree_water_use_data(), error = function(e) NULL)
    req(!is.null(result))
    mark_stage_done("radial")
  })
  observe({
    result <- tryCatch(agg_out$aggregated(), error = function(e) NULL)
    req(!is.null(result))
    mark_stage_done("aggregation")
  })

  # Push tick state to the sidebar via JS (one observer per stage).
  # Self-guarding: any JS error here is swallowed so it can never abort the
  # surrounding websocket message batch (which would blank out outputs).
  lapply(STAGE_ORDER, function(stg) {
    observe({
      shinyjs::runjs(sprintf(
        "try { if (typeof updateStageIndicator === 'function') { updateStageIndicator('%s', %s); } } catch (e) { console.error('updateStageIndicator failed:', e); }",
        STAGE_TAB[[stg]], tolower(as.character(rv$stage_complete[[stg]]))
      ))
    })
  })

  # ---- Start Over ----
  observeEvent(input$start_over, {
    shinyWidgets::confirmSweetAlert(
      session = session, inputId = "start_over_confirmed",
      title = "Start over?",
      text = "This clears all uploaded data and resets every stage of the pipeline.",
      type = "warning", btn_labels = c("Cancel", "Yes, start over"), danger_mode = TRUE
    )
  }, ignoreNULL = TRUE)
  observeEvent(input$start_over_confirmed, {
    req(isTRUE(input$start_over_confirmed))
    shinyjs::runjs("location.reload();")
  })

  # Fallback data flow: use calibrated data if available, else wound corrected, else spacing corrected
  final_vh <- reactive({
    # Priority 1: Calibrated/sDMA data
    if (!is.null(vh_calibrated_sdma())) {
      return(vh_calibrated_sdma())
    }
    # Priority 2: Wound-corrected data
    wound_data <- wound_module$wound_corrected_data()
    if (!is.null(wound_data)) {
      return(wound_data)
    }
    # Priority 3: Spacing-corrected only
    corrected_vh()
  })

  # Module: Visualise Corrected (Tab 8) - Shows corrected data
  selected_pulse_id_corrected <- plotTimeseriesServer("plot_timeseries_corrected", final_vh, reactive(rv$daily_vpd), reactive(rv$weather_vpd), plot_settings = plot_settings_rv)
  pulseTraceServer("pulse_trace_corrected", reactive(rv$corrected_data), selected_pulse_id_corrected, final_vh, plot_settings = plot_settings_rv)

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
  toolWoodServer("tool_wood", heat_pulse_data = heat_pulse_data)

}

# Run App ----
shinyApp(ui, server)
