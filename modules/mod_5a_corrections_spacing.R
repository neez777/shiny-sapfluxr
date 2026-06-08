# mod_corrections.R
# Module for Changepoint-Based Spacing Correction
#
# Implements segment-based Burgess et al. (2001) spacing correction
# using changepoint detection to identify baseline shifts

# UI ----
correctionsUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # Left column: Configuration and Controls
      column(
        width = 4,

        # Info box
        box(
          width = 12,
          title = "About Changepoint-Based Correction",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          p(class = "text-info", strong("Note:"), " View Tab 4 (Visualise Raw HPV) first to identify data quality before correction."),
          hr(),
          p("This approach detects baseline shifts in daily minimum velocities caused by probe movement (tree swelling/shrinkage)."),
          tags$ul(
            tags$li(strong("Step 1:"), " Detect changepoints that divide data into segments"),
            tags$li(strong("Step 2:"), " Apply separate Burgess corrections per segment"),
            tags$li(strong("Step 3:"), " Review segment-specific results")
          ),
          p(tags$small(em("Based on Burgess et al. (2001) with PELT changepoint detection")))
        ),

        # Heartwood warning (dynamic - shown only if inner sensor is in heartwood)
        uiOutput(ns("heartwood_warning")),

        # Changepoint definition with tabbed interface
        box(
          width = 12,
          title = "Define Zero-Flow Changepoints",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,

          helpText("Changepoints mark dates where probe alignment shifts, dividing data into segments for separate calibration."),

          # Tabbed interface for Manual vs Auto-detect
          tabsetPanel(
            id = ns("changepoint_tabs"),
            type = "tabs",

            # Tab 1: Dual-Criterion (VPD + vh stable) - RECOMMENDED DEFAULT
            tabPanel(
              "Dual-Criterion",
              br(),

              div(class = "alert alert-info",
                icon("star"),
                strong(" Recommended Method"),
                br(),
                "Detects changepoints when ", strong("BOTH"), " VPD ", strong("AND"), " sap flow are stable during pre-dawn periods. This dual-criterion approach eliminates false positives from stem refilling and provides higher confidence in baseline detection."
              ),

              # Check if both weather and vh data are available
              conditionalPanel(
                condition = "output.weather_data_available == false",
                ns = ns,
                div(class = "alert alert-warning",
                  icon("exclamation-triangle"),
                  strong(" Weather data required"),
                  br(),
                  "Please upload weather data in Tab 1 and calculate VPD before using this method."
                )
              ),

              conditionalPanel(
                condition = "output.weather_data_available == true",
                ns = ns,

                h5("VPD Stability Criteria"),

                # VPD Threshold (Mean)
                sliderInput(
                  ns("dual_vpd_threshold"),
                  HTML('Mean VPD Threshold (kPa) <span style="color: #999; cursor: help;" title="Maximum mean VPD during pre-dawn. Lower = more conservative."><i class="fa fa-circle-question"></i></span>'),
                  min = 0.1,
                  max = 1.5,
                  value = 0.5,
                  step = 0.05
                ),

                # VPD Stability Threshold (SD)
                sliderInput(
                  ns("dual_vpd_stability"),
                  HTML('VPD Stability (SD kPa) <span style="color: #999; cursor: help;" title="Maximum standard deviation of VPD. Lower = more stable required."><i class="fa fa-circle-question"></i></span>'),
                  min = 0.01,
                  max = 0.5,
                  value = 0.1,
                  step = 0.01
                ),

                hr(),
                h5("Sap Flow Stability Criteria"),

                # vh Threshold (Mean)
                sliderInput(
                  ns("dual_vh_threshold"),
                  HTML('Mean Sap Flow Threshold (cm/hr) <span style="color: #999; cursor: help;" title="Maximum mean sap flow during pre-dawn. Species-specific: Conifers ~1.0, Hardwoods ~2.5"><i class="fa fa-circle-question"></i></span>'),
                  min = 0.5,
                  max = 5.0,
                  value = 2.0,
                  step = 0.1
                ),

                # vh Stability Threshold (SD)
                sliderInput(
                  ns("dual_vh_stability"),
                  HTML('Sap Flow Stability (SD cm/hr) <span style="color: #999; cursor: help;" title="Maximum standard deviation of sap flow. Lower = flatter baseline required."><i class="fa fa-circle-question"></i></span>'),
                  min = 0.1,
                  max = 2.0,
                  value = 0.5,
                  step = 0.1
                ),

                hr(),
                h5("Detection Settings"),

                # Predawn mode selection (always visible)
                radioButtons(
                  ns("dual_predawn_mode"),
                  HTML('Predawn Mode <span style="color: #999; cursor: help;" title="Static uses fixed clock hours; Dynamic adjusts window relative to astronomical dawn each day."><i class="fa fa-circle-question"></i></span>'),
                  choices = c("Static (fixed clock hours)" = "static",
                              "Dynamic (dawn-relative)"    = "dynamic"),
                  selected = "static",
                  inline = TRUE
                ),

                # Static predawn window
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'static'", ns("dual_predawn_mode")),

                  fluidRow(
                    column(6,
                      selectInput(
                        ns("static_start_hr"),
                        "From:",
                        choices = setNames(0:23, paste0(sprintf("%02d", 0:23), ":00")),
                        selected = "2"
                      )
                    ),
                    column(6,
                      selectInput(
                        ns("static_end_hr"),
                        HTML('To <span style="font-size:0.85em; color:#999;">(exclusive)</span>:'),
                        choices = setNames(0:23, paste0(sprintf("%02d", 0:23), ":00")),
                        selected = "6"
                      )
                    )
                  ),
                  uiOutput(ns("static_predawn_preview"))
                ),

                # Dynamic predawn window
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'dynamic'", ns("dual_predawn_mode")),

                  # Compact lat/lon entry (auto-populated from Tree Properties)
                  p(style = "font-size:0.85em; color:#555; margin-bottom:4px;",
                    icon("map-marker-alt"), " Site coordinates for dawn calculation:"),
                  fluidRow(
                    column(6,
                      numericInput(
                        ns("dyn_latitude"),
                        "Latitude:",
                        value = NA_real_,
                        min = -90, max = 90, step = 0.0001
                      )
                    ),
                    column(6,
                      numericInput(
                        ns("dyn_longitude"),
                        "Longitude:",
                        value = NA_real_,
                        min = -180, max = 180, step = 0.0001
                      )
                    )
                  ),
                  p(style = "font-size:0.8em; color:#999; margin-top:-8px; margin-bottom:8px;",
                    "Edit full location details in Step 2 → Tree Properties."),

                  # Inverted range slider: left = 12 h before dawn, right = 0 (at dawn)
                  sliderInput(
                    ns("dynamic_predawn"),
                    HTML('Hours before astronomical dawn <span style="color: #999; cursor: help;" title="Select how many hours before dawn to start (left) and end (right) the pre-dawn window. E.g. left = −4, right = −1 = 4 h to 1 h before dawn."><i class="fa fa-circle-question"></i></span>'),
                    min   = -12,
                    max   = 0,
                    step  = 0.5,
                    value = c(-4, -1)
                  ),
                  uiOutput(ns("dynamic_predawn_preview"))
                ),

                # Minimum segment days
                numericInput(
                  ns("dual_min_segment_days"),
                  HTML('Min Spacing (days) <span style="color: #999; cursor: help;" title="Minimum days between changepoints."><i class="fa fa-circle-question"></i></span>'),
                  value = 7,
                  min = 1,
                  max = 30
                ),

                # Sensor for vh detection
                selectInput(
                  ns("dual_sensor_position"),
                  HTML('Sensor Position <span style="color: #999; cursor: help;" title="Which sensor to use for sap flow stability detection."><i class="fa fa-circle-question"></i></span>'),
                  choices = c("Outer" = "outer", "Inner" = "inner"),
                  selected = "outer"
                ),

                # Method for vh detection
                selectInput(
                  ns("dual_method_filter"),
                  HTML('Method <span style="color: #999; cursor: help;" title="HPV calculation method to use for sap flow detection."><i class="fa fa-circle-question"></i></span>'),
                  choices = c("HRM" = "HRM", "MHR" = "MHR"),
                  selected = "HRM"
                ),

                br(),

                # Run detection button
                actionButton(
                  ns("detect_dual_stable_changepoints"),
                  "Run Dual-Criterion Detection",
                  icon = icon("check-double"),
                  class = "btn-primary",
                  width = "100%"
                ),

                br(), br(),

                # Results display
                conditionalPanel(
                  condition = sprintf("output['%s']", ns("dual_stable_changepoints_detected")),
                  h5("Detected Dual-Stable Changepoints:"),
                  helpText(icon("info-circle"), " Changepoints where ", strong("BOTH"), " VPD and sap flow are stable. Shown as ", tags$span(style = "color: purple;", "purple dotted lines"), " on plot."),
                  uiOutput(ns("detected_dual_stable_changepoints_list")),
                  br(),
                  actionButton(
                    ns("add_detected_dual_stable_changepoints"),
                    "Add All Dual-Criterion Changepoints",
                    icon = icon("check"),
                    class = "btn-success",
                    width = "100%"
                  )
                )
              )
            ),


            # Tab 2: Auto-Detect (unchanged)
            tabPanel(
              "Auto-Detect",
              br(),

              helpText(
                icon("info-circle"),
                " Automatically detect baseline shifts in daily minimum velocities using PELT changepoint detection."
              ),

              div(
                style = "margin-bottom: 10px;",
                tags$label(
                  "Penalty Type",
                  HTML('<span style="color: #999; cursor: help; margin-left: 5px;" title="MBIC (Modified Bayesian Information Criterion): Most conservative, fewest changepoints. BIC (Bayesian Information Criterion): Moderate number of changepoints. Manual: Set custom penalty value (0-100), higher values = fewer changepoints."><i class="fa fa-circle-question"></i></span>')
                ),
                selectInput(
                  ns("penalty_type"),
                  NULL,
                  choices = c(
                    "MBIC (Conservative)" = "MBIC",
                    "BIC (Moderate)" = "BIC",
                    "Manual (Custom)" = "Manual"
                  ),
                  selected = "MBIC"
                )
              ),

              conditionalPanel(
                condition = sprintf("input['%s'] == 'Manual'", ns("penalty_type")),
                sliderInput(
                  ns("penalty_value"),
                  HTML('Penalty Value <span style="color: #999; cursor: help;" title="Higher values produce fewer changepoints (more conservative). Lower values produce more changepoints (more sensitive to small shifts). Range: 0-100."><i class="fa fa-circle-question"></i></span>'),
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 5
                )
              ),

              fluidRow(
                column(
                  6,
                  numericInput(
                    ns("min_segment_days"),
                    HTML('Min Segment Days <span style="color: #999; cursor: help;" title="Minimum number of days required for a segment. Shorter segments indicate spurious changepoints and will be merged with adjacent segments. Typical: 7-14 days."><i class="fa fa-circle-question"></i></span>'),
                    value = 7,
                    min = 1,
                    max = 30,
                    step = 1
                  ),
                  selectInput(
                    ns("detect_method_filter"),
                    HTML('Method <span style="color: #999; cursor: help;" title="HPV calculation method to use for daily minima. HRM (Heat Ratio Method) is recommended as it is validated by Burgess for low flows."><i class="fa fa-circle-question"></i></span>'),
                    choices = c("HRM" = "HRM", "MHR" = "MHR"),
                    selected = "HRM"
                  )
                ),
                column(
                  6,
                  selectInput(
                    ns("detect_sensor_position"),
                    HTML('Sensor Position <span style="color: #999; cursor: help;" title="Which sensor to use for changepoint detection. Outer is recommended as it is more reliably in sapwood and less affected by heartwood interference."><i class="fa fa-circle-question"></i></span>'),
                    choices = c("Outer" = "outer", "Inner" = "inner"),
                    selected = "outer"
                  ),
                  checkboxInput(
                    ns("merge_short_segments"),
                    HTML('Merge Short Segments <span style="color: #999; cursor: help;" title="Automatically merge segments shorter than Min Segment Days with adjacent segments. Recommended to avoid spurious changepoints from outliers."><i class="fa fa-circle-question"></i></span>'),
                    value = TRUE
                  )

                ),
              ),

              br(),

              # Run detection button
              actionButton(
                ns("detect_changepoints"),
                "Run PELT Detection",
                icon = icon("chart-line"),
                class = "btn-primary",
                width = "100%"
              ),

              br(), br(),

              # Results display
              conditionalPanel(
                condition = sprintf("output['%s']", ns("changepoints_detected")),
                h5("Detected Changepoints:"),
                helpText(icon("info-circle"), " Detected changepoints shown as ", tags$span(style = "color: orange;", "orange dotted lines"), " on plot. Click ", tags$code("[+]"), " to add individually or use button below to add all."),
                uiOutput(ns("detected_changepoints_list")),
                br(),
                actionButton(
                  ns("add_detected_changepoints"),
                  "Add All Detected Changepoints",
                  icon = icon("check"),
                  class = "btn-success",
                  width = "100%"
                )
              )
            ),

            # Tab 3: Manual Definition
            tabPanel(
              "Manual",
              br(),

              helpText("Add a changepoint at a specific date/time where you know the baseline shifted."),

              fluidRow(
                column(6, dateInput(ns("changepoint_date"), "Changepoint Date", value = NULL)),
                column(6, textInput(ns("changepoint_time"), "Time (HH:MM)", value = "00:00"))
              ),

              actionButton(ns("add_changepoint"), "Add Changepoint", icon = icon("plus"),
                          class = "btn-success", width = "100%")
            ),

            # [HIDDEN]             # Tab 3: VPD Detect
            # [HIDDEN]             tabPanel(
            # [HIDDEN]               "VPD Detect",
            # [HIDDEN]               br(),

            # [HIDDEN]               helpText(
            # [HIDDEN]                 icon("info-circle"),
            # [HIDDEN]                 " Detect suitable dates for spacing correction based on low VPD (atmospheric demand) conditions."
            # [HIDDEN]               ),

            # [HIDDEN]               # Check if weather data is available
            # [HIDDEN]               conditionalPanel(
            # [HIDDEN]                 condition = "output.weather_data_available == false",
            # [HIDDEN]                 ns = ns,
            # [HIDDEN]                 div(class = "alert alert-warning",
            # [HIDDEN]                   icon("exclamation-triangle"),
            # [HIDDEN]                   strong(" Weather data required"),
            # [HIDDEN]                   br(),
            # [HIDDEN]                   "Please upload weather data in Tab 1 and calculate VPD before using this method."
            # [HIDDEN]                 )
            # [HIDDEN]               ),

            # [HIDDEN]               conditionalPanel(
            # [HIDDEN]                 condition = "output.weather_data_available == true",
            # [HIDDEN]                 ns = ns,

            # [HIDDEN]                 # VPD Threshold
            # [HIDDEN]                 sliderInput(
            # [HIDDEN]                   ns("vpd_threshold"),
            # [HIDDEN]                   HTML('VPD Threshold (kPa) <span style="color: #999; cursor: help;" title="Maximum VPD threshold. Days with minimum VPD at or below this value are selected as changepoints. 0.3 kPa = very conservative, 0.5 kPa = moderate (recommended), 0.8 kPa = permissive."><i class="fa fa-circle-question"></i></span>'),
            # [HIDDEN]                   min = 0.1,
            # [HIDDEN]                   max = 1.5,
            # [HIDDEN]                   value = 0.5,
            # [HIDDEN]                   step = 0.1
            # [HIDDEN]                 ),

            # [HIDDEN]                 fluidRow(
            # [HIDDEN]                   column(6,
            # [HIDDEN]                     numericInput(
            # [HIDDEN]                       ns("vpd_min_segment_days"),
            # [HIDDEN]                       HTML('Min Segment Days <span style="color: #999; cursor: help;" title="Minimum days between selected changepoints. If closer than this, only the day with lowest VPD is retained."><i class="fa fa-circle-question"></i></span>'),
            # [HIDDEN]                       value = 7,
            # [HIDDEN]                       min = 1,
            # [HIDDEN]                       max = 30
            # [HIDDEN]                     )
            # [HIDDEN]                   ),
            # [HIDDEN]                   column(6,
            # [HIDDEN]                     numericInput(
            # [HIDDEN]                       ns("vpd_min_consecutive_days"),
            # [HIDDEN]                       HTML('Min Consecutive Days <span style="color: #999; cursor: help;" title="Minimum consecutive days with low VPD required for changepoint selection. Set to 1 to select any single day below threshold."><i class="fa fa-circle-question"></i></span>'),
            # [HIDDEN]                       value = 1,
            # [HIDDEN]                       min = 1,
            # [HIDDEN]                       max = 10
            # [HIDDEN]                     )
            # [HIDDEN]                   )
            # [HIDDEN]                 ),

            # [HIDDEN]                 br(),

            # [HIDDEN]                 # Run detection button
            # [HIDDEN]                 actionButton(
            # [HIDDEN]                   ns("detect_vpd_changepoints"),
            # [HIDDEN]                   "Run VPD Detection",
            # [HIDDEN]                   icon = icon("cloud"),
            # [HIDDEN]                   class = "btn-primary",
            # [HIDDEN]                   width = "100%"
            # [HIDDEN]                 ),

            # [HIDDEN]                 br(), br(),

            # [HIDDEN]                 # Results display
            # [HIDDEN]                 conditionalPanel(
            # [HIDDEN]                   condition = sprintf("output['%s']", ns("vpd_changepoints_detected")),
            # [HIDDEN]                   h5("Detected VPD-Based Changepoints:"),
            # [HIDDEN]                   helpText(icon("info-circle"), " Detected changepoints shown as ", tags$span(style = "color: purple;", "purple dotted lines"), " on plot. Click ", tags$code("[+]"), " to add individually or use button below to add all."),
            # [HIDDEN]                   uiOutput(ns("detected_vpd_changepoints_list")),
            # [HIDDEN]                   br(),
            # [HIDDEN]                   actionButton(
            # [HIDDEN]                     ns("add_detected_vpd_changepoints"),
            # [HIDDEN]                     "Add All VPD Changepoints",
            # [HIDDEN]                     icon = icon("check"),
            # [HIDDEN]                     class = "btn-success",
            # [HIDDEN]                     width = "100%"
            # [HIDDEN]                   )
            # [HIDDEN]                 )
            # [HIDDEN]               )
            # [HIDDEN]             ),
          ),

          # Current changepoints display (shared between tabs)
          hr(),
          h5("Current Changepoints:"),
          helpText("Confirmed changepoints shown as ", tags$span(style = "color: red;", "red dashed lines"), " with baselines. Click ", tags$code("[X]"), " to remove."),
          uiOutput(ns("changepoint_list")),
          br(),
          actionButton(ns("clear_changepoints"), "Clear All Changepoints", icon = icon("trash"),
                      class = "btn-warning", width = "100%")
        ),

        # Step 2: Application box
        box(
          width = 12,
          title = "Step 2: Apply Spacing Correction",
          status = "success",
          solidHeader = TRUE,

          h5("Offset Model (Shape)"),
          radioButtons(
            ns("offset_model"),
            NULL,
            choices = c(
              "Segment (Step-wise)" = "segment",
              "Gradient (Continuous)" = "gradient"
            ),
            selected = "segment",
            inline = FALSE
          ),

          helpText(
            tags$strong("Segment:"), " Traditional - uses a constant offset for each period between changepoints.", tags$br(),
            tags$strong("Gradient:"), " Continuous - linearly interpolates the baseline between points. ",
            tags$span(
              style = "color: #ff6b6b; cursor: help;",
              title = "Gradient method is strictly applied between identified anchors. Data outside this range will not be corrected.",
              icon("exclamation-triangle"),
              tags$strong(" Anchors Required")
            )
          ),

          hr(),

          h5("Correction Math (Physics)"),
          radioButtons(
            ns("correction_math"),
            NULL,
            choices = c(
              "Burgess (Physics-based)" = "burgess",
              "Linear (Simple shift)" = "linear"
            ),
            selected = "burgess",
            inline = FALSE
          ),

          helpText(
            tags$strong("Burgess:"), " Uses Burgess et al. (2001) physics model. Highly accurate for HRM.", tags$br(),
            tags$strong("Linear:"), " Simple empirical subtraction. Robust for large offsets and non-HRM methods."
          ),

          br(),

          actionButton(
            ns("run_correction"),
            "Apply Correction",
            icon = icon("play"),
            class = "btn-primary",
            width = "100%"
          )
        )
      ),

      # Right column: Visualisation and Results
      column(
        width = 8,

        # Interactive changepoint plot
        box(
          width = 12,
          title = "Daily Minimum Velocities with Changepoints",
          status = "info",
          solidHeader = TRUE,
          collapsible = TRUE,

          helpText("Visualise daily minimum velocities and changepoints. Click on the plot to add a changepoint at that date."),

          radioButtons(
            ns("display_sensor"),
            "Sensor to Display:",
            choices = c("Outer" = "outer", "Inner" = "inner"),
            selected = "outer",
            inline = TRUE
          ),

          fluidRow(
            column(6,
              checkboxInput(
                ns("show_original_data"),
                "Overlay original data points",
                value = FALSE
              )
            ),
            column(6,
              conditionalPanel(
                condition = "output.weather_data_available == true",
                ns = ns,
                checkboxInput(
                  ns("show_vpd_overlay"),
                  "Show VPD overlay",
                  value = FALSE
                )
              )
            )
          ),

          # Warning when gradient overlay is hidden due to sensor/method mismatch
          uiOutput(ns("gradient_mismatch_warning")),

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("plot_changepoints"), height = "500px"),
            type = 6,
            color = "#3c8dbc"
          ),

          br(),
          p(class = "text-muted", tags$small(
            icon("info-circle"),
            " Tip: Click any date on the plot to populate the Manual tab for adding a changepoint."
          ))
        ),

        # Spacing Correction Results
        box(
          width = 12,
          title = "Spacing Correction Results",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,

          conditionalPanel(
            condition = sprintf("!output['%s']", ns("has_correction_results")),
            p(em("No results yet. Define changepoints and run spacing correction."))
          ),

          conditionalPanel(
            condition = sprintf("output['%s']", ns("has_correction_results")),

            tabsetPanel(
              id = ns("results_tabs"),

              tabPanel(
                "Before/After Comparison",
                br(),

                fluidRow(
                  column(6,
                    selectInput(
                      ns("plot_sensor_position_spacing"),
                      "Sensor Position:",
                      choices = c("Outer" = "outer", "Inner" = "inner"),
                      selected = "outer"
                    )
                  ),
                  column(6,
                    checkboxInput(
                      ns("show_raw_overlay"),
                      "Show Raw Data Overlay",
                      value = FALSE
                    )
                  )
                ),

                shinycssloaders::withSpinner(
                  plotly::plotlyOutput(ns("plot_before_after"), height = "500px"),
                  type = 6,
                  color = "#3c8dbc"
                )
              ),

              tabPanel(
                "Segment Results",
                br(),
                helpText("Each segment between changepoints gets separate Burgess correction coefficients."),
                DT::dataTableOutput(ns("segment_results_table"))
              ),

              tabPanel(
                "Summary",
                br(),
                verbatimTextOutput(ns("correction_summary"))
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
correctionsServer <- function(id, vh_results, heat_pulse_data, probe_config, wood_properties, calc_methods,
                              daily_vpd = reactive(NULL), weather_vpd = reactive(NULL), 
                              code_tracker = NULL, plot_settings = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
      changepoints = list(),  # List of POSIXct dates
      detected_result = NULL,  # Changepoint detection result (PELT)
      vpd_detected_result = NULL,  # VPD changepoint detection result
      dual_stable_detected_result = NULL,  # Dual-criterion (VPD + vh) detection result
      correction_result = NULL,  # Spacing correction result
      corrected_vh = NULL,
      correction_applied = FALSE
    )

    # Auto-populate dynamic lat/lon fields from wood_properties.
    # Note: WoodProperties is R6 — Shiny's reactiveVal uses identical() so the
    # NULL-then-set in apply_site_location (mod_2) is required to force this to fire.
    observe({
      wp <- wood_properties()
      req(!is.null(wp))
      loc <- wp$site_location
      if (is.null(loc)) return()
      if (!is.null(loc$latitude)  && !is.na(loc$latitude))
        updateNumericInput(session, "dyn_latitude",  value = loc$latitude)
      if (!is.null(loc$longitude) && !is.na(loc$longitude))
        updateNumericInput(session, "dyn_longitude", value = loc$longitude)
    })

    # Belt-and-suspenders: also re-populate when user navigates to Dual-Criterion tab,
    # in case the reactive fired before the inputs were bound client-side.
    observeEvent(input$changepoint_tabs, {
      if (!identical(input$changepoint_tabs, "Dual-Criterion")) return()
      wp <- isolate(wood_properties())
      if (is.null(wp)) return()
      loc <- wp$site_location
      if (is.null(loc)) return()
      if (!is.null(loc$latitude)  && !is.na(loc$latitude))
        updateNumericInput(session, "dyn_latitude",  value = loc$latitude)
      if (!is.null(loc$longitude) && !is.na(loc$longitude))
        updateNumericInput(session, "dyn_longitude", value = loc$longitude)
    })

    # Static predawn window preview
    output$static_predawn_preview <- renderUI({
      start <- as.integer(input$static_start_hr %||% 2)
      end   <- as.integer(input$static_end_hr   %||% 6)
      hours <- sapfluxr::resolve_predawn_hours(c(start, end), mode = "static")
      p(style = "font-size:0.85em; color:#3c763d; background:#dff0d8; padding:5px 8px; border-radius:3px; margin-top:4px;",
        icon("clock"),
        sprintf(" Analysing local hours: %s", paste(hours, collapse = ", "))
      )
    })

    # Dynamic predawn window preview
    output$dynamic_predawn_preview <- renderUI({
      vals <- input$dynamic_predawn
      if (is.null(vals)) return(NULL)
      h_before_start <- abs(vals[1])   # further from dawn (left handle)
      h_before_end   <- abs(vals[2])   # closer  to dawn  (right handle)

      has_loc <- !is.null(input$dyn_latitude)  && !is.na(input$dyn_latitude) &&
                 !is.null(input$dyn_longitude) && !is.na(input$dyn_longitude)

      if (has_loc) {
        p(style = "font-size:0.85em; color:#3c763d; background:#dff0d8; padding:5px 8px; border-radius:3px; margin-top:4px;",
          icon("clock"),
          sprintf(" Window: %.1f h to %.1f h before dawn each day", h_before_start, h_before_end)
        )
      } else {
        tagList(
          p(style = "font-size:0.85em; color:#3c763d; background:#dff0d8; padding:5px 8px; border-radius:3px; margin-top:4px;",
            icon("clock"),
            sprintf(" Window: %.1f h to %.1f h before dawn each day", h_before_start, h_before_end)
          ),
          p(style = "font-size:0.85em; color:#8a6d3b; background:#fcf8e3; padding:5px 8px; border-radius:3px; margin-top:4px;",
            icon("exclamation-triangle"),
            " Enter site coordinates above to enable dawn-time calculation."
          )
        )
      }
    })

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

    # Initialize settings from configuration
    observe({
      req(vh_results())

      vh_data <- vh_results()
      if (!is.null(vh_data) && nrow(vh_data) > 0) {
        # Set initial date to midpoint of data
        date_range <- range(vh_data$datetime, na.rm = TRUE)
        mid_date <- as.Date(date_range[1]) + as.numeric(diff(as.Date(date_range))) / 2

        updateDateInput(session, "changepoint_date", value = mid_date)
      }
    })

    # Initialize analysis settings from actual config
    observe({
      # Get k from wood properties if available
      if (!is.null(wood_properties())) {
        wood <- wood_properties()
        if (!is.null(wood$thermal_diffusivity)) {
          updateNumericInput(session, "k_assumed", value = wood$thermal_diffusivity)
        }
      }

      # Get probe spacing from probe config if available
      if (!is.null(probe_config())) {
        probe <- probe_config()
        if (!is.null(probe$probe_spacing)) {
          updateNumericInput(session, "probe_spacing", value = probe$probe_spacing)
        }
      }
    })

    # ==================================================================
    # MANUAL CHANGEPOINT ENTRY
    # ==================================================================

    observeEvent(input$add_changepoint, {
      req(input$changepoint_date, input$changepoint_time)

      tryCatch({
        cp_str <- paste(input$changepoint_date, input$changepoint_time)
        cp_dt <- as.POSIXct(cp_str, format = "%Y-%m-%d %H:%M")

        if (is.na(cp_dt)) {
          showNotification("Invalid date/time format", type = "error")
          return()
        }

        # Add to list (keep as POSIXct)
        rv$changepoints <- c(rv$changepoints, list(cp_dt))

        # Sort changepoints chronologically
        rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

        showNotification(
          sprintf("Added changepoint: %s", format(cp_dt, "%Y-%m-%d %H:%M")),
          type = "message",
          duration = 5
        )

      }, error = function(e) {
        showNotification(paste("Error adding changepoint:", e$message), type = "error")
      })
    })

    # Clear all changepoints
    observeEvent(input$clear_changepoints, {
      rv$changepoints <- list()
      rv$detected_result <- NULL
      showNotification("All changepoints cleared", type = "message", duration = 5)
    })

    # Display changepoint list with delete buttons
    output$changepoint_list <- renderUI({
      if (length(rv$changepoints) == 0) {
        return(p(em("No changepoints defined yet.")))
      }

      # Create list of changepoints with delete buttons
      cp_items <- lapply(seq_along(rv$changepoints), function(i) {
        cp <- rv$changepoints[[i]]
        cp_text <- format(cp, "%Y-%m-%d %H:%M")

        # Get baseline value if we have detection results
        baseline_text <- ""
        if (!is.null(rv$detected_result) && !is.null(rv$detected_result$segments)) {
          segments <- rv$detected_result$segments
          # Find which segment this changepoint ends
          seg_idx <- which(segments$end_date == as.Date(cp))
          if (length(seg_idx) > 0 && "baseline_value" %in% names(segments)) {
            baseline <- segments$baseline_value[seg_idx[1]]
            baseline_text <- sprintf(" (baseline: %.2f cm/hr)", baseline)
          }
        }

        div(
          style = "margin-bottom: 5px;",
          actionButton(
            session$ns(paste0("delete_cp_", i)),
            label = NULL,
            icon = icon("times"),
            class = "btn-xs btn-danger",
            style = "margin-right: 10px;"
          ),
          span(paste0(i, ". ", cp_text, baseline_text))
        )
      })

      tagList(cp_items)
    })

    # Handle delete button clicks
    observe({
      # Create observers for each delete button
      lapply(seq_along(rv$changepoints), function(i) {
        btn_id <- paste0("delete_cp_", i)
        observeEvent(input[[btn_id]], {
          rv$changepoints <- rv$changepoints[-i]
          showNotification(
            sprintf("Removed changepoint %d", i),
            type = "message",
            duration = 5
          )
        }, ignoreInit = TRUE)
      })
    })

    # ==================================================================
    # AUTO-DETECT CHANGEPOINTS
    # ==================================================================

    observeEvent(input$detect_changepoints, {
      req(vh_results())

      vh_data <- vh_results()

      # Show waiter
      waiter <- waiter::Waiter$new(
        html = waiter::spin_fading_circles(),
        color = waiter::transparent(0.5)
      )
      waiter$show()

      tryCatch({
        # Step 1: Detect changepoints (calculates minima internally)
        cpt_result <- sapfluxr::detect_changepoints(
          vh_data = vh_data,
          sensor_position = input$detect_sensor_position,
          hpv_method = input$detect_method_filter,
          penalty = input$penalty_type,
          penalty_value = if (input$penalty_type == "Manual") input$penalty_value else NULL,
          detection_type = "mean",
          min_segment_days = input$min_segment_days,
          merge_short_segments = input$merge_short_segments
        )

        # Hide waiter after detection completes
        waiter$hide()

        if (length(cpt_result$changepoints) == 0) {
          # No changepoints detected
          # Only clear rv$detected_result if it was previously set
          # This avoids triggering plot re-render if already NULL
          if (!is.null(rv$detected_result)) {
            rv$detected_result <- NULL
          }

          showNotification(
            "No changepoints detected with current settings. Try reducing penalty or minimum segment days.",
            type = "warning",
            duration = 5
          )
        } else {
          # Update detected results - this will trigger plot re-render
          rv$detected_result <- cpt_result

          # Track changepoint detection in the reproducible script
          if (!is.null(code_tracker)) {
            pen_val <- if (identical(input$penalty_type, "Manual")) {
              sprintf("%g", input$penalty_value)
            } else {
              "NULL"
            }
            code_tracker$add_step(
              step_name = "Detect Changepoints",
              code = sprintf(paste0(
                "# Detect zero-flow baseline changepoints via PELT\n",
                "cpt_result <- sapfluxr::detect_changepoints(\n",
                "  vh_data = vh_results,\n",
                "  sensor_position = \"%s\",\n",
                "  hpv_method = \"%s\",\n",
                "  penalty = \"%s\",\n",
                "  penalty_value = %s,\n",
                "  detection_type = \"mean\",\n",
                "  min_segment_days = %g,\n",
                "  merge_short_segments = %s\n)\n",
                "changepoints <- cpt_result$changepoints"),
                input$detect_sensor_position, input$detect_method_filter,
                input$penalty_type, pen_val, input$min_segment_days,
                toupper(as.character(input$merge_short_segments))),
              description = sprintf(
                "PELT detection: %d changepoint(s), %d segment(s)",
                length(cpt_result$changepoints),
                cpt_result$parameters$n_segments))
          }

          showNotification(
            sprintf("Detected %d changepoint(s), creating %d segment(s)",
                   length(cpt_result$changepoints),
                   cpt_result$parameters$n_segments),
            type = "message",
            duration = 5
          )
        }

      }, error = function(e) {
        waiter$hide()
        showNotification(
          paste("Error detecting changepoints:", e$message),
          type = "error",
          duration = 10
        )
        rv$detected_result <- NULL
      })
    })

    # Show/hide detected changepoints output
    output$changepoints_detected <- reactive({
      !is.null(rv$detected_result) && length(rv$detected_result$changepoints) > 0
    })
    outputOptions(output, "changepoints_detected", suspendWhenHidden = FALSE)

    # Display detected changepoints with individual add buttons
    output$detected_changepoints_list <- renderUI({
      req(rv$detected_result)

      cpts <- rv$detected_result$changepoints
      segments <- rv$detected_result$segments

      if (length(cpts) == 0) {
        return(p(em("No changepoints detected.")))
      }

      # Create header
      header <- tags$p(strong(sprintf("Detected %d changepoint(s):", length(cpts))))

      # Create list of detected changepoints with individual add buttons
      cpt_items <- lapply(seq_along(cpts), function(i) {
        cp_date <- format(cpts[i], "%Y-%m-%d")

        # Get baseline info
        seg <- segments[segments$end_date == cpts[i], ]
        baseline_text <- ""
        if (nrow(seg) > 0 && "baseline_value" %in% names(seg)) {
          baseline <- seg$baseline_value[1]
          baseline_text <- sprintf(" (baseline: %.2f cm/hr, %d days)",
                                  baseline, seg$n_days[1])
        }

        div(
          style = "margin-bottom: 5px;",
          actionButton(
            session$ns(paste0("add_detected_cp_", i)),
            label = NULL,
            icon = icon("plus"),
            class = "btn-xs btn-success",
            style = "margin-right: 10px;"
          ),
          span(paste0(i, ". ", cp_date, baseline_text))
        )
      })

      # Combine header, items, and summary
      tagList(
        header,
        tagList(cpt_items),
        tags$p(
          tags$small(
            sprintf("This creates %d segment(s). Click [+] to add individual changepoints or use 'Add All' below.",
                   rv$detected_result$parameters$n_segments)
          )
        )
      )
    })

    # Handle individual add button clicks for detected changepoints
    observe({
      req(rv$detected_result)

      cpts <- rv$detected_result$changepoints

      # Create observers for each individual add button
      lapply(seq_along(cpts), function(i) {
        btn_id <- paste0("add_detected_cp_", i)
        observeEvent(input[[btn_id]], {
          # Convert Date to POSIXct at midnight
          cp_date <- cpts[i]
          cp_posix <- as.POSIXct(paste(cp_date, "00:00:00"), format = "%Y-%m-%d %H:%M:%S")

          # Add to changepoints list
          rv$changepoints <- c(rv$changepoints, list(cp_posix))

          # Sort chronologically
          rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

          showNotification(
            sprintf("Added changepoint: %s", format(cp_date, "%Y-%m-%d")),
            type = "message",
            duration = 3
          )
        }, ignoreInit = TRUE)
      })
    })

    # Add all detected changepoints to the list
    observeEvent(input$add_detected_changepoints, {
      req(rv$detected_result)

      cpts <- rv$detected_result$changepoints

      if (length(cpts) == 0) {
        showNotification("No changepoints to add", type = "warning")
        return()
      }

      # Convert Date to POSIXct at midnight
      cpts_posix <- lapply(cpts, function(d) {
        as.POSIXct(paste(d, "00:00:00"), format = "%Y-%m-%d %H:%M:%S")
      })

      # Add to existing changepoints
      rv$changepoints <- c(rv$changepoints, cpts_posix)

      # Sort chronologically
      rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

      showNotification(
        sprintf("Added %d changepoint(s) to the list", length(cpts)),
        type = "message",
        duration = 5
      )

      # Clear detected results after adding
      # rv$detected_result <- NULL  # Keep results for plot
    })

    # ==================================================================
    # VPD-BASED CHANGEPOINT DETECTION
    # ==================================================================

    # Weather data availability flag
    output$weather_data_available <- reactive({
      !is.null(daily_vpd()) && nrow(daily_vpd()) > 0
    })
    outputOptions(output, "weather_data_available", suspendWhenHidden = FALSE)

    # VPD changepoint detection
    observeEvent(input$detect_vpd_changepoints, {
      req(daily_vpd())

      vpd_data <- daily_vpd()

      # Show waiter
      waiter <- waiter::Waiter$new(
        html = waiter::spin_fading_circles(),
        color = waiter::transparent(0.5)
      )
      waiter$show()

      tryCatch({
        # Detect VPD-based changepoints
        vpd_result <- sapfluxr::detect_vpd_changepoints(
          daily_vpd = vpd_data,
          vpd_threshold = input$vpd_threshold,
          min_segment_days = input$vpd_min_segment_days,
          min_consecutive_days = input$vpd_min_consecutive_days
        )

        # Hide waiter
        waiter$hide()

        if (length(vpd_result$changepoints) == 0) {
          # No changepoints detected
          if (!is.null(rv$vpd_detected_result)) {
            rv$vpd_detected_result <- NULL
          }

          showNotification(
            sprintf("No suitable VPD dates found with threshold %.2f kPa. Try increasing the threshold or reducing min_segment_days.",
                   input$vpd_threshold),
            type = "warning",
            duration = 5
          )
        } else {
          # Update detected results
          rv$vpd_detected_result <- vpd_result

          showNotification(
            sprintf("Detected %d VPD-based changepoint(s) from %d days below threshold",
                   length(vpd_result$changepoints),
                   vpd_result$n_days_below_threshold),
            type = "message",
            duration = 5
          )
        }

      }, error = function(e) {
        waiter$hide()
        showNotification(
          paste("Error detecting VPD changepoints:", e$message),
          type = "error",
          duration = 10
        )
        rv$vpd_detected_result <- NULL
      })
    })

    # Show/hide VPD detected changepoints output
    output$vpd_changepoints_detected <- reactive({
      !is.null(rv$vpd_detected_result) && length(rv$vpd_detected_result$changepoints) > 0
    })
    outputOptions(output, "vpd_changepoints_detected", suspendWhenHidden = FALSE)

    # Display VPD detected changepoints with individual add buttons
    output$detected_vpd_changepoints_list <- renderUI({
      req(rv$vpd_detected_result)

      cpts <- rv$vpd_detected_result$changepoints
      vpd_vals <- rv$vpd_detected_result$vpd_values

      if (length(cpts) == 0) return(NULL)

      # Create list of changepoints with add buttons
      tagList(
        lapply(seq_along(cpts), function(i) {
          div(
            style = "margin-bottom: 5px;",
            actionButton(
              session$ns(paste0("add_vpd_cp_", i)),
              label = NULL,
              icon = icon("plus"),
              class = "btn-xs btn-success",
              style = "padding: 2px 6px; margin-right: 8px;"
            ),
            tags$span(
              sprintf("%s (VPD: %.3f kPa)", format(cpts[i], "%Y-%m-%d"), vpd_vals[i]),
              style = "font-family: monospace;"
            )
          )
        })
      )
    })

    # Handle individual add button clicks for VPD detected changepoints
    observe({
      req(rv$vpd_detected_result)

      cpts <- rv$vpd_detected_result$changepoints

      # Create observers for each individual add button
      lapply(seq_along(cpts), function(i) {
        btn_id <- paste0("add_vpd_cp_", i)
        observeEvent(input[[btn_id]], {
          # Convert Date to POSIXct at midnight
          cp_date <- cpts[i]
          cp_posix <- as.POSIXct(paste(cp_date, "00:00:00"), format = "%Y-%m-%d %H:%M:%S")

          # Add to changepoints list
          rv$changepoints <- c(rv$changepoints, list(cp_posix))

          # Sort chronologically
          rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

          showNotification(
            sprintf("Added VPD changepoint: %s", format(cp_date, "%Y-%m-%d")),
            type = "message",
            duration = 3
          )
        }, ignoreInit = TRUE)
      })
    })

    # Add all VPD detected changepoints to the list
    observeEvent(input$add_detected_vpd_changepoints, {
      req(rv$vpd_detected_result)

      cpts <- rv$vpd_detected_result$changepoints

      if (length(cpts) == 0) {
        showNotification("No VPD changepoints to add", type = "warning")
        return()
      }

      # Convert Date to POSIXct at midnight
      cpts_posix <- lapply(cpts, function(d) {
        as.POSIXct(paste(d, "00:00:00"), format = "%Y-%m-%d %H:%M:%S")
      })

      # Add to existing changepoints
      rv$changepoints <- c(rv$changepoints, cpts_posix)

      # Sort chronologically
      rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

      showNotification(
        sprintf("Added %d VPD changepoint(s) to the list", length(cpts)),
        type = "message",
        duration = 5
      )

      # Clear detected results after adding
      # rv$vpd_detected_result <- NULL  # Keep results for plot
    })


    # ==================================================================
    # DUAL-CRITERION (VPD + vh) CHANGEPOINT DETECTION
    # ==================================================================

    # Dual-criterion changepoint detection
    observeEvent(input$detect_dual_stable_changepoints, {
      req(weather_vpd(), vh_results())

      # Get raw weather data (hourly/sub-hourly)
      weather_data <- weather_vpd()
      vh_data <- vh_results()

      # Show waiter
      waiter <- waiter::Waiter$new(
        html = waiter::spin_fading_circles(),
        color = waiter::transparent(0.5)
      )
      waiter$show()

      tryCatch({
        predawn_mode <- input$dual_predawn_mode %||% "static"

        # Build predawn_window in the format expected by find_dual_stable_periods
        if (identical(predawn_mode, "static")) {
          start_hr <- as.integer(input$static_start_hr %||% 2)
          end_hr   <- as.integer(input$static_end_hr   %||% 6)
          predawn_range <- c(start_hr, end_hr)
        } else {
          # Dynamic: slider values are negative (−hours_before_dawn)
          vals <- input$dynamic_predawn %||% c(-4, -1)
          predawn_range <- c(abs(vals[1]), abs(vals[2]))   # c(hours_before_start, hours_before_end)
        }

        # Get sensor and method
        sensor <- input$dual_sensor_position %||% "outer"
        method <- input$dual_method_filter %||% "HRM"

        # Get vh column name (prefer current best estimate)
        vh_col <- if ("Vs_cm_hr" %in% names(vh_data)) "Vs_cm_hr" else "Vh_cm_hr"

        # Resolve site timezone — used by both dynamic dawn computation and
        # predawn hour alignment inside find_dual_stable_periods().
        wp <- wood_properties()
        tz <- if (!is.null(wp$site_location$timezone) && nzchar(wp$site_location$timezone))
                wp$site_location$timezone else NULL

        # Resolve dynamic dawn times using panel lat/lon (auto-populated from wood_properties)
        dawn_times <- NULL
        if (identical(predawn_mode, "dynamic")) {
          lat <- input$dyn_latitude
          lon <- input$dyn_longitude
          if (is.null(lat) || is.na(lat) || is.null(lon) || is.na(lon)) {
            stop("Latitude and longitude are required for Dynamic mode. ",
                 "Enter coordinates in the panel above or in Step 2 → Tree Properties.")
          }
          tryCatch({
            dates <- unique(as.Date(vh_data$datetime))
            dawn_df <- suncalc::getSunlightTimes(
              date = dates, lat = lat, lon = lon, keep = "dawn",
              tz = tz %||% Sys.timezone()
            )
            dawn_times <- dawn_df$dawn
          }, error = function(e) {
            showNotification(
              paste("Could not compute dawn times:", e$message,
                    "— falling back to static mode."),
              type = "warning", duration = 6
            )
            predawn_mode <<- "static"
            predawn_range <<- c(abs(vals[1]), abs(vals[2]))
          })
        }

        # Detect dual-stable periods. We deliberately pass timezone = NULL
        # because data is already in local time (even if tagged as UTC during import).
        # Passing 'tz' here would cause with_tz to shift the clock hours incorrectly.
        dual_stable_result <- sapfluxr::find_dual_stable_periods(
          vh_data = vh_data,
          weather_data = weather_data,
          vh_col = vh_col,
          method = method,
          sensor_position = sensor,
          vpd_col = "vpd_kpa",
          predawn_window = predawn_range,
          mode = predawn_mode,
          dawn_times = dawn_times,
          timezone = NULL,
          vpd_threshold = input$dual_vpd_threshold,
          vpd_stability = input$dual_vpd_stability,
          vh_threshold = input$dual_vh_threshold,
          vh_stability = input$dual_vh_stability,
          min_n_points = 4,
          min_segment_days = input$dual_min_segment_days,
          max_changepoints = NULL
        )

        # Hide waiter
        waiter$hide()

        if (length(dual_stable_result$dual_stable_dates) == 0) {
          # No dual-stable periods detected
          if (!is.null(rv$dual_stable_detected_result)) {
            rv$dual_stable_detected_result <- NULL
          }

          # Get diagnostic counts
          n_vpd <- dual_stable_result$vpd_results$n_dates_selected
          n_vh <- dual_stable_result$vh_results$n_dates_selected

          showNotification(
            sprintf("No dual-stable periods found. VPD-stable: %d, vh-stable: %d. Try relaxing thresholds.",
                   n_vpd, n_vh),
            type = "warning",
            duration = 5
          )
        } else {
          # Update detected results AND store which sensor/method was used
          rv$dual_stable_detected_result <- dual_stable_result
          rv$dual_stable_detected_result$detection_sensor <- sensor
          rv$dual_stable_detected_result$detection_method <- method

          # Track changepoint detection in the reproducible script
          if (!is.null(code_tracker)) {
            dawn_expr <- if (identical(predawn_mode, "dynamic")) {
              "dawn_times  # computed via suncalc::getSunlightTimes() for the site"
            } else {
              "NULL"
            }
            code_tracker$add_step(
              step_name = "Detect Changepoints",
              code = sprintf(paste0(
                "# Detect zero-flow baseline changepoints via dual-stable periods\n",
                "# (predawn windows where BOTH VPD and sap velocity are stable)\n",
                "dual_stable <- sapfluxr::find_dual_stable_periods(\n",
                "  vh_data = vh_results,\n",
                "  weather_data = weather_vpd,\n",
                "  vh_col = \"%s\",\n",
                "  method = \"%s\",\n",
                "  sensor_position = \"%s\",\n",
                "  vpd_col = \"vpd_kpa\",\n",
                "  predawn_window = c(%g, %g),\n",
                "  mode = \"%s\",\n",
                "  dawn_times = %s,\n",
                "  timezone = NULL,\n",
                "  vpd_threshold = %g,\n",
                "  vpd_stability = %g,\n",
                "  vh_threshold = %g,\n",
                "  vh_stability = %g,\n",
                "  min_n_points = 4,\n",
                "  min_segment_days = %g,\n",
                "  max_changepoints = NULL\n)\n",
                "changepoints <- dual_stable$changepoints$timestamp"),
                vh_col, method, sensor,
                predawn_range[1], predawn_range[2], predawn_mode, dawn_expr,
                input$dual_vpd_threshold, input$dual_vpd_stability,
                input$dual_vh_threshold, input$dual_vh_stability,
                input$dual_min_segment_days),
              description = sprintf(
                "Dual-stable detection: %d period(s), %d changepoint(s) (%s sensor, %s)",
                length(dual_stable_result$dual_stable_dates),
                nrow(dual_stable_result$changepoints),
                toupper(sensor), method))
          }

          showNotification(
            sprintf("Detected %d dual-stable period(s) with %d changepoint(s) using %s sensor, %s method",
                   length(dual_stable_result$dual_stable_dates),
                   nrow(dual_stable_result$changepoints),
                   toupper(sensor), method),
            type = "message",
            duration = 5
          )
        }

      }, error = function(e) {
        waiter$hide()
        showNotification(
          paste("Error detecting dual-stable periods:", e$message),
          type = "error",
          duration = 10
        )
        rv$dual_stable_detected_result <- NULL
      })
    })

    # Show/hide dual-stable detected changepoints output
    output$dual_stable_changepoints_detected <- reactive({
      !is.null(rv$dual_stable_detected_result) && nrow(rv$dual_stable_detected_result$changepoints) > 0
    })
    outputOptions(output, "dual_stable_changepoints_detected", suspendWhenHidden = FALSE)

    # Display dual-stable detected changepoints with individual add buttons
    output$detected_dual_stable_changepoints_list <- renderUI({
      req(rv$dual_stable_detected_result, vh_results())

      cpts <- rv$dual_stable_detected_result$changepoints

      if (nrow(cpts) == 0) return(NULL)

      # Create summary
      n_vpd <- rv$dual_stable_detected_result$vpd_results$n_dates_selected
      n_vh <- rv$dual_stable_detected_result$vh_results$n_dates_selected
      n_dual <- nrow(cpts)

      # Calculate edge periods
      vh_data <- vh_results()
      data_start <- min(vh_data$datetime, na.rm = TRUE)
      data_end <- max(vh_data$datetime, na.rm = TRUE)
      first_cp <- min(cpts$timestamp)
      last_cp <- max(cpts$timestamp)

      edge_before_days <- as.numeric(difftime(first_cp, data_start, units = "days"))
      edge_after_days <- as.numeric(difftime(data_end, last_cp, units = "days"))
      correctable_days <- as.numeric(difftime(last_cp, first_cp, units = "days"))
      total_days <- as.numeric(difftime(data_end, data_start, units = "days"))

      edge_total_days <- edge_before_days + edge_after_days
      edge_pct <- round(100 * edge_total_days / total_days, 1)
      correctable_pct <- round(100 * correctable_days / total_days, 1)

      # Create list of changepoints with add buttons
      tagList(
        tags$p(
          style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
          tags$strong("Detection Summary:"), tags$br(),
          sprintf("VPD-stable dates: %d | vh-stable dates: %d | DUAL-stable: %d", n_vpd, n_vh, n_dual)
        ),

        # Edge period warning
        div(
          style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 10px; margin-top: 10px; margin-bottom: 10px;",
          icon("exclamation-triangle", style = "color: #856404;"),
          tags$strong(" Edge Period Analysis", style = "color: #856404;"), tags$br(),
          tags$small(
            sprintf("Before first CP: %.1f days (%.1f%%)", edge_before_days, 100 * edge_before_days / total_days), tags$br(),
            sprintf("Between changepoints: %.1f days (%.1f%%) ", correctable_days, correctable_pct),
            tags$span(style = "color: #28a745;", icon("check"), " Gradient-correctable"), tags$br(),
            sprintf("After last CP: %.1f days (%.1f%%)", edge_after_days, 100 * edge_after_days / total_days), tags$br(),
            tags$strong(sprintf("Total edge exclusion: %.1f days (%.1f%%)", edge_total_days, edge_pct))
          )
        ),

        br(),
        lapply(seq_len(nrow(cpts)), function(i) {
          div(
            style = "margin-bottom: 5px;",
            actionButton(
              session$ns(paste0("add_dual_cp_", i)),
              label = NULL,
              icon = icon("plus"),
              class = "btn-xs btn-success",
              style = "padding: 2px 6px; margin-right: 8px;"
            ),
            tags$span(
              sprintf("%s @ %s (vh: %.3f cm/hr)",
                     format(cpts$date[i], "%Y-%m-%d"),
                     format(cpts$timestamp[i], "%H:%M"),
                     cpts$vh_value[i]),
              style = "font-family: monospace;"
            )
          )
        })
      )
    })

    # Handle individual add button clicks for dual-stable detected changepoints
    observe({
      req(rv$dual_stable_detected_result)

      cpts <- rv$dual_stable_detected_result$changepoints

      # Create observers for each individual add button
      lapply(seq_len(nrow(cpts)), function(i) {
        btn_id <- paste0("add_dual_cp_", i)
        observeEvent(input[[btn_id]], {
          # Use the exact timestamp from changepoints (already POSIXct)
          cp_posix <- cpts$timestamp[i]

          # Add to changepoints list
          rv$changepoints <- c(rv$changepoints, list(cp_posix))

          # Sort chronologically
          rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

          showNotification(
            sprintf("Added dual-stable changepoint: %s", format(cp_posix, "%Y-%m-%d %H:%M")),
            type = "message",
            duration = 3
          )
        }, ignoreInit = TRUE)
      })
    })

    # Add all dual-stable detected changepoints to the list
    observeEvent(input$add_detected_dual_stable_changepoints, {
      req(rv$dual_stable_detected_result)

      cpts <- rv$dual_stable_detected_result$changepoints

      if (nrow(cpts) == 0) {
        showNotification("No dual-stable changepoints to add", type = "warning")
        return()
      }

      # Use exact timestamps from changepoints (already POSIXct)
      cpts_posix <- as.list(cpts$timestamp)

      # Add to existing changepoints
      rv$changepoints <- c(rv$changepoints, cpts_posix)

      # Sort chronologically
      rv$changepoints <- rv$changepoints[order(sapply(rv$changepoints, as.numeric))]

      showNotification(
        sprintf("Added %d dual-stable changepoint(s) to the list", nrow(cpts)),
        type = "message",
        duration = 5
      )

      # Clear detected results after adding
      # rv$dual_stable_detected_result <- NULL  # Keep results for plot
    })

    # ==================================================================
    # CACHED DAILY MINIMA CALCULATION
    # ==================================================================

    # Cache daily minima to avoid recalculating on every plot render
    cached_daily_min <- reactive({
      req(vh_results())

      vh_data <- vh_results()

      # Display sensor (single selection from radio button)
      display_sensor <- if (!is.null(input$display_sensor)) {
        input$display_sensor
      } else {
        "outer"  # Default
      }

      method <- if (!is.null(input$detect_method_filter)) {
        input$detect_method_filter
      } else {
        "HRM"
      }

      # Calculate daily minima using DISPLAY sensor (user's current selection)
      sapfluxr::calculate_daily_minima(
        vh_data = vh_data,
        sensor_position = display_sensor,
        method = method,
        vh_col = if ("Vs_cm_hr" %in% names(vh_data)) "Vs_cm_hr" else "Vh_cm_hr"
      )
    })

    # ==================================================================
    # INTERACTIVE CHANGEPOINT PLOT
    # ==================================================================

    output$plot_changepoints <- plotly::renderPlotly({
      req(vh_results())

      # Add explicit dependency on changepoints to trigger re-render
      rv$changepoints

      # Also depend on detected results
      rv$detected_result

      # Depend on VPD overlay checkbox
      input$show_vpd_overlay

      # Depend on baseline method selection to show gradient overlay
      input$offset_model

      # Depend on dual-stable results for gradient visualization
      rv$dual_stable_detected_result

      # Debug: Track when plot re-renders
      start_time <- Sys.time()
      message("=== PLOT RENDER START ===")

      # Show progress during plot rendering for large datasets
      withProgress(message = 'Rendering plot...', value = 0, {
        incProgress(0.2)

        tryCatch({
          vh_data <- vh_results()
          daily_min <- cached_daily_min()
          incProgress(0.2)

          message(sprintf("Daily minima cached: %d rows", nrow(daily_min)))

        # Display sensor (single selection from radio button)
        display_sensor <- if (!is.null(input$display_sensor)) {
          input$display_sensor
        } else {
          "outer"  # Default
        }

        method <- if (!is.null(input$detect_method_filter)) {
          input$detect_method_filter
        } else {
          "HRM"
        }

        if (nrow(daily_min) == 0) {
          return(plotly::plotly_empty(source = "changepoint_plot") %>% plotly::event_register("plotly_click"))
        }

        # Convert current changepoints to dates for plotting
        cpts_dates <- if (length(rv$changepoints) > 0) {
          # Convert list of POSIXct to vector of Dates
          # Using do.call to combine list elements into a vector
          as.Date(do.call(c, rv$changepoints))
        } else {
          NULL
        }

        # Get segments if available
        segments <- if (!is.null(rv$detected_result)) {
          rv$detected_result$segments
        } else {
          NULL
        }

        # Filter vh_data to DISPLAY sensors (can be both) and method for overlay
        vh_filtered <- vh_data[
          vh_data$sensor_position == display_sensor &
          vh_data$method == method,
        ]

        # Get proposed changepoints from auto-detect (not yet confirmed)
        proposed_cpts <- if (!is.null(rv$detected_result)) {
          rv$detected_result$changepoints
        } else {
          NULL
        }

        # Debug: Log what we're passing to the plot
        n_confirmed <- if (!is.null(cpts_dates)) length(cpts_dates) else 0
        n_proposed <- if (!is.null(proposed_cpts)) length(proposed_cpts) else 0

          message(sprintf("Plot rendering: %d confirmed changepoints, %d proposed changepoints",
                         n_confirmed, n_proposed))

          incProgress(0.2, detail = "Calculating daily minima")

          # Create interactive plot
          incProgress(0.3, detail = "Generating plot")
          # NOTE: segments = NULL so they will be auto-generated from confirmed changepoints only
          p <- sapfluxr::plot_changepoints_interactive(
            daily_min = daily_min,
            changepoints = cpts_dates,  # Confirmed changepoints (red lines + baselines)
            segments = NULL,  # Let plot auto-generate from confirmed changepoints
            proposed_changepoints = proposed_cpts,  # Detected but not confirmed (orange lines, no baselines)
            vh_data = vh_filtered,
            title = sprintf("Daily Minimum Velocities - %s Sensor (%s)",
                           toupper(display_sensor), method),
            show_baseline_values = TRUE,
            show_original_data = isTRUE(input$show_original_data),
            vpd_data = daily_vpd(),
            show_vpd = isTRUE(input$show_vpd_overlay)
          )

          incProgress(0.2, detail = "Finalizing")

          # Add gradient overlay if gradient method is selected and dual-stable results exist
          baseline_method <- input$offset_model %||% "segment_minimum"

          if (baseline_method == "gradient" && !is.null(rv$dual_stable_detected_result)) {
            dual_results <- rv$dual_stable_detected_result

            # Check if displayed sensor/method matches detection sensor/method
            detection_sensor <- dual_results$detection_sensor %||% "outer"
            detection_method <- dual_results$detection_method %||% "HRM"

            # Only show gradient overlay if sensor/method match
            if (display_sensor == detection_sensor && method == detection_method) {
              if (!is.null(dual_results$changepoints) && nrow(dual_results$changepoints) > 0) {
                changepoints_df <- dual_results$changepoints

              # Calculate gradient interpolation for visualization
              # Extend to edges of data range
              date_range <- range(daily_min$date, na.rm = TRUE)

              # Create extended changepoint data for plotting (including edges)
              gradient_data <- data.frame(
                datetime = c(
                  as.POSIXct(paste(date_range[1], "00:00:00")),
                  changepoints_df$timestamp,
                  as.POSIXct(paste(date_range[2], "23:59:59"))
                ),
                vh_value = c(
                  changepoints_df$vh_value[1],  # Extend first value to start
                  changepoints_df$vh_value,
                  changepoints_df$vh_value[nrow(changepoints_df)]  # Extend last value to end
                )
              )

              # Add gradient line to plot
              p <- p %>%
                plotly::add_lines(
                  data = gradient_data,
                  x = ~datetime,
                  y = ~vh_value,
                  name = "Gradient Baseline",
                  line = list(
                    color = "purple",
                    width = 2,
                    dash = "solid"
                  ),
                  hovertemplate = paste(
                    "<b>Gradient Baseline</b><br>",
                    "Date: %{x|%Y-%m-%d %H:%M}<br>",
                    "Offset: %{y:.3f} cm/hr<br>",
                    "<extra></extra>"
                  ),
                  showlegend = TRUE
                )

              # Add points at changepoints
              p <- p %>%
                plotly::add_markers(
                  data = changepoints_df,
                  x = ~timestamp,
                  y = ~vh_value,
                  name = "Gradient Anchors",
                  marker = list(
                    color = "purple",
                    size = 8,
                    symbol = "diamond"
                  ),
                  hovertemplate = paste(
                    "<b>Gradient Anchor</b><br>",
                    "Date: %{x|%Y-%m-%d %H:%M}<br>",
                    "vh: %{y:.3f} cm/hr<br>",
                    "<extra></extra>"
                  ),
                  showlegend = TRUE
                )
              }  # Close changepoints check
            } else {
              # Sensor/method mismatch - add note to plot
              message(sprintf(
                "Gradient overlay hidden: Detection used %s sensor (%s method), but plot shows %s sensor (%s method)",
                toupper(detection_sensor), detection_method,
                toupper(display_sensor), method
              ))
            }  # Close sensor/method match check
          }  # Close gradient method check

          end_time <- Sys.time()
          elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
          message(sprintf("=== PLOT RENDER COMPLETE: %.2f seconds ===", elapsed))

          # Register click event to avoid warnings and apply standard layout
          base_layout <- get_standard_layout(
            title = "",
            xtitle = "Date",
            ytitle = "Velocity (cm/hr)",
            uirevision = "changepoint_plot_zoom"
          )
          
          # Handle VPD axis alignment if shown
          if (isTRUE(input$show_vpd_overlay) && !is.null(daily_vpd()) && nrow(daily_vpd()) > 0) {
            vpd <- daily_vpd()
            
            # Calculate Y ranges for alignment
            vh_min <- min(0, min(daily_min$min_value, na.rm = TRUE) * 1.1)
            vh_max <- max(0, max(daily_min$min_value, na.rm = TRUE) * 1.1)
            
            vpd_max <- max(0, max(vpd$min_vpd, na.rm = TRUE) * 1.1)
            vpd_min <- if (vh_max > 0) vpd_max * (vh_min / vh_max) else 0
            
            base_layout$yaxis$range <- c(vh_min, vh_max)
            base_layout$yaxis2 <- list(
              title = "VPD (kPa)",
              overlaying = 'y',
              side = 'right',
              range = c(vpd_min, vpd_max),
              fixedrange = TRUE,
              showgrid = FALSE,
              zeroline = TRUE,
              zerolinecolor = "black",
              zerolinewidth = 0.5,
              showline = TRUE,
              linecolor = "black"
            )
            base_layout$margin$r <- 80
          }
          
          # Override legend position so it's placed horizontally below the x-axis
          # to avoid overlapping with the x-axis line or labels
          base_layout$legend <- list(
            orientation = "h",
            yanchor = "top",
            y = -0.15,
            xanchor = "center",
            x = 0.5
          )
          
          p <- p %>%
            plotly::layout(
              plot_bgcolor = base_layout$plot_bgcolor,
              paper_bgcolor = base_layout$paper_bgcolor,
              xaxis = base_layout$xaxis,
              yaxis = base_layout$yaxis,
              yaxis2 = base_layout$yaxis2,
              showlegend = base_layout$showlegend,
              legend = base_layout$legend,
              uirevision = base_layout$uirevision,
              margin = base_layout$margin
            ) %>%
            apply_standard_plotly_config(filename = "changepoints_plot", add_csv_download = TRUE) %>%
            plotly::event_register("plotly_click")
          p

        }, error = function(e) {
          # Show error to user
          showNotification(
            paste("Error rendering changepoint plot:", e$message),
            type = "error",
            duration = 10
          )
          # Return empty plot on error
          plotly::plotly_empty(source = "changepoint_plot") %>% plotly::event_register("plotly_click")
        })
      })
    })

    # Render warning when gradient overlay is hidden due to sensor/method mismatch
    output$gradient_mismatch_warning <- renderUI({
      # Check if gradient method is selected
      baseline_method <- input$offset_model %||% "segment_minimum"

      if (baseline_method == "gradient" && !is.null(rv$dual_stable_detected_result)) {
        dual_results <- rv$dual_stable_detected_result

        # Get detection parameters
        detection_sensor <- dual_results$detection_sensor %||% "outer"
        detection_method <- dual_results$detection_method %||% "HRM"

        # Get display parameters
        display_sensor <- input$display_sensor %||% "outer"
        method <- input$detect_method_filter %||% "HRM"

        # Check for mismatch
        if (display_sensor != detection_sensor || method != detection_method) {
          div(
            class = "alert alert-warning",
            style = "margin-top: 10px; margin-bottom: 10px;",
            icon("exclamation-triangle"),
            tags$strong(" Gradient Overlay Hidden"), br(),
            sprintf(
              "Dual-criterion detection used %s sensor (%s method), but plot displays %s sensor (%s method). ",
              toupper(detection_sensor), detection_method,
              toupper(display_sensor), method
            ),
            tags$strong(
              sprintf(
                "Change plot to %s sensor and %s method to see gradient overlay.",
                toupper(detection_sensor), detection_method
              )
            )
          )
        } else {
          NULL  # No warning if match
        }
      } else {
        NULL  # No warning if not gradient method
      }
    })

    # Handle plot click events to populate manual changepoint date
    observeEvent(plotly::event_data("plotly_click", source = "changepoint_plot"), {
      click_data <- plotly::event_data("plotly_click", source = "changepoint_plot")

      if (!is.null(click_data)) {
        # Extract the x value (date)
        clicked_date <- as.Date(click_data$x)

        if (!is.na(clicked_date)) {
          # Update the manual tab date input
          updateDateInput(session, "changepoint_date", value = clicked_date)

          showNotification(
            sprintf("Date %s populated in Manual tab. Click 'Add Changepoint' to confirm.",
                   format(clicked_date, "%Y-%m-%d")),
            type = "message",
            duration = 3
          )
        }
      }
    })

    # ==================================================================
    # SPACING CORRECTION
    # ==================================================================

    observeEvent(input$run_correction, {
      req(vh_results(), wood_properties(), probe_config())

      vh_data <- vh_results()

      # Show waiter
      waiter <- waiter::Waiter$new(
        html = waiter::spin_fading_circles(),
        color = waiter::transparent(0.5)
      )
      waiter$show()
      on.exit(waiter$hide(), add = TRUE)

      tryCatch({
        # 1. Resolve Parameters
        offset_model    <- input$offset_model %||% "segment"
        correction_math <- input$correction_math %||% "burgess"
        wood_props      <- wood_properties()
        probe_conf      <- probe_config()

        # Resolve anchors based on model
        if (offset_model == "gradient") {
          if (is.null(rv$dual_stable_detected_result)) {
            stop("Gradient model requires dual-criterion changepoints. Please run detection first.")
          }
          anchors <- rv$dual_stable_detected_result$changepoints
        } else {
          # Segment model uses the confirm-list of dates
          anchors <- if (!is.null(rv$changepoints) && length(rv$changepoints) > 0) {
            # Convert list to vector of Dates
            as.Date(do.call(c, rv$changepoints))
          } else {
            as.Date(character(0)) # Empty segment (single whole range)
          }
        }

        # Resolve k value
        if (!is.null(wood_props$derived_properties) &&
            !is.null(wood_props$derived_properties$thermal_diffusivity_actual_cm2_s)) {
          k_value <- wood_props$derived_properties$thermal_diffusivity_actual_cm2_s
        } else if (!is.null(wood_props$wood_constants$thermal_diffusivity_default_cm2_s)) {
          k_value <- wood_props$wood_constants$thermal_diffusivity_default_cm2_s
        } else {
          k_value <- 0.0025 # Fallback
        }

        # Resolve probe spacing (distance between heater and thermistors, NOT radial depth)
        probe_spacing <- 0.5 # Default 0.5 cm
        if (!is.null(probe_conf)) {
          if (inherits(probe_conf, "ProbeConfiguration")) {
            # Safely extract via active binding if it's the R6 object
            val <- tryCatch(probe_conf$probe_spacing, error = function(e) NULL)
            if (!is.null(val)) probe_spacing <- val
          } else if (!is.null(probe_conf$probe_spacing)) {
            # Or from a standard list
            probe_spacing <- probe_conf$probe_spacing
          }
        }

        # 2. Call New Unified API
        # This handles both sensors internally and maintains the audit workflow
        corrected_data <- sapfluxr::apply_spacing_correction(
          vh_data = vh_data,
          changepoints = anchors,
          offset_model = offset_model,
          correction_math = correction_math,
          sensor_position = "both",
          hpv_method = "HRM",
          wood_properties = wood_props,
          probe_spacing = probe_spacing,
          measurement_time = 80,
          verbose = FALSE
        )

        # 3. Store Results
        # Extract metadata for the module state
        n_segments <- if(offset_model == "gradient") nrow(anchors) + 1 else (length(anchors) + 1)
        
        rv$correction_result <- list(
          vh_corrected = corrected_data,
          metadata = list(
            sensor_position = "both",
            offset_model = offset_model,
            correction_math = correction_math,
            n_segments = n_segments,
            changepoints = if(offset_model == "gradient") anchors$timestamp else anchors,
            date_applied = Sys.time(),
            approach = sprintf("%s (%s)", 
                               if(offset_model == "gradient") "Gradient Interpolation" else "Segment Minimum",
                               if(correction_math == "burgess") "Burgess" else "Linear")
          )
        )
        
        rv$corrected_vh <- corrected_data
        rv$correction_applied <- TRUE

        # Track spacing correction in the reproducible script
        if (!is.null(code_tracker)) {
          anchors_expr <- if (offset_model == "gradient") {
            ts <- format(anchors$timestamp, "%Y-%m-%d %H:%M:%S")
            sprintf(
              "data.frame(timestamp = as.POSIXct(c(%s)))",
              paste(sprintf('"%s"', ts), collapse = ", ")
            )
          } else if (length(anchors) > 0) {
            sprintf(
              "as.Date(c(%s))",
              paste(sprintf('"%s"', format(anchors, "%Y-%m-%d")), collapse = ", ")
            )
          } else {
            "as.Date(character(0))"
          }
          code_tracker$add_step(
            step_name = "Apply Spacing Correction",
            code = sprintf(paste0(
              "# Apply spacing correction (zero-flow baseline offset)\n",
              "vh_corrected <- sapfluxr::apply_spacing_correction(\n",
              "  vh_data = vh_results,\n",
              "  changepoints = %s,\n",
              "  offset_model = \"%s\",\n",
              "  correction_math = \"%s\",\n",
              "  sensor_position = \"both\",\n",
              "  hpv_method = \"HRM\",\n",
              "  wood_properties = wood_properties,\n",
              "  probe_spacing = %g,\n",
              "  measurement_time = 80,\n",
              "  verbose = FALSE\n)"),
              anchors_expr, offset_model, correction_math, probe_spacing),
            description = sprintf(
              "Spacing correction applied (%s offset, %s math, %d segment%s)",
              offset_model, correction_math, n_segments,
              if (n_segments != 1) "s" else "")
          )
        }

        # 4. Notifications
        showNotification(
          sprintf("Success: Applied %s correction (%s math) to both sensors.", 
                  toupper(offset_model), toupper(correction_math)),
          type = "message"
        )

      }, error = function(e) {
        showNotification(paste("Correction failed:", e$message), type = "error", duration = 10)
      })
    })

    # Has results flag
    output$has_correction_results <- reactive({
      !is.null(rv$correction_result)
    })
    outputOptions(output, "has_correction_results", suspendWhenHidden = FALSE)

    # Segment results table
    output$segment_results_table <- DT::renderDataTable({
      req(rv$correction_result)

      # Build segment results table with required columns
      changepoint_dates <- if (!is.null(rv$changepoints) && length(rv$changepoints) > 0) {
        as.Date(do.call(c, rv$changepoints))
      } else {
        NULL
      }

      # Calculate daily minima for baseline values using RAW data
      vh_data <- vh_results()

      # Determine sensor and method
      sensor <- input$detect_sensor_position %||% "outer"
      method <- input$detect_method_filter %||% "HRM"

      daily_min <- sapfluxr::calculate_daily_minima(
        vh_data = vh_data,
        sensor_position = sensor,
        method = method,
        vh_col = "Vh_cm_hr"  # Use raw column for baseline
      )

      # Get correction method name
      correction_method <- if (!is.null(rv$correction_result$metadata$method)) {
        rv$correction_result$metadata$method
      } else {
        "Spacing Correction"
      }

      # Create segments from changepoints
      if (is.null(changepoint_dates) || length(changepoint_dates) == 0) {
        # Single segment - get overall minimum
        baseline_vh <- if (nrow(daily_min) > 0 && any(!is.na(daily_min$min_value))) {
          round(min(daily_min$min_value, na.rm = TRUE), 2)
        } else {
          NA_real_
        }

        segment_data <- data.frame(
          Segment = 1,
          `Start Date` = format(min(vh_data$datetime, na.rm = TRUE), "%Y-%m-%d"),
          `End Date` = format(max(vh_data$datetime, na.rm = TRUE), "%Y-%m-%d"),
          `Previous Min Vh (cm/hr)` = baseline_vh,
          `Correction Applied` = correction_method,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      } else {
        # Multiple segments
        n_segments <- length(changepoint_dates) + 1
        segment_list <- vector("list", n_segments)

        for (i in 1:n_segments) {
          start_date <- if (i == 1) {
            as.Date(min(vh_data$datetime, na.rm = TRUE))
          } else {
            changepoint_dates[i-1]
          }

          end_date <- if (i == n_segments) {
            as.Date(max(vh_data$datetime, na.rm = TRUE))
          } else {
            changepoint_dates[i]
          }

          # Get daily min for this segment
          segment_daily <- daily_min[
            daily_min$date >= start_date &
            daily_min$date <= end_date,
          ]

          baseline_vh <- if (nrow(segment_daily) > 0 && any(!is.na(segment_daily$min_value))) {
            round(min(segment_daily$min_value, na.rm = TRUE), 2)
          } else {
            NA_real_
          }

          segment_list[[i]] <- data.frame(
            Segment = i,
            `Start Date` = format(start_date, "%Y-%m-%d"),
            `End Date` = format(end_date, "%Y-%m-%d"),
            `Previous Min Vh (cm/hr)` = baseline_vh,
            `Correction Applied` = correction_method,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
        }

        segment_data <- do.call(rbind, segment_list)
      }

      DT::datatable(
        segment_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE
      )
    })

    # Pre-filtered data for spacing plot
    spacing_plot_data <- reactive({
      req(rv$correction_result, rv$corrected_vh)

      vh_data_original <- vh_results()
      vh_data_corrected <- rv$corrected_vh

      # Get sensor and method from plot inputs (independent of changepoint detection inputs)
      sensor <- input$plot_sensor_position_spacing %||% "outer"
      method <- input$detect_method_filter %||% "HRM"

      # Filter datasets for selected sensor and method
      before <- vh_data_original[
        vh_data_original$sensor_position == sensor &
        vh_data_original$method == method,
      ]

      after <- vh_data_corrected[
        vh_data_corrected$sensor_position == sensor &
        vh_data_corrected$method == method,
      ]

      # Only sample if REALLY large (>50k points)
      if (nrow(after) > 50000) {
        sample_idx <- seq(1, nrow(after), length.out = 50000)
        after <- after[sample_idx, ]
      }
      if (nrow(before) > 50000) {
        sample_idx <- seq(1, nrow(before), length.out = 50000)
        before <- before[sample_idx, ]
      }

      list(before = before, after = after)
    })

    # Before/After Comparison Plot - initial render with base trace only
    output$plot_before_after <- plotly::renderPlotly({
      req(spacing_plot_data())

      plot_data <- spacing_plot_data()
      after <- plot_data$after

      # Get sensor and method from inputs
      sensor <- input$plot_sensor_position_spacing %||% "outer"
      method <- input$detect_method_filter %||% "HRM"

      # Validate filtered data
      if (nrow(after) == 0) {
        stop("No data found for sensor '", sensor, "' and method '", method,
             "'. Available sensors: ", paste(unique(after$sensor_position), collapse = ", "),
             "; Available methods: ", paste(unique(after$method), collapse = ", "))
      }

      # Determine which corrected column to use
      corrected_col <- if ("Vh_cm_hr_sc" %in% names(after) &&
                          sum(!is.na(after$Vh_cm_hr_sc)) > 0) {
        "Vh_cm_hr_sc"
      } else if ("Vh_cm_hr" %in% names(after)) {
        "Vh_cm_hr"
      } else {
        stop("No velocity column found in corrected data.")
      }

      # Get styling
      style_config <- plot_settings()
      style <- get_plot_style(method = method, sensor = sensor, is_corrected = TRUE, config = style_config)

      # Create plot with ONLY the corrected trace (base layer)
      p <- plotly::plot_ly(
        data = after,
        x = ~datetime,
        y = as.formula(paste0("~", corrected_col)),
        type = 'scatter',
        mode = 'lines',
        name = 'Spacing Corrected',
        line = style,
        fill = 'none',
        hovertemplate = paste(
          "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
          "<b>Corrected:</b> %{y:.2f} cm/hr<br>",
          "<extra></extra>"
        )
      )

      # Apply standard layout
      base_layout <- get_standard_layout(
        title = sprintf("Spacing Correction: Before vs After (%s, %s)", toupper(sensor), method),
        xtitle = "Date",
        ytitle = "Vh (cm/hr)",
        uirevision = "spacing_comparison_zoom"
      )
      
      p <- p %>%
        plotly::layout(
          title = base_layout$title,
          xaxis = base_layout$xaxis,
          yaxis = base_layout$yaxis,
          showlegend = base_layout$showlegend,
          legend = base_layout$legend,
          hovermode = base_layout$hovermode,
          uirevision = base_layout$uirevision,
          plot_bgcolor = base_layout$plot_bgcolor,
          paper_bgcolor = base_layout$paper_bgcolor,
          margin = base_layout$margin
        ) %>%
        apply_standard_plotly_config(filename = "spacing_correction_comparison", add_csv_download = TRUE)

      p
    })

    # Use plotlyProxy to add/remove raw data overlay (preserves zoom)
    observe({
      req(spacing_plot_data())
      
      plot_data <- spacing_plot_data()
      after <- plot_data$after

      # First, try to remove any existing raw data trace
      tryCatch({
        plotly::plotlyProxy("plot_before_after", session) %>%
          plotly::plotlyProxyInvoke("deleteTraces", list(1))
      }, error = function(e) {})

      # Then add it back if checkbox is ticked
      if (isTRUE(input$show_raw_overlay)) {
        raw_col <- if ("Vh_cm_hr_raw" %in% names(after)) "Vh_cm_hr_raw" else "Vh_cm_hr"

        plotly::plotlyProxy("plot_before_after", session) %>%
          plotly::plotlyProxyInvoke(
            "addTraces",
            list(
              x = after$datetime,
              y = after[[raw_col]],
              type = "scatter",
              mode = "lines",
              name = "Raw Data",
              line = list(color = "#d62728", width = 1.0),
              fill = "none",
              hovertemplate = paste(
                "<b>Date:</b> %{x|%Y-%m-%d %H:%M}<br>",
                "<b>Raw:</b> %{y:.2f} cm/hr<br>",
                "<extra></extra>"
              )
            )
          )
      }
    }) %>% bindEvent(input$show_raw_overlay, input$plot_sensor_position_spacing)

    # Correction summary
    output$correction_summary <- renderPrint({
      req(rv$correction_result)

      result <- rv$correction_result
      metadata <- result$metadata

      cat("SPACING CORRECTION SUMMARY\n")
      cat(strrep("=", 72), "\n\n")

      cat(sprintf("Sensor: %s\n", toupper(metadata$sensor_position)))
      cat(sprintf("Method: %s\n", metadata$method))
      cat(sprintf("k assumed: %.4f cm²/s\n", metadata$k_assumed))
      cat(sprintf("Probe spacing: %.1f cm\n", metadata$probe_spacing))
      cat(sprintf("Number of segments: %d\n", metadata$n_segments))
      cat(sprintf("Changepoints: %s\n",
                 if (length(metadata$changepoints) == 0) {
                   "None (single segment)"
                 } else {
                   paste(metadata$changepoints, collapse = ", ")
                 }))
      cat(sprintf("Date applied: %s\n", format(metadata$date_applied, "%Y-%m-%d %H:%M:%S")))
      cat(sprintf("Approach: %s\n", metadata$approach))

      cat("\n")
      cat(strrep("=", 72), "\n")
    })

    # Reset to uncorrected data
    observeEvent(input$reset_corrections, {
      rv$corrected_vh <- NULL
      rv$correction_applied <- FALSE
      showNotification(
        "Reset to uncorrected data.",
        type = "message",
        duration = 3
      )
    })

    # Correction status
    output$correction_status <- renderPrint({
      if (!rv$correction_applied || is.null(rv$correction_result)) {
        cat("✗ NO CORRECTIONS APPLIED\n")
        cat("──────────────────────────────────────\n")
        cat("Data shows original uncorrected velocities\n")

      } else {
        cat("✓ SEGMENT-BASED CORRECTIONS ACTIVE\n")
        cat("──────────────────────────────────────\n")

        metadata <- rv$correction_result$metadata

        cat(sprintf("Sensor: %s\n", toupper(metadata$sensor_position)))
        cat(sprintf("k used: %.4f cm²/s (assumed)\n", metadata$k_assumed))
        cat(sprintf("Segments: %d\n", metadata$n_segments))

        if (metadata$n_segments > 1) {
          cat(sprintf("Changepoints: %d\n", length(metadata$changepoints)))
        }

        n_corrected <- sum(rv$correction_result$vh_corrected$spacing_correction_applied, na.rm = TRUE)
        cat(sprintf("Data points corrected: %d\n", n_corrected))
      }
    })

    # Return corrected data for use in other modules.
    # `vh` is a passthrough (original data until a correction is committed) so
    # downstream stages keep working when spacing correction is skipped; `applied`
    # is the explicit "committed" signal that drives the stage-completion tick so
    # it only turns green once the user actually applies a correction.
    return(list(
      vh = reactive({
        if (is.null(rv$corrected_vh)) {
          vh_results()  # Return original if no correction applied
        } else {
          rv$corrected_vh  # Return corrected data
        }
      }),
      applied = reactive(isTRUE(rv$correction_applied))
    ))
  })
}
