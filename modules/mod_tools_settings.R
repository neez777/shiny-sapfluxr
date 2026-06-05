# mod_tools_settings.R
# Module for Plot Style and Color Settings
#
# Allows users to customize the visual appearance of plots across the application.
# Settings are saved to a YAML configuration file.

# UI ----
settingsUI <- function(id) {
  ns <- NS(id)

  # Helper to create a method row in the settings grid
  method_row <- function(method_id, label, default_outer, default_inner) {
    fluidRow(
      style = "margin-bottom: 5px; align-items: center; display: flex;",
      column(1, strong(label)),
      column(2, textInput(ns(paste0("col_", method_id, "_outer")), NULL, value = default_outer, width = "100%")),
      column(2, textInput(ns(paste0("col_", method_id, "_inner")), NULL, value = default_inner, width = "100%")),
      column(1, numericInput(ns(paste0("width_", method_id, "_raw")), NULL, value = 1.0, min = 0.1, max = 5, step = 0.1, width = "100%")),
      column(2, selectInput(ns(paste0("dash_", method_id, "_raw")), NULL, choices = c("solid", "dash", "dot"), selected = "solid", width = "100%")),
      column(1, numericInput(ns(paste0("width_", method_id, "_corr")), NULL, value = 0.7, min = 0.1, max = 5, step = 0.1, width = "100%")),
      column(2, selectInput(ns(paste0("dash_", method_id, "_corr")), NULL, choices = c("solid", "dash", "dot"), selected = "solid", width = "100%"))
    )
  }

  tagList(
    tags$style(HTML("
      .color-input-active {
        transition: background-color 0.3s ease;
        font-weight: bold;
      }
    ")),
    fluidRow(
      column(
        width = 12,
        box(
          width = NULL,
          title = "Plot Visual Settings",
          status = "primary",
          solidHeader = TRUE,
          
          p("Customise the colours, line thickness, and styles for all time-series plots. Changes will be applied globally once saved."),
          
          hr(),
          
          h4("Method Colours"),
          helpText("Outer sensors typically use a darker version of the colour, while Inner sensors use a lighter version."),
          
          # Headers for the grid
          fluidRow(
            style = "margin-bottom: 10px;",
            column(1, ""),
            column(2, strong("Line Colour"), br(), "Outer:"),
            column(2, br(), "Inner:"),
            column(3, strong("Raw/Uncorrected Data:"), br(), fluidRow(column(6, "Line Thickness"), column(6, "Style:"))),
            column(3, strong("Corrected Data:"), br(), fluidRow(column(6, "Line Thickness"), column(6, "Style:")))
          ),
          
          method_row("hrm", "HRM", "#1f77b4", "#aec7e8"),
          method_row("mhr", "MHR", "#ff7f0e", "#ffbb78"),
          method_row("tmaxklu", "Tmax_Klu", "#2ca02c", "#98df8a"),
          method_row("tmaxcoh", "Tmax_Coh", "#d62728", "#ff9896"),
          method_row("sdmatmaxklu", "sDMA:Tmax_Klu", "#9467bd", "#c5b0d5"),
          method_row("sdmamhr", "sDMA:MHR", "#e377c2", "#f7b6d2"),
          method_row("sdmatmaxcoh", "sDMA:Tmax_Coh", "#8c564b", "#c49c94"),
          
          hr(),
          
          h4("VPD Trace Style"),
          fluidRow(
            column(4, textInput(ns("col_vpd"), "Colour:", value = "#000000")),
            column(4, numericInput(ns("width_vpd"), "Width:", value = 1.0, min = 0.1, max = 5, step = 0.1)),
            column(4, selectInput(ns("dash_vpd"), "Style:", choices = c("solid", "dash", "dot", "dashdot"), selected = "dash"))
          ),
          
          hr(),
          
          h4("Peclet Number Trace Style"),
          fluidRow(
            column(4, textInput(ns("col_peclet"), "Colour:", value = "#666666")),
            column(4, numericInput(ns("width_peclet"), "Width:", value = 1.0, min = 0.1, max = 5, step = 0.1)),
            column(4, selectInput(ns("dash_peclet"), "Style:", choices = c("solid", "dash", "dot", "dashdot"), selected = "dot"))
          ),
          
          br(),
          
          actionButton(ns("save_settings"), "Save Plot Settings", icon = icon("save"), class = "btn-success", width = "100%", style = "background-color: #a0af6f; border-color: #a0af6f;"),
          br(), br(),
          actionButton(ns("reset_to_default"), "Reset to Defaults", icon = icon("undo"), class = "btn-warning", width = "100%", style = "background-color: #78909c; border-color: #78909c;")
        )
      )
    )
  )
}

# Server ----
settingsServer <- function(id, plot_settings_rv) {
  moduleServer(id, function(input, output, session) {
    
    # Path to YAML config
    config_path <- "inst/configurations/plot_settings.yaml"
    
    # List of all color input IDs
    color_input_ids <- c(
      "col_hrm_outer", "col_hrm_inner",
      "col_mhr_outer", "col_mhr_inner",
      "col_tmaxklu_outer", "col_tmaxklu_inner",
      "col_tmaxcoh_outer", "col_tmaxcoh_inner",
      "col_sdmatmaxklu_outer", "col_sdmatmaxklu_inner",
      "col_sdmamhr_outer", "col_sdmamhr_inner",
      "col_sdmatmaxcoh_outer", "col_sdmatmaxcoh_inner",
      "col_vpd", "col_peclet"
    )

    # Function to update input background color based on hex value
    update_input_color <- function(input_id, color_hex) {
      if (is.null(color_hex) || color_hex == "" || !grepl("^#", color_hex)) return()
      
      # Basic brightness check to decide text color (black or white)
      tryCatch({
        # Remove # and convert to RGB
        hex <- gsub("#", "", color_hex)
        if (nchar(hex) == 6) {
          r <- strtoi(substr(hex, 1, 2), 16)
          g <- strtoi(substr(hex, 3, 4), 16)
          b <- strtoi(substr(hex, 5, 6), 16)
          
          brightness <- (r * 0.299 + g * 0.587 + b * 0.114)
          text_col <- if (brightness > 128) "#000000" else "#ffffff"
          
          shinyjs::runjs(sprintf("
            var el = $('#%s');
            el.css('background-color', '%s');
            el.css('color', '%s');
            el.addClass('color-input-active');
          ", session$ns(input_id), color_hex, text_col))
        }
      }, error = function(e) NULL)
    }

    # Observe each color input and update its background
    lapply(color_input_ids, function(id) {
      observe({
        update_input_color(id, input[[id]])
      })
    })

    # Helper to update a method row
    update_method_row <- function(method_id, method_key, settings) {
      m_settings <- settings$methods[[method_key]]
      updateTextInput(session, paste0("col_", method_id, "_outer"), value = m_settings$outer %||% "#000000")
      updateTextInput(session, paste0("col_", method_id, "_inner"), value = m_settings$inner %||% "#000000")
      updateNumericInput(session, paste0("width_", method_id, "_raw"), value = m_settings$raw_width %||% 1.0)
      updateSelectInput(session, paste0("dash_", method_id, "_raw"), selected = m_settings$raw_style %||% "solid")
      updateNumericInput(session, paste0("width_", method_id, "_corr"), value = m_settings$corrected_width %||% 0.7)
      updateSelectInput(session, paste0("dash_", method_id, "_corr"), selected = m_settings$corrected_style %||% "solid")
    }

    # Load settings from YAML into inputs on start
    observe({
      settings <- plot_settings_rv()
      req(settings)
      
      update_method_row("hrm", "HRM", settings)
      update_method_row("mhr", "MHR", settings)
      update_method_row("tmaxklu", "Tmax_Klu", settings)
      update_method_row("tmaxcoh", "Tmax_Coh", settings)
      update_method_row("sdmatmaxklu", "sDMA:Tmax_Klu", settings)
      update_method_row("sdmamhr", "sDMA:MHR", settings)
      update_method_row("sdmatmaxcoh", "sDMA:Tmax_Coh", settings)
      
      # VPD
      updateTextInput(session, "col_vpd", value = settings$special_traces$vpd$color %||% "#000000")
      updateNumericInput(session, "width_vpd", value = settings$special_traces$vpd$width %||% 1.0)
      updateSelectInput(session, "dash_vpd", selected = settings$special_traces$vpd$dash %||% "dash")

      # Peclet
      updateTextInput(session, "col_peclet", value = settings$special_traces$peclet$color %||% "#666666")
      updateNumericInput(session, "width_peclet", value = settings$special_traces$peclet$width %||% 1.0)
      updateSelectInput(session, "dash_peclet", selected = settings$special_traces$peclet$dash %||% "dot")
    })
    
    # Save settings
    observeEvent(input$save_settings, {
      get_method_data <- function(method_id) {
        list(
          outer = input[[paste0("col_", method_id, "_outer")]],
          inner = input[[paste0("col_", method_id, "_inner")]],
          raw_width = input[[paste0("width_", method_id, "_raw")]],
          raw_style = input[[paste0("dash_", method_id, "_raw")]],
          corrected_width = input[[paste0("width_", method_id, "_corr")]],
          corrected_style = input[[paste0("dash_", method_id, "_corr")]]
        )
      }

      new_settings <- list(
        methods = list(
          HRM = get_method_data("hrm"),
          MHR = get_method_data("mhr"),
          Tmax_Klu = get_method_data("tmaxklu"),
          Tmax_Coh = get_method_data("tmaxcoh"),
          `sDMA:Tmax_Klu` = get_method_data("sdmatmaxklu"),
          `sDMA:MHR` = get_method_data("sdmamhr"),
          `sDMA:Tmax_Coh` = get_method_data("sdmatmaxcoh")
        ),
        special_traces = list(
          vpd = list(
            color = input$col_vpd,
            width = input$width_vpd,
            dash = input$dash_vpd
          ),
          peclet = list(
            color = input$col_peclet,
            width = input$width_peclet,
            dash = input$dash_peclet
          )
        )
      )
      
      # Save to YAML
      tryCatch({
        yaml::write_yaml(new_settings, config_path)
        # Update reactive value to trigger plot updates
        plot_settings_rv(new_settings)
        showNotification("Settings saved successfully and applied globally!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error saving settings:", e$message), type = "error")
      })
    })
    
    # Reset to defaults
    observeEvent(input$reset_to_default, {
      default_method <- function(outer, inner) {
        list(outer = outer, inner = inner, raw_width = 1.0, raw_style = "solid", corrected_width = 0.7, corrected_style = "solid")
      }

      default_settings <- list(
        methods = list(
          HRM = default_method("#1f77b4", "#aec7e8"),
          MHR = default_method("#ff7f0e", "#ffbb78"),
          Tmax_Klu = default_method("#2ca02c", "#98df8a"),
          Tmax_Coh = default_method("#d62728", "#ff9896"),
          `sDMA:Tmax_Klu` = default_method("#9467bd", "#c5b0d5"),
          `sDMA:MHR` = default_method("#e377c2", "#f7b6d2"),
          `sDMA:Tmax_Coh` = default_method("#8c564b", "#c49c94")
        ),
        special_traces = list(
          vpd = list(color = "#000000", width = 1.0, dash = "dot"),
          peclet = list(color = "#666666", width = 1.0, dash = "dot")
        )
      )
      
      yaml::write_yaml(default_settings, config_path)
      plot_settings_rv(default_settings)
      showNotification("Settings reset to defaults.", type = "message")
    })
    
  })
}
