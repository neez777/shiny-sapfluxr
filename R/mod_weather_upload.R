# Weather Data Upload Module
#
# Module for uploading, processing, and visualising environmental/weather data
# Integrates with sapfluxr::read_weather_data() and sapfluxr::calc_vpd()

#' Weather Upload UI
#'
#' @param id Module namespace ID
#'
#' @export
weatherUploadUI <- function(id) {
  ns <- NS(id)

  tagList(
    # File upload
    fileInput(
      ns("weather_file"),
      "Choose Weather Data CSV File",
      accept = c(".csv", "text/csv", "text/comma-separated-values")
    ),

    # Manual column specification (initially hidden)
    shinyjs::hidden(
      div(
        id = ns("manual_columns"),
        hr(),
        h4("Manual Column Specification"),
        p(class = "text-muted",
          "Auto-detection failed. Please specify columns manually."),

        fluidRow(
          column(6,
                 selectInput(
                   ns("datetime_col"),
                   "Datetime Column:",
                   choices = NULL
                 )
          ),
          column(6,
                 selectInput(
                   ns("temp_col"),
                   "Temperature Column:",
                   choices = NULL
                 )
          )
        ),

        fluidRow(
          column(6,
                 selectInput(
                   ns("rh_col"),
                   "Relative Humidity Column:",
                   choices = NULL
                 )
          ),
          column(6,
                 selectInput(
                   ns("pressure_col"),
                   "Pressure Column (Optional):",
                   choices = NULL
                 )
          )
        ),

        actionButton(
          ns("apply_manual"),
          "Apply Manual Columns",
          class = "btn-primary"
        )
      )
    ),

    # VPD calculation options
    shinyjs::hidden(
      div(
        id = ns("vpd_options"),
        hr(),
        h4("VPD Calculation"),

        checkboxInput(
          ns("calc_vpd"),
          "Calculate Vapor Pressure Deficit (VPD)",
          value = TRUE
        ),

        conditionalPanel(
          condition = "input.calc_vpd",
          ns = ns,

          fluidRow(
            column(6,
                   numericInput(
                     ns("magnus_coef"),
                     "Magnus coefficient (α):",
                     value = 17.27,
                     min = 15,
                     max = 20,
                     step = 0.01
                   )
            ),
            column(6,
                   numericInput(
                     ns("magnus_base_temp"),
                     "Magnus base temp (β, °C):",
                     value = 237.7,
                     min = 200,
                     max = 250,
                     step = 0.1
                   )
            )
          ),

          checkboxInput(
            ns("return_components"),
            "Include SVP and AVP in output",
            value = FALSE
          ),

          p(class = "text-muted small",
            "Default parameters (α=17.27, β=237.7°C) are optimal for 0-60°C. ",
            "Adjust for special conditions (e.g., sub-zero temperatures).")
        ),

        actionButton(
          ns("process_weather"),
          "Process Weather Data",
          class = "btn-success"
        )
      )
    ),

    # Data summary
    shinyjs::hidden(
      div(
        id = ns("data_summary"),
        hr(),
        h4("Weather Data Summary"),
        verbatimTextOutput(ns("summary_text")),

        h5("Preview"),
        DT::dataTableOutput(ns("preview_table"))
      )
    ),

    # Visualisation
    shinyjs::hidden(
      div(
        id = ns("visualisation"),
        hr(),
        h4("Weather Data Visualisation"),

        tabsetPanel(
          tabPanel(
            "Time Series",
            plotOutput(ns("timeseries_plot"), height = "500px")
          ),
          tabPanel(
            "VPD vs Temperature",
            plotOutput(ns("vpd_temp_plot"), height = "400px")
          ),
          tabPanel(
            "Diurnal Pattern",
            plotOutput(ns("diurnal_plot"), height = "400px")
          )
        )
      )
    ),

    # Download processed data
    shinyjs::hidden(
      div(
        id = ns("download_section"),
        hr(),
        downloadButton(
          ns("download_data"),
          "Download Processed Weather Data",
          class = "btn-info"
        )
      )
    )
  )
}


#' Weather Upload Server
#'
#' @param id Module namespace ID
#'
#' @return Reactive containing processed weather data
#'
#' @export
weatherUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    rv <- reactiveValues(
      raw_data = NULL,
      weather_data = NULL,
      auto_detect_failed = FALSE,
      column_mapping = NULL
    )

    # File upload handler
    observeEvent(input$weather_file, {
      req(input$weather_file)

      tryCatch({
        # Try auto-detection
        weather <- sapfluxr::read_weather_data(
          input$weather_file$datapath
        )

        rv$weather_data <- weather
        rv$auto_detect_failed <- FALSE

        # Extract column mapping
        rv$column_mapping <- attr(weather, "column_mapping")

        # Show success notification
        showNotification(
          "Weather data imported successfully!",
          type = "message",
          duration = 3
        )

        # Show VPD options
        shinyjs::show("vpd_options")

      }, error = function(e) {
        # Auto-detection failed - show manual column selection
        rv$raw_data <- readr::read_csv(input$weather_file$datapath, show_col_types = FALSE)
        rv$auto_detect_failed <- TRUE

        # Populate column choices
        col_names <- c("(None)" = "", names(rv$raw_data))

        updateSelectInput(session, "datetime_col", choices = col_names)
        updateSelectInput(session, "temp_col", choices = col_names)
        updateSelectInput(session, "rh_col", choices = col_names)
        updateSelectInput(session, "pressure_col", choices = col_names, selected = "")

        # Show manual column selection
        shinyjs::show("manual_columns")

        showNotification(
          paste("Auto-detection failed:", e$message, "\nPlease specify columns manually."),
          type = "warning",
          duration = 5
        )
      })
    })

    # Manual column application
    observeEvent(input$apply_manual, {
      req(rv$raw_data)
      req(input$datetime_col, input$temp_col, input$rh_col)

      tryCatch({
        weather <- sapfluxr::read_weather_data(
          input$weather_file$datapath,
          datetime_col = if (input$datetime_col != "") input$datetime_col else NULL,
          temp_col = if (input$temp_col != "") input$temp_col else NULL,
          rh_col = if (input$rh_col != "") input$rh_col else NULL,
          pressure_col = if (input$pressure_col != "") input$pressure_col else NULL
        )

        rv$weather_data <- weather
        rv$column_mapping <- list(
          datetime = input$datetime_col,
          temperature = input$temp_col,
          humidity = input$rh_col,
          pressure = input$pressure_col
        )

        # Hide manual selection, show VPD options
        shinyjs::hide("manual_columns")
        shinyjs::show("vpd_options")

        showNotification(
          "Weather data imported successfully!",
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        showNotification(
          paste("Import failed:", e$message),
          type = "error",
          duration = 5
        )
      })
    })

    # Process weather data (calculate VPD)
    observeEvent(input$process_weather, {
      req(rv$weather_data)

      tryCatch({
        if (input$calc_vpd) {
          rv$weather_data <- sapfluxr::calc_vpd(
            rv$weather_data,
            magnus_coef = input$magnus_coef,
            magnus_base_temp = input$magnus_base_temp,
            return_components = input$return_components
          )

          showNotification(
            "VPD calculated successfully!",
            type = "message",
            duration = 3
          )
        }

        # Show summary and visualisation
        shinyjs::show("data_summary")
        shinyjs::show("visualisation")
        shinyjs::show("download_section")

      }, error = function(e) {
        showNotification(
          paste("VPD calculation failed:", e$message),
          type = "error",
          duration = 5
        )
      })
    })

    # Summary output
    output$summary_text <- renderPrint({
      req(rv$weather_data)

      cat("Weather Data Summary\n")
      cat("====================\n\n")

      if (!is.null(attr(rv$weather_data, "source_file"))) {
        cat("Source file:", basename(attr(rv$weather_data, "source_file")), "\n")
      }

      cat("Records:", nrow(rv$weather_data), "\n")
      cat("Time range:", format(min(rv$weather_data$datetime)), "to",
          format(max(rv$weather_data$datetime)), "\n\n")

      cat("Columns:\n")
      cat(paste("  -", names(rv$weather_data), collapse = "\n"), "\n\n")

      if (!is.null(rv$column_mapping)) {
        cat("Column mapping:\n")
        cat("  Datetime:", rv$column_mapping$datetime %||% "(auto-detected)", "\n")
        cat("  Temperature:", rv$column_mapping$temperature %||% "(auto-detected)", "\n")
        cat("  Humidity:", rv$column_mapping$humidity %||% "(auto-detected)", "\n")
        if (!is.null(rv$column_mapping$pressure) && rv$column_mapping$pressure != "") {
          cat("  Pressure:", rv$column_mapping$pressure, "\n")
        }
      }

      if ("vpd_kpa" %in% names(rv$weather_data)) {
        cat("\nVPD Statistics:\n")
        cat("  Min:", sprintf("%.3f kPa", min(rv$weather_data$vpd_kpa, na.rm = TRUE)), "\n")
        cat("  Mean:", sprintf("%.3f kPa", mean(rv$weather_data$vpd_kpa, na.rm = TRUE)), "\n")
        cat("  Max:", sprintf("%.3f kPa", max(rv$weather_data$vpd_kpa, na.rm = TRUE)), "\n")
      }

      validation_issues <- attr(rv$weather_data, "validation_issues")
      if (!is.null(validation_issues) && length(validation_issues) > 0) {
        cat("\nValidation Issues:\n")
        cat(paste("  -", validation_issues, collapse = "\n"), "\n")
      }
    })

    # Preview table
    output$preview_table <- DT::renderDataTable({
      req(rv$weather_data)

      DT::datatable(
        rv$weather_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = which(sapply(rv$weather_data, is.numeric)), digits = 2)
    })

    # Time series plot
    output$timeseries_plot <- renderPlot({
      req(rv$weather_data)

      # Select variables to plot
      plot_vars <- c("air_temp_c", "relative_humidity")
      if ("vpd_kpa" %in% names(rv$weather_data)) {
        plot_vars <- c(plot_vars, "vpd_kpa")
      }

      plot_data <- rv$weather_data %>%
        select(datetime, all_of(plot_vars)) %>%
        tidyr::pivot_longer(
          cols = -datetime,
          names_to = "variable",
          values_to = "value"
        )

      # Create labels
      var_labels <- c(
        air_temp_c = "Air Temperature (°C)",
        relative_humidity = "Relative Humidity (%)",
        vpd_kpa = "VPD (kPa)"
      )

      ggplot(plot_data, aes(x = datetime, y = value)) +
        geom_line(color = "steelblue", linewidth = 0.8) +
        facet_wrap(
          ~variable,
          ncol = 1,
          scales = "free_y",
          labeller = labeller(variable = var_labels)
        ) +
        theme_minimal(base_size = 13) +
        labs(
          title = "Weather Variables Over Time",
          x = "Datetime",
          y = "Value"
        ) +
        theme(
          strip.text = element_text(face = "bold", size = 12),
          panel.grid.minor = element_blank()
        )
    })

    # VPD vs Temperature plot
    output$vpd_temp_plot <- renderPlot({
      req(rv$weather_data)
      req("vpd_kpa" %in% names(rv$weather_data))

      ggplot(rv$weather_data, aes(x = air_temp_c, y = vpd_kpa)) +
        geom_point(
          aes(color = relative_humidity),
          size = 2.5,
          alpha = 0.7
        ) +
        scale_color_gradient(
          low = "blue",
          high = "red",
          name = "RH (%)"
        ) +
        geom_smooth(
          method = "loess",
          se = TRUE,
          color = "black",
          linetype = "dashed",
          linewidth = 1
        ) +
        theme_minimal(base_size = 13) +
        labs(
          title = "VPD Relationship with Temperature and Humidity",
          x = "Air Temperature (°C)",
          y = "VPD (kPa)"
        ) +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank()
        )
    })

    # Diurnal pattern plot
    output$diurnal_plot <- renderPlot({
      req(rv$weather_data)
      req("vpd_kpa" %in% names(rv$weather_data))

      # Calculate hour of day
      weather_hourly <- rv$weather_data %>%
        mutate(
          hour = as.numeric(format(datetime, "%H")) +
                 as.numeric(format(datetime, "%M")) / 60
        ) %>%
        group_by(hour) %>%
        summarise(
          vpd_mean = mean(vpd_kpa, na.rm = TRUE),
          vpd_sd = sd(vpd_kpa, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) %>%
        mutate(
          vpd_se = vpd_sd / sqrt(n)
        )

      ggplot(weather_hourly, aes(x = hour, y = vpd_mean)) +
        geom_ribbon(
          aes(ymin = vpd_mean - vpd_se, ymax = vpd_mean + vpd_se),
          fill = "darkgreen",
          alpha = 0.2
        ) +
        geom_line(color = "darkgreen", linewidth = 1.2) +
        geom_point(color = "darkgreen", size = 3) +
        scale_x_continuous(breaks = seq(0, 24, 4)) +
        theme_minimal(base_size = 13) +
        labs(
          title = "Diurnal VPD Pattern",
          subtitle = "Mean ± SE",
          x = "Hour of Day",
          y = "VPD (kPa)"
        ) +
        theme(
          panel.grid.minor = element_blank()
        )
    })

    # Download handler
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("weather_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        readr::write_csv(rv$weather_data, file)
      }
    )

    # Return reactive weather data for use by other modules
    return(reactive({ rv$weather_data }))
  })
}
