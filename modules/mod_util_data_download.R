#' Data Export Module
#'
#' Shiny module that lets the user download any dataset produced by the
#' pipeline at any time, as CSV, a single R object (.rds), or named/lazy R
#' objects (.rda). Mirrors the Code Generator tool: it sits under the Tools
#' menu and reads the current pipeline state rather than driving it.
#'
#' @param id Module ID
#' @param datasets A reactive returning a named list of candidate datasets
#'   (label -> data.frame). Entries that are NULL or have zero rows are
#'   filtered out automatically, so callers can pass everything unconditionally.
#'

# Map of human-readable labels -> snake_case object/file names. The object name
# is used for .rda variable names and as the CSV/zip file stem, so it must be a
# valid R symbol.
DATA_EXPORT_OBJECT_NAMES <- c(
  "Raw heat-pulse measurements"    = "raw_measurements",
  "Aligned/corrected measurements" = "aligned_measurements",
  "Heat-pulse velocity (Vh)"       = "heat_pulse_velocity",
  "Sap flux density (Jv)"          = "sap_flux_density",
  "Tree water use (Q)"             = "tree_water_use",
  "Temporal aggregation"           = "aggregated_data",
  "Weather"                        = "weather_data"
)

# UI ----
dataDownloadUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    # Left column: export controls
    column(
      width = 4,
      box(
        width = 12,
        title = "Export Options",
        status = "primary",
        solidHeader = TRUE,

        p("Download any dataset produced so far. The list of available datasets ",
          "grows as you complete each step of the pipeline."),

        uiOutput(ns("dataset_picker")),

        hr(),

        radioButtons(
          ns("format"),
          "File format",
          choices = c(
            "CSV (.csv)"                 = "csv",
            "R single object (.rds)"     = "rds",
            "R named / lazy data (.rda)" = "rda"
          ),
          selected = "csv"
        ),

        helpText(
          tags$ul(
            style = "padding-left: 18px; margin-bottom: 0;",
            tags$li(strong("CSV"), " - universal; one file per dataset (zipped if several)."),
            tags$li(strong("RDS"), " - one R object via ", code("readRDS()"),
                    "; preserves date/time and factor types."),
            tags$li(strong("RDA"), " - reloads under original names via ", code("load()"),
                    " (the lazy ", code("data()"), " style).")
          )
        ),

        hr(),

        downloadButton(
          ns("download_data"),
          "Download",
          icon = icon("download"),
          class = "btn-success",
          style = "width: 100%;"
        )
      )
    ),

    # Right column: availability + preview
    column(
      width = 8,
      box(
        width = 12,
        title = "Preview",
        status = "primary",
        solidHeader = TRUE,

        uiOutput(ns("availability")),
        DT::DTOutput(ns("preview"))
      )
    )
  )
}

# Server ----
dataDownloadServer <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Available = non-NULL data frames with at least one row, in declared order.
    available <- reactive({
      ds <- datasets()
      if (is.null(ds) || length(ds) == 0) return(list())
      Filter(
        function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0,
        ds
      )
    })

    # Dataset chooser (only available datasets are offered; all selected by default).
    output$dataset_picker <- renderUI({
      av <- available()
      if (length(av) == 0) {
        return(
          p(em("No datasets available yet. Complete analysis steps to enable downloads."))
        )
      }
      checkboxGroupInput(
        ns("which"),
        "Datasets to include",
        choices  = names(av),
        selected = names(av)
      )
    })

    # Short availability note above the preview table.
    output$availability <- renderUI({
      av <- available()
      if (length(av) == 0) {
        return(p(em("Nothing to preview yet.")))
      }
      sel <- intersect(input$which, names(av))
      if (length(sel) == 0) {
        return(p(em("Select at least one dataset to enable the download.")))
      }
      p(
        sprintf(
          "%d dataset%s selected. Preview shows the first selected dataset (up to 100 rows).",
          length(sel), if (length(sel) == 1) "" else "s"
        )
      )
    })

    # Preview the first selected dataset.
    output$preview <- DT::renderDT({
      av <- available()
      sel <- intersect(input$which, names(av))
      req(length(sel) > 0)
      DT::datatable(
        utils::head(av[[sel[1]]], 100),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10, dom = "tip")
      )
    })

    # Resolve selected datasets to a named list keyed by snake_case object names.
    selected_objects <- reactive({
      av <- available()
      sel <- intersect(input$which, names(av))
      if (length(sel) == 0) return(list())
      objs <- av[sel]
      names(objs) <- unname(DATA_EXPORT_OBJECT_NAMES[sel])
      objs
    })

    output$download_data <- downloadHandler(
      filename = function() {
        objs <- selected_objects()
        stamp <- format(Sys.time(), "%Y%m%d_%H%M")
        fmt <- input$format
        if (fmt == "csv") {
          if (length(objs) == 1) {
            sprintf("%s_%s.csv", names(objs), stamp)
          } else {
            sprintf("sapfluxr_export_%s.zip", stamp)
          }
        } else if (fmt == "rds") {
          sprintf("sapfluxr_export_%s.rds", stamp)
        } else {
          sprintf("sapfluxr_export_%s.rda", stamp)
        }
      },
      content = function(file) {
        objs <- selected_objects()
        validate(need(length(objs) > 0, "Select at least one dataset to download."))
        fmt <- input$format

        if (fmt == "csv") {
          if (length(objs) == 1) {
            utils::write.csv(objs[[1]], file, row.names = FALSE)
          } else {
            # Bundle multiple CSVs into a single zip with flat file names.
            tmp <- tempfile("sapfluxr_csv_")
            dir.create(tmp)
            on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
            csv_names <- paste0(names(objs), ".csv")
            for (i in seq_along(objs)) {
              utils::write.csv(objs[[i]], file.path(tmp, csv_names[i]), row.names = FALSE)
            }
            if (requireNamespace("zip", quietly = TRUE)) {
              zip::zipr(file, files = csv_names, root = tmp)
            } else {
              utils::zip(file, files = file.path(tmp, csv_names), flags = "-j9X")
            }
          }

        } else if (fmt == "rds") {
          # One object stays a bare data frame; several become a named list.
          if (length(objs) == 1) {
            saveRDS(objs[[1]], file)
          } else {
            saveRDS(objs, file)
          }

        } else {
          # .rda: assign each object under its snake_case name, then save() them
          # so load() restores the original names (the lazy data() style).
          e <- new.env(parent = emptyenv())
          for (nm in names(objs)) assign(nm, objs[[nm]], envir = e)
          save(list = names(objs), file = file, envir = e)
        }
      }
    )
  })
}
