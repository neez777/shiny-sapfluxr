# Shiny Sap Flow Analyser

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Interactive web application for processing and visualising heat pulse velocity data from ICT SFM1x sensors.

## Overview

This Shiny application provides an easy-to-use interface for students and researchers to:
- Load heat pulse data from ICT sensors (JSON, CSV, or legacy formats)
- Correct for clock drift
- Configure probe and wood properties (YAML or manual entry)
- Calculate heat pulse velocity using multiple methods
- Visualise results with interactive time series plots
- Examine individual pulse traces
- Export publication-quality plots (PNG, PDF, TIF, HTML)

Built on the [sapfluxr](../sapfluxr) R package.

## Installation

### Prerequisites

```r
# Install required packages
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "plotly",
  "DT",
  "dplyr",
  "tidyr",
  "purrr",
  "yaml",
  "lubridate",
  "ggplot2",
  "webshot2"
))

# Install sapfluxr (from local directory)
devtools::install("../sapfluxr")
```

## Running the App

### Option 1: Local Development
```r
# From the shiny-sapfluxr directory
shiny::runApp()
```

### Option 2: From R Console
```r
# From anywhere
shiny::runApp("path/to/shiny-sapfluxr")
```

### Option 3: Load as Package (future)
```r
library(shinysapfluxr)
launch_app()
```

## Features

### Data Import
- Automatic format detection (ICT JSON, CSV, legacy formats)
- Data validation with quality checks
- Optional clock drift correction

### Configuration
- **YAML Mode**: Select pre-defined probe and wood property configurations
- **Manual Mode**: Enter custom parameters with real-time derived value calculation
- Configurations include:
  - Probe geometry and sensor positions
  - Wood thermal and physical properties
  - Tree measurements (DBH, sapwood depth)

### Heat Pulse Velocity Methods
- **HRM** (Heat Ratio Method) - Low/reverse flows
- **MHR** (Maximum Heat Ratio) - Moderate to high flows
- *(Future)* HRMXa, HRMXb, Tmax variants, sDMA, CHPM, DRM

### Interactive Visualisation
- **Time Series Plot**: Interactive plotly chart with:
  - Multiple method comparison
  - Toggle methods on/off
  - Range slider for date selection
  - Quality flag indicators
  - Hover tooltips

- **Pulse Trace Viewer**: Click on time series to view:
  - Individual pulse temperature traces
  - All four thermistor readings (do, di, uo, ui)
  - Pulse diagnostics and metadata

### Export Options
- **Plots**: PNG, PDF, TIF (publication quality), HTML (interactive)
- **Data**: Results as CSV, configurations as YAML

## Workflow

1. **Upload Data** → Load heat pulse data file
2. **Correct Clock** (optional) → Fix timestamp drift
3. **Configure** → Select or enter probe/wood properties
4. **Select Methods** → Choose calculation methods
5. **Calculate** → Run heat pulse velocity analysis
6. **Visualise** → Explore interactive time series and pulse traces
7. **Export** → Download plots and results

## Project Structure

```
shiny-sapfluxr/
├── app.R                     # Main application entry point
├── DESCRIPTION               # Package dependencies
├── R/                        # Shiny modules
│   ├── mod_data_upload.R    # Data import module
│   ├── mod_clock_drift.R    # Clock drift correction
│   ├── mod_config.R         # Configuration interface
│   ├── mod_method_select.R  # Method selection
│   ├── mod_calculate.R      # Calculation engine
│   ├── mod_plot_timeseries.R # Time series visualisation
│   ├── mod_plot_pulse.R     # Pulse trace viewer
│   ├── mod_export.R         # Export functionality
│   └── utils.R              # Helper functions
├── www/                      # Web assets
│   └── custom.css           # Custom styling
└── inst/
    └── example_data/         # Sample datasets
```

## Development

This app is under active development. See the main [sapfluxr CLAUDE.md](../sapfluxr/CLAUDE.md) for development guidelines.

### Current Status
- ✅ Project structure
- 🚧 Data upload module
- 🚧 Clock drift correction
- ⬜ Configuration interface
- ⬜ Method selection
- ⬜ Calculation engine
- ⬜ Time series plotting
- ⬜ Pulse trace viewer
- ⬜ Export functionality

## License

GPL-3

## Authors

- Grant Joyce (Developer)
- Gavan McGrath (Original R script development)
- Tim Bleby (Scientific review and Excel workflow design)

## Issues & Feedback

Please report issues at: https://github.com/neez777/sapfluxr/issues
