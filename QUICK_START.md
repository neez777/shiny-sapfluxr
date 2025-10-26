# Quick Start Guide - Shiny Sap Flow Analyser

## Session 1 Complete! ✓

You now have a working foundation for the Shiny app with:
- ✅ Project structure
- ✅ Dashboard layout with sidebar navigation
- ✅ Data upload module
- ✅ Clock drift correction module
- ✅ Custom CSS styling

## Testing the App

### 1. Install Dependencies

First, make sure all required packages are installed:

```r
# Install from CRAN
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
  "ggplot2"
))

# Install webshot2 for plot exports (will be used in Session 6)
install.packages("webshot2")

# Make sure sapfluxr is installed
devtools::install("E:/R/project/sapfluxr")
```

### 2. Load and Test sapfluxr

Before running the Shiny app, verify sapfluxr is working:

```r
library(sapfluxr)

# Check that key functions are available
?read_heat_pulse_data
?fix_clock_drift
?calc_heat_pulse_velocity
```

### 3. Run the Shiny App

From R console:

```r
# Option 1: Run from directory
setwd("E:/R/project/shiny-sapfluxr")
shiny::runApp()

# Option 2: Specify path
shiny::runApp("E:/R/project/shiny-sapfluxr")
```

### 4. Test the Upload Module

1. Navigate to **"1. Data Upload"** tab
2. Click **"Browse..."** and select a heat pulse data file
3. The app should:
   - Read the file using `sapfluxr::read_heat_pulse_data()`
   - Display file metadata (filename, format, pulse count, date range)
   - Show validation results (OK/WARNING/ERROR)
4. Check the **"Data Summary"** box at the bottom for detailed output

### 5. Test Clock Drift Correction (Optional)

1. After uploading data, scroll to **"Clock Drift Correction"** panel
2. Enter two reference points:
   - **Device Time**: What the logger showed
   - **Actual Time**: What the actual time was
3. Click **"Apply Clock Drift Correction"**
4. You should see a success message if correction was applied

## What's Currently Working

### ✅ Implemented (Session 1)

- **Dashboard Layout**: Clean 6-step workflow navigation
- **Data Upload**:
  - File upload with drag-and-drop area
  - Automatic format detection (JSON, CSV, legacy)
  - Validation results display
  - Metadata summary
- **Clock Drift Correction**:
  - Two reference point inputs (date + time)
  - Time validation (HH:MM:SS format)
  - Applies to both measurements and diagnostics
  - Success/error notifications

### ⬜ Placeholder Tabs (Future Sessions)

- **2. Configuration**: YAML selector + manual entry (Session 2)
- **3. Methods**: Method selection checkboxes (Session 3)
- **4. Calculate**: Run HPV calculations (Session 3)
- **5. Visualise**: Interactive plots (Sessions 4-5)
- **6. Export**: Download plots and data (Session 6)

## File Structure Created

```
shiny-sapfluxr/
├── app.R                      # Main app - dashboard layout
├── DESCRIPTION                # Package dependencies
├── README.md                  # Project documentation
├── QUICK_START.md            # This file!
│
├── R/                         # Shiny modules
│   ├── mod_data_upload.R     # ✅ Upload & validate data
│   ├── mod_clock_drift.R     # ✅ Clock correction
│   └── utils.R               # ✅ Helper functions
│
├── www/                       # Web assets
│   └── custom.css            # ✅ Custom styling (includes .derived-value class)
│
└── inst/
    └── example_data/          # (Empty - you can add test data here)
```

## Troubleshooting

### App won't start
- **Error: could not find function "read_heat_pulse_data"**
  - Solution: Install sapfluxr with `devtools::install("E:/R/project/sapfluxr")`

### Upload fails
- **Error reading file**
  - Check that your data file is in a supported format
  - Try running `sapfluxr::read_heat_pulse_data("path/to/file.txt")` directly to see the error

### Clock drift correction fails
- **Time validation error**
  - Make sure times are in **HH:MM:SS** format (e.g., `12:00:00`, not `12:00`)
  - Valid examples: `00:00:00`, `09:30:45`, `23:59:59`

## Next Steps

### Session 2: Configuration System
Will implement:
- Dropdown to select probe YAML configs
- Dropdown to select wood property YAMLs
- Manual entry tabs (Probe Settings / Wood Properties)
- **Derived value calculation with blue highlighting** ← Your requested feature!
- Live validation of config values

### Session 3: Method Selection & Calculations
Will implement:
- Checkboxes for method selection (HRM, MHR, future: HRMXa, etc.)
- "HRM required for sDMA" logic
- Calculate button
- Results table preview

### Sessions 4-5: Interactive Visualisation
Will implement:
- Plotly time series with range slider
- Method toggle checkboxes
- Quality flag vertical lines
- Click-to-view pulse traces ← Your key requested feature!

### Session 6: Export
Will implement:
- PNG/PDF/TIF download
- HTML interactive export
- CSV results export

## Notes for Development

- All business logic stays in `sapfluxr` package
- Shiny app is just a UI wrapper
- Modules keep code organised and reusable
- Custom CSS in `www/custom.css` for styling (already includes `.derived-value` class for blue text!)

## Questions or Issues?

Test the app and let me know if you encounter any issues before we move on to Session 2!

---

**Session 1 Status**: ✅ Complete
**Next**: Session 2 - Configuration Interface
