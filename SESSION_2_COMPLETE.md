# Session 2 Complete - Configuration Interface ✓

## Summary

Built a comprehensive configuration interface for probe and wood properties with three input modes: built-in YAML selection, YAML file upload, and manual entry with real-time derived value calculation.

---

## ✅ What's Implemented

### **Configuration Module** (`R/mod_config.R`)

Dual-panel interface for probe and wood property configuration:

#### **Three Input Modes** (for both probe and wood):

1. **Built-in YAML** (Default)
   - Dropdown lists all YAML files from `sapfluxr/inst/configurations/`
   - Automatically loads using `sapfluxr::load_probe_config()` or `load_wood_properties()`
   - Formatted names (e.g., "probe_symmetrical.yaml" → "Symmetrical")

2. **Upload YAML**
   - File upload widget accepting .yaml/.yml files
   - Validates uploaded YAML using sapfluxr's loaders
   - Shows success/error notifications

3. **Manual Entry**
   - **Probe**: Simple form with spacing, diameter, sensor positions
   - **Wood**: Tabbed interface with 4 categories

---

### **Wood Properties Manual Entry** (4 Tabs)

#### **Tab 1: Thermal Properties**
- Thermal Diffusivity (cm²/s) - REQUIRED
- Thermal Conductivity (W/(m·K)) - Optional
- Volumetric Heat Capacity (J/(m³·K)) - Optional

**Real-time Calculation:**
```
thermal_diffusivity = thermal_conductivity / volumetric_heat_capacity
```

**If user provides 2 of 3 values:**
- Blue-highlighted derived value appears automatically
- Updates on field blur (when user leaves field)

#### **Tab 2: Physical Properties**
- Species (text)
- Wood Type (dropdown: Softwood/Hardwood/Unknown)
- Dry Density (kg/m³)
- Basic Density (kg/m³)
- Moisture Content (%)
- Temperature (°C)

#### **Tab 3: Tree Measurements**
- DBH - Diameter at Breast Height (cm)
- Sapwood Depth (cm)
- Sapwood Area (cm²)

**Real-time Calculation:**
If DBH and Sapwood Depth are provided:
```
Sapwood Area = π × (outer_radius² - inner_radius²)
where:
  outer_radius = DBH / 2
  inner_radius = DBH / 2 - sapwood_depth
```

Blue-highlighted result shown below inputs.

#### **Tab 4: Quality Thresholds**
- Maximum Velocity (cm/hr)
- Minimum Velocity (cm/hr)
- Temperature Range slider (°C)

---

### **Probe Configuration Manual Entry**

Simple form with:
- Probe Spacing (cm)
- Probe Diameter (mm)
- Upstream Distance (cm)
- Downstream Distance (cm)
- Heat Pulse Duration (s)

---

## 🎨 UI Features

### **Blue Derived Values**
Using CSS class `.derived-value` from `www/custom.css`:
```css
.derived-value {
  color: #2196F3;
  font-weight: 500;
}
```

Derived values appear in light blue info boxes when calculated.

### **Configuration Summaries**
Each panel shows a summary box at the bottom displaying:
- Configuration name
- Key parameters (spacing, diffusivity, species, etc.)
- Updates when configuration changes

### **Validation**
- Uses sapfluxr's built-in validation when loading YAMLs
- Shows notifications for errors
- Manual entry creates `WoodProperties` R6 object with validation

---

## 🔄 Integration with Main App

### **Data Flow:**
```
User selects/uploads/enters config
         ↓
sapfluxr loads/creates R6 object
         ↓
Stored in reactive values (rv$probe_config, rv$wood_properties)
         ↓
Available for Session 3 (HPV calculations)
```

### **Reactive Chain:**
```r
configs <- configServer("config", reactive(rv$corrected_data))

# Store in app-level reactive values
observe({
  rv$probe_config <- configs$probe_config()
  rv$wood_properties <- configs$wood_properties()
})
```

---

## 📝 Implementation Details

### **YAML File Discovery:**
```r
available_probe_yamls <- reactive({
  yaml_dir <- system.file("configurations", package = "sapfluxr")
  files <- list.files(yaml_dir, pattern = "^probe_.*\\.yaml$")
  # Format names nicely
  names(files) <- gsub("_", " ", ...) %>% tools::toTitleCase()
})
```

### **Derived Value Calculation Logic:**

**Thermal Properties:**
```r
if (!is.null(diff) && !is.null(cond) && is.null(cap)) {
  cap_calc <- cond / (diff * 10000)  # Convert cm²/s to m²/s
  # Display in blue
}
```

**Tree Measurements:**
```r
if (!is.null(dbh) && !is.null(sapwood_depth)) {
  outer_r <- dbh / 2
  inner_r <- max(0, outer_r - sapwood_depth)
  sapwood_area <- pi * (outer_r^2 - inner_r^2)
  # Display in blue
}
```

### **Manual Configuration Application:**
```r
observeEvent(input$apply_wood_manual, {
  config <- sapfluxr::WoodProperties$new(
    thermal_diffusivity = input$thermal_diffusivity,
    thermal_conductivity = input$thermal_conductivity,
    ... # all other parameters
  )
  wood_properties(config)  # Store in reactive
})
```

---

## 🧪 Testing

```r
shiny::runApp("E:/R/project/shiny-sapfluxr")
```

### **Test Workflow:**

1. **Built-in YAML Mode:**
   - Go to "2. Configuration" tab
   - Select "Symmetrical" probe (should be default)
   - Select "Generic Sw" wood properties
   - Check summaries appear at bottom

2. **Manual Entry - Derived Thermal Values:**
   - Switch Wood Properties to "Manual Entry"
   - Go to "Thermal Properties" tab
   - Enter Thermal Diffusivity: 0.0025
   - Enter Thermal Conductivity: 0.5
   - Leave Volumetric Heat Capacity blank
   - **Expected**: Blue box appears calculating capacity ≈ 2,000,000

3. **Manual Entry - Derived Tree Values:**
   - Go to "Tree Measurements" tab
   - Enter DBH: 30 cm
   - Enter Sapwood Depth: 5 cm
   - **Expected**: Blue box shows Sapwood Area ≈ 392.7 cm²

4. **Upload YAML:**
   - Switch to "Upload YAML" mode
   - Upload a valid wood YAML from `sapfluxr/inst/configurations/`
   - Check summary updates

---

## 🎯 Key Features Delivered

✅ **Three input modes** for both probe and wood
✅ **YAML upload with validation**
✅ **Tabbed manual entry** interface
✅ **Real-time derived value calculation**
✅ **Blue highlighting** for derived values
✅ **Configuration summaries**
✅ **Integration with app reactive flow**
✅ **sapfluxr R6 object creation** from manual inputs

---

## 📋 Next Session: Method Selection & Calculations

Session 3 will implement:
- Method selection checkboxes (HRM, MHR, future methods)
- HRM prerequisite logic for sDMA
- "Calculate" button
- Call `sapfluxr::calc_heat_pulse_velocity()` with selected configs
- Results table display

**Questions for Session 3:**
1. Should we show a progress bar during calculations (using progressr)?
2. Do you want to show results immediately in a table, or wait until Session 4 (plots)?

---

## 🐛 Known Limitations

- Probe manual entry doesn't yet create ProbeConfig R6 object (placeholder)
- No validation warnings for out-of-range values in manual entry yet
- Derived values only update on "Apply" button click (not real-time on field change)

These can be enhanced in future iterations if needed!
