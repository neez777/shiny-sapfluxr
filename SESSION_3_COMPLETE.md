# Session 3 Complete - Method Selection & Calculations ✓

## Summary

Built a comprehensive method selection and calculation engine that lets users choose HPV calculation methods, apply sDMA post-processing, and run calculations with progress reporting. Results are displayed in an interactive table.

---

## ✅ What's Implemented

### **Methods Selection Module** (`R/mod_methods.R`)

#### **Method Selection** (Checkboxes)
Users can select multiple methods simultaneously:
- ✅ **HRM** - Heat Ratio Method (low/reverse flows)
- ✅ **MHR** - Maximum Heat Ratio (moderate to high flows)
- ✅ **HRMXa** - Modified HRM variant A
- ✅ **HRMXb** - Modified HRM variant B
- ✅ **Tmax (Cohen)** - Time-to-peak method
- ✅ **Tmax (Kluitenberg)** - Time-to-peak method

**Default selection**: HRM + MHR (most common combination)

#### **sDMA Post-Processing**
- ✅ Checkbox to enable sDMA (Selectable Dual Method Approach)
- ✅ Dropdown to select secondary method (MHR, Tmax_Coh, Tmax_Klu, HRMXa, HRMXb)
- ✅ **HRM Prerequisite Validation**:
  - Warning appears if sDMA is checked but HRM isn't selected
  - Calculate button validates before running
  - Clear error message if validation fails

---

## 🎯 Key Features

### **1. Intelligent Warnings**
**sDMA without HRM:**
```
⚠️ Warning: sDMA requires HRM to be selected. Please check HRM above.
```
Shows in orange warning box when sDMA is enabled but HRM isn't selected.

### **2. Pre-Calculation Status**
Before clicking Calculate, shows:
- ✅ Data filename
- ✅ Number of pulses
- ✅ Probe configuration name
- ✅ Wood properties name

### **3. Progress Reporting**
Uses `progressr::with_progress()` to show:
- Calculation progress (from sapfluxr)
- sDMA processing progress (if enabled)
- Notifications during calculation
- Success/error messages

### **4. Calculation Engine**
Calls sapfluxr directly:
```r
results <- progressr::with_progress({
  sapfluxr::calc_heat_pulse_velocity(
    heat_pulse_data = data,
    methods = input$methods,
    probe_config = probe,
    wood_properties = wood,
    confirm_parameters = FALSE  # No interactive prompts in Shiny
  )
})

# Apply sDMA if requested
if (input$apply_sdma) {
  results <- sapfluxr::apply_sdma_processing(
    vh_results = results,
    secondary_method = input$sdma_secondary
  )
}
```

### **5. Results Summary**
After calculation, displays:
- ✅ Total measurements calculated
- ✅ Methods used
- ✅ Date range of results
- ✅ Prompt to proceed to visualization

### **6. Interactive Results Table**
- **DT::datatable** with:
  - Sortable columns
  - Searchable data
  - Pagination (25 rows per page)
  - Horizontal scroll for many columns
  - Formatted datetime and numeric values

**Columns displayed:**
- datetime
- pulse_id
- method
- sensor_position (inner/outer)
- Vh_cm_hr (velocity in cm/hr)
- quality_flag
- peclet_number (if HRM calculated)
- calc_window_start_sec, calc_window_end_sec
- calc_time_sec
- selected_method (if sDMA applied)

---

## 🔄 Data Flow

```
User selects methods
       ↓
User clicks "Calculate"
       ↓
Validate requirements (HRM for sDMA)
       ↓
Get data + configs from reactive values
       ↓
Call sapfluxr::calc_heat_pulse_velocity()
  (with progress reporting)
       ↓
Apply sDMA if requested
  (with progress reporting)
       ↓
Store results in vh_results reactive
       ↓
Display summary + table
       ↓
Results available for Session 4 (plots)
```

---

## 📋 Integration with App

### **Reactive Chain:**
```r
# Methods module returns vh_results reactive
vh_results <- methodsServer(
  "methods",
  heat_pulse_data = reactive(rv$corrected_data),
  probe_config = configs$probe_config,
  wood_properties = configs$wood_properties
)

# Store in app-level reactive values
observe({
  rv$vh_results <- vh_results()
})
```

### **UI Placement:**
- Combines **Tab 3 (Methods)** and **Tab 4 (Calculate)**
- Tab 4 now just shows redirect message
- All functionality in Tab 3 for cleaner workflow

---

## 🧪 Testing

```r
shiny::runApp("E:/R/project/shiny-sapfluxr")
```

### **Test Workflow:**

1. **Upload data** (Tab 1)
2. **Configure** probe & wood properties (Tab 2)
3. **Select methods** (Tab 3):
   - Check HRM, MHR, Tmax_Klu
   - Enable sDMA checkbox
   - Select "MHR" as secondary method
4. **Click Calculate**
   - Should show progress notification
   - Wait for calculation (may take time with large datasets)
   - Should show success message
5. **View results:**
   - Summary box shows method count, date range
   - Table shows detailed results (sortable, searchable)
6. **Test sDMA validation:**
   - Uncheck HRM
   - Enable sDMA
   - Warning should appear
   - Click Calculate → should show error

---

## 💡 Smart Features

### **1. No Configuration? Uses Defaults**
If user skips Tab 2:
- Uses default symmetric probe
- Uses default generic softwood properties
- Calculation still works!

### **2. Method Descriptions**
Checkbox labels include brief descriptions:
```
☑ HRM - Heat Ratio Method (low/reverse flows)
☑ MHR - Maximum Heat Ratio (moderate to high flows)
```

### **3. sDMA Info Text**
When sDMA is enabled:
```
ℹ️ sDMA requires HRM and at least one other method. It automatically
switches between HRM (low flows) and your selected secondary method
(high flows) based on Péclet number.
```

### **4. Error Handling**
Comprehensive try-catch:
- File read errors
- Configuration errors
- Calculation errors
- sDMA processing errors

All show user-friendly notifications.

---

## 🎨 UI Design

### **Two-Column Layout:**

**Left Column:**
- Method checkboxes
- sDMA options
- Warning messages

**Right Column:**
- Pre-calculation status
- Calculate button (big, green, full-width)
- Results summary (green success box)

**Bottom Row:**
- Collapsible results table (starts collapsed)
- Expands to show full data

---

## 📊 Results Data Structure

The `vh_results` reactive contains a tibble from sapfluxr:

```r
# Columns:
datetime           # POSIXct timestamp
pulse_id           # Integer pulse ID
method             # Character: "HRM", "MHR", "sDMA:MHR", etc.
sensor_position    # "inner" or "outer"
Vh_cm_hr           # Numeric: velocity in cm/hr
quality_flag       # Character: "OK", "WARNING", "ERROR", etc.
peclet_number      # Numeric: Péclet number (HRM only)
calc_window_start_sec  # Numeric: calculation window start
calc_window_end_sec    # Numeric: calculation window end
calc_time_sec      # Numeric: specific calculation time
selected_method    # Character: which method was used (sDMA only)
```

---

## 🚀 Performance Notes

### **Progress Reporting:**
sapfluxr uses `progressr` package:
- Works automatically in Shiny
- Shows progress in notifications
- User can see % complete for large datasets

### **Large Datasets:**
- 19,355 pulses × 6 methods = ~116,000 rows
- Calculation may take 30-60 seconds
- Results table handles large data well (DT::datatable pagination)

---

## 📋 Next Session: Visualization (Sessions 4-5)

Will implement:
1. **Session 4:**
   - Plotly time series with range slider
   - Method toggle (show/hide lines via checkboxes)
   - Quality flag vertical lines
   - Hover tooltips

2. **Session 5:**
   - **Click-to-view pulse traces** ← Your key feature!
   - Pulse trace modal/panel
   - Show all 4 thermistor traces (do, di, uo, ui)

**Question for Session 4:**
- Do you want separate plots for inner/outer sensors, or combined on one plot?
- How should we handle multiple methods - different colors, or separate plots?

---

## ✨ Session 3 Achievement Unlocked

Users can now:
- ✅ Upload heat pulse data
- ✅ Configure probe & wood properties
- ✅ Select multiple calculation methods
- ✅ Apply intelligent sDMA switching
- ✅ **Run full HPV calculations!**
- ✅ View results in interactive table

**The app is now functional for basic analysis!** Sessions 4-5 will add visualization, and Session 6 will add export.
