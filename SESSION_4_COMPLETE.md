# Session 4 Complete - Interactive Time Series Visualization ✓

## Summary

Built a comprehensive interactive plotly time series visualization with dynamic method toggles, sensor position filtering, quality flag markers, range slider, and date selection controls. Users can now visualize HPV results with publication-quality interactive plots.

---

## ✅ What's Implemented

### **Time Series Visualization Module** (`R/mod_plot_timeseries.R`)

#### **Interactive Plotly Plot**
- ✅ Multi-method time series with automatic colour assignment
- ✅ Separate traces for inner/outer sensor positions
- ✅ Inner sensors: solid lines
- ✅ Outer sensors: dashed lines
- ✅ Hover tooltips showing method, time, velocity
- ✅ Zoom/pan interactions
- ✅ Export to PNG with custom sizing

#### **Method Toggle Controls**
- ✅ Dynamic checkboxes generated from available methods
- ✅ All methods selected by default
- ✅ Instant plot update when toggling methods on/off
- ✅ Supports all methods: HRM, MHR, HRMXa, HRMXb, Tmax variants, sDMA combinations

#### **Sensor Position Filtering**
- ✅ Toggle between inner/outer sensors
- ✅ Different line styles (solid vs dashed) when both displayed
- ✅ Clear legend showing method and position

#### **Quality Flag Indicators**
- ✅ Toggle to show/hide quality flag markers
- ✅ Different symbols for different flags:
  - ⚠️ WARNING: Orange triangle
  - ❌ ERROR: Red X
  - ⁉️ SUSPECT: Magenta diamond
- ✅ Hover tooltips on markers showing flag type and details

#### **Range Slider & Date Selection**
- ✅ Interactive range slider below main plot
- ✅ Quick date selection buttons:
  - 1 day, 1 week, 1 month, 3 months, All
- ✅ Manual zoom via click-and-drag
- ✅ Reset Zoom button to restore full view

#### **Display Options**
- ✅ Toggle data points on/off (lines only by default)
- ✅ Show/hide quality flag markers
- ✅ Reset zoom functionality

---

## 🎨 Method Colours

Consistent colour palette across all plots:

| Method | Colour | Hex Code |
|--------|--------|----------|
| HRM | Blue | #1f77b4 |
| MHR | Orange | #ff7f0e |
| HRMXa | Green | #2ca02c |
| HRMXb | Red | #d62728 |
| Tmax_Coh | Purple | #9467bd |
| Tmax_Klu | Brown | #8c564b |
| sDMA:HRM | Pink | #e377c2 |
| sDMA:MHR | Gray | #7f7f7f |
| sDMA:Tmax_Coh | Yellow-green | #bcbd22 |
| sDMA:Tmax_Klu | Cyan | #17becf |

---

## 🎯 Key Features

### **1. Dynamic Method Toggles**
Methods checkboxes are generated automatically from the data:
```r
available_methods <- reactive({
  unique(vh_results()$method)
})

# Creates checkboxes only for methods present in results
checkboxGroupInput(
  session$ns("methods"),
  NULL,
  choices = setNames(methods, methods),
  selected = methods  # All selected by default
)
```

### **2. Smart Sensor Position Handling**
If both inner and outer selected:
- Creates separate traces with same colour
- Inner: solid line
- Outer: dashed line
- Legend shows: "HRM (inner)", "HRM (outer)"

If only one position selected:
- Single trace per method
- No position suffix in legend

### **3. Quality Flag Markers**
Only non-OK flags are shown:
```r
markers <- data %>%
  filter(quality_flag != "OK") %>%
  select(datetime, quality_flag, method, sensor_position, Vh_cm_hr)
```

Each flag type gets distinct marker:
- WARNING: triangle-up, orange
- ERROR: x, red
- SUSPECT: diamond, magenta

### **4. Plot Information Panel**
Collapsible info box showing:
- Number of displayed points
- Methods displayed
- Sensor positions displayed
- Date range
- Quality flag summary (count by type)

Updates automatically when filters change.

### **5. Export to PNG**
Custom export button settings:
- Filename: "heat_pulse_velocity_timeseries.png"
- Size: 1200 × 600 px
- Scale: 2× (high DPI for publications)

---

## 📊 Plot Layout

### **Two-Column Design:**

**Left Column (3 wide):**
- Method checkboxes (dynamic)
- Sensor position toggles
- Display options
- Reset zoom button
- Plot information (collapsible)

**Right Column (9 wide):**
- Main plotly time series (600px height)
- Range slider below plot
- Date selection buttons

---

## 🔄 Data Flow

```
vh_results reactive
       ↓
Filter by selected methods
       ↓
Filter by sensor positions
       ↓
Create plotly traces (one per method × position)
       ↓
Add quality flag markers (if enabled)
       ↓
Apply layout with range slider
       ↓
Render interactive plot
```

---

## 💡 Smart Features

### **1. Hover Tooltips**
Custom hover templates for clarity:
```
Method (position)
Time: 2024-01-15 14:30:00
Vh: 12.45 cm/hr
```

For quality flags:
```
WARNING
Time: 2024-01-15 14:30:00
Vh: 12.45 cm/hr
```

### **2. Range Selector Buttons**
Quick navigation with preset time ranges:
- **1d**: Last 24 hours
- **1w**: Last 7 days
- **1m**: Last month
- **3m**: Last 3 months
- **All**: Full dataset

### **3. Responsive Legend**
- Horizontal orientation below plot
- Wraps when many methods shown
- Click to toggle individual traces
- Double-click to isolate single trace

### **4. Zoom Persistence**
- User's zoom level maintained when toggling methods
- Reset Zoom button restores full view
- Range slider syncs with zoom level

---

## 📋 Integration with App

### **Module Call:**
```r
# UI
plotTimeseriesUI("plot_timeseries")

# Server
plotTimeseriesServer("plot_timeseries", vh_results)
```

### **Reactive Chain:**
```r
# Results from methods module
vh_results <- methodsServer(...)

# Pass to plot module
plotTimeseriesServer("plot_timeseries", vh_results)
```

---

## 🧪 Testing Workflow

```r
shiny::runApp("E:/R/project/shiny-sapfluxr")
```

### **Test Steps:**

1. **Upload & Calculate** (Tabs 1-3):
   - Upload heat pulse data
   - Configure probe/wood (or use defaults)
   - Select methods (e.g., HRM, MHR, Tmax_Klu)
   - Click Calculate

2. **Navigate to Visualise Tab (Tab 5)**:
   - Should see plot with all methods displayed
   - Both inner and outer sensors shown (solid/dashed)

3. **Test Method Toggles**:
   - Uncheck MHR → plot updates, MHR disappears
   - Check MHR → plot updates, MHR reappears

4. **Test Sensor Filtering**:
   - Uncheck "Inner" → only outer sensors shown (dashed lines)
   - Uncheck "Outer" → only inner sensors shown (solid lines)

5. **Test Quality Flags**:
   - If data has quality issues, markers should appear
   - Uncheck "Show quality flag markers" → markers disappear
   - Hover over marker → should show flag type

6. **Test Range Slider**:
   - Click "1w" button → zooms to last week
   - Click "1d" button → zooms to last day
   - Drag range slider handles → plot zooms to selection
   - Click "Reset Zoom" → returns to full view

7. **Test Zoom/Pan**:
   - Click and drag on plot → pans view
   - Scroll wheel → zooms in/out
   - Box select tool → zooms to selection

8. **Test Export**:
   - Click camera icon in plot toolbar
   - Should download "heat_pulse_velocity_timeseries.png"
   - Check image is high quality (2400 × 1200 px @ 2× scale)

---

## 🎨 UI Design Notes

### **Line Styles:**
- Solid lines for inner sensors (primary measurement)
- Dashed lines for outer sensors (radial variation)
- Line width: 2px (visible but not overwhelming)
- Points: 4px when enabled

### **Marker Sizes:**
- Quality flags: 10px (prominent)
- Data points: 4px (subtle when enabled)
- Black outline on markers for visibility

### **Plot Height:**
- 600px main plot (tall enough to see details)
- Additional space for range slider below
- Legend below plot (doesn't overlap data)

---

## 📊 Plotly Configuration

### **Enabled Interactions:**
- Zoom (box zoom, scroll zoom)
- Pan (click and drag)
- Hover (tooltips)
- Export to PNG
- Reset axes
- Toggle traces (click legend)
- Autoscale

### **Disabled Interactions:**
- Lasso select (not needed for time series)
- Box select for non-zoom purposes
- Autoscale button (have Reset Zoom instead)

### **Custom Export Settings:**
```r
toImageButtonOptions = list(
  format = "png",
  filename = "heat_pulse_velocity_timeseries",
  height = 600,
  width = 1200,
  scale = 2  # High DPI for publications
)
```

---

## 🚀 Performance Notes

### **Large Datasets:**
With 100,000+ points:
- Plotly handles rendering efficiently
- Lines-only mode (no points) renders faster
- WebGL mode could be enabled for >500k points if needed

### **Plot Updates:**
- Toggling methods: Instant (just show/hide traces)
- Zoom/pan: Smooth and responsive
- Range slider: Real-time preview

---

## 📋 Next Session: Pulse Trace Viewer (Session 5)

Will implement the key interactive feature requested:
1. **Click event on time series plot**
   - Detect which pulse was clicked
   - Extract pulse_id and datetime

2. **Pulse trace modal/panel**
   - Show individual pulse temperature traces
   - Plot all 4 thermistors (do, di, uo, ui)
   - Show heat pulse timing (vertical line)
   - Display pulse metadata

3. **Trace diagnostics**
   - Show baseline temperatures
   - Highlight analysis windows
   - Display calculated velocities
   - Show quality flags and reasons

**Design question for Session 5:**
- Modal dialog or sidebar panel for pulse trace viewer?
- Should we show multiple pulses side-by-side for comparison?

---

## ✨ Session 4 Achievement Unlocked

Users can now:
- ✅ Upload heat pulse data
- ✅ Configure probe & wood properties
- ✅ Select multiple calculation methods
- ✅ Apply sDMA processing
- ✅ Run full HPV calculations
- ✅ View results in interactive table
- ✅ **Visualize time series with plotly!**
- ✅ **Toggle methods and sensor positions**
- ✅ **See quality flag indicators**
- ✅ **Zoom, pan, and navigate data**

**Next up: Click-to-view pulse traces - the feature that makes this app unique!**

---

## 🎓 Technical Implementation Details

### **Reactive Filtering:**
```r
filtered_data <- reactive({
  req(vh_results())
  req(input$methods)
  req(input$sensor_position)

  data <- vh_results()

  # Filter by method
  data <- data %>%
    filter(method %in% input$methods)

  # Filter by sensor position
  data <- data %>%
    filter(sensor_position %in% input$sensor_position)

  data
})
```

### **Dynamic Trace Generation:**
```r
# Add trace for each method
for (method in unique(data$method)) {
  method_data <- data %>% filter(method == !!method)

  # Separate by sensor position if both selected
  if (length(input$sensor_position) > 1) {
    for (pos in unique(method_data$sensor_position)) {
      pos_data <- method_data %>% filter(sensor_position == !!pos)

      trace_name <- paste0(method, " (", pos, ")")
      line_dash <- if (pos == "inner") "solid" else "dash"

      p <- p %>% add_trace(...)
    }
  } else {
    # Single sensor position
    p <- p %>% add_trace(...)
  }
}
```

### **Quality Flag Markers:**
```r
# Get non-OK quality flags
markers <- data %>%
  filter(quality_flag != "OK") %>%
  select(datetime, quality_flag, method, sensor_position, Vh_cm_hr) %>%
  distinct()

# Add trace for each flag type
for (flag in unique(markers$quality_flag)) {
  flag_data <- markers %>% filter(quality_flag == !!flag)

  p <- p %>%
    add_trace(
      data = flag_data,
      x = ~datetime,
      y = ~Vh_cm_hr,
      type = "scatter",
      mode = "markers",
      name = flag,
      marker = list(
        symbol = flag_shapes[[flag]],
        size = 10,
        color = flag_colours[[flag]]
      )
    )
}
```

---

## 🐛 Known Limitations

- No WebGL rendering yet (may be needed for extremely large datasets >500k points)
- Click events not yet implemented (Session 5)
- No comparison mode (multiple plots side-by-side) yet
- No statistical annotations (mean, median lines) yet

These can be enhanced in future iterations!

---

## 📚 Plot Features Summary

| Feature | Status | Implementation |
|---------|--------|----------------|
| Multi-method traces | ✅ | Dynamic trace generation |
| Inner/outer sensors | ✅ | Solid/dashed lines |
| Method colours | ✅ | Consistent palette |
| Quality flags | ✅ | Symbol markers |
| Hover tooltips | ✅ | Custom templates |
| Range slider | ✅ | Plotly layout option |
| Date selection | ✅ | Range selector buttons |
| Zoom/pan | ✅ | Plotly interactions |
| Export PNG | ✅ | Custom config |
| Toggle methods | ✅ | Checkbox filtering |
| Toggle sensors | ✅ | Checkbox filtering |
| Reset zoom | ✅ | plotlyProxy |
| Plot info | ✅ | Dynamic summary |
| Click to view pulse | ⬜ | Session 5 |

---

## 🎉 Visualization Complete!

The app now provides a **publication-quality interactive time series viewer** with all the controls researchers need to explore their heat pulse velocity data. Next session will add the unique "click to view pulse trace" feature that sets this app apart from static plotting!
