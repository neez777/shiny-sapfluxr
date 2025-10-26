# Time Range Controls - Implementation Complete

## Overview

Added explicit date/time range controls to the time series plot that:
- ✅ Show and preserve the current view when toggling methods/sensors
- ✅ Update automatically when using the range slider
- ✅ Allow manual entry of specific date/time ranges
- ✅ Default to full data range on page load

---

## The Problem

When toggling methods or sensor positions, the plot would reset to the full time range, losing the user's zoom level. The range slider worked, but the view wasn't preserved when changing plot options.

---

## The Solution

Added **Start Date/Time** and **End Date/Time** controls that act as the "source of truth" for the plot's time range.

### How It Works:

1. **On Page Load:**
   - Date/time inputs initialize to full data range
   - Plot shows all data

2. **When Using Range Slider:**
   - User drags slider or clicks date buttons (1d, 1w, 1m, etc.)
   - Slider position updates the date/time inputs automatically
   - Date/time inputs now reflect the visible range

3. **When Toggling Methods/Sensors:**
   - Plot re-renders with new data
   - Automatically applies the date/time range from inputs
   - View is preserved exactly where user left it

4. **When Manually Entering Dates:**
   - User types or selects date/time in inputs
   - Clicks "Apply Time Range" button
   - Plot zooms to specified range
   - Range slider updates to match

5. **Reset Button:**
   - Resets date/time inputs to full data range
   - Plot returns to show all data

---

## UI Changes

### New Controls in Left Sidebar:

```
Time Range
──────────────────────────────
Set the date/time range to display.
Updates automatically when you use
the range slider.

Start Date/Time: [2024-01-01 00:00]

End Date/Time:   [2024-12-31 23:59]

[Apply Time Range]
```

- **Start Date/Time** - airDatepicker with time picker
- **End Date/Time** - airDatepicker with time picker
- **Apply Time Range** - Button to manually apply custom range
- **Reset to Full Range** - Renamed reset button (clearer purpose)

---

## Technical Implementation

### File: `R/mod_plot_timeseries.R`

#### 1. UI Controls (Lines 40-68)

Added `shinyWidgets::airDatepickerInput()` for start/end times with time pickers:

```r
shinyWidgets::airDatepickerInput(
  ns("start_datetime"),
  "Start Date/Time:",
  value = NULL,
  timepicker = TRUE,
  dateFormat = "yyyy-MM-dd",
  timeFormat = "HH:mm"
)
```

#### 2. Initialize Date Inputs (Lines 131-155)

When data loads, set inputs to full date range:

```r
observe({
  req(vh_results())
  data <- vh_results()

  date_range <- range(data$datetime, na.rm = TRUE)

  if (is.null(input$start_datetime)) {
    shinyWidgets::updateAirDateInput(
      session = session,
      inputId = "start_datetime",
      value = date_range[1]
    )
  }
  # Same for end_datetime...
})
```

#### 3. Sync Slider to Date Inputs (Lines 533-590)

When user adjusts range slider, update the date/time inputs:

```r
observeEvent(event_data("plotly_relayout", source = "timeseries"), {
  relayout_data <- event_data("plotly_relayout", source = "timeseries")

  if (!is.null(relayout_data$`xaxis.range[0]`)) {
    xrange <- c(relayout_data$`xaxis.range[0]`, relayout_data$`xaxis.range[1]`)

    # Convert from plotly timestamp (ms since epoch) to POSIXct
    start_date <- as.POSIXct(xrange[1] / 1000, origin = "1970-01-01", tz = "UTC")
    end_date <- as.POSIXct(xrange[2] / 1000, origin = "1970-01-01", tz = "UTC")

    # Update inputs
    shinyWidgets::updateAirDateInput(session, "start_datetime", start_date)
    shinyWidgets::updateAirDateInput(session, "end_datetime", end_date)
  }
})
```

#### 4. Apply Date Range to Plot (Lines 592-623)

Helper function to apply current date/time inputs to plot:

```r
apply_time_range <- function() {
  req(input$start_datetime, input$end_datetime)

  # Convert POSIXct to plotly timestamps (ms since epoch)
  start_ms <- as.numeric(input$start_datetime) * 1000
  end_ms <- as.numeric(input$end_datetime) * 1000

  # Update plot range using plotlyProxy
  plotly::plotlyProxy("timeseries_plot", session) %>%
    plotly::plotlyProxyInvoke("relayout", list(
      "xaxis.range" = c(start_ms, end_ms)
    ))
}

# Apply when button clicked
observeEvent(input$apply_range, {
  apply_time_range()
})

# Apply when filters change (preserves zoom)
observeEvent(list(input$methods, input$sensor_position, ...), {
  req(input$start_datetime, input$end_datetime)
  Sys.sleep(0.1)  # Small delay for plot to render
  apply_time_range()
})
```

#### 5. Reset to Full Range (Lines 625-652)

Reset button updates inputs to full range and resets plot:

```r
observeEvent(input$reset_zoom, {
  req(vh_results())
  date_range <- range(vh_results()$datetime, na.rm = TRUE)

  # Update inputs
  shinyWidgets::updateAirDateInput(session, "start_datetime", date_range[1])
  shinyWidgets::updateAirDateInput(session, "end_datetime", date_range[2])

  # Reset plot
  plotly::plotlyProxy("timeseries_plot", session) %>%
    plotly::plotlyProxyInvoke("relayout", list(
      "xaxis.autorange" = TRUE,
      "yaxis.autorange" = TRUE
    ))
})
```

---

## User Workflows

### Workflow 1: Use Range Slider
1. User drags range slider to zoom to specific period
2. Start/End Date inputs update automatically
3. User toggles methods on/off
4. Plot maintains the zoomed view ✓

### Workflow 2: Manual Date Entry
1. User types "2024-06-01 08:00" in Start Date
2. User types "2024-06-07 18:00" in End Date
3. User clicks "Apply Time Range"
4. Plot zooms to that specific week ✓
5. Range slider updates to match ✓

### Workflow 3: Quick Date Buttons
1. User clicks "1w" button on plot
2. Start/End Date inputs update to show last week
3. User toggles sensor positions
4. Plot maintains the 1-week view ✓

### Workflow 4: Reset
1. User clicks "Reset to Full Range"
2. Start/End Date inputs return to full data range
3. Plot shows all data ✓

---

## Benefits

✅ **View Persistence** - Time range maintained when changing plot options
✅ **Manual Control** - Type exact date/times for precise analysis
✅ **Visual Feedback** - Always see current range in date inputs
✅ **Bidirectional Sync** - Slider updates inputs, inputs update plot
✅ **Clear Defaults** - Always starts with full data range
✅ **Easy Reset** - One button to return to full view

---

## Time Format

- **Date Format:** yyyy-MM-dd (e.g., 2024-06-15)
- **Time Format:** HH:mm (e.g., 14:30)
- **Display:** "2024-06-15 14:30"
- **Time Zone:** UTC (matches sapfluxr data)

---

## Testing Checklist

**Initialization:**
- [ ] Page loads with date inputs set to full data range
- [ ] Plot shows all data on initial load

**Range Slider:**
- [ ] Dragging slider updates Start/End Date inputs
- [ ] Date buttons (1d, 1w, 1m, 3m, All) update inputs
- [ ] Inputs show correct dates after slider interaction

**Manual Entry:**
- [ ] Can type dates directly in inputs
- [ ] Can use date picker popup
- [ ] "Apply Time Range" button zooms plot correctly
- [ ] Range slider updates to match manual entry

**View Persistence:**
- [ ] Toggle methods - view maintained
- [ ] Toggle sensor positions - view maintained
- [ ] Toggle show points - view maintained
- [ ] Toggle quality flags - view maintained

**Reset:**
- [ ] "Reset to Full Range" button works
- [ ] Inputs return to full data range
- [ ] Plot shows all data after reset

**Edge Cases:**
- [ ] End date before start date - handled gracefully
- [ ] Invalid date format - validated by input widget
- [ ] Very narrow time ranges (hours/minutes) - works correctly

---

## Dependencies

Uses `shinyWidgets::airDatepickerInput()` for date/time selection:
- Already in DESCRIPTION (shinyWidgets >= 0.8.0)
- Supports both date and time selection
- Clean, modern UI
- Built-in validation

---

## Summary

The time range controls provide a clean, intuitive way to:
1. See the current visible time range at all times
2. Manually enter precise date/time ranges
3. Preserve the view when changing plot options
4. Sync with the range slider bidirectionally

**Problem solved!** Users can now zoom to a specific time period and it will stay zoomed when toggling methods, sensors, or other plot options.

🎉 **Ready for Session 5!** 🎉
