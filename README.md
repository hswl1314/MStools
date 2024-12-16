# MStools 

## Overview

MStools provides efficient tools for mass spectrometry data analysis and experiment planning. Designed for LC-MS researchers, it simplifies time management, sample planning, and data analysis.

## Installation

```r
# Install from GitHub
devtools::install_github("hswl1314/MStools")
```

## Key Functions

### ⏱️ Time Management

#### Calculate Analysis End Time
```r
# When will your analysis finish?
when_you_add_samples(
  sample_count = 75,           # Number of samples
  minutes_per_sample = 13,     # Time per sample
  start_time = "09:00"        # Start time (optional)
)
```

#### Calculate Sample Capacity
```r
# How many samples can you add?
how_many_sample_add(
  end_time = "17:00",         # Deadline
  minutes_per_sample = 13,    # Time per sample
  start_time = "09:00"        # Start time (optional)
)
```

### 📊 Gradient Analysis

#### Calculate Mobile Phase Volumes
```r
# Create gradient program data
gradient_data <- data.frame(
  Time = c(0.000, 0.500, 7.500, 8.000, 9.000, 9.100, 12.000),
  Flow = c(0.50, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50),
  B = c(95.0, 95.0, 65.0, 40.0, 40.0, 95.0, 95.0)
)

# Calculate volumes for 10 samples
result <- calculate_gradient_volume(
  data = gradient_data,
  sample_number = 10,
  extra_volume = 1.2  # 20% extra volume
)
```

### 📈 Standard Curve Analysis

#### Plot Standard Curves
```r
# Create example data
std_data <- data.frame(
  Conc = c(5, 2.5, 1.25, 0.625, 0.3125),
  AA = c(68619120, 29206615, 16247479, 7586211, 4235692),
  DHA = c(58619120, 25206615, 14247479, 6586211, 3235692)
)

# Plot standard curves
plot_standard_curves(
  data = std_data,
  compounds = c("AA", "DHA"),  # or "all" for all compounds
  log = TRUE,                  # Use log transformation
  split = FALSE               # Combine plots in one figure
)
```

### 🔄 Data Format Conversion

#### MSdial to Jupyter Converter
```r
# Launch the Shiny app
run_MSdial_to_Jupyter_app()
```

## Function Details

### Time Format Support
All time-related functions accept:
- `"HH:MM"` (e.g., "09:00")
- `"YYYY-MM-DD HH:MM"` (e.g., "2024-01-20 09:00")
- `Sys.time()` (current system time)

### Gradient Volume Calculator Features
- Calculates volumes for both mobile phases A and B
- Supports multiple samples
- Includes customizable extra volume factor
- Returns detailed calculations and summaries

### Standard Curve Analysis Features
- Supports multiple compounds
- Optional log transformation
- Split or combined plot options
- Returns regression statistics

### MSdial to Jupyter Converter Features
- User-friendly Shiny interface
- Supports large files (up to 500MB)
- Customizable sample number
- Downloads formatted input files

## Getting Help

```r
# View function documentation
?when_you_add_samples
?how_many_sample_add
?calculate_gradient_volume
?plot_standard_curves
?run_MSdial_to_Jupyter_app
```

## Requirements

* R >= 4.0.0
* Required packages: shiny, dplyr

## Contributing

Interested in contributing? Check out our [contribution guidelines](CONTRIBUTING.md).

## Issues

Found a bug? Have a feature request? Please [open an issue](https://github.com/hswl1314/MStools/issues).

## License

[MIT](LICENSE.md) © Your Name

## Citation

```bibtex
@software{MStools2024,
  author = {Your Name},
  title = {MStools: Mass Spectrometry Analysis Tools},
  year = {2024},
  publisher = {GitHub},
  url = {https://github.com/hswl1314/MStools}
}
```

