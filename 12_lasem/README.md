# LaSEM - Land Suitability Evaluation Module

## Overview

LaSEM evaluates biophysical land suitability for agricultural commodities using the **Maximum Limitation Method** (Liebig's Law of the Minimum). It produces land suitability maps, factor-level classification maps, and intervention scenario maps to inform land-use planning decisions.

## Architecture

```
12_lasem/
├── rscript/
│   ├── app.R                    # Shiny application (3-tab UI)
│   ├── LaSEM_functions.R        # Legacy functions (kept for compatibility)
│   ├── functions_all.R          # Orchestrator (sources domain files)
│   ├── functions_io.R           # I/O utilities (read_raster_files, etc.)
│   ├── functions_analysis.R     # Core pipeline (prepare, validate, build, format)
│   ├── functions_validation.R   # Validation (CSV schema, raster geometry)
│   └── functions_export.R       # Export utilities (GeoPackage, Shapefile)
├── report_template/
│   └── LaSEM_report.Rmd         # HTML report template (tidyterra static maps)
├── helpfile/
│   ├── lasem_user_guide.Rmd     # Step-by-step user guide
│   └── lasem_model_theory.md    # Model theory and assumptions
└── README.md                    # This file
```

## Workflow

1. **Upload & Preview** - Upload three CSV files (raster inputs, crop suitability, intervention lookup) and select output directory
2. **Inspect Factors** (optional) - Review individual input rasters and classification criteria before analysis
3. **Results** - Run analysis and view interactive suitability map, area summary, and export report

## Key Features

- **Guided 3-tab UI** using `bslib::navset_card_tab()`
- **Automatic validation** - CSV schema, file existence, raster geometry (CRS/extent/resolution)
- **Factor inspection** - Optional pre-analysis raster review with "Looks Good" / "Flag Issue" verdicts
- **Interactive results** - Full-size Leaflet map in the Shiny app
- **Static report** - Lightweight HTML report with `tidyterra` maps using `patchwork`
- **Comprehensive exports** - GeoTIFF, RDS (via `terra::wrap()`), GeoPackage, CSV

## Input Data

### 1. Raster Inputs CSV
Maps parameters to raster file paths.

| Column | Description |
|--------|-------------|
| `ID` | Numeric identifier |
| `parameter` | Human-readable name |
| `parameter_name` | Machine-readable name (must match raster layers AND crop suitability CSV) |
| `availability` | `Yes` or `No` |
| `raster_path` | Absolute or relative path to GeoTIFF |

### 2. Crop Suitability CSV
Defines crop-specific thresholds.

| Column | Description |
|--------|-------------|
| `name_common` | Common crop name |
| `name_sp` | Scientific name |
| `class` | `S1`, `S2`, `S3`, or `N` |
| `name_parameter` | Parameter name (must match raster inputs) |
| `value` | Threshold: `20-30`, `>20`, `<15` |
| `unit` | Unit of measurement |

### 3. Intervention Lookup CSV
Defines management improvement potential.

| Column | Description |
|--------|-------------|
| `no` | Numeric identifier |
| `karakteristik_lahan` | Land characteristic (Indonesian) |
| `name_parameter` | Parameter name |
| `intervention` | `TRUE` or `FALSE` |
| `low` | Low intervention (classes improved) |
| `med` | Medium intervention |
| `high` | High intervention |

## Output Files

| File | Description |
|------|-------------|
| `land_suitability.tif` | Final suitability raster (S1/S2/S3/N) |
| `land_suitability.rds` | Suitability polygons (sf object) |
| `suitability_factors.tif` | Individual factor classifications |
| `soil_climate_factors.rds` | Harmonised inputs (`terra::wrap` format) |
| `LaSEM_report.html` | Comprehensive analysis report |

## Running the Module

### From LUMENS Hub
```r
shiny::runApp("12_lasem/rscript", port = 8787)
```

### Standalone
```r
Rscript call13.R
```

### Development
```r
# Run tests
Rscript -e "testthat::test_dir('tests/testthat/')"

# Style code
Rscript -e "styler::style_dir('12_lasem/rscript', filetype = 'R')"
```

## Model Theory

See `helpfile/lasem_model_theory.md` for detailed explanation of:
- Maximum Limitation Method (Liebig's Law)
- Reclassification process
- Concatenation and attribute resolution
- Intervention scenario modeling
- Key assumptions and limitations

## References

- FAO (1976). *A framework for land evaluation*. FAO Soils Bulletin 32.
- Jaisli et al. (2018). Suitability evaluation system for agricultural commodities. *Computers and Electronics in Agriculture*.
- Ritung et al. (2011). *Petunjuk Teknis Evaluasi Lahan Untuk Komoditas Pertanian*. Balai Besar Litbang SDLP, Bogor.
- Sys et al. (1991). *Land Evaluation Part III: Crop requirements*. Agricultural Publications No. 7.
