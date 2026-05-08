# functions_all.R - Orchestrator for LaSEM function modules
# Sources all domain-specific function files in correct order

# 1. I/O utilities (create_layer_dataframe, etc.)
source("functions_io.R")

# 2. Analysis pipeline (prepare, validate, build, format)
source("functions_analysis.R")

# 3. Validation functions (schema, geometry checks)
if (file.exists("functions_validation.R")) {
  source("functions_validation.R")
}

# 4. Export functions (GeoPackage, Shapefile, etc.)
if (file.exists("functions_export.R")) {
  source("functions_export.R")
}

# 5. Plotting functions (tidyterra, patchwork)
if (file.exists("functions_plotting.R")) {
  source("functions_plotting.R")
}

# 6. Session info utilities
if (file.exists("functions_session.R")) {
  source("functions_session.R")
}
