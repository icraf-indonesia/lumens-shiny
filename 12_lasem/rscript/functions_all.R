# functions_all.R - Orchestrator for LaSEM function modules
# Sources all domain-specific function files in correct order

# Determine the directory where this file is located
.functions_dir <- getwd()
if (file.exists("functions_io.R")) {
  .functions_dir <- getwd()
} else if (file.exists("12_lasem/rscript/functions_io.R")) {
  .functions_dir <- "12_lasem/rscript"
} else if (file.exists("../rscript/functions_io.R")) {
  .functions_dir <- "../rscript"
}

# Helper to source from the correct directory
.source_local <- function(filename) {
  filepath <- file.path(.functions_dir, filename)
  if (file.exists(filepath)) {
    source(filepath)
  }
}

# 1. I/O utilities (create_layer_dataframe, etc.)
.source_local("functions_io.R")

# 2. Analysis pipeline (prepare, validate, build, format)
.source_local("functions_analysis.R")

# 3. Validation functions (schema, geometry checks)
.source_local("functions_validation.R")

# 4. Export functions (GeoPackage, Shapefile, etc.)
.source_local("functions_export.R")

# 5. Plotting functions (tidyterra, patchwork)
.source_local("functions_plotting.R")

# 6. Session info utilities
.source_local("functions_session.R")

# Clean up helper
rm(.functions_dir, .source_local)
