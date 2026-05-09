# LaSEM Model Theory

## Maximum Limitation Method (Liebig's Law of the Minimum)

LaSEM uses the **Maximum Limitation Method (MLM)** based on Liebig's Law of the Minimum: for each pixel (land unit), the biophysical factor with the lowest suitability class determines the final land suitability rating. This is a **worst-case aggregation** approach.

In other words, if a location has "highly suitable" (S1) ratings for temperature, precipitation, and soil pH, but "not suitable" (N) for soil depth, the overall suitability for that location is **N** (Not Suitable), with soil depth as the **limiting factor**.

The priority order for determining the lowest class is:

```
N > S3 > S2 > S1
```

Where:
- **S1** - Highly Suitable: no significant limitations to sustained crop production
- **S2** - Moderately Suitable: minor limitations that reduce productivity but are manageable
- **S3** - Marginally Suitable: significant limitations requiring substantial management inputs
- **N** - Not Suitable: severe limitations that cannot be overcome with current technology

## Reclassification Process

Each input raster layer is reclassified into suitability classes (S1/S2/S3/N) using crop-specific thresholds defined in the crop suitability parameter table. The `classify_suitability_predictors()` function handles this step:

1. **Continuous variables** (temperature, precipitation, pH, etc.):
   - Range-based thresholds: `20-30` means values from 20 to 30 (inclusive)
   - Greater-than thresholds: `>20` means values greater than 20
   - Less-than thresholds: `<15` means values less than 15
   - Values are parsed using regex patterns to extract lower/upper bounds
   - A reclassification matrix is built and passed to `terra::classify()`

2. **Categorical variables** (soil texture):
   - USDA texture codes are mapped to numerical values using a lookup table
   - Each suitability class specifies which texture codes qualify

## Concatenation and Final Classification

After individual factor reclassification:

1. **Concatenation** (`concat_rasters()`): All classified factor rasters are combined into a single raster where each pixel's value is a unique category ID encoding the combination of factor classes (e.g., `S1_S2_S1_N` for a 4-factor analysis).

2. **Attribute resolution** (`build_suitability_attributes()`): The categorical raster is processed to compute:
   - `suitability` - the final suitability class via MLM
   - `limiting_factor_actual` - which factor(s) currently limit the land
   - `limiting_factor_potential` - which factors could limit the land after improvement

3. **Polygon conversion** (`format_suitability_outputs()`): The raster is converted to vector polygons with comprehensive attributes for export and visualization.

## Intervention Scenarios

The `calculate_suitability_potential_table()` function computes three intervention scenarios:

| Level | Description | Potential Improvement |
|-------|-------------|----------------------|
| **Low** | Minimal management input | Improve by 1 class (e.g., N → S3) |
| **Medium** | Standard management | Improve by up to 2 classes |
| **High** | Intensive management | Improve by up to 3 classes (e.g., N → S1) |

The intervention lookup table defines which factors can be improved and by how much. Factors marked `FALSE` for `intervention` cannot be improved regardless of management level.

For each pixel, the potential suitability is calculated as: `actual_class - intervention_level`, constrained to a minimum of S1. If actual suitability is already S1, the potential remains S1.

## Raster Serialization Pattern

LaSEM uses `terra::wrap()` and `terra::unwrap()` for serializing `SpatRaster` objects to RDS format:

```r
# Saving
saveRDS(terra::wrap(raster_object), file = "output.rds")

# Loading
raster_object <- terra::unwrap(readRDS("output.rds"))
```

This pattern preserves the full raster metadata (CRS, extent, levels) across sessions, unlike `terra::writeRaster()` which requires writing to a file format like GeoTIFF.

## Key Assumptions

1. **Liebig's Law of the Minimum**: The most limiting factor determines overall suitability. There are no compensatory effects between factors.

2. **Biophysical factors only**: Social, economic, and policy factors are excluded from the analysis. The suitability map considers only climate, soil, and terrain conditions.

3. **Static conditions**: The analysis evaluates current biophysical conditions and does not account for climate change, seasonal variability, or extreme weather events.

4. **Linear intervention potential**: Management improvements are modeled as additive class improvements (e.g., improving by 1 class at a time). Interactions between improved factors are not modeled.

5. **Data quality parity**: All input raster layers are treated with equal weight. No uncertainty quantification or sensitivity analysis is performed on input data quality.

## Limitations

- **Categorical approach**: Ignores uncertainty in data and interrelationships between biophysical factors. A pixel just above a threshold is treated identically to one far above it.

- **Semi-quantitative output**: Results show suitability classes, not crop yield in weight per unit area. Suitability does not directly translate to productivity.

- **No temporal dynamics**: Does not consider crop rotation, fallow periods, or seasonal planting windows.

- **Scale dependency**: Results are sensitive to input data resolution. Fine-scale heterogeneity may be masked at coarser resolutions.

- **No economic optimization**: The module identifies biophysically suitable areas but does not optimize for profit, market access, or labor availability.

- **Management intervention assumes technology availability**: The intervention scenarios assume that the management practices needed to achieve the stated improvements are technically feasible and available in the study area.

## References

- FAO (1976). *A framework for land evaluation*. FAO Soils Bulletin 32.
- Jaisli et al. (2018). Suitability evaluation system for agricultural commodities. *Computers and Electronics in Agriculture*.
- Ritung et al. (2011). *Petunjuk Teknis Evaluasi Lahan Untuk Komoditas Pertanian*. Balai Besar Litbang SDLP, Bogor.
- Sys et al. (1991). *Land Evaluation Part III: Crop requirements*. Agricultural Publications No. 7.
- Wahyunto et al. (2016). *Pedoman Penilaian Kesesuaian Lahan untuk Komoditas Pertanian Strategis*. Balai Besar Litbang SDLP, Bogor.
