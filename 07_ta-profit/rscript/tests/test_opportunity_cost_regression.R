library(dplyr)
library(testthat)
source('../functions.R', encoding = 'UTF-8')

make_input <- function(area, carbon_change, economic_per_ha, pu = seq_along(area)) {
  data.frame(
    LULCC = 'Forest to crop', PU = as.character(pu), Ha = area,
    C_T1 = 100, C_T2 = 100 - carbon_change,
    NPV1 = 0, NPV2 = economic_per_ha * area
  )
}

test_that('area does not change per-transition opportunity cost', {
  result <- build_opcost_table(make_input(c(1, 10), c(10, 10), c(100, 100)), 10, 11)
  expect_equal(result$data_em_sel$opcost, rep(100 / 36.7, 2))
  expect_equal(result$data_em_sel$em_tot, c(36.7, 367))
})

test_that('grouping uses total economic change and carbon change', {
  input <- make_input(c(1, 2), c(10, 10), c(183.5, 183.5))
  grouped <- build_grouped_data(prepare_curve_data(build_opcost_table(input, 10, 3)$opcost_all))
  expect_equal(grouped$opportunity_cost, 5)
  unequal <- make_input(c(1, 3), c(10, 10), c(36.7, 110.1))
  grouped <- build_grouped_data(prepare_curve_data(build_opcost_table(unequal, 10, 4)$opcost_all))
  expect_equal(grouped$opportunity_cost, 2.5)
  unsplit <- make_input(4, 10, 91.75)
  reference <- build_grouped_data(prepare_curve_data(build_opcost_table(unsplit, 10, 4)$opcost_all))
  expect_equal(grouped$opportunity_cost, reference$opportunity_cost)
})

test_that('directions and dominant planning units remain separate', {
  input <- make_input(c(1, 2), c(10, -10), c(183.5, 183.5), c('emission', 'sequestration'))
  curve <- prepare_curve_data(build_opcost_table(input, 10, 3)$opcost_all)
  grouped <- build_grouped_data(curve)
  final <- prepare_final_dataset(split_emission_direction(grouped), calculate_pu_dominance(curve))
  expect_equal(sort(grouped$carbon_direction), c(-1, 1))
  expect_equal(final$planning_unit[final$carbon_direction == 1], 'emission')
  expect_equal(final$planning_unit[final$carbon_direction == -1], 'sequestration')
  expect_equal(sort(grouped$opportunity_cost), c(-5, 5))
})

test_that('zero and missing values are handled explicitly', {
  zero_carbon <- build_opcost_table(make_input(1, 0, 100), 10, 1)
  expect_true(is.na(zero_carbon$data_em_sel$opcost))
  expect_equal(nrow(zero_carbon$opcost_all), 0)
  zero_cost <- make_input(c(1, 1), c(10, 10), c(0, 367))
  grouped <- build_grouped_data(prepare_curve_data(build_opcost_table(zero_cost, 10, 2)$opcost_all))
  expect_equal(grouped$opportunity_cost, 5)
  missing <- make_input(1, NA_real_, 100)
  expect_error(build_opcost_table(missing, 10, 1), 'missing or non-finite')
})



test_that('report export and chart receive corrected values', {
  skip_if_not_installed('terra')
  skip_if_not_installed('openxlsx')
  skip_if_not_installed('plotly')
  library(ggplot2)
  library(plotly)
  library(terra)
  suppressWarnings(library(openxlsx))
  input <- make_input(c(1, 2), c(10, -10), c(183.5, 183.5), c('A', 'B'))
  input$LC1 <- 'Forest'
  input$LC2 <- 'Crop'
  input$deltaNPV <- input$NPV2 - input$NPV1
  input$Freq <- 1
  input$ID_LC1 <- 1
  input$ID_LC2 <- 2
  input$EAE_LC1 <- 0
  input$EAE_LC2 <- 183.5
  raster <- terra::rast(nrows = 1, ncols = 1, vals = 1)
  maps <- list(npv1_map = raster, npv2_map = raster, deltaNPV_map = raster,
               LULCT1 = raster, LULCT2 = raster, PU = raster)
  paths <- list(pathLookupCstock = 'carbon.csv')
  times <- list(start_time = Sys.time(), end_time = Sys.time(), valueT1 = 2000,
                valueT2 = 2010, eae_year = 2010)
  output_dir <- tempfile()
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE))
  result <- suppressWarnings(generate_report_params(
    list(combinedRasterTable = input, period = 10, total_area = 3),
    maps, paths, times, output_dir, list(), 'USD'
  ))
  exported <- openxlsx::read.xlsx(file.path(output_dir, 'tabel_perhitungan_keuntungan_usaha_tani.xlsx'), sep.names = ' ')
  expect_equal(exported[['Biaya Peluang (USD/tahun per ton CO2-eq)']], c(5, -5))
  expect_equal(exported[['Total Emisi (ton CO2-eq)']], c(36.7, -73.4))
  expect_s3_class(result$emission_plot, 'plotly')
  expect_s3_class(result$sequestration_plot, 'plotly')
  text <- unlist(lapply(result$emission_plot$x$data, function(trace) trace$text))
  expect_true(any(grepl('Abatement cost (berdasarkan EAE tahunan): 5', text, fixed = TRUE)))
})


test_that('the Bungo rubber and open-land example occupies quadrants 3 and 4', {
  lookup <- read.csv('../../../data/table/profitability_table_bungo.csv')
  open_land <- lookup[lookup$LC == 'Open Land', ]
  rubber <- lookup[lookup$LC == 'Rubber Monoculture', ]
  expect_equal(nrow(open_land), 1)
  expect_equal(nrow(rubber), 1)
  transitions <- data.frame(
    LULCC = c('Open to rubber', 'Rubber to open'),
    PU = c('Example', 'Example'), Ha = c(1, 1),
    C_T1 = c(open_land$CARBON, rubber$CARBON),
    C_T2 = c(rubber$CARBON, open_land$CARBON),
    NPV1 = c(open_land$EAE, rubber$EAE),
    NPV2 = c(rubber$EAE, open_land$EAE)
  )
  output <- build_opcost_table(transitions, 10, 2)$data_em_sel
  expect_lt(output$em_tot[1], 0)
  expect_lt(output$opcost[1], 0)
  expect_gt(output$em_tot[2], 0)
  expect_lt(output$opcost[2], 0)
  expect_equal(output$opcost, rep(-23.56599836438916, 2))
})
