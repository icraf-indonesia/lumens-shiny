suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))
library(testthat)
source('../functions.R', encoding = 'UTF-8')

costs <- c(-100, -1, -0.5, 0, 0.5, 1, 100)
curve <- expand.grid(cost = costs, direction = c(1, -1)) %>%
  mutate(
    emission_rate = direction,
    opportunity_cost = cost,
    land_use_change = paste0('cost ', cost),
    planning_unit = 'Example',
    area = 1,
    economic_change = cost * direction,
    emissions_tco2 = direction
  )
grouped <- build_grouped_data(curve)
final <- prepare_final_dataset(split_emission_direction(grouped), calculate_pu_dominance(curve))
plots <- list(
  emission = plot_emission_curve(final, 'USD'),
  sequestration = plot_sequestration_curve(final, 'USD'),
  planning_unit = generate_plots_by_pu(curve, 'USD')[[1]]
)

test_that('the actual plots preserve cost signs and order around zero', {
  for (name in names(plots)) {
    traces <- Filter(function(trace) !is.null(trace$text) && any(grepl('Abatement cost (berdasarkan EAE tahunan):', trace$text, fixed = TRUE)), plots[[name]]$x$data)
    for (cost in costs) {
      needle <- paste0('Perubahan Lahan: cost ', cost, '<br>')
      matched <- Filter(function(trace) any(grepl(needle, trace$text, fixed = TRUE)), traces)
      expect_true(length(matched) >= 1, info = paste(name, cost))
      for (trace in matched) {
        endpoint <- trace$y[which.max(abs(trace$y))]
        expect_equal(sign(as.numeric(endpoint)), sign(cost), info = paste(name, cost))
      }
    }
    zero_tick <- which(plots[[name]]$x$layout$yaxis$tickvals == 0)
    expect_true(length(zero_tick) > 0, info = name)
    expect_equal(plots[[name]]$x$layout$yaxis$ticktext[zero_tick], '0', info = name)
  }
  expect_equal(grouped$opportunity_cost[grouped$carbon_direction == 1], costs)
  expect_equal(grouped$opportunity_cost[grouped$carbon_direction == -1], costs)
})


test_that('a planning unit with only zero cost still produces a chart', {
  zero_curve <- filter(curve, opportunity_cost == 0)
  zero_plot <- generate_plots_by_pu(zero_curve, 'USD')
  expect_s3_class(zero_plot[[1]], 'plotly')
})
