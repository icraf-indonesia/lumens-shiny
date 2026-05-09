test_that("validate_csv_schema checks required columns", {
  # Valid raster inputs CSV
  fixtures <- load_test_fixtures()

  result <- validate_csv_schema(
    fixtures$raster_inputs,
    required_cols = c("ID", "parameter", "name_parameter", "availability", "raster_path")
  )

  expect_true(result$valid)
  expect_equal(nrow(result$errors), 0)
})

test_that("validate_csv_schema detects missing columns", {
  bad_csv <- tibble::tibble(
    ID = 1:2,
    parameter = c("A", "B")
    # Missing name_parameter, availability, raster_path
  )

  result <- validate_csv_schema(
    bad_csv,
    required_cols = c("ID", "parameter", "name_parameter", "availability", "raster_path")
  )

  expect_false(result$valid)
  expect_gt(nrow(result$errors), 0)
  expect_match(result$errors$message[1], "Missing required columns")
})

test_that("validate_csv_schema detects wrong data types", {
  bad_csv <- tibble::tibble(
    ID = c("a", "b"), # Should be numeric
    parameter = c("A", "B"),
    name_parameter = c("a", "b"),
    availability = c("Yes", "No"),
    raster_path = c("path1", "path2")
  )

  result <- validate_csv_schema(
    bad_csv,
    required_cols = c("ID", "parameter", "name_parameter", "availability", "raster_path")
  )

  expect_false(result$valid)
  expect_match(result$errors$message[1], "Column 'ID' should be numeric")
})
