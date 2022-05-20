
input_data <- data.table::data.table(
  var=c(0, 0, 0, 1,
        0, 0, 0, 0,
        0, 0, 1, 0),
  time=c(0, 1, 2, 3,
         0, 1, 2, 3,
         4, 5, 6, 7)
)
true_result <- data.table::data.table(
  var=c(0, 0, 1,
        0, 0, 0,
        0, 1, 0),
  time=c(1, 2, 3,
         1, 2, 3,
         5, 6, 7)
)
cli_options <- list(
  binarize=TRUE
)
config <- list(
  time_variable="time",
  baseline_time=c(0, 4)
)


# baseline is discarded if binarize=TRUE
output_data <- simUtils::discard_baseline(
  input_data,
  cli_options,
  config
)

# function does nothing when users specify binarize=FALSE
cli_options$binarize=FALSE
address_input <- address(input_data)
unchanged_output <- simUtils::discard_baseline(
  input_data,
  cli_options,
  config
)
address_unchanged_output <- address(unchanged_output)


# tests
test_that(
  "discard_baseline discards correctly",
  {
    expect_equal(
      output_data,
      true_result
    )
  }
)
test_that(
  "discard_baseline does not touch input data if binarize==FALSE",
  {
    expect_equal(
      address_input,
      address_unchanged_output
    )
  }
)
