
input_data <- data.table::data.table(
  count=c(20, 13, 12, 11,
          15, 12, 14, 17,
          10, 7, 5, 6)
)
true_result <- data.table::data.table(
  count=c(0, 0, 0, 1,
          0, 0, 0, 0,
          0, 0, 1, 0)
)
cli_options <- list(
  binarize=TRUE,
  target="count"  # name of target variable
)
config <- list(
  binary_threshold=0.6,  # more than 40% decrease necessary to obtain a "1"
  blocklength=4  # 4 values are grouped together in a block
)


# capture memory addresses before function call
init_address_dt <- address(input_data)
init_address_target <- address(input_data[[cli_options$target]])

# function call
simUtils::binarize_target(
  input_data,
  cli_options,
  config
)

# capture memory addresses after function call
post_address_dt <- address(input_data)
post_address_target <- address(input_data[[cli_options$target]])


# tests
test_that(
  "binarization yields only zeroes and ones",
  {
    expect_identical(
      unique(input_data[[cli_options$target]]),
      c(0, 1)
    )
  }
)
test_that(
  "binarization operates in place",
  {
    expect_identical(
      init_address_dt,
      post_address_dt
    )
    expect_identical(
      init_address_target,
      post_address_target
    )
  }
)
test_that(
  "binarization is semantically correct",
  {
    expect_equal(
      input_data,
      true_result
    )
  }
)
