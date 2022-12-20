
input_data <- data.table::data.table(
  y=c(20,  13,  12,  11, -20,
      15,  12,  14,  17,   0,
      10,   7,   5,   6,  10,
       3,   3,   3,   0,  -1,
       0,  10,  11,  12,   0)
)
true_result <- data.table::data.table(
  y=c(20,  -7,  -8,  -9, -40,
      15,  -3,  -1,   2, -15,
      10,  -3,  -5,  -4,   0,
       3,   0,   0,  -3,  -4,
       0,  10,  11,  12,   0)
)
cli_options <- list(
  subtract=TRUE,
  target="y"  # name of target variable
)
config <- list(
  blocklength=5  # 5 values are grouped together in a block
)

# capture memory addresses before function call
init_address_dt <- address(input_data)
init_address_target <- address(input_data[[cli_options$target]])

# function call
subtract_baseline(
  input_data,
  cli_options,
  config
)

# capture memory addresses after function call
post_address_dt <- address(input_data)
post_address_target <- address(input_data[[cli_options$target]])


test_that(
  "subtraction operates in place",
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
  "subtraction is semantically correct",
  {
    expect_equal(
      input_data,
      true_result
    )
  }
)


# function does nothing when users specify binarize=FALSE
cli_options$subtract=FALSE
deep_copy <- copy(input_data)
subtract_baseline(
  input_data,
  cli_options,
  config
)
test_that(
  "subtract_baseline does not touch input data if subtract==FALSE",
  {
    expect_equal(
      input_data,
      deep_copy
    )
  }
)
