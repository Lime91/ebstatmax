
# global config
target <- "Pain"
effect <- "pois"
options <- list(
  target=target,
  effect=effect
)
config <- CONFIG
parameter <- c(lambda=10)  # must match options$effect
seed <- 1

# load and prepare study data
data("diacerein")  # provided in simUtils package
data <- diacerein
data <- exclude_na_blocks(data, options$target, config$blocklength)
data <- harmonize_period_times(data, config)

# number of blocks to which the effect is added
n <- sum(data$Group == config$placebo_group) / config$blocklength

# timepoints
w_2 <- which(data$Group == "P" & data$Time == 2)  # second timepoint
w_4 <- which(data$Group == "P" & data$Time == 4)  # third timepoint
w_7 <- which(data$Group == "P" & data$Time == 7)  # last timepoint




################################################################################
# scenario 1

# simUtils version
options$scenario <- 1
effect_data <- copy(data)
set.seed(seed)
add_effect(effect_data, parameter, options, config)

# manual version
x <- data[[target]]
set.seed(seed)
r <- rpois(n, parameter)
x[w_4] <- x[w_4] + r
x <- ifelse(x > 10, 10, x)

test_that(
  "add_effect works for scenario 1 with variable 'Pain' and pois-effects",
  {
    expect_identical(
      effect_data[[options$target]],
      x
    )
  }
)


################################################################################
# scenario 2

# simUtils version
options$scenario <- 2
effect_data <- copy(data)
set.seed(seed)
add_effect(effect_data, parameter, options, config)

# manual version
x <- data[[target]]
set.seed(seed)
r <- rpois(n, parameter)
x[w_4] <- x[w_4] + r
x[w_7] <- x[w_7] + round(r / 2, 1)
x <- ifelse(x > 10, 10, x)

# check
test_that(
  "add_effect works for scenario 2 with variable 'Pain' and pois-effects",
  {
    expect_identical(
      effect_data[[options$target]],
      x
    )
  }
)


################################################################################
# scenario 3

# simUtils version
options$scenario <- 3  # only integer effects in scenario 3
effect_data <- copy(data)
set.seed(seed)
add_effect(effect_data, parameter, options, config)

# manual version
x <- data[[target]]
set.seed(seed)
r <- rpois(n, parameter)
x[w_2] <- x[w_2] + round(r / 2)
x[w_4] <- x[w_4] + r
x[w_7] <- x[w_7] + round(r / 2) + round(rnorm(n))
x <- ifelse(x > 10, 10, x)

# check
test_that(
  "add_effect works for scenario 3 with variable 'Pain' and pois-effects",
  {
    expect_identical(
      effect_data[[options$target]],
      x
    )
  }
)


# reset parameter to obtain negative values
parameter <- c("lambda"=0)
seed <- 16630  # yields negative values with rounded rnorm

# sanity check
set.seed(seed)
rpois(n, parameter)
test_that(
  "rnorm produces highly negatively skewed values for the given random seed",
  {
    expect_identical(
      sum(round(rnorm(n))),
      -15
    )
  }
)

# simUtils version
effect_data <- copy(data)
set.seed(seed)
add_effect(effect_data, parameter, options, config)

# manual version
x <- data[[target]]
set.seed(seed)
r <- rpois(n, parameter)
x[w_2] <- x[w_2] + round(r / 2)
x[w_4] <- x[w_4] + r
x[w_7] <- x[w_7] + round(r / 2) + round(rnorm(n))
x <- ifelse(x > 10, 10, x)
x <- ifelse(x < 0, 0, x)

# check
test_that(
  "add_effect works for scenario 3 when negative values are introduced",
  {
    expect_identical(
      effect_data[[options$target]],
      x
    )
  }
)


