
#' Check User Input
#'
#' Perform simple sanity checks. Throw error if invalid user input is detected.
#'
#' @param options `list` with user-provided command line arguments
#' @param config `list` that contains valid options to compare with
sanity_check <- function(options,
                         config) {
  if (!(options$scenario %in% config$valid_scenarios))
    stop("Invalid scenario! Must be one of (",
         paste(config$valid_scenarios, collapse=", "), ")")
  
  if (!(options$method %in% config$valid_methods))
    stop("Invalid method! Must be one of ('",
         paste(config$valid_methods, collapse="', '"), "')")
  
  if (!is.null(options$effect))
    if (!(options$effect %in% config$valid_effects))
      stop("Invalid effect! Must be one of ('",
           paste(config$valid_effects, collapse="', '"), "')")
}


#' Permute Target Variable Values
#'
#' Permutation distributes the target variable randomly across study subjects. 
#' More precisely, the target variable is distributed across study entities. 
#' Each study entity consists of a uniformly sized block of target variable 
#' measurements for one subject. The measurements are permuted across these 
#' entities, but not within a block.
#' 
#' `data` will be modified in place.
#'
#' @param data `data.table` with the simulation data
#' @param target name of the target variable column in data
#' @param blocklength number of measurements in a block
permute <- function(data,
                    target,
                    blocklength) {
  n <- length(data$Id)
  blocks <- n/blocklength
  matr_perm <- matrix(data[[target]], nrow=blocklength, ncol=blocks)
  matr_perm <- matr_perm[, sample(1:blocks)]
  data[, c(target) := c(matr_perm)]
}


#' Generate Additive Random Effect
#'
#' The generated effects are supposed to be added to the target variable 
#' afterwards.
#' 
#' The returned vector is rounded to integers.
#'
#' @param effect_type name of the effect distribution
#' @param n size of the returned random sample
#' @param params named vector that maps parameter names to parameter values
#'
#' @return vector with a random sample of size n
generate_effect <- function(effect_type,
                            n,
                            params) {
  if (effect_type == "nbinom") {
    effect <- rnbinom(n, size=params["r"], prob=params["p"])
  } else if (effect_type == "pois") {
    effect <- rpois(n, lambda=params["lambda"])
  } else if (effect_type == "lnorm") {
    effect <- rlnorm(n, meanlog=params["meanlog"], sdlog=params["sdlog"])
  } else if (effect_type == "norm") {
    # avoid negative values
    effect <- rnorm(n, mean=params["mean"], sd=params["sd"])
    effect <- ifelse(effect < 0, 0.0, effect)
  } else {
    stop("invalid effect type")
  }
  return(round(effect, 1))
}


#' Add Independent Random Effect to Target
#' 
#' At a specific group and at specific points of time, add random integer 
#' effects to the target variable.
#' 
#' `options$effect_type` is the name of the distribution of the added effects. 
#' The params argument provides additional parameters for this distribution.
#' `options$target` is the user-defined name of the target variable in `data` 
#' to which effects are applied.
#' 
#' `config$main_effect_time` is a vector that specifies the points of time at 
#' which effects are added.
#' `config$time_variable` is the name of the column in `data` that contains 
#' times.
#' `config$placebo_group` is the name of the group to which effects are added.
#' `config$group_variable` is the name of the column in `data` that contains 
#' groups.
#' 
#' Note that `data` will be modified in place.
#' 
#' @param data `data.table` with the simulation data
#' @param params named vector that maps parameter names to parameter values
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
#'
#' @return vector with the effects that were added to the target variable
add_main_effect <- function(data,
                            params,
                            options,
                            config) {
  times <- config$main_effect_time
  group <- config$placebo_group
  time_variable <- config$time_variable
  group_variable <- config$group_variable
  w <- which(
    (data[[time_variable]] %in% times) & (data[[group_variable]] == group))
  n <- length(w)
  effect <- generate_effect(options$effect, n, params)
  shifted_values <- data[[options$target]][w] + effect
  data[w, c(options$target) := shifted_values]
  return(effect)
}


#' Add Depended Effect to Target
#' 
#' In scenario 2, additionally to the independent effects, there exist also 
#' dependent effects. This function adds these dependent effects to the target 
#' variable: At a specific group and at specific points of time, integer 
#' effects are added to the target variable. These effects depend on the ones 
#' provided as argument to this function.
#' 
#' Currently the dependent effect is simply half of the given effect. It is 
#' ensured that the depended effect consists only of integers by applying 
#' integer division.
#' 
#' `config$dependent_effect_time` is a vector that specifies the points of time 
#' at which effects are added.
#' `config$time_variable` is the name of the column in `data` that contains 
#' timepoints.
#' `config$placebo_group` is the name of the group to which effects are added.
#' `config$group_variable` is the name of the column in `data` that contains 
#' groups.
#' 
#' Note that `data` will be modified in place.
#'
#' @param data `data.table` with the simulation data
#' @param effect vector with integer effects
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
add_dependent_effect <- function(data,
                                 effect,
                                 options,
                                 config) {
  if (options$scenario == 1) {
    return()
  } else if (options$scenario == 2) {
    times <- config$dependent_effect_time
    group <- config$placebo_group
    time_variable <- config$time_variable
    group_variable <- config$group_variable
    w <- which(
      (data[[time_variable]] %in% times) & (data[[group_variable]] == group))
    dependent_effect <- round(effect/2, 1)
    shifted_values <- data[[options$target]][w] + dependent_effect
    data[w, c(options$target) := shifted_values]
  } else {
    stop(paste("scenario", scenario, "is invalid!"))
  }
}


#' Truncate Target Variable
#'
#' To ensure that the target variable values do not exceed their scale they can 
#' be truncated with this function.
#' 
#' Truncation is only applied to the variables specified by the names of 
#' `max_values`.
#' 
#' Note that `data` is modified in place.
#' 
#' @param data `data.table` with the simulation data
#' @param target name of the target column in data
#' @param max_values named vector that maps variable names to maximum values
truncate_target <- function(data,
                            target,
                            max_values) {
  if (target %in% names(max_values)) {
    m <- max_values[target]
    v <- data[[target]]
    data[, c(target) := ifelse(v <= m, v, m)]
  }
}


#' Dichotomize Target Variable
#'
#' If desired, replace the target variable values with 0/1 values.
#' 
#' Target variable values are supposed to be divided into blocks of equal size. 
#' Each block combines measurements for one subject at different points of 
#' time. Whether a measurement is replaced by 0 or by 1 depends on its 
#' magnitude relative to the first value in the same block (i.e., relative to 
#' the baseline measurement). After all target values have been replaced by 
#' either 0 or 1, the baseline measurement is removed from data. This is 
#' because all baseline value would equal 0 (as they cannot decrease relative 
#' to themselves), and hence, do not contain additional information.
#' 
#' `options$target` contains the name of the target variable.
#' `options$binarize` determines if the target should be binarized or not 
#' (`TRUE`/`FALSE`).
#' `config$binary_threshold` a relative measure that determines whether a 
#' target value is replaced by 0 or 1.
#' `config$blocklength` is the number of measurements in a block that refer to 
#' one subject.
#' 
#' Note that `data` is modified in place.
#'
#' @param data `data.table` with the simulation data
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
binarize_target <- function(data,
                            options,
                            config) {
  if (options$binarize) {
    target <- options$target
    n <- nrow(data)
    block_begins <- which(1:n %% config$blocklength == 1)
    for (b in block_begins) {  # 1, blocklength + 1, 2*blocklength + 1, ...
      range <- b:(b + config$blocklength - 1)
      block <- data[range, ..target][[target]]
      binarized <- ifelse(
        block < block[1]*config$binary_threshold,  # decrease desired
        1,
        0)
      data[range, c(target) := binarized]
    }
  }
}


#' Remove Baseline Measurements
#'
#' After all target values have been replaced by either 0 or 1, the baseline 
#' measurement can be removed from data. This is because all baseline values 
#' equal 0 (as they cannot decrease relative to themselves), and hence, do not 
#' contain additional information.
#' 
#' `options$binarize` determines if the target was be binarized or not (`TRUE`/
#' `FALSE`).
#' `config$time_variable` is the name of the variable containing timepoints in 
#' the dataset.
#' `config$baseline_time` contains the points in time at which baseline measures 
#' were collected.
#'
#' @param data `data.table` with the simulation data
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
#' @return the shrinked dataset (original dataset remains untouched)
discard_baseline <- function(data,
                             options,
                             config) {
  if (options$binarize) {
    query <- !(data[[config$time_variable]] %in% config$baseline_time)
    data <- data[query]
  }
  return(data)
}


#' Shift Specific Target Values
#' 
#' In order to simulate power of a statistical test, a random effect is added 
#' to the (permuted) target variable values. Thus, a situation in which H1 
#' holds is established. The distribution of the additive effect is specified 
#' by user command line arguments together with a config object. Depending on 
#' the (user-specified) target variable, the respective values will be 
#' truncated after the effects have been added.
#' 
#' The function is also capable of not adding an effect at all (this is needed 
#' for simulating the type-I error). If this is desired, `NULL` must be handed 
#' over to the `params` argument.
#' 
#' `data` is modified in place.
#' 
#' `options$scenario` determines the simulation scenario. If 
#' `options$scenario == 1`, only independent main effects are added. 
#' `options$scenario == 2` means that, in addition to the main effects, also 
#' dependent effects are added.
#' 
#' `options$target` contains the name of the target variable in `data`.
#' `config$max_values` contains a named vector with maximum values for various 
#' target variables.
#' Moreover, `options` and `config` must contain all entries required by 
#' `add_main_effect` and `add_dependent_effect`.
#' 
#' @param data `data.table` with the simulation data
#' @param params named vector that maps parameter names to parameter values. 
#' If alternatively `NULL` is given, the function returns immediately (without 
#' adding an effect)
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
add_effect <- function(data,
                       params,
                       options,
                       config) {
  if (is.null(params)) return()
  effect_vals <- add_main_effect(data, params, options, config)
  add_dependent_effect(data, effect_vals, options, config)
  truncate_target(data, options$target, config$max_values)
}


#' Perform Hypothesis Test
#'
#' Execute statistical testing procedure selected by user.
#'
#' @param data `data.table` with the simulation data
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
#' 
#' `options$method` is the selected statistical testing procedure.
#' `options$target` contains the name of the target variable in data.
#' `config$first_period_end` is the last point of time that belongs to the first 
#' study period.
#' `config$time_variable` is the name of the variable containing timepoints in 
#' data.
#' `config$subject_variable` is the name of the variable that identifies 
#' subjects in data.
#' `config$group_variable` is the name of the group variable in data.
#' `config$alpha` is the type-I error rate.
#' Moreover, options and config must contain all entries required by 
#' `discard_baseline`.
#'
#' @return a list with keys `period_1`, `period_2`, and `combined`. The  
#' associated values are `TRUE` if the null hypothesis has been rejected and 
#' `FALSE` otherwise. Moreover, `NA` is assigned if the test could not be 
#' performed.
#' @export
perform_test <- function(data,
                         options,
                         config) {
  method <- config$functions[[options$method]]
  args <- method$arguments
  args[["data"]] <- data
  args[["options"]] <- options
  args[["config"]] <- config
  return(do.call(paste0(method$name), args))
}


#' Compute Null Hypothesis Rejection Rate
#'
#' based on data.frame of test results
#'
#' @param results_df `data.frame` with columns of individual test results (p-values)
#' @param alpha type-I error rate
#'
#' @return list of rejection rates with names equal to the input's column names
rejection_rate <- function(results_df,
                           alpha) {
  rates <- apply(
    results_df,
    2,
    function(x) {
      m <- mean(x < alpha, na.rm=TRUE)
      ifelse(is.nan(m), NA_real_, m)
    }
  )
  return(as.list(rates))
}


#' Count Number of Failed Hypothesis Tests
#' 
#' based on data.frame of test results that contains `NA` for each failed test
#'
#' @param results_df `data.frame` with columns of individual test results (p-values)
#'
#' @return list of sums with names equal to the input's column names
na_count <- function(results_df) {
  v <- apply(results_df, 2, function(x) sum(is.na(x)))
  return(as.list(v))
}


#' Summarize Hypothesis Tests
#' 
#' compute H0 rejection rate and count number of failed tests based on a 
#' data.frame of test results that contains p-values (or `NA` for a 
#' failed test).
#'
#' @param results_df `data.frame` with columns of individual test results (p-values)
#' @param alpha type-I error rate
#'
#' @return summary list
summarize_tests <- function(results_df,
                            alpha) {
  l <- list(
    "rejection_rate"=rejection_rate(results_df, alpha),
    "NA_count"=na_count(results_df)
  )
  return(l)
}


#' Simulation-Based Computation of H0 Rejection Rate
#' 
#' For a given number of repetitions, firstly permute the target variable and 
#' secondly, add effects to selected values such that a a situation in which 
#' H1 holds is established. Perform hypotheses tests for both periods of the 
#' trial separately. Store all test results and return the average for both 
#' periods.
#' 
#' The additive random effect is specified by a combination of user input (the 
#' attribute `options$effect`) and the `params` argument. If `NULL` is assigned 
#' to the `params` argument, then no random effect will be added. This way, the 
#' type-I error can be simulated.
#' 
#' Moreover, users can choose to binarize the target variable (cf. 
#' `binarize_target`).
#' 
#' `options$target` contains the name of the target variable.
#' `config$repetitions` is the number of repetitions to perform (i.e., the 
#' number of tests performed for each period)
#' `config$blocklength` is the number of measurements in a block that refer to 
#' one subject.
#' `config$alpha` is the expected type I error rate.
#' `options$binarize` determines whether the target should be binarized (cf. 
#' `binarize_target`).
#' 
#' Moreover, `options` and `config` must contain all attributes required by 
#' `add_effect`, `binarize_target`, `discard_baseline`and `perform_test`.
#'
#' @param data `data.table` with the simulation data
#' @param params named vector that maps parameter names to values or `NULL`
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
#'
#' @return vector with average power values for both periods
#' @export
compute_rejection_rate <- function(data,
                                   params,
                                   options,
                                   config) {
  target <- options$target
  r <- config$repetitions
  p_values <- data.frame(
    "period_1"=rep(NA_real_, r),
    "period_2"=rep(NA_real_, r),
    "combined"=rep(NA_real_, r)
  )
  for (i in 1:r) {
    if ((i - 1) %% (r/5) == 0) cat(i, "/", r, "\n", sep="", file=stderr())
    original <- data.table::copy(data[, ..target])  # save from passing by ref
    permute(data, target, config$blocklength)
    add_effect(data, params, options, config)
    binarize_target(data, options, config)
    testing_data <- discard_baseline(data, options, config)
    p_values[i, ] <- perform_test(testing_data, options, config)
    data[, c(target) := original[[target]]]  # restore original
  }
  return(summarize_tests(p_values, config$alpha))
}
