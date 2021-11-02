# functions to be deployed in diacerin_sim_cli.R

# attach required libraries
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(nparLD))


#' Permute Target Variable Values
#'
#' Permutation distributes the target variable randomly across study subjects. 
#' More precisely, the target variable is distributed across study entities. 
#' Each study entity consists of a uniformly sized block of target variable 
#' measurements for one subject. The measurements are permuted across these 
#' entities, but not within a block.
#' 
#' The given dataset will be modified in place.
#'
#' @param data data.table with the full study dataset
#' @param target name of the target variable column in data
#' @param blocklength number of measurements in a block
permute <- function(data,
                    target,
                    blocklength) {
  n <- length(data$Id)
  blocks <- n/blocklength
  matr_perm = matrix(data[[target]], nrow=blocklength, ncol=blocks)
  matr_perm = matr_perm[, sample(1:blocks)]
  data[, c(target) := c(matr_perm)]
}


#' Generate Additive Random Effect
#'
#' The generated effects are supposed to be added to the target variable 
#' afterwards.
#' 
#' The returned vector is rounded to integers.
#'
#' @param effect_type name of the effect distribtion
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
    effect <- rnorm(n, mean=params["mean"], sd=params["sd"])
  } else {
    stop("invalid effect type")
  }
  return(round(effect))
}


#' Add Independent Random Effect to Target
#' 
#' At a specific group and at specific points of time, add random integer 
#' effects to the target variable.
#' 
#' options$effect_type is the name of the distribution of the added effects. 
#' The params argument provides additional parameters for this distribution.
#' 
#' options$target is the user-defined name of the target variable in data to 
#' which effects are applied.
#' 
#' config$main_effect_time is a vector that specifies the points of time at 
#' which effects are added.
#' config$time_variable is the name of the column in data that contains times.
#' 
#' config$effect_group is the name of the group to which effects are added.
#' config$group_variable is the name of the column in data that contains groups.
#' 
#' Note that data will be modified in place.
#' 
#' @param data data.table with the full study dataset
#' @param params named vector that maps parameter names to parameter values
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
#'
#' @return vector with the effects that were added to the target variable
add_main_effect <- function(data,
                            params,
                            options,
                            config) {
  times <- config$main_effect_time
  group <- config$effect_group
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
#' config$dependent_effect_time is a vector that specifies the points of time 
#' at which effects are added.
#' config$time_variable is the name of the column in data that contains times.
#' 
#' config$effect_group is the name of the group to which effects are added.
#' config$group_variable is the name of the column in data that contains groups.
#' 
#' Note that data will be modified in place.
#'
#' @param data data.table with the full study dataset
#' @param effect vector with integer effects
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
add_dependent_effect <- function(data,
                                 effect,
                                 options,
                                 config) {
  if (options$scenario == 1) {
    return()
  } else if (options$scenario == 2) {
    times <- config$dependent_effect_time
    group <- config$effect_group
    time_variable <- config$time_variable
    group_variable <- config$group_variable
    w <- which(
      (data[[time_variable]] %in% times) & (data[[group_variable]] == group))
    dependent_effect <- effect %/% 2
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
#' max_values.
#' 
#' Note that data is modified in place.
#' 
#' @param data data.table with the full study dataset
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
#' magnitude relative to the first value in the same block.
#' 
#' Note that data is modified in place.
#'
#' @param binarize whether to apply binarization or not (TRUE/FALSE)
#' @param data data.table with the full study dataset
#' @param target name of the target column in data
#' @param blocklength number of measurements in a block
#' @param threshold decides whether a value is replaced by 0 or 1
binarize_target <- function(binarize,
                            data,
                            target,
                            blocklength,
                            threshold) {
  if (binarize) {
    n <- nrow(data)
    block_begins <- which(1:n %% blocklength == 1)
    for (b in block_begins) {  # 1, blocklength + 1, 2*blocklength + 1, ...
      range <- b:(b + blocklength - 1)
      block <- data[range, ..target][[target]]
      relative <- block/block[1]
      binarized <- ifelse(relative < threshold, 1, 0)  # decrease is desired
      data[range, c(target) := binarized]
    }
  }
}


#' Shift Specific Target Values
#' 
#' In order to simulate power of a statistical test, a random effect is added 
#' to the (permuted) target variable values. Thus, a situation in which H1 
#' holds is established. The distribution of the additive effect is specified 
#' by user command line arguments together with a config object. Depending on 
#' the (user-specified) target variable, the respective values will be 
#' truncated after the effects have been added. Moreover, users may choose to 
#' binarize the target variable, which is done also in this function.´
#' 
#' data is modified in place.
#' 
#' options$scenario determines the simulation scenario. If 
#' options$scenario == 1, only independent main effects are added. 
#' options$scenario == 2 means that, in addition to the main effects, also 
#' dependet effects are added.
#' 
#' options$target contains the name of the target variable in data.
#' options$binarize determines if the target should be binarized (cf. 
#' binarize_target).
#' config$max_values contains a named vector with maximum values for various 
#' target variables.
#' Moreover, options and config must contain all entries required by 
#' add_main_effect and add_dependent_effect.
#' 
#' @param data data.table with the full study dataset
#' @param params named vector that maps parameter names to parameter values
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
add_effect <- function(data,
                       params,
                       options,
                       config) {
  effect_vals <- add_main_effect(data, params, options, config)
  add_dependent_effect(data, effect_vals, options, config)
  truncate_target(data, options$target, config$max_values)
  binarize_target(options$binarize, data, options$target,
                  config$blocklength, config$binary_threshold)
}


#' Perform Hypothesis Test
#'
#' Split the dataset accorind to the soecified period and perform a hypothesis 
#' test.
#'
#' @param data data.table with the full study dataset
#' @param period study period (either 1 or 2)
#' @param target name of the target variable
#' @param alpha type-I error rate
#'
#' @return the test result (TRUE if H0 is rejected, FALSE otherwise)
test_h0 <- function(data,
                    period,
                    target,
                    alpha) {
  if (period == 1) {
    data <- subset(data, TimeNum <= 4)
  } else {
    data <- subset(data, TimeNum > 4)
  }
  form <- as.formula(paste(target, "GroupNum * TimeNum", sep=" ~ "))
  capture.output(
    p_value <- nparLD(form, data, subject="Id")$ANOVA.test[3,3]
  )
  return(p_value < alpha)
}


#' Simulation-based Computation of the Type-I Error
#' 
#' For a given number of repetitions, permute the target variable to establish 
#' a situation in which H0 holds. Perform hypotheses tests for both periods of 
#' the trial separately. Store all test results and return the average for both 
#' periods.
#' 
#' options$target contains the name of the target variable.
#' config$repetitions is the number of repetitions to perform (i.e., the 
#' number of tests performed for each period)
#' config$blocklength is the number of measurements in a block that refer to 
#' one subject.
#' config$alpha is the expected type I error rate.
#'
#' @param data data.table with the full study dataset
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
#'
#' @return vector with average type-I errors for both periods
compute_alpha_error <- function(data,
                                options,
                                config) {
  target <- options$target
  r <- config$repetitions
  results1 <- rep(-1, r) 
  results2 <- rep(-1, r)
  for (i in 1:r) {
    original <- copy(data[, ..target])  # save from passing by reference
    permute(data, target, config$blocklength)
    results1[i] <- test_h0(data, 1, target, config$alpha)
    results2[i] <- test_h0(data, 2, target, config$alpha)
    data[, c(target) := original[[target]]]  # restore original
  }
  return(c(period1=mean(results1), period2=mean(results2)))
}


#' Simulation-based Computation of Power
#' 
#' For a given number of repetitions, firstly permute the target variable and 
#' secondly, add effects to selected values such that a a situation in which 
#' H1 holds is established. Perform hypotheses tests for both periods of the 
#' trial separately. Store all test results and return the average for both 
#' periods.
#' 
#' The additive random effect is specified by a combination of user input (the 
#' attribute options$effect) and the params argument.
#' 
#' options$target contains the name of the target variable.
#' config$repetitions is the number of repetitions to perform (i.e., the 
#' number of tests performed for each period)
#' config$blocklength is the number of measurements in a block that refer to 
#' one subject.
#' config$alpha is the expected type I error rate.
#' 
#' Moreover, options and config must contain all attributes required by 
#' add_effect.
#'
#' @param data data.table with the full study dataset
#' @param params named vector that maps parameter names to values
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
#'
#' @return vector with average power values for both periods
compute_power <- function(data,
                          params,
                          options,
                          config) {
  target <- options$target
  r <- config$repetitions
  results1 <- rep(-1, r) 
  results2 <- rep(-1, r)
  for (i in 1:r) {
    original <- copy(data[, ..target])  # save from passing by reference
    permute(data, target, config$blocklength)
    add_effect(data, params, options, config)
    results1[i] <- test_h0(data, 1, target, config$alpha)
    results2[i] <- test_h0(data, 2, target, config$alpha)
    data[, c(target) := original[[target]]]  # restore original
  }
  return(c(period1=mean(results1), period2=mean(results2)))
}
