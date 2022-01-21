
#' Remove Blocks Containing NA in Target
#' 
#' If NAs occur in the target variable, the respective blocks will be removed 
#' from the dataset (i.e., the NA-rows as well as their surrounding rows will 
#' be removed)
#'
#' @param data data.table with the simulation data
#' @param target name of the target variable column in data
#' @param blocklength number of measurements in a block
#'
#' @return the input data reduced by excluded NA-blocks
#' @export
exclude_na_blocks <- function(data,
                              target,
                              blocklength) {
  w <- which(is.na(data[[target]]))
  if (length(w) == 0) {  # nothing to exclude
    return(data)
  }
  modulo <- w %% blocklength
  division <- w %/% blocklength
  block_numbers <- unique(ifelse(modulo == 0, division, division + 1))
  select <- integer()
  for (k in block_numbers) {
    select <- c(select, ((k - 1)*blocklength + 1):(k*blocklength))
  }
  return(data[-select])
}


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
#' @param data data.table with the simulation data
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
    effect <- max(rnorm(n, mean=params["mean"], sd=params["sd"]), 0)
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
#' config$placebo_group is the name of the group to which effects are added.
#' config$group_variable is the name of the column in data that contains groups.
#' 
#' Note that data will be modified in place.
#' 
#' @param data data.table with the simulation data
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
#' config$dependent_effect_time is a vector that specifies the points of time 
#' at which effects are added.
#' config$time_variable is the name of the column in data that contains times.
#' 
#' config$placebo_group is the name of the group to which effects are added.
#' config$group_variable is the name of the column in data that contains groups.
#' 
#' Note that data will be modified in place.
#'
#' @param data data.table with the simulation data
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
    group <- config$placebo_group
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
#' @param data data.table with the simulation data
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
#' options$target contains the name of the target variable.
#' options$binarize determines if the target should be binarized or not (TRUE/
#' FALSE).
#' config$binary_threshold a relative measure that determines whether a target 
#' value is replaced by 0 or 1.
#' config$blocklength is the number of measurements in a block that refer to 
#' one subject.
#' config$baseline_time contains the points in time at which baseline measures 
#' were collected.
#' 
#' Note that data is modified in place.
#'
#' @param data data.table with the simulation data
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
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
#' options$binarize determines if the target was be binarized or not (TRUE/
#' FALSE).
#' config$time_variable is the name of the variable containing timepoints in 
#' the dataset.
#' config$baseline_time contains the points in time at which baseline measures 
#' were collected.
#'
#' @param data data.table with the simulation data
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
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
#' data is modified in place.
#' 
#' options$scenario determines the simulation scenario. If 
#' options$scenario == 1, only independent main effects are added. 
#' options$scenario == 2 means that, in addition to the main effects, also 
#' dependent effects are added.
#' 
#' options$target contains the name of the target variable in data.
#' config$max_values contains a named vector with maximum values for various 
#' target variables.
#' Moreover, options and config must contain all entries required by 
#' add_main_effect and add_dependent_effect.
#' 
#' @param data data.table with the simulation data
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
}


#' Perform Hypothesis Test
#'
#' Split the dataset according to the specified period and perform a hypothesis 
#' test.
#' 
#' options$target contains the name of the target variable in data.
#' config$first_period_end is the last point of time that belongs to the first 
#' study period.
#' config$time_variable is the name of the variable containing timepoints in 
#' data.
#' config$subject_variable is the name of the variable that identifies subjects 
#' in data.
#' config$group_variable is the name of the group variable in data.
#' config$alpha is the type-I error rate.
#' Moreover, options and config must contain all entries required by 
#' discard_baseline.
#'
#' @param data data.table with the simulation data
#' @param period study period (either 1 or 2)
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
#' @param target name of the target variable
#' @param alpha type-I error rate
#'
#' @return the test result (TRUE if H0 is rejected, FALSE otherwise)
test_h0 <- function(data,
                    period,
                    options,
                    config) {
  data <- discard_baseline(data, options, config)  # discard baseline if needed
  query <- data[[config$time_variable]] <= config$first_period_end
  if (period == 1) {
    data <- data[query]
  } else {  # period == 2
    data <- data[!query]
  }
  form <- as.formula(paste(
    options$target,
    paste(config$group_variable, config$time_variable, sep=" * "),
    sep=" ~ "))
  capture.output(
    p_value <- nparLD::nparLD(
      form,
      data,
      subject=config$subject_variable)$ANOVA.test[3,3]
  )
  return(p_value < config$alpha)
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
#' @param data data.table with the simulation data
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
#'
#' @return vector with average type-I errors for both periods
#' @export
compute_alpha_error <- function(data,
                                options,
                                config) {
  target <- options$target
  non_binarized <- data.table::copy(data[, ..target])  # save from binarization
  binarize_target(data, options, config)
  r <- config$repetitions
  results1 <- rep(-1, r) 
  results2 <- rep(-1, r)
  for (i in 1:r) {
    original <- data.table::copy(data[, ..target])  # save from passing by ref
    permute(data, target, config$blocklength)
    results1[i] <- test_h0(data, 1, options, config)
    results2[i] <- test_h0(data, 2, options, config)
    data[, c(target) := original[[target]]]  # restore original
  }
  data[, c(target) := non_binarized[[target]]]  # restore after binarization
  l <- list(
    period_1=list(
      error=mean(results1, na.rm=TRUE),
      na_count=sum(is.na(results1))),
    period_2=list(
      error=mean(results2, na.rm=TRUE),
      na_count=sum(is.na(results2))))
  return(l)
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
#' attribute options$effect) and the params argument. Users can also choose to 
#' binarize the target variable.
#' 
#' options$target contains the name of the target variable.
#' config$repetitions is the number of repetitions to perform (i.e., the 
#' number of tests performed for each period)
#' config$blocklength is the number of measurements in a block that refer to 
#' one subject.
#' config$alpha is the expected type I error rate.
#' options$binarize determines if the target should be binarized (cf. 
#' binarize_target).
#' 
#' Moreover, options and config must contain all attributes required by 
#' add_effect.
#'
#' @param data data.table with the simulation data
#' @param params named vector that maps parameter names to values
#' @param options list with user-defined command line arguments
#' @param config list with further arguments
#'
#' @return vector with average power values for both periods
#' @export
compute_power <- function(data,
                          params,
                          options,
                          config) {
  target <- options$target
  r <- config$repetitions
  results1 <- rep(-1, r) 
  results2 <- rep(-1, r)
  for (i in 1:r) {
    original <- data.table::copy(data[, ..target])  # save from passing by ref
    permute(data, target, config$blocklength)
    add_effect(data, params, options, config)
    binarize_target(data, options, config)
    results1[i] <- test_h0(data, 1, options, config)
    results2[i] <- test_h0(data, 2, options, config)
    data[, c(target) := original[[target]]]  # restore original
  }
  l <- list(
    period_1=list(
      power=mean(results1, na.rm=TRUE),
      na_count=sum(is.na(results1))),
    period_2=list(
      power=mean(results2, na.rm=TRUE),
      na_count=sum(is.na(results2))))
  return(l)
}


#' Read in Diacerin Study Dataset
#'
#' The dataset is read in as data.table. This is important as many of the other 
#' functions require this data type. The dataset is supposed to consist of 
#' tab-separated values (tsv). Its rows will be reordered according to 
#' config$subject_variable and config$time_variable. Moreover, the time 
#' variable will be casted to numeric to establish an order w.r.t. time.
#' 
#' @param filename path to the tab-separated dataset file
#' @param config list with further arguments
#'
#' @return the preprocessed dataset as data.table
#' @export
read_data <- function(filename,
                      config) {
  data = read.delim2(filename, na.strings=c("n/a"))
  # create numeric time variable to establish time order
  time <- config$time_variable
  data[[time]] <- as.numeric(gsub("\\D", "", data[[time]]))
  # reorder rows to establish subject-time blocks
  subject <- config$subject_variable
  o <- order(data[[subject]], data[[time]])
  data <- data[o, ]
  return(data.table::as.data.table(data))
}


#' Summarise Dataset Containing a Single Trial Period
#' 
#' Create a (multiline) string with information about the given dataset.
#'
#' @param data data.table with the study data
#' @param config global config object
#' 
#' config$subject_variable is the name of the variable that identifies subjects 
#' in data.
#' config$group_variable is the name of the group variable in data.
#'
#' @return printable string with dataset info.
#' @export
get_period_info <- function(data,
                            config) {
  svar <- config$subject_variable
  gvar <- config$group_variable
  v <- config$verum_group
  p <- config$placebo_group
  p_count <- 0
  v_count <- 0
  ids <- data[[config$subject_variable]]
  unique_ids <- sort(unique(ids))
  text <- ""
  for (id in unique_ids) {
    id_frame <- data[ids == id, ]
    n <- nrow(id_frame)
    g <- unique(as.character(id_frame[[gvar]]))
    if (g == v) {
      v_count <- v_count + 1
    } else if (g == p) {
      p_count <- p_count + 1
    } else {
      stop(paste0("group '", g, "' is invalid (subject ", id, ")"))
    }
    row <- paste0(id, ", ", n, ", ", g, "\n")
    text <- paste0(text, row)
  }
  text <- paste0(length(unique_ids),
                 " subjects found (",
                 v_count, " verum and ", p_count, " placebo).\n",
                 svar, ", count, ", gvar, ":\n",
                 text)
  return(text)
}


#' Summarize Dataset Split According to Trial Period
#'
#' Create a (multiline) string with information about the given dataset.
#'
#' @param data data.table with the study data
#' @param config global config object
#' 
#' config$first_period_end is the last timepoint in the first trial period.
#' config$time_variable is the name of the variable containing timepoints in 
#' the dataset.
#' config$subject_variable is the name of the variable that identifies subjects 
#' in data.
#' config$group_variable is the name of the group variable in data.
#'
#' @return printable string with dataset info.
#' @export
get_split_info <- function(data,
                           config) {
  select <- data[[config$time_variable]] <= config$first_period_end
  periods <- list(data[select], data[!select])
  text <- ""
  for (i in 1:2) {
    text <- paste0(text, "period ", i, ":\n",
                   get_period_info(periods[[i]], config), "\n")
  }
  return(text)
}