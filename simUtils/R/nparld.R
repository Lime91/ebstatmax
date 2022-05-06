#' Perform Hypothesis Test with nparLD
#'
#' Split the dataset according to the specified period and perform a hypothesis 
#' test.
#' 
#' `options$target` contains the name of the target variable in data.
#' `config$first_period_end` is the last point of time that belongs to the first 
#' study period.
#' `config$time_variable` is the name of the variable containing timepoints in 
#' data.
#' `config$subject_variable` is the name of the variable that identifies 
#' subjects in data.
#' `config$group_variable` is the name of the group variable in data.
#' `config$alpha` is the type-I error rate.
#'
#' @param data `data.table` with the simulation data
#' @param options `list` with user-defined command line arguments
#' @param config `list` with further arguments
#'
#' @return the test result (`TRUE` if H0 is rejected, `FALSE` otherwise)
#' @export
nparld <- function(data,
                   options,
                   config) {
  query <- data[[config$time_variable]] <= config$first_period_end
  period1_data <- data[query]
  period2_data <- data[!query]
  form <- as.formula(paste(
    options$target,
    paste(config$group_variable, config$time_variable, sep=" * "),
    sep=" ~ "))
  capture.output(
    p_value1 <- nparLD::nparLD(
      form,
      period1_data,
      subject=config$subject_variable)$ANOVA.test[3,3],
    p_value2 <- nparLD::nparLD(
      form,
      period2_data,
      subject=config$subject_variable)$ANOVA.test[3,3]
  )
  l <- list(
    period_1=(p_value1),
    period_2=(p_value2),
    combined=NA_real_
  )
  return(l)
}


