# Functions to print program details
# Copyright (C) 2022  Konstantin Emil Thiel  <konstantin.thiel@pmu.ac.at>


# used for printing tables
DUMMY_LINE <- paste(rep("-", 35), collapse="")

#' Pretty-Print a Flat List to Stderr
#'
#' @param l The list to print
#' @param header Title of the list. Will be printed above
print_list_to_stderr <- function(l,
                                 header) {
  df <- t(as.data.frame(l))
  rows <- capture.output(
    print(df, quote=FALSE)
  )
  serialized <- paste(rows[2:length(rows)], collapse="\n")
  cat(
    DUMMY_LINE,
    header,
    DUMMY_LINE,
    serialized,
    DUMMY_LINE,
    sep="\n",
    file=stderr()
  )
}


#' Pretty-Print Program Configuration.
#'
#' @param options List with command line options. All except from 'help' will 
#' be printed
#' @param config List with config parameters. Only selected elements will be 
#' printed
#'
#' @export
print_config_to_stderr <- function(options,
                                   config) {
  l <- within(options, rm(help))
  l$binary_threshold <- config$binary_threshold
  l$alpha <- config$alpha
  l$random_seed <- config$seed
  print_list_to_stderr(l, "PROGRAM CONFIGURATION")
}


#' Summarise Trial Period in a Dataframe
#' 
#' Create a data.frame with information about `data`, which is supposed to 
#' contain only records from a single trial period (i.e., period splitting 
#' must be performed in prior to using this function). The resulting data.frame 
#' will have a row for each subject and three columns. Two of the columns are 
#' named according to `config$subject_variable` and `config$group_variable`, 
#' respectively. The last one is named `Count` and contains a record count of 
#' the respective subject.
#'
#' @param data `data.table` with the study data of one trial period
#' @param config global config object
#' 
#' `config$subject_variable` is the name of the variable that identifies 
#' subjects in data.
#' `config$group_variable` is the name of the group variable in `data`.
#'
#' @return (printable) data.frame with the desired information.
get_period_frame <- function(data,
                             config) {
  subjects <- as.character(data[[config$subject_variable]])
  unique_subjects <- sort(unique(subjects))
  n_subjects <- length(unique_subjects)
  df <- data.frame(
    Count=integer(n_subjects),
    grp=character(n_subjects),
    row.names=unique_subjects,
    stringsAsFactors=FALSE
  )
  for (subject in unique_subjects) {
    subject_frame <- data[subjects == subject, ]
    n <- nrow(subject_frame)
    group <- unique(as.character(subject_frame[[config$group_variable]]))
    df[subject, ] <- list(n, group)
  }
  names(df)[2] <- config$group_variable
  return(df)
}


#' Summarize Dataset Split According to Trial Period
#'
#' Print each trial period info as a separate data.frame.
#'
#' @param data `data.table` with the study data
#' @param config global config object
#' 
#' `config$first_period_end` is the last timepoint in the first trial period.
#' `config$time_variable` is the name of the variable containing timepoints in 
#' `data`.
#' `config$subject_variable` is the name of the variable that identifies 
#' subjects in `data`.
#' `config$group_variable` is the name of the group variable in `data`.
#'
#' @export
print_data_info_to_stderr <- function(data,
                                      config) {
  cat(
    DUMMY_LINE,
    "DATASET CHARACTERISTICS",
    DUMMY_LINE,
    sep="\n",
    file=stderr()
  )
  select <- data[[config$time_variable]] <= config$first_period_end
  periods <- list(data[select], data[!select])
  for (i in 1:2) {
    df <- get_period_frame(periods[[i]], config)
    n_subjects <- nrow(df)
    n_verum <- sum(df[[config$group_variable]] == config$verum_group)
    n_placebo <- sum(df[[config$group_variable]] == config$placebo_group)
    header1 <- paste0("Period ", i)
    header2 <- paste0(
      n_subjects, " subjects (", n_verum, " verum, ", n_placebo, " placebo)")
    rows <- capture.output(
      print.data.frame(df, quote=FALSE)
    )
    serialized <- paste(rows, collapse="\n")
    cat(
      header1,
      header2,
      serialized,
      DUMMY_LINE,
      sep="\n",
      file=stderr()
    )
  }
}
