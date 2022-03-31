
#' Read in Diacerin Study Dataset
#'
#' The dataset is read in as `data.table`. This is important as many of the 
#' other functions require this specific data type. The dataset is supposed to 
#' consist of tab-separated values (tsv). Its rows will be sorted according 
#' to `config$subject_variable` and `config$time_variable`. For this purpose, 
#' the time variable will be casted to numeric.
#' 
#' @param filename path to the tab-separated dataset file
#' @param config `list` with further arguments
#'
#' @return the preprocessed dataset as `data.table`
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


#' Remove Blocks Containing NA in Target
#' 
#' If NAs occur in the target variable, the respective blocks will be removed 
#' from the dataset (i.e., the NA-rows as well as their surrounding rows will 
#' be removed)
#'
#' @param data `data.table` with the simulation data
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
