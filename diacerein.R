#!/usr/bin/Rscript

# simulation of the Diacerin study for the EBStatMax project
# Copyright (C) 2022  Konstantin Emil Thiel  <konstantin.thiel@pmu.ac.at>

# usage (on linux, ubuntu):   ./diacerein.R --help
# usage (general):            Rscript diacerein.R --help


suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(devtools))
suppressPackageStartupMessages(require(jsonlite))

# load self-written utilities
if (!suppressPackageStartupMessages(
    suppressWarnings(require(simUtils))))
  suppressMessages(devtools::load_all("simUtils"))


# command line option parsing
option_list <- list(
  make_option(c("-m", "--method"),
              action="store",
              default=CONFIG$valid_methods[1],
              type="character",
              help=paste0("Statistical testing procedure to simulate. One of (",
                          paste(CONFIG$valid_methods, collapse=", "),
                          "). [default %default]")),
  make_option(c("-d", "--dataset"),
              action="store",
              type="character",
              help=paste0("Path to the Diacerin-study dataset file. ",
                          "If omitted, the original study dataset from the ",
                          "simUtils package will be used.")),
  make_option(c("-s", "--scenario"),
              action="store",
              default=CONFIG$valid_scenarios[1],
              type="integer",
              help=paste0("The required simulation scenario. ",
                          "#1: add effects at post-treatment only. ",
                          "#2: effects at post-treatment and, less markedly, ",
                          "at follow-up. [default %default]")),
  make_option(c("-t", "--target"),
              action="store",
              default="Blister_count",
              type="character",
              help=paste0("Name of the outcome variable to which effects ",
                          "are applied. [default %default]")),
  make_option(c("-e", "--effect"),
              action="store",
              type="character",
              help=paste0("Type of the added effects. One of (",
                          paste(CONFIG$valid_effects, collapse=", "),
                          "). If omitted, no random effects are added and ",
                          "type-I error is simulated instead of power.")),
  make_option(c("-b", "--binarize"),
              action="store_true",
              default=FALSE,
              type="logical",
              help=paste0("Binarize the target variable before testing the H0.",
                          " The binary target is computed w.r.t. the baseline ",
                          "observation. Hence, the baseline itself becomes ",
                          "redundant and will be discarded. The relative ",
                          "binarization threshold is set in the config file.")),
  make_option(c("-u", "--side"),
              action="store",
              default=2,
              type="integer",
              help=paste0("Perform either a one-sided (1) or a two-sided (2) ",
                          " hypothesis test. This parameter is only valid for ",
                          "the gpc methods. [default %default]")),
  make_option(c("-r", "--subtract"),
              action="store_true",
              default=FALSE,
              type="logical",
              help=paste0("Subtract baseline measurement from all other ",
                          "observations. [default %default]")),
  make_option(c("-i", "--discard"),
              action="store_true",
              default=FALSE,
              type="logical",
              help=paste0("Discard baseline measurement. This option is ",
                          "implied by '--binarize' and '--subtract'. ",
                          "[default %default]")),
  make_option(c("-n", "--runs"),
              action="store",
              default=CONFIG$repetitions,
              type="integer",
              help=paste0("Number of runs. [default %default]"))
)

opt <- parse_args(OptionParser(option_list=option_list),
                  convert_hyphens_to_underscores=TRUE)
simUtils::sanity_check(opt, simUtils::CONFIG)
if (opt$binarize || opt$subtract) opt$discard <- TRUE
simUtils::print_config_to_stderr(opt, simUtils::CONFIG)

# load data
if (is.null(opt$dataset)) {
  data("diacerein")
  dataset <- diacerein
  rm(diacerein)
} else {
  dataset <- simUtils::read_data(opt$dataset, simUtils::CONFIG)
}

# exclude NAs and print dataset info
reduced_data <- simUtils::exclude_na_blocks(
  dataset, opt$target, simUtils::CONFIG$blocklength)
diff <- nrow(dataset) - nrow(reduced_data)
if (diff != 0) {
  cat(diff, "rows have been removed from the dataset due to NA-values.\n\n",
      file=stderr())
  dataset <- reduced_data
} else 
  cat("\n", file=stderr())
simUtils::print_data_info_to_stderr(dataset, simUtils::CONFIG)

# gpc requires time ids harmonized over periods
if (opt$method != "nparld") {
  dataset <- simUtils::harmonize_period_times(dataset, CONFIG)
}

# prepare results for stdout
results <- list(
  "method"=opt$method,
  "target"=opt$target,
  "effect"=ifelse(is.null(opt$effect), "NA", opt$effect),
  "scenario"=opt$scenario,
  "side"=opt$side,
  "binarize"=opt$binarize,
  "runs"=opt$runs
)

# start simulations
set.seed(simUtils::CONFIG$seed)


if (is.null(opt$effect)) {
  cat("computing alpha error...\n", file=stderr())
  results[["alpha_error"]] <- simUtils::compute_rejection_rate(
    dataset, NULL, opt, simUtils::CONFIG)
} else {
  cat("computing power...\n", file=stderr())
  power <- list()
  parameters <- simUtils::CONFIG$parameters[[opt$effect]]
  for (params in parameters) {
    key <- paste(names(params), round(params, 2), sep="=", collapse=", ")
    cat(key, "\n", sep="", file=stderr())
    pwr <- simUtils::compute_rejection_rate(
      dataset, params, opt, simUtils::CONFIG)
    power[[key]] <- pwr
  }
  results[["power"]] <- power
}

# print results to stdout
j <- jsonlite::toJSON(
  results,
  pretty=T,
  auto_unbox=T
)
cat(j, "\n")
