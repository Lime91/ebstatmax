#!/usr/bin/Rscript


# simulation of the Diacerin study for the EBStatMax project

# usage (on linux, ubuntu):   ./diacerin_sim_cli.R --help
# usage (general):            Rscript diacerin_sim_cli.R --help


suppressPackageStartupMessages(require(optparse))

source("./functions.R")  # import functions
source("./config.R")  # import a global CONFIG object

# command line option parsing
option_list <- list(
  make_option(c("-d", "--dataset"),
              action="store",
              default="./data/Diacerein_study-setup.txt",
              type="character",
              help=paste0("Path to the Diacerin-study dataset file. ",
                          "[default %default].")),
  make_option(c("-s", "--scenario"),
              action="store",
              default=CONFIG$valid_scenarios[1],
              type="integer",
              help=paste0("The required simulation scenario. ",
                          "#1: add effects at post-treatment only. ",
                          "#2: effects at post-treatment and (less markedly) ",
                          "at follow-up. [default %default].")),
  make_option(c("-t", "--target"),
              action="store",
              default="Blister_count",
              help=paste0("Name of the explained variable ",
                          "(to which effects are applied). ",
                          "[default %default]")),
  make_option(c("-e", "--effect"),
              action="store",
              default=CONFIG$valid_effects[1],
              type="character",
              help=paste0("Type of the added effects. One of (",
                          paste(CONFIG$valid_effects, collapse=", "),
                          ") [default %default].")),
  make_option(c("-c", "--compute-alpha"),
              action="store_true",
              default=FALSE,
              help="Set flag to compute alpha error in addition to power."),
  make_option(c("-b", "--binarize"),
              action="store_true",
              default=FALSE,
              help=paste0("Set flag to binarize the target variable before ",
                          "testing the H0. The relative binarization ",
                          "threshold is set in the config."))
)
opt <- parse_args(OptionParser(option_list=option_list),
                  convert_hyphens_to_underscores=TRUE)

# sanity check
if (!(opt$scenario %in% CONFIG$valid_scenarios))
  stop("Scenario must be in (",
       paste(CONFIG$valid_scenarios, collapse=", "), ")")

if (!(opt$effect %in% CONFIG$valid_effects))
  stop("Effect must be in ('",
       paste(CONFIG$valid_effects, collapse="', '"), "')")

# status message
cat("\n")
cat("Starting simulations with the following parameters:\n")
cat("-) dataset:", opt$dataset, "\n")
cat("-) scenario:", opt$scenario, "\n")
cat("-) target variable:", opt$target, "\n")
cat("-) effect:", opt$effect, "\n")
cat("-) binarization:", opt$binarize)
cat(" (threshold = ", CONFIG$binary_threshold, ")\n", sep="")
cat("-) #repetitions:", CONFIG$repetitions, "\n")
cat("-) alpha-level:", CONFIG$alpha, "\n")
cat("\n")


# determine parameters to iterate over
parameters <- CONFIG$parameters[[opt$effect]]

# program start
set.seed(CONFIG$seed)
data <- read_data(opt$dataset, CONFIG)

# exclude NAs and print dataset info
reduced_data <- exclude_na_blocks(data, opt$target, CONFIG$blocklength)
diff <- nrow(data) - nrow(reduced_data)
if (diff != 0) {
  cat(diff, "rows have been removed from the dataset due to NA-values.\n\n")
  data <- reduced_data
}
cat(get_dataset_info(data, CONFIG), "\n")

# start simulations
if (opt$compute_alpha) {
  cat("computing alpha error...\n")
  alpha_errors <- compute_alpha_error(data, opt, CONFIG)
  cat("P1: ", alpha_errors[1], ", P2: ", alpha_errors[2], "\n\n", sep="")
}
cat("computing power...\n")
power <- list()
for (p in parameters) {
  results <- compute_power(data, p, opt, CONFIG)
  key <- paste(names(p), p, sep="=", collapse=", ")
  power[[key]] = results
  cat(key, "\n")
  cat("P1: ", results[1], ", P2: ", results[2], "\n\n", sep="")
}
