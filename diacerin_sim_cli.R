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
cat(get_split_info(data, CONFIG), "\n")

# start simulations
if (opt$compute_alpha) {
  cat("computing alpha error...\n")
  l <- compute_alpha_error(data, opt, CONFIG)
  cat("period 1: error= ", l$period_1$error,
      " (#NA= ", l$period_1$na_count, ")\n",
      "period 2: error= ", l$period_2$error,
      " (#NA= ", l$period_2$na_count, ")\n\n",
      sep="")
}
cat("computing power...\n")
for (p in parameters) {
  l <- compute_power(data, p, opt, CONFIG)
  key <- paste(names(p), p, sep="=", collapse=", ")
  cat(key, "\n")
  cat("period 1: power= ", l$period_1$power,
      " (#NA= ", l$period_1$na_count, ")\n",
      "period 2: power= ", l$period_2$power,
      " (#NA= ", l$period_2$na_count, ")\n\n",
      sep="")
}
