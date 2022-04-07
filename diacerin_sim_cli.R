#!/usr/bin/Rscript


# simulation of the Diacerin study for the EBStatMax project

# usage (on linux, ubuntu):   ./diacerin_sim_cli.R --help
# usage (general):            Rscript diacerin_sim_cli.R --help


suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(devtools))
suppressPackageStartupMessages(require(jsonlite))

suppressMessages(devtools::load_all("simUtils"))  # load self-written utilities


# command line option parsing
option_list <- list(
  make_option(c("-m", "--method"),
              action="store",
              default=CONFIG$valid_methods[1],
              type="character",
              help=paste0("Statistical testing procedure to simulate. One of (",
                          paste(CONFIG$valid_methods, collapse=", "),
                          ") [default %default].")),
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

simUtils::sanity_check(opt, simUtils::CONFIG)
simUtils::print_config_to_stderr(opt, simUtils::CONFIG)

set.seed(simUtils::CONFIG$seed)
data <- simUtils::read_data(opt$dataset, simUtils::CONFIG)

# exclude NAs and print dataset info
reduced_data <- simUtils::exclude_na_blocks(
  data, opt$target, simUtils::CONFIG$blocklength)
diff <- nrow(data) - nrow(reduced_data)
if (diff != 0) {
  cat(diff, "rows have been removed from the dataset due to NA-values.\n\n",
      file=stderr())
  data <- reduced_data
} else 
  cat("\n", file=stderr())
simUtils::print_data_info_to_stderr(data, simUtils::CONFIG)

# start simulations
if (opt$compute_alpha) {
  cat("\ncomputing alpha error...\n", file=stderr())
  l <- simUtils::compute_alpha_error(data, opt, simUtils::CONFIG)
  cat("period 1: error= ", l$period_1$error,
      " (#NA= ", l$period_1$na_count, ")\n",
      "period 2: error= ", l$period_2$error,
      " (#NA= ", l$period_2$na_count, ")\n\n",
      sep="",
      file=stderr())
}

cat("\ncomputing power...\n", file=stderr())
parameters <- simUtils::CONFIG$parameters[[opt$effect]]
for (p in parameters) {
  l <- simUtils::compute_power(data, p, opt, simUtils::CONFIG)
  key <- paste(names(p), p, sep="=", collapse=", ")
  j_list <- list()
  j_list[[key]] <- l
  j <- jsonlite::toJSON(
    j_list,
    pretty=T,
    auto_unbox=T
  )
  cat(j, "\n")
}
