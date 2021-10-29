#!/usr/bin/Rscript


# simulation of the Diacerin study for the EBStatMax project

# usage (on linux, ubuntu):   ./diacerin_sim_cli.R --help
# usage (general):            Rscript diacerin_sim_cli.R --help


suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(nparLD))


# command line option parsing
option_list <- list(
  make_option(c("-d", "--data"),
              action="store",
              default="./data/diacerin.b.p.p.rds",
              type="character",
              help=paste0("Path to the preprocessed RData file. ",
                          "[default %default].")),
  make_option(c("-s", "--scenario"),
              action="store",
              default=1,
              type="integer",
              help=paste0("The required simulation scenario. ",
                          "#1: add effects at post-treatment only. ",
                          "#2: effects at post-treatment and (less markedly) ",
                          "at follow-up. [default %default].")),
  make_option(c("-t", "--target-variable"),
              action="store",
              default="Blister_count",
              help=paste0("Name of the explained variable ",
                          "(to which effects are applied). ",
                          "[default %default]")),
  make_option(c("-e", "--effect"),
              action="store",
              default="pois",
              type="character",
              help=paste0("Type of the added effects. One of ",
                          "('pois', 'nbinom', 'lnorm', 'norm'). ",
                          "[default '%default'].")),
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


# valid input
VALID_SCENARIOS <- c(1, 2)
VALID_EFFECTS <- c("pois", "nbinom", "lnorm", "norm")

# effect parameters
POIS_PARAMETERS <- list(
  c("lambda"=2),
  c("lambda"=3),
  c("lambda"=4)
)
NBINOM_PARAMETERS <- list(
  c("r"=2, "p"=0.5),
  c("r"=2/9, "p"=0.1),
  c("r"=18, "p"=0.9),
  c("r"=3, "p"=0.5),
  c("r"=1/3, "p"=0.1),
  c("r"=27, "p"=0.9),
  c("r"=4, "p"=0.5),
  c("r"=4/9, "p"=0.1),
  c("r"=36, "p"=0.9)
)
LNORM_PARAMETERS <- list(
  c("meanlog"=0.2, "sdlog"=1),
  c("meanlog"=0.6, "sdlog"=1),
  c("meanlog"=0.9, "sdlog"=1)
)
NORM_PARAMETERS <- list(
  c("mean"=2, "sd"=1),
  c("mean"=3, "sd"=1),
  c("mean"=4, "sd"=1)
)
PARAMETERS <- list(
  "pois"=POIS_PARAMETERS,
  "nbinom"=NBINOM_PARAMETERS,
  "lnorm"=LNORM_PARAMETERS,
  "norm"=NORM_PARAMETERS
)

# maximum value per target (target will be truncated after the effect was added)
MAX_VALUE <- c(
  "Pruritus"=10,
  "Pain"=10
)

# other simulation parameters
RANDOM_SEED <- 1
REPETITIONS <- 50
ALPHA_LEVEL <- 0.05
BLOCKLENGTH <- 4
BINARY_THRESHOLD <- 0.6


# sanity check
if (!(opt$scenario %in% VALID_SCENARIOS))
  stop("Scenario must be in (", paste(VALID_SCENARIOS, collapse=", "), ")")

if (!(opt$effect %in% VALID_EFFECTS))
  stop("Effect must be in ('", paste(VALID_EFFECTS, collapse="', '"), "')")


# status message
cat("\n")
cat("Starting simulations with the following parameters:\n")
cat("-) data:", opt$data, "\n")
cat("-) scenario:", opt$scenario, "\n")
cat("-) target variable:", opt$target_variable, "\n")
cat("-) effect:", opt$effect, "\n")
cat("-) binarization:", opt$binarize, ",threshold =", BINARY_THRESHOLD, "\n")
cat("-) #repetitions:", REPETITIONS, "\n")
cat("-) alpha-level:", ALPHA_LEVEL, "\n")
cat("\n")


# functions to deploy
permute <- function(data,
                    target) {
  n <- length(data$Id)
  blocks <- n/BLOCKLENGTH
  matr_perm = matrix(data[[target]], nrow=BLOCKLENGTH, ncol=blocks)
  matr_perm = matr_perm[, sample(1:blocks)]
  data[, c(target) := c(matr_perm)]
}

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
  return(effect)
}

add_main_effect <- function(data,
                            effect_type,
                            params,
                            target) {
  w <- which((data$Time == "t4" | data$Time == "t12") & data$Group == "P")
  n <- length(w)
  effect <- generate_effect(effect_type, n, params)
  shifted_values <- data[[target]][w] + effect
  data[w, c(target) := shifted_values]
  return(effect)
}

add_dependent_effect <- function(data,
                                 effect,
                                 scenario,
                                 target) {
  if (scenario == 2) {
    dependent_effect <- floor(effect/2)
    w <- which((data$Time == "t7" | data$Time== "t15") & data$Group == "P")
    shifted_values <- data[[target]][w] + dependent_effect
    data[w, c(target) := shifted_values]
  }
}

truncate_target <- function(data,
                            target) {
  if (target %in% names(MAX_VALUE)) {
    m <- MAX_VALUE[target] 
    v <- data[[target]]
    data[, c(target) := ifelse(v <= m, v, m)]
  }
}

dichotomize_target <- function(data,
                               target,
                               dichotomize,
                               blocklength,
                               threshold) {
  if (dichotomize) {
    n <- length(data$Id)
    block_begins <- which(1:n %% blocklength == 1)
    for (b in block_begins) {  # 1, blocklength + 1, 2*blocklength + 1, ...
      range <- b:(b + blocklength - 1)
      block <- data[range, ..target][[target]]
      relative <- block/block[1]
      dichotomized <- ifelse(relative < threshold, 1, 0)  # decrease is desired
      data[range, c(target) := dichotomized]
    }
  }
}

add_effect <- function(data,
                       options,
                       params) {
  effect_vals <- add_main_effect(data, options$effect, params, options$target)
  add_dependent_effect(data, effect_vals, options$scenario, options$target)
  truncate_target(data, options$target)
  dichotomize_target(data, options$target, options$binarize,
                     BLOCKLENGTH, BINARY_THRESHOLD)
}

test_h0 <- function(data,
                    period,
                    target) {
  if (period == 1) {
    data <- subset(data, TimeNum <= 4)
  } else {
    data <- subset(data, TimeNum > 4)
  }
  form <- as.formula(paste(target, "GroupNum * TimeNum", sep=" ~ "))
  capture.output(
    p_value <- nparLD(form, data, subject="Id")$ANOVA.test[3,3]
  )
  return(p_value < ALPHA_LEVEL)
}

compute_alpha_error <- function(data,
                                options) {
  target <- options$target
  results1 <- rep(-1, REPETITIONS) 
  results2 <- rep(-1, REPETITIONS)
  for (i in 1:REPETITIONS) {
    original <- copy(data[, ..target])  # save from passing by reference
    permute(data, target)
    results1[i] <- test_h0(data, 1, target)
    results2[i] <- test_h0(data, 2, target)
    data[, c(target) := original[[target]]]  # restore original
  }
  return(c(period1=mean(results1), period2=mean(results2)))
}

compute_power <- function(data,
                          options,
                          params) {
  target <- options$target
  results1 <- rep(-1, REPETITIONS) 
  results2 <- rep(-1, REPETITIONS)
  for (i in 1:REPETITIONS) {
    original <- copy(data[, ..target])  # save from passing by reference
    permute(data, target)
    add_effect(data, options, params)
    results1[i] <- test_h0(data, 1, target)
    results2[i] <- test_h0(data, 2, target)
    data[, c(target) := original[[target]]]  # restore original
  }
  return(c(period1=mean(results1), period2=mean(results2)))
}


# determine parameters to iterate over
parameters <- PARAMETERS[[opt$effect]]

# program start
data <- readRDS(opt$data)
set.seed(RANDOM_SEED)

if (opt$compute_alpha) {
  cat("computing alpha error...\n")
  alpha_errors <- compute_alpha_error(data, opt)
  cat("P1: ", alpha_errors[1], ", P2: ", alpha_errors[2], "\n\n", sep="")
}
     
cat("computing power...\n")
power <- list()
for (p in parameters) {
  results <- compute_power(data, opt, p)
  key <- paste(names(p), p, sep="=", collapse=", ")
  power[[key]] = results
  cat(key, "\n")
  cat("P1: ", results[1], ", P2: ", results[2], "\n\n", sep="")
}

#EOF