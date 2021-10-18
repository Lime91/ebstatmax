#!/usr/bin/Rscript


# simulation of the Diacerin study for the EBStatMax project

# usage (on linux, ubuntu):   ./diacerin_sim_cli.R --help
# usage (general):            Rscript diacerin_sim_cli.R --help


suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(nparLD))


# command line option parsing
option_list <- list(
  make_option(c("-d", "--data"),
              action="store",
              default="./diacerin.b.p.p.rds",
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
  make_option(c("-e", "--effect"),
              action="store",
              default="pois",
              type="character",
              help=paste0("Type of the added effects. ",
                          "Either 'pois' or 'nbinom'. [default '%default'].")),
  make_option(c("-c", "--compute-alpha"),
              action="store_true",
              default=FALSE,
              help="Set flag to compute alpha error in addition to power.")
)
opt <- parse_args(OptionParser(option_list=option_list))


# valid input
VALID_SCENARIOS <- c(1, 2)
VALID_EFFECTS <- c("pois", "nbinom")

# effect parameters
POIS_PARAMETERS <- list(
  c("lambda"=2),
  c("lambda"=3),
  c("lambda"=4)
)
NBINOM_PARAMETERS <- list(
  c("r"=3, "p"=0.5),
  c("r"=1/3, "p"=0.1),
  c("r"=27, "p"=0.9),
  c("r"=4, "p"=0.5),
  c("r"=4/9, "p"=0.1),
  c("r"=36, "p"=0.9),
  c("r"=2, "p"=0.5),
  c("r"=2/9, "p"=0.1),
  c("r"=18, "p"=0.9)
)

# other simulation parameters
RANDOM_SEED <- 1
REPETITIONS <- 5000
ALPHA_LEVEL <- 0.05
BLOCKLENGTH <- 4


# sanity check
if (!(opt$s %in% VALID_SCENARIOS)) {
  cat(paste0("Error! Scenario must be in (",
             paste(VALID_SCENARIOS, collapse=", "),
             ").\n")
  )
  quit(status=-1)
}
if (!(opt$e %in% VALID_EFFECTS)) {
  cat(paste0("Error! Effect must be in ('",
             paste(VALID_EFFECTS, collapse="', '"),
             "').\n")
  )
  quit(status=-1)
}

# status message
cat("\n")
cat("Starting simulations with the following parameters:\n")
cat("-) data:", opt$d, "\n")
cat("-) scenario:", opt$s, "\n")
cat("-) effect:", opt$e, "\n")
cat("-) #repetitions:", REPETITIONS, "\n")
cat("-) alpha-level:", ALPHA_LEVEL, "\n")
cat("\n")


# functions to deploy
permute <- function(data,
                    target_col="Blister_count",
                    new_colname="PermuteBlister_count") {
  n <- length(data$Id)
  blocks <- n/BLOCKLENGTH
  matr_perm = matrix(data[[target_col]], nrow=BLOCKLENGTH, ncol=blocks)
  matr_perm = matr_perm[,sample(1:blocks)]
  data[[new_colname]]= c(matr_perm)
  return(data)
}

add_effect <- function(data,
                       scenario,
                       effect_type,
                       params,
                       target_col="PermuteBlister_count",
                       new_colname="SampleBlister_count") {
  # select data
  w3 <- which(data$Time=="t4" & data$Group=="P")
  w4 <- which(data$Time=="t12" & data$Group=="P")
  nw3 <- length(w3)
  nw4 <- length(w4)
  
  # generate effects
  if (effect_type == "nbinom") {
    effect3 <- rnbinom(nw3, size=params["r"], prob=params["p"])
    effect4 <- rnbinom(nw4, size=params["r"], prob=params["p"])
  } else {
    effect3 <- rpois(nw3, lambda=params["lambda"])
    effect4 <- rpois(nw4, lambda=params["lambda"])
  }
  
  # add effects
  target <- data[[target_col]]
  target[w3] = target[w3] + effect3
  target[w4] = target[w4] + effect4
  
  # add dependent effects
  if (scenario == 2) {
    effect5 <- floor(effect3/2)
    effect6 <- floor(effect4/2)
    w5 <- which(data$Time=="t7" & data$Group=="P")
    w6 <- which(data$Time=="t15" & data$Group=="P")
    target[w5] = target[w5] + effect5
    target[w6] = target[w6] + effect6
  }
  
  data[[new_colname]] <- target
  return(data)
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
                                target="PermuteBlister_count") {
  results1 <- rep(-1, REPETITIONS) 
  results2 <- rep(-1, REPETITIONS)
  for (i in 1:REPETITIONS) {
    permuted_data <- permute(data)
    results1[i] <- test_h0(permuted_data, 1, target)
    results2[i] <- test_h0(permuted_data, 2, target)
  }
  return(c(period1=mean(results1), period2=mean(results2)))
}

compute_power <- function(data,
                          scenario,
                          effect_type,
                          params,
                          target="PermuteBlister_count") {
  results1 <- rep(-1, REPETITIONS) 
  results2 <- rep(-1, REPETITIONS)
  for (i in 1:REPETITIONS) {
    permuted_data <- permute(data)
    shifted_data <- add_effect(
      permuted_data,
      scenario,
      effect_type,
      params,
      target,
      new_colname="SampleBlister_count")
    results1[i] <- test_h0(shifted_data, 1, "SampleBlister_count")
    results2[i] <- test_h0(shifted_data, 2, "SampleBlister_count")
  }
  return(c(period1=mean(results1), period2=mean(results2)))
}


# determine parameters to iterate over
if (opt$e == 'pois') {
  parameters <- POIS_PARAMETERS
} else {
  parameters <- NBINOM_PARAMETERS
}


# program start
original_data <- readRDS(opt$data)
set.seed(RANDOM_SEED)

if (opt$c) {
  cat("computing alpha error...\n")
  alpha_errors <- compute_alpha_error(original_data)
  cat("P1: ", alpha_errors[1], ", P2: ", alpha_errors[2], "\n\n", sep="")
}
     
cat("computing power...\n")
power <- list()
for (p in parameters) {
  results <- compute_power(
    original_data,
    opt$s,
    opt$e,
    p)
  key <- paste(names(p), p, sep="=", collapse=", ")
  power[[key]] = results
  cat(key, "\n")
  cat("P1: ", results[1], ", P2: ", results[2], "\n\n", sep="")
}

#EOF