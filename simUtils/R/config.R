# compilation of a global CONFIG object that is used in diacerin_sim_cli.R

# general simulation parameters
SEED <- 1
REPETITIONS <- 5000
ALPHA <- 0.05
BLOCKLENGTH <- 4
BINARY_THRESHOLD <- 0.6

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
MAX_VALUES <- c(
  "Pruritus"=10,
  "Pain"=10
)

# effect localization
MAIN_EFFECT_TIME <- c(4, 12)
DEPENDENT_EFFECT_TIME <- c(7, 15)
BASELINE_TIME <- c(0, 8)
PLACEBO_GROUP <- "P"
VERUM_GROUP <- "V"

# variable names in dataset
TIME_VARIABLE <- "Time"
GROUP_VARIABLE <- "Group"
SUBJECT_VARIABLE <- "Id"

# period separation time
FIRST_PERIOD_END <- 7

# prioritized GPC
TIME_MAPPING <- list(
  "0"  = 0,
  "2"  = 2,
  "4"  = 4,
  "7"  = 7,
  "8"  = 0,
  "10" = 2,
  "12" = 4,
  "15" = 7
)
REPEATED_PRIORITY <- c(
  4, 7, 2, 0
)

# statistical testing procedures
NPARLD <- list(
  "name"="nparld",
  "arguments"=list()
)
GPC_ARGUMENTS <- list(
  best="lower",
  repeated=REPEATED_PRIORITY
)
UNIVARIATE_MATCHED_GPC <- list(
  "name"="gpc",
  "arguments"=c(
    GPC_ARGUMENTS,
    list(
      "type"="univariate",
      "matching"="matched"
    )
  )
)
UNIVARIATE_UNMATCHED_GPC <- list(
  "name"="gpc",
  "arguments"=c(
    GPC_ARGUMENTS,
    list(
      "type"="univariate",
      "matching"="unmatched"
    )
  )
)
PRIORITIZED_MATCHED_GPC <- list(
  "name"="gpc",
  "arguments"=c(
    GPC_ARGUMENTS,
    list(
      "type"="prioritized",
      "matching"="matched"
    )
  )
)
PRIORITIZED_UNMATCHED_GPC <- list(
  "name"="gpc",
  "arguments"=c(
    GPC_ARGUMENTS,
    list(
      "type"="prioritized",
      "matching"="unmatched"
    )
  )
)
NON_PRIORITIZED_UNMATCHED_GPC <- list(
  "name"="gpc",
  "arguments"=c(
    GPC_ARGUMENTS,
    list(
      "type"="non-prioritized",
      "matching"="unmatched"
    )
  )
)
FUNCTIONS <- list(
  "nparld"=NPARLD,
  "univariate-matched-gpc"=UNIVARIATE_MATCHED_GPC,
  "univariate-unmatched-gpc"=UNIVARIATE_UNMATCHED_GPC,
  "prioritized-matched-gpc"=PRIORITIZED_MATCHED_GPC,
  "prioritized-unmatched-gpc"=PRIORITIZED_UNMATCHED_GPC,
  "non-prioritized-unmatched-gpc"=NON_PRIORITIZED_UNMATCHED_GPC
)

# valid user input
VALID_SCENARIOS <- c(1, 2)
VALID_EFFECTS <- names(PARAMETERS)
VALID_METHODS <- names(FUNCTIONS)


# combine parameters in global config object
CONFIG <- list(
  valid_scenarios       = VALID_SCENARIOS,
  valid_effects         = VALID_EFFECTS,
  valid_methods         = VALID_METHODS,
  parameters            = PARAMETERS,
  time_variable         = TIME_VARIABLE,
  group_variable        = GROUP_VARIABLE,
  subject_variable      = SUBJECT_VARIABLE,
  main_effect_time      = MAIN_EFFECT_TIME,
  dependent_effect_time = DEPENDENT_EFFECT_TIME,
  baseline_time         = BASELINE_TIME,
  placebo_group         = PLACEBO_GROUP,
  verum_group           = VERUM_GROUP,
  first_period_end      = FIRST_PERIOD_END,
  max_values            = MAX_VALUES,
  blocklength           = BLOCKLENGTH,
  binary_threshold      = BINARY_THRESHOLD,
  alpha                 = ALPHA,
  repetitions           = REPETITIONS,
  seed                  = SEED,
  functions             = FUNCTIONS,
  time_mapping          = TIME_MAPPING,
  repeated_priority     = REPEATED_PRIORITY
)
