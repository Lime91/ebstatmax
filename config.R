# compilation of a global CONFIG object that is used in diacerin_sim_cli.R

# general simulation parameters
SEED <- 1
REPETITIONS <- 5000
ALPHA <- 0.05
BLOCKLENGTH <- 4
BINARY_THRESHOLD <- 0.6

# valid user input
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
MAX_VALUES <- c(
  "Pruritus"=10,
  "Pain"=10
)

# effect localization
MAIN_EFFECT_TIME <- c(4, 12)
DEPENDENT_EFFECT_TIME <- c(7, 15)
EFFECT_GROUP <- "P"

# variable names in dataset
TIME_VARIABLE <- "Time"
GROUP_VARIABLE <- "Group"
SUBJECT_VARIABLE <- "Id"

# combine parameters in global config object
CONFIG <- list(
  valid_scenarios       = VALID_SCENARIOS,
  valid_effects         = VALID_EFFECTS,
  parameters            = PARAMETERS,
  time_variable         = TIME_VARIABLE,
  group_variable        = GROUP_VARIABLE,
  subject_variable      = SUBJECT_VARIABLE,
  main_effect_time      = MAIN_EFFECT_TIME,
  dependent_effect_time = DEPENDENT_EFFECT_TIME,
  effect_group          = EFFECT_GROUP,
  max_values            = MAX_VALUES,
  blocklength           = BLOCKLENGTH,
  binary_threshold      = BINARY_THRESHOLD,
  alpha                 = ALPHA,
  repetitions           = REPETITIONS,
  seed                  = SEED
)