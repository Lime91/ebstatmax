# load simulation utilities
devtools::load_all("simUtils")

# global configuration
options <- list(target="Pain")
config <- list()

# load data
blocklength <- 4
data <- simUtils::read_data("data/Diacerein_study-setup.txt", CONFIG)
data <- simUtils::exclude_na_blocks(data, options$target, blocklength)
data <- simUtils::harmonize_period_times(data, CONFIG)

# input for gpc
best <- "lower"
side <- 2  # two sided test
repeated <- simUtils::CONFIG$repeated_priority


################################################################################
### univariate 
################################################################################

type <- "univariate"

matching <- "unmatched"  # expected: 0.9384
gpc(data, type, repeated, matching, side, best, options, config)

matching <- "matched"  # expected: 0.3173
gpc(data, type, repeated, matching, side, best, options, config)


################################################################################
### prioritized
################################################################################

type <- "prioritized"

matching <- "unmatched"  # expected: 0.4723
gpc(data, type, repeated, matching, side, best, options, config)

matching <- "matched"  # expected: 1.0000
gpc(data, type, repeated, matching, side, best, options, config)


################################################################################
### non-prioritized
################################################################################

type <- "non-prioritized"

matching <- "unmatched"  # expected: 0.7229
gpc(data, type, repeated, matching, side, best, options, config)

