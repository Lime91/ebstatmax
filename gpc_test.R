# load simulation utilities
devtools::load_all("simUtils")

# global configuration
options <- list(target="Pain")
config <- list()
best <- "lower"
side <- 2  # two sided test
repeated <- 7  # t7

# load data
blocklength <- 4
full_data <- simUtils::read_data("data/Diacerein_study-setup.txt", CONFIG)
unmatched_data <- simUtils::exclude_na_blocks(full_data, options$target, blocklength)
double_arm_ids <- unmatched_data %>% count(Id) %>% filter(n == 2*blocklength) %>% pull(Id)
matched_data <- unmatched_data %>% filter(Id %in% double_arm_ids)



################################################################################
### univariate 
################################################################################

type <- "univariate"

matching <- "unmatched"
gpc(unmatched_data, type, repeated, matching, side, best, options, config)

matching <- "matched"
gpc(matched_data, type, repeated, matching, side, best, options, config)


################################################################################
### prioritized
################################################################################

type <- "prioritized"

matching <- "unmatched"
gpc(unmatched_data, type, repeated, matching, side, best, options, config)

matching <- "matched"
gpc(matched_data, type, repeated, matching, side, best, options, config)  # differs from paper result


################################################################################
### non-prioritized
################################################################################

type <- "non-prioritized"

matching <- "unmatched"
gpc(unmatched_data, type, repeated, matching, side, best, options, config)  # differs from paper result

