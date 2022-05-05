library(dplyr)
devtools::load_all("simUtils")

# global configuration
target <- "Pain"
best <- "lower"
side <- 2  # two sided test
repeated <- 7  # t7

# load data
blocklength <- 4
full_data <- simUtils::read_data("data/Diacerein_study-setup.txt", CONFIG)
unmatched_data <- simUtils::exclude_na_blocks(full_data, target, blocklength)
double_arm_ids <- unmatched_data %>% count(Id) %>% filter(n == 2*blocklength) %>% pull(Id)
matched_data <- unmatched_data %>% filter(Id %in% double_arm_ids)



################################################################################
### univariate 
################################################################################

type <- "univariate"

matching <- "unmatched"
gpc(unmatched_data, target, type, repeated, matching, side, best)

matching <- "matched"
gpc(matched_data, target, type, repeated, matching, side, best)


################################################################################
### prioritized
################################################################################

type <- "prioritized"

matching <- "unmatched"
gpc(unmatched_data, target, type, repeated, matching, side, best)

matching <- "matched"
gpc(matched_data, target, type, repeated, matching, side, best)  # differs from paper result


################################################################################
### non-prioritized
################################################################################

type <- "non-prioritized"

matching <- "unmatched"
gpc(unmatched_data, target, type, repeated, matching, side, best)  # differs from paper result

