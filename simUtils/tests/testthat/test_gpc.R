# global config
options <- list(
    target="Pain",
    side=2  # two-sided test
)
config <- simUtils::CONFIG

# other input for gpc
best <- "lower"
repeated <- config$repeated_priority

# load and prepare study data
data("diacerein")  # provided in simUtils package
data <- diacerein
blocklength <- 4
data <- simUtils::exclude_na_blocks(data, options$target, blocklength)
data <- simUtils::harmonize_period_times(data, config)

################################################################################
### univariate
################################################################################

type <- "univariate"

matching <- "unmatched"
l <- simUtils::gpc(data, type, repeated, matching, best, options, config)
test_that(
    "univariate unmatched GPC produces correct p-value on study data",
    {
        expect_equal(
            l$combined,
            0.9384,
            tolerance=0.001
        )
    }
)

matching <- "matched"
l <- simUtils::gpc(data, type, repeated, matching, best, options, config)
test_that(
    "univariate matched GPC produces correct p-value on study data",
    {
        expect_equal(
            l$combined,
            0.3173,
            tolerance=0.001
        )
    }
)


################################################################################
### prioritized
################################################################################

type <- "prioritized"

matching <- "unmatched"
l <- simUtils::gpc(data, type, repeated, matching, best, options, config)
test_that(
    "prioritized unmatched GPC produces correct p-value on study data",
    {
        expect_equal(
            l$combined,
            0.521,
            tolerance=0.001
        )
    }
)

matching <- "matched"
l <- simUtils::gpc(data, type, repeated, matching, best, options, config)
test_that(
    "prioritized matched GPC produces correct p-value on study data",
    {
        expect_equal(
            l$combined,
            1.0000,
            tolerance=0.001
        )
    }
)


################################################################################
### non-prioritized
################################################################################

type <- "non-prioritized"

matching <- "unmatched"
l <- simUtils::gpc(data, type, repeated, matching, best, options, config)
test_that(
    "non-prioritized unmatched GPC produces correct p-value on study data",
    {
        expect_equal(
            l$combined,
            0.823,
            tolerance=0.001
        )
    }
)
