test_that("summary works as expected for jointdata", {
    # Create a jointdata object
    heart.surv <- UniqueVariables(heart.valve,
        var.col = c("fuyrs", "status"),
        id.col = "num"
    )
    heart.long <- heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)]
    heart.jd <- jointdata(
        longitudinal = heart.long,
        survival = heart.surv,
        id.col = "num",
        time.col = "time"
    )
    result <- summary(heart.jd)
    expected_survival <- data.frame(
        c(54L, 202L),
        row.names = c(
            "Number of subjects that fail:",
            "Number of subjects censored:"
        )
    )
    names(expected_survival) <- NULL
    expected <- list(
        subjects = "Number of subjects: 256",
        longitudinal = data.frame(
            class = c(
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "integer"
            ),
            row.names = c(
                "time", "fuyrs", "grad",
                "log.grad", "lvmi", "log.lvmi", "ef"
            )
        ),
        survival = expected_survival,
        baseline = "No baseline covariates data available",
        times = "Unbalanced longitudinal study or more than twenty observation times"
    )
    expect_identical(result, expected)
})

test_that("summary works as expected with tibble", {
    # Create a jointdata object
    heart.surv <- UniqueVariables(heart.valve,
        var.col = c("fuyrs", "status"),
        id.col = "num"
    )
    heart.long <- tibble::as.tibble(heart.valve[, c(1, 4, 5, 7, 8, 9, 10, 11)])
    heart.jd <- jointdata(
        longitudinal = heart.long,
        survival = heart.surv,
        id.col = "num",
        time.col = "time"
    )
    result <- summary(heart.jd)
    expected_survival <- data.frame(
        c(54L, 202L),
        row.names = c(
            "Number of subjects that fail:",
            "Number of subjects censored:"
        )
    )
    names(expected_survival) <- NULL
    expected <- list(
        subjects = "Number of subjects: 256",
        longitudinal = data.frame(
            class = c(
                "numeric", "numeric", "numeric", "numeric", "numeric",
                "numeric", "integer"
            ),
            row.names = c(
                "time", "fuyrs", "grad",
                "log.grad", "lvmi", "log.lvmi", "ef"
            )
        ),
        survival = expected_survival,
        baseline = "No baseline covariates data available",
        times = "Unbalanced longitudinal study or more than twenty observation times"
    )
    expect_identical(result, expected)
})
