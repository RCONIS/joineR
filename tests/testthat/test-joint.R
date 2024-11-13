test_that("joint also works when there is no baseline data", {
    heart.grad <- heart.valve[!is.na(heart.valve$grad), ]
    heart.grad.long <- heart.grad[, c(1, 4, 7)]
    heart.grad.surv <- UniqueVariables(heart.grad,
        var.col = c("fuyrs", "status"),
        id.col = "num"
    )
    heart.grad.jd <- jointdata(
        longitudinal = heart.grad.long,
        survival = heart.grad.surv,
        id.col = "num",
        time.col = "time"
    )
    model.jointrandom.heart <- joint(heart.grad.jd,
        grad ~ 1,
        Surv(fuyrs, status) ~ 1,
        model = "int"
    )
    summary(model.jointrandom.heart)
})
