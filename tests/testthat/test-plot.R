test_that("factor-information plots can be requested by term", {
  fit <- nparLD(
    resp ~ group1 * group2 * time,
    data = shoulder,
    subject = "subject",
    hypothesis = "H0p",
    Factor.Information = TRUE
  )

  p1 <- plot(fit, term = "time")
  p2 <- plot(fit, term = c("time", "group1:time"))

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("factor-information plot requires Factor.Information", {
  fit <- nparLD(
    resp ~ group1 * group2 * time,
    data = shoulder,
    subject = "subject",
    hypothesis = "H0p"
  )

  expect_error(
    plot(fit, term = "time"),
    "Factor.Information = TRUE"
  )
})