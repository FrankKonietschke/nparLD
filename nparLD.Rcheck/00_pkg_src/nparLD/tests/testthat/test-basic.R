test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("nparLD runs for simple one-factor longitudinal design", {
  set.seed(123)
  dat <- data.frame(
    subject = rep(1:8, each = 3),
    time = rep(1:3, times = 8),
    y = rnorm(24)
  )

  fit <- nparLD(y ~ time, data = dat, subject = "subject", hypothesis = "H0p")

  expect_s3_class(fit, "nparld_fit")
  expect_true("effects" %in% names(fit))
  expect_true("WTS" %in% names(fit))
  expect_true("ATS" %in% names(fit))
})



test_that("nparLD recognizes replicate variable", {
  dat <- data.frame(
    CultureNo = rep(1:5, each = 12),
    Replic = rep(rep(1:3, times = 4), times = 5),
    Dose = rep(rep(c(0, 0.1, 1, 10), each = 3), times = 5),
    Brdu = rnorm(60)
  )

  fit <- nparLD(
    Brdu ~ Dose,
    data = dat,
    subject = "CultureNo",
    replicate = "Replic",
    hypothesis = "H0p"
  )

  expect_s3_class(fit, "nparld_fit")
})


test_that("nparLD recognizes replicate variable", {
  dat <- data.frame(
    CultureNo = rep(1:5, each = 12),
    Replic = rep(rep(1:3, times = 4), times = 5),
    Dose = rep(rep(c(0, 0.1, 1, 10), each = 3), times = 5),
    Brdu = rnorm(60)
  )

  fit <- nparLD(
    Brdu ~ Dose,
    data = dat,
    subject = "CultureNo",
    replicate = "Replic",
    hypothesis = "H0p"
  )

  expect_s3_class(fit, "nparld_fit")
})

test_that("interaction factor-information labels follow internal cell order", {

  data("shoulder", package = "nparLD")

  fit <- nparLD(
    resp ~ group1 * group2 * time,
    data = shoulder,
    subject = "subject",
    hypothesis = "H0p",
    Factor.Information = TRUE
  )

  fi <- factorInfo(fit)

  expect_equal(
    rownames(fi$`group1:time`),
    c(
      "N:1", "N:2", "N:3",
      "N:4", "N:5", "N:6",
      "Y:1", "Y:2", "Y:3",
      "Y:4", "Y:5", "Y:6"
    )
  )

  expect_equal(
    rownames(fi$`group1:group2:time`),
    c(
      paste0("N:F:", 1:6),
      paste0("N:M:", 1:6),
      paste0("Y:F:", 1:6),
      paste0("Y:M:", 1:6)
    )
  )
})