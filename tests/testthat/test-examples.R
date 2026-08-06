
test_that("nparLD runs for unbalanced group-by-time design with H0p and contrast", {
  set.seed(123)

  T <- 4
  n <- c(10, 20, 30)
  N <- sum(T * n)

  y <- rnorm(N)
  times <- c(rep(1:T, n[1]), rep(1:T, n[2]), rep(1:T, n[3]))
  grp <- c(rep(1, n[1] * T), rep(2, n[2] * T), rep(3, n[3] * T))
  subjekte <- c(
    sort(rep(1:n[1], T)),
    sort(rep((n[1] + 1):(n[1] + n[2]), T)),
    sort(rep((n[1] + n[2] + 1):(n[1] + n[2] + n[3]), T))
  )

  datensatz <- data.frame(times, grp, subjekte, y)

  fit <- nparLD(
    y ~ grp * times,
    data = datensatz,
    subject = "subjekte",
    hypothesis = "H0p",
    contrast = list("grp:times")
  )

  expect_s3_class(fit, "nparld_fit")
  expect_true("effects" %in% names(fit))
  expect_true("WTS" %in% names(fit))
  expect_true("ATS" %in% names(fit))
})


test_that("nparLD runs for unbalanced group-by-time design with missing values and H0F", {
  set.seed(123)

  T <- 4
  n <- c(10, 20, 30)
  N <- sum(T * n)

  y <- rnorm(N)
  times <- c(rep(1:T, n[1]), rep(1:T, n[2]), rep(1:T, n[3]))
  grp <- c(rep(1, n[1] * T), rep(2, n[2] * T), rep(3, n[3] * T))
  subjekte <- c(
    sort(rep(1:n[1], T)),
    sort(rep((n[1] + 1):(n[1] + n[2]), T)),
    sort(rep((n[1] + n[2] + 1):(n[1] + n[2] + n[3]), T))
  )

  y[c(1:2, 17, 28:30)] <- NA

  datensatz <- data.frame(times, grp, subjekte, y)

  fit <- nparLD(
    y ~ grp * times,
    data = datensatz,
    subject = "subjekte",
    hypothesis = "H0F",
    effect = "unweighted"
  )

  expect_s3_class(fit, "nparld_fit")
  expect_true("WTS" %in% names(fit))
  expect_true("ATS" %in% names(fit))
})


test_that("nparLD runs for balanced group-by-time design with replicates", {
  set.seed(123)

  T <- 4
  n <- c(10, 10, 10)
  N <- sum(T * n)

  y <- rnorm(N)
  times <- c(rep(1:T, n[1]), rep(1:T, n[2]), rep(1:T, n[3]))
  grp <- c(rep(1, n[1] * T), rep(2, n[2] * T), rep(3, n[3] * T))
  subjekte <- c(
    sort(rep(1:n[1], T)),
    sort(rep((n[1] + 1):(n[1] + n[2]), T)),
    sort(rep((n[1] + n[2] + 1):(n[1] + n[2] + n[3]), T))
  )

  datensatz <- data.frame(times, grp, subjekte, y)

  datensatz$rep <- rep(1:3, length.out = nrow(datensatz))

  fit1 <- nparLD(
    y ~ grp * times,
    data = datensatz,
    subject = "subjekte",
    replicate = "rep"
  )

  fit2 <- nparLD(
    y ~ grp * times,
    data = datensatz,
    subject = "subjekte",
    replicate = "rep",
    hypothesis = "H0p",
    contrast = list("grp:times")
  )

  expect_s3_class(fit1, "nparld_fit")
  expect_s3_class(fit2, "nparld_fit")
  expect_true("effects" %in% names(fit1))
  expect_true("effects" %in% names(fit2))
})


test_that("nparLD runs for balanced group-by-time design with missing values", {
  set.seed(123)

  T <- 4
  n <- c(10, 10, 10)
  N <- sum(T * n)

  y <- rnorm(N)
  times <- c(rep(1:T, n[1]), rep(1:T, n[2]), rep(1:T, n[3]))
  grp <- c(rep(1, n[1] * T), rep(2, n[2] * T), rep(3, n[3] * T))
  subjekte <- c(
    sort(rep(1:n[1], T)),
    sort(rep((n[1] + 1):(n[1] + n[2]), T)),
    sort(rep((n[1] + n[2] + 1):(n[1] + n[2] + n[3]), T))
  )

  datensatz <- data.frame(times, grp, subjekte, y)
  datensatz$y[1:3] <- NA

  fit <- nparLD(
    y ~ times * grp,
    data = datensatz,
    subject = "subjekte"
  )

  expect_s3_class(fit, "nparld_fit")
  expect_true("effects" %in% names(fit))
})


test_that("plot method returns a plot object for nparLD fit", {
  set.seed(123)

  T <- 4
  n <- c(10, 10, 10)
  N <- sum(T * n)

  y <- rnorm(N)
  times <- c(rep(1:T, n[1]), rep(1:T, n[2]), rep(1:T, n[3]))
  grp <- c(rep(1, n[1] * T), rep(2, n[2] * T), rep(3, n[3] * T))
  subjekte <- c(
    sort(rep(1:n[1], T)),
    sort(rep((n[1] + 1):(n[1] + n[2]), T)),
    sort(rep((n[1] + n[2] + 1):(n[1] + n[2] + n[3]), T))
  )

  datensatz <- data.frame(times, grp, subjekte, y)
  datensatz$y[1:3] <- NA

  fit <- nparLD(
    y ~ times * grp,
    data = datensatz,
    subject = "subjekte"
  )

  p <- plot(fit)

  expect_s3_class(fit, "nparld_fit")
  expect_s3_class(p, "ggplot")
})


test_that("nparLD runs for factorial design with two whole-plot and two subplot factors", {
  set.seed(123)

  dat <- data.frame()
  n_subjects <- 10
  subject_id <- 1

  for (a in c("A1", "A2")) {
    for (b in c("B1", "B2")) {
      for (subject in 1:n_subjects) {
        subject_effect <- rnorm(1, 0, 1.5)

        for (c in c("C1", "C2")) {
          for (d in c("D1", "D2")) {
            base_response <- 50
            a_effect <- ifelse(a == "A2", 5, 0)
            b_effect <- ifelse(b == "B2", 3, 0)
            c_effect <- ifelse(c == "C2", 4, 0)
            d_effect <- ifelse(d == "D2", 2, 0)

            response <- base_response +
              a_effect + b_effect + c_effect + d_effect +
              subject_effect + rnorm(1, 0, 0.5)

            dat <- rbind(
              dat,
              data.frame(
                ID = subject_id,
                WholePlot = paste0("WP_", a, "_", b),
                A = a,
                B = b,
                C = c,
                D = d,
                Response = round(response, 2)
              )
            )
          }
        }

        subject_id <- subject_id + 1
      }
    }
  }

  fit <- nparLD(
    Response ~ B * C * D,
    data = dat,
    subject = "ID"
  )

  expect_s3_class(fit, "nparld_fit")
  expect_true("effects" %in% names(fit))
  expect_true("WTS" %in% names(fit))
  expect_true("ATS" %in% names(fit))
})


test_that("nparLD runs for factorial design with missing values", {
  set.seed(123)

  dat <- data.frame()
  n_subjects <- 10
  subject_id <- 1

  for (a in c("A1", "A2")) {
    for (b in c("B1", "B2")) {
      for (subject in 1:n_subjects) {
        subject_effect <- rnorm(1, 0, 1.5)

        for (c in c("C1", "C2")) {
          for (d in c("D1", "D2")) {
            base_response <- 50
            a_effect <- ifelse(a == "A2", 5, 0)
            b_effect <- ifelse(b == "B2", 3, 0)
            c_effect <- ifelse(c == "C2", 4, 0)
            d_effect <- ifelse(d == "D2", 2, 0)

            response <- base_response +
              a_effect + b_effect + c_effect + d_effect +
              subject_effect + rnorm(1, 0, 0.5)

            dat <- rbind(
              dat,
              data.frame(
                ID = subject_id,
                WholePlot = paste0("WP_", a, "_", b),
                A = a,
                B = b,
                C = c,
                D = d,
                Response = round(response, 2)
              )
            )
          }
        }

        subject_id <- subject_id + 1
      }
    }
  }

  dat$Response[c(23, 51, 100)] <- NA

  fit <- nparLD(
    Response ~ B * C * D,
    data = dat,
    subject = "ID"
  )

  expect_s3_class(fit, "nparld_fit")
  expect_true("effects" %in% names(fit))
})



test_that("nparLD runs with true dependent replicate observations", {
  set.seed(123)

  T <- 4
  n <- c(10, 10, 10)
  N <- sum(T * n)

  y <- rnorm(N)
  times <- c(rep(1:T, n[1]), rep(1:T, n[2]), rep(1:T, n[3]))
  grp <- c(rep(1, n[1] * T), rep(2, n[2] * T), rep(3, n[3] * T))
  subjekte <- c(
    sort(rep(1:n[1], T)),
    sort(rep((n[1] + 1):(n[1] + n[2]), T)),
    sort(rep((n[1] + n[2] + 1):(n[1] + n[2] + n[3]), T))
  )

  datensatz <- data.frame(times, grp, subjekte, y)

  datensatz2 <- datensatz[rep(seq_len(nrow(datensatz)), each = 3), ]
  datensatz2$rep <- rep(1:3, times = nrow(datensatz))
  datensatz2$y <- rnorm(nrow(datensatz2))

  fit_subjects <- nparLD(
    y ~ grp * times,
    data = datensatz2,
    subject = "subjekte",
    replicate = "rep",
    hypothesis = "H0p",
    cell.weights = "subjects",
    contrast = list("grp:times")
  )

  fit_observations <- nparLD(
    y ~ grp * times,
    data = datensatz2,
    subject = "subjekte",
    replicate = "rep",
    hypothesis = "H0p",
    cell.weights = "observations",
    contrast = list("grp:times")
  )

  expect_s3_class(fit_subjects, "nparld_fit")
  expect_s3_class(fit_observations, "nparld_fit")

  expect_true("effects" %in% names(fit_subjects))
  expect_true("effects" %in% names(fit_observations))


  expect_equal(nrow(fit_subjects$effects), nrow(fit_observations$effects))
})

test_that("brdu replicate data can be analyzed", {
  fit <- nparLD(
    resp ~ dose,
    data = brdu,
    subject = "culture",
    replicate = "replicate",
    hypothesis = "H0p",
    cell.weights = "subjects"
  )

  expect_s3_class(fit, "nparld_fit")
  expect_true("effects" %in% names(fit))
  expect_true(nrow(fit$effects) > 0)
})

