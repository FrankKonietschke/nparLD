## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(nparLD)

## ----eval=FALSE---------------------------------------------------------------
# 
# nparLD(response ~ factor1 * factor2,
#        data = dat,
#        subject = "subject")
# 

## ----eval = FALSE-------------------------------------------------------------
# hypothesis = "H0p"

## ----eval = FALSE-------------------------------------------------------------
# hypothesis = "H0F"

## ----dental-example-----------------------------------------------------------
data(dental)

fit_dental <- nparLD(
  resp ~ time,
  data = dental,
  subject = "subject",
  hypothesis = "H0p",
  covariance = TRUE
)

fit_dental

## -----------------------------------------------------------------------------
WTS(fit_dental)
ATS(fit_dental)

## ----shoulder-example, fig.width = 7, fig.height = 5--------------------------
data(shoulder)
set.seed(12345)
fit_shoulder <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0p",
  contrast = list("group1:time")
)

fit_shoulder
plot(fit_shoulder)
plot(MCTP(fit_shoulder))

## ----missing-example----------------------------------------------------------
set.seed(123)

dat_miss <- dental
dat_miss$resp[c(2, 7, 12)] <- NA

fit_miss <- nparLD(
  resp ~ time,
  data = dat_miss,
  subject = "subject",
  hypothesis = "H0p"
)

fit_miss

## ----eval = FALSE-------------------------------------------------------------
# nparLD(response ~ group * time,
#        data = dat,
#        subject = "subject",
#        replicate = "replicate")

## ----eval = FALSE-------------------------------------------------------------
# cell.weights = "subjects"

## ----eval = FALSE-------------------------------------------------------------
# cell.weights = "observations"

## ----replicate-example--------------------------------------------------------

data(brdu)

fit_brdu_subjects <- nparLD(
  resp ~ dose,
  data = brdu,
  subject = "culture",
  replicate = "replicate",
  hypothesis = "H0p",
  cell.weights = "subjects"
)


fit_brdu_observations <- nparLD(
  resp ~ dose,
  data = brdu,
  subject = "culture",
  replicate = "replicate",
  hypothesis = "H0p",
  cell.weights = "observations"
)

fit_brdu_subjects
fit_brdu_observations

effects(fit_brdu_subjects)
effects(fit_brdu_observations)

## ----plot-cell-effects, fig.width=7, fig.height=4-----------------------------
plot(fit_shoulder)

## ----plot-term effects, fig.width=7, fig.height=4-----------------------------
fit_shoulder_info <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0p",
  Factor.Information = TRUE
)

plot(fit_shoulder_info, term = "group1:time")

## ----plot-cell-effectsseveral, fig.width=7, fig.height=4----------------------
plot(fit_shoulder_info, term = c("time", "group1:time"))

## ----eval = FALSE-------------------------------------------------------------
# contrast = list("time", "Dunnett")

## ----eval = FALSE-------------------------------------------------------------
# contrast = list("group:time")

## ----eval = FALSE-------------------------------------------------------------
# plot(MCTP(fit))

## ----shoulder-data------------------------------------------------------------
data(shoulder)
str(shoulder)

## ----shoulder-h0f-------------------------------------------------------------
fit_shoulder_F <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0F"
)

fit_shoulder_F

## ----shoulder-h0p-------------------------------------------------------------
fit_shoulder_p <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0p"
)

fit_shoulder_p

## ----shoulder-plot, fig.width = 7, fig.height = 5-----------------------------
plot(fit_shoulder_p)

## ----shoulder-mctp------------------------------------------------------------
set.seed(12345)
fit_shoulder_contrast <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0p",
  contrast = list("group1:time")
)

MCTP(fit_shoulder_contrast)

## ----shoulder-mctp-plot, fig.width = 7, fig.height = 5------------------------
plot(MCTP(fit_shoulder_contrast))

## ----summary_fit_shoulder_p---------------------------------------------------
summary(fit_shoulder_p)

## ----paired-permutation-------------------------------------------------------
data("panic", package = "nparLD")
panic.paired <- subset(panic, time <= 2)
set.seed(12415)
fit.panic <- nparLD(
  resp ~ time,
  data = panic.paired,
  subject = "subject",
  hypothesis = "H0p",
  perm.test = TRUE,
  B = 999
)

permTest(fit.panic)

## ----permutation-plot, fig.width = 7, fig.height = 5--------------------------
plot(permTest(fit.panic))

