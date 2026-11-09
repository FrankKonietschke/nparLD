pkgname <- "nparLD"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "nparLD-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('nparLD')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("amylase")
### * amylase

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: amylase
### Title: Alpha-amylase saliva study
### Aliases: amylase
### Keywords: datasets

### ** Examples

data(amylase)

fit <- nparLD(resp ~ time1 * time2,
              data = amylase,
              subject = "subject",
              effect = "weighted",
              hypothesis = "H0F")
fit

# Dunnett-type contrasts for the levels of time2
fit <- nparLD(resp ~ time1 * time2,
              data = amylase,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("time2", "Dunnett"),
              Factor.Information = TRUE)
fit
plot(fit)
plot(MCTP(fit))




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("amylase", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("brdu")
### * brdu

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: brdu
### Title: BrdU incorporation in fibroblasts
### Aliases: brdu
### Keywords: datasets

### ** Examples

## No test: 
data(brdu)

fit <- nparLD(
  resp ~ dose,
  data = brdu,
  subject = "culture",
  replicate = "replicate",
  hypothesis = "H0p",
  cell.weights = "subjects"
)

fit
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("brdu", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dental")
### * dental

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dental
### Title: Dental growth study
### Aliases: dental
### Keywords: datasets

### ** Examples

data(dental)

dental$time <- factor(dental$time)

## No test: 
fit <- nparLD(resp ~ time,
              data = dental,
              subject = "subject",
              hypothesis = "H0F")
fit
## End(No test)

## No test: 
fit <- nparLD(resp ~ time,
              data = dental,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("time", "Tukey"))
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dental", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("edema")
### * edema

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: edema
### Title: Postoperative edema study
### Aliases: edema
### Keywords: datasets

### ** Examples

data(edema)

## No test: 
fit <- nparLD(resp ~ group * time1 * time2,
              data = edema,
              subject = "subject",
              hypothesis = "H0F")
fit
## End(No test)

## No test: 
fit <- nparLD(resp ~ group * time1 * time2,
              data = edema,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("group", "Tukey"),
              Factor.Information = TRUE)
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("edema", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nparLD")
### * nparLD

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nparLD
### Title: Nonparametric analysis of longitudinal data
### Aliases: nparLD

### ** Examples

## One repeated-measures factor: dental data
data(dental)

fit_dental <- nparLD(
  resp ~ time,
  data = dental,
  subject = "subject",
  hypothesis = "H0p"
)

fit_dental
effects(fit_dental)

## Crossed factorial longitudinal design: shoulder data
data(shoulder)

## No test: 
fit_shoulder <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",Factor.Information=TRUE,
  hypothesis = "H0p"
)

fit_shoulder
effects(fit_shoulder)
factorInfo(fit_shoulder)
## End(No test)

## Multiple contrast procedure for an interaction

## No test: 
fit_contrast <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0p",
  contrast = list("group1:time")
)

MCTP(fit_contrast)
print(MCTP(fit_contrast), show.matrix = TRUE)
## End(No test)

## Missing response values
## No test: 
dat_miss <- dental
dat_miss$resp[c(2, 7, 12)] <- NA

fit_miss <- nparLD(
  resp ~ time,
  data = dat_miss,
  subject = "subject",
  hypothesis = "H0p"
)

fit_miss
## End(No test)

## Dependent replicate measurements: BrdU data
data(brdu)

## No test: 
fit_brdu <- nparLD(
  resp ~ dose,
  data = brdu,
  subject = "culture",
  replicate = "replicate",
  hypothesis = "H0p",
  cell.weights = "subjects"
)

fit_brdu
effects(fit_brdu)
## End(No test)

## No test: 
## Graphical displays
plot(fit_dental)
plot(fit_shoulder)
plot(MCTP(fit_contrast))
## End(No test)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nparLD", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nparLDApp")
### * nparLDApp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nparLDApp
### Title: Launch the nparLD Shiny application
### Aliases: nparLDApp

### ** Examples

## Not run: 
##D nparLDApp()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nparLDApp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("panic")
### * panic

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: panic
### Title: Panic disorder study I
### Aliases: panic
### Keywords: datasets

### ** Examples

data(panic)

## No test: 
fit <- nparLD(resp ~ time,
              data = panic,
              subject = "subject",
              hypothesis = "H0F", contrast =list("time", "Dunnett"))
fit
## End(No test)

## No test: 
fit <- nparLD(resp ~ time,
              data = panic,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("time", "Dunnett"))
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("panic", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("panic2")
### * panic2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: panic2
### Title: Panic disorder study II
### Aliases: panic2
### Keywords: datasets

### ** Examples

data(panic2)

## No test: 
fit <- nparLD(resp ~ group * time,
              data = panic2,
              subject = "subject",
              hypothesis = "H0F", effect="unweighted")
fit
## End(No test)

## No test: 
fit <- nparLD(resp ~ group * time,
              data = panic2,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("group:time"),
              Factor.Information = TRUE,
              covariance=TRUE)
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("panic2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plasma")
### * plasma

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plasma
### Title: Plasma-renin activity study
### Aliases: plasma
### Keywords: datasets

### ** Examples

data(plasma)

## No test: 
fit <- nparLD(resp ~ group * time,
              data = plasma,
              subject = "subject",
              hypothesis = "H0F", effect="weighted")
fit
## End(No test)

## No test: 
fit <- nparLD(resp ~ group * time,
              data = plasma,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("group", "Tukey"))
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plasma", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.nparld_fit")
### * plot.nparld_fit

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.nparld_fit
### Title: Plot nparLD results
### Aliases: plot.nparld_fit

### ** Examples

## No test: 
data(shoulder)

fit <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0p",
  Factor.Information = TRUE
)

## Cell-level relative effects
plot(fit)

## Factor-specific relative effects and confidence intervals
plot(fit, term = "time")
plot(fit, term = "group1:time")
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.nparld_fit", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.nparld_mctp")
### * plot.nparld_mctp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.nparld_mctp
### Title: Plot nparLD multiple contrast results
### Aliases: plot.nparld_mctp

### ** Examples

## No test: 
data(shoulder)

fit <- nparLD(
  resp ~ group1 * group2 * time,
  data = shoulder,
  subject = "subject",
  hypothesis = "H0p",
  contrast = list("time", "Dunnett")
)

plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.nparld_mctp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("rat")
### * rat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: rat
### Title: Rat growth study
### Aliases: rat
### Keywords: datasets

### ** Examples


## No test: 
data(rat)

fit <- nparLD(resp ~ group * time,
              data = rat,
              subject = "subject",
              hypothesis = "H0F")
fit
## End(No test)

## No test: 
fit <- nparLD(resp ~ group * time,
              data = rat,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("group:time"))
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("rat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("respiration")
### * respiration

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: respiration
### Title: Respiratory disorder study
### Aliases: respiration
### Keywords: datasets

### ** Examples


## No test: 
data(respiration)

fit <- nparLD(resp ~ center * treatment * time,
              data = respiration,
              subject = "patient",
              hypothesis = "H0F")
fit
## End(No test)

## No test: 
fit <- nparLD(resp ~ center * treatment * time,
              data = respiration,
              subject = "patient",
              hypothesis = "H0p",
              contrast = list("treatment", "Tukey"))
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("respiration", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("shoulder")
### * shoulder

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: shoulder
### Title: Shoulder tip pain study
### Aliases: shoulder
### Keywords: datasets

### ** Examples

data(shoulder)

## No test: 
fit <- nparLD(resp ~ group1 * group2 * time,
              data = shoulder,
              subject = "subject",
              hypothesis = "H0F")
fit
## End(No test)

## No test: 
fit <- nparLD(resp ~ group1 * group2 * time,
              data = shoulder,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("group1:time"),
              Factor.Information=TRUE)
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("shoulder", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tree")
### * tree

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tree
### Title: Vitality of treetops
### Aliases: tree
### Keywords: datasets

### ** Examples

data(tree)

## No test: 
fit <- nparLD(resp ~ group * time,
              data = tree,
              subject = "subject",
              hypothesis = "H0F")
fit
## End(No test)


## No test: 
fit <- nparLD(resp ~ group * time,
              data = tree,
              subject = "subject",
              hypothesis = "H0p",
              contrast = list("group:time"))
fit
plot(fit)
plot(MCTP(fit))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tree", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
