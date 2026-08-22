pkgname <- "MANOVA.RM"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('MANOVA.RM')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("EEG")
### * EEG

flush(stderr()); flush(stdout())

### Name: EEG
### Title: EEG Measurements in Patients with Alzheimer's Disease (long
###   format)
### Aliases: EEG
### Keywords: datasets

### ** Examples

if(requireNamespace("ggplot2")){ 
  library(ggplot2)
  ggplot(EEG, aes(x=sex, y=resp)) + geom_point(alpha=0.5) + facet_grid(region+feature~diagnosis) +
     stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, colour = "red")
  }
  



cleanEx()
nameEx("MANOVA")
### * MANOVA

flush(stderr()); flush(stdout())

### Name: MANOVA
### Title: Tests for Multivariate Data in Semi-Parametric Factorial Designs
### Aliases: MANOVA

### ** Examples

data(EEG)
EEG_mod <- MANOVA(resp ~ sex * diagnosis, 
                    data = EEG, subject = "id", resampling = "paramBS", 
                    alpha = 0.05, iter = 10)
summary(EEG_mod)




cleanEx()
nameEx("MANOVA.wide")
### * MANOVA.wide

flush(stderr()); flush(stdout())

### Name: MANOVA.wide
### Title: Tests for Multivariate Data in Semi-Parametric Factorial Designs
### Aliases: MANOVA.wide

### ** Examples

#Example on producing plastic film from Krzanowski (1998, p. 381), see \code{manova.summary}
tear <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3,
          6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
gloss <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4,
           9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
opacity <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7,
             2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
rate     <- gl(2,10, labels = c("Low", "High"))
additive <- gl(2, 5, length = 20, labels = c("Low", "High"))
example <- data.frame(tear, gloss, opacity, rate, additive)
fit <- MANOVA.wide(cbind(tear, gloss, opacity) ~ rate * additive, 
data = example, iter = 1000)
summary(fit)




cleanEx()
nameEx("RM")
### * RM

flush(stderr()); flush(stdout())

### Name: RM
### Title: Tests for Repeated Measures in Semi-Parametric Factorial Designs
### Aliases: RM

### ** Examples

data(o2cons)
## Not run: 
##D oxy <- RM(O2 ~ Group * Staphylococci * Time, data = o2cons, 
##D             subject = "Subject", no.subf = 2, iter = 1000,
##D             resampling = "Perm")
##D summary(oxy)
##D plot(oxy, factor = "Group") 
##D  
##D # For more details including the output of the examples also refer to the 
##D # package vignette.
##D 
##D # using the EEG data, consider additional within-subjects factors 'brain region' 
##D # and 'feature'
##D data(EEG)
##D EEG_model <- RM(resp ~ sex * diagnosis * feature * region, 
##D                data = EEG, subject = "id", within = c("feature", "region"),
##D                resampling = "WildBS",
##D                iter = 1000,  alpha = 0.01, seed = 987, dec = 2)
##D summary(EEG_model)
## End(Not run)




cleanEx()
nameEx("conf.reg")
### * conf.reg

flush(stderr()); flush(stdout())

### Name: conf.reg
### Title: The conf.reg() function calculates confidence regions for
###   contrasts in multivariate factorial designs. In the two-dimensional
###   case, confidence ellipsoids can be plotted via the generic 'plot()'
###   function.
### Aliases: conf.reg

### ** Examples

data(EEG)
EEG_mod <- MANOVA(resp ~ sex * diagnosis, 
                    data = EEG, subject = "id", resampling = "paramBS", 
                    alpha = 0.05, iter = 10, CPU = 1)
conf.reg(EEG_mod, nullhypo = "sex")





cleanEx()
nameEx("multRM")
### * multRM

flush(stderr()); flush(stdout())

### Name: multRM
### Title: Tests for Repeated Measures in Multivariate Semi-Parametric
###   Factorial Designs
### Aliases: multRM

### ** Examples

## Not run: 
##D data(EEG)
##D library(tidyr)
##D eeg <- spread(EEG, feature, resp)
##D fit <- multRM(cbind(brainrate, complexity) ~ sex * region, data = eeg, 
##D               subject = "id", within = "region")
##D summary(fit)
## End(Not run)




cleanEx()
nameEx("o2cons")
### * o2cons

flush(stderr()); flush(stdout())

### Name: o2cons
### Title: Oxygen Consumption of Leukocytes
### Aliases: o2cons
### Keywords: datasets

### ** Examples

  if(requireNamespace("ggplot2")){ 
  library(ggplot2)
  ggplot(o2cons, aes(x=Group, y=O2)) + geom_point(alpha=0.5) + facet_grid(Staphylococci~Time) +
  stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, colour = "red") 
}




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
