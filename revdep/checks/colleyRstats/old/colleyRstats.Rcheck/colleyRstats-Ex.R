pkgname <- "colleyRstats"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('colleyRstats')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_pareto_emoa_column")
### * add_pareto_emoa_column

flush(stderr()); flush(stdout())

### Name: add_pareto_emoa_column
### Title: Add 'PARETO_EMOA' Column to a Data Frame
### Aliases: add_pareto_emoa_column

### ** Examples

# Define objective columns
objectives <- c("trust", "predictability", "perceivedSafety", "Comfort")

# Example data frame
main_df <- data.frame(
  trust = runif(10),
  predictability = runif(10),
  perceivedSafety = runif(10),
  Comfort = runif(10)
)

# Add the Pareto front column
main_df <- add_pareto_emoa_column(data = main_df, objectives)
head(main_df)



cleanEx()
nameEx("add_pareto_moocore_column")
### * add_pareto_moocore_column

flush(stderr()); flush(stdout())

### Name: add_pareto_moocore_column
### Title: Add 'PARETO_MOOCORE' Column to a Data Frame
### Aliases: add_pareto_moocore_column

### ** Examples

# Define objective columns
objectives <- c("trust", "predictability", "perceivedSafety", "Comfort")

# Example data frame
main_df <- data.frame(
  trust = runif(10),
  predictability = runif(10),
  perceivedSafety = runif(10),
  Comfort = runif(10)
)

# Add the Pareto front column
main_df <- add_pareto_moocore_column(data = main_df, objectives)
head(main_df)



cleanEx()
nameEx("analyze_and_report")
### * analyze_and_report

flush(stderr()); flush(stdout())

### Name: analyze_and_report
### Title: Analyze one dependent variable and produce everything a paper
###   needs
### Aliases: analyze_and_report

### ** Examples




cleanEx()
nameEx("assumption_methods_text")
### * assumption_methods_text

flush(stderr()); flush(stdout())

### Name: assumption_methods_text
### Title: Methods-section sentence justifying the test selection
### Aliases: assumption_methods_text

### ** Examples

set.seed(1)
d <- data.frame(g = rep(c("A", "B"), each = 20), v = rnorm(40))
assumption_methods_text(d, x = "g", y = "v")



cleanEx()
nameEx("checkAssumptionsForAnova")
### * checkAssumptionsForAnova

flush(stderr()); flush(stdout())

### Name: checkAssumptionsForAnova
### Title: Check the assumptions for an ANOVA with a variable number of
###   factors: Normality and Homogeneity of variance assumption.
### Aliases: checkAssumptionsForAnova check_assumptions_anova

### ** Examples




cleanEx()
nameEx("cite_methods")
### * cite_methods

flush(stderr()); flush(stdout())

### Name: cite_methods
### Title: Citations and methods boilerplate for the analyses used
### Aliases: cite_methods

### ** Examples

cite_methods("ggstatsplot", bibtex = FALSE)



cleanEx()
nameEx("classify_outcome")
### * classify_outcome

flush(stderr()); flush(stdout())

### Name: classify_outcome
### Title: Classify the measurement scale of an outcome variable
### Aliases: classify_outcome

### ** Examples

classify_outcome(rnorm(50)) # "continuous"
classify_outcome(factor(sample(1:5, 50, TRUE), ordered = TRUE)) # "ordinal"
classify_outcome(sample(0:1, 50, TRUE)) # "binary"
classify_outcome(rpois(50, 3)) # "count"



cleanEx()
nameEx("colleyRstats_setup")
### * colleyRstats_setup

flush(stderr()); flush(stdout())

### Name: colleyRstats_setup
### Title: Configure Global R Environment for colleyRstats
### Aliases: colleyRstats_setup

### ** Examples

# Runs everywhere, no extra packages, no session side effects
colleyRstats::colleyRstats_setup(
  set_options = FALSE,
  set_theme = FALSE,
  set_conflicts = FALSE,
  print_citation = FALSE,
  verbose = FALSE
)




cleanEx()
nameEx("colley_theme")
### * colley_theme

flush(stderr()); flush(stdout())

### Name: colley_theme
### Title: The colleyRstats ggplot2 theme
### Aliases: colley_theme

### ** Examples




cleanEx()
nameEx("debug_contr_error")
### * debug_contr_error

flush(stderr()); flush(stdout())

### Name: debug_contr_error
### Title: Debug contrast errors in ANOVA-like models
### Aliases: debug_contr_error

### ** Examples




cleanEx()
nameEx("define_result_macro")
### * define_result_macro

flush(stderr()); flush(stdout())

### Name: define_result_macro
### Title: Define a named LaTeX macro for a single result (single source of
###   truth)
### Aliases: define_result_macro

### ** Examples

define_result_macro("tlx_mental_omnibus", "F(2, 57) = 4.50, p = .02")



cleanEx()
nameEx("emit_name_macros")
### * emit_name_macros

flush(stderr()); flush(stdout())

### Name: emit_name_macros
### Title: Generate \newcommand stubs for variable/factor names
### Aliases: emit_name_macros

### ** Examples

emit_name_macros(c("Video", "DriverPosition"))
emit_name_macros(c(tlxMental = "TLX Mental Demand"))



cleanEx()
nameEx("emit_overleaf")
### * emit_overleaf

flush(stderr()); flush(stdout())

### Name: emit_overleaf
### Title: Bundle an analysis into an Overleaf-ready folder
### Aliases: emit_overleaf

### ** Examples




cleanEx()
nameEx("expand_latex_macros")
### * expand_latex_macros

flush(stderr()); flush(stdout())

### Name: expand_latex_macros
### Title: Expand the colleyRstats LaTeX macros to plain standard LaTeX
### Aliases: expand_latex_macros

### ** Examples

expand_latex_macros("A significant effect (\\F{2}{57}{4.50}, \\p{0.012}).")



cleanEx()
nameEx("figure_base_size")
### * figure_base_size

flush(stderr()); flush(stdout())

### Name: figure_base_size
### Title: Base font size for a figure of a given width
### Aliases: figure_base_size

### ** Examples

figure_base_size(3.33)  # 7
figure_base_size(7)     # 9



cleanEx()
nameEx("generateEffectPlot")
### * generateEffectPlot

flush(stderr()); flush(stdout())

### Name: generateEffectPlot
### Title: Function to define a plot, either showing the main or
###   interaction effect in bold.
### Aliases: generateEffectPlot plot_effect

### ** Examples




cleanEx()
nameEx("generateMoboPlot")
### * generateMoboPlot

flush(stderr()); flush(stdout())

### Name: generateMoboPlot
### Title: Generate a Multi-objective Optimization Plot
### Aliases: generateMoboPlot plot_mobo

### ** Examples

library(ggplot2)
library(ggpmisc)

# Example with numeric x-axis
df <- data.frame(
  x = 1:20,
  y = rnorm(20),
  ConditionID = rep(c("A", "B"), 10)
)
generateMoboPlot(df, x = "x", y = "y")

# Example with factor x-axis
df <- data.frame(
  x = factor(rep(1:5, each = 4)),
  y = rnorm(20),
  ConditionID = rep(c("A", "B"), 10)
)
generateMoboPlot(df, x = "x", y = "y", numberSamplingSteps = 3)



cleanEx()
nameEx("generateMoboPlot2")
### * generateMoboPlot2

flush(stderr()); flush(stdout())

### Name: generateMoboPlot2
### Title: Generate a Multi-objective Optimization Plot
### Aliases: generateMoboPlot2 plot_mobo2

### ** Examples

library(ggplot2)
library(ggpmisc)

# Example with numeric x-axis
df <- data.frame(
  x = 1:20,
  y = rnorm(20),
  ConditionID = rep(c("A", "B"), 10),
  Phase = rep(c("Sampling", "Optimization"), 10)
)
generateMoboPlot2(data = df, x = "x", y = "y")



cleanEx()
nameEx("ggbetweenstatsWithPriorNormalityCheck")
### * ggbetweenstatsWithPriorNormalityCheck

flush(stderr()); flush(stdout())

### Name: ggbetweenstatsWithPriorNormalityCheck
### Title: Check the data's distribution. If non-normal, take the
###   non-parametric variant of _ggbetweenstats_. x and y have to be in
###   parentheses, e.g., "ConditionID".
### Aliases: ggbetweenstatsWithPriorNormalityCheck plot_between_stats

### ** Examples




cleanEx()
nameEx("ggbetweenstatsWithPriorNormalityCheckAsterisk")
### * ggbetweenstatsWithPriorNormalityCheckAsterisk

flush(stderr()); flush(stdout())

### Name: ggbetweenstatsWithPriorNormalityCheckAsterisk
### Title: Check the data's distribution. If non-normal, take the
###   non-parametric variant of _ggbetweenstats_. x and y have to be in
###   parentheses, e.g., "ConditionID".
### Aliases: ggbetweenstatsWithPriorNormalityCheckAsterisk
###   plot_between_stats_asterisk

### ** Examples




cleanEx()
nameEx("ggwithinstatsWithPriorNormalityCheck")
### * ggwithinstatsWithPriorNormalityCheck

flush(stderr()); flush(stdout())

### Name: ggwithinstatsWithPriorNormalityCheck
### Title: Check the data's distribution. If non-normal, take the
###   non-parametric variant of _ggwithinstats_. x and y have to be in
###   parentheses, e.g., "ConditionID".
### Aliases: ggwithinstatsWithPriorNormalityCheck plot_within_stats

### ** Examples




cleanEx()
nameEx("ggwithinstatsWithPriorNormalityCheckAsterisk")
### * ggwithinstatsWithPriorNormalityCheckAsterisk

flush(stderr()); flush(stdout())

### Name: ggwithinstatsWithPriorNormalityCheckAsterisk
### Title: Check the data's distribution. If non-normal, take the
###   non-parametric variant of _ggwithinstats_. x and y have to be in
###   parentheses, e.g., "ConditionID". Add Asterisks instead of p-values.
### Aliases: ggwithinstatsWithPriorNormalityCheckAsterisk
###   plot_within_stats_asterisk

### ** Examples




cleanEx()
nameEx("latex_escape")
### * latex_escape

flush(stderr()); flush(stdout())

### Name: latex_escape
### Title: Escape LaTeX special characters in plain text
### Aliases: latex_escape

### ** Examples

latex_escape("tlx_mental")
latex_escape("cost (%) & margin")



cleanEx()
nameEx("latex_preamble")
### * latex_preamble

flush(stderr()); flush(stdout())

### Name: latex_preamble
### Title: LaTeX preamble required by the report functions
### Aliases: latex_preamble

### ** Examples

latex_preamble()



cleanEx()
nameEx("latexify_report")
### * latexify_report

flush(stderr()); flush(stdout())

### Name: latexify_report
### Title: Transform text from 'report::report()' into LaTeX-friendly
###   output.
### Aliases: latexify_report

### ** Examples




cleanEx()
nameEx("na.zero")
### * na.zero

flush(stderr()); flush(stdout())

### Name: na.zero
### Title: Replace NA values with zero
### Aliases: na.zero

### ** Examples

na.zero(c(NA, 1, NA, 2))



cleanEx()
nameEx("normalize")
### * normalize

flush(stderr()); flush(stdout())

### Name: normalize
### Title: This function normalizes the values in a vector to the range
###   [new_min, new_max] based on their original range [old_min, old_max].
### Aliases: normalize

### ** Examples

normalize(c(1, 2, 3, 4, 5), 1, 5, 0, 1)



cleanEx()
nameEx("rFromNPAV")
### * rFromNPAV

flush(stderr()); flush(stdout())

### Name: rFromNPAV
### Title: Calculation based on Rosenthal's formula (1994). N stands for
###   the _number of measurements_.
### Aliases: rFromNPAV

### ** Examples

rFromNPAV(0.02, N = 180)



cleanEx()
nameEx("rFromWilcox")
### * rFromWilcox

flush(stderr()); flush(stdout())

### Name: rFromWilcox
### Title: Calculation based on Rosenthal's formula (1994). N stands for
###   the _number of measurements_.
### Aliases: rFromWilcox

### ** Examples

set.seed(1)
d <- data.frame(
  group = rep(c("A", "B"), each = 10),
  value = rnorm(20)
)
w <- stats::wilcox.test(value ~ group, data = d, exact = FALSE)
rFromWilcox(w, N = nrow(d))



cleanEx()
nameEx("rFromWilcoxAdjusted")
### * rFromWilcoxAdjusted

flush(stderr()); flush(stdout())

### Name: rFromWilcoxAdjusted
### Title: rFromWilcoxAdjusted
### Aliases: rFromWilcoxAdjusted

### ** Examples




cleanEx()
nameEx("recommend_test")
### * recommend_test

flush(stderr()); flush(stdout())

### Name: recommend_test
### Title: Recommend a principled analysis for one outcome
### Aliases: recommend_test recommend_analysis

### ** Examples

set.seed(1)
d <- data.frame(
  id    = factor(rep(1:20, each = 3)),
  cond  = factor(rep(c("A", "B", "C"), times = 20)),
  score = rnorm(60),
  rating = factor(sample(1:5, 60, TRUE), ordered = TRUE)
)
# Ordinal outcome measured repeatedly within subject -> CLMM
recommend_test(d, outcome = "rating", predictors = "cond", cluster = "id")
# Continuous, between-subjects -> ANOVA or its rank-based fallback
recommend_test(d, outcome = "score", predictors = "cond")



cleanEx()
nameEx("remove_outliers_REI")
### * remove_outliers_REI

flush(stderr()); flush(stdout())

### Name: remove_outliers_REI
### Title: Flag suspicious survey responses via the Response Entropy Index
###   (REI)
### Aliases: remove_outliers_REI

### ** Examples




cleanEx()
nameEx("replace_values")
### * replace_values

flush(stderr()); flush(stdout())

### Name: replace_values
### Title: Replace values across a data frame
### Aliases: replace_values

### ** Examples




cleanEx()
nameEx("reportART")
### * reportART

flush(stderr()); flush(stdout())

### Name: reportART
### Title: Generate the Latex-text based on the ARTool (see
###   <https://github.com/mjskay/ARTool>). The ART result must be piped
###   into an anova(). Only significant main and interaction effects are
###   reported. P-values are rounded for the third digit. Attention: Effect
###   sizes are not calculated! Attention: the independent variables of the
###   formula and the term specifying the participant must be factors
###   (i.e., use as.factor()).
### Aliases: reportART report_art

### ** Examples




cleanEx()
nameEx("reportArtCon")
### * reportArtCon

flush(stderr()); flush(stdout())

### Name: reportArtCon
### Title: Report significant ART contrasts (art.con) as LaTeX text
### Aliases: reportArtCon report_art_con

### ** Examples




cleanEx()
nameEx("reportArtConTable")
### * reportArtConTable

flush(stderr()); flush(stdout())

### Name: reportArtConTable
### Title: Report ART contrasts (art.con) as a LaTeX table. Customizable
###   with sensible defaults. Companion to 'reportDunnTestTable()'.
### Aliases: reportArtConTable report_art_con_table

### ** Examples




cleanEx()
nameEx("reportCLMM")
### * reportCLMM

flush(stderr()); flush(stdout())

### Name: reportCLMM
### Title: Report a cumulative link (mixed) model in LaTeX/APA style
### Aliases: reportCLMM report_clmm

### ** Examples




cleanEx()
nameEx("reportDunnTest")
### * reportDunnTest

flush(stderr()); flush(stdout())

### Name: reportDunnTest
### Title: Report dunnTest as text. Required commands in LaTeX:
###   '\newcommand{\padjminor}{\textit{p$_{adj}<$}}'
###   '\newcommand{\padj}{\textit{p$_{adj}$=}}'
###   '\newcommand{\rankbiserial}[1]{$r_{rb} = #1$}'
### Aliases: reportDunnTest report_dunn_test

### ** Examples




cleanEx()
nameEx("reportDunnTestTable")
### * reportDunnTestTable

flush(stderr()); flush(stdout())

### Name: reportDunnTestTable
### Title: report Dunn test as a table. Customizable with sensible
###   defaults. Required commands in LaTeX:
###   '\newcommand{\padjminor}{\textit{p$_{adj}<$}}'
###   '\newcommand{\padj}{\textit{p$_{adj}$=}}'
###   '\newcommand{\rankbiserial}[1]{$r_{rb} = #1$}'
### Aliases: reportDunnTestTable report_dunn_test_table

### ** Examples




cleanEx()
nameEx("reportGLMM")
### * reportGLMM

flush(stderr()); flush(stdout())

### Name: reportGLMM
### Title: Report a (generalized) linear mixed model in LaTeX/APA style
### Aliases: reportGLMM report_glmm

### ** Examples




cleanEx()
nameEx("reportMeanAndSD")
### * reportMeanAndSD

flush(stderr()); flush(stdout())

### Name: reportMeanAndSD
### Title: Report the mean and standard deviation of a dependent variable
###   for all levels of an independent variable rounded to the 2nd digit.
### Aliases: reportMeanAndSD report_mean_sd

### ** Examples




cleanEx()
nameEx("reportNPAV")
### * reportNPAV

flush(stderr()); flush(stdout())

### Name: reportNPAV
### Title: Generate the Latex-text based on the NPAV by Lüpsen (see
###   <https://www.uni-koeln.de/~luepsen/R/>). Only significant main and
###   interaction effects are reported. P-values are rounded for the third
###   digit and partial eta squared values are provided when possible.
###   Attention: the independent variables of the formula and the term
###   specifying the participant must be factors (i.e., use as.factor()).
### Aliases: reportNPAV

### ** Examples

model <- data.frame(
  Df = c(1, 1, 10),
  `F value` = c(6.12, 5.01, NA),
  `Pr(>F)` = c(0.033, 0.045, NA),
  check.names = FALSE
)
rownames(model) <- c("Video", "gesture:eHMI", "Residuals")
reportNPAV(model, dv = "mental workload")



cleanEx()
nameEx("reportNparLD")
### * reportNparLD

flush(stderr()); flush(stdout())

### Name: reportNparLD
### Title: Report the model produced by nparLD. The model provided must be
###   the model generated by the command 'nparLD' 'nparLD' (see
###   <https://CRAN.R-project.org/package=nparLD>).
### Aliases: reportNparLD report_nparld

### ** Examples




cleanEx()
nameEx("report_all")
### * report_all

flush(stderr()); flush(stdout())

### Name: report_all
### Title: Analyze and report several dependent variables at once
### Aliases: report_all

### ** Examples




cleanEx()
nameEx("reportggstatsplot")
### * reportggstatsplot

flush(stderr()); flush(stdout())

### Name: reportggstatsplot
### Title: Report statistical details for ggstatsplot.
### Aliases: reportggstatsplot report_ggstatsplot

### ** Examples




cleanEx()
nameEx("reportggstatsplotPostHoc")
### * reportggstatsplotPostHoc

flush(stderr()); flush(stdout())

### Name: reportggstatsplotPostHoc
### Title: Report significant post-hoc pairwise comparisons
### Aliases: reportggstatsplotPostHoc report_ggstatsplot_posthoc

### ** Examples




cleanEx()
nameEx("reshape_data")
### * reshape_data

flush(stderr()); flush(stdout())

### Name: reshape_data
### Title: Reshape Excel Data Based on Custom Markers and Include Custom ID
###   Column
### Aliases: reshape_data

### ** Examples




cleanEx()
nameEx("save_paper_figure")
### * save_paper_figure

flush(stderr()); flush(stdout())

### Name: save_paper_figure
### Title: Save a plot with publication-ready defaults
### Aliases: save_paper_figure

### ** Examples




cleanEx()
nameEx("stat_sum_df")
### * stat_sum_df

flush(stderr()); flush(stdout())

### Name: stat_sum_df
### Title: Generating the sum and adding a crossbar.
### Aliases: stat_sum_df

### ** Examples




cleanEx()
nameEx("use_colleyrstats_sty")
### * use_colleyrstats_sty

flush(stderr()); flush(stdout())

### Name: use_colleyrstats_sty
### Title: Write colleyRstats.sty into a project (for Overleaf)
### Aliases: use_colleyrstats_sty

### ** Examples

use_colleyrstats_sty(tempdir(), overwrite = TRUE)



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
