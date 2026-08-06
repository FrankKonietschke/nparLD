#' Alpha-amylase Saliva Study
#'
#' Measurements of alpha-amylase levels in saliva from healthy volunteers.
#' The study illustrates a two-factor longitudinal design with two repeated
#' measures factors.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Alpha-amylase measurement.}
#'   \item{time1}{First repeated-measures factor.}
#'   \item{time2}{Second repeated-measures factor.}
#'   \item{subject}{Subject identifier.}
#' }
#'
#' @examples
#' data(amylase)
#'
#' fit <- nparLD(resp ~ time1 * time2,
#'               data = amylase,
#'               subject = "subject",
#'               effect = "weighted",
#'               hypothesis = "H0F")
#' fit
#'
#' # Dunnett-type contrasts for the levels of time2
#' fit <- nparLD(resp ~ time1 * time2,
#'               data = amylase,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("time2", "Dunnett"),
#'               Factor.Information = TRUE)
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#'
#' @docType data
#' @keywords datasets
"amylase"


#' Dental Growth Study
#'
#' Measurements of distances between the center of the pituitary and the
#' pterygomaxillary fissure in boys. The data illustrate a one-factor
#' longitudinal design.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Distance measurement in millimeters.}
#'   \item{time}{Age at measurement.}
#'   \item{subject}{Subject identifier.}
#' }
#'
#' @examples
#' data(dental)
#'
#' dental$time <- factor(dental$time)
#'
#' \dontrun{
#' fit <- nparLD(resp ~ time,
#'               data = dental,
#'               subject = "subject",
#'               hypothesis = "H0F")
#' fit
#' }
#'
#' \dontrun{
#' fit <- nparLD(resp ~ time,
#'               data = dental,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("time", "Tukey"))
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#'}
#'
#' @docType data
#' @keywords datasets
"dental"


#' Postoperative Edema Study
#'
#' Skin-temperature measurements from patients after hand surgery. The study
#' compares treatment groups and includes two repeated-measures factors.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Skin temperature measurement.}
#'   \item{time1}{Hand factor.}
#'   \item{time2}{Day factor.}
#'   \item{group}{Treatment group.}
#'   \item{subject}{Subject identifier.}
#' }
#'
#' @examples
#' data(edema)
#'
#' \dontrun{
#' fit <- nparLD(resp ~ group * time1 * time2,
#'               data = edema,
#'               subject = "subject",
#'               hypothesis = "H0F")
#' fit
#' }
#'
#' \dontrun{
#' fit <- nparLD(resp ~ group * time1 * time2,
#'               data = edema,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("group", "Tukey"),
#'               Factor.Information = TRUE)
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#' }
#'
#' @docType data
#' @keywords datasets
"edema"


#' Panic Disorder Study I
#'
#' Clinical Global Impression scores from patients with panic disorder and
#' agoraphobia. The data illustrate a one-factor longitudinal design with
#' ordinal responses.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Clinical Global Impression score.}
#'   \item{time}{Visit or week of assessment.}
#'   \item{subject}{Patient identifier.}
#' }
#'
#' @examples
#' data(panic)
#'
#' \dontrun{
#' fit <- nparLD(resp ~ time,
#'               data = panic,
#'               subject = "subject",
#'               hypothesis = "H0F", contrast =list("time", "Dunnett"))
#' fit
#' }
#'
#' \dontrun{
#' fit <- nparLD(resp ~ time,
#'               data = panic,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("time", "Dunnett"))
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#' }
#'
#' @docType data
#' @keywords datasets
"panic"


#' Panic Disorder Study II
#'
#' Panic and agoraphobia scores from patients with panic disorder, with or
#' without agoraphobia. The data illustrate a factorial longitudinal design with
#' one whole-plot factor and one repeated-measures factor.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Panic and agoraphobia score.}
#'   \item{time}{Visit or week of assessment.}
#'   \item{group}{Agoraphobia group.}
#'   \item{subject}{Patient identifier.}
#' }
#'
#' @examples
#' data(panic2)
#'
#' fit <- nparLD(resp ~ group * time,
#'               data = panic2,
#'               subject = "subject",
#'               hypothesis = "H0F", effect="unweighted")
#' fit
#'
#' fit <- nparLD(resp ~ group * time,
#'               data = panic2,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("group:time"),
#'               Factor.Information = TRUE,
#'               covariance=TRUE)
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#'
#' @docType data
#' @keywords datasets
"panic2"


#' Plasma-renin Activity Study
#'
#' Plasma-renin activity measurements from a randomized study of healthy
#' non-smokers. The data illustrate a factorial longitudinal design with one
#' whole-plot factor and one repeated-measures factor.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Plasma-renin activity measurement.}
#'   \item{time}{Measurement time.}
#'   \item{group}{Drug group.}
#'   \item{subject}{Subject identifier.}
#' }
#'
#' @examples
#' data(plasma)
#'
#' fit <- nparLD(resp ~ group * time,
#'               data = plasma,
#'               subject = "subject",
#'               hypothesis = "H0F", effect="weighted")
#' fit
#'
#' fit <- nparLD(resp ~ group * time,
#'               data = plasma,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("group", "Tukey"))
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#'
#' @docType data
#' @keywords datasets
"plasma"


#' Rat Growth Study
#'
#' Body-weight measurements from rats observed over a five-week period. The data
#' illustrate longitudinal growth curves in several treatment groups.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Body weight in grams.}
#'   \item{time}{Week of measurement.}
#'   \item{group}{Treatment group.}
#'   \item{subject}{Rat identifier.}
#' }
#'
#' @examples
#' data(rat)
#'
#' fit <- nparLD(resp ~ group * time,
#'               data = rat,
#'               subject = "subject",
#'               hypothesis = "H0F")
#' fit
#'
#' fit <- nparLD(resp ~ group * time,
#'               data = rat,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("group:time"))
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#'
#' @docType data
#' @keywords datasets
"rat"


#' Respiratory Disorder Study
#'
#' Ordinal health-status measurements from patients with a respiratory disorder.
#' The data include study center, treatment group, and repeated visits.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Ordinal health-status response.}
#'   \item{time}{Visit number.}
#'   \item{center}{Study center.}
#'   \item{treatment}{Treatment group.}
#'   \item{patient}{Patient identifier.}
#' }
#'
#' @examples
#' data(respiration)
#'
#' fit <- nparLD(resp ~ center * treatment * time,
#'               data = respiration,
#'               subject = "patient",
#'               hypothesis = "H0F")
#' fit
#'
#' \dontrun{
#' fit <- nparLD(resp ~ center * treatment * time,
#'               data = respiration,
#'               subject = "patient",
#'               hypothesis = "H0p",
#'               contrast = list("treatment", "Tukey"))
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#' }
#'
#' @docType data
#' @keywords datasets
"respiration"


#' Shoulder Tip Pain Study
#'
#' Shoulder pain scores from patients after laparoscopic abdominal surgery. The
#' study includes treatment group, gender group, and repeated pain measurements.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Shoulder pain score.}
#'   \item{time}{Measurement occasion.}
#'   \item{group1}{Treatment group.}
#'   \item{group2}{Gender group.}
#'   \item{subject}{Patient identifier.}
#' }
#'
#' @examples
#' data(shoulder)
#'
#' \dontrun{
#' fit <- nparLD(resp ~ group1 * group2 * time,
#'               data = shoulder,
#'               subject = "subject",
#'               hypothesis = "H0F")
#' fit
#' }
#'
#' \dontrun{
#' fit <- nparLD(resp ~ group1 * group2 * time,
#'               data = shoulder,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("group1:time"),
#'               Factor.Information=TRUE)
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#' }
#'
#' @docType data
#' @keywords datasets
"shoulder"


#' Vitality of Treetops
#'
#' Repeated vitality scores of treetops from three experimental areas. The data
#' illustrate an ordinal longitudinal response in several groups.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{resp}{Vitality score.}
#'   \item{time}{Year or measurement occasion.}
#'   \item{group}{Experimental area.}
#'   \item{subject}{Tree identifier.}
#' }
#'
#' @examples
#' data(tree)
#'
#' \dontrun{
#' fit <- nparLD(resp ~ group * time,
#'               data = tree,
#'               subject = "subject",
#'               hypothesis = "H0F")
#' fit
#'}
#'
#'
#' \dontrun{
#' fit <- nparLD(resp ~ group * time,
#'               data = tree,
#'               subject = "subject",
#'               hypothesis = "H0p",
#'               contrast = list("group:time"))
#' fit
#' plot(fit)
#' plot(fit$MCTP)
#'}
#'
#' @docType data
#' @keywords datasets
"tree"

#' BrdU incorporation in fibroblasts
#'
#' BrdU incorporation in fibroblast cultures measured under four dose
#' conditions. For each culture and dose condition, three replicate measurements
#' are available. The data set is provided in long format and can be used to
#' illustrate dependent replicate measurements in `nparLD()`.
#'
#' @format A data frame with 60 rows and 4 variables:
#' \describe{
#'   \item{resp}{BrdU incorporation response.}
#'   \item{dose}{Dose condition, with levels `0`, `0.1`, `1`, and `10`.}
#'   \item{culture}{Culture identifier. This is the independent experimental unit.}
#'   \item{replicate}{Replicate identifier within each culture-dose condition.}
#' }
#'
#' @details
#' The original variables were `CultureNo`, `Replic`, `Dose`, and `Brdu`.
#' In the package data set, the variables were renamed to `culture`,
#' `replicate`, `dose`, and `resp` for consistency with the examples.
#'
#' @examples
#' data(brdu)
#'
#' fit <- nparLD(
#'   resp ~ dose,
#'   data = brdu,
#'   subject = "culture",
#'   replicate = "replicate",
#'   hypothesis = "H0p",
#'   cell.weights = "subjects"
#' )
#'
#' fit
#'
#' @name brdu
#' @docType data
#' @keywords datasets
"brdu"