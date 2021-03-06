#' Data from one patient and 28 controls on the size-weight illusion
#'
#' A dataset containing data from 28 healthy controls and one patient, DF, with
#' visual form agnosia (inability to perceive the form of objects) from
#' bilateral lesions to the lateral occipital complex. The size-weight illusion
#' occurs when a person underestimates the weight of a larger item when compared
#' to a smaller of equal weight (Charpentier, 1891). From these data, one can
#' assess the magnitude of the illusion for patient DF by comparison to
#' age-matched controls under visual and kinaesthetic cue conditions. The
#' measure of the size-weight illusion is a scaled measure expressing the
#' number of grams weight difference perceived per cubic cm of volume change
#' (Hassan et al, 2020).
#'
#' @format A data frame with 29 rows and 6 variables:
#' \describe{
#'   \item{GROUP}{factor with patient (SC) or control group (HC)}
#'   \item{PPT}{participant identifier}
#'   \item{SEX}{gender of partcipants}
#'   \item{YRS}{age of participants}
#'   \item{V_SWI}{SWI measure from the visual task}
#'   \item{K_SWI}{SWI measure from the kinaesthetic task}
#' }
#'
#' @references
#' Hassan, E. K., Sedda, A., Buckingham, G., & McIntosh, R. D. (2020). The
#' size-weight illusion in visual form agnosic patient DF. Neurocase, 1-8.
#' https://doi.org/10.1080/13554794.2020.1800748
#'
#' @source \url{https://osf.io/3s2fp/?view_only=50c8af0b39ee436b85d292b0a701cc3b}
"size_weight_illusion"
