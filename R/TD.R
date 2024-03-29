#' Test of Deficit
#'
#' Crawford and Howell's (1998) modified t-test. Takes a single observation and
#' compares it to a distribution estimated by a control sample. Calculates
#' standardised difference between the case score and the mean of the controls
#' and proportions falling above or below the case score, as well as associated
#' confidence intervals.
#'
#' Returns the point estimate of the standardised difference
#' between the case score and the mean of the controls and the point estimate
#' of the p-value (i.e. the percentage of the population that would be
#' expected to obtain a lower or higher score, depending on the alternative
#' hypothesis).
#'
#' @section Note of caution:
#' Calculating the confidence intervals relies on finding non-centrality
#' parameters for non-central t-distributions. Depending on the degrees of
#' freedom, the confidence level and the effect size exact accuracy from the
#' \code{stats::qt()} function used can not be guaranteed. However, the
#' approximations should be good enough for most cases.
#' See \url{https://stat.ethz.ch/pipermail/r-help/2008-June/164843.html}.
#'
#' @param case Case observation, can only be a single value.
#' @param controls Numeric vector of observations from the control sample. If
#'   single value, treated as mean.
#' @param sd If input of controls is single value, the standard
#'   deviation of the sample must be given as well.
#' @param sample_size If input of controls is single value, the size of the
#'   sample must be gven as well.
#' @param alternative A character string specifying the alternative hypothesis,
#'   must be one of \code{"less"} (default), \code{"greater"} or
#'   \code{"two.sided"}. You can specify just the initial letter.
#' @param conf_int Initiates a search algorithm for finding confidence
#'   intervals. Defaults to \code{TRUE}, set to \code{FALSE} for faster
#'   calculation (e.g. for simulations).
#' @param conf_level Level of confidence for intervals, defaults to 95\%.
#' @param conf_int_spec The size of iterative steps for calculating confidence
#'   intervals. Smaller values gives more precise intervals but takes longer to
#'   calculate. Defaults to a specificity of 0.01.
#' @param na.rm Remove \code{NA}s from controls.
#'
#' @return A list of class \code{"htest"} containing the following components:
#' \tabular{llll}{
#' \code{statistic}   \tab the value of the t-statistic.\cr\cr  \code{parameter}
#' \tab the degrees of freedom for the t-statistic.\cr\cr \code{p.value}    \tab
#' the p-value for the test.\cr\cr \code{estimate}    \tab estimated
#' standardised difference (Z-CC) and point estimate of p-value. \cr\cr
#' \code{null.value}   \tab the value of the difference under the null
#' hypothesis.\cr\cr \code{interval}     \tab named numerical vector containing
#' level of confidence and confidence intervals for both Z-CC and p-value. \cr\cr
#' \code{desc}     \tab named numerical containing descriptive statistics: mean
#' and standard deviations of controls as well as sample size and standard error
#' used in the t-formula. \cr\cr \code{alternative}     \tab a character string
#' describing the alternative hypothesis.\cr\cr \code{method} \tab a character
#' string indicating what type of t-test was performed.\cr\cr \code{data.name}
#' \tab a character string giving the name(s) of the data as well as
#' summaries. }
#'
#' @export
#'
#' @examples
#' TD(case = -2, controls = 0, sd = 1, sample_size = 20)
#'
#' TD(case = size_weight_illusion[1, "V_SWI"],
#'    controls = size_weight_illusion[-1, "V_SWI"], alternative = "l")
#'
#' @references
#'
#' Crawford, J. R., & Howell, D. C. (1998). Comparing an Individual's Test Score
#' Against Norms Derived from Small Samples. \emph{The Clinical Neuropsychologist,
#' 12}(4), 482 - 486. \doi{10.1076/clin.12.4.482.7241}
#'
#' Crawford, J. R., & Garthwaite, P. H. (2002). Investigation of the single case
#' in neuropsychology: Confidence limits on the abnormality of test scores and
#' test score differences. \emph{Neuropsychologia, 40}(8), 1196-1208.
#' \doi{10.1016/S0028-3932(01)00224-X}




TD <- function (case, controls, sd = NULL, sample_size = NULL,
                alternative = c("less", "greater", "two.sided"),
                conf_int = TRUE, conf_level = 0.95,
                conf_int_spec = 0.01,  na.rm = FALSE) {

  ###
  # Set up of error and warning messages
  ###

  case <- as.numeric(unlist(case))
  controls <- as.numeric(unlist(controls))

  if (length(case)>1) stop("Case should only have 1 observation")
  if (length(controls)<2 & is.null(sd) == TRUE) {
    stop("Not enough obs. Set sd and sample size for input of controls to be treated as mean")
  }

  if (length(controls)<2 & is.null(sd) == FALSE & is.null(sample_size) == TRUE) stop("Input sample size")
  if (is.na(case)==TRUE) stop("Case is NA")

  if (na.rm == TRUE) controls <- controls[!is.na(controls)]
  if (sum(is.na(controls)) > 0) stop("Controls contains NA, set na.rm = TRUE to proceed")

  if (conf_int == TRUE & (conf_level < 0 | conf_level >= 1)) stop("Confidence level must be between 0 and < 1")


  ###
  # Extract relevant statistics
  ###

  alternative <- match.arg(alternative)

  con_m <- mean(controls) # Mean of the control sample

  con_sd <- stats::sd(controls) # Standard deviation of the control sample (returns NA if summary stats used)
  if (length(controls)<2 & is.null(sd) == FALSE) con_sd <- sd

  n <- length(controls)
  if (length(controls)<2 & is.null(sd) == FALSE & is.null(sample_size) == FALSE) n <- sample_size

  stderr <- con_sd * sqrt((n + 1)/n) # Standard error by C&H (1998) method

  zcc <- (case - con_m)/con_sd
  tstat <- (case - con_m)/stderr # C&H's modified t
  df <- n - 1 # The degrees of freedom

  ###
  # Get p-value depending on alternative hypothesis
  ###

  if (alternative == "less") {

    pval <- stats::pt(tstat, df = df)

  } else if (alternative == "greater") {

    pval <- stats::pt(tstat, df = df, lower.tail = FALSE)

  } else if (alternative == "two.sided") {

    pval <- 2 * (1 - stats::pt(abs(tstat), df = df))

  }

  ###
  # Set relevant estimates
  ###

  estimate <- c(zcc, pval*100)
  if (alternative == "two.sided") estimate <- c(zcc, (pval/2)*100)

  ###
  # Find the confidence boundaries
  ###

  if (conf_int == T) {


    # Below is a search algorithm to find the non-centrality parameter of two non-central t-distributions
    # which have their alpha/2 and 1-alpha/2 percentile at the std effect size * sqrt(n), respectively.
    # These non-centrality paramters / sqrt(n) are then taken as the limits of the CIs.
    # Method described in Crawford and Garthwaite (2002).

    alph <- 1 - conf_level

    stop_ci_lo <- FALSE
    ncp_lo <- zcc*sqrt(n)
    perc_lo <- 1 - (alph/2)
    while (stop_ci_lo == FALSE) {

      # Here we search downwards with each step being as big as specified in conf_int_spec
      ncp_lo <- ncp_lo - conf_int_spec

      suppressWarnings( # Depending on ncp and percentile qt gives approximations, which produces warnings
        quant <- stats::qt(perc_lo, df = df, ncp = ncp_lo)
      )

      if (quant <= zcc*sqrt(n)) { # When the specified quantile reaches zcc*sqrt(n) the search stops
        stop_ci_lo <- TRUE
      }
    }

    stop_ci_up <- FALSE
    ncp_up <- zcc*sqrt(n)
    perc_up <- (alph/2)
    while (stop_ci_up == FALSE) {

      # Here we search upwards with each step being as big as specified in conf_int_spec
      ncp_up <- ncp_up + conf_int_spec

      suppressWarnings( # Depending on ncp and percentile qt gives approximations, which produces warnings
        quant <- stats::qt(perc_up, df = df, ncp = ncp_up)
      )

      if (quant >= zcc*sqrt(n)) { # Wen the specified quantile reaches zcc*sqrt(n) the search stops
        stop_ci_up <- TRUE
      }
    }

    ci_lo_zcc <- ncp_lo/sqrt(n)
    ci_up_zcc <- ncp_up/sqrt(n)
    cint_zcc <- c(ci_lo_zcc, ci_up_zcc)

    ###
    # Set the name of the zcc estimate so that the CI is shown for print()
    ###

    zcc.name <- paste0("Std. case score (Z-CC), ",
                       100*conf_level, "% CI [",
                       format(round(cint_zcc[1], 2), nsmall = 2),", ",
                       format(round(cint_zcc[2], 2), nsmall = 2),"]")
    ###
    # Get CIs for p-estimates, and set the names
    ###

      if (alternative == "less") {

        ci_lo_p <- stats::pnorm(ci_lo_zcc)*100
        ci_up_p <- stats::pnorm(ci_up_zcc)*100
        cint_p <- c(ci_lo_p, ci_up_p)

        p.name <- paste0("Proportion below case (%), ",
                         100*conf_level, "% CI [",
                         format(round(cint_p[1], 2), nsmall = 2),", ",
                         format(round(cint_p[2], 2), nsmall = 2),"]")

      } else if (alternative == "greater") {

        ci_lo_p <- (1 - stats::pnorm(ci_lo_zcc))*100
        ci_up_p <- (1 - stats::pnorm(ci_up_zcc))*100

        # NOTE (!): Because of right side of dist, lower and upper CI must switch to
        # be consistent with lower CI to the left and upper to the right in output
        cint_p <- c(ci_up_p, ci_lo_p)

        p.name <- paste0("Proportion above case (%), ",
                         100*conf_level, "% CI [",
                         format(round(cint_p[1], 2), nsmall = 2),", ",
                         format(round(cint_p[2], 2), nsmall = 2),"]")

      } else {
        if (tstat < 0) {

          ci_lo_p <- stats::pnorm(ci_lo_zcc)*100
          ci_up_p <- stats::pnorm(ci_up_zcc)*100
          cint_p <- c(ci_lo_p, ci_up_p)

          p.name <- paste0("Proportion below case (%), ",
                           100*conf_level, "% CI [",
                           format(round(cint_p[1], 2), nsmall = 2),", ",
                           format(round(cint_p[2], 2), nsmall = 2),"]")

        } else {

          ci_lo_p <- (1 - stats::pnorm(ci_lo_zcc))*100
          ci_up_p <- (1 - stats::pnorm(ci_up_zcc))*100

          # NOTE (!): Because of right side of dist, lower and upper CI must switch to
          # be consistent with lower CI to the left and upper to the right in output
          cint_p <- c(ci_up_p, ci_lo_p)

          p.name <- paste0("Proportion above case (%), ",
                           100*conf_level, "% CI [",
                           format(round(cint_p[1], 2), nsmall = 2),", ",
                           format(round(cint_p[2], 2), nsmall = 2),"]")
        }
      }


    ###
    # Set names for the CI stored in the output
    ###

    names(cint_zcc) <- c("Lower Z-CC CI", "Upper Z-CC CI")
    names(cint_p) <- c("Lower p CI", "Upper p CI")

    typ.int <- 100*conf_level
    names(typ.int) <- "Confidence (%)"

    interval <- c(typ.int, cint_zcc, cint_p)

    names(estimate) <- c(zcc.name, p.name)


  } else {

    ###
    # If CI is not desired
    ###

    interval <- NULL

    if (alternative == "less") {
      names(estimate) <- c("Std. case score (Z-CC)",
                           "Proportion below case (%)")
    } else if (alternative == "greater") {
      names(estimate) <- c("Std. case score (Z-CC)",
                           "Proportion above case (%)")
    } else {
      names(estimate) <- c("Std. case score (Z-CC)",
                           paste("Proportion", ifelse(tstat < 0, "below", "above"), "case (%)"))
    }

  }

  ###
  # Set names for objects in output
  ###

  names(tstat) <- "t"
  names(df) <- "df"
  null.value <- 0 # Null hypothesis: difference = 0
  names(null.value) <- "diff. between case and controls"
  names(con_m) <- "Mean (controls)"
  names(con_sd) <- "SD (controls)"
  names(n) <- "Sample size"
  names(stderr) <- "Std.err by C&H-method"


  # Build output to be able to set class as "htest" object for S3 methods.
  # See documentation for "htest" class for more info

  output <- list(statistic = tstat, parameter = df, p.value = pval,
                 estimate = estimate, null.value = null.value,
                 interval = interval,
                 desc = c(con_m, con_sd, n, stderr),
                 alternative = alternative,
                 method = paste("Test of Deficit"),
                 data.name = paste0("Case = ", format(round(case, 2), nsmall = 2),
                                    ", Controls (m = ", format(round(con_m, 2), nsmall = 2),
                                    ", sd = ", format(round(con_sd, 2), nsmall = 2),
                                    ", n = ", n, ")"))

  class(output) <- "htest"
  output

}
