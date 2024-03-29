#' Bayesian Standardised Difference Test
#'
#' A test on the discrepancy between two tasks in a single case, by comparison
#' to the discrepancy of means in the same two tasks in a control sample. Can
#' take both tasks measured on the same scale with the same underlying
#' distribution or tasks measured on different scales by setting
#' \code{unstandardised} to \code{TRUE} or \code{FALSE} (default). Calculates a
#' standardised effects size of task discrepancy as well as a point estimate of
#' the proportion of the control population that would be expected to show a
#' more extreme discrepancy as well as relevant credible intervals. This test
#' is based on random number generation which means that results may vary
#' between runs. This is by design and the reason for not using \code{set.seed()}
#' to reproduce results inside the function is to emphasise the randomness of
#' the test. To get more accurate and stable results please increase the number
#' of iterations by increasing \code{iter} whenever feasible. Developed by
#' Crawford and Garthwaite (2007).
#'
#' Uses random generation of inverse wishart distributions from the
#' CholWishart package (Geoffrey Thompson, 2019).
#'
#' @param case_a Case's score on task A.
#' @param case_b Case's score on task B.
#' @param controls_a Controls' scores on task A. Takes either a vector of
#'   observations or a single value interpreted as mean. \emph{Note}: you can
#'   supply a vector as input for task A while mean and SD for task B.
#' @param controls_b Controls' scores on task A. Takes either a vector of
#'   observations or a single value interpreted as mean. \emph{Note}: you can
#'   supply a vector as input for task B while mean and SD for task A.
#' @param sd_a If single value for task A is given as input you must
#'   supply the standard deviation of the sample.
#' @param sd_b If single value for task B is given as input you must
#'   supply the standard deviation of the sample.
#' @param sample_size If A or B is given as mean and SD you must supply the
#'   sample size. If controls_a is given as vector and controls_b as mean and
#'   SD, sample_size must equal the number of observations in controls_a.
#' @param r_ab If A or B is given as mean and SD you must supply the
#'   correlation between the tasks.
#' @param alternative A character string specifying the alternative hypothesis,
#'   must be one of \code{"two.sided"} (default), \code{"greater"} or
#'   \code{"less"}. You can specify just the initial letter. Since the direction
#'   of the expected effect depends on which task is set as A and which is set
#'   as B, be very careful if changing this parameter.
#' @param int_level Level of confidence for credible intervals, defaults to 95\%.
#' @param iter Number of iterations, defaults to 10000. Greater number gives better
#'   estimation but takes longer to calculate.
#' @param unstandardised Estimate z-value based on standardised or
#'   unstandardised task scores. Set to \code{TRUE} only if tasks are measured on the
#'   same scale with the same underlying distribution.
#' @param calibrated \code{TRUE} is default. Whether or not to use the standard theory (Jeffreys) prior
#'   distribution (if set to \code{FALSE}) or a calibrated prior examined by
#'   Berger and Sun (2008). The sample estimation of the covariance matrix is
#'   based on the sample size being n - 1 when the calibrated prior is used. See
#'   Crawford et al. (2011) for further information. Calibrated prior is
#'   recommended.
#' @param na.rm Remove \code{NA}s from controls.
#'
#' @return A list with class \code{"htest"} containing the following components:
#'   \tabular{llll}{ \code{statistic}   \tab the mean z-value over \code{iter}
#'   number of iterations. \cr\cr \code{parameter} \tab the degrees of freedom
#'   used to specify the posterior distribution. \cr\cr \code{p.value}    \tab
#'   the mean p-value over \code{iter} number of iterations. \cr\cr
#'   \code{estimate} \tab case scores expressed as z-scores on task A and B.
#'   Standardised effect size (Z-DCC) of task difference between case and
#'   controls and point estimate of the proportion of the control population
#'   estimated to show a more extreme task difference. \cr\cr  \code{null.value}
#'   \tab the value of the difference under the null hypothesis.\cr\cr
#'   \code{alternative}     \tab a character string describing the alternative
#'   hypothesis.\cr\cr \code{method} \tab a character string indicating what
#'   type of test was performed.\cr\cr \code{data.name} \tab a character string
#'   giving the name(s) of the data}
#'
#' @export
#'
#' @examples
#' BSDT(-3.857, -1.875, controls_a = 0, controls_b = 0, sd_a = 1,
#' sd_b = 1, sample_size = 20, r_ab = 0.68, iter = 100)
#'
#' BSDT(case_a = size_weight_illusion[1, "V_SWI"], case_b = size_weight_illusion[1, "K_SWI"],
#'  controls_a = size_weight_illusion[-1, "V_SWI"],
#'  controls_b = size_weight_illusion[-1, "K_SWI"], iter = 100)
#'
#' @references
#' Berger, J. O., & Sun, D. (2008). Objective Priors for the Bivariate Normal
#' Model. \emph{The Annals of Statistics, 36}(2), 963-982. JSTOR.
#'
#' Crawford, J. R., & Garthwaite, P. H. (2007). Comparison of a single case to a
#' control or normative sample in neuropsychology: Development of a Bayesian
#' approach. \emph{Cognitive Neuropsychology, 24}(4), 343-372.
#' \doi{10.1080/02643290701290146}
#'
#' Crawford, J. R., Garthwaite, P. H., & Ryan, K. (2011). Comparing a single
#' case to a control sample: Testing for neuropsychological deficits and
#' dissociations in the presence of covariates. \emph{Cortex, 47}(10),
#' 1166-1178. \doi{10.1016/j.cortex.2011.02.017}
#'
#' Geoffrey Thompson (2019). CholWishart: Cholesky Decomposition of the Wishart
#' Distribution. R package version 1.1.0.
#' \url{https://CRAN.R-project.org/package=CholWishart}




BSDT <- function (case_a, case_b, controls_a, controls_b,
                  sd_a = NULL, sd_b = NULL,
                  sample_size = NULL, r_ab = NULL,
                  alternative = c("two.sided", "greater", "less"),
                  int_level = 0.95,
                  iter = 10000,
                  unstandardised = FALSE,
                  calibrated = TRUE,
                  na.rm = FALSE) {

  ###
  # Set up of error and warning messages
  ###

  case_a <- as.numeric(unlist(case_a))
  controls_a <- as.numeric(unlist(controls_a))

  case_b <- as.numeric(unlist(case_b))
  controls_b <- as.numeric(unlist(controls_b))

  if (length(case_a) > 1 | length(case_b) > 1) stop("Case scores should be single value")
  if (length(controls_a) > 1 & length(controls_b) > 1) {
    if (length(controls_a) != length(controls_b)) stop("Sample sizes must be equal")
  }

  if (length(controls_a) > 1 & length(controls_b) > 1 & is.null(sample_size) == FALSE) message("Value on sample_size will be ignored")

  if (length(controls_a) > 1 & is.null(sd_a) == FALSE) message("Value on sd_a will be ignored")
  if (length(controls_b) > 1 & is.null(sd_b) == FALSE) message("Value on sd_b will be ignored")

  if (length(controls_a) == 1 & is.null(sd_a) == TRUE) stop("Please give sd and n on task A if controls_a is to be treated as mean")
  if (length(controls_b) == 1 & is.null(sd_b) == TRUE) stop("Please give sd and n on task B if controls_b is to be treated as mean")


  # Handling of NA use cases below
  if(is.na(case_a) == TRUE | is.na(case_b) == TRUE) stop("One or both case scores is NA")

  if (na.rm == TRUE) {
    if (sum(is.na(controls_a))  > 0 & sum(is.na(controls_b)) == 0 ) {
      controls_b <- controls_b[!is.na(controls_a)]
      controls_a <- controls_a[!is.na(controls_a)]
      warning("Removal of NAs on controls_a resulted in removal of non-NAs on controls_b")
    }

    if (sum(is.na(controls_b))  > 0 & sum(is.na(controls_a)) == 0 ) {
      controls_a <- controls_a[!is.na(controls_b)]
      controls_b <- controls_b[!is.na(controls_b)]
      warning("Removal of NAs on controls_b resulted in removal of non-NAs on controls_a")
    }

    if (sum(is.na(controls_b))  > 0 & sum(is.na(controls_a)) > 0 ) {

      if (identical(!is.na(controls_a), !is.na(controls_b)) == TRUE) {
        controls_a <- controls_a[!is.na(controls_a)]
        controls_b <- controls_b[!is.na(controls_b)]
      } else {
        conx <- controls_a[!is.na(controls_a) & !is.na(controls_b)]
        cony <- controls_b[!is.na(controls_a) & !is.na(controls_b)]

        controls_a <- conx
        controls_b <- cony

        warning("Removal of NAs on one control sample resulted in removal of non-NAs on the other")
      }

    }

  }
  if (sum(is.na(controls_a)) > 0 | sum(is.na(controls_b)) > 0) stop("Controls contains NA, set na.rm = TRUE to proceed")
  # End of NA use cases


  if (length(controls_a) > 1 & length(controls_b) > 1) {
    if (length(controls_a) != length(controls_b)) stop("Sample sizes must be equal")
  }

  ###
  # Extract relevant statistics and set up further errors
  ###

  alternative <- match.arg(alternative)

  con_m_a <- mean(controls_a) # Mean of the control sample on task x
  con_m_b <- mean(controls_b) # Mean of the control sample on task y

  con_sd_a <- stats::sd(controls_a) # Standard deviation of the control sample on task x
  if (length(controls_a) == 1 & is.null(sd_a) == FALSE) con_sd_a <- sd_a

  con_sd_b <- stats::sd(controls_b) # Standard deviation of the control sample on task y
  if (length(controls_b) == 1 & is.null(sd_b) == FALSE) con_sd_b <- sd_b


  # Since controls x and y need to be of equal length n is the length of any of them
  n <- length(controls_a)
  if (length(controls_a) == 1 | length(controls_b) == 1) {
    if (is.null(sample_size) == TRUE) stop("Please set sample size")
    n <- sample_size
    if (length(controls_a) > 1 & n != length(controls_a)) stop("Sample sizes must be equal")
    if (length(controls_b) > 1 & n != length(controls_b)) stop("Sample sizes must be equal")
  }

  if (is.null(r_ab) == TRUE & length(controls_a) == 1) stop("Please set correlation between tasks")
  if (is.null(r_ab) == TRUE & length(controls_b) == 1) stop("Please set correlation between tasks")

  if (is.null(r_ab) == FALSE){
    if (r_ab < -1 | r_ab > 1) stop("Correlation must be between -1 and 1")
  }

  r <- r_ab
  if (length(controls_a) > 1 & length(controls_b) > 1) r <- stats::cor(controls_a, controls_b)


  if (length(controls_a) > 1 & length(controls_b) > 1) {

    con_mat <- cbind(controls_a, controls_b)
    # Calculate SSCP matrix and call it A as in Crawford Garthwaite-notation - the scale matrix

    A <- (n - 1)* stats::cov(con_mat)

  } else {

    saa <- con_sd_a^2 * (n - 1)
    sbb <- con_sd_b^2 * (n - 1)

    sab <- con_sd_a*con_sd_b* r * (n - 1)

    A <- matrix(c(saa, sab, sab, sbb), nrow = 2)
  }

  ###
  # Below follows sampling for the "standard theory" prior
  ###

  if (calibrated == FALSE) {
    df <- n

    # Drawing a random seed that will be used to generate (inverse) Wishart
    # draws and cholesky decomposed (inverse) Wishart draws (i.e. so the
    # draws are the same but Cholesky decomp has been applied)
    seed <- stats::runif(5)

    # The CholWishart package is used for C++ implemented sampling
    # from the (inverse) Wishart distribution

    withr::with_seed(seed, Sigma_hat <- CholWishart::rInvWishart(iter, n, A))
    # So that both the inverse wishart draws and the cholesky decomp on them are the same

    withr::with_seed(seed, Tchol <- CholWishart::rInvCholWishart(iter, n, A))
    # Simulates same as above but with cholesky decomp in C++

    Tchol <- aperm(Tchol, perm = c(2, 1, 3)) # Transposes each matrix to lower triangual instead of upper

    # Get distributions of means
    Mu_hat <- matrix(nrow = iter, ncol = 2)
    for (i in 1:iter) Mu_hat[i , ] <-  as.numeric(c(con_m_a, con_m_b) + (Tchol[ , , i]%*%stats::rnorm(2))/sqrt(n))


  } else { # i.e if calibrated == TRUE

    ###
    # Below follows the rejection sampling of Sigma
    # for the "calibrated" prior detailed in the vignette
    ###

    A_ast <- ((n - 2)*A) / (n - 1)
    df <- n-2

    step_it <- iter
    Sigma_hat_acc_save <- array(dim = c(2, 2, 1))

    while(dim(Sigma_hat_acc_save)[3] < iter + 1) {

      Sigma_hat <- CholWishart::rInvWishart(step_it, df = n - 2, A_ast)

      rho_hat_pass <- Sigma_hat[1, 2, ] / sqrt(Sigma_hat[1, 1, ] * Sigma_hat[2, 2, ])

      u <- stats::runif(step_it, min = 0, max = 1)

      Sigma_hat_acc <- array(Sigma_hat[ , , (u^2 <= (1 - rho_hat_pass^2))],
                             dim = c(2, 2, sum(u^2 <= (1 - rho_hat_pass^2))))

      Sigma_hat_acc_save <- array(c(Sigma_hat_acc_save, Sigma_hat_acc), # Bind the arrays together
                                  dim = c(2, 2, (dim(Sigma_hat_acc_save)[3] + dim(Sigma_hat_acc)[3])))

      step_it <- iter - dim(Sigma_hat_acc_save)[3] + 1

    }

    Sigma_hat <- Sigma_hat_acc_save[ , , -1] # Remove the first matrix that is filled with NA
    rm(Sigma_hat_acc_save, step_it, u, Sigma_hat_acc, rho_hat_pass) # Flush all variables not needed

    # Perform Cholesky decomposition on the accepted Covariance matrices
    Tchol <- array(dim = c(2, 2, iter))
    for (i in 1:iter) Tchol[ , , i] <- t(chol(Sigma_hat[ , , i]))

    # Get distributions of means
    Mu_hat <- matrix(nrow = iter, ncol = 2)
    for (i in 1:iter) Mu_hat[i , ] <-  as.numeric(c(con_m_a, con_m_b) + (Tchol[ , , i]%*%stats::rnorm(2))/sqrt(n))

  }


  if (unstandardised == FALSE) {

    ###
    # Get standardised effect measures distribution
    ###

    zx <- (case_a - Mu_hat[ , 1]) / sqrt(Sigma_hat[1, 1, ])
    zy <- (case_b - Mu_hat[ , 2]) / sqrt(Sigma_hat[2, 2, ])

    rho_hat <- Sigma_hat[1, 2, ] / sqrt(Sigma_hat[1, 1, ] * Sigma_hat[2, 2, ])

    z_ast <- (zx - zy) / sqrt(2 - 2*rho_hat)

  } else {

    ###
    # Get unstandardised effects distribution
    ###

    std.err <- sqrt(Sigma_hat[1, 1, ] + Sigma_hat[2, 2, ] - 2*Sigma_hat[1, 2, ])

    z_ast <- ((case_a - Mu_hat[ , 1]) - (case_b - Mu_hat[ , 2])) / std.err

  }

  ###
  # Get distribution of p-values
  ###

  if (alternative == "two.sided") {
    pval <- 2 * stats::pnorm(abs(z_ast), lower.tail = FALSE)
  } else if (alternative == "greater") {
    pval <- stats::pnorm(z_ast, lower.tail = FALSE)
  } else { # I.e. if alternative == "less"
    pval <- stats::pnorm(z_ast, lower.tail = TRUE)
  }

  ###
  # Get credible intervals for effect and p-estimate
  ###
  alpha <- 1 - int_level

  zdcc_int <- stats::quantile(z_ast, c(alpha/2, (1 - alpha/2)))
  names(zdcc_int) <- c("Lower Z-DCC CI", "Upper Z-DCC CI")

  p_est <- mean(pval)

  p_int <- stats::quantile(pval, c(alpha/2, (1 - alpha/2)))*100
  if (alternative == "two.sided") p_int <- stats::quantile(pval/2, c(alpha/2, (1 - alpha/2)))*100
  names(p_int) <- c("Lower p CI", "Upper p CI")

  ###
  # Get point estimates of effects
  ###

  std_a <- (case_a - con_m_a)/con_sd_a
  std_b <- (case_b - con_m_b)/con_sd_b

  zdcc <- (std_a - std_b) / sqrt(2 - 2*r) # Estimated effect size

  estimate <- c(std_a, std_b, zdcc, ifelse(alternative == "two.sided", (p_est/2*100), p_est*100))

  ###
  # Set names for the estimates
  ###

  if (alternative == "two.sided") {
    if (zdcc < 0) {
      alt.p.name <- "Proportion below case (%), "
    } else {
      alt.p.name <- "Proportion above case (%), "
    }
  } else if (alternative == "greater") {
    alt.p.name <- "Proportion above case (%), "
  } else {
    alt.p.name <- "Proportion below case (%), "
  }

  p.name <- paste0(alt.p.name,
                   100*int_level, "% CI [",
                   format(round(p_int[1], 2), nsmall = 2),", ",
                   format(round(p_int[2], 2), nsmall = 2),"]")

  zdcc.name <- paste0("Std. discrepancy (Z-DCC), ",
                     100*int_level, "% CI [",
                     format(round(zdcc_int[1], 2), nsmall = 2),", ",
                     format(round(zdcc_int[2], 2), nsmall = 2),"]")

  names(estimate) <- c("Std. case score, task A (Z-CC)",
                       "Std. case score, task B (Z-CC)",
                       zdcc.name,
                       p.name)

  ###
  # Set names for the interval stored in the output object
  ###

  typ.int <- 100*int_level
  names(typ.int) <- "Credible (%)"
  interval <- c(typ.int, zdcc_int, p_int)

  ###
  # Set names for objects in output
  ###

  names(df) <- "df"
  null.value <- 0 # Null hypothesis: difference = 0
  names(null.value) <- "difference between tasks"
  names(con_m_a) <- "Mean (controls)"
  names(con_sd_a) <- "SD (controls)"
  names(con_m_b) <- "Mean (controls)"
  names(con_sd_b) <- "SD (controls)"
  names(n) <- "Sample size"
  dname <- paste0("Case A: ", format(round(case_a, 2), nsmall = 2), ", ",
                  "B: ", format(round(case_b, 2), nsmall = 2), ", ",
                  "Ctrl. A (m, sd): (", format(round(con_m_a, 2), nsmall = 2), ",",format(round(con_sd_a, 2), nsmall = 2), "), ",
                  "B: (", format(round(con_m_b, 2), nsmall = 2), ",",format(round(con_sd_b, 2), nsmall = 2), ")")

  # Build output to be able to set class as "htest" object for S3 methods.
  # See documentation for "htest" class for more info
  output <- list(parameter = df,
                 p.value = p_est,
                 estimate = estimate,
                 null.value = null.value,
                 interval = interval,
                 desc = c(con_m_a, con_sd_a, con_m_b, con_sd_b, n),
                 alternative = alternative,
                 method = paste("Bayesian Standardised Difference Test"),
                 data.name = dname)

  class(output) <- "htest"
  output


}

