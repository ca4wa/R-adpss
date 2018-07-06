#' Analyze data according to a locally efficient adaptive design.
#' 
#' \code{adaptive_analysis_norm_local} performs an locally efficient adaptive test, 
#' a Frequentist adaptive test with the specified significance level
#' with full flexibility.
#' Normality with known variance is assumed for the test statistic
#' (more accurately, the test statistic is assumed to follow Brownian motion.)
#' Null hypothesis is fixed at 0 without loss of generality.
#' No procedure to calculate p-value or confidence intervals is employed.
#' 
#' @param overall_sig_level Overall significance level in (0, 1). Default is 0.025.
#' @param min_effect_size The minimum effect size.  It should be positive. The working test will be constructed to have the power of \code{1 - work_beta} for this effect size.  Default is 1.
#' @param times The sequence of times (sample size or information level) at which analyses were conducted.
#' @param stats The sequence of test statistics.
#' @param final_analysis If \code{TRUE}, the result input will be regarded as complete (no more data will be obtained) and the significance level will be exhausted. If \code{FALSE}, the current analysis will be regarded as an interim analysis and the significance level will be preserved.
#' @param input_check Indicate whether or not the arguments input by user contain invalid values.
#' @return List of results including the conditional Type I error probability.
#' @references
#' Kashiwabara, K., Matsuyama, Y. An efficient adaptive design approximating fixed sample size designs. In preparation.
#' @examples
#' # Sample size calculation
#' sample_size_norm_local(
#'   overall_sig_level = 0.025,
#'   min_effect_size = -log(0.65),
#'   effect_size = 11.11 / 20.02, # needs not be MLE
#'   time = 20.02,
#'   target_power = 0.75,
#'   sample_size = TRUE
#'   )
#' 
#' # Final analysis
#' final_analysis <- adaptive_analysis_norm_local(
#'   overall_sig_level = 0.025,
#'   min_effect_size = -log(0.65),
#'   times = c(9.18, 20.02, 24.86),
#'   stats = c(4.35, 11.11, 14.84),
#'   final_analysis = TRUE
#'   )
#' print( with(final_analysis, data.frame(analysis=0:par$analyses, time=par$times,
#'   intercept=char$intercept, stat=par$stats, boundary=char$boundary,
#'   pr_cond_err=char$cond_type_I_err, reject_H0=char$rej_H0)) )
#' @seealso
#' \code{\link{sample_size_norm_local}}.
#' @importFrom stats dnorm pnorm qnorm
#' @export
adaptive_analysis_norm_local <- function(
  overall_sig_level = 0.025,
  min_effect_size = 1,
  times = 0,
  stats = 0,
  final_analysis = TRUE,
  input_check = TRUE
  ) {

  if ( length(input_check) > 1 ) stop("'input_check' should be scalar.")
  if ( input_check ) {
    if ( length(overall_sig_level) != 1 ) stop("'overall_sig_level' should be scalar.")
    if ( length(min_effect_size) != 1 ) stop("'min_effect_size' should be scalar.")
    if ( length(final_analysis) != 1 ) stop("'final_analysis' should be scalar.")

    if ( min_effect_size < 0 ) stop("'min_effect_size' should be positive.")

    if ( length(times) != length(stats) ) stop("'times' and 'stats' should have the same length.")
    if ( any(times < 0) ) stop("All values of 'times' should be non-negative.")
    if ( any(diff(times) <= 0) ) stop("All intervals of 'times' should be positive.")
  }

  #%%%%%%%% DESIGNATION %%%%%%%%#
  #=== FOR ADAPTIVE TEST ===#
  ### Trial Settings ###
  # Significance Level (One-Sided) #
  alpha.0 <- overall_sig_level
  # Rho: The Minimal Effect Size Which Should Be Detected #
  rho <- min_effect_size

  ### Current Data ###
  # Information time sequence: S_k = {t_k: k = 1, 2, ...} #
  S.k <- times
  # Test statistic: x(S_k) = {x(t_k): k = 1, 2, ...} #
  x.S.k <- stats
  # Is the latest analysis the final ? TRUE or FALSE #
  fin <- final_analysis


  #%%%%%%%% ADAPTIVE TEST %%%%%%%%#
  #=== ANALYSIS ===#
  ### Data ###
  S.k <- sort(unique(c(0, S.k)))  # information level (time)
  x.S.k <- sort(unique(c(0, x.S.k)))  # score
  ana.num <- length(x.S.k)

  ### Initialize ###
  # z_alpha #
  za <- -qnorm(alpha.0)
  # Initial Working Design #
  xi <- -log(alpha.0) / rho
  # History #
  alp.k <- alpha.0
  H.alp.k <- alpha.0
  H.xi <- xi
  H.rho <- rho
  H.b.k <- xi

  ### Interim Analyses ###
  kk <- 1  # Current stage
  while( 1 ){
    #print( paste("%%%%%%%% Analysis for Stage :", kk, "%%%%%%%%") )
    t.k <- S.k[kk + 1]

    ### Working design update ###
    # rho #
    rho <- rho
      H.rho[kk + 1] <- rho
    # xi: Bisection method #
    d.t.k <- t.k - S.k[kk]
    cond.xi <- -log(alp.k) / rho
    xi.mod <- cond.xi * 20
    sgn <- -1
    while( (abs(xi.mod) > 1e-8 / 2) || (sgn > 0)  ){
      xi.mod <- xi.mod/2
      cond.xi <- cond.xi + sgn * xi.mod
      sq.d.t.k <- sqrt(d.t.k)
      r.d.t.k <- pnorm(-(cond.xi + rho * d.t.k / 2) / sq.d.t.k)
      alp.d.t.k <- exp(-cond.xi * rho + 
                       log(pnorm((cond.xi - rho * d.t.k / 2) / sq.d.t.k)))
      sgn <- sign(r.d.t.k + alp.d.t.k - alp.k)
    }
    xi <- cond.xi + x.S.k[kk] - 1/2 * rho * S.k[kk]
    H.xi[kk + 1] <- xi

    ### Interim analysis ###
    b.k <- xi + 1/2 * rho * t.k
      H.b.k[kk + 1] <- b.k
    x.k <- x.S.k[kk + 1]
    alp.k <- exp(- rho * (b.k - x.k))
    H.alp.k[kk + 1] <- min(1, alp.k)

    ### Decision ###
    if( alp.k >= 1 ){
      #print( "INTERIM STOP" )
      break;
    } else if( kk >= (ana.num - 1 - fin) ) {
      #print( "CONTINUE" )
      break;
    } else {
      #print( "CONTINUE" )
    }

    ### Next stage ###
    kk <- kk + 1
  }

  ### Final Analysis ###
  if( alp.k < 1 && fin )
  {
    kk <- kk + 1
    #print( paste("%%%%%%%% Analysis for Stage :", kk, "(FINAL) %%%%%%%%") )
    t.k <- S.k[kk + 1]

    ### Dummy ###
    H.rho[kk + 1] <- NA
    H.xi[kk + 1] <- NA

    ### Final analysis ###
    b.k <- x.S.k[kk] - qnorm(alp.k) * sqrt(t.k - S.k[kk])
      H.b.k[kk + 1] <- b.k
    x.k <- x.S.k[kk + 1]
    alp.k <- (x.k >= b.k) * 1
      H.alp.k[kk + 1] <- min(1, alp.k)
  } else if( ana.num > (kk + 1) ){
    ### Dummy ###
    H.rho[ana.num] <- NA
    H.xi[ana.num] <- NA
    H.alp.k[ana.num] <- NA
    H.b.k[ana.num] <- NA
  }

  ### Processing for Display ###
  kk <- 0:(ana.num - 1)
  rej.H0 <- (H.alp.k >= 1)
  final <- (kk==(ana.num - fin))


  dpar <- list(
    overall_sig_level = overall_sig_level,
    min_effect_size = min_effect_size,
    analyses = ana.num - 1,
    times = S.k,
    stats = x.S.k,
    final_analysis = final_analysis
    )

  dchar <- list(
    intercept = H.xi,
    boundary = H.b.k,
    cond_type_I_err = H.alp.k,
    rej_H0 = rej.H0
    )

  res <- list(par = dpar, char = dchar)
  return( res )
}

#' Calculate sample size or power for a locally efficient adaptive design.
#'
#' \code{sample_size_norm_local} calculates the power if the time of the final
#' analysis is given and otherwise the sample size.
#' The computed power for \code{effect_size} is an approximate lower bound.
#' Sample size is also calculated on the basis of the bound.
#' 
#' @param overall_sig_level Overall significance level in (0, 1). Default is 0.025.
#' @param min_effect_size The minimum effect size.  It should be positive. The working test will be constructed to have the power of \code{1 - work_beta} for this effect size.  Default is 1.
#' @param sample_size If \code{TRUE}, the function will return the sample size required by the locally efficient adaptive design to have the power of \code{target_power}. If \code{FALSE}, the function will return the power when the final interim analysis and the final analysis are conducted at \code{time} and \code{final_time}, respectively.
#' @param effect_size The effect size, on the basis of which the power or sample size calculation will be performed. In locally efficient adaptive designs, any real value no less than \code{min_effect_size / 2} is allowed.
#' @param time The time of the current analysis.
#' @param target_power The power, on the basis of which the sample size calculation will be performed.
#' @param final_time The time of the final analysis.
#' @param tol_sample_size The precision in calculation of the sample size.
#' @param input_check Indicate whether or not the arguments input by user contain invalid values.
#' @return It returns the sample size (when \code{sample_size = TRUE}) or the power (when \code{sample_size = FALSE}).
#' @seealso
#' \code{\link{adaptive_analysis_norm_local}} for example of this function.
#' @export
sample_size_norm_local <- function(
  overall_sig_level = 0.025,
  min_effect_size = 1,
  sample_size = TRUE,
  effect_size = 1,
  time = 0,
  target_power = 0.8,
  final_time = 0,
  tol_sample_size = 1e-8,
  input_check = TRUE
  ) {

  if ( length(input_check) > 1 ) stop("'input_check' should be scalar.")
  if ( input_check ) {
    if ( length(overall_sig_level) != 1 ) stop("'overall_sig_level' should be scalar.")
    if ( length(min_effect_size) != 1 ) stop("'min_effect_size' should be scalar.")
    if ( length(sample_size) != 1 ) stop("'sample_size' should be scalar.")
    if ( length(effect_size) != 1 ) stop("'effect_size' should be scalar.")
    if ( length(time) != 1 ) stop("'time' should be scalar.")
    if ( length(target_power) != 1 ) stop("'target_power' should be scalar.")
    if ( length(final_time) != 1 ) stop("'final_time' should be scalar.")

    if ( overall_sig_level <= 0 || overall_sig_level >= 1 ) stop("'overall_sig_level' should be a value in (0, 1).")
    if ( min_effect_size < 0 ) stop("'min_effect_size' should be positive.")
    if ( time < 0 ) stop("'time' should be non-negative.")
    if ( final_time < 0 ) stop("'final_time' should be non-negative.")
    if ( target_power <= 0 || target_power >= 1 ) stop("'target_power' should be a value in (0, 1).")
    if ( tol_sample_size <= 0 ) stop("'tol_sample_size' should be positive.")

    if ( (!sample_size) && (time > final_time) ) warning("Because 'sample_size' is FLASE but 'final_time' is omitted or less than 'time', 'time' was substituted into 'final_time'.")
    if ( sample_size && (effect_size <= 0) ) stop("When 'sample_size' is TRUE, 'effect_size' should be positive.")
  }
  if ( (!sample_size) && (time > final_time) ) final_time <- time

  #%%%%%%%% DESIGNATION %%%%%%%%#
  #=== FOR ADAPTIVE TEST ===#
  ### Trial Settings ###
  # Significance Level (One-Sided) #
  alpha.0 <- overall_sig_level
  # Rho: The Minimal Effect Size Which Should Be Detected #
  rho <- min_effect_size
  #=== FOR ADAPTIVE SAMPLE SIZE DETERMINATION ===#
  ### Hypothesis mu_1 ###
  mu.1 <- effect_size
  ### Type II Error Probability ###
  if ( target_power == 0 ) target_power <- 0.8
  beta_0 <- 1 - target_power

  #%%%%%%%% ADAPTIVE SAMPLE SIZE DETERMINATION %%%%%%%%#
  # The time of the final interim analysis: t = m
  # The time of the final analysis: t = n

  #=== COMPUTATION ===#
  ### Initialize ###
  # z_alpha #
  za <- -qnorm(alpha.0)

  ### Grid Points (Jennison & Turnbull (2000), chap.19) ###
  rr <- 128
  zdev1 <- -3 - 4 * log(rr / (1:(rr - 1)))
  zdev2 <- -3 + 3 * (0:(4*rr)) / (2 * rr)
  zdev <- c(zdev1, zdev2, -rev(zdev1))
  zdev.l <- rr * 6 - 1

  ### Fixed sample size ###
  FSS <- (za - qnorm(beta_0))^2 / mu.1^2

  ### Rejection Probability until t = m (r_m.0) ###
  xi.0 <- -log(alpha.0) / rho
  mu.1.sym <- mu.1 - 1/2 * rho
  mm <- time
  sq.mm <- sqrt(mm)
  r_m.0 <- pnorm(-(xi.0 - mu.1.sym * mm) / sq.mm) +
           exp(2 * xi.0 * mu.1.sym) * pnorm((-xi.0 - mu.1.sym * mm) / sq.mm)
  if ( (!sample_size) && (time == final_time) ) return( r_m.0 )

  # Check of overpowering #
  overpower <- sign(r_m.0 - (1 - beta_0))
  nn <- mm
  r.m.n <- r_m.0

  ### Find t = N if underpowered ###
  if( overpower < 0 ){
  nn <- FSS
  nn.mod <- FSS * 10
  sgn <- 1
  iter <- 0
  max_iter <- log2(nn.mod) - log2(tol_sample_size / 2) + 2
  if ( (!sample_size) && (time < final_time) ) {
    nn <- final_time
    nn.mod <- tol_sample_size / 2 + tol_sample_size / 4
    sgn <- 0
    max_iter <- 1
  }
  while( ((abs(nn.mod) > (tol_sample_size / 2)) || (sgn > 0)) && iter < max_iter ){
    nn.mod <- nn.mod/2
    nn <- nn + sgn * nn.mod

    ### Rejection Probability of Final Analysis at t = n (r.m_n.m) ###
    nn_mm <- nn - mm
    # Grid points #
    b.m <- xi.0 + (1/2 * rho) * mm
    w.m.dev <- zdev * sq.mm + mu.1 * mm
    w.m.sel <- (w.m.dev < b.m)
    w.m.o <- c(w.m.dev[w.m.sel], b.m)

    # Weight by Simpson's rule #
    w.m.odd <- w.m.o
    w.m.l <- length(w.m.odd)
    w.m.d <- diff(w.m.odd)
    w.m <- c(w.m.odd, w.m.odd[-w.m.l] + w.m.d / 2)
    ww <- c((c(w.m.d, 0) + c(0, w.m.d)), w.m.d * 4) / 6

    # Conditional probabilities #
    r.m_w.m <- pmin(1, exp(-2 * xi.0 * (xi.0 + (1/2 * rho) * mm - w.m) / mm))
    phi_w.m <- dnorm(w.m, mu.1 * mm, sq.mm)
    pr_w.m <- phi_w.m * (1 - r.m_w.m)
    w.m.xi <- b.m - w.m
    a.m_w.m <- pmin(exp(-rho * w.m.xi), 1)
    b.n_w.m <- -qnorm(a.m_w.m) * sqrt(nn_mm)
    r.m_n.m.w.m <- pnorm(-(b.n_w.m - mu.1 * nn_mm) / sqrt(nn_mm))

    # Numerical integration #
    r.m_n.m <- sum(pr_w.m * r.m_n.m.w.m * ww)

    # Marginal power (r.m.n) #
    r.m.n <- r_m.0 + r.m_n.m

    ### Update direction ###
    sgn <- sign(1 - beta_0 - r.m.n)
    nn.mod <- nn.mod + nn.mod * (abs(nn.mod) <= (tol_sample_size / 4)) 
    iter <- iter + 1
  }}
  #if ( sample_size && (iter >= max_iter) ) stop("No solution of sample size was found.")

  if ( sample_size ) return( nn )
  if ( !sample_size ) return( r.m.n )
}



