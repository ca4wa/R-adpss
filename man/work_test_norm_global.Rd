% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/work_test_norm_global.R
\name{work_test_norm_global}
\alias{work_test_norm_global}
\title{Construct a working test and implement an interim or the final analysis for a globally efficient adaptive design.}
\usage{
work_test_norm_global(
  overall_sig_level = 0.025,
  work_beta = 0.05,
  cond_alpha = overall_sig_level,
  cost_type_1_err = 0,
  cost_type_2_err = 0,
  prev_cost = 0,
  min_effect_size = 1,
  effect_size = 0,
  basic_schedule_num = 50,
  basic_schedule_power = 2,
  basic_schedule = 0,
  prior_dist = 0,
  prev_time = 0,
  time = 0,
  next_time = 0,
  stat = 0,
  input_check = TRUE,
  out_process = FALSE,
  simpson_div = 6,
  tol_boundary = 1e-08,
  tol_cost = 1e-08
)
}
\arguments{
\item{overall_sig_level}{Overall significance level in (0, 1). Default is 0.025.}

\item{work_beta}{Type II error probability of the working test. Default is 0.05.}

\item{cond_alpha}{Conditional Type I error probability in (0, 1). Default is 0.025.}

\item{cost_type_1_err}{The loss caused by erroneously rejecting the null hypothesis. If 0 is specified, the loss leading to the working test with its Type I error probability being \code{significance_level} will be calculated.  Default is 0.}

\item{cost_type_2_err}{The loss caused by erroneously accepting the null hypothesis. If 0 is specified, the loss will be set to the value of the maximum of the basic analysis schedule.  Default is 0.}

\item{prev_cost}{The value of \code{cost_type_1_err} of the working test in the analysis just before the current analysis.}

\item{min_effect_size}{The minimum effect size.  It should be positive. The working test will be constructed to have the power of \code{1 - work_beta} for this effect size.  Default is 1.}

\item{effect_size}{The effect size for which the probability of rejecting the null hypothesis will be calculated. If \code{cost_type_1_err = 0}, \code{effect_size} will be forced to be the null value, 0.  Default is 0.}

\item{basic_schedule_num}{The number of analysis of the working test. Default is 50.}

\item{basic_schedule_power}{Determine the intervals between analyses. Default is 2.}

\item{basic_schedule}{The basic analysis schedule arbitrarily specified by user.}

\item{prior_dist}{Prior distribution for effect sizes of \code{min_effect_size * 0:10 / 2}.}

\item{prev_time}{The time of the analysis just before the current analysis. Either \code{prev_time} or \code{next_time} should be 0. See the example below.}

\item{time}{The time of the current analysis.}

\item{next_time}{The time of the next analysis. Either \code{prev_time} or \code{next_time} should be 0. See the example below.}

\item{stat}{The value of the current test statistic. The value of \code{stat} should be 0 at \code{time = 0}.}

\item{input_check}{Indicate whether or not the arguments input by user contain invalid values.}

\item{out_process}{The values used in calculation will be output in addition to the main output. Default is FALSE.}

\item{simpson_div}{The precision determining the precision of numerical integration. The default value is 6.}

\item{tol_boundary}{The precision in calculation of the stopping boundary of the working test.}

\item{tol_cost}{The precision in calculation of the loss, \code{cost_type_1_error}.}
}
\value{
List of values of the parameters specified, information of the working test, and the conditional probability of rejecting the null hypothesis.
}
\description{
This function is used as a routine by \code{\link{adaptive_analysis_norm_global}} and \code{\link{sample_size_norm_global}}.
}
\seealso{
\code{\link{adaptive_analysis_norm_global}} for example of this function.
}
