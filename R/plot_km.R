plot_km <- function(fit, break.x.by = 6){
  if (!inherits(fit, "survfit")) {
    rlang::abort("fit object is not of class 'survfit'")
  }
  ggsurvfit::ggsurvfit(
    fit,
    type = "survival"
  ) + ggsurvfit::add_censor_mark() +
    ggsurvfit::add_risktable(
      risktable_stats = "n.risk"
    ) + ggplot2::scale_x_continuous(breaks = seq(0,max(fit$time),break.x.by))
}