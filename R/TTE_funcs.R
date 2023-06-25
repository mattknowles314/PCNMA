km_estimates <- function(TTE){
  out <- ggsurvfit::survfit2(
    survival::Surv(
      time = time, 
      event = status,
      type = "right"
    ) ~ 1, data = TTE)
}

plot_km <- function(fit, break.x.by = 7){
  if (!"survfit" %in% class(fit)) {
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
 
km_estimates(IPD_GemOS_Colluci) %>% 
  plot_km()
