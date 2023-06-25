library(survminer)
library(flexsurv)

km_estimates <- function(TTE){
  out <- ggsurvfit::survfit2(
    survival::Surv(
      time = time, 
      event = status,
      type = "right"
    ) ~ 1, data = TTE)
}

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


fit_distribution <- function(distributions, data){
  df <- data.frame(Distributions = distributions)
  df <- df %>% 
    mutate(Model = purrr::map( 
      Distributions,
      function(.x) {
        flexsurvreg(
          Surv(time, status) ~ 1,
          dist = .x,
          data = data)
      }
    ))
  
}

plot_fitted_distribution <- function(fit){
  if (!inherits(fit, "flexsurvreg")) {
    rlang::abort("fit object is not of class 'flexsurvreg'")
  }
  
  
}

dists <- c("exponential")

B <- fit_distribution(distributions = dists, IPD_GemOS_Colluci)

# GAMLSS