library(survminer)
library(dplyr)
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

.fit_distribution <- function(distribution, data){
  fit <- flexsurvreg(
    Surv(time, status) ~ 1,
    dist = distribution,
    data = data
  )
}

fit_distribution <- function(distributions, data){
  df <- data.frame(Distributions = distributions)
  df <- df %>% 
    mutate(Model = purrr::map( 
      Distributions,
      .fit_distribution,
      data
    ))
}

plot_fitted_distribution <- function(fit, km){
  df <- data.frame(time = summary(fit)[[1]]$time, km = km$surv, fitted = summary(fit)[[1]]$est)
  p <- ggplot(data = df) +
    geom_line(aes(x = time, y = km)) +
    geom_line(aes(x = time, y = fitted), color = "#7EBE91")
  p
  
}

dists <- c("exponential")
A <- km_estimates(IPD_GemOS_Colluci)
B <- fit_distribution(distributions = dists, IPD_GemOS_Colluci)
C <- plot_fitted_distribution(B, A)
# GAMLSS