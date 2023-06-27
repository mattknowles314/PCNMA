.fit_distribution <- function(distribution, data){
  fit <- flexsurv::flexsurvreg(
    survival::Surv(time, status) ~ 1,
    dist = distribution,
    data = data
  )
}

fit_distribution <- function(distributions, data){
  df <- data.frame(Distributions = distributions)
  df <- df %>% 
    dplyr::mutate(Model = purrr::map( 
      Distributions,
      .fit_distribution,
      data
    ))
}

plot_fitted_distribution <- function(fit, km){
  df <- data.frame(time = flexsurv::summary(fit)[[1]]$time, km = km$surv, fitted = flexsurv::summary(fit)[[1]]$est)
  p <- ggplot2::ggplot(data = df) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = km)) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = fitted), color = "#7EBE91", linewidth = 1.5) +
    ggplot2::labs(x = "Time", y = "Survival")
  p
}