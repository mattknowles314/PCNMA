#' Fit a single distribution
#' 
#' @param distribution A single distribution
#' @param data An IPD dataset
#' 
#' @returns A [flexsurv::flexsurvreg] object
.fit_distribution <- function(distribution, data){
  fit <- flexsurv::flexsurvreg(
    survival::Surv(data$time, data$status) ~ data$Treatment,
    dist = distribution,
    data = data
  )
  fit
}

#' Fit distributions to a dataset
#'
#' @param distributions A list of distributions
#' @param data An IPD dataset
#' 
#' @returns A dataframe with fitted values
#'
#' @export
fit_distribution <- function(distributions, data){
  df <- tidyr::tibble(Distribution = names(distributions), Data = list(data)) |> 
    dplyr::mutate(Model = purrr::map2( 
      distributions[Distribution],
      Data,
      .fit_distribution
    )) |> 
    dplyr::group_by(Distribution) |> 
    dplyr::mutate(Model_Data = purrr::map(
      Model,
      flexsurv:::summary.flexsurvreg,
      tidy = TRUE
    )) 
  class(df) <- c("fitted_distribution", class(df))
  df
}

#' Plot a fitted distributions object
#'
#' @param fit A [PCNMA::fitted_distribution] object
#' @param CI Include a confidence interval?
#' @param km Add the original KM curve?
#' @param ... For S3 consistency
#'
#' @export
#' 
plot.fitted_distribution <- function(fit, 
                                      CI = FALSE, 
                                      km = FALSE, 
                                      alpha = 0.5, 
                                      linewidth = 1, 
                                      ...){
  if (!inherits(fit, "fitted_distribution")) {
    rlang::abort("`fit` must be of class fitted_distribution")
  }
  
  
  df <- fit |> tidyr::unnest(Model_Data) |> 
    dplyr::select(-c(Data, Model)) |> 
    dplyr::rename(Treatment = `data$Treatment`) |> 
    dplyr::mutate(across(Treatment, as.factor))
  
  p <- ggplot2::ggplot(df)
  
  if (CI) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(
        x = time,
        ymin = lcl,
        ymax = ucl,
        fill = Treatment,
      ), alpha = alpha)
  }
  
  if (km) {
    IPD <- fit$Data[[1]]
    survfit <- ggsurvfit::survfit2(survival::Surv(
      time,
      event = status,
      type = "right"
    ) ~ Treatment, data = IPD)
    df2 <- survminer::surv_summary(survfit, data = IPD) |> 
      dplyr::select(time, surv, Treatment)
    p <- p +
      ggplot2::geom_line(data = df2, 
                         ggplot2::aes(x = time, y = surv))
  }

  p <- p +
    ggplot2::geom_line(ggplot2::aes(x = time, y = est, colour = Distribution), 
                       linewidth = linewidth) +
    ggplot2::labs(x = "Time",
         y = "Survival") +
    ggplot2::facet_grid(~Treatment)
  p
}

#' Summary of a set of fitted models
summary.fitted_distribution <- function(fit, AIC = FALSE){
  df <- tidyr::tibble(Distribution = fit$Distribution)
  
  if (AIC) {
    df <- df |> 
      dplyr::mutate(AIC = purrr::map_dbl(
        fit$Model,
        .get_attribute,
        "AIC"
      ))
  }
  
  df
}
 
#' Coefficients of fitted models
#' 
#' @param fit A [PCNMA::fitted_distribution] object
#' @param ... for S3 consistency
#' 
#' @export
#' 
coef.fitted_distribution <- function(fit, ...){
  coefList <- purrr::map(fit$Model, .get_attribute, "coefficients")
  df <- stack(coefList)
  names(df) <- c("Value", "Distribution")
  df <- df |> 
    dplyr::mutate(term = nice_parametric_paramlist) |> 
    dplyr::select(Distribution, term, Value)
  df
}
