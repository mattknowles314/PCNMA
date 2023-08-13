#' Fit a single distribution
#' 
#' @param distribution A single distribution
#' @param data An IPD dataset
#' 
#' @returns A [flexsurv::flexsurvreg] object
.fit_distribution <- function(distribution, data){
  fit <- flexsurv::flexsurvreg(
    survival::Surv(data$time, data$censored) ~ 1,
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
      flexsurv:::summary.flexsurvreg
    )) |> dplyr::mutate(Model_Data = Model_Data[[1]]) #This is a bit ugly, but it works
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
plot.fitted_distribution <-  function(fit, CI = FALSE, km = FALSE, alpha = 0.5, linewidth = 1, ...){
  df <- fit |> tidyr::unnest(Model_Data) |> 
    dplyr::select(-c(Data, Model))
  
  p <- ggplot2::ggplot(df)
  
  if (CI) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(
        x = time,
        ymin = lcl,
        ymax = ucl,
        fill = Distribution,
      ), alpha = alpha)
  }
  
  if (km) {
    IPD <- fit$Data[[1]]
    survobj <- survival::Surv(IPD$time, IPD$censored)
    survfit <- survminer::surv_fit(survobj ~ 1, data = IPD)
    df2 <- survminer::surv_summary(survfit) |> 
      dplyr::select(time, surv)
    p <- p +
      ggplot2::geom_line(data = df2, 
                         ggplot2::aes(x = time, y = surv))
  }

  p <- p +
    ggplot2::geom_line(ggplot2::aes(x = time, y = est, color = Distribution), 
                       linewidth = linewidth) +
    ggplot2::labs(x = "Time",
         y = "Survival")
  p
}

#' Summary of a set of fitted models
#'
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

coef <- function(fit, ...) UseMethod("coef")
 
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
