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
  df <- data.frame(Distribution = names(distributions))
  df <- df |>  
    dplyr::mutate(Model = purrr::map( 
      distributions[Distribution],
      .fit_distribution,
      data
    )) |> 
    dplyr::mutate(Model_Data = purrr::map(
      Model,
      summary
    ))
  class(df) <- c("fitted_distribution", class(df))
  df
}

#' Plot a fitted distributions
#'
#' @param model_data The Model_Data column of a [PCNMA::fitted_distribtuion] object
#' @param CI Include a confidence interval?
#' @param km Add the original KM curve?
#' @param km a dataframe with a surv column
#' @param ... For S3 consistency
#'
#' @export
plot.fitted_distribution <-  function(model_data, CI = FALSE, km = NULL, alpha = 1, ...){
  df <- as.data.frame(model_data)
  names(df) <- c("time", "survival", "survival.lcl", "survival.ucl")
  
  # Base plot, just the model
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = survival)) 
  
  if (CI) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = survival.lcl, ymax = survival.ucl),
                  fill = "grey70", alpha = alpha)
  }
  
  if (!is.null(km)) {
    
  }
  
  # Add the model line and final cosmetic changes
  p <- p + 
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time",
         y = "Survival")
  p
}

#' Obtain summary statistics about the models
#'
#'
summary.fitted_distribution <- function(fit, ...){
  df1 <- data.frame(Distribution = fit$Distribution)
  df1 <- df1 |> 
    dplyr::mutate(AIC = purrr::map(
      fit$Model,
      .get_attribute,
      "AIC"
    )) 
  coefs <- purrr::map(
      fit$Model,
      .get_attribute,
      "coefficients"
    )
}
