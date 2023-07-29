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
#' @param fit A [PCNMA::fitted_distribtuion] object
#' @param CI Include a confidence interval?
#' @param km Add the original KM curve?
#' @param km a dataframe with a surv column
#' @param ... For S3 consistency
#'
#' @export
plot.fitted_distribution <-  function(fit, CI = FALSE, km = FALSE, ...){
  df <- as.data.frame(fit[["Model_Data"]])
  names(df) <- c("time", "survival", "survival.lcl", "survival.ucl")
  
  # Base plot, just the model
  p <- ggplot(df, aes(x = time, y = survival)) 
  
  if (CI) {
    p <- p +
      geom_ribbon(aes(x = time, ymin = survival.lcl, ymax = survival.ucl),
                  fill = "grey70")
  }
  
  # Add the model line and final cosmetic changes
  p <- p + 
    geom_line() +
    labs(x = "Time",
         y = "Survival")
  
  p
}


