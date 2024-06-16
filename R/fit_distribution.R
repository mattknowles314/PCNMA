#' Fit a single survival distribution
#' 
#' This function is a wrapper on `flexsurv::flexsurvreg`, and fits a single distribution to the specified data.
#' 
#' @param distribution A single distribution
#' @param data An IPD dataset
#' @param strata Strata for the RHS of the `survival::Surv` function
#' 
#' @returns A [flexsurv::flexsurvreg] object
.fit_distribution <- function(distribution, data, strata = "Treatment"){
  fit <- flexsurv::flexsurvreg(
    formula = .gen_surv_formula2(strata),
    dist = distribution,
    data = data
  )
  fit
}

#' Fit survival distributions to a dataset.
#' 
#' This function extends the `PCNMA:::.fit_distribution` function, by fitting a given set of distributions to a TTE dataset.
#'
#' @param distributions A list of distributions
#' @param data An IPD dataset
#' @param strata Stratification variables
#' @param maxT maximum time to calculate fitted values at  
#'  
#' @returns A dataframe with fitted values
#'
#' @export
fit_distribution <- function(distributions = nice_parametric_dists, data, strata = "Treatment", maxT = 60) {
  df <- tidyr::tibble(Distribution = names(distributions), Data = list(data)) |> 
    dplyr::mutate(Model = purrr::map2( 
      distributions[Distribution],
      Data,
      .fit_distribution,
      strata
    )) |> 
    dplyr::group_by(Distribution) |> 
    dplyr::mutate(Model_Data = purrr::map(
      Model,
      flexsurv:::summary.flexsurvreg,
      tidy = TRUE,
      t = seq(0, maxT, 0.1)
    )) 
  class(df) <- c("fitted_distribution", class(df))
  df
}

#' Plot a fitted distributions object
#'
#' Creates a plot for a result of `PCNMA::fit_distribution`.
#'
#' @param fit A `PCNMA::fitted_distribution` object
#' @param CI Include a confidence interval?
#' @param km Add the original KM curve?
#' @param ... For S3 consistency
#'
#' @export
#' 
plot.fitted_distribution <- function(fit, 
                                      CI = FALSE, 
                                      km = FALSE, 
                                      km_alpha = 1,
                                      linewidth = 0.75, 
                                      linetype = "dashed",
                                      theme = "bw",
                                      facet_by = "Treatment",
                                      ...){
  if (!inherits(fit, "fitted_distribution")) {
    rlang::abort("`fit` must be of class fitted_distribution")
  }
  
  df <- fit |> tidyr::unnest(Model_Data) |> 
    select(-c(Data, Model))
  # if (!("Treatment" %in% colnames(df))) {
  #   # This currently doesn't work for strata = 1.
  #   df <- df |> dplyr::rename(Treatment = `Data$Treatment`)
  # } 
  # df <- df |> dplyr::mutate(across(Treatment, as.factor))
  
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = est, colour = Distribution), 
                       linewidth = linewidth, linetype = linetype) +
    ggplot2::labs(x = "Time",
                  y = "Survival") +
    ggplot2::facet_grid(facet_by)
  
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
    IPD <- bind_rows(fit$Data[[1]], fit$Data[[1]]) # This is hardcoded right now, but shouldnt be!
    survfit <- ggsurvfit::survfit2(.gen_surv_formula("Treatment"), data = IPD)
    df2 <- survminer::surv_summary(survfit, data = IPD) |> 
      dplyr::select(time, surv, Treatment)
    p <- p +
      ggplot2::geom_step(data = df2, ggplot2::aes(x = time, y = surv), alpha = km_alpha)
  }
  
  if (theme == "bw") {
    p <- p + theme_bw()
  }
  
  p
}

#' Summary of a set of fitted models
#' 
#' @param fit A `PCNMA::fitted_distributions` object.
#' @param AIC Returns the AIC scores for a set of models
#' @param median Returns a table of median estimates for a set of models
#' 
#' @export
#' 
summary.fitted_distribution <- function(fit, AIC = FALSE, median = FALSE) {
  if (AIC) {
    df <- fit |> 
      dplyr::select(c(Distribution, Model)) |> 
      dplyr::mutate(AIC = purrr::map_dbl(
        Model,
        .get_attribute,
        "AIC"
      )) |> 
      tidyr::unnest(AIC) |> 
      select(-c(Model)) 
  }
  
  if (median) {
    df <- fit |> 
      dplyr::select(-c(Data, Model_Data)) |> 
      dplyr::mutate(Median = purrr::map(
        Model,
        flexsurv:::summary.flexsurvreg,
        type = "median",
        tidy = TRUE)) |> 
      tidyr::unnest(Median) |> 
      dplyr::select(-c(Model)) |> 
      dplyr::rename(Median = est) |> 
      dplyr::rename(L95 = lcl) |> 
      dplyr::rename(U95 = ucl)
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
coef.fitted_distribution <- function(fit, studies, ...){
  coefList <- purrr::map(fit$Model, .get_attribute, "coefficients")
  df <- stack(coefList)
  names(df) <- c("Value", "Distribution")
  #df <- df |> 
  #  dplyr::mutate(term = nice_parametric_paramlist) |> 
  #  dplyr::select(Distribution, term, Value)
  
  df
}

#' Hazard ratios of a fitted model
#'
#' @param fit A `PCNMA::fitted_distributions` object
#'
#'

#' RMST of fitted models
#' 
#' @param fit A [PCNMA::fitted_distribution] objeect,
#' @param x Time to calculate RMST for
#' @param ... For S3 consistency
#' 
#' @export
#' 
rmst <- function(fit, x, ...) {
  d <- fit[["Model_Data"]] |> 
    tibble::enframe() |> 
    tidyr::unnest() |> 
    dplyr::filter(time <= x)
  out <- data.frame("Estimate" = .trapez_rule(d$est),
                    "LCL" = .trapez_rule(d$lcl),
                    "UCL" = .trapez_rule(d$ucl),
                    "Timepoint" = x) 
  out
}


