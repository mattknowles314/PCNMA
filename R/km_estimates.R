#' Generate KM estimates
#'
#' Generate Kaplan-Meier estimates from a TTE object. Uses `ggsurvfit::survfit2` to generate KM estimates. Only requires strata to be specified.
#'
#' @param TTE A TTE dataframe
#'
#' @returns A [PCNMA::km_obj] object
#' 
#' @export
km_estimates <- function(TTE, strata = "1"){
  out <- ggsurvfit::survfit2(formula = .gen_surv_formula(strata), data = TTE)
  class(out) <- c("km_obj", class(out))
  out
}

#' Plot a KM curve
#' 
#' @param fit A [PCNMA::km_obj] object
#' @param break.x.by A numeric value for splitting x axis
#' @param xMax The maximum time value to plot
#' @param risktable.height The proportion of the figure to be taken up by the risk table
#' @param ... For S3 consistency
#'
#' @returns A plotted km curve
#' 
#' @export
plot.km_obj <- function(fit, break.x.by = 5, xMax = 40, risktable.height = 0.3, ...){
  if (!inherits(fit, "km_obj")) {
    rlang::abort("fit object is not of class 'km_obj'")
  }
  
  ggsurvfit::ggsurvfit(
    fit,
    type = "survival",
    linetype_aes = "strata"
  ) + 
    ggsurvfit::add_censor_mark() +
    ggsurvfit::add_risktable(
      risktable_stats = "n.risk",
      risktable_group = "strata",
      risktable_height = risktable.height,
      stats_label = list(n.risk = "")
    ) +
    ggsurvfit::scale_ggsurvfit(
      x_scales = list(breaks = seq(0, xMax, by = break.x.by),
                      limits = c(0,xMax))
    )
}

#' Summarise KM data
#' 
#' @param fit A `PCNMA::km_obj` object
#' 
#' @returns A summary table of the KM data
#' 
#' @export
summary.km_obj <- function(fit, ...) {
  if (!inherits(fit, "km_obj")) {
    rlang::abort("fit object is not of class 'km_obj'")
  }
  
  df <- survival:::summary.survfit(fit)[["table"]] 
  df <- data.frame(df)
  
  summary_table <- data.frame(Study = rownames(df)) |> 
    dplyr::mutate(Study = stringr::str_remove(Study, "Study=")) |> 
    dplyr::mutate(Median = df[["median"]]) |> 
    dplyr::mutate(MedianL95 = df[["X0.95LCL"]]) |> 
    dplyr::mutate(MedianU95 = df[["X0.95UCL"]])
  
  summary_table
}
