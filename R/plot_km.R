#' Plot a KM curve
#' 
#' @param fit A [survival::survfit] object
#' @param break.x.by A numeric value for splitting x axis
#' @param xMax The maximum time value to plot
#' @param risktable.height The proportion of the figure to be taken up by the risk table
#'
#' @returns A plotted km curve
#' 
#' @export
plot_km <- function(fit, break.x.by = 5, xMax = 40, risktable.height = 0.3){
  if (!inherits(fit, "survfit")) {
    rlang::abort("fit object is not of class 'survfit'")
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
