#' Generate KM estimates
#'
#' @param TTE A TTE dataframe
#'
#' @returns A [ggsurvfit::survfit2] object
#' 
#' @export
km_estimates <- function(TTE){
  out <- ggsurvfit::survfit2(
    survival::Surv(
      time = time, 
      event = status,
      type = "right"
    ) ~ 1, data = TTE)
  out
}