km_estimates <- function(TTE){
  out <- ggsurvfit::survfit2(
    survival::Surv(
      time = time, 
      event = status,
      type = "right"
    ) ~ 1, data = TTE)
}