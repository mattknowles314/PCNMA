#' Write coefficients of models to excel
#'
#' @param models A [fitted_distribution] object
#' @param study The study 
#' @param treatment The treatment
#' 
#' @export
write_coefficients <- function(models, study, treatment, PARAMCD){
  openxlsx::write.xlsx(
    x = list(coef.fitted_distribution(models) |> 
               mutate(Study = study) |> 
               mutate(Treatment = treatment)),
    file = paste0("/home/matthew/Documents/MScThesis/Results/Models/",PARAMCD,"/",study,"_",treatment,".xlsx")
  )
}

#' Save coefficients of models
#'
#' @param models A [fitted_distribution] object
#' @param study The study 
#' @param treatment The treatment
#' 
#' @export
save_coefficients <- function(models, study, treatment){
    out <- coef.fitted_distribution(models) |> 
               mutate(Study = study) |> 
               mutate(Treatment = treatment)
    out
}
