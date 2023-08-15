#' Write coefficients of models to excel
#'
#' 
#' @export
write_coefficients <- function(models, study, treatment){
  openxlsx::write.xlsx(
    x = list(coef.fitted_distribution(models) |> 
               mutate(Study = study) |> 
               mutate(Treatment = treatment)),
    file = paste0("/home/matthew/Documents/MScThesis/Results/Models/PFS/",study,"_",treatment,".xlsx")
  )
}