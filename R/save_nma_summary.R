#' Write model summary to xlsx
#'
#' @param nma A [mutinma::stan_data] object
#' @param PARAMCD one of OS or PFS
#' @param effect type
#' @param model_type one of parametric or fp
#' 
#' @export
save_nma_summary <- function(nma, PARAMCD, effects, model_type){
  openxlsx::write.xlsx(
    x = list(multinma:::summary.stan_nma(nma)),
    file = paste0("/home/matthew/Documents/MScThesis/Results/Models/",PARAMCD,"/",model_type,"_",effects,".xlsx")
  )
}