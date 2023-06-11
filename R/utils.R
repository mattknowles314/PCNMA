read_def <- function(path){
  openxlsx::read.xlsx(path, sheet = "DEF", startRow = 3)
}

colours <- list(
  "green" = "#7EBE91",
  "blue" = "#7EABBE",
  "purple" = "#b17ebe"
)