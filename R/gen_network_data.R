#' Generate network data
#'
#' @param data A date extraction dataset
#' @param ref A character reference treatment
#'
#' @returns A dataframe 
#' 
#' @export
gen_network_data <- function(data, ref){
  data %>% dplyr::select(Study, Year, Total.Patients, Treatment.1, Treatment.2, `N.(Trt1)`, `N.(Trt2)`, Reported.OS, Reported.PFS) %>% 
    dplyr::rename(N = Total.Patients) %>% 
    dplyr::mutate(Study = paste0(Study, ", ", Year)) %>% 
    dplyr::pivot_longer(cols = 4:5, values_to = "Treatment") %>% 
    dplyr::mutate(r = ifelse(Treatment == ref, `N.(Trt1)`, `N.(Trt2)`)) %>% 
    dplyr::select(-c(name, `N.(Trt1)`, `N.(Trt2)`))
}
