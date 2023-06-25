system("python3 src/IPD_handler.py")

for (i in list.files("Data/IPD/", pattern = ".csv")) {
  assign(
    stringr::str_remove(i, ".csv"),
    read.csv(paste0("Data/IPD/",i)) %>% 
      mutate(status = ifelse(censored == TRUE, 0, 1))
  ) 
}
