if (!dir.exists("data")) dir.create("data")
"/Users/choisy/Dropbox/aaa/projects/ViParc/AMU metrics/Supplementary_Data_Frontiers.xlsx" %>% 
  readxl::read_excel() %>% 
  tidyr::gather("week", "weight", -FlockID, -Chicken) %>% 
  na.exclude() %>% 
  dplyr::mutate(week = sub("Week ", "", week)) %>% 
  dplyr::mutate_at(c("FlockID", "Chicken", "week"), as.integer) %>% 
  write.csv("data/weights.csv", FALSE, row.names = FALSE)
