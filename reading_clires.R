library(readxl)
library(magrittr)

#data_folder <- "../data from CliRes"
data_folder <- "../data"
files <- dir(data_folder, full.names = TRUE)

read_excel_file <- function(x) {
  require(readxl)
  sheet_names <- excel_sheets(x)
  setNames(lapply(sheet_names, read_xls, path = x), sheet_names)
}

read_data <- function(x) {
  require(magrittr)
  files %>%
    grep("Category", ., value = TRUE, invert = x) %>%
    read_excel_file()
}

drugs <- read_data(FALSE)
surveillance <- read_data(TRUE)


