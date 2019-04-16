library(magrittr)

path <- "../data from CliRes/28-3-2019-_18ZN_"
antimicrobial <- c("ANTIMICROBIAL", "ANTIMICROBIAL_GridEvent")
disease <- c("DISEASE", "DISEASE_GridEvent")

lapply2 <- function(X, FUN, ...) setNames(lapply(X, FUN, ...), X)

surveillance <- c(antimicrobial, disease) %>%
  lapply2(readxl::read_excel, path = paste0(path, "V1_Data.xls"))

amu0 <- surveillance[antimicrobial] %>% 
  unname() %>% 
  purrr::invoke(dplyr::left_join, ., by = c("USUBJID", "ANTIMICROBIAL_SEQ")) %>% 
  dplyr::select(USUBJID, FLOCKSEQUENCE, WEEKNO, CODE) %>% 
  na.exclude()

drug_data <- c("ANTIMICROBIAL", "ANTIMICROBIAL_GridAnti") %>% 
  lapply(readxl::read_excel, path = paste0(path, "Category_V1_Data.xls"))

dictionary <- drug_data[[2]] %>%
  dplyr::select(ANTINAME, ANTINAME1) %>%
  na.exclude() %>%
  unique() %>% 
  dplyr::filter(!(ANTINAME == "NORFLOXACIN"      & ANTINAME1 == "neomycin")) %>% 
  dplyr::filter(!(ANTINAME == "SUNFAMETHOXAZOLE" & ANTINAME1 == "sunphamethoxazole")) %$%
  setNames(ANTINAME1, ANTINAME) %>% 
  c("ERYTHROMYCIN THIOCYANATE" = "erythromycin",
    "APRAMYCIN SULFATE"        = "apramycin",
    "CEFTIOFUR (HCL)"          = "ceftiofur",
    "SULFADIAZINE SODIUM"      = "sulfadiazine",
    "Florfenicol"              = "florfenicol",
    "Tylosin"                  = "tylosin",
    "Marbofloxacin"            = "marbofloxalin",
    "GENTAMYCIN SULFAT"        = "gentamicin",
    "COLISTIN (SULFATE)"       = "colistin")

drug_codes <- drug_data %>% 
  purrr::invoke(dplyr::right_join, ., by = "ANTIMICROBIAL_SEQ") %>% 
  dplyr::select(CODE, ANTINAME) %>%
  dplyr::mutate(ANTINAME = dictionary[ANTINAME]) %>% 
  unique() %>% 
  dplyr::filter(! ANTINAME %in% c("alicin", "axit oxolinic", "iodo-hydroxyquinoline", "metronidazol", "nystatin")) %>% 
  dplyr::mutate(ANTINAME = dplyr::recode(ANTINAME, sunfadimethoxine  = "sulfadimethoxine"))

amu <- dplyr::left_join(amu0, drug_codes, "CODE") %>%
  dplyr::mutate(presence = 1) %>%
  tibble::rowid_to_column() %>% # spread require unique row identifiers
  tidyr::replace_na(list(ANTINAME = "unknown")) %>% # because some antibiotic names are unknown
  tidyr::spread(ANTINAME, presence) %>%
  dplyr::select(-CODE, -rowid) %>% # we don't need CODE and rowid anymore
  replace(is.na(.), 0) %>%
  dplyr::group_by(USUBJID, FLOCKSEQUENCE, WEEKNO) %>%
  dplyr::summarise_all(~sum(.)) %>%
  dplyr::mutate_at(dplyr::vars(-USUBJID, -FLOCKSEQUENCE, -WEEKNO), function(x) x > 0)

symptoms <- surveillance[disease] %>% 
  unname() %>% 
  purrr::invoke(dplyr::left_join, ., by = c("USUBJID", "DISEASE_SEQ")) %>%
  dplyr::select(USUBJID, FLOCKSEQUENCE, WEEK, RESPIRATORY, DIARRHOEA, CNS, MALAISE, LEGLESIONS, SUDDENDEATH)

# corrections on weeks:
symptoms %<>%
# correction 1: 2 weeks 8 and no week 9 for cycle 2 of farm 8. These 2 weeks are identical
  dplyr::filter(USUBJID == "75-008", FLOCKSEQUENCE == "02", WEEK == 8) %>% 
  dplyr::mutate(WEEK = 8:9) %>%
  dplyr::bind_rows(dplyr::filter(symptoms, !(USUBJID == "75-008" & FLOCKSEQUENCE == "02" & WEEK == 8))) %>% 
# correction 2: week 19 instead of 10 for cycle 6 of farm 21:
  dplyr::mutate(WEEK = ifelse(USUBJID == "75-021" & FLOCKSEQUENCE == "06" & WEEK == 19, 10, WEEK))
  
# Checking there is no missing week in the symptoms dataset:
symptoms %>%
  dplyr::group_by(USUBJID, FLOCKSEQUENCE) %>%
  dplyr::arrange(WEEK) %>%
  dplyr::summarise(a = length(unique(diff(WEEK)))) %>%
  dplyr::ungroup() %>%
  dplyr::filter(a > 1)

# Checking that there are no week defined in amu but not in symptoms:
dplyr::anti_join(amu, symptoms, by = c("USUBJID", "FLOCKSEQUENCE", "WEEKNO" = "WEEK"))

# Merging:
viparc <- dplyr::left_join(symptoms, amu, by = c("USUBJID", "FLOCKSEQUENCE", "WEEK" = "WEEKNO")) %>% 
  dplyr::mutate_at(dplyr::vars(FLOCKSEQUENCE, WEEK), as.integer) %>% 
  dplyr::arrange(USUBJID, FLOCKSEQUENCE, WEEK)

# Writing to disk:
if (!dir.exists("data")) dir.create("data")
write.csv(viparc, "data/viparc.csv", FALSE, row.names = FALSE)