library(magrittr)

clires_folder <- "../data from CliRes/16-4-2019-_18ZN_"
antimicrobial <- c("ANTIMICROBIAL", "ANTIMICROBIAL_GridEvent")
disease <- c("DISEASE", "DISEASE_GridEvent")
chicken <- c("MID_INOUT", "MID_INOUT_GridEvent")

lapply2 <- function(X, FUN, ...) setNames(lapply(X, FUN, ...), X)

esvac <- read.csv("data/esvac.csv") %>% 
  dplyr::filter(!grepl("methane", ab_vet_med)) %>% 
  dplyr::select(ab_oie, mg.IU) %>% 
  dplyr::mutate(ab_oie = sub(" .*$", "", tolower(ab_oie))) %>% 
  dplyr::bind_rows(data.frame(ab_oie = "josamycin",
                              mg.IU  = 0.001, stringsAsFactors = FALSE)) %$%
  setNames(mg.IU, ab_oie)

surveillance <- c(antimicrobial, disease, chicken, "SAMPLE") %>%
  lapply2(readxl::read_excel, path = paste0(clires_folder, "V1_Data.xls"))

samplesdata <- surveillance$SAMPLE %>% 
  dplyr::select(USUBJID, FLOCKSEQUENCE, SAMPLINGDATE, SAMPLINGVISIT)

samplesdata %>%
  dplyr::filter(SAMPLINGVISIT == "S") %>%
  select(-SAMPLINGVISIT) %>%
  rename(start = SAMPLINGDATE) %>%
  right_join(samplesdata) %>%
  mutate(WEEK     = as.integer(floor((SAMPLINGDATE - start) / (60 * 60 * 24 * 7)) + 1),
         sampling = TRUE) %>% 
  select(-start, -SAMPLINGDATE, -SAMPLINGVISIT)
  
chickdata <- surveillance[chicken] %>% 
  unname() %>% 
  purrr::invoke(dplyr::left_join, ., by = c("USUBJID", "MID_INOUT_SEQ")) %>% 
  dplyr::select(USUBJID, FLOCKSEQUENCE, WEEK, CHICKENTOTAL) %>% 
  dplyr::mutate_at("CHICKENTOTAL", as.integer)
  
amu0 <- surveillance[antimicrobial] %>% 
  unname() %>% 
  purrr::invoke(dplyr::left_join, ., by = c("USUBJID", "ANTIMICROBIAL_SEQ")) %>% 
  dplyr::select(USUBJID, FLOCKSEQUENCE, WEEKNO, CODE, AMOUTUSED, PACKAGEUNIT) %>%
  na.exclude() %>% 
  dplyr::mutate(PACKAGEUNIT = dplyr::na_if(PACKAGEUNIT, "TAB"))

drug_data <- c("ANTIMICROBIAL", "ANTIMICROBIAL_GridAnti") %>% 
  lapply(readxl::read_excel, path = paste0(clires_folder, "Category_V1_Data.xls"))

drug_codes <- drug_data %>% 
  purrr::invoke(dplyr::right_join, ., by = "ANTIMICROBIAL_SEQ") %>% 
  dplyr::select(CODE, ANTINAME1, CONCENTRATION, GRIDUNIT, GRIDPACK, GRIDPACKUNIT) %>%
  dplyr::filter(! ANTINAME1 %in% c("alicin", "axit oxolinic", "iodo-hydroxyquinoline", "metronidazol", "nystatin")) %>% 
  dplyr::mutate(ANTINAME1     = dplyr::recode(ANTINAME1, sunfadimethoxine  = "sulfadimethoxine",
                                                         sunphamethoxazole = "sulphamethoxazole"),
                GRIDUNIT      = dplyr::na_if(GRIDUNIT, "NA"),
                GRIDUNIT      = dplyr::na_if(GRIDUNIT, "na"),
                GRIDUNIT      = dplyr::recode(GRIDUNIT, UI = "IU", mg = "MG"),
                GRIDPACKUNIT  = dplyr::na_if(GRIDPACKUNIT, "na"),
                GRIDPACKUNIT  = dplyr::na_if(GRIDPACKUNIT, "VI"),
                GRIDPACKUNIT  = dplyr::recode(GRIDPACKUNIT, g  = "G", ml = "G", ML = "G"),
                CONCENTRATION = dplyr::na_if(CONCENTRATION, "NA"),
                CONCENTRATION = dplyr::na_if(CONCENTRATION, "na"),
                CONCENTRATION = dplyr::recode(CONCENTRATION, S = "500"), # this will ultimately be corrected in CliRes
                CONCENTRATION = as.numeric(CONCENTRATION),
                CONCENTRATION = ifelse(GRIDUNIT == "IU", CONCENTRATION * esvac[ANTINAME1], CONCENTRATION),
                GRIDUNIT      = ifelse(GRIDUNIT == "IU", "MG", GRIDUNIT), # HAS TO BE HERE!!!
                CONCENTRATION = ifelse(GRIDUNIT == "MG", CONCENTRATION / 1000, CONCENTRATION),
                GRIDUNIT      = ifelse(GRIDUNIT == "MG", "G", GRIDUNIT), # HAS TO BE HERE!!!
                GRIDPACK      = dplyr::na_if(GRIDPACK, "na"),
                GRIDPACK      = as.numeric(GRIDPACK),
                GRIDPACK      = ifelse(GRIDPACKUNIT == "KG", 1000 * GRIDPACK, GRIDPACK),
                GRIDPACKUNIT  = ifelse(GRIDPACKUNIT == "KG", "G", GRIDPACKUNIT), # HAS TO BE HERE!!! 
                proportion    = CONCENTRATION / GRIDPACK) %>% 
  unique()

amu <- dplyr::left_join(amu0, drug_codes, "CODE") %>%
  tidyr::replace_na(list(ANTINAME1 = "unknown")) %>% # because some antibiotic names are unknown
  dplyr::mutate(presence   = 1,
                antibiotic = proportion * AMOUTUSED,
                antiname   = toupper(ANTINAME1)) %>%
  dplyr::select(USUBJID, FLOCKSEQUENCE, WEEKNO, ANTINAME1, presence, antibiotic, antiname) %>% 
  tibble::rowid_to_column() %>% # spread require unique row identifiers
  tidyr::spread(ANTINAME1, presence) %>%
  tidyr::spread(antiname, antibiotic) %>%
  dplyr::select(-rowid) %>% # we don't need rowid anymore
  replace(is.na(.), 0) %>%
  dplyr::group_by(USUBJID, FLOCKSEQUENCE, WEEKNO) %>%
  dplyr::summarise_all(~sum(.)) %>%
  dplyr::mutate_at(vars(matches("^[a-z]", FALSE)), function(x) x > 0)

symptoms <- surveillance[disease] %>% 
  unname() %>% 
  purrr::invoke(dplyr::left_join, ., by = c("USUBJID", "DISEASE_SEQ")) %>%
  dplyr::select(USUBJID, FLOCKSEQUENCE, WEEK, RESPIRATORY, DIARRHOEA, CNS, MALAISE, LEGLESIONS, SUDDENDEATH) %>% 
# corrections on weeks (this cannot be corrected in CliRes and has to be corrected here)
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

# Checking that there are no week defined in chickdata but not in viparc:
dplyr::anti_join(chickdata, symptoms, by = c("USUBJID", "FLOCKSEQUENCE", "WEEK"))

# Merging:
viparc <- symptoms %>% 
  dplyr::left_join(amu, by = c("USUBJID", "FLOCKSEQUENCE", "WEEK" = "WEEKNO")) %>% 
  dplyr::left_join(chickdata, by = c("USUBJID", "FLOCKSEQUENCE", "WEEK")) %>% 
  replace(is.na(.), FALSE) %>% ### what is this for???
  dplyr::mutate_at(dplyr::vars(FLOCKSEQUENCE, WEEK), as.integer) %>% 
  dplyr::arrange(USUBJID, FLOCKSEQUENCE, WEEK)

# Writing to disk:
if (!dir.exists("data")) dir.create("data")
write.csv(viparc, "data/viparc.csv", FALSE, row.names = FALSE)