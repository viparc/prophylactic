
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