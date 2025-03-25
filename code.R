# setup ----
library(readr)
library(dplyr)
library(stringr)
# exercise 1.1 ----
survey_metadata <- read_tsv(file = "data/survey_metadata.tsv")
reef_fish_biomass <- read_delim(file = "data/reef_fish_biomass.dude",
                                delim = "|")
Sys.setenv("VROOM_CONNECTION_SIZE" = 2000000)
reef_fish_abundance <- read_csv(file = "data/reef_fish_abundance.zip",
                                guess_max = Inf)
# exercise 1.2 ----
function_1_2 <- function(x, pos) subset(x, country == "Indonesia" & depth < 20 & visibility > 10)
survey_metadata_selection_1_2 <- read_tsv_chunked(file = "data/survey_metadata.tsv",
                                                  callback = DataFrameCallback$new(function_1_2),
                                                  chunk_size = 1000)
# exercise 2.1 ----
biomass_exercice2_1_distinct <- distinct(.data = reef_fish_biomass)
biomass_exercice2_1_distinct_filter <- filter(.data = biomass_exercice2_1_distinct,
                                              biomass != 0
                                              & ! family %in% c("Mullidae",
                                                                "Apogonidae")
                                              & size_class <= 10)
# exercise 2.2 ----
biomass_exercice2_2 <- mutate(.data = biomass_exercice2_1_distinct_filter,
                              survey_id_full = paste(survey_id,
                                                     species_name,
                                                     size_class,
                                                     sep = "_")) %>%
  relocate(survey_id_full,
           .before = survey_id) %>%
  select(-4,
         -reporting_name) %>%
  rename(size_class_cm = size_class) %>%
  mutate(size_class_mm = size_class_cm * 10) %>%
  select(-size_class_cm) %>%
  arrange(desc(size_class_mm))
# exercise 2.3 ----
biomass_exercice2_3 <- biomass_exercice2_2 %>%
  dplyr::group_by(survey_id_full) %>%
  mutate(sum_biomass = sum(x = biomass)) %>%
  ungroup() %>%
  distinct(survey_id_full,
           .keep_all = TRUE)
biomass_exercice2_3_sample_verification <- dplyr::group_by(.data = biomass_exercice2_2,
                                                           family) %>%
  summarise(nb_sample_family = n()) %>%
  mutate(total = sum(nb_sample_family))
# exercise 2.4 ----
survey_metadata_distinct <- distinct(.data = survey_metadata)
biomass_survey_exercice2_4 <- biomass_exercice2_3 %>%
  left_join(survey_metadata_distinct,
            by = "survey_id")
# exercice 4.1 ----
biomass_survey_exercice4_1 <- slice(.data = biomass_survey_exercice2_4,
                                    str_which(string = biomass_survey_exercice2_4$family,
                                              pattern = "Embiotocidae"))
sum_data_embiotoca_exercice4_1 <- sum(str_count(string = biomass_survey_exercice4_1$species_name,
                                                pattern = "Embiotoca."))
location_biomass_survey_exercice4_1 <- unique(str_subset(string = biomass_survey_exercice4_1$location,
                                                         pattern = "British Columbia",
                                                         negate = TRUE))
# exercice 4.2 ----
biomass_survey_exercice4_2 <- biomass_survey_exercice4_1 %>%
  mutate(site_name_length_ori = str_length(site_name),
         site_name_clean = str_squish(site_name),
         site_name_clean = str_to_sentence(site_name_clean)) %>%
  relocate(site_name_length_ori,
           site_name_clean,
           .after = site_name)
# exercice 4.3 ----
biomass_survey_exercice4_3 <- biomass_survey_exercice4_2
  
