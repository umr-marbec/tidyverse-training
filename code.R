# setup
library(readr)
library(dplyr)
# exercise 1.1
survey_metadata <- read_tsv(file = "data/survey_metadata.tsv")
reef_fish_biomass <- read_delim(file = "data/reef_fish_biomass.dude",
                                delim = "|")
reef_fish_abundance <- read_csv(file = "data/reef_fish_abundance.zip",
                                guess_max = Inf)
# exercise 1.2
function_1_2 <- function(x, pos) subset(x, country == "Indonesia" & depth < 20 & visibility > 10)
survey_metadata_selection_1_2 <- read_tsv_chunked(file = "data/survey_metadata.tsv",
                                                  callback = DataFrameCallback$new(function_1_2),
                                                  chunk_size = 1000)
# exercise 2.1 ----
biomass_exercice2_1_distinct <- distinct(.data = global_reef_fish_biomass)
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
  dplyr::select(-4,
                -reporting_name) %>%
  rename(size_class_cm = size_class) %>%
  mutate(size_class_mm = size_class_cm * 10) %>%
  dplyr::select(-size_class_cm) %>%
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
save(biomass_survey_exercice2_4, file = "./data/biomass_survey_exercice2_4.RData")
# exercise 3.1 ----
abundance <- read_delim(file = "./data/Global_reef_fish_abundance.csv")
# exercise 3.2 ----
abundance_long <- abundance %>%
  pivot_longer(
    .,
    cols = 2:17523,
    names_to = "sp_size",
    values_to = "abundance",
    values_drop_na = TRUE
  ) %>%
  unite(., col = "survey_id_full", c(survey_id, sp_size), sep = "_")
slice_head(abundance_long)
# exercise 3.3 ----
biomass_abundance <- biomass_survey_exercice2_4 %>%
  left_join(., abundance_long, by = "survey_id_full")
# exercise 3.4 ----
biomass_abundance <- biomass_abundance %>%
  separate(., species_name, into = c("genus", "species"))
slice_head(biomass_abundance[,8:11])
# exercise 3.5 ----
data_sf <- sf::st_as_sf(biomass_abundance,
                        coords = c("longitude", "latitude") ,
                        remove = FALSE,
                        crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# exercise 3.6 ---
biomass_survey_abundance_exercice3_6 <- biomass_abundance %>%
  drop_na(latitude) %>% 
  drop_na(longitude) %>%
  sf::st_as_sf(
    .,
    coords = c("longitude", "latitude"),
    remove = FALSE,
    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  )
save(biomass_survey_abundance_exercice3_6,
     file = "./data/biomass_survey_abundance_exercice3_6.RData")