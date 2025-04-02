# setup ----
library(readr)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
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
# exercice 3.1 ----
head(x = biomass_survey_exercice2_4)
View(x = biomass_survey_exercice2_4)
# exercice 3.2 ----
abundance_exercice3_2 <- reef_fish_abundance %>%
  pivot_longer(.,
               cols = 2:ncol(.),
               names_to = "sp_size",
               values_to = "abundance")
# exercice 3.3 ----
abundance_exercice3_3 <- abundance_exercice3_2 %>%
  drop_na(., 
          abundance)
# exercice 3.4 ----
abundance_biomass_survey_exercice3_4 <- abundance_exercice3_3 %>%
  distinct() %>%
  unite(.,
        col = "survey_id_full",
        c(survey_id,
          sp_size),
        sep = "_")  %>%
  right_join(.,
             biomass_survey_exercice2_4,
             by = "survey_id_full") %>%
  relocate(abundance,
           .after = biomass)
# exercice 3.5 ----
abundance_biomass_survey_exercice3_5 <- abundance_biomass_survey_exercice3_4 %>%
  separate(.,
           species_name,
           into = c("genus",
                    "species")) %>%
  select(-c(23:ncol(.)))
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
biomass_survey_exercice4_3 <- biomass_survey_exercice4_2 %>%
  mutate(species_name_first = str_split_i(string = species_name,
                                          pattern = "[.]",
                                          i = 1),
         species_name_second = str_split_i(string = species_name,
                                           pattern = "[.]",
                                           i = 2),
         biomass_percentage = str_glue("{round(x = biomass * 100 / sum_biomass)}%"),
         description = str_c(species_name_first ,
                             species_name_second,
                             str_c("(",
                                   biomass_percentage,
                                   " of total biomass)"),
                             sep = " "),
         description_shinny = str_wrap(string = description,
                                       width = 20))
# exercice 4.4 ----
biomass_survey_exercice4_4 <- biomass_survey_exercice4_3 %>%
  mutate(site_code_letters = str_extract(string = site_code,
                                         pattern = "^[:upper:]+"),
         site_code_numbers = str_extract(string = site_code,
                                         pattern = "[:digit:]+$")) %>%
  relocate(site_code_letters,
           site_code_numbers,
           .after = site_code)
# exercice 5.1 ----
biomass_survey_exercice5_1 <- biomass_survey_exercice4_3 %>%
  mutate(survey_date_as_date = dmy(survey_date),
         survey_year = year(x = survey_date_as_date),
         duration_since_now = today() - survey_date_as_date,
         next_survey_date = survey_date_as_date + dmonths(x = 12),
         next_survey_date_correct = next_survey_date %>%
           {hour(.) <- 12; .}) %>%
  relocate(survey_date_as_date,
           next_survey_date,
           survey_year,
           duration_since_now,
           next_survey_date_correct,
           .after = survey_date)

# Exercice 6.1 ----
biomass_exercice6_1 <- abundance_biomass_survey_exercice3_5 %>%
  mutate(survey_date_as_date = dmy(survey_date),
         survey_year = year(survey_date_as_date)) %>%
  filter(survey_year == 2011) %>%
  group_by(country, survey_id) %>%
  summarise(biomass = sum(biomass)) 
## Exercice 6.2 ----
p6_2 <- p6_1 +
  geom_boxplot()
# Exercice 6.3 ----
p6_3 <- ggplot(data = biomass_exercice6_1) +
  geom_boxplot(aes(x = reorder(country, -biomass, FUN = median), y = biomass)) 
p6_3
# Exercice 6.4 ----
p6_4 <- p6_3 +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p6_4  
# Exercice 6.5 ----
ggsave(filename = "figures/boxplot_biomass_vs_country.png", 
       plot     = p6_4,
       width    = 8,
       height   = 5)
# Exercice 6.6 ----
biomass_exercice6_6 <- biomass_exercice6_1 %>%
  filter(country %in% c("Spain", "United States", "Italy", "Costa Rica", "Panama")) %>%
  mutate(continent = case_when((country == "Spain"| country == "Italy") ~ "Europe",
                                country == "United States" ~ "North America",
                                TRUE ~ "South America"))
p6_6 <- ggplot() +
  geom_boxplot(data    = biomass_exercice6_6,
               mapping = aes(x = reorder(country, -biomass, FUN = median), 
                             y = biomass, 
                             fill = continent)) +
  scale_fill_manual(values = c("Europe" = "skyblue", 
                               "South America" = "aquamarine", 
                               "North America" = "moccasin")) +
  labs(x = "Country", y = "Biomass (g)") + 
  theme_bw()
p6_6
# Exercice 6.7 ----
biomass_exercice6_7 <- biomass_exercice6_6 %>%
  group_by(country) %>%
  summarise(biom_mean = mean(biomass),
            biom_sd   = sd(biomass)) 
p6_7 <- ggplot(biomass_exercice6_7) +
  geom_col(aes(x = reorder(country, biom_mean), y = biom_mean)) +
  geom_errorbar(aes(x = country, ymin = biom_mean-biom_sd, ymax = biom_mean+biom_sd),
                    width = 0.2, colour = "red") +
  labs(x = "Country", y = "Biomass (g)") + 
  theme_bw()
p6_7

