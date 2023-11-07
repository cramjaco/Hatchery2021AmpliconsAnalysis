library(tidyverse)
library(here)

raw_text <- read_lines(here("IntermediateData", "NCBI_Out", "GM8P2DA9016-Alignment.txt"))


target_text <- tibble(raw_text = raw_text) %>%
  filter(str_detect(raw_text,"(^Vibrio)|(^Query #)"))

working_text0 <- target_text %>%
  mutate(isQuery = case_when(str_detect(raw_text, "^Query") ~ 1,
                          str_detect(raw_text, "Vibrio") ~ 0
                          )) %>%
  mutate(queryNumber = cumsum(isQuery))

query_data <- working_text0 %>%
  filter(isQuery==1) %>%
  mutate(ASV = str_extract(raw_text, "ASV_\\d+")) %>%
  select(-c(isQuery, raw_text))

working_text <- working_text0 %>%
  filter(isQuery == 0) %>%
  select(-isQuery) %>%
  left_join(query_data, by = "queryNumber") %>%
  mutate(species = str_extract(raw_text, "^Vibrio\\s\\w*"))

vibrio_species_data <- working_text %>%
  select(ASV, species) %>%
  filter(species != "Vibrio sp") %>%
  group_by(ASV, species) %>%
  summarise(hits = n()) %>%
  mutate(nASV = parse_number(ASV)) %>%
  arrange(nASV, -hits) %>%
  mutate(spec_nogenus = str_extract(species, "(?<=Vibrio )\\w+"))

vibrio_species_tags <- vibrio_species_data %>%
  select(nASV, ASV, spec_nogenus) %>%
  group_by(nASV, ASV) %>%
  summarise(species = paste(spec_nogenus, collapse = " | "), .groups = "keep") %>%
  ungroup() %>%
  mutate(Tag_species = paste0("V. ", species, " ; ", nASV)) %>%
  mutate(Tag_species = fct_reorder(Tag_species, nASV))

