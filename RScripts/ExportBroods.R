library(here)
library(readr)
library(dplyr)

dataDir <- "Hatchery_UVexperiment_All"
metadata <- read_csv(here(dataDir, paste0("metadata_", dataDir, ".csv")))

unique_broods <- metadata %>% select(`Brood#`) %>% arrange(`Brood#`) %>% unique()

write_csv(unique_broods, file = here(dataDir, "unique_broods.csv"))
