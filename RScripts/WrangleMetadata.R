## Import libraries
library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(here)
library(tidyr)
library(ggplot2)
library(purrr)




## Import metadata
dataDir <- "Hatchery_UVexperiment_All"
metadata <- read_csv(here(dataDir, paste0("metadata_", dataDir, ".csv")))

## Import brood info data
broodInfo <- read_csv(here(dataDir, "brood_info.csv"))

# Join bood info data
metadata01 <- metadata %>%
  left_join(broodInfo, by = "Brood#")


# Identify which UV experiements are UV treated and which are controls
metadata02 <-  metadata01 %>%
  mutate(InfoUV = if_else(Info1 == "UV",
                          case_when(
                            Info2 == "Main" & `Tube#` %in% c("0012", "0230") ~ "Control",
                            Info2 == "Main" & `Tube#` %in% c("0015", "0236") ~ "UV",
                            Info2 == "Exp" ~ str_extract(`Brood#`, "^.{2,4}(?=\\s)"),
                            TRUE ~ "Err"
                          ),
                          ""
      ))

# fix day info

metadata03 <- metadata02 %>%
  separate(Day, into = c("DayA", "DayB")) %>%
  mutate(across(.cols = c(DayA, DayB), as.numeric)) %>%
  mutate(DayN = map2_dbl(DayA, DayB, ~mean(na.omit(.)))) %>%
  select(-DayA, -DayB)

write_csv(metadata03, here(dataDir, "metadata_expanded.csv"))
