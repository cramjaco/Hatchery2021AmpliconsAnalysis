## Inital attempt to explore drain log data.

## Libraries, etc
library(tidyverse)
library(here)
library(readxl)
library(lubridate)

## Spawn Info
# `Date Spawned` = "D"
spawnInfo00 <- read_excel(here("HatcheryData", "2021 Spawn info.xlsx"), col_types = c("text", "date", "numeric"))

  

## Bring in Drain log data

drainLogRaw00 <- read_excel(here("HatcheryData", "2021 Drain Log Final_jacedit.xlsx"),
                            sheet = "raw drain day")
drainLogRaw01 <- drainLogRaw00 %>%
  mutate(datetime = ymd_hms(datetime))