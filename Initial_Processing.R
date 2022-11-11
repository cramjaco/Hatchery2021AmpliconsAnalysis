library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(here)
library(tidyr)
library(ggplot2)
source(here("RScripts", "Oyster_Library.R"))
library(vegan)
library(ggvegan)


# More color-blind friendly colorbalettes
#http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=10
cb10 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')

cb12 <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')

# Less color-blind friendly, but still nice.
#https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
trub20 <- c('#e6194b','#3cb44b','#ffe119','#0082c8','#f58231','#911eb4','#46f0f0','#f032e6','#d2f53c','#fabebe','#008080','#e6beff','#aa6e28','#fffac8','#800000','#aaffc3','#808000','#ffd8b1','#000080','#808080','#FFFFFF','#000000')

dataDir <- "Hatchery_UVexperiment_All"
metadata <- read_csv(here(dataDir, paste0("metadata_expanded.csv")))
counts <- read_csv(here(dataDir, paste0("counts_", dataDir, ".csv"))) %>%
  rename(ASV = 1)
tax <- read_csv(here(dataDir, paste0("tax_", dataDir, ".csv"))) %>% add_tag_to_tax()

## Convert into long format
nonOyster_wm <- fortify_oyster_wm(counts, tax, metadata)

## Keep only stuff from the main hatchery study

hatch <- nonOyster_wm %>% 
  filter(Info1 %in% c("Good", "Crash"))

## Total bacterial abundance
hatch_abun <- hatch %>%
  group_by(Sample0) %>%
  summarise(ratio = sum(ratio))

## Only samples from the "early" (first) time-points
early <- hatch %>%
  filter(DayN <=5)

mid <- hatch %>%
  filter(DayN >= 7, DayN <= 8)

