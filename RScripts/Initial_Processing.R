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
metadata0 <- read_csv(here(dataDir, paste0("metadata_expanded.csv")))
counts <- read_csv(here(dataDir, paste0("counts_", dataDir, ".csv"))) %>%
  rename(ASV = 1)
tax0 <- read_csv(here(dataDir, paste0("tax_", dataDir, ".csv"))) %>% add_tag_to_tax()
tax <- tax0 %>%
  mutate(Phylum = if_else(Phylum == "Proteobacteria", Class, Phylum)) %>%# Use class names for Proteobacteria
  mutate(Phylum = str_replace_all(Phylum, c("^Alpha" = "α-", "^Beta" = "β-", "^Gamma" = "γ-", "^Delta" = "δ-", "^Epsilon" = "ε", "^Zeta" = "ζ" )))

## Correct issues with metadata

# Several taxa have multiple occurances
# 21-56 (day 7) -- tube 183 is actually from 21-54 (not part of this analysis)
# 21-58 (day 8) -- 213 Should be 11 days, not 8
# 21-57 (day 10) -- Tank 8 (219 should be 14 (not 10) days)
# 21-11 (day 17) -- (Tube H57 is bottom of tank and the other is normal screen larvae)
# 
# To do: remove tubes 183 and 057
# Change DayN of 219 and 213 up front.

metadata <- metadata0
#  # 21-56 (day 7) -- tube 183 is actually from 21-54 (not part of this analysis)
metadata[which(metadata$`Tube#` == "0187"), "Brood#"] <- "21-54"
metadata[which(metadata$`Tube#` == "0187"), "Info1"] <- "Unk"
metadata[which(metadata$`Tube#` == "0187"), "Info2"] <- "Unk"

# 21-58 (day 8) -- 213 Should be 11 days, not 8
metadata[which(metadata$`Tube#` == "0213"), "DayN"] <- 11

# 21-57 (day 10) -- Tank 8 (219 should be 14 (not 10) days)
metadata[which(metadata$`Tube#` == "0219"), "DayN"] <- 14

# 21-11 (day 17) -- (Tube H57 is bottom of tank and the other is normal screen larvae)
metadata[which(metadata$`Tube#` == "0057"), "Info2"] <- "Sludge"
metadata[which(metadata$`Tube#` == "0057"), "Info1"] <- "CrashSludge"

# Save better metadata back out for better records
write_csv(metadata,  here(dataDir, "metadata_corrected.csv"))

## Convert into long format
nonOyster_wm <- fortify_oyster_wm(counts, tax, metadata)

## Keep only stuff from the main hatchery study

hatch <- nonOyster_wm %>% 
  filter(Info1 %in% c("Good", "Crash")) %>%
  filter(Order != "Chloroplast") %>%
  # remove odd samples
  filter(!(`Info2` %in% c("Unk", "Sludge"))) %>% 
  identity()

## Total bacterial abundance
hatch_abun <- hatch %>%
  group_by(Sample0) %>%
  summarise(ratio = sum(ratio))

## Only samples from the "early" (first) time-points
early <- hatch %>%
  filter(DayN <=5)

mid <- hatch %>%
  filter(DayN >= 7, DayN <= 8)

dayCat <- read_csv("DayCat.csv") %>%
  mutate(DayCat = ordered(DayCat, levels = unique(DayCat)))

metadata_main <- metadata %>%
  filter(Info1 %in% c("Good", "Crash")) %>%
  filter(!(`Info2` %in% c("Unk", "Sludge"))) %>%
  left_join(read_csv("DayCat.csv"), by = "DayN")



## Calculations from inital processing
# Microbe to host gene ratios, in a long format table with sample info and taxonomy infomation
nonOyster <- fortify_oyster(counts, tax)

# Only the "main" data. Not the UV experiment
nonOysterM <- nonOyster %>%
  filter(Sample0 %in% unique(metadata_main$Sample0))

# Determie in what fraction we see microbes
nSamples <- nonOysterM$Sample0 %>% unique() %>% length()
hitFracDf <- nonOysterM %>%
  select(ASV, count, ratio) %>%
  mutate(enoughHits = count>=3) %>%
  group_by(ASV) %>%
  summarise(hits = sum(enoughHits)) %>%
  mutate(hitfrac = hits/nSamples) %>%
  identity()

# which bugs show up 20% of the time
hitFracOk <- hitFracDf %>%
  filter(hitfrac > 0.2)

# just subset bacteria that show up at least 20% of the time
# makes ordination analyses work better
nonOyster20 <- nonOysterM %>%
  filter(ASV %in% hitFracOk$ASV)

nonOyster20_wm <-  left_join(nonOyster20, metadata, by = "Sample0")

# wide format data frame for ordination
ratioDf <- nonOyster20 %>%
  select(Sample0, ASV, ratio) %>%
  pivot_wider(names_from = ASV, values_from = ratio, values_fill = 0) %>%
  column_to_rownames("Sample0") %>%
  identity()

