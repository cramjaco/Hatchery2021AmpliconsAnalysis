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

## Lets start with just one cast. I like 21-38 a "best" batch.

spawn2138 <- spawnInfo00 %>% 
  filter(brood == "21-38")

drain2138 <- drainLogRaw01 %>%
  filter(brood == "21-38")

## The process of what I'd evenutally like a funciton to do.
## Track each open tank at each time-point and keep track of dead, swimming, and graduated larvae

## Notes: Can't handle systems other than 8 and 4
## 11 should be synonomous with 4 (refrigerator/settled)
## 28 with death, I think, but I think it could be considered graduated and wouldn't screw things up too much and would be easier to handle

# > drainLogRaw01 %>% group_by(toSystem) %>% summarise(n())
# # A tibble: 5 × 2
#   toSystem `n()`
#      <dbl> <int>
# 1        4  2198
# 2        8   584
# 3       11     9
# 4       28    10
# 5       NA    38

# Setup

dead <- 0
swimming <- 0
graduted <- 0

active_tanks <- c()

dates_in_brood <- drain2138$datetime %>% unique() %>% sort()

brood_table <- tibble(
  datetime = c(spawn2138$`Date Spawned`, drain2138$datetime) %>% unique() %>% sort(),
  observed = NA,
  dead = NA,
  swimming = NA,
  graduated = NA
)

# Step 0 -- the spawnlings go into the first tank

current_date <- spawn2138$`Date Spawned`



swimming <- spawn2138$`eggs produced (m)`

active_tanks <- drain2138[1,]$fromLocation # One active tank at first
tank_amounts <- swimming # there is only one tank, all swimming larvae are in it

active_tanks_table <- tibble(
  tanks = active_tanks,
  amount_active = tank_amounts
)

brood_table[1,]$observed <- TRUE
brood_table[1,]$dead <- 0
brood_table[1,]$swimming <- swimming
brood_table[1,]$graduated <- 0

# Step i -- At each day, active tanks transfer to their decendents
for(iter in 1:(nrow(brood_table)-1)){
#for(iter in 1:5) {
  
  
  current_date <- dates_in_brood[iter]
  
  drainlog_indexes <- which(drain2138$datetime == current_date)
  brood_table_index <- which(brood_table$datetime == current_date)
  
  
  transferred_tanks_table <- drain2138[drainlog_indexes,c("fromLocation", "toLocation", "toSystem", "amount")]
  
  tansferred_tanks_table_summed <- transferred_tanks_table %>%
    group_by(fromLocation, toLocation, toSystem) %>%
    summarise(amount = sum(amount)) %>%
    ungroup()
  
  day_accounting_table <- full_join(
    active_tanks_table,
    tansferred_tanks_table_summed, 
    by = c("tanks" = "fromLocation"), multiple = "all"
  )
  
  ##day_accounting_table <- day_accounting_table 
    ## mutate(died = amount_active - amount) ## wrong
  
  # We only want to calculate deaths on observed tables
  observed_tanks_table <- active_tanks_table %>%
    filter(tanks %in% transferred_tanks_table$fromLocation)
  
  total_died_today = sum(observed_tanks_table$amount_active) - sum(transferred_tanks_table$amount) # This can be slightly negative
  
  are_all_tanks_observed <- sum(is.na(day_accounting_table$toLocation)) == 0
  
  brood_table[brood_table_index,]$observed = are_all_tanks_observed
  brood_table[brood_table_index,]$dead = brood_table[brood_table_index-1,]$dead + total_died_today
  
  total_graduated_today <- tansferred_tanks_table_summed %>%
    filter(toSystem %in% c(4,11,28)) %>% # 28 for now, will try to handle differently (like death) later
    summarise(amount = sum(amount)) %>%
    pull(amount)
  
  brood_table[brood_table_index,]$graduated = brood_table[brood_table_index-1,]$graduated + total_graduated_today
  
  if(are_all_tanks_observed){
    
    total_swimming_today <- tansferred_tanks_table_summed %>%
    filter(toSystem == 8) %>%
    summarise(amount = sum(amount)) %>%
    pull(amount)
    
    brood_table[brood_table_index,]$swimming = total_swimming_today
  }
  
  # setup active tanks table for the next day
  # Should contain all of the new tanks, plus the unclosed ones
  unclosed_tanks <- day_accounting_table %>%
    filter(is.na(toSystem)) %>%
    select(tanks, amount_active)
  new_tanks <- day_accounting_table %>%
    filter(toSystem == 8) %>%
    select(tanks = toLocation,
           amount_active = amount)
  
  active_tanks_table <- bind_rows(unclosed_tanks,
                            new_tanks)
  
}

brood_table %>%
  ggplot(aes(x = datetime, y = swimming + graduated)) + 
  geom_point() +
  lims(y = c(0, NA))

brood_table %>%
  select(-observed) %>%
  pivot_longer(dead:graduated, names_to = "status", values_to = "amount") %>%
  filter(!is.na(amount)) %>%
  ggplot(aes(x = datetime, y = amount, shape = status)) + 
  geom_point() +
  geom_path() + 
  scale_shape_manual(values = c(dead = 4, graduated = 15, swimming = 1))

# 
# 
# 
# 
# tanks_closed <- drain2138[drainlog_indexes,]$fromLocation
# tanks_opened <- drain2138[drainlog_indexes,]$toLocation
# 
# untouched_tanks <- setdiff(active_tanks, tanks_closed)
# 
# are_all_observed <- length(untouched_tanks) == 0
# 
# brood_table[brood_table_index,]$observed <- are_all_observed
# 
# # Now what
# 
# # For the tanks closed, add the graduated and dead larvae to the totals.
# # Dead larvae
# graduated_indexes <- which(drain2138$datetime == current_date & drain2138$toSystem == 4)
# larvae_graduated_today <- sum(drain2138[graduated_indexes,]$amount)
# brood_table[brood_table_index,]$graduated <- larvae_graduated_today
# 
# confirmed_dead <- swimming - sum(drain2138[drainlog_indexes,]$amount) # this is going to break when I close more than one tank, but works for the first few iterations
# 
# brood_table[brood_table_index,]$dead <- confirmed_dead
# 
# swimming <- NULL # gOD HELP ME
# 
# # Create a new set of tanks to which we add the live larvae, per the drain log
# 
# 
