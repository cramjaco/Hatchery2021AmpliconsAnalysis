
# Take spawnDf -- small data frame with spawning info for a brood and 
# drainDf -- drain info for a brood

# return processed data table
process_drain_log_brood <- function(spawnDf, drainDf){
  
  # Setup
  
  dead <- 0
  swimming <- 0
  graduted <- 0
  
  active_tanks <- c()
  
  dates_in_brood <- drainDf$datetime %>% unique() %>% sort()
  
  brood_table <- tibble(
    datetime = c(spawnDf$`Date Spawned`, drainDf$datetime) %>% unique() %>% sort(),
    observed = NA,
    dead = NA,
    swimming = NA,
    graduated = NA
  )
  
  
  
  # Step 0 -- the spawnlings go into the first tank
  
  current_date <- spawnDf$`Date Spawned`
  
  
  
  swimming <- spawnDf$`eggs produced (m)`
  
  active_tanks <- drainDf[1,]$fromLocation # One active tank at first
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
    
    drainlog_indexes <- which(drainDf$datetime == current_date)
    brood_table_index <- which(brood_table$datetime == current_date)
    
    
    transferred_tanks_table <- drainDf[drainlog_indexes,c("fromLocation", "toLocation", "toSystem", "amount")]
    
    tansferred_tanks_table_summed <- transferred_tanks_table %>%
      group_by(fromLocation, toLocation, toSystem) %>%
      summarise(amount = sum(amount), .groups = "keep") %>%
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
  
  brood_table
}

# Take just brood number, subset and then run the above
process_brood <- function(broodID){
  spawn_loc <- spawnInfo00 %>% 
    filter(brood == broodID)
  
  drain_loc <- drainLogRaw01 %>%
    filter(brood == broodID)
  
  dl <- process_drain_log_brood(spawn_loc, drain_loc)
  
  dl
  
}