source("Load_In_Drain_Logs.R")
source("Drain_Log_Library.R")

## Lets start with just one cast. I like 21-38 a "best" batch.

spawn2138 <- spawnInfo00 %>% 
  filter(brood == "21-38")

drain2138 <- drainLogRaw01 %>%
  filter(brood == "21-38")

dl2138 <- process_drain_log_brood(spawn2138, drain2138)



#test_brood <- process_brood("21-38")

# Not working with other broods. Probably something hard coded.
test_brood <- process_brood("21-43")

test_brood %>%
  select(-observed) %>%
  pivot_longer(dead:graduated, names_to = "status", values_to = "amount") %>%
  filter(!is.na(amount)) %>%
  ggplot(aes(x = datetime, y = amount, shape = status)) + 
  geom_point() +
  geom_path() + 
  scale_shape_manual(values = c(dead = 4, graduated = 15, swimming = 1))
