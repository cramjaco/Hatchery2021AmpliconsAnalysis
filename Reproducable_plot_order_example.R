# Reproducible plotting example
set.seed(10034)

nbatches = 50

data <- tibble(
  timepoint_one = runif(n = nbatches, min = 0, max = 100),
  fraction_surviving = runif(n = nbatches, min = 0, max = 1)
) %>%
 mutate(timepoint_two = timepoint_one * fraction_surviving) %>%
  arrange(timepoint_two) %>%
  mutate(outcome = if_else(timepoint_two < 2, "Bad", "Good")) %>%
  mutate(outcome = factor(outcome, levels = c("Good", "Bad"))) %>%
  mutate(batch = seq(from = 0, by = 1, length.out = nbatches), .before = everything())

data01 <- data %>%
  select(batch, timepoint_one, timepoint_two) %>%
  pivot_longer(-batch, names_to = "timepoint", values_to = "value") %>%
  left_join(data %>% select(batch, outcome), by = "batch") %>%
  mutate(timepoint = if_else(timepoint == "timepoint_one", 1, 2))

data01 %>%
  ggplot(aes(x = timepoint, y = value, color = outcome)) + 
  geom_path() +
  scale_color_manual(values = c(Good = "darkblue", Bad = "red"))

data01 %>%
  arrange(desc(outcome)) %>%
  ggplot(aes(x = timepoint, y = value, color = outcome)) + 
  geom_path() +
  scale_color_manual(values = c(Good = "darkblue", Bad = "red"))
