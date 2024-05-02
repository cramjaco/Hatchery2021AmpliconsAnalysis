library(here)
source(here("RScripts", "Load_In_Drain_Logs.R"))
source(here("RScripts", "Drain_Log_Library.R"))

library(ggrepel)

## Lets start with just one cast. I like 21-38 a "best" batch.

spawn2138 <- spawnInfo00 %>% 
  filter(brood == "21-38")

drain2138 <- drainLogRaw01 %>%
  filter(brood == "21-38")

dl2138 <- process_drain_log_brood(spawn2138, drain2138)



#test_brood <- process_brood("21-38")

# Not working with other broods. Probably something hard coded.
test_brood <- process_brood("21-43") # odd dips in swimmers and dead
test_brood <- process_brood("21-23")
test_brood <- process_brood("21-38")

test_brood %>%
  select(-observed) %>%
  pivot_longer(dead:graduated, names_to = "status", values_to = "amount") %>%
  filter(!is.na(amount)) %>%
  ggplot(aes(x = datetime, y = amount, shape = status)) + 
  geom_point() +
  geom_path() + 
  scale_shape_manual(values = c(dead = 4, graduated = 15, swimming = 1))

all_broods <- drainLogRaw01$brood %>% unique() %>% tibble(brood = .) %>%
  filter(str_detect(brood, "^21-\\d{2}$"))

## 23-39 does not exist. Don't use. Currently just mandating things start with 21. A problem later.

safe_process_brood <- possibly(process_brood)

pt0 <- proc.time()
broods_df <- all_broods %>%
  mutate(survivorship = map(brood, safe_process_brood))
pt1 <- proc.time()
pt1 - pt0

survivorship_df <- broods_df %>%
  unnest(survivorship)

## process ages

survivorship_df_01 <- survivorship_df %>%
  left_join(spawnInfo00, by = "brood") %>%
  mutate(age = datetime - `Date Spawned`) %>%
  mutate(surviving = swimming + graduated) %>%
  mutate(fraction_surviving = surviving / `eggs produced (m)`) %>%
  mutate(age_days = as.numeric(age)/60/60/24)

write_csv(survivorship_df_01, here("IntermediateData", "Survivorship2021.csv"))

## Outcomes of every brood in the whole year.

# outcomes <- survivorship_df_01 %>%
#   group_by(brood) %>%
#   summarise(survived_by_end = min(na.omit(surviving)),
#             graduated_by_end = max(na.omit(graduated)))


data_ends <- survivorship_df_01 %>%
  #filter(age_days <= 12) %>%
  group_by(brood) %>%
  top_n(1,age_days) %>%
  ungroup() %>%
  mutate(outcome = if_else(graduated > 10, "Good", "Crash")) %>%
  mutate(outcome = factor(outcome, levels = c("Good", "Crash"))) %>%
  identity

## Summary statistics
data_ends %>%
  filter(!is.na(graduated)) %>%
  summarise(total_plantable_larvae = sum((graduated)),
            min_plantable_larvae = min((graduated)),
            max_plantable_larvae = max((graduated)),
            median_plantable_larvae = median((graduated)),
            mad_plantable_larvae = mad((graduated)),
            n_broods = n())

data_ends %>%
  filter(graduated <= 1) %>%
  summarise(n())

data_ends %>%
  filter(graduated == 0) %>%
  summarise(n())

spawnInfo00 %>%
  filter(!is.na(`eggs produced (m)`)) %>%
  summarise(across(`eggs produced (m)`, list(sum = sum, min = min, max = max, median = median, mad = mad)))

outcomes <- data_ends %>%
  select(brood, outcome)

survivorship_df_01_woutcomes <- survivorship_df_01 %>%
  ungroup() %>%
  left_join(outcomes, by = "brood") %>%
  arrange((outcome))



frac_survive_plot <- survivorship_df_01_woutcomes %>%
  na.omit() %>% 
  ggplot(aes(x = age_days, y = fraction_surviving, .group = brood, color = outcome, shape = outcome)) +
  geom_point(aes(), na.rm = TRUE, data = data_ends, size = 2) +
  geom_path(alpha = 0.5) +
  #geom_text_repel(aes(label = brood), nudge_x = 1, na.rm = TRUE, color = "navy", data = data_ends) +
  #xlim(0, 12) +
  labs(x = "Age (Days)", y = "Incubating + Harvested Larvae", color = "Batch Outcome") +
  scale_color_manual(values = c(Good = "darkblue", Crash = "red")) + 
  scale_shape_manual(name = "Batch Outcome", values = c(1,4)) + 
  theme_bw()
frac_survive_plot


tot_survive_plot<- survivorship_df_01_woutcomes %>%
  na.omit() %>%
  #mutate(ordered = fct_reorder(ordered, desc(ordered))) %>%
  ggplot(aes(x = age_days, y = surviving, .group = brood, color = outcome, shape = outcome, alpha = outcome, linewidth = outcome)) +
  # geom_path(data = survivorship_df_01_woutcomes %>% filter(outcome == "Good")) +
  geom_path() +
  geom_path(data = survivorship_df_01_woutcomes %>% filter(outcome == "Crash")) +
  geom_point(aes(), na.rm = TRUE, data = data_ends, size = 3) +
  geom_hline(yintercept = 10, color = "brown", linetype = "dashed", stroke = 2) + 
  #geom_text_repel(aes(label = brood), nudge_x = 1, na.rm = TRUE, color = "navy", data = data_ends) +
  labs(x = "Age (Days)", y = "Incubating + Harvested Larvae \n (millions)") +
  scale_x_continuous(breaks = seq(from = 0, to = 35, by = 3)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 1800, by = 200)) +
  scale_color_manual(name = "Batch Outcome", values = c(Good = "darkblue", Crash = "red")) + 
  scale_shape_manual(name = "Batch Outcome", values = c(Good = 1, Crash = 15)) + 
  scale_alpha_manual(name = "Batch Outcome", values = c(Good = 1, Crash = 1)) + 
  scale_linewidth_manual(name = "Batch Outcome", values = c(Good = .5,Crash = .75)) + 
  #xlim(0, 12) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.85, 0.5),
        legend.box.background = element_rect(color = "black", size = 2)
  )
tot_survive_plot

tot_survive_plot + scale_y_log10() ## Josh W

ggsave(here("Figures", "Total2021Survival.png"), tot_survive_plot, width = 7, height = 4)

all_2021_survival <- cowplot::plot_grid(tot_survive_plot, frac_survive_plot)
ggsave(here("Figures", "2021Survival.png"), all_2021_survival, width = 8, height = 4)


## Stuff I want to do
## Flag my samples -- as crashes and not
## Show just my samples -- as crashes and not


## 
cram_brood_info <- read_csv(here("Hatchery_UVexperiment_All", "brood_info_2.csv"))

survivorship_df_02 <- survivorship_df_01 %>%
  left_join(cram_brood_info, by = c("brood" = "Brood#"))

data_ends_jac <- survivorship_df_02 %>%
  #filter(age_days <= 12) %>%
  group_by(brood) %>%
  top_n(1,age_days) %>%
  filter(!is.na(Info2) & Info2 != "Main")

## Summary statistics
data_ends_jac %>%
  ungroup() %>% 
  left_join(outcomes, by = "brood") %>%
  group_by(outcome) %>%
  summarise(across(graduated, list(min = min, max = max, median = median, mad = mad, length)))

## Debugging narrowness
data_ends_jac %>% 
  ggplot(aes(x = age_days, y = surviving)) + geom_point()

fracPlot <- survivorship_df_02 %>%
  filter(!is.na(surviving)) %>%
  filter(!is.na(Info2) & Info2 != "Main") %>%
  mutate(Info2 = factor(Info2, levels = c("Best", "Ok", "Poor", "NoSurvivors"))) %>% 
  ggplot(aes(x = age_days, y = fraction_surviving, .group = brood, color = Info1)) +
  geom_path(alpha = 1, lwd = .75) +
  geom_text_repel(aes(label = brood), nudge_x = 1, na.rm = TRUE, color = "gray30", data = data_ends_jac, max.overlaps = 20) +
  #scale_color_manual(values = c("black", "blue", "goldenrod", "red")) +
  scale_color_manual(values = c(Good = "darkblue", Crash = "red")) + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Age (Days)", y = "Fraction Incubating + Harvested") +
  #xlim(0, 12) +
  theme_bw(base_size = 14)

totPlot <- survivorship_df_02 %>%
  filter(!is.na(surviving)) %>%
  filter(!is.na(Info2) & Info2 != "Main") %>%
  mutate(Info2 = factor(Info2, levels = c("Best", "Ok", "Poor", "NoSurvivors"))) %>% 
  ggplot(aes(x = age_days, y = surviving, .group = brood, color = Info1)) +
  geom_path(alpha = 1, lwd = .75) +
  #geom_text_repel(aes(label = brood), nudge_x = 1, na.rm = TRUE, color = "gray30", data = data_ends_jac, max.overlaps = 20) +
  #scale_color_manual(name = "Run Outcome",values = c("black", "blue", "goldenrod", "red")) +
  scale_color_manual(values = c(Good = "darkblue", Crash = "red")) + 
  scale_y_continuous() +
  labs(x = "Age (Days)", y = "Incubating + Harvested Larvae") +
  #xlim(0, 12) +
  theme_bw(base_size = 14)
totPlot

my_legend <- cowplot::get_legend(totPlot)

amplicon_samples_survival <- cowplot::plot_grid(totPlot + theme(legend.position = "none"),
                                                fracPlot + theme(legend.position = "none"),
                                                my_legend,
                                                nrow = 1,
                                                rel_widths = c(3,3,1),
                                                labels = c("A", "B", ""))
amplicon_samples_survival

ggsave(here("Figures", "CombinedAmpliconSamplesSurvival.png"), amplicon_samples_survival, width = 10, height = 4)

totPlotSG <- survivorship_df_02 %>%
  filter(!is.na(surviving)) %>%
  filter(!is.na(Info2) & Info2 != "Main") %>%
  mutate(Info2 = factor(Info2, levels = c("Best", "Ok", "Poor", "NoSurvivors"))) %>% 
  mutate(Info1 = factor(Info1, levels = c("Good", "Crash"))) %>% 
  ggplot(aes(x = age_days, y = surviving, .group = brood, color = Info1, shape = Info1)) +
  geom_path(alpha = 1, lwd = .75) +
  #geom_point() +
  geom_point(aes(), na.rm = TRUE, data = data_ends_jac, size = 3) +
  #geom_text_repel(aes(label = brood), nudge_x = 1, na.rm = TRUE, color = "gray30", data = data_ends_jac, max.overlaps = 20) +
  #scale_color_manual(name = "Run Outcome",values = c("black", "blue", "goldenrod", "red")) +
  scale_color_manual(name = "Batch Outcome", values = c(Good = "darkblue", Crash = "red")) + 
  scale_shape_manual(name = "Batch Outcome", values = c(1,4)) + 
  scale_x_continuous(breaks = seq(from = 0, to = 27, by = 3)) +
  labs(x = "Age (Days)", y = "Incubating + Harvested Larvae \n (millions)") +
  #xlim(0, 12) +
  theme_bw(base_size = 14) +
  theme(legend.position = c(.85, 0.5),
        legend.box.background = element_rect(color = "black", size = 2)
  )
totPlotSG

ggsave(here("Figures", "2021AmpliconSurvival.png"), totPlotSG, width = 7, height = 4)
ggsave(here("Figures", "Fig1_2021AmpliconSurvival.jpg"), totPlotSG, width = 7, height = 4)

## As above but log transfomed
totPlotSG_log <- totPlotSG +
  scale_y_log10() +
  theme(legend.position = c(.85, .25))
ggsave(here("Figures", "2021AmpliconSurvival_log10.png"), totPlotSG_log, width = 7, height = 4)
ggsave(here("Figures", "Fig1_2021AmpliconSurvival_log10.jpeg"), totPlotSG_log, width = 7, height = 4)

harvested <- survivorship_df_02 %>%
  filter(!is.na(surviving)) %>%
  filter(!is.na(Info2) & Info2 != "Main") %>%
  mutate(Info2 = factor(Info2, levels = c("Best", "Ok", "Poor", "NoSurvivors"))) %>% 
  ggplot(aes(x = age_days, y = graduated, .group = brood, color = Info1)) +
  geom_path(alpha = 1, lwd = .75) +
  #geom_point() +
  #geom_text_repel(aes(label = brood), nudge_x = 1, na.rm = TRUE, color = "gray30", data = data_ends_jac, max.overlaps = 20) +
  #scale_color_manual(name = "Run Outcome",values = c("black", "blue", "goldenrod", "red")) +
  scale_color_manual(values = c(Good = "darkblue", Crash = "red")) + 
  scale_x_continuous(breaks = seq(from = 0, to = 20, by = 3)) +
  #scale_y_log10() + 
  labs(x = "Age (Days)", y = "Harvested Larvae") +
  #xlim(0, 12) +
  theme_bw(base_size = 14)
harvested


survivorship_df_01 %>%
  ggplot(aes(x = age_days, y = graduated, .group = brood, color = datetime)) +
  geom_path() +
  scale_color_datetime() #+
  #scale_y_log10()
  

# we kind of care about how many "graduate" by 16 days or some other threshold

## Matt, does slope of decrease at beginning predict end result?
