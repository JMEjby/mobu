# library(tidyverse)
# library(RColorBrewer, lib.loc = "/exports/eddie/scratch/s1917169/libs")
# 
# main_dir <- "/exports/eddie/scratch/s1917169/ea_sim_pack/analysis"
# setwd(main_dir)

#data <- read_csv("/exports/eddie/scratch/s1917169/ea_sim_pack/results/combined_aggregated_data_20250813_191224.csv")
#params <- read_csv("/exports/eddie/scratch/s1917169/ea_sim_pack/parameter_combinations.csv")

#data <- data |> rename(combo_id = "combo-id")

# ------------------------------------------------------------------------------
# Leader Election
# ------------------------------------------------------------------------------

# Number of eas ----
count_data <-  data |>
  filter(social_influence == 0.5,
         log_desertion_strictness == -0.75,
         log_recruitment_strictness == 0.5,
         relative_leader_influence == 75,
         movement_saturation == 2.5,
         leader_values == "value-profile-A",
         ea_bias == 1.5
  )|>
  mutate(leader_election = factor(leader_election, labels = c("External", "Most aligned EA", "Random EA")),
    tick = (tick+1)/2)

count_summary <- count_data |>
  group_by(tick, leader_election) |>
  summarise(mean_ea_count = mean(count_eas, na.rm = T), 
            sd_ea_count = sd(count_eas, na.rm = T), 
            .groups = "drop") |>
  mutate(ribbon_upper = mean_ea_count + sd_ea_count,
         ribbon_lower = ifelse(mean_ea_count - sd_ea_count < 0, 0, mean_ea_count - sd_ea_count))

cap_text <- paste("Simulations per combination:", max(count_data$run_id), 
                  "| Number of agents per simulation:", count_data$n_agents[1], 
                  "| Initial movement saturation:", count_data$movement_saturation[1], " %",
                  "| Signal representation:", str_to_title(count_data$signal_representation[1]),
                  "|\nRelative leader influence:", count_data$relative_leader_influence[1], " %",
                  "| Recruitment strictness level:", count_data$log_recruitment_strictness[1],
                  "| Desertion strictness level:", count_data$log_desertion_strictness[1],
                  "| Social influence Level:", count_data$social_influence[1],
                  "| Leader values:", str_to_title(gsub(pattern = "-",  " ", count_data$leader_values[1])),
                  "|\nEA value bias:", count_data$ea_bias[1],
                  "| Population Composition:", str_to_title(gsub(pattern = "-",  " ", count_data$population_composition[1])))

ggplot() +
  geom_line(data = count_data, 
            aes(x = tick, y = count_eas, 
                color = leader_election, 
                group = interaction(combo_id, run_id, leader_election)), 
            alpha = 0.2) +
  geom_ribbon(data = count_summary, 
              aes(x = tick, 
                  ymin = ribbon_lower, 
                  ymax = ribbon_upper, 
                  fill = leader_election), 
              alpha = 0.3) +
  geom_line(data = count_summary, 
            aes(x = tick, y = mean_ea_count, color = leader_election), 
            linewidth = 1.2) +
  geom_point(data = count_summary, 
             aes(x = tick, y = mean_ea_count, color = leader_election), 
             size = 1.5) +
  scale_x_continuous(breaks = unique(count_data$tick)) +
  scale_y_continuous(limits = c(0, 310)) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Leader \nElection \nMethod") +
  scale_fill_brewer(type = "qual", palette = "Set1", name = "Leader \nElection \nMethod") +
  labs(x = "Years", 
       y = "Number of EAs",
       caption = cap_text,
       title = "Number of EAs over time by Leader Election Method") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "lightgrey"),
        panel.grid.minor.x = element_line(color = NA),          
        panel.grid.major.x = element_line(color = NA),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 10),
        axis.text.x = element_text(angle = 20),
        axis.title = element_text(face = "bold", size = 11),
        title = element_text(size = 10),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = "bottom")

ggsave("./figures/leader_election_number_of_eas.png",
       width = 30, height = 16, dpi = 600, units = "cm")

# Number of graduating eas ----
grad_count_data <-  data |>
  filter(social_influence == 0.5,
         log_recruitment_strictness == 0.5,
         relative_leader_influence == 75,
         movement_saturation == 2.5,
         leader_values == "value-profile-A",
         log_desertion_strictness == -0.75,
         ea_bias == 1.5,
         tick %in% seq(1,32,2) # semester 2 sims
  )|>
  mutate(leader_election = factor(leader_election, labels = c("External", "Most aligned EA", "Random EA")),
         tick = (tick + 1)/2)

cap_text <- paste("Simulations per combination:", max(grad_count_data$run_id), 
                  "| Number of agents per simulation:", grad_count_data$n_agents[1], 
                  "| Initial movement saturation:", grad_count_data$movement_saturation[1], " %",
                  "| Signal representation:", str_to_title(grad_count_data$signal_representation[1]),
                  "|\nRelative leader influence:", grad_count_data$relative_leader_influence[1], " %",
                  "| Recruitment strictness level:", grad_count_data$log_recruitment_strictness[1],
                  "| Desertion strictness level:", grad_count_data$log_desertion_strictness[1],
                  "| Social influence Level:", grad_count_data$social_influence[1],
                  "| Leader values:", str_to_title(gsub(pattern = "-",  " ", grad_count_data$leader_values[1])),
                  "|\nEA value bias:", grad_count_data$ea_bias[1],
                  "| Population Composition:", str_to_title(gsub(pattern = "-",  " ", grad_count_data$population_composition[1])))

grad_count_summary <- grad_count_data |>
  group_by(tick, leader_election) |>
  summarise(mean_ea_year4 = mean(count_year4_eas), 
            sd_ea_year4 = sd(count_year4_eas), 
            .groups = "drop") |>
  mutate(ribbon_upper = mean_ea_year4 + sd_ea_year4,
         ribbon_lower = ifelse(mean_ea_year4 - sd_ea_year4 < 0, 0, mean_ea_year4 - sd_ea_year4))

ggplot() +
  geom_line(data = grad_count_data, 
            aes(x = tick, y = count_year4_eas, 
                color = leader_election, 
                group = interaction(combo_id, run_id, leader_election)), 
            alpha = 0.2) +
  geom_ribbon(data = grad_count_summary, 
              aes(x = tick, 
                  ymin = ribbon_lower, 
                  ymax = ribbon_upper, 
                  fill = leader_election), 
              alpha = 0.3) +
  geom_line(data = grad_count_summary, 
            aes(x = tick, y = mean_ea_year4, color = leader_election), 
            linewidth = 1.2) +
  geom_point(data = grad_count_summary, 
             aes(x = tick, y = mean_ea_year4, color = leader_election), 
             size = 1.5) +
  scale_x_continuous(breaks = unique(grad_count_data$tick), 
                     limits = c(1, 16)) +
  scale_y_continuous(limits = c(0, 110)) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Leader \nElection \nMethod") +
  scale_fill_brewer(type = "qual", palette = "Set1", name = "Leader \nElection \nMethod") +
  labs(x = "Years (Semester 2 only)", 
       y = "Number of Graduating EAs",
       caption = cap_text,
       title = "Number of EAs Graduating at the end of each year by Leader Election") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "lightgrey"),
        panel.grid.minor.x = element_line(color = NA),          
        panel.grid.major.x = element_line(color = NA),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        title = element_text(size = 10),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = "bottom")

ggsave("./figures/leader_election_number_of_ea_grads.png",
       width = 22, height = 16, dpi = 600, units = "cm")

# Sum of signals ----

#mean_signals_eas * 5 * count_eas

# aggregate expected value of their next action ----
exp_data <-  data |>
  filter(social_influence == 0.5,
         log_recruitment_strictness == 0.5,
         relative_leader_influence == 75,
         movement_saturation == 2.5,
         leader_values == "value-profile-A",
         log_desertion_strictness == -0.75,
         ea_bias == 1.5
  )|>
  mutate(leader_election = factor(leader_election, labels = c("External", "Most aligned EA", "Random EA")),
         tick = (tick + 1)/2) 

cap_text <- paste("Simulations per combination:", max(exp_data$run_id), 
                  "| Number of agents per simulation:", exp_data$n_agents[1], 
                  "| Initial movement saturation:", exp_data$movement_saturation[1], " %",
                  "|\nSignal representation:", str_to_title(exp_data$signal_representation[1]),
                  "| Relative leader influence level:", exp_data$relative_leader_influence[1], " %",
                  "| Recruitment strictness level:", exp_data$log_recruitment_strictness[1],                  
                  "| Desertion strictness level:", exp_data$log_desertion_strictness[1],
                  "|\nSocial influence Level:", exp_data$social_influence[1],
                  "| Leader values:", str_to_title(gsub(pattern = "-",  " ", exp_data$leader_values[1])),
                  "| EA value bias:", exp_data$ea_bias[1],
                  "|\nPopulation Composition:", str_to_title(gsub(pattern = "-",  " ", exp_data$population_composition[1])))

# Pivot the mean_signal_weight columns
df_long <- exp_data |> 
  select(combo_id, run_id,tick, 
         leader_election, 
         starts_with("mean_signal_weight_"), 
         count_eas, count_noneas
  )|>
  pivot_longer(
    cols = starts_with("mean_signal_weight_"),
    names_to = c("signal", "type"),
    names_pattern = "mean_signal_weight_(.+)_(.+)",
    values_to = "sum_signal_weight"
  ) |>
  # Convert signal values (neg1 -> -1, neg2 -> -2)
  mutate(
    signal = case_when(
      signal == "neg1" ~ -1,
      signal == "neg2" ~ -2,
      TRUE ~ as.numeric(signal)
    )
  ) |> filter(type != "leader")

# Pivot the count columns separately
df_counts <- exp_data |>
  select(combo_id, run_id, tick, leader_election, count_eas, count_noneas) |>
  pivot_longer(
    cols = starts_with("count_"),
    names_to = "type",
    names_pattern = "count_(.+)",
    values_to = "count"
  ) |>
  # Rename eas to match the type column from mean_signal_weight
  mutate(type = ifelse(type == "eas", "eas", "noneas"))

# Join the datasets
exp_data <- df_long |>
  left_join(df_counts, by = c("combo_id", "run_id", "tick","leader_election","type")) |>
  mutate(
    sum_signal_weights = sum_signal_weight * count,
    signal_weighting = case_when(
      signal == 3 ~ 100,
      signal == 2 ~ 1,
      signal == 1 ~ 0.1,
      signal == -1 ~ -0.1,
      signal == -2 ~ -1,
      .default = signal,
      TRUE ~ as.numeric(signal)
    ),
    exp_value = signal_weighting*sum_signal_weight) |>
  group_by(combo_id, run_id, tick,leader_election,type) |>
  summarise(exp_value = sum(exp_value))

df_total <- exp_data |>
  group_by(combo_id, run_id, tick,leader_election) |>
  summarise(exp_value = sum(exp_value)) |>
  mutate(type = "total")

exp_data <- rbind(exp_data, df_total) |>
  filter(type != "noneas")

rm(df_long)
rm(df_counts)
rm(df_total)

exp_summary <- exp_data |>
  group_by(tick, leader_election, type) |>
  summarise(mean_exp_value = mean(exp_value, na.rm = T), 
            sd_exp_value = sd(exp_value, na.rm = T), 
            .groups = "drop") |>
  mutate(ribbon_upper = ifelse(mean_exp_value + sd_exp_value > 100, 100, mean_exp_value + sd_exp_value),
         ribbon_lower = ifelse(mean_exp_value - sd_exp_value < -10, -10, mean_exp_value - sd_exp_value))

labs <- c(eas = "EAs",
          total = "All Agents")

ggplot() +
  geom_line(data = exp_data, 
            aes(x = tick, y = exp_value, 
                color = leader_election, 
                group = interaction(combo_id, run_id, leader_election)), 
            alpha = 0.2) +
  geom_ribbon(data = exp_summary, 
              aes(x = tick, 
                  ymin = ribbon_lower, 
                  ymax = ribbon_upper, 
                  fill = leader_election), 
              alpha = 0.3) +
  geom_line(data = exp_summary, 
            aes(x = tick, y = mean_exp_value, color = leader_election), 
            linewidth = 1.2) +
  geom_point(data = exp_summary, 
             aes(x = tick, y = mean_exp_value, color = leader_election), 
             size = 1.5) +
  facet_wrap(~type, ncol = 1, labeller = labeller(type = labs))+
  scale_x_continuous(breaks = seq(min(exp_data$tick), max(exp_data$tick), by = 1)) +
  scale_y_continuous(limits = c(ifelse(min(exp_data$exp_value) > 0, 0, min(exp_data$exp_value)), max(exp_data$exp_value))) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Leader \nElection \nMethod") +
  scale_fill_brewer(type = "qual", palette = "Set1", name = "Leader \nElection \nMethod") +
  labs(x = "Years", 
       y = "Expected Signal Value",
       caption = cap_text,
       title = "Aggregated Expected Signal Value over time by Leader Election Method") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "lightgrey"),
        panel.grid.minor.x = element_line(color = NA),          
        panel.grid.major.x = element_line(color = NA),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        title = element_text(size = 10),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = "bottom")

ggsave("./figures/leader_election_agg_expected_value.png",
       width = 20, height = 30, dpi = 600, units = "cm")
