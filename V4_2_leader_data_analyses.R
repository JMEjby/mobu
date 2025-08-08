library(tidyverse)
library(patchwork)

if (file.exists(file = "/V4_2_leader_data/leader_sim.RData")) {
  load("/V4_2_leader_data/leader_sim.RData")
} else {
  files <- list.files(
    path = "/V4_2_leader_data",
    full.names = TRUE
  )
  
  df <- data.frame()
  
  for (f in files) {
    
    df_new <- read_csv(f)
    
    df <- rbind(df, df_new)
  }
  
  rm(df_new, files, f)
}

df <-  df |>
  mutate(mean_signals_leader = 4 - mean_signals_leader,
         mean_signals_eas = 4 - mean_signals_eas,
         mean_signals_outsiders = 4 - mean_signals_outsiders
  )

fixed_params <-  df[5*20,] |>
  select(run_id, n_agents, signal_representation, movement_saturation, log_recruitment_strictness,
         log_desertion_strictness, social_influence, n_signals, new_signal_rate, 
         leader_lifespan, n_ticks_total)

labels <- c("Number of simulations per combination", "Number of agents",
            "Signal Representation", "Movement Saturation", "log(recruitment strictness)",
            "log(desertion strictness)", "Social influence level", "Number of signals", 
            "New Signal Rate", "Lifespan of leader", "Number of ticks simulated")
suffixes <- c("", "", "", " %", "", "","", "", " %", " ticks", " ticks")

# Create caption from fixed_params
caption_text <- paste(
  paste(labels, ":", sapply(fixed_params, function(x) paste(unique(x), collapse = ", ")), suffixes, collapse = " | "),
  collapse = "\n"
)

# Helper function to format facet labels
format_facet_label <- function(x) {
  # Replace "-" with " " and capitalize first letter only
  formatted <- str_replace_all(x, "-", " ")
  str_sub(formatted, 1, 1) <- str_to_upper(str_sub(formatted, 1, 1))
  return(formatted)
}

# ==== Analysis 1: Signals =====

# Data processing
p1_data <- df |>
  select(run_id, tick, 
         relative_leader_influence, 
         leader_values,
         leader_election,
         count_eas, count_non_eas,
         mean_signals_leader, 
         mean_signals_eas, 
         mean_signals_outsiders, 
         sd_signals_eas,
         sd_signals_outsiders) |>
  rename(count_outsiders = count_non_eas) |>
  mutate(count_leader = 1,
         sd_signals_leader = 0) |>
  pivot_longer(
    cols = c(mean_signals_leader, mean_signals_eas, mean_signals_outsiders, 
             sd_signals_eas, sd_signals_outsiders, 
             count_outsiders, count_eas, count_leader),
    names_to = c(".value", "agent_type"),
    names_pattern = "(.*)_(.*)",
    values_drop_na = TRUE
  ) |>
  mutate(sum_signals = mean_signals * count)

p1_summary <- p1_data |>
  group_by(tick, relative_leader_influence, 
           leader_values, leader_election, agent_type) |>
  summarise(mean_signals = sum(sum_signals)/sum(count),
            sd_signals = sqrt(sum((sd_signals^2 * count))/sum(count)),
            total_sum_signals = mean(sum_signals)) |>
  mutate(ribbon_upper = pmin(mean_signals + sd_signals, 3),
         ribbon_lower = pmax(mean_signals - sd_signals, -2))

# Function to create plot for each agent type
create_mean_plot <- function(agent_name, show_legend = FALSE, show_caption = FALSE) {
  plot_title <- case_when(
    agent_name == "leader" ~ "Leader",
    agent_name == "eas" ~ "EAs", 
    agent_name == "outsiders" ~ "Non-EAs"
  )
  
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    # Summary ribbons
    geom_ribbon(data = p1_summary |> filter(agent_type == agent_name), 
                aes(x = tick, 
                    ymin = ribbon_lower, 
                    ymax = ribbon_upper, 
                    fill = factor(relative_leader_influence)), 
                alpha = 0.15) +
    # Individual run lines (background)
    geom_line(data = p1_data |> filter(agent_type == agent_name), 
              aes(x = tick, y = mean_signals, 
                  color = factor(relative_leader_influence), 
                  group = interaction(run_id, relative_leader_influence)), 
              alpha = 0.4, linewidth = 0.3) +
    # Summary lines
    geom_line(data = p1_summary |> filter(agent_type == agent_name), 
              aes(x = tick, y = mean_signals, 
                  color = factor(relative_leader_influence)), 
              linewidth = 1.2) +
    # Summary points
    geom_point(data = p1_summary |> filter(agent_type == agent_name), 
               aes(x = tick, y = mean_signals, 
                   color = factor(relative_leader_influence)), 
               size = 1.5) +
    # Faceting
    facet_grid(leader_values ~ leader_election, 
               labeller = labeller(
                 leader_values = function(x) paste("Leader Values:", format_facet_label(x)),
                 leader_election = function(x) paste("Leader Election:", format_facet_label(x))
               )) +
    # Scales and aesthetics
    scale_color_brewer(type = "div", palette = "Spectral", name = "Relative Leader\nInfluence (%)") +
    scale_fill_brewer(type = "div", palette = "Spectral", name = "Relative Leader\nInfluence (%)") +
    scale_y_continuous(limits = c(-2, 3)) +
    labs(x = if(!show_caption) NULL else "Time (Ticks)", 
         y = "Mean Signals", 
         caption = if(show_caption) caption_text else NULL,
         title = plot_title) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major.y = element_line(color = "grey90"),
          panel.grid.minor.y = element_line(color = "grey95"),
          panel.grid.minor.x = element_line(color = NA),          
          panel.grid.major.x = element_line(color = "grey90"),
          strip.background = element_rect(fill = "white", color = "black"),
          strip.text = element_text(size = 9, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.text = element_text(color = "black", size = 8),
          axis.title = element_text(face = "bold", size = 10),
          title = element_text(size = 11),
          plot.caption = element_text(hjust = 0, size = 7),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.position = if(show_legend) "bottom" else "none",
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8))
  
  return(p)
}

# Create plots for each agent type
p_leader <- create_mean_plot("leader")
p_eas <- create_mean_plot("eas")
p_outsiders <- create_mean_plot("outsiders")

# Combine plots using patchwork with collected guides
p_combined_means <- (p_leader | p_eas | p_outsiders) + 
  plot_annotation(caption = caption_text,
                  tag_levels = "A")+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Display and save the combined plot
print(p_combined_means)
ggsave("leader_influence_means_combined.png", p_combined_means, 
       width = 55, height = 20, dpi = 600, units = "cm")

# SECOND PLOT: Sum of signals (no ribbons, just individual lines and summary)
create_sum_plot <- function(agent_name) {
  plot_title <- case_when(
    agent_name == "leader" ~ "Leader - Sum of Signals",
    agent_name == "eas" ~ "EAs - Sum of Signals", 
    agent_name == "outsiders" ~ "Non-EAs - Sum of Signals"
  )
  
  p <- ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    # Individual run lines
    geom_line(data = p1_data |> filter(agent_type == agent_name), 
              aes(x = tick, y = sum_signals, 
                  color = factor(relative_leader_influence), 
                  group = interaction(run_id, relative_leader_influence)), 
              alpha = 0.4, linewidth = 0.3) +
    # Summary lines
    geom_line(data = p1_summary |> filter(agent_type == agent_name), 
              aes(x = tick, y = total_sum_signals, 
                  color = factor(relative_leader_influence)), 
              linewidth = 1.2) +
    # Summary points
    geom_point(data = p1_summary |> filter(agent_type == agent_name), 
               aes(x = tick, y = total_sum_signals, 
                   color = factor(relative_leader_influence)), 
               size = 1.5) +
    # Faceting
    facet_grid(leader_values ~ leader_election, 
               labeller = labeller(
                 leader_values = function(x) paste("Leader Values:", format_facet_label(x)),
                 leader_election = function(x) paste("Leader Election:", format_facet_label(x))
               )) +
    # Scales and aesthetics
    scale_color_brewer(type = "div", palette = "Spectral", name = "Relative Leader\nInfluence (%)") +
    scale_fill_brewer(type = "div", palette = "Spectral", name = "Relative Leader\nInfluence (%)") +
    labs(x = "Time (Ticks)", 
         y = "Sum of Signals", 
         title = plot_title) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major.y = element_line(color = "grey90"),
          panel.grid.minor.y = element_line(color = "grey95"),
          panel.grid.minor.x = element_line(color = NA),          
          panel.grid.major.x = element_line(color = "grey90"),
          strip.background = element_rect(fill = "white", color = "black"),
          strip.text = element_text(size = 9, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.text = element_text(color = "black", size = 8),
          axis.title = element_text(face = "bold", size = 10),
          title = element_text(size = 11),
          plot.caption = element_text(hjust = 0, size = 7),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.position = "bottom",
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8))
  
  return(p)
}

# Create sum plots for each agent type
p_leader_sum <- create_sum_plot("leader")
p_eas_sum <- create_sum_plot("eas")
p_outsiders_sum <- create_sum_plot("outsiders")

# Combine sum plots using patchwork with collected guides
p_combined_sums <- (p_leader_sum | p_eas_sum | p_outsiders_sum) + 
  plot_annotation(caption = caption_text,
                  tag_levels = "A")+
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Display and save the combined sum plot
print(p_combined_sums)

ggsave("leader_influence_sums_combined.png", p_combined_sums, 
       width = 55, height = 22, dpi = 600, units = "cm")

# ==== Analysis 2: Signal Weights =====

# Data processing for signal weights
p2_data <- df |>
  select(run_id, tick, 
         relative_leader_influence, 
         leader_values,
         leader_election,
         count_eas, count_non_eas,
         # Select all signal weight columns
         matches("(ea|nonea|leader)_signal_weight_[0-5]_(mean|sd)")) |>
  # Rename nonea to outsiders and ea to eas for consistency
  rename_with(~ str_replace(.x, "nonea_", "outsiders_"), 
              matches("nonea_signal_weight")) |>
  rename_with(~ str_replace(.x, "ea_", "eas_"), 
              matches("ea_signal_weight")) |>
  rename(count_outsiders = count_non_eas) |>
  mutate(count_leader = 1) |>
  
  # Rename counts to match signal weight pattern for consistent pivoting
  rename(eas_count = count_eas,
         outsiders_count = count_outsiders,
         leader_count = count_leader) |>
  
  # Now pivot all variables (signal weights and counts) together
  pivot_longer(
    cols = c(matches("(eas|outsiders|leader)_signal_weight_[0-5]_(mean|sd)"),
             eas_count, outsiders_count, leader_count),
    names_to = c("agent_type", ".value"),
    names_pattern = "([^_]+)_(.*)",
    values_drop_na = TRUE
  ) |>
  
  # Now pivot the signal weights to separate weight numbers from mean/sd
  pivot_longer(
    cols = matches("signal_weight_[0-5]_(mean|sd)"),
    names_to = c("signal", ".value"),
    names_pattern = "(.*)_(.*)",
    values_drop_na = TRUE
  ) |>
  
  # Apply the remapping to signal weight numbers
  mutate(
    signal= case_when(
      signal == "signal_weight_0" ~ 3,
      signal == "signal_weight_1" ~ 2,
      signal == "signal_weight_2" ~ 1,
      signal == "signal_weight_3" ~ 0,
      signal == "signal_weight_4" ~ -1,
      signal == "signal_weight_5" ~ -2
    ),
    sd = ifelse(is.na(sd), 0, sd)
  ) 

p2_summary <- p2_data |>
  group_by(tick, relative_leader_influence, 
           leader_values, leader_election, agent_type, signal) |>
  summarise(
    weighted_mean = sum(mean * count) / sum(count),
    weighted_sd = sqrt(sum((sd^2 * count)) / sum(count))
  ) |>
  mutate(
    ribbon_upper = pmin(weighted_mean + weighted_sd, 1),
    ribbon_lower = pmax(weighted_mean - weighted_sd, 0)
  )

# Also update p2_data with the same interaction column
p2_data <- p2_data |>
  mutate(leader_combo = paste(format_facet_label(leader_values), "|", format_facet_label(leader_election)))

# Function to create signal weight plot for each agent type
create_weight_plot <- function(agent_name) {
  plot_title <- case_when(
    agent_name == "leader" ~ "Leader - Signal Weights",
    agent_name == "eas" ~ "EAs - Signal Weights", 
    agent_name == "outsiders" ~ "Non-EAs - Signal Weights"
  )
  
  p <- ggplot() +
    # Summary ribbons
    geom_ribbon(data = p2_summary |> filter(agent_type == agent_name), 
                aes(x = tick, 
                    ymin = ribbon_lower, 
                    ymax = ribbon_upper, 
                    fill = factor(signal)), 
                alpha = 0.15) +
    # Individual run lines (background)
    geom_line(data = p2_data |> filter(agent_type == agent_name), 
              aes(x = tick, y = mean, 
                  color = factor(signal), 
                  group = interaction(run_id, signal)), 
              alpha = 0.4, linewidth = 0.3) +
    # Summary lines
    geom_line(data = p2_summary |> filter(agent_type == agent_name), 
              aes(x = tick, y = weighted_mean, 
                  color = factor(signal)), 
              linewidth = 1.2) +
    # Summary points
    geom_point(data = p2_summary |> filter(agent_type == agent_name), 
               aes(x = tick, y = weighted_mean, 
                   color = factor(signal)), 
               size = 1.5) +
    # Faceting - rows: relative leader influence, columns: leader values + election
    facet_grid(relative_leader_influence ~ leader_combo, 
               labeller = labeller(
                 relative_leader_influence = function(x) paste("Leader Influence:", x, "%")
               )) +
    # Scales and aesthetics
    scale_color_brewer(type = "div", palette = "RdYlBu", name = "Signal Weight\n(Mapped)") +
    scale_fill_brewer(type = "div", palette = "RdYlBu", name = "Signal Weight\n(Mapped)") +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Time (Ticks)", 
         y = "Mean Signal Weights", 
         title = plot_title) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major.y = element_line(color = "grey90"),
          panel.grid.minor.y = element_line(color = "grey95"),
          panel.grid.minor.x = element_line(color = NA),          
          panel.grid.major.x = element_line(color = "grey90"),
          strip.background = element_rect(fill = "white", color = "black"),
          strip.text = element_text(size = 7, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.text = element_text(color = "black", size = 6),
          axis.title = element_text(face = "bold", size = 8),
          title = element_text(size = 9),
          legend.background = element_rect(fill = "white", color = "black"))
  
  return(p)
}

# Create plots for each agent type - adjust text sizes for the larger grid
p2_leader <- create_weight_plot("leader")
p2_eas <- create_weight_plot("eas") 
p2_outsiders <- create_weight_plot("outsiders")

# Combine plots using patchwork - stack vertically for the larger grid
p2_combined_weights <- (p2_leader / p2_eas / p2_outsiders) + 
  plot_annotation(caption = caption_text,
                  tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Display and save the combined plot - adjust dimensions for vertical stacking
print(p2_combined_weights)
