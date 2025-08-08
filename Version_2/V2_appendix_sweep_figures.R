# Load required libraries
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(viridis)

# Read the data
# note this dataset was overwritten by another sweep, so I do not have it anymore :( 
# you can recreate that sweep using the V2 model to run the code below
# all the figures generated from this code are saved as images
df <- read_csv("V2 data and figures/sweep_data.csv")
df$n_agents <- factor(df$n_agents)

# Create method combination variable (now directly from aggregation_method)
df$method_combo <- case_when(
  df$aggregation_method == 'average' ~ 'Average',
  df$aggregation_method == 'product' ~ 'Product',
  TRUE ~ df$aggregation_method  # fallback in case of unexpected values
)

# Order the factor so product is last
df$method_combo <- factor(df$method_combo, 
                          levels = c('Average', 'Product'))

# Create signal representation labels
df$signal_rep_label <- case_when(
  df$signal_representation == 'binary' ~ 'Binary Signals',
  df$signal_representation == 'categorical-dirichlet' ~ 'Categorical Signals',
  TRUE ~ df$signal_representation
)

# Get number of simulations per combination
runs_per_combo <- max(df$run_id)

# ============================================================================
# PLOT 1: Current plot with equal weighting (relative_leader_influence = 50)
# ============================================================================

# Filter for equal weighting condition
df_equal <- df |> 
  filter(relative_leader_influence == 50)

# Calculate summary statistics for equal weighting
summary_df_equal <- df_equal |> 
  group_by(n_agents, movement_saturation, method_combo, signal_rep_label) |> 
  summarise(mean_cr = mean(conversion_rate), 
            sd_cr = sd(conversion_rate), 
            .groups = "drop") |>
  mutate(ribbon_upper = ifelse(mean_cr + sd_cr > 1, 1, mean_cr + sd_cr),
         ribbon_lower = ifelse(mean_cr - sd_cr < 0, 0, mean_cr - sd_cr))

# Create custom facet labels for n_agents
agent_facet_labels <- function(x) {
  paste(x, "agents")
}

# Create the equal weighting plot
p1 <- ggplot() +
  geom_line(data = df_equal, aes(x = movement_saturation, y = conversion_rate, 
                                 color = signal_rep_label, 
                                 group = interaction(run_id, signal_rep_label)), 
            alpha = 0.6) +
  geom_ribbon(data = summary_df_equal, 
              aes(x = movement_saturation, 
                  ymin = ribbon_lower, 
                  ymax = ribbon_upper, 
                  fill = signal_rep_label), 
              alpha = 0.4) +
  geom_line(data = summary_df_equal, 
            aes(x = movement_saturation, y = mean_cr, color = signal_rep_label), 
            linewidth = 1) +
  geom_point(data = summary_df_equal, 
             aes(x = movement_saturation, y = mean_cr, color = signal_rep_label), 
             size = 1.8) +
  facet_grid(n_agents ~ method_combo , 
             labeller = labeller(n_agents = agent_facet_labels)) +
  scale_x_continuous(breaks = unique(df$movement_saturation), 
                     limits = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_color_brewer(type = "qual", palette = "Set1", name = "Signal \nRepresentation") +
  scale_fill_brewer(type = "qual", palette = "Set1", name = "Signal \nRepresentation") +
  labs(x = "Movement Saturation (%)", 
       y = "Conversion Rate", 
       title = "Conversion Rate by Movement Saturation (Equal Leader Weighting)",
       caption = paste("Simulations per combination:", runs_per_combo, "| Leader influence = 50%")) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y  = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "lightgrey"),
        panel.grid.minor.x  = element_line(color = NA),
        panel.grid.major.x = element_line(color = NA),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        title = element_text(size = 11),
        plot.caption = element_text(hjust = 0, size = 10))

# Save the equal weighting plot
ggsave("sweep_results_equal_weighting.png", p1, width = 15.9, height = 21, dpi = 600, units = "cm")

# ============================================================================
# PLOT 2: Leader influence analysis with binned movement saturation
# ============================================================================

# Create binned movement saturation variable
df_binned <- df |>
  filter(movement_saturation %in% c(5, 25, 45, 55, 75, 95)) |>
  mutate(mov_sat_bin = factor(paste0(movement_saturation, "%"), 
                              levels = c("5%", "25%", "45%", "55%", "75%", "95%")))

# Calculate summary statistics for leader influence analysis
summary_df_leader <- df_binned |>
  group_by(n_agents, relative_leader_influence, method_combo, signal_rep_label, mov_sat_bin) |>
  summarise(mean_cr = mean(conversion_rate), 
            sd_cr = sd(conversion_rate), 
            .groups = "drop") |>
  mutate(ribbon_upper = ifelse(mean_cr + sd_cr > 1, 1, mean_cr + sd_cr),
         ribbon_lower = ifelse(mean_cr - sd_cr < 0, 0, mean_cr - sd_cr))

# Create a function to make plots for each aggregation method
create_leader_plot <- function(method_name) {
  data_subset <- summary_df_leader |> filter(method_combo == method_name)
  raw_data_subset <- df_binned |> filter(method_combo == method_name)
  
  ggplot() +
    geom_line(data = raw_data_subset, 
              aes(x = relative_leader_influence, y = conversion_rate, 
                  color = mov_sat_bin, 
                  group = interaction(run_id, mov_sat_bin)), 
              alpha = 0.4) +
    geom_ribbon(data = data_subset, 
                aes(x = relative_leader_influence, 
                    ymin = ribbon_lower, 
                    ymax = ribbon_upper, 
                    fill = mov_sat_bin), 
                alpha = 0.3) +
    geom_line(data = data_subset, 
              aes(x = relative_leader_influence, y = mean_cr, color = mov_sat_bin), 
              size = 1) +
    geom_point(data = data_subset, 
               aes(x = relative_leader_influence, y = mean_cr, color = mov_sat_bin), 
               size = 1.5) +
    #facet_grid( n_agents ~ signal_rep_label, 
               #labeller = labeller(n_agents = agent_facet_labels)) +
    scale_x_continuous(breaks = unique(df$relative_leader_influence), 
                       limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), 
                       limits = c(0, 1)) +
    scale_color_brewer(type = "qual", palette = "Spectral", name = "Movement \nSaturation") +
    scale_fill_brewer(type = "qual", palette = "Spectral", name = "Movement \nSaturation") +
    labs(x = "Relative Leader Influence (%)", 
         y = "Conversion Rate", 
         title = paste("Leader Influence Effect:", method_name)) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major.y  = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "lightgrey"),
          panel.grid.minor.x  = element_line(color = NA),          panel.grid.major.x = element_line(color = NA),
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
}

# Create plots for each aggregation method
p2_sum <- create_leader_plot("Sum")
p2_product <- create_leader_plot("Product")

# Combine the three plots side by side using patchwork
p2_combined <- p2_sum + p2_product + 
  plot_layout(ncol = 2, guides='collect')+
  plot_annotation(
    tag_levels = c("A", "B"),
    title = "Effect of Relative Leader Influence on Conversion Rate",
    caption = paste("Simulations per combination:", runs_per_combo),
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.caption = element_text(hjust = 0.5, size = 10),
                  legend.position = "bottom"
  ))

# Save the leader influence analysis plot
ggsave("leader_influence_analysis.png", p2_combined, width = 30, height = 30, dpi = 600, units = "cm")

# Save the individual plots
ggsave("leader_influence_analysis_sum.png", p2_sum, width = 15.9, height = 21, dpi = 600, units = "cm")
ggsave("leader_influence_analysis_product.png", p2_product, width = 15.9, height = 21, dpi = 600, units = "cm")


# =======================================================================================
# PLOT 3: Leader influence analysis with binned movement saturation (Binary signals only)
# =======================================================================================

# Create a function to make plots for each aggregation method
create_leader_plot <- function(method_name) {
  data_subset <- summary_df_leader |> 
    filter(method_combo == method_name & signal_rep_label == "Binary Signals")
  raw_data_subset <- df_binned |> 
    filter(method_combo == method_name & signal_rep_label == "Binary Signals")
  
  ggplot() +
    geom_line(data = raw_data_subset, 
              aes(x = relative_leader_influence, y = conversion_rate, 
                  color = mov_sat_bin, 
                  group = interaction(run_id, mov_sat_bin)), 
              alpha = 0.4) +
    geom_ribbon(data = data_subset, 
                aes(x = relative_leader_influence, 
                    ymin = ribbon_lower, 
                    ymax = ribbon_upper, 
                    fill = mov_sat_bin), 
                alpha = 0.3) +
    geom_line(data = data_subset, 
              aes(x = relative_leader_influence, y = mean_cr, color = mov_sat_bin), 
              size = 1) +
    geom_point(data = data_subset, 
               aes(x = relative_leader_influence, y = mean_cr, color = mov_sat_bin), 
               size = 1.5) +
    facet_wrap(~ n_agents, ncol = 2, 
               labeller = labeller(n_agents = agent_facet_labels)) +
    scale_x_continuous(breaks = unique(df$relative_leader_influence), 
                       limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), 
                       limits = c(0, 1)) +
    scale_color_brewer(type = "qual", palette = "Spectral", name = "Movement \nSaturation") +
    scale_fill_brewer(type = "qual", palette = "Spectral", name = "Movement \nSaturation") +
    labs(x = "Relative Leader Influence (%)", 
         y = "Conversion Rate", 
         title = paste("Leader Influence Effect:", method_name)) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major.y  = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "lightgrey"),
          panel.grid.minor.x  = element_line(color = NA),
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
}

# Create plots for each aggregation method
p3_sum <- create_leader_plot("Sum")
p3_product <- create_leader_plot("Product")

# Combine the three plots side by side using patchwork
p3_combined <- p3_sum + p3_product + 
  plot_layout(ncol = 2, guides='collect')+
  plot_annotation(
    title = "Effect of Relative Leader Influence on Conversion Rate (Binary Signal)",
    caption = paste("Simulations per combination:", runs_per_combo),
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.caption = element_text(hjust = 0.5, size = 10),
                  legend.position = "bottom"
                  ))

# Save the leader influence analysis plot
ggsave("leader_influence_analysis_binary.png", p3_combined, width = 30, height = 30, dpi = 600, units = "cm")

# Save the individual plots
ggsave("fig9_sum.png", p3_sum, width = 15.9, height = 21, dpi = 600, units = "cm")
ggsave("fig9_product.png", p3_product, width = 15.9, height = 21, dpi = 600, units = "cm")

# =======================================================================================
# END OF SCRIPT
# =======================================================================================