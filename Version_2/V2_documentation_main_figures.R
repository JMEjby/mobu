# ==============================================================================
# LEADER INFLUENCE ANALYSIS: Movement Saturation and Value Profile Effects
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP AND DATA LOADING
# ------------------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(cowplot)
library(mgcv)
library(viridis)

# Read the data
df <- read_csv("V2 data and figures/sweep_data.csv")

# Basic data preparation
df$n_agents <- factor(df$n_agents)

df <- df |>
  mutate(
    total_converted = converted_A + converted_B + converted_C + converted_D + converted_E,
    total_agents = total_A + total_B + total_C + total_D + total_E,
    total_unconverted = total_agents - total_converted
  )

# Create signal representation labels
df$signal_rep_label <- case_when(
  df$signal_representation == 'binary' ~ 'Binary Signals',
  df$signal_representation == 'categorical-dirichlet' ~ 'Categorical Signals',
  TRUE ~ df$signal_representation
)

# ------------------------------------------------------------------------------
# ANALYSIS 1: OVERALL LEADER INFLUENCE BY MOVEMENT SATURATION
# ------------------------------------------------------------------------------

# Create binned movement saturation variable
df_binned <- df |>
  filter(movement_saturation %in% c(5, 25, 45, 55, 75, 95)) |>
  mutate(mov_sat_bin = factor(paste0(movement_saturation, "%"), 
                              levels = c("5%", "25%", "45%", "55%", "75%", "95%")))

# Calculate summary statistics for leader influence analysis
summary_df_leader <- df_binned |>
  group_by(n_agents, relative_leader_influence, mov_sat_bin) |>
  summarise(mean_cr = mean(conversion_rate), 
            sd_cr = sd(conversion_rate), 
            .groups = "drop") |>
  mutate(ribbon_upper = ifelse(mean_cr + sd_cr > 1, 1, mean_cr + sd_cr),
         ribbon_lower = ifelse(mean_cr - sd_cr < 0, 0, mean_cr - sd_cr))

# Get metadata for captions
runs_per_combo <- max(df$run_id)
n_agents_value <- df$n_agents[1]

# Create main leader influence plot
p1 <- ggplot() +
  geom_line(data = df_binned, 
            aes(x = relative_leader_influence, y = conversion_rate, 
                color = mov_sat_bin, 
                group = interaction(run_id, mov_sat_bin)), 
            alpha = 0.2) +
  geom_ribbon(data = summary_df_leader, 
              aes(x = relative_leader_influence, 
                  ymin = ribbon_lower, 
                  ymax = ribbon_upper, 
                  fill = mov_sat_bin), 
              alpha = 0.3) +
  geom_line(data = summary_df_leader, 
            aes(x = relative_leader_influence, y = mean_cr, color = mov_sat_bin), 
            linewidth = 1.2) +
  geom_point(data = summary_df_leader, 
             aes(x = relative_leader_influence, y = mean_cr, color = mov_sat_bin), 
             size = 1.5) +
  scale_x_continuous(breaks = unique(df$relative_leader_influence), 
                     limits = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_color_brewer(type = "qual", palette = "Spectral", name = "Movement \nSaturation") +
  scale_fill_brewer(type = "qual", palette = "Spectral", name = "Movement \nSaturation") +
  labs(x = "Relative Leader Influence (%)", 
       y = "Conversion Rate", 
       caption = paste("Simulations per combination:", runs_per_combo, 
                       " | Number of agents:", n_agents_value, 
                       " | Signal representation: Binary |\nAggregation method: Average"),
       title = "Leader Influence Effect by Movement Saturation") +
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

# Save the main analysis plot
# ggsave("fig4.png", p1, 
#        width = 15.9, height = 16, dpi = 600, units = "cm")

# ------------------------------------------------------------------------------
# ANALYSIS 2: VALUE PROFILE-SPECIFIC EFFECTS
# ------------------------------------------------------------------------------

# Data preparation for value profile analysis
df_long <- df |>
  rename(conversion_rate_total = conversion_rate) |>
  pivot_longer(
    cols = matches("(conversion_rate|converted|total)_[A-Z]$"),
    names_to = c(".value", "value_profile"),
    names_pattern = "(conversion_rate|converted|total)_([A-Z])",
    values_to = c("conversion_rate", "converted", "total")
  ) |>
  # Filter out profiles that don't have complete conversion rates across 
  # relative_leader_influence within each movement_saturation level
  group_by(value_profile, movement_saturation) |>
  filter(all(!is.na(conversion_rate))) |>
  ungroup()

# Create binned movement saturation variable for long format
df_binned_long <- df_long |>
  filter(movement_saturation %in% c(5, 25, 45, 55, 75, 95)) |>
  mutate(mov_sat_bin = factor(paste0(movement_saturation, "%"), 
                              levels = c("5%", "25%", "45%", "55%", "75%", "95%")))

# Create summary statistics for the long format
summary_df_leader_long <- df_binned_long |>
  group_by(relative_leader_influence, mov_sat_bin, value_profile) |>
  summarise(
    mean_cr = mean(conversion_rate, na.rm = TRUE),
    sd_cr = sd(conversion_rate, na.rm = TRUE),
    n = n(),
    se_cr = sd_cr / sqrt(n),
    ribbon_lower = pmax(0, mean_cr - 1.96 * se_cr),
    ribbon_upper = pmin(1, mean_cr + 1.96 * se_cr),
    # Also summarize the raw counts
    mean_converted = mean(converted, na.rm = TRUE),
    mean_total = mean(total, na.rm = TRUE),
    .groups = 'drop'
  )

# PLOTTING FUNCTIONS ------------------------------------------------------------------------------

#Function to create a plot for a specific value profile with manual marginals
create_profile_plot_with_marginal <- function(profile_name, 
                                              show_legend = TRUE, 
                                              show_title = TRUE, 
                                              max_total = NULL, 
                                              marginal_type = "detailed") {
  
  # Filter data for the specific value profile
  df_binned_profile <- df_binned_long |> 
    filter(value_profile == profile_name)
  
  summary_df_profile <- summary_df_leader_long |> 
    filter(value_profile == profile_name)
  
  # Create the main plot
  main_plot <- ggplot(data = summary_df_profile, 
                      aes(x = relative_leader_influence, y = mean_cr)) +
    geom_line(data = df_binned_profile, 
              aes(x = relative_leader_influence, y = conversion_rate, 
                  color = mov_sat_bin, 
                  group = interaction(run_id, mov_sat_bin)), 
              alpha = 0.1, linewidth = 0.25) +
    geom_ribbon(aes(ymin = ribbon_lower, 
                    ymax = ribbon_upper, 
                    fill = mov_sat_bin), 
                alpha = 0.3) +
    geom_line(aes(color = mov_sat_bin), linewidth = 0.5) +
    geom_point(aes(color = mov_sat_bin), size = 1) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                       limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), 
                       limits = c(0, 1)) +
    scale_color_brewer(type = "qual", palette = "Spectral", direction = -1, 
                       name = "Movement \nSaturation",
                       limits = c("5%", "25%", "45%", "55%", "75%", "95%"),
                       labels = c("5%", "25%", "45%", "55%", "75%", "95%")) +
    scale_fill_brewer(type = "qual", palette = "Spectral", direction = -1, 
                      name = "Movement \nSaturation",
                      limits = c("5%", "25%", "45%", "55%", "75%", "95%"),
                      labels = c("5%", "25%", "45%", "55%", "75%", "95%")) +
    labs(x = "Relative Leader Influence (%)", 
         y = "\nConversion Rate") +
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
          legend.background = element_rect(fill = "white", color = "black"))
  
  # Create marginal histogram using geom_col with total counts
  # Create marginal data based on type
  if (marginal_type == "summary") {
    # Summary version: aggregate across all leader influence levels
    marginal_data <- df_long |>
      filter(value_profile == profile_name,
             movement_saturation %in% c(5, 25, 45, 55, 75, 95)) |>
      mutate(mov_sat_bin = factor(paste0(movement_saturation, "%"), 
                                  levels = c("5%", "25%", "45%", "55%", "75%", "95%"))) |>
      group_by(mov_sat_bin) |>
      summarise(total_count = mean(total, na.rm = TRUE), .groups = 'drop') |>
      # Ensure all movement saturation levels are present (even with 0 counts)
      complete(mov_sat_bin = factor(c("5%", "25%", "45%", "55%", "75%", "95%"), 
                                    levels = c("5%", "25%", "45%", "55%", "75%", "95%")), 
               fill = list(total_count = NA)) |>
      # Ensure factor levels are preserved for consistent coloring
      mutate(mov_sat_bin = factor(mov_sat_bin, levels = c("5%", "25%", "45%", "55%", "75%", "95%")))
    
    marginal_plot <- ggplot(marginal_data, 
                            aes(x = mov_sat_bin, y = total_count, 
                                fill = mov_sat_bin)) +
      geom_col(color = "black", width = 0.7) +  # Add explicit width
      scale_fill_brewer(type = "qual", palette = "Spectral", direction = -1, 
                        limits = c("5%", "25%", "45%", "55%", "75%", "95%"),
                        guide = "none") +
      labs(x = "Movement Saturation (%)", y = "Avg.* Non-EA\nPopulation") +
      theme(      panel.background = element_rect(fill = "white", color = "black"),
                  panel.grid.major.y = element_line(color = "grey"),
                  panel.grid.minor.x = element_line(color = NA),          
                  panel.grid.major.x = element_line(color = NA),
                  strip.background = element_rect(fill = "white", color = "black"),
                  strip.text = element_text(size = 11, face = "bold"),
                  axis.line = element_line(color = "black"),
                  axis.ticks = element_line(color = "black"),
                  axis.text = element_text(color = "black", size = 10),
                  axis.title = element_text(face = "bold", size = 11),
                  legend.background = element_rect(fill = "white", color = "black"))
  } else {
    # Detailed version: show bars for each leader influence level (original)
    marginal_data <- df_long |>
      filter(value_profile == profile_name,
             movement_saturation %in% c(5, 25, 45, 55, 75, 95)) |>
      mutate(mov_sat_bin = factor(paste0(movement_saturation, "%"), 
                                  levels = c("5%", "25%", "45%", "55%", "75%", "95%"))) |>
      group_by(relative_leader_influence, mov_sat_bin) |>
      summarise(total_count = sum(total, na.rm = TRUE), .groups = 'drop') |>
      # Ensure factor levels are preserved for consistent coloring
      mutate(mov_sat_bin = factor(mov_sat_bin, levels = c("5%", "25%", "45%", "55%", "75%", "95%")))
    
    marginal_plot <- ggplot(marginal_data, 
                            aes(x = relative_leader_influence, y = total_count, 
                                fill = mov_sat_bin)) +
      geom_col(position = "dodge", color = "black") +
      scale_fill_brewer(type = "qual", palette = "Spectral", direction = -1, 
                        limits = c("5%", "25%", "45%", "55%", "75%", "95%"),
                        guide = "none") +
      scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), 
                         limits = c(0, 100)) +
      labs(x = "Movement Saturation (%)", y = "Population\nsize") +
      theme(panel.background = element_rect(fill = "white", color = "black"),    
            panel.grid.major.y = element_line(color = "grey"),
            panel.grid.minor.x = element_line(color = NA),          
            panel.grid.major.x = element_line(color = NA),
            strip.background = element_rect(fill = "white", color = "black"),
            strip.text = element_text(size = 11, face = "bold"),
            axis.line = element_line(color = "black"),
            axis.ticks = element_line(color = "black"),
            axis.text = element_text(color = "black", size = 10),
            axis.title = element_text(face = "bold", size = 11),
            legend.background = element_rect(fill = "white", color = "black"))
  }
  
  # Set consistent y-scale for marginal plots if max_total is provided
  if (!is.null(max_total)) {
      marginal_plot <- marginal_plot + 
        scale_y_continuous(limits = c(0, max_total), 
                           breaks = seq(0, max_total, length.out = 3))
  }
  
  # Add title to marginal plot if requested
  if (show_title) {
    main_plot <- main_plot + ggtitle(paste("Value Profile", profile_name))
  }
  
  # Show/hide legend
  if (!show_legend) {
    main_plot <- main_plot + theme(legend.position = "none")
  } else {
    main_plot <- main_plot + theme(legend.position = "right")
  }
  
  # Combine plots using patchwork
  combined <-  main_plot / marginal_plot +plot_layout(heights = c(2.5, 1))
  
  return(combined)
}

# CREATE VALUE PROFILE PLOTS------------------------------------------------------------------------------

# Configuration
max_total_count <- 750 * 0.4
profiles <- unique(df_long$value_profile)

# Create individual plots for each value profile
plot_list <- list()
for (i in seq_along(profiles)) {
  plot_with_marginal <- create_profile_plot_with_marginal(
    profiles[i], 
    show_legend = FALSE,  # Never show legend on individual plots
    show_title = TRUE,
    max_total = max_total_count,
    marginal_type = "summary"  # Use "summary" for aggregated marginal, "detailed" for original
  )
  
  plot_list[[i]] <- plot_with_marginal
}

# CREATE LEGEND FOR VALUE PROFILE ANALYSIS ------------------------------------------------------------------------------


# Create dummy data for legend
legend_data <- data.frame(
  x = 1:6,
  y = 1:6,
  mov_sat_bin = factor(c("5%", "25%", "45%", "55%", "75%", "95%"), 
                       levels = c("5%", "25%", "45%", "55%", "75%", "95%"))
)

# Create legend plot
legend_plot <- ggplot(legend_data, aes(x = x, y = y, color = mov_sat_bin)) +
  geom_point(size = 1.2) +
  geom_line(size = 1) +
  geom_density(aes(x = x, fill = mov_sat_bin), alpha = 0.3) +
  scale_color_brewer(type = "qual", palette = "Spectral", direction = -1, 
                     name = "Movement \nSaturation",
                     limits = c("5%", "25%", "45%", "55%", "75%", "95%"),
                     labels = c("5%", "25%", "45%", "55%", "75%", "95%")) +
  scale_fill_brewer(type = "qual", palette = "Spectral", direction = -1, 
                    name = "Movement \nSaturation",
                    limits = c("5%", "25%", "45%", "55%", "75%", "95%"),
                    labels = c("5%", "25%", "45%", "55%", "75%", "95%")) +
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
        legend.position = "right",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text(face = "bold", size = 9),
        legend.text = element_text(size = 8),
        legend.direction = "horizontal")

# Extract and position legend
legend <- get_legend(legend_plot)
smaller_legend <- plot_spacer() / legend  + plot_layout(heights = c(1, 2))
plot_list[[6]] <- smaller_legend

# COMBINE AND SAVE VALUE PROFILE ANALYSIS ------------------------------------------------------------------------------

# Combine all plots
combined_plot <- wrap_plots(plot_list, ncol = 2)

# Add overall title and caption
final_plot <- combined_plot + 
  plot_annotation(
    caption = paste("Simulations per combination:", runs_per_combo, 
                    " | N agents per simulation:", n_agents_value,
                    " | Signal format: Binary | Aggregation: Average.",
                    "\n* Population sizes for each value profile were averaged across simulations and relative leader influence levels.")
  )

# Display and save the value profile analysis
print(final_plot)

# ggsave("fig5.png", final_plot, 
#        width = 15.9, height = 24, dpi = 600, units = "cm")

# ------------------------------------------------------------------------------
# ANALYSIS 3: GAMs and contour plots across value profiles
# ------------------------------------------------------------------------------
gam_1 <- gam(cbind(total_converted, total_unconverted) ~ 
               te(relative_leader_influence, movement_saturation) + 
               s(run_id, bs = "re"),
             family = binomial,
             data = df)

# Check the summary of the GAM model
summary(gam_1)

# Plot the GAM model results ----
# Create a grid for prediction
leader_seq <- seq(0, 100, length.out = 50)
movement_seq <- seq(5, 95, length.out = 50)

# grid
grid_all <- expand.grid(
  relative_leader_influence = leader_seq,
  movement_saturation = movement_seq,
  run_id = runif(1, min = 1, max = 50)       # Fix at reference
)

pred_all <- predict(gam_1, newdata = grid_all)
plot_data<- cbind(grid_all, prediction = pred_all)

# Create contour plots
ggplot(plot_data, aes(x = relative_leader_influence, 
                               y = movement_saturation, 
                               z = prediction)) +
  geom_contour_filled(bins = 15, alpha = 0.6) +
  scale_fill_viridis_d(name = "Conversion\nRate") +
  labs(x = "Relative Leader Influence (%)", 
       y = "Movement Saturation (%)",
       title = "Predicted Conversion Rate by Method")

# ------------------------------------------------------------------------------
# ANALYSIS 4: GAMs and contour plots by value profiles
# ------------------------------------------------------------------------------
# Fit GAM model with value profile interactions using binomial family
gam_2 <- gam(cbind(converted, total - converted) ~ 
               te(relative_leader_influence, movement_saturation, by = factor(value_profile)) + 
               s(run_id, bs = "re"),
             family = binomial, 
             data = df_long)

# Check the summary of the GAM model
summary(gam_2)

# Create prediction grids for contour plots
leader_seq <- seq(0, 100, length.out = 50)
movement_seq <- seq(5, 95, length.out = 50)

# Get available value profiles
available_profiles <- unique(df_long$value_profile)

# Create prediction grids for each value profile
prediction_data_list <- list()

for (profile in available_profiles) {
  grid_profile <- expand.grid(
    relative_leader_influence = leader_seq,
    movement_saturation = movement_seq,
    value_profile = profile,
    run_id = 1  # Fix at reference
  )
  
  # Make predictions (on response scale for proportions)
  pred_profile <- predict(gam_2, newdata = grid_profile, type = "response")
  
  # Store in list
  prediction_data_list[[profile]] <- cbind(grid_profile, prediction = pred_profile)
}

# Combine all prediction data
plot_data_combined <- do.call(rbind, prediction_data_list)

# Create contour plots for all value profiles
contour_plot <- ggplot(plot_data_combined, aes(x = relative_leader_influence, 
                                               y = movement_saturation, 
                                               z = prediction)) +
  geom_contour_filled(bins = 15) +
  facet_wrap(~ paste("Value Profile", value_profile), ncol = 2, axes ="all") +
  scale_y_continuous(limits = c(5, 95), breaks = seq(5,95, 10))+
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_viridis_d(name = "Conversion\nRate", option = "plasma") +
  labs(x = "Relative Leader Influence (%)", 
       y = "Movement Saturation (%)",
       title = "Predicted Conversion Rate by Value Profile (GAM Model)",
       caption = paste0("Model: te(leader_influence, movement_saturation, by = value_profile) + s(run_id, bs = 're') |\n Variance explained (R²): ", 
                        round(summary(gam_2)$r.sq * 100, 2), " %")
       )+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 10, face = "bold"),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 9.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.position = "bottom")

# Display the contour plot
print(contour_plot)

# Save the contour plot
# ggsave("fig6.png", contour_plot, 
#        width = 15.6, height = 19.5, dpi = 600, units = "cm")

# ------------------------------------------------------------------------------
# RAW DATA CONTOUR PLOTS (Alternative to GAM predictions)
# ------------------------------------------------------------------------------

# Create contour plots from raw data
# First, create a summary of the raw data by averaging across runs
raw_data_summary <- df |>
  rename(conversion_rate_total = conversion_rate) |>
  pivot_longer(
    cols = matches("(conversion_rate|converted|total)_[A-Z]$"),
    names_to = c(".value", "value_profile"),
    names_pattern = "(conversion_rate|converted|total)_([A-Z])",
    values_to = c("conversion_rate", "converted", "total")
  ) |>
  group_by(relative_leader_influence, movement_saturation, value_profile) |>
  summarise(
    mean_conversion_rate = mean(conversion_rate, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  )

# Create raw data contour plots
raw_contour_plot <- ggplot(raw_data_summary, aes(x = relative_leader_influence, 
                                                 y = movement_saturation, 
                                                 z = mean_conversion_rate)) +
  geom_contour_filled(bins = 15, alpha = 0.8) +
  facet_wrap(~ paste("Value Profile", value_profile), ncol = 3) +
  scale_y_continuous(limits = c(5, 95), breaks = c(5,25,45,55,75,95))+
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_fill_viridis_d(name = "Conversion\nRate") +
  labs(x = "Relative Leader Influence (%)", 
       y = "Movement Saturation (%)",
       title = "Observed Conversion Rate by Value Profile (Raw Data)",
       caption = "Contours based on mean conversion rates across simulation runs") +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"))

# Display the raw data contour plot
print(raw_contour_plot)

# Save the raw data contour plot
# ggsave("raw_data_contour_plots_value_profiles.png", raw_contour_plot, 
#        width = 20, height = 15, dpi = 600, units = "cm")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================