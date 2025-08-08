library(tidyverse)
library(viridis)
library(patchwork)

# Function for Softmax Update (5-dimensional)
update_weights_softmax <- function(current_weights, update_vector, social_influence = 0.1) {
  # Clamp current weights to avoid log(0)
  clamped_weights <- pmax(0.001, pmin(0.999, current_weights))
  
  # Convert to log space
  log_weights <- log(clamped_weights)
  
  # Apply updates in log space
  updated_log_weights <- log_weights + (update_vector * social_influence)
  
  # Convert back via softmax (automatically normalizes to sum = 1)
  max_log_weight <- max(updated_log_weights)  # For numerical stability
  exp_weights <- exp(updated_log_weights - max_log_weight)
  exp_sum <- sum(exp_weights)
  new_weights <- exp_weights / exp_sum
  
  return(new_weights)
}

# Define starting probability vectors from Dirichlet parameters
dirichlet_params <- list(
  A = c(8, 7, 3, 1.5, 0.5) * 20,
  B = c(4.5, 6.0, 5.5, 3.25, 0.75) * 20,
  C = c(1, 5, 8, 5, 1) * 20,
  D = c(0.75, 3.25, 5.5, 6, 4.5) * 20,
  E = c(0.5, 1.5, 3, 7, 8) * 20
)

# Calculate expected probabilities (normalized)
starting_probs <- map(dirichlet_params, ~ .x / sum(.x))

# Define update vectors (rounded to 2 decimal places)
update_vectors <- list(
  ea52 = round(c(0.04418181818181818, -0.34845454545454546, -0.3165454545454545, 0.1509090909090909, 0.4699090909090909), 2),
  ea53 = round(c(0.04418181818181819, 0.05645454545454546, -0.11409090909090908, -0.45645454545454545, 0.4699090909090909), 2),
  ea78 = round(c(-0.15827272727272726, -0.146, 0.08836363636363638, 0.1509090909090909, 0.06499999999999997), 2),
  ea65 = round(c(0.04418181818181818, -0.34845454545454546, -0.3165454545454545, 0.1509090909090909, 0.46990909090909094), 2),
  ea71 = round(c(0.04418181818181818, -0.146, -0.3165454545454545, -0.051545454545454554, 0.4699090909090909), 2),
  ea76 = round(c(-0.15827272727272726, 0.05645454545454548, -0.11409090909090908, -0.254, 0.4699090909090909), 2),
  ea67 = round(c(-0.15827272727272726, 0.05645454545454548, -0.519, 0.1509090909090909, 0.46990909090909094), 2),
  ea59 = round(c(-0.15827272727272726, -0.5509090909090909, -0.11409090909090908, 0.3533636363636363, 0.4699090909090909), 2),
  ea2 = round(c(0.04418181818181819, -0.146, -0.519, 0.3533636363636364, 0.26745454545454544), 2),
  ea68 = round(c(-0.36072727272727273, 0.05645454545454548, -0.11409090909090908, 0.1509090909090909, 0.26745454545454544), 2),
  ea72 = round(c(-0.15827272727272726, 0.05645454545454546, -0.519, 0.1509090909090909, 0.4699090909090909), 2),
  ea50 = round(c(0.04418181818181818, 0.05645454545454546, -0.11409090909090908, 0.1509090909090909, -0.13745454545454544), 2),
  ea56 = round(c(0.0441818181818182, -0.146, -0.3165454545454545, -0.05154545454545456, 0.4699090909090909), 2),
  ea54 = round(c(0.0441818181818182, -0.146, -0.11409090909090908, 0.1509090909090909, 0.06499999999999997), 2),
  ea77 = round(c(0.0441818181818182, -0.146, -0.519, 0.1509090909090909, 0.4699090909090909), 2),
  ea61 = round(c(-0.36072727272727273, -0.146, -0.11409090909090908, 0.3533636363636364, 0.26745454545454544), 2),
  ea5 = round(c(-0.15827272727272726, -0.7533636363636363, 0.08836363636363638, 0.3533636363636364, 0.4699090909090909), 2),
  ea57 = round(c(0.044181818181818176, -0.34845454545454546, 0.08836363636363638, 0.1509090909090909, 0.06499999999999996), 2),
  ea51 = round(c(-0.5631818181818182, 0.05645454545454548, -0.11409090909090908, 0.1509090909090909, 0.4699090909090909), 2),
  ea63 = round(c(-0.15827272727272726, -0.34845454545454546, -0.11409090909090908, 0.1509090909090909, 0.4699090909090909), 2),
  ea60 = round(c(-0.7656363636363637, 0.05645454545454546, -0.11409090909090908, 0.3533636363636364, 0.4699090909090909), 2),
  ea70 = round(c(0.04418181818181819, -0.146, -0.3165454545454545, 0.1509090909090909, 0.26745454545454544), 2),
  ea64 = round(c(0.04418181818181819, 0.05645454545454548, -0.7214545454545453, 0.1509090909090909, 0.4699090909090909), 2)
)

# Extract unique adjustment values for vertical lines (flattened from all update vectors)
all_adjustments <- unlist(update_vectors) |> unique() |> sort()

# Create simulation parameters
social_influences <- seq(0, 1, by = 0.2)
adjustment_range <- seq(-1, 1, by = 0.05)

# Create comprehensive data frame for Plot 1
# We'll simulate different types of adjustments across the 5 dimensions
results_plot1 <- data.frame()

# Define different update patterns to show softmax behavior
update_patterns <- list(
  component_1 = c(1, 0, 0, 0, 0),    # Only component 1
  component_2 = c(0, 1, 0, 0, 0),    # Only component 2
  component_3 = c(0, 0, 1, 0, 0),    # Only component 3
  component_4 = c(0, 0, 0, 1, 0),    # Only component 4
  component_5 = c(0, 0, 0, 0, 1)     # Only component 5
)

for (si in social_influences) {
  for (start_set in names(starting_probs)) {
    start_weights <- starting_probs[[start_set]]
    
    for (adj in adjustment_range) {
      for (pattern_name in names(update_patterns)) {
        # Apply scaled update pattern
        update_vector <- update_patterns[[pattern_name]] * adj
        
        # Get updated weights
        new_weights <- update_weights_softmax(start_weights, update_vector, si)
        
        # Add results for each probability component
        for (component in 1:5) {
          results_plot1 <- rbind(results_plot1, data.frame(
            adjustment = adj,
            social_influence = si,
            starting_set = start_set,
            update_pattern = pattern_name,
            signal = component,
            initial_prob = start_weights[component],
            new_prob = new_weights[component],
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
}

# Prepare data for plotting
plot_data1 <- results_plot1 |>
  mutate(
    social_influence_label = paste("σ =", social_influence),
    starting_set = factor(starting_set),
    signal = factor(signal),
    update_pattern = factor(update_pattern),
    # Create vector labels for the update patterns
    update_vector_label = case_when(
      update_pattern == "component_1" ~ "Updating \nProbability \nof Signal = 1",
      update_pattern == "component_2" ~ "Updating \nProbability \nof Signal = 2",
      update_pattern == "component_3" ~ "Updating \nProbability \nof Signal = 3",
      update_pattern == "component_4" ~ "Updating \nProbability \nof Signal = 4",
      update_pattern == "component_5" ~ "Updating \nProbability \nof Signal = 5"
    ),
    update_vector_label = factor(update_vector_label)
  )

# Create vertical lines data from actual update vectors (per component)
vlines_data <- expand_grid(
  adjustment = all_adjustments,
  update_vector_label = levels(plot_data1$update_vector_label)
)

# Create Plot 1 split by starting sets
plot_list <- list()

for (set_name in names(starting_probs)) {
  # Filter data for this starting set
  plot_data_subset <- plot_data1 |>
    filter(starting_set == set_name)
  
  # Filter vlines data for this starting set
  vlines_data_subset <- vlines_data
  
  # Create individual plot
  p_individual <- ggplot(plot_data_subset, aes(x = adjustment, y = new_prob, color = signal)) +
    geom_vline(data = vlines_data, 
               aes(xintercept = adjustment), 
               color = "darkgrey", alpha = 0.3, size = 0.2) +
    geom_line(size = 0.8, alpha = 0.8) +
    facet_grid(social_influence_label ~ update_vector_label) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
    scale_color_viridis(discrete = TRUE, option = "D", direction = -1) +
    labs(
      title = paste("Expected Starting Probabilities for Value Profile", set_name, ":", paste(round(starting_probs[[set_name]], 3), collapse = ", ")),
      x = "Perceived EA Misalignment",
      y = "Updated Probability",
      color = "Signal",
      caption = "Vertical lines show actual perceived EA misalignment values from model runs.\nσ denotes the social influence level."
    ) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major = element_line(color = "grey"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "white", color = "black"),
          strip.text = element_text(size = 11, face = "bold"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(face = "bold", size = 11),
          plot.title = element_text(size = 12),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.position = "bottom")
  
  # Store in list
  plot_list[[set_name]] <- p_individual
  
  # Save individual plot
  ggsave(paste0("Softmax_set_", set_name, ".png"), plot = p_individual, 
         width = 21, height = 29, dpi = 600, units = "cm")
  
  print(paste("Saved plot for starting set", set_name))
}

# Print individual plots
for (set_name in names(starting_probs)) {
  print(plot_list[[set_name]])
}

# Combine all plots using patchwork
combined_plot <- plot_list$A / plot_list$B / plot_list$C / plot_list$D / plot_list$E +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Add overall title and caption
combined_plot <- combined_plot + 
  plot_annotation(
    title = "Softmax Update Function: Different Update Patterns Across Starting Sets",
    caption = "Each panel shows a different starting probability set (A-E).\nColumns show update vectors, rows show social influence levels (σ).\nVertical lines show actual adjustment values from model runs.",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0)
    )
  )

print(combined_plot)

# Save combined plot
ggsave("Softmax_all_sets_combined.png", plot = combined_plot, 
       width = 18, height = 40, dpi = 600, units = "cm")

print("Saved combined plot")

# Create Plot 2: Initial vs Updated probabilities with actual update vectors
# Sample 6 update vectors randomly
set.seed(123)  # For reproducibility
sampled_update_names <- sample(names(update_vectors), 6)
sampled_updates <- update_vectors[sampled_update_names]

# Create data for Plot 2
results_plot2 <- data.frame()

for (si in social_influences) {
  for (start_set in names(starting_probs)) {
    start_weights <- starting_probs[[start_set]]
    
    for (update_name in names(sampled_updates)) {
      update_vec <- sampled_updates[[update_name]]
      
      # Get updated weights
      new_weights <- update_weights_softmax(start_weights, update_vec, si)
      
      # Debug: Print first few iterations to check
      if (si == 2 && start_set == "A" && update_name == names(sampled_updates)[1]) {
        cat(sprintf("Plot2 - SI: %g, Set: %s, Update: %s\n", si, start_set, update_name))
        cat("Start weights:", paste(round(start_weights, 4), collapse = ", "), "\n")
        cat("Update vector:", paste(update_vec, collapse = ", "), "\n")
        cat("New weights:", paste(round(new_weights, 4), collapse = ", "), "\n\n")
      }
      
      # Add results for each probability component
      for (component in 1:5) {
        results_plot2 <- rbind(results_plot2, data.frame(
          social_influence = si,
          starting_set = start_set,
          update_vector = update_name,
          signal = component,
          initial_prob = start_weights[component],
          new_prob = new_weights[component],
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# Prepare data for plotting
plot_data2 <- results_plot2 |>
  mutate(
    social_influence_label = paste("σ =", social_influence),
    starting_set = factor(starting_set),
    signal = factor(signal),
    update_vector = factor(update_vector)
  )

# Create Plot 2
p2 <- ggplot(plot_data2, aes(x = initial_prob, y = new_prob, color = update_vector)) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", alpha = 0.5) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_line(aes(group = interaction(update_vector, signal)), size = 0.8, alpha = 0.6) +
  facet_grid(social_influence_label ~ starting_set) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  labs(
    title = "Softmax Updates: Initial vs Updated Probabilities with Actual EA Misalignment Vectors",
    x = "Initial Probability",
    y = "Updated Probability", 
    color = "EA Misalignment \nVector",
    caption = "Dashed line shows no change (y = x). Points show response for each signal.\nσ denotes the social influence level."
  ) +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        plot.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0, size = 10),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = "bottom")

print(p2)
ggsave("Softmax_actual_updates.png", plot = p2, width = 19, height = 21, dpi = 600, units = "cm")