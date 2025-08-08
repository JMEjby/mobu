# setup ----
library(tidyverse)
library(patchwork)
library(RColorBrewer)

setwd("C:/Users/s1917169/OneDrive - University of Edinburgh/3.1 EA movement building/V4 model")

cm_to_pixels <- function(cm, dpi = 600) {
  inches <- cm / 2.54  # Convert cm to inches
  pixels <- inches * dpi
  return(round(pixels))
}

generate_population_values <- function(n_agents, movement_saturation, pop_mean, pop_sd, 
                                       leader_bias = 3.0, ea_bias = 2) {
  
  # Helper function to convert continuous values to categorical value profiles
  value_category <- function(value) {
    if (value > 1.5) return("A")
    if (value > 0.5) return("B") 
    if (value > -0.5) return("C")
    if (value > -1.5) return("D")
    return("E")
  }
  
  # Helper function for weighted selection favoring higher values
  select_weighted_index <- function(bias, indices, values) {
    if (length(indices) == 0) return(NULL)
    if (length(indices) == 1) return(indices[1])
    
    # Get values for available indices
    available_values <- values[indices]
    
    # Apply exponential weighting (higher bias = stronger preference for high values)
    weights <- exp(bias * scale(available_values)[,1])
    
    # Sample with these weights
    selected_idx <- sample(indices, 1, prob = weights)
    return(selected_idx)
  }
  
  # Calculate number of each agent type
  n_eas <- ceiling((n_agents * movement_saturation / 100)) - 1
  n_outsiders <- n_agents - n_eas - 1
  n_leaders <- 1
  
  # Generate normally distributed values for entire population
  population_values <- rnorm(n_agents, mean = pop_mean, sd = pop_sd)
  
  # Sort values in descending order (highest first)
  population_values <- sort(population_values, decreasing = TRUE)
  
  # Track available indices
  available_indices <- 1:length(population_values)
  
  # Initialize result vectors
  value_profiles <- character(n_agents)
  breeds <- character(n_agents)
  
  # Assign leader values (with strong bias toward high values)
  if (n_leaders > 0) {
    leader_idx <- select_weighted_index(leader_bias, available_indices, population_values)
    value_profiles[leader_idx] <- value_category(population_values[leader_idx])
    breeds[leader_idx] <- "Leader"
    available_indices <- available_indices[available_indices != leader_idx]
  }
  
  # Assign EA values (with moderate bias toward high values)
  ea_indices <- numeric(n_eas)
  for (i in 1:n_eas) {
    if (length(available_indices) > 0) {
      ea_idx <- select_weighted_index(ea_bias, available_indices, population_values)
      ea_indices[i] <- ea_idx
      value_profiles[ea_idx] <- value_category(population_values[ea_idx])
      breeds[ea_idx] <- "EA"
      available_indices <- available_indices[available_indices != ea_idx]
    }
  }
  
  # Assign remaining values to outsiders (no bias - random selection)
  for (idx in available_indices) {
    value_profiles[idx] <- value_category(population_values[idx])
    breeds[idx] <- "Non-\nEA"
  }
  
  # Create data frame, removing any unassigned agents
  result <- data.frame(
    value_profile = value_profiles[breeds != ""],
    breed = breeds[breeds != ""],
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# Function to create value distribution plots
create_value_distribution_plots <- function(pop_mean = 0, 
                                            pop_sd = 1, 
                                            n_agents = 500, 
                                            movement_saturation = 5,
                                            n_sims = 1000000,
                                            save_filename = NULL,
                                            plot_title_suffix = "") {
  
  # Generate data for distribution plot
  data <- data.frame(x = rnorm(n_sims, mean = pop_mean, sd = pop_sd))
  
  # Generate population groups
  data_groups <- generate_population_values(n_agents, movement_saturation, pop_mean, pop_sd)
  data_groups$value_profile <- factor(data_groups$value_profile, levels = c("E", "D", "C", "B", "A"))
  
  # Calculate percentages for annotations (using standard normal for consistency)
  pct_a <- round((pnorm(Inf, mean = pop_mean, sd = pop_sd) - pnorm(1.5, mean = pop_mean, sd = pop_sd)) * 100, 0)
  pct_b <- round((pnorm(1.5, mean = pop_mean, sd = pop_sd) - pnorm(0.5, mean = pop_mean, sd = pop_sd)) * 100, 0)
  pct_c <- round((pnorm(0.5, mean = pop_mean, sd = pop_sd) - pnorm(-0.5, mean = pop_mean, sd = pop_sd)) * 100, 0)
  pct_d <- round((pnorm(-0.5, mean = pop_mean, sd = pop_sd) - pnorm(-1.5, mean = pop_mean, sd = pop_sd)) * 100, 0)
  pct_e <- round((pnorm(-1.5, mean = pop_mean, sd = pop_sd) - pnorm(-Inf, mean = pop_mean, sd = pop_sd)) * 100, 0)
  
  # Plot 1: Distribution of EA-Aligned Values
  p1 <- ggplot(data, aes(x = x)) +
    geom_density(linewidth = 1, color = "#345da9", fill = "#859ecb", alpha = 0.7) +
    scale_x_continuous(limits = c(-5, 5), 
                       breaks = c(-5, 0, 5),
                       labels = c("\nStrongly \nmisaligned", "\n \nNeutral", "\nStrongly \naligned")) +
    labs(x = "EA Value Alignment", 
         y = "Density", 
         title = paste0("Distribution of EA-Aligned \nValues in the Population", plot_title_suffix)) +
    scale_y_continuous(limits = c(0, 0.55)) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "grey"),
      panel.grid.minor = element_line(color = NA),
      panel.grid.major.x = element_line(color = NA),
      axis.ticks = element_line(color = NA),
      axis.text = element_text(color = "black", size = 12),
      axis.title = element_text(face = "bold", size = 13),
      title = element_text(size = 11),
      axis.line.x = element_line(arrow = arrow(angle = 30,
                                               length = unit(0.15, "inches"),
                                               ends = "both", 
                                               type = "closed"))
    )
  
  # Plot 2: Value Profile Categories
  p2 <- ggplot(data, aes(x = x)) +
    geom_density(linewidth = 1, color = "#345da9", fill = "#859ecb", alpha = 0.5) +
    geom_vline(xintercept = c(1.5, 0.5, -0.5, -1.5), 
               color = "black", linetype = "dashed", linewidth = 1) +
    annotate("text", x = -2.75, y = 0.5, label = paste0("E \n ", pct_e, "%"), color = "black", size = 3.25) +
    annotate("text", x = -1, y = 0.5, label = paste0("D \n ", pct_d, "%"), color = "black", size = 3.25) +
    annotate("text", x = 0, y = 0.5, label = paste0("C \n ", pct_c, "%"), color = "black", size = 3.25) +
    annotate("text", x = 1, y = 0.5, label = paste0("B \n ", pct_b, "%"), color = "black", size = 3.25) +
    annotate("text", x = 2.75, y = 0.5, label = paste0("A \n ", pct_a, "%"), color = "black", size = 3.25) +
    scale_x_continuous(limits = c(-5, 5), 
                       breaks = c(-5, 0, 5),
                       labels = c("\nStrongly \nmisaligned", "\n \nNeutral", "\nStrongly \naligned")) +
    scale_y_continuous(limits = c(0, 0.55)) +
    labs(x = "EA Value Alignment", y = "Density", title = "Value Profile Categories") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "grey"),
      panel.grid.minor = element_line(color = NA),
      panel.grid.major.x = element_line(color = NA),
      axis.ticks = element_line(color = NA),
      axis.text = element_text(color = "black", size = 12),
      axis.title = element_text(face = "bold", size = 13),
      title = element_text(size = 11),
      axis.line.x = element_line(arrow = arrow(angle = 30,
                                               length = unit(0.15, "inches"),
                                               ends = "both", 
                                               type = "closed"))
    )
  
  # Plot 3: Example Distribution by Agent Type
  p3 <- ggplot(data_groups, aes(x = value_profile, fill = breed)) +
    geom_bar() +
    scale_fill_manual(values = c("#345da9", "#1f3968", "#e07f96")) +
    annotate("text", x = 1:5, 
             y = sapply(c("E", "D", "C", "B", "A"), function(x) nrow(filter(data_groups, value_profile == x)) + 10),
             label = c("E", "D", "C", "B", "A"), color = "black", size = 4) +
    scale_y_continuous(limits = c(0, 210)) +
    scale_x_discrete(labels = c("\nStrongly \nmisaligned", "", "\n \nNeutral", "", "\nStrongly \naligned")) +
    labs(x = "EA Value Alignment", 
         y = "Number of agents", 
         fill = "Agent \n type", 
         title = "Example Distribution of \nValue Profiles by Agent \nType",
         caption = paste0("Number of agents: ", n_agents, " | Movement saturation: ", movement_saturation, "%")) +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "grey"),
      panel.grid.minor = element_line(color = NA),
      panel.grid.major.x = element_line(color = NA),
      axis.ticks = element_line(color = NA),
      axis.text = element_text(color = "black", size = 12),
      axis.title = element_text(face = "bold", size = 13),
      title = element_text(size = 11),
      axis.line.x = element_line(arrow = arrow(angle = 30,
                                               length = unit(0.15, "inches"),
                                               ends = "both", 
                                               type = "closed")),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      legend.key.size = unit(0.55, "cm")
    )
  
  # Combine plots
  combined_plot <- ((p1/p2) | p3) +
    plot_layout(widths = c(0.57, 0.43)) +
    plot_annotation(tag_levels = "A")
  
  # Save if filename provided
  if (!is.null(save_filename)) {
    ggsave(save_filename, combined_plot, width = 21, height = 18, dpi = 1200, units = "cm")
  }
  
  return(combined_plot)
}

# Narrow (Standard) Normal ----
plot1 <- create_value_distribution_plots(
  pop_mean = 0, 
  pop_sd = 1, 
  n_agents = 500,
  movement_saturation = 5,
  n_sims = 1000000,
  save_filename = "value_alignment_distribution.png"
)

# Wide Normal ----
plot2 <- create_value_distribution_plots(
  pop_mean = 0, 
  pop_sd = 1.5,
  n_agents = 500,
  movement_saturation = 5,
  n_sims = 1000000,
  save_filename = "value_alignment_distribution_wide.png"
)

# EA skewed normal ----
plot3 <- create_value_distribution_plots(
  pop_mean = 0.5, 
  pop_sd = 1,
  n_agents = 500,
  movement_saturation = 5,
  n_sims = 1000000,
  save_filename = "value_alignment_distribution_ea_skewed.png"
)

# Non-EA skewed normal ----
plot4 <- create_value_distribution_plots(
  pop_mean = -0.5, 
  pop_sd = 1,
  n_agents = 500,
  movement_saturation = 5,
  n_sims = 1000000,
  save_filename = "value_alignment_distribution_non_ea_skewed.png"
)

# Display plots
plot1
plot2
plot3
plot4
