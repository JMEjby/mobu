# setup ----
library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(shadowtext)  # Add this package for black outlines

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

# Function to create the combined plots
create_combined_distribution_plots <- function(n_agents = 500, 
                                               movement_saturation = 5,
                                               n_sims = 1000000,
                                               save_filename = NULL) {
  
  # Define parameterizations
  params <- list(
    "Narrow \n(Standard)" = list(mean = 0, sd = 1),
    "Wide" = list(mean = 0, sd = 1.5),
    "EA \nSkewed" = list(mean = 0.5, sd = 1),
    "Non-EA \nSkewed" = list(mean = -0.5, sd = 1)
  )
  
  # Get colors from RColorBrewer Set3
  colors <- brewer.pal(length(params), "Set2")
  
  # Generate data for all parameterizations
  all_data <- map_dfr(names(params), function(name) {
    param <- params[[name]]
    data.frame(
      x = rnorm(n_sims, mean = param$mean, sd = param$sd),
      parameterization = name
    )
  })
  
  # Calculate percentages for all parameterizations
  pct_data <- map_dfr(names(params), function(name) {
    param <- params[[name]]
    pct_a <- round((pnorm(Inf, mean = param$mean, sd = param$sd) - pnorm(1.5, mean = param$mean, sd = param$sd)) * 100, 0)
    pct_b <- round((pnorm(1.5, mean = param$mean, sd = param$sd) - pnorm(0.5, mean = param$mean, sd = param$sd)) * 100, 0)
    pct_c <- round((pnorm(0.5, mean = param$mean, sd = param$sd) - pnorm(-0.5, mean = param$mean, sd = param$sd)) * 100, 0)
    pct_d <- round((pnorm(-0.5, mean = param$mean, sd = param$sd) - pnorm(-1.5, mean = param$mean, sd = param$sd)) * 100, 0)
    pct_e <- round((pnorm(-1.5, mean = param$mean, sd = param$sd) - pnorm(-Inf, mean = param$mean, sd = param$sd)) * 100, 0)
    
    data.frame(
      parameterization = name,
      category = c("E", "D", "C", "B", "A"),
      x_pos = c(-2.75, -1, 0, 1, 2.75),
      percentage = c(pct_e, pct_d, pct_c, pct_b, pct_a)
    )
  })
  
  # Create stacked labels data
  label_data <- pct_data |>
    arrange(category, desc(parameterization)) |>
    group_by(category) |>
    mutate(
      y_pos = seq(0.45, by = 0.05, length.out = n()),
      label_text = paste0(percentage, "%")
    ) |>
    ungroup()
  
  # Add color mapping
  label_data$color <- colors[match(label_data$parameterization, names(params))]
  all_data$color <- colors[match(all_data$parameterization, names(params))]
  
  # Create category labels data (black E, D, C, B, A labels)
  category_labels <- data.frame(
    x_pos = c(-2.75, -1, 0, 1, 2.75),
    y_pos = rep(max(label_data$y_pos) + 0.05, 5),  # Place above the highest colored labels
    label_text = c("E", "D", "C", "B", "A")
  )
  
  # Plot 1: Multiple Value Profile Categories
  p1 <- ggplot() +
    geom_density(data = all_data, aes(x = x, color = parameterization, fill = parameterization), 
                 linewidth = 1, alpha = 0.3) +
    geom_vline(xintercept = c(1.5, 0.5, -0.5, -1.5), 
               color = "black", linetype = "dashed", linewidth = 0.5) +
    # Add black category labels (E, D, C, B, A)
    geom_text(data = category_labels,
              aes(x = x_pos, y = y_pos, label = label_text),
              size = 3, hjust = 0.5, color = "black", fontface = "bold") +
    # Add colored percentage labels with black outline
    geom_label(data = label_data, 
              aes(x = x_pos, y = y_pos, label = label_text, color = parameterization),
              size = 3, hjust = 0.5,
              fill = "white") +
    scale_color_manual(values = setNames(colors, names(params))) +
    scale_fill_manual(values = setNames(colors, names(params))) +
    scale_x_continuous(limits = c(-4.5, 4.5), 
                       breaks = c(-4.5, 0, 4.5),
                       labels = c("\nStrongly \nmisaligned", "\n \nNeutral", "\nStrongly \naligned")) +
    scale_y_continuous(limits = c(0, 0.66)) +
    labs(x = "EA Value Alignment", 
         y = "Density", 
         title = "Value Profile Categories Across \nDifferent Distributions",
         color = "Population \ncomposition",
         fill = "Population \ncomposition") +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(color = "grey"),
      panel.grid.minor = element_line(color = NA),
      panel.grid.major.x = element_line(color = NA),
      axis.ticks = element_line(color = NA),
      axis.text = element_text(color = "black", size = 11),
      axis.title = element_text(face = "bold", size = 12),
      title = element_text(size = 11),
      axis.line.x = element_line(arrow = arrow(angle = 30,
                                               length = unit(0.15, "inches"),
                                               ends = "both", 
                                               type = "closed")),
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold"),
      legend.position = "bottom",
      legend.key.size = unit(0.5, "cm")
    )
  
  # Plot 2: Example Distribution by Agent Type (Narrow distribution only)
  data_groups_narrow <- generate_population_values(n_agents, movement_saturation, 0, 1)
  data_groups_narrow$value_profile <- factor(data_groups_narrow$value_profile, levels = c("E", "D", "C", "B", "A"))
  
  p2 <- ggplot(data_groups_narrow, aes(x = value_profile, fill = breed)) +
    geom_bar() +
    scale_fill_manual(values = c("#345da9", "#1f3968", "#e07f96")) +
    annotate("text", x = 1:5, 
             y = sapply(c("E", "D", "C", "B", "A"), function(x) nrow(filter(data_groups_narrow, value_profile == x)) + 10),
             label = c("E", "D", "C", "B", "A"), color = "black", size = 4) +
    scale_y_continuous(limits = c(0, 210)) +
    scale_x_discrete(labels = c("\nStrongly \nmisaligned", "", "\n \nNeutral", "", "\nStrongly \naligned")) +
    labs(x = "EA Value Alignment", 
         y = "Number of agents", 
         fill = "Agent \n type", 
         title = "Example Distribution of \nValue Profiles by Agent \nType (Narrow Distribution)",
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
      legend.key.size = unit(0.5, "cm")
    )
  
  # Combine plots side by side
  combined_plot <- p1 + p2 +
    plot_layout(widths = c(0.67, 0.33)) +
    plot_annotation(tag_levels = "A")
  
  # Save if filename provided
  if (!is.null(save_filename)) {
    ggsave(save_filename, combined_plot, width = 20, height = 14, dpi = 600, units = "cm")
  }
  
  return(combined_plot)
}

# Create the combined plot
combined_plot <- create_combined_distribution_plots(
  n_agents = 500,
  movement_saturation = 5,
  n_sims = 1000000,
  save_filename = "combined_value_distribution_plots.png"
)

# Display the plot
combined_plot
