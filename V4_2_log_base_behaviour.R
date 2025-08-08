library(tidyverse)
library(patchwork)

# Create a range of probabilities
p <- seq(0.01, 0.99, by = 0.01)

# Define beta values (our primary parameter)
beta_values <- seq(-1, 1, by = 0.25)

# Recreate beta_range for smooth curves (finer resolution)
beta_range <- seq(-1, 1, by = 0.01)

# Define the different log bases to explore
log_bases <- c(1, 10, 50, 100)

# Function to create transformation plot for a given base
create_transformation_plot <- function(base) {
  # Calculate corresponding gamma values for this base
  gamma_values <- base^beta_values
  
  # Create data for visualization
  data_beta <- expand.grid(p = p, beta = beta_values) %>%
    mutate(
      gamma = base^beta,
      p_transformed = p^gamma,
      beta_label = paste("β =", beta)
    )
  
  # Create custom legend labels showing beta and corresponding gamma values
  beta_labels <- unique(data_beta$beta)
  gamma_values_for_legend <- base^beta_labels
  legend_labels <- sapply(1:length(beta_labels), function(i) {
    as.expression(bquote("β"[r] * " =" * .(beta_labels[i]) * " (γ"[r] * " =" * .(round(gamma_values_for_legend[i], 2)) * ")"))
  })
  
  # Create the plot
  p_plot <- ggplot(data_beta, aes(x = p, y = p_transformed, color = factor(beta))) +
    geom_line(size = 1.2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5) +
    labs(
      title = bquote("Base" ~ .(base) * ": " * P^(.(base)^beta[r])),
      x = expression(atop("Perceived EA Alignment Probability", " (" * P(EA)[i] * ")")),
      y = expression(atop("Strictness-Adjusted", "Recruitment Probability (" * P(EA)[i]^gamma[r] * ")")),
      color = expression(beta[r] * " (" * gamma[r] * " = " * .(base)^beta[r] * ")"),
      caption = bquote(beta[r] * " is the " * log[.(base)] * "-scaled parameter, " * gamma[r] * " = " * .(base)^beta[r])
    ) +
    scale_color_brewer(type = "div", palette = "RdBu", direction = -1, 
                       labels = legend_labels) +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "lightgrey"),
          panel.grid.minor.x = element_line(color = NA),          
          panel.grid.major.x = element_line(color = NA),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(size = 11),
          title = element_text(size = 10),
          plot.caption = element_text(hjust = 0, size = 9),
          legend.background = element_rect(fill = "white", color = "black"),
          legend.key.size = unit(0.4, "cm"),
          legend.text = element_text(size = 8))
  
  return(p_plot)
}

# Create plots for each base
plots_list <- lapply(log_bases, create_transformation_plot)

# Function to create parameter mapping plot for a given base
create_mapping_plot <- function(base) {
  # Calculate parameter mapping for this base
  param_mapping <- data.frame(
    beta = beta_values,
    gamma = base^beta_values
  )
  
  p_mapping <- ggplot(data.frame(beta = beta_range, gamma = base^beta_range), 
                      aes(x = beta, y = gamma)) +
    geom_line(size = 1.2, color = "darkblue") +
    geom_point(data = param_mapping, aes(x = beta, y = gamma), 
               size = 3, color = "red") +
    geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    labs(
      title = bquote("Base" ~ .(base) * ": " * beta[r] * " → " * gamma[r]),
      x = expression(atop(beta[r], " (model parameter)")),
      y = expression(atop(gamma[r], "(actual exponent)")),
      caption = bquote(gamma[r] * " = " * .(base)^beta[r])
    ) +
    scale_y_log10() +
    theme(panel.background = element_rect(fill = "white", color = "black"),
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "lightgrey"),
          panel.grid.minor.x = element_line(color = NA),          
          panel.grid.major.x = element_line(color = NA),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(size = 11),
          title = element_text(size = 10),
          plot.caption = element_text(hjust = 0, size = 9))
  
  return(p_mapping)
}

# Create mapping plots for each base
mapping_plots_list <- lapply(log_bases, create_mapping_plot)

# Combine all transformation plots only
final_plot <- wrap_plots(plots_list, ncol = 2, nrow = 2) + 
  plot_annotation(
    title = "Strictness Parameter Behavior Across Different Log Bases",
    subtitle = "Probability Transformations for Different Log Bases",
    tag_levels = "A"
  )

print(final_plot)

# Save the plot
ggsave("log_strictness_multibase_comparison.png", 
       width = 30, height = 24, dpi = 600, units = "cm")
