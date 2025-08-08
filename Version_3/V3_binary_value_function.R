library(tidyverse)
library(viridis)

# Function for Approach 1: Proportional Shift (Linear)
update_binary_weights_linear <- function(current_weights, adjustment, learning_rate = 0.1) {
  weight_0 <- current_weights[1]
  weight_1 <- current_weights[2]
  
  # Positive adjustment = need more 1s, negative = need more 0s
  shift <- adjustment * learning_rate
  
  # Shift probability mass
  new_weight_1 <- weight_1 + shift
  new_weight_0 <- weight_0 - shift
  
  # Ensure bounds [0,1] and sum to 1
  new_weight_1 <- max(0, min(1, new_weight_1))
  new_weight_0 <- 1 - new_weight_1
  
  return(c(new_weight_0, new_weight_1))
}

# Function for Approach 2: Logistic Transformation
update_binary_weights_logistic <- function(current_weights, adjustment, learning_rate = 0.1) {
  weight_1 <- current_weights[2]
  
  # Avoid log(0) by adding small epsilon
  weight_1 <- max(1e-10, min(1-1e-10, weight_1))
  
  # Convert to log-odds
  log_odds <- log(weight_1 / (1 - weight_1))
  
  # Update log-odds
  log_odds <- log_odds + (adjustment * learning_rate)
  
  # Convert back to probability
  new_weight_1 <- exp(log_odds) / (1 + exp(log_odds))
  new_weight_0 <- 1 - new_weight_1
  
  return(c(new_weight_0, new_weight_1))
}

# Function for Approach 3: Logistic Transformation with 5x multiplier
update_binary_weights_logistic_5x <- function(current_weights, adjustment, learning_rate = 0.1) {
  weight_1 <- current_weights[2]
  
  # Avoid log(0) by adding small epsilon
  weight_1 <- max(1e-10, min(1-1e-10, weight_1))
  
  # Convert to log-odds
  log_odds <- log(weight_1 / (1 - weight_1))
  
  # Update log-odds with 5x multiplier
  log_odds <- log_odds + (adjustment * learning_rate * 5)
  
  # Convert back to probability
  new_weight_1 <- exp(log_odds) / (1 + exp(log_odds))
  new_weight_0 <- 1 - new_weight_1
  
  return(c(new_weight_0, new_weight_1))
}

# Create simulation parameters
adjustments <- seq(-1, 1, by = 0.05)
learning_rates <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
starting_weights <- list(
  c(0.9, 0.1),
  c(0.7, 0.3),
  c(0.5, 0.5),
  c(0.3, 0.7),
  c(0.1, 0.9)
)

# Create comprehensive data frame
results <- data.frame()
for (lr in learning_rates) {
  for (i in seq_along(starting_weights)) {
    start_weights <- starting_weights[[i]]
    start_label <- start_weights[2]
    
    for (adj in adjustments) {
      # Linear approach
      linear_result <- update_binary_weights_linear(start_weights, adj, lr)
      
      # Logistic approach  
      logistic_result <- update_binary_weights_logistic(start_weights, adj, lr)
      
      # Logistic approach with 5x multiplier
      logistic_5x_result <- update_binary_weights_logistic_5x(start_weights, adj, lr)
      
      # Add to results
      results <- rbind(results, data.frame(
        adjustment = adj,
        learning_rate = lr,
        starting_weights = start_label,
        linear = linear_result[2],
        logistic = logistic_result[2],
        logistic_5x = logistic_5x_result[2]
      ))
    }
  }
}

# Reshape for plotting
plot_data <- results |>
  pivot_longer(cols = c(logistic, logistic_5x, linear), 
               names_to = "method", 
               values_to = "new_weight_1") |>
  mutate(
    method = factor(case_when(
      method == "logistic" ~ "Logistic \nTransformation",
      method == "logistic_5x" ~ "Logistic \nTransformation (5σ)",
      method == "linear" ~ "Linear \n(Proportional Shift)"
    ), levels = c("Logistic \nTransformation", "Logistic \nTransformation (5σ)", "Linear \n(Proportional Shift)")),
    learning_rate_label = paste("σ = ", learning_rate),
    starting_weights = factor(starting_weights)
  )

# Extract actual adjustment values from the model
actual_adjustments <- data.frame(
  value = c(-0.28, -0.492, -0.068, -0.068, -0.28, -0.492,  # 70% relative-leader-influence
            0.316, 0.316, 0.316, -0.108, -0.108, 0.528,     # 70% relative-leader-influence
            0.28, -0.16, -0.16, 0.28, 0.5, 0.06,            # 50% relative-leader-influence
            0.4, 0.4, -0.26, -0.26, -0.04, -0.04,           # 50% relative-leader-influence
            -0.032, 0.196, -0.488, -0.26, -0.032, 0.196,    # 30% relative-leader-influence
            -0.2, -0.2, -0.2, 0.256, 0.028, 0.256)         # 30% relative-leader-influence
)
# choose examples
sampled_adjustments <- sort(c(-0.492, 0.528, 0.028, -0.032, 0.316, -0.280, 0.196, -0.200))


# Create the plot with facet_grid and vertical lines
ggplot(plot_data, aes(x = adjustment, y = new_weight_1, color = starting_weights)) +
  geom_vline(data = actual_adjustments, 
             aes(xintercept = value), linetype = "longdash", 
             color = "black", alpha = 0.3, linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  facet_grid(learning_rate_label ~ method) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  labs(
    title = "Comparison of EA-Signal Probability Update Methods Across \nSocial Influence Levels",
    x = "Perceived EA Misalignment Value",
    y = "Updated EA Signal Probability",
    color = "Initial EA-Signal Probability",
    caption = "Vertical lines show actual perceived EA misalignment values from model runs.\nσ denotes the social influence level."
  )  +
  scale_color_viridis(discrete = TRUE, option = "D", direction = -1) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor= element_blank(),
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

ggsave("Value_function_behaviour_bi.png", width = 16, height = 24.5, dpi = 600, units = "cm")

# Create data for the second plot using sampled adjustment values
plot_data_sampled <- expand_grid(
  adjustment = sampled_adjustments,
  learning_rate = learning_rates,
  starting_prob = seq(0.01, 0.99, by = 0.01),
  method = c("Logistic \nTransformation", "Logistic \nTransformation (5σ)", "Linear \n(Proportional Shift)")
) |>
  mutate(
    start_weights_vec = map(starting_prob, ~ c(1 - .x, .x))
  ) |>
  rowwise() |>
  mutate(
    new_weight_1 = case_when(
      method == "Linear \n(Proportional Shift)" ~ 
        update_binary_weights_linear(start_weights_vec, adjustment, learning_rate)[2],
      method == "Logistic \nTransformation" ~ 
        update_binary_weights_logistic(start_weights_vec, adjustment, learning_rate)[2],
      method == "Logistic \nTransformation (5σ)" ~ 
        update_binary_weights_logistic_5x(start_weights_vec, adjustment, learning_rate)[2]
    ),
    adjustment_label = round(adjustment, 3),
    learning_rate_label = paste("σ = ", learning_rate)
  ) |>
  ungroup() |>
  mutate(
    method = factor(method, levels = c("Logistic \nTransformation", "Logistic \nTransformation (5σ)", "Linear \n(Proportional Shift)")),
    adjustment_label = factor(adjustment_label, levels = round(sort(sampled_adjustments), 3))
  )

# Create the second plot with swapped axes
p2 <- ggplot(plot_data_sampled, aes(x = starting_prob, y = new_weight_1, color = adjustment_label)) +
  geom_line(size = 0.7) +
  facet_grid(learning_rate_label ~ method) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(
    title = "EA-Signal Updates: Response to Actual Model Adjustment Values",
    x = "Initial EA-Signal Probability",
    y = "Upadted EA-Signal Probability",
    color = "Perceived EA \nMisalignment Value",
    caption = "Shows response curves for 8 sampled perceived EA misalignment values from actual model runs.\nσ denotes the social influence level."
  ) +
  scale_color_viridis(discrete = TRUE) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 12, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 11),
        axis.title = element_text(face = "bold", size = 12),
        title = element_text(size = 10),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.position = "bottom")
print(p2)
ggsave("Value_function_examples_bi.png", width = 16, height = 22, dpi = 600, units = "cm")
