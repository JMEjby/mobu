# NetLogo Leader Selection Monte Carlo Simulation
# Simulates the leader selection process with exponential bias

library(tidyverse)

# Function to convert continuous value to category
value_category <- function(cont_val) {
  case_when(
    cont_val > 1.5 ~ "A",
    cont_val > 0.5 ~ "B", 
    cont_val > -0.5 ~ "C",
    cont_val > -1.5 ~ "D",
    TRUE ~ "E"
  )
}

# Function to select weighted index (mimics NetLogo function)
select_weighted_index <- function(bias_strength, population_values) {
  # Calculate exponential weights
  weights <- exp(bias_strength * population_values)
  
  # Sample with weighted probability
  selected_index <- sample(length(population_values), size = 1, prob = weights)
  return(selected_index)
}

# Function to simulate leader selection for one population
simulate_leader_selection <- function(n_agents, bias_strength = 3.0) {
  # Generate normally distributed values
  population_values <- rnorm(n_agents, mean = -0.5, sd = 1)
  
  # Sort in descending order (highest first)
  population_values <- sort(population_values, decreasing = TRUE)
  
  # Select leader with exponential bias
  selected_index <- select_weighted_index(bias_strength, population_values)
  leader_value <- population_values[selected_index]
  
  # Convert to category
  leader_category <- value_category(leader_value)
  
  return(list(
    category = leader_category,
    value = leader_value,
    rank = selected_index,
    n_agents = n_agents
  ))
}

# Monte Carlo simulation
run_monte_carlo <- function(n_agents_list, n_simulations = 10000, bias_strength = 3.0) {
  results <- data.frame()
  
  for (n_agents in n_agents_list) {
    cat("Running simulations for n_agents =", n_agents, "\n")
    
    # Run simulations
    sim_results <- replicate(n_simulations, 
                             simulate_leader_selection(n_agents, bias_strength), 
                             simplify = FALSE)
    
    # Extract results
    categories <- sapply(sim_results, function(x) x$category)
    values <- sapply(sim_results, function(x) x$value)
    ranks <- sapply(sim_results, function(x) x$rank)
    
    # Create data frame for this population size
    sim_df <- data.frame(
      n_agents = n_agents,
      category = categories,
      value = values,
      rank = ranks,
      simulation = 1:n_simulations
    )
    
    results <- rbind(results, sim_df)
  }
  
  return(results)
}

# Run simulations
n_agents_list <- seq(50,1000, by = 50)  # Population sizes from 50 to 1000
set.seed(123)  # For reproducibility
results <- run_monte_carlo(n_agents_list)

# Calculate probabilities
probability_summary <- results |>
  group_by(n_agents, category) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(n_agents) |>
  mutate(
    probability = count / sum(count),
    percentage = probability * 100
  ) |>
  ungroup()

# Print results
cat("\n=== LEADER CATEGORY PROBABILITIES ===\n\n")
for (n in n_agents_list) {
  cat("Population size:", n, "\n")
  subset_data <- probability_summary[probability_summary$n_agents == n, ]
  
  # Ensure all categories are represented (even if 0%)
  all_categories <- data.frame(category = c("A", "B", "C", "D", "E"))
  subset_data <- merge(all_categories, subset_data, by = "category", all.x = TRUE)
  subset_data[is.na(subset_data)] <- 0
  subset_data <- subset_data[order(subset_data$category), ]
  
  for (i in 1:nrow(subset_data)) {
    cat(sprintf("  %s: %6.2f%% (%d/%d)\n", 
                subset_data$category[i], 
                subset_data$percentage[i], 
                subset_data$count[i], 
                1000))
  }
  cat("\n")
}

# Additional statistics
cat("=== ADDITIONAL STATISTICS ===\n\n")
rank_summary <- results |>
  group_by(n_agents) |>
  summarise(
    mean_rank = mean(rank),
    median_rank = median(rank),
    rank_1_pct = sum(rank == 1) / n() * 100,
    top_5_pct = sum(rank <= 5) / n() * 100,
    top_10_pct = sum(rank <= 10) / n() * 100,
    .groups = "drop"
  )

print(rank_summary)

# Create visualization
p1 <- ggplot(probability_summary, aes(x = category, y = percentage, fill = factor(n_agents))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~n_agents, labeller = labeller(n_agents = function(x) paste("n =", x))) +
  labs(
    title = "Leader Category Probabilities by Population Size",
    subtitle = "Based on 1000 Monte Carlo simulations with bias strength = 3.0",
    x = "Value Profile Category",
    y = "Probability (%)",
    fill = "Population Size"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

# Create rank distribution plot
p2 <- ggplot(results, aes(x = rank)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
  facet_wrap(~n_agents, scales = "free_x", 
             labeller = labeller(n_agents = function(x) paste("n =", x))) +
  labs(
    title = "Distribution of Leader Selection Rank",
    subtitle = "Rank 1 = highest value in population",
    x = "Rank (1 = highest)",
    y = "Frequency"
  ) +
  theme_minimal()

print(p2)

# Save results
write.csv(probability_summary, "leader_probabilities.csv", row.names = FALSE)
write.csv(results, "simulation_results.csv", row.names = FALSE)

cat("\nResults saved to 'leader_probabilities.csv' and 'simulation_results.csv'\n")