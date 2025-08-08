# Generate parameter combinations CSV file for NetLogo array jobs
# Simple non-interactive version - just edit the parameters below and run!

# =============================================================================
# CONFIGURATION - EDIT THESE SETTINGS
# =============================================================================

# Output filename
output_file <- "parameter_combinations_narrow.csv"

# Choose parameter set: "test", "focused" or "extra"
preset <- "narrow"

# =============================================================================
# PARAMETER DEFINITIONS
# =============================================================================

if (preset == "test") {
  # Small test set for debugging (~100 tasks)
  parameters <- list(
    movement_saturation = 5,
    n_agents = c(100),
    signal_representation = c("categorical"),
    relative_leader_influence = 50,
    log_recruitment_strictness = 0.5,
    log_desertion_strictness = -0.5,
    social_influence = 0.25,
    n_signals = 5,
    new_signal_rate = 100,
    leader_values = "value-profile-A",
    leader_election = "most-aligned",
    leader_lifespan = 100,
    population_composition = "normal-narrow",
    ea_bias = 1.5,
    n_ticks = 32,
    data_interval = 1,
    runs_per_combo = 10
  )
  
} else if (preset == "narrow") {
  parameters <- list(
    movement_saturation = seq(2.5, 10, by=2.5),
    n_agents = 600,
    signal_representation = c("categorical"),
    relative_leader_influence = seq(0, 100, by=25),
    log_recruitment_strictness = c(-1.5, seq(-1,1,by=0.25), 1.5),
    log_desertion_strictness = c(-1.5, seq(-1,1,by=0.25), 1.5),
    social_influence = c(0, 0.25, 0.5, 1, 3),
    n_signals = 5,
    new_signal_rate = 100,
    leader_values = c("value-profile-A", "value-profile-B", "value-profile-C"),
    leader_election = c("random-ea", "most-aligned", "external"),
    leader_lifespan = 33,
    population_composition = c("normal-narrow"),
    ea_bias = c(0, 1, 1.5),
    n_ticks = 32,
    data_interval = 1,
    runs_per_combo = 10
  )
} else if (preset == "custom") {
  parameters <- list(
    movement_saturation = seq(2,10, by=2),
    n_agents = c(600),
    signal_representation = c("categorical"),
    relative_leader_influence = seq(0,100,by=10),
    log_recruitment_strictness = c(-2, -1.5, seq(-1,1,by=0.2), 1, 2),
    log_desertion_strictness = c(-2, -1.5, seq(-1,1,by=0.2), 1, 2),
    social_influence = c(0, 0.2, 0.6, 1, 3),
    n_signals = 5,
    new_signal_rate = 100,
    leader_values = c("value-profile-A", "value-profile-B", "value-profile-C", "value-profile-D"),
    leader_election = c("random-ea", "most-aligned", "external"),
    leader_lifespan = 33,
    population_composition = c("normal-narrow", "normal-wide"),
    ea_bias = c(0, 1, 1.5),
    n_ticks = 32,
    data_interval = 1,
    runs_per_combo = 20
  )
} 

# =============================================================================
# GENERATION PROCESS - NO NEED TO EDIT BELOW
# =============================================================================

cat("NetLogo Parameter Combinations Generator\n")
cat("=========================================\n\n")

# Print parameter set being used
cat(sprintf("Using %s parameter set:\n", preset))
for (param_name in names(parameters)) {
  param_values <- parameters[[param_name]]
  if (length(param_values) > 5) {
    # Show first few and last few values for long lists
    display_values <- c(head(param_values, 3), "...", tail(param_values, 2))
    cat(sprintf("  %s: %s\n", param_name, paste(display_values, collapse=", ")))
  } else {
    cat(sprintf("  %s: %s\n", param_name, paste(param_values, collapse=", ")))
  }
}

# Calculate total combinations
runs_per_combo <- max(parameters$runs_per_combo)

total_combos <- prod(sapply(parameters, length))
total_tasks <- total_combos * runs_per_combo

cat(sprintf("\nTotal parameter combinations: %d\n", total_combos))
cat(sprintf("Runs per combination: %d\n", runs_per_combo))
cat(sprintf("Total tasks: %d\n", total_tasks))

# Generate all parameter combinations
cat("\nGenerating parameter combinations...\n")

# Use expand.grid to create all combinations
param_grid <- expand.grid(parameters, stringsAsFactors = FALSE)

# Create final data frame with combo_id
combinations <- data.frame()

new_col <- data.frame(
  combo_id = 1:nrow(param_grid)
)
combinations <- cbind(new_col, param_grid)


# Save to CSV
write.csv(combinations, file = output_file, row.names = FALSE)

cat(sprintf("\n✓ Generated %d parameter combinations\n", nrow(combinations)))
cat(sprintf("✓ Saved to: %s\n", output_file))
cat(sprintf("✓ Total tasks: %d\n", nrow(combinations)))

cat(sprintf("\nTo submit array job on cluster:\n"))
cat(sprintf("qsub -t 1-%d run_array_job.sh %s\n", nrow(combinations), output_file))
cat(sprintf("\nTo limit concurrent tasks (recommended):\n"))
cat(sprintf("qsub -t 1-%d -tc 50 run_array_job.sh %s\n", nrow(combinations), output_file))

# Show first few rows as example
cat("\nFirst few parameter combinations:\n")
print(head(combinations, 3))

cat("\n✓ Parameter generation complete!\n")#!/usr/bin/env Rscript
