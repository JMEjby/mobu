# ==============================================================================
# Dirichlet Distribution Analysis for Signal Probability Modeling
# ==============================================================================
# This script analyzes Dirichlet distributions for different value profiles
# and creates side-by-side visualizations for 5-signal and 6-signal scenarios

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(MCMCpack)   # For Dirichlet distribution sampling
library(patchwork)  # For combining plots
library(RColorBrewer)  # For colour palettes

# ==============================================================================
# Function Definition
# ==============================================================================

#' Calculate Expected Values for Dirichlet Distribution
#' 
#' @param alpha Numeric vector of Dirichlet parameters (must be positive)
#' @param c Concentration parameter (for reference, not used in calculations)
#' @return Tibble with components, alpha values, expected values, and sample means
dirichlet_expected_values <- function(alpha, c) {
  
  # Input validation
  if (!is.numeric(alpha) || length(alpha) < 2) {
    stop("alpha must be a numeric vector with at least 2 elements")
  }
  
  if (any(alpha <= 0)) {
    stop("All alpha values must be positive")
  }
  
  # Calculate theoretical expected values for Dirichlet distribution
  # E[X_i] = alpha_i / sum(alpha)
  alpha_sum <- sum(alpha)
  expected_values <- alpha / alpha_sum
  
  # Generate Monte Carlo samples to verify theoretical calculations
  samples <- rdirichlet(10000, alpha)
  sample_means <- colMeans(samples)
  
  # Return structured results
  result <- tibble(
    Component = paste0("s = ", 1:length(alpha)),
    Alpha = alpha,
    concentration = c,
    Expected_Value = expected_values,
    Sample_Mean = round(sample_means, 2)
  )
  
  return(result)
}

# Function to create a centered Dirichlet parameter vector
create_dirichlet_params <- function(center, concentration, spread, k = 5, decay = 2) {
  # Create Dirichlet alpha parameters centered on a specific category
  # center: which category to center on (1 to k)
  # concentration: overall concentration (higher = more peaked)
  # spread: how much to spread to neighboring categories
  
  alphas <- rep(1, k)  # Base alpha = 1 (uniform prior)
  
  # Add concentration to the center
  alphas[center] <- 1 + concentration
  
  # Add some spread to neighbors based on distance
  for(i in 1:k) {
    if(i != center) {
      distance <- abs(i - center)
      alphas[i] <- 1 + (concentration * spread * exp(-decay * distance))
    }
  }
  
  return(alphas)
}

# ==============================================================================
# 5-Signal Analysis
# ==============================================================================

# Define alpha parameter vectors for different value profiles
A_vec <- c(8, 7, 3, 1.5, 0.5)      # Profile A: High preference for early signals
B_vec <- c(4.5, 6, 5.5, 3.25, 0.75) # Profile B: Moderate preference distribution
C_vec <- c(1, 5, 8, 5, 1)           # Profile C: Peak preference for middle signal
D_vec <- c(0.75, 3.25, 5.5, 6, 4.5) # Profile D: Increasing preference toward end
E_vec <- c(0.5, 1.5, 3, 7, 8)       # Profile E: Strong preference for later signals

# Organize vectors and names for processing
vecs <- list(A_vec, B_vec, C_vec, D_vec, E_vec)
vec_names <- c("A", "B", "C", "D", "E")
c_value <- 20  # Concentration parameter (scaling factor)

# Calculate expected values for each profile
combined_df_5 <- map2_dfr(vecs, vec_names, ~ {
  tab <- dirichlet_expected_values(.x, c_value)
  tab$value_profile <- .y
  tab$summary <- paste0("An agent with value Profile ", .y, 
                        " produces 1s ", 
                        round(tab$Expected_Value[1]*100), " % of the time, ",
                        "2s ", 
                        round(tab$Expected_Value[2]*100), " % of the time, ",
                        "3s ", 
                        round(tab$Expected_Value[3]*100), " % of the time, ",
                        "4s ", 
                        round(tab$Expected_Value[4]*100), " % of the time, and", 
                        "5s ", 
                        round(tab$Expected_Value[5]*100), " % of the time")
  return(tab)
})

# Generate Dirichlet samples for visualization (500 samples each)
samples_A <- rdirichlet(500, A_vec * c_value)
samples_B <- rdirichlet(500, B_vec * c_value)
samples_C <- rdirichlet(500, C_vec * c_value)
samples_D <- rdirichlet(500, D_vec * c_value)
samples_E <- rdirichlet(500, E_vec * c_value)

# ==============================================================================
# Label Formatting Functions
# ==============================================================================

#' Format Alpha Values and Expected Values for Plot Labels
#' 
#' @param alpha_vec Original alpha parameter vector
#' @param exp_vals Expected values from Dirichlet distribution
#' @return List with formatted alpha and expected value strings
format_alpha_and_exp <- function(alpha_vec, exp_vals) {
  # Format scaled alpha values (alpha * c)
  alpha_formatted <- paste0("[", paste(round(alpha_vec * c_value, 3), collapse = ", "), "]")
  # Format expected values with 3 decimal places
  exp_formatted <- paste0("[", paste(round(exp_vals, 3), collapse = ", "), "]")
  return(list(alpha = alpha_formatted, exp = exp_formatted))
}

# Extract expected values for each profile from combined dataframe
exp_A <- combined_df_5 |> filter(value_profile == "A") |> pull(Expected_Value)
exp_B <- combined_df_5 |> filter(value_profile == "B") |> pull(Expected_Value)
exp_C <- combined_df_5 |> filter(value_profile == "C") |> pull(Expected_Value)
exp_D <- combined_df_5 |> filter(value_profile == "D") |> pull(Expected_Value)
exp_E <- combined_df_5 |> filter(value_profile == "E") |> pull(Expected_Value)

# Create formatted labels for each profile
label_A <- format_alpha_and_exp(A_vec, exp_A)
label_B <- format_alpha_and_exp(B_vec, exp_B)
label_C <- format_alpha_and_exp(C_vec, exp_C)
label_D <- format_alpha_and_exp(D_vec, exp_D)
label_E <- format_alpha_and_exp(E_vec, exp_E)

# ==============================================================================
# Data Preparation for 5-Signal Visualization
# ==============================================================================

# Combine sample data with enhanced multi-line labels
df_combined_5 <- rbind(
  data.frame(samples_A, alpha_group = paste0("Value Profile A\nα = ", label_A$alpha, ", \nE[p] = ", label_A$exp)),
  data.frame(samples_B, alpha_group = paste0("Value Profile B\nα = ", label_B$alpha, ", \nE[p] = ", label_B$exp)),
  data.frame(samples_C, alpha_group = paste0("Value Profile C\nα = ", label_C$alpha, ", \nE[p] = ", label_C$exp)),
  data.frame(samples_D, alpha_group = paste0("Value Profile D\nα = ", label_D$alpha, ", \nE[p] = ", label_D$exp)),
  data.frame(samples_E, alpha_group = paste0("Value Profile E\nα = ", label_E$alpha, ", \nE[p] = ", label_E$exp))
)

# Transform to long format for ggplot visualization
df_long_combined_5 <- df_combined_5 |>
  pivot_longer(cols = -alpha_group, 
               names_to = "variable", 
               values_to = "value") |>
  # Rename variables to meaningful signal labels
  mutate(variable = case_when(
    variable == "X1" ~ "s = 1",
    variable == "X2" ~ "s = 2",
    variable == "X3" ~ "s = 3",
    variable == "X4" ~ "s = 4",
    variable == "X5" ~ "s = 5"
  ))

# Create colour palette for 5 signals using RcolourBrewer Set3
colours_5 <- brewer.pal(5, "Set3")

# ==============================================================================
# 5-Signal Plot Creation
# ==============================================================================

plot_5 <- ggplot(df_long_combined_5, aes(x = value, fill = variable)) +
  # Create overlapping histograms with transparency and black outlines
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, colour = "black") +
  # Create separate facets for each value profile (single column layout)
  facet_wrap(~alpha_group, ncol = 1) +
  # Set consistent axis scales across all plots
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  # Apply custom colour palette
  scale_fill_manual(values = colours_5) +
  # Add labels and titles
  labs(title = "5 Signal Distribution",
       x = "Probability", 
       y = "Frequency",
       fill = "Signal") +
  # Apply custom theme matching reference style
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "black"),
        strip.text = element_text(size = 11, face = "bold", lineheight = 0.8),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        title = element_text(size = 10),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.position = "bottom")  # Position legend at bottom

# ==============================================================================
# 6-Signal Analysis
# ==============================================================================

# Define alpha parameter vectors for 6-signal scenario
A_vec_6 <- c(4, 4, 1.5, 0.399, 0.1, 0.001) # Profile A: Extended with additional high signal
B_vec_6 <- c(0.1, 4, 4, 1.5, 0.395, 0.005) # Profile A: Extended with additional high signal
C_vec_6 <- c(0.1, 0.2, 1.2, 7, 1.2, 0.3) # Profile A: Extended with additional high signal
D_vec_6 <- c(0.005, 0.395, 1.5, 4, 4, 0.1) # Profile A: Extended with additional high signal
E_vec_6 <- c(0.001, 0.05, 0.05, 0.399, 5.5, 4) # Profile A: Extended with additional high signal

# Organize 6-signal vectors for processing
vecs_6 <- list(A_vec_6, B_vec_6, C_vec_6, D_vec_6, E_vec_6)
vec_names_6 <- c("A", "B", "C", "D", "E")
c_value <- 40

# Calculate expected values for 6-signal profiles
combined_df_6 <- map2_dfr(vecs_6, vec_names_6, ~ {
  tab <- dirichlet_expected_values(.x, c_value)
  tab$Alpha <- round(tab$Alpha,2)
  tab$Expected_Value <- round(tab$Expected_Value, 2)
  tab$value_profile <- .y
  tab$summary <- paste0("An agent with value Profile ", .y, 
                        " produces 3s ", 
                        round(tab$Expected_Value[1]*100), " % of the time, ",
                        "2s ", 
                        round(tab$Expected_Value[2]*100), " % of the time, ",
                        "1s ", 
                        round(tab$Expected_Value[3]*100), " % of the time, ",
                        "0s ", 
                        round(tab$Expected_Value[4]*100), " % of the time, ", 
                        "-1s ", 
                        round(tab$Expected_Value[5]*100), " % of the time, and ",
                        "-2s ",
                        round(tab$Expected_Value[6]*100), " % of the time.")
  return(tab)
})

# Generate Dirichlet samples for 6-signal visualization
samples_A_6 <- rdirichlet(500, A_vec_6 * c_value)
samples_B_6 <- rdirichlet(500, B_vec_6 * c_value)
samples_C_6 <- rdirichlet(500, C_vec_6 * c_value)
samples_D_6 <- rdirichlet(500, D_vec_6 * c_value)
samples_E_6 <- rdirichlet(500, E_vec_6 * c_value)

# Extract expected values for 6-signal profiles
exp_A_6 <- combined_df_6 |> filter(value_profile == "A") |> pull(Expected_Value)
exp_B_6 <- combined_df_6 |> filter(value_profile == "B") |> pull(Expected_Value)
exp_C_6 <- combined_df_6 |> filter(value_profile == "C") |> pull(Expected_Value)
exp_D_6 <- combined_df_6 |> filter(value_profile == "D") |> pull(Expected_Value)
exp_E_6 <- combined_df_6 |> filter(value_profile == "E") |> pull(Expected_Value)

# Create formatted labels for 6-signal profiles
label_A_6 <- format_alpha_and_exp(A_vec_6, exp_A_6)
label_B_6 <- format_alpha_and_exp(B_vec_6, exp_B_6)
label_C_6 <- format_alpha_and_exp(C_vec_6, exp_C_6)
label_D_6 <- format_alpha_and_exp(D_vec_6, exp_D_6)
label_E_6 <- format_alpha_and_exp(E_vec_6, exp_E_6)

# ==============================================================================
# Data Preparation for 6-Signal Visualization
# ==============================================================================

# Combine 6-signal sample data with enhanced labels
df_combined_6 <- rbind(
  data.frame(samples_A_6, alpha_group = paste0("Value Profile A\nα = ", label_A_6$alpha, ", \nE[p] = ", label_A_6$exp)),
  data.frame(samples_B_6, alpha_group = paste0("Value Profile B\nα = ", label_B_6$alpha, ", \nE[p] = ", label_B_6$exp)),
  data.frame(samples_C_6, alpha_group = paste0("Value Profile C\nα = ", label_C_6$alpha, ", \nE[p] = ", label_C_6$exp)),
  data.frame(samples_D_6, alpha_group = paste0("Value Profile D\nα = ", label_D_6$alpha, ", \nE[p] = ", label_D_6$exp)),
  data.frame(samples_E_6, alpha_group = paste0("Value Profile E\nα = ", label_E_6$alpha, ", \nE[p] = ", label_E_6$exp))
)

# Transform 6-signal data to long format with ORDERED factor levels
df_long_combined_6 <- df_combined_6 |>
  pivot_longer(cols = -alpha_group, 
               names_to = "variable", 
               values_to = "value") |>
  # Rename variables to meaningful signal labels (now including s = 6)
  mutate(variable = case_when(
    variable == "X1" ~ "s = 3",
    variable == "X2" ~ "s = 2",
    variable == "X3" ~ "s = 1",
    variable == "X4" ~ "s = 0",
    variable == "X5" ~ "s = -1",
    variable == "X6" ~ "s = -2"
  )) |>
  # Set factor levels in desired order
  mutate(variable = factor(variable, levels = c("s = 3", "s = 2", "s = 1", "s = 0", "s = -1", "s = -2")))

# Create colour palette for 6 signals
colours_6 <- brewer.pal(6, "Set3")
colours_6 <- colours_6[c(6, 1,2,3,4,5)] # gets colours to match

# ==============================================================================
# 6-Signal Plot Creation
# ==============================================================================

plot_6 <- ggplot(df_long_combined_6, aes(x = value, fill = variable)) +
  # Create overlapping histograms with transparency and black outlines
  geom_histogram(alpha = 0.6, position = "identity", bins = 30, colour = "black") +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "black") +  # Add vertical line at 0
  # Create separate facets for each value profile (single column layout)
  facet_wrap(~alpha_group, ncol = 1) +
  # Set consistent axis scales across all plots (same as 5-signal plot)
  scale_x_continuous(limits = c(0,1), breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  # Apply custom colour palette for 6 signals
  scale_fill_manual(values = colours_6) +
  # Add labels and titles
  labs(title = "6 Signal Distribution",
       x = "Probability", 
       y = "Frequency",
       fill = "Signal") +
  # Apply same custom theme with legend included
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "black"),
        strip.text = element_text(size = 11, face = "bold", lineheight = 0.8),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        title = element_text(size = 10),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.position = "bottom")  # Include legend for 6-signal plot

# ==============================================================================
# Third Plot - Comparison Plot
# ==============================================================================

# Create comparison data for specific signal pairs
# Pairs: (3) (1,2) (2,1) (3,0) (4,-1) (5,-2)
# where first element is from 5-signal and second is from 6-signal

# First, let's create data frames with profile information for easier merging
df_5_with_profile <- df_combined_5 |>
  mutate(value_profile = case_when(
    grepl("Value Profile A", alpha_group) ~ "A",
    grepl("Value Profile B", alpha_group) ~ "B", 
    grepl("Value Profile C", alpha_group) ~ "C",
    grepl("Value Profile D", alpha_group) ~ "D",
    grepl("Value Profile E", alpha_group) ~ "E"
  ))

df_6_with_profile <- df_combined_6 |>
  mutate(value_profile = case_when(
    grepl("Value Profile A", alpha_group) ~ "A",
    grepl("Value Profile B", alpha_group) ~ "B", 
    grepl("Value Profile C", alpha_group) ~ "C",
    grepl("Value Profile D", alpha_group) ~ "D",
    grepl("Value Profile E", alpha_group) ~ "E"
  ))

# Define the signal pairs for comparison
comparison_pairs <- list(
  list(signal_5 = NA, signal_6 = "X1", column_name = "s = 3 only"),
  list(signal_5 = "X1", signal_6 = "X2", column_name = "s = 1 vs s = 2"),  
  list(signal_5 = "X2", signal_6 = "X3", column_name = "s = 2 vs s = 1"),
  list(signal_5 = "X3", signal_6 = "X4", column_name = "s = 3 vs s = 0"),
  list(signal_5 = "X4", signal_6 = "X5", column_name = "s = 4 vs s = -1"),
  list(signal_5 = "X5", signal_6 = "X6", column_name = "s = 5 vs s = -2")
)

# Create comparison dataset

comparison_data <- map_dfr(comparison_pairs, function(pair) {
  
  result_list <- map_dfr(c("A", "B", "C", "D", "E"), function(profile) {
    
    # Initialize empty data frames
    data_5 <- data.frame()
    data_6 <- data.frame()
    
    # Get 5-signal data if signal_5 exists
    if (!is.na(pair$signal_5)) {
      # Filter for the profile
      temp_df_5 <- df_5_with_profile[df_5_with_profile$value_profile == profile, ]
      # Extract the specific column and create new data frame
      data_5 <- data.frame(
        value = temp_df_5[[pair$signal_5]],  # Use [[ ]] for column extraction
        source = "5-signal",
        signal_pair = pair$column_name,
        value_profile = paste0("Value Profile ", profile),
        stringsAsFactors = FALSE
      )
    }
    
    # Get 6-signal data if signal_6 exists
    if (!is.na(pair$signal_6)) {
      # Filter for the profile
      temp_df_6 <- df_6_with_profile[df_6_with_profile$value_profile == profile, ]
      # Extract the specific column and create new data frame
      data_6 <- data.frame(
        value = temp_df_6[[pair$signal_6]],  # Use [[ ]] for column extraction
        source = "6-signal",
        signal_pair = pair$column_name,
        value_profile = paste0("Value Profile ", profile),
        stringsAsFactors = FALSE
      )
    }
    
    # Combine the data frames
    if (nrow(data_5) > 0 && nrow(data_6) > 0) {
      return(rbind(data_5, data_6))
    } else if (nrow(data_5) > 0) {
      return(data_5)
    } else if (nrow(data_6) > 0) {
      return(data_6)
    } else {
      return(data.frame())
    }
  })
  
  return(result_list)
})

# Set the desired order for signal pairs in the comparison plot
comparison_data <- comparison_data |>
  mutate(signal_pair = factor(signal_pair, 
                              levels = c("s = 3 only", 
                                         "s = 1 vs s = 2", 
                                         "s = 2 vs s = 1", 
                                         "s = 3 vs s = 0", 
                                         "s = 4 vs s = -1", 
                                         "s = 5 vs s = -2"))
  ) 

# Create the comparison plot
plot_comparison <- ggplot(comparison_data, aes(x = value, fill = signal_pair, linetype = source, alpha = source)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "black")+
  geom_histogram(position = "identity", bins = 30, colour = "black") +
  facet_grid(value_profile~signal_pair, scales = "free") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0,1, by = 0.2)) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, by = 100)) +
  scale_fill_manual(values = colours_6) +
  scale_alpha_manual(values = c("5-signal" = 0.4, "6-signal" = 0.8)) +
  scale_linetype_manual(values = c("5-signal" = "dashed", "6-signal" = "solid")) +
  labs(title = "Signal Comparison: 5-Signal vs 6-Signal",
       x = "Probability", 
       y = "Frequency",
       fill = "Source",
       linetype = "Source",
       alpha = "Source") +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white", colour = "black"),
        strip.text = element_text(size = 9, face = "bold"),
        strip.text.x = element_blank(),  # Hide y-axis strip text
        strip.background.x = element_blank(),  # Hide y-axis strip background
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 8),
        axis.title = element_text(face = "bold", size = 10),
        title = element_text(size = 10),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.position = "none")

print(plot_comparison)

# ==============================================================================
# Final Plot Combination and Display
# ==============================================================================

# Combine all three plots using patchwork with custom widths
combined_plot <- (plot_6 | plot_comparison) +
  plot_layout(ncol = 2, widths = c(1, 1, 1.7)) 

combined_plot <-combined_plot + 
  plot_annotation(
    title = "Distribution of 500 Dirichlet Generated Probabilities for each Value Profile",
    theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
  )

# Display the final combined visualization
print(combined_plot)
