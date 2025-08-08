library(tidyverse)
library(patchwork)

# Create a range of probabilities
p <- seq(0.01, 0.99, by = 0.01)

# Define beta values (our primary parameter)
beta_values <- seq(-1, 1, by = 0.25)

# Calculate corresponding gamma values
gamma_values <- 100^beta_values

# Show the parameter mapping
param_mapping <- data.frame(
  beta = beta_values,
  gamma = gamma_values
)

# Create data for visualization
data_beta <- expand.grid(p = p, beta = beta_values) |>
  mutate(
    gamma = 100^beta,
    p_transformed = p^gamma,
    beta_label = paste("β =", beta)
  )

# Plot 1: Main transformation plot with beta labels
# Create custom legend labels showing beta and corresponding gamma values
beta_labels <- unique(data_beta$beta)
gamma_values_for_legend <- 100^beta_labels
legend_labels <- sapply(1:length(beta_labels), function(i) {
  as.expression(bquote("β"[r] * " =" * .(beta_labels[i]) * " (γ"[r] * " =" * .(round(gamma_values_for_legend[i], 2)) * ")"))
})

p1 <- ggplot(data_beta, aes(x = p, y = p_transformed, color = factor(beta))) +
  geom_line(size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha=0.5)+
  labs(
    title = expression("Probability Transformation: " * P^(100^beta[r])),
    x = expression(atop("Perceived EA Alignment Probability", " (" * P(EA)[i] * ")")),
    y = expression(atop("Strictness-Adjusted", "Recruitment Probability (" * P(EA)[i]^gamma[r] * ")")),
    color = expression(beta[r] * " (" * gamma[r] * " = " * 100^beta[r] * ")"),
    caption = expression(beta[r] * " is the " * log[100] * "-scaled parameter, " * gamma[r] * " = " * 100^beta[r])
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
        legend.background = element_rect(fill = "white", color = "black"))

# Plot 2: Show the beta -> gamma mapping
p2 <- ggplot(data.frame(beta = beta_values, gamma = 100^beta_values), 
             aes(x = beta, y = gamma)) +
  geom_line(linewidth = 1.2, color = "darkblue") +
  geom_point(data = param_mapping, aes(x = beta, y = gamma), 
             size = 4, color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_y_continuous(trans = scales::log_trans(base = 100)) +
  labs(
    title = expression("Parameter Mapping: " * beta[r] * " → " * gamma[r]),
    x = expression(atop(beta[r], " (model parameter)")),
    y = expression(atop(gamma[r], "(actual exponent)")),
    caption = expression(gamma[r] * " = " * 100^beta[r] * " (red points show chosen " * beta[r] * " values)")
  ) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "lightgrey"),
        panel.grid.minor.x = element_line(color = NA),          
        panel.grid.major.x = element_line(color = NA),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(size = 12),
        title = element_text(size = 10),
        plot.caption = element_text(hjust = 0, size = 10),
        legend.background = element_rect(fill = "white", color = "black"))

# Plot 3: Moderator effects on desertion probability (original approach)
# Define moderator values
moderator_values <- c(1, 2, 3, 4,5,6)

# Create data for moderator plot
data_moderator <- expand.grid(
  p = p, 
  beta = beta_values, 
  moderator = moderator_values
) |>
  mutate(
    gamma_moderated = 100^(beta*moderator),
    p_desertion = 1 - p^gamma_moderated,
    beta_label = paste("β =", beta),
    moderator_label = factor(paste("t(EA)[i] ==", moderator), 
                             levels = paste("t(EA)[i] ==", moderator_values))
  )

p3 <- ggplot(data_moderator, aes(x = p, y = p_desertion, color = factor(beta))) +
  geom_line(size = 1.2) +
  facet_wrap(~moderator_label, ncol = 2, axes = "all_x", labeller = label_parsed) +
  labs(
    x = expression("Perceived EA Alignment Probability: " * PA(EA)[i]),
    y = expression("Desertion Probability: " * 1 - PA(EA)[i]^gamma[r]),
    color = expression(beta[r]),
    caption = expression("Desertion probability = " * 1 - PA^(100^(beta[r] * "×" * t(EA)[i])) * ", where mod is the moderator value")
  ) +
  scale_color_brewer(type = "div", palette = "RdBu", direction = -1) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "lightgrey"),
        panel.grid.minor.x = element_line(color = NA),          
        panel.grid.major.x = element_line(color = NA),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 11, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 9),
        axis.title = element_text(size = 11),
        title = element_text(size = 10),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.background = element_rect(fill = "white", color = "black")
        )

# Plot 4: Logit transformation with moderators
# Define logit transformation function
transform_logit <- function(p, gamma) {
  logit_p <- log(p / (1 - p))          # Transform to logit space
  scaled_logit <- gamma + logit_p       # Scale by gamma
  exp(scaled_logit) / (1 + exp(scaled_logit))  # Transform back to probability
}

# Create data for logit moderator plot
data_logit_moderator <- expand.grid(
  p = p,
  beta = beta_values, 
  moderator = moderator_values
) |>
  mutate(
    beta_moderated = beta*moderator,
    p_transformed_logit = transform_logit(p, beta_moderated),
    p_desertion_logit = 1 - p_transformed_logit,
    beta_label = paste("β =", beta),
    moderator_label = factor(paste("Moderator =", moderator), 
                             levels = paste("Moderator =", moderator_values))
  )

p4 <- ggplot(data_logit_moderator, aes(x = p, y = p_desertion_logit, color = factor(beta))) +
  geom_line(size = 1.2) +
  facet_wrap(~moderator_label, ncol = 2, axes = "all_x") +
  labs(
    title = expression("Logit Transformation: Desertion Probability"),
    x = expression(atop("Perceived EA Alignment Probability", P(EA)[i] )),
    y = expression(atop("Desertion Probability", "(1 - logit"^{-1} * "(γ"[r] * "×mod×logit(" * P(EA)[i] * ")))")),
    color = expression(beta[r]),
    caption = expression("Logit transformation: " * P[transformed] * " = logit"^{-1} * "(100"^{beta[r]} * "×mod×logit(" * P * "))")
  ) +
  scale_color_brewer(type = "div", palette = "RdBu", direction = -1) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major = element_line(color = "grey"),
        panel.grid.minor = element_line(color = "lightgrey"),
        #panel.grid.minor.x = element_line(color = NA),          
       # panel.grid.major.x = element_line(color = NA),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 9),
        axis.title = element_text(size = 11),
        title = element_text(size = 10),
        plot.caption = element_text(hjust = 0, size = 9),
        legend.background = element_rect(fill = "white", color = "black"),
        strip.background = element_rect(fill = "lightgrey", color = "black"),
        strip.text = element_text(size = 10))

# Combine first two plots
plot_grid <- ( (p1) /( p2 ))+ 
  plot_annotation(tag_level = "A") 

# Display plots
print(plot_grid)
print(p3)
print(p4)

# Save plots
ggsave("log_strictness_behaviour.png", plot_grid, width = 19, height = 18, dpi = 600, units = "cm")
ggsave("moderator_effects_desertion.png", p3, width = 16, height = 15, dpi = 600, units = "cm")
ggsave("logit_moderator_effects_desertion.png", p4, width = 20, height = 15, dpi = 600, units = "cm")