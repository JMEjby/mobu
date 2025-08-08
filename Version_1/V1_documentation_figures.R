# ==============================================================================
# Figures for Documentation for EA Growth Model (V1)
# ==============================================================================

# ------------------------------------------------------------------------------
# SETUP 
# ------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(DiagrammeR)
library(RColorBrewer)

cm_to_pixels <- function(cm, dpi = 600) {
  inches <- cm / 2.54  # Convert cm to inches
  pixels <- inches * dpi
  return(round(pixels))
}

# ------------------------------------------------------------------------------
# Figure 1: Conceptual Flow Diagram
# ------------------------------------------------------------------------------
# Get colors for consistency using Set3 palette
set_colors <- brewer.pal(8, "Set3")

# Create the main conceptual flow diagram with Set3 colors
flow_diagram <- grViz("
digraph model_flow {
  
  # Graph attributes
  graph [layout = dot, rankdir = TB, bgcolor = white, fontsize = 16]
  
  # Node attributes
  node [shape = box, style = filled, fontname = 'Arial', fontsize = 14]
  
  # Define node styles using Set3 palette colors 5-8 (blue, orange, green, pink)
  subgraph cluster_1 {
    label = 'AGENT INITIALIZATION'
    style = filled
    fillcolor = '#80B1D3'  # Set3 blue (color 5)
    fontsize = 16
    fontname = 'Arial'
    
    # Population generation
    pop [label = 'Generate Population (N agents)', fillcolor = '#B3D1E6']  # Lighter blue
    
    # Value assignment  
    values [label = 'Assign Value Profiles (A-E categories)\nBased on EA alignment', fillcolor = '#B3D1E6']
    
    # Agent types
    types [label = 'Assign Agent Types:\n• 1 Leader (highly value aligned)\n• N_EA EAs (mod-highly value aligned)\n• Non-EAs (low-moderately aligned)', fillcolor = '#B3D1E6']
  }
  
  subgraph cluster_2 {
    label = 'SIGNAL GENERATION'
    style = filled
    fillcolor = '#FDB462'  # Set3 orange (color 6)
    fontsize = 16
    fontname = 'Arial'
    
    # Signal generation
    signals [label = 'Generate 5 Signals per Agent \nSignals set to be either binary or categorical (5 levels)', 
                      fillcolor = '#FECA91']  # Light orange
    signals_prob [label = '\nP(EA signal) depends on\nvalue profile', 
                      fillcolor = '#FECA91']  # Light orange
  }
  
  subgraph cluster_3 {
    label = 'ALIGNMENT & CONNECTIONS'
    style = filled
    fillcolor = '#B3DE69'  # Set3 green (color 7)
    fontsize = 16
    fontname = 'Arial'
    
    # Connections
    connect [label = 'Create Full Network (all agents connected)', fillcolor = '#D1E9A4']  # Lighter green
    
    # Alignement calculation
    alignement [label = 'Calculate Perceived Alignment between all agent pairs\nBased on signal overlap', fillcolor = '#D1E9A4']
  }
  
  subgraph cluster_4 {
    label = 'RECRUITMENT PROCESS'
    style = filled
    fillcolor = '#FCCDE5'  # Set3 pink (color 8)
    fontsize = 16
    fontname = 'Arial'
    
    # EA perception
    ea_sim [label = 'Each non-EA calculates Perceived EA Alignment: \nWeighted average between alignment \nwith leader and EA peers.', fillcolor = '#FDE6F2']  # Light pink
    
    # Recruitment decision
    recruit [label = 'Recruitment Decision:\nP(join) = Perceived EA Alignment', fillcolor = '#FDE6F2']
  }
  
  # Define edges with labels
  pop -> values [label = '']
  values -> types [label = '']
  types -> signals [label = '']
  signals -> signals_prob [label = '']
  signals_prob -> connect [label = '']
  connect -> alignement [label = '']
  alignement -> ea_sim [label = '']
  ea_sim -> recruit [label = '']
  
  }
")

# Display the diagram
flow_diagram

# Save the diagram as a PNG file 
# This is a bit buggy, you can also just export it from the plot viewer
DiagrammeR::export_graph(flow_diagram, file_name = "flow_diagram.png", 
                         file_type = "png",
                         width = cm_to_pixels(16),)

# ------------------------------------------------------------------------------
# Figure 3: Value Distributions
# ------------------------------------------------------------------------------

# Generate data
data <- data.frame(x = rnorm(1000000, mean = 0, sd = 1))
data_groups <- data.frame(x = c(rep("A", 35),
                                rep("B", 57),
                                rep("C", 29),
                                rep("D", 3),
                                "A",
                                rep("A", 4),
                                rep("B", 71),
                                rep("C", 154),
                                rep("D", 121),
                                rep("E", 25)),
                          group = c(rep("EA", 124),
                                    "Leader",
                                    rep("Non-\nEA", 375)))
data_groups$x <- factor(data_groups$x, levels = c("E", "D", "C", "B", "A"))

# Create Figure 3A
p1 <- ggplot(data, 
             aes(x = x)) +
  geom_density(linewidth = 1, color = "#345da9", fill = "#859ecb", alpha = 0.7) +
  scale_x_continuous(limits = c(-4, 4), 
                     breaks = c(-4, 4),
                     labels = c("\nStrongly \nmisaligned",
                                "\nStrongly \naligned")) +
  labs(x = "EA Value Alignment", y = "Density", title = "Distribution of EA-Aligned \nValues in the Population") +
  scale_y_continuous(limits = c(0, 0.55)) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y  = element_line(color = "grey"),
    panel.grid.minor  = element_line(color = NA),
    panel.grid.major.x = element_line(color = NA),
    axis.ticks = element_line(color = NA),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 13),
    title = element_text(size = 11),
    axis.line.x = element_line(arrow = arrow(angle = 30,
                                             length = unit(0.15, "inches"),
                                             ends = "both", 
                                             type = "closed")))
# Create Figure 3B
p2 <- 
  ggplot(data, 
         aes(x = x)) +
  geom_density(linewidth = 1, color = "#345da9", fill = "#859ecb", alpha = 0.5) +
  geom_vline(xintercept = 1.5, color = "black", linetype = "dashed", linewidth=1) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed", linewidth=1) +
  geom_vline(xintercept = -0.5, color = "black", linetype = "dashed", linewidth=1) +
  geom_vline(xintercept = -1.5, color = "black", linetype = "dashed", linewidth=1) +
  annotate("text", x = -2.75, y = 0.5, label = str_c("E \n ", round(pnorm(Inf) - pnorm(1.5), 2) * 100, "%"), color = "black", size = 3.25) +
  annotate("text", x = -1, y = 0.5, label = str_c("D \n ", round(pnorm(1.5) - pnorm(0.5), 2)* 100, "%"), color = "black", size = 3.25) +
  annotate("text", x = 0, y = 0.5, label = str_c("C \n ", round(pnorm(0.5) - pnorm(-0.5), 2)* 100, "%"), color = "black", size = 3.25) +
  annotate("text", x = 1, y = 0.5, label = str_c("B \n ", round(pnorm(-0.5) - pnorm(-1.5), 2)* 100, "%"), color = "black", size = 3.25) +
  annotate("text", x = 2.75, y = 0.5, label = str_c("A \n ", round(pnorm(-1.5) - pnorm(-Inf), 2)* 100, "%"), color = "black", size = 3.25) +
  scale_x_continuous(limits = c(-4, 4), 
                     breaks = c(-4, 4),
                     labels = c("\nStrongly \nmisaligned",
                                "\nStrongly \naligned")) +
  scale_y_continuous(limits = c(0, 0.55)) +
  labs(x = "EA Value Alignment", y = "Density", title = "Value Profile Categories") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y  = element_line(color = "grey"),
    panel.grid.minor  = element_line(color = NA),
    panel.grid.major.x = element_line(color = NA),
    axis.ticks = element_line(color = NA),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 13),
    title = element_text(size = 11),
    axis.line.x = element_line(arrow = arrow(angle = 30,
                                             length = unit(0.15, "inches"),
                                             ends = "both", 
                                             type = "closed")))
# Create Figure 3C
p3 <- 
  ggplot(data_groups, aes(x = x, fill = group)) +
  geom_bar()+
  scale_fill_manual(values = c("#345da9", "#1f3968", "#e07f96"))+
  annotate("text", x = 1, y = nrow(filter(data_groups, x == "E"))+10, label = "E", color = "black", size = 4) +
  annotate("text", x = 2, y = nrow(filter(data_groups, x == "D"))+10, label = "D", color = "black", size = 4) +
  annotate("text", x = 3, y = nrow(filter(data_groups, x == "C"))+10, label = "C", color = "black", size = 4) +
  annotate("text", x = 4, y = nrow(filter(data_groups, x == "B"))+10, label = "B", color = "black", size = 4) +
  annotate("text", x = 5, y = nrow(filter(data_groups, x == "A"))+10, label = "A", color = "black", size = 4) +
  scale_y_continuous(limits = c(0, 210)) +
  scale_x_discrete(labels = c("\nStrongly \nmisaligned", "", "", "",
                              "\nStrongly \naligned")) +
  labs(x = "EA Value Alignment", 
       y = "Number of agents", 
       fill = "Agent \n type", 
       title = "Example Distribution of \nValue Profiles by Agent \nType",
       caption = "n-agents = 500 | movement-saturation = 25%" ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y  = element_line(color = "grey"),
    panel.grid.minor  = element_line(color = NA),
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
    legend.key.size = unit(0.55, "cm"))

# Combine into one labelled figure
((p1/p2)|p3) +
  plot_layout(widths = c(0.57,0.43))+
  plot_annotation(tag_levels = "A")
# save as png
ggsave("value_alignment_distribution.png", width = 19, height = 18, dpi = 1200, units = "cm")

# ------------------------------------------------------------------------------
# Figure 4: Conversion Rates Across the Parameter Space
# ------------------------------------------------------------------------------

# Read the data
df <- read.csv("sweep_data.csv")
df$n_agents <- factor(df$n_agents)
# Calculate summary statistics
summary_df <- df |> 
  filter(aggregation_method == "product") |>
  group_by(n_agents, movement_saturation) |> 
  summarise(mean_cr = mean(conversion_rate), 
            sd_cr = sd(conversion_rate), 
            .groups = "drop") |>
  mutate(ribbon_upper = ifelse(mean_cr + sd_cr > 1, 1, mean_cr + sd_cr),
         ribbon_lower = ifelse(mean_cr - sd_cr < 0, 0, mean_cr - sd_cr))
# Create custom facet labels
facet_labels <- function(x) {
  paste(x, "agents")
}
# Create the plot
p <- ggplot() +
  geom_line(data = filter(df, aggregation_method == "product"), 
            aes(x = movement_saturation, y = conversion_rate, 
                color = n_agents, 
                group = interaction(n_agents, run_id)), 
            alpha = 0.6) +
  geom_ribbon(data = summary_df, 
              aes(x = movement_saturation, 
                  ymin = ribbon_lower, 
                  ymax = ribbon_upper, 
                  fill = n_agents), 
              alpha = 0.4) +
  geom_line(data = summary_df, 
            aes(x = movement_saturation, y = mean_cr, color = n_agents), 
            size = 1) +
  geom_point(data = summary_df, 
             aes(x = movement_saturation, y = mean_cr, color = n_agents), 
             size = 1.8) +
  facet_wrap(~ n_agents, labeller = labeller(n_agents = facet_labels), axes = "all", ncol = 2) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), 
                     limits = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(x = "Movement Saturation (%)", 
       y = "Conversion Rate", 
       title = "Conversion Rate by Movement Saturation and \nPopulation Size") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y  = element_line(color = "grey"),
        panel.grid.minor  = element_line(color = NA),
        panel.grid.major.x = element_line(color = NA),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = "none",
        title = element_text(size = 11)) 
# Save the plot
ggsave("sweep_results.png", p, width = 15.9, height = 18, dpi = 1200, units = "cm")

# ------------------------------------------------------------------------------
# Figure 5: Estimated Weights by Bias Strength Parameter
# ------------------------------------------------------------------------------

bias_df <- data.frame(x = rep(seq(-4,4, by = 0.25), 3),
                      bias = rep(c(3, 1.5, 0), each = 33)) |> 
  mutate(y = exp(-x * bias),
         bias_type = ifelse(bias == 3, "Strong bias", "Moderate and no bias"),
         bias = factor(bias, labels = c("\nNo bias \n(bias=0)", "EAs: \nmoderate bias \n(bias =1.5)", "Leader: \nstrong bias \n(bias=3)"))
  )

# same as documentation but I fixed the axes
p4 <- ggplot() +
  geom_line(data = bias_df, 
            aes(x = x,
                y = y,
                color = bias),linewidth= 1.5)+
  geom_area(data = bias_df, aes(x = x, 
                                y = y, 
                                fill = bias, 
                                color = bias), 
            alpha = 0.5, position = "identity", show.legend = FALSE)+
  facet_wrap(~ bias_type, ncol = 1, scales = "free_y") +
  scale_x_continuous(limits = c(-4, 4), 
                     breaks = c(-4, 4),
                     labels = c("\nStrongly \naligned",
                                "\nStrongly \nmisaligned")) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(x = "EA Value Alignment", y = "Probability of Trait Value", title = "Probability of EA-Alignment Draws for Each Agent Type by Bias \nStrengths", color = "Bias \nlevel") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y  = element_line(color = "grey"),
    panel.grid.minor  = element_line(color = NA),
    panel.grid.major.x = element_line(color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 13, face = "bold"),
    axis.ticks = element_line(color = NA),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 13),
    title = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    axis.line.x = element_line(arrow = arrow(angle = 30,
                                             length = unit(0.15, "inches"),
                                             ends = "both", 
                                             type = "closed")))


bias_df_2 <- data.frame(bias = rep(seq(-3,3, by = 1), each = 33),
                        x = rep(seq(-4,4, by = 0.25), 7)) |> 
  mutate(y = exp(x * bias),
         bias = factor(bias))

p5 <- 
  ggplot(data = bias_df_2, 
         aes(x = x,
             y = y,
             color = bias)) +
  geom_line(linewidth= 1.5)+
  scale_x_continuous(limits = c(-4, 4), 
                     breaks = c(-3.2, 3.2),
                     labels = c("\nStrongly \naligned",
                                "\nStrongly \nmisaligned")) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  labs(x = "EA Value Alignment", y = "Weight", title = "Weights for EA-Alignment Values for a Range of Bias \nStrengths", color = "Bias \nlevel") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y  = element_line(color = "grey"),
    panel.grid.minor  = element_line(color = NA),
    panel.grid.major.x = element_line(color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 13, face = "bold"),
    axis.ticks = element_line(color = NA),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 13),
    title = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    axis.line.x = element_line(arrow = arrow(angle = 30,
                                             length = unit(0.15, "inches"),
                                             ends = "both", 
                                             type = "closed")))
(p4/p5) + 
  plot_annotation(tag_levels = "A")
ggsave("bias_weights.png", width = 15.9, height = 20, dpi = 1200, units = "cm")

# ------------------------------------------------------------------------------
# Improved version of Figure 5 - not in any documentation
# ------------------------------------------------------------------------------

p4a <- ggplot() +
  geom_density(data = data,aes(x=x),linewidth = 1, color = "#345da9", fill = "#859ecb", alpha = 0.3) +
  geom_vline(xintercept = 1.5, color = "black", linetype = "dashed", linewidth=1) +
  geom_vline(xintercept = 0.5, color = "black", linetype = "dashed", linewidth=1) +
  geom_vline(xintercept = -0.5, color = "black", linetype = "dashed", linewidth=1) +
  geom_vline(xintercept = -1.5, color = "black", linetype = "dashed", linewidth=1) +
  annotate("text", x = 2.75, y = 0.6, label = str_c("E \n ", round(pnorm(Inf) - pnorm(1.5), 2) * 100, "%"), color = "black", size = 3.25) +
  annotate("text", x = 1, y = 0.6, label = str_c("D \n ", round(pnorm(1.5) - pnorm(0.5), 2)* 100, "%"), color = "black", size = 3.25) +
  annotate("text", x = 0, y = 0.6, label = str_c("C \n ", round(pnorm(0.5) - pnorm(-0.5), 2)* 100, "%"), color = "black", size = 3.25) +
  annotate("text", x = -1, y = 0.6, label = str_c("B \n ", round(pnorm(-0.5) - pnorm(-1.5), 2)* 100, "%"), color = "black", size = 3.25) +
  annotate("text", x = -2.75, y = 0.6, label = str_c("A \n ", round(pnorm(-1.5) - pnorm(-Inf), 2)* 100, "%"), color = "black", size = 3.25) +
  geom_line(data = bias_df, 
            aes(x = x,
                y = y_norm,
                color = bias),linewidth= 1.5)+
  geom_area(data = bias_df, aes(x = x, 
                                y = y_norm, 
                                fill = bias, 
                                color = bias), 
            alpha = 0.5, position = "identity", show.legend = FALSE)+
  scale_x_continuous(limits = c(-4, 4), 
                     breaks = c(-4, 4),
                     labels = c("\nStrongly \naligned",
                                "\nStrongly \nmisaligned")) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_y_continuous(limits = c(0, 0.6), sec.axis = sec_axis(~., name="Density"))+
  labs(x = "EA Value Alignment", y = "Probability of Trait Value", title = "Probability of EA-Alignment Draws for Each Agent Type by Bias \nStrengths", color = "Bias \nlevel") +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major.y  = element_line(color = "grey"),
    panel.grid.minor  = element_line(color = NA),
    panel.grid.major.x = element_line(color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 13, face = "bold"),
    axis.ticks = element_line(color = NA),
    axis.text = element_text(color = "black", size = 12),
    axis.title = element_text(face = "bold", size = 13),
    title = element_text(size = 11),
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.position = "bottom",
    axis.line.x = element_line(arrow = arrow(angle = 30,
                                             length = unit(0.15, "inches"),
                                             ends = "both", 
                                             type = "closed")))



# ------------------------------------------------------------------------------
# Figure 6: Aggregation Methods’ Impact on Conversion Rate by Movement Saturation and Population size
# ------------------------------------------------------------------------------
# Read the data
df <- read.csv("sweep_data.csv")
df$n_agents <- factor(df$n_agents)
# Calculate summary statistics
summary_df <- df |> 
  group_by(n_agents, movement_saturation, aggregation_method) |> 
  summarise(mean_cr = mean(conversion_rate), 
            sd_cr = sd(conversion_rate), 
            .groups = "drop") |>
  mutate(ribbon_upper = ifelse(mean_cr + sd_cr > 1, 1, mean_cr + sd_cr),
         ribbon_lower = ifelse(mean_cr - sd_cr < 0, 0, mean_cr - sd_cr))
# Create custom facet labels
facet_labels <- function(x) {
  paste(x, "agents")
}
# Create the plot
p6 <- ggplot() +
  geom_line(data = df, 
            aes(x = movement_saturation, y = conversion_rate, 
                color = aggregation_method, 
                group = interaction(n_agents, run_id, aggregation_method)), 
            alpha = 0.6) +
  geom_ribbon(data = summary_df, 
              aes(x = movement_saturation, 
                  ymin = ribbon_lower, 
                  ymax = ribbon_upper, 
                  fill = aggregation_method), 
              alpha = 0.4) +
  geom_line(data = summary_df, 
            aes(x = movement_saturation, y = mean_cr, color = aggregation_method), 
            size = 1) +
  geom_point(data = summary_df, 
             aes(x = movement_saturation, y = mean_cr, color = aggregation_method), 
             size = 1.8) +
  facet_wrap(~ n_agents, labeller = labeller(n_agents = facet_labels), axes = "all", ncol = 2) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), 
                     limits = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(x = "Movement Saturation (%)", 
       y = "Conversion Rate", 
       title = "Conversion Rate by Movement Saturation and \nPopulation Size") +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.y  = element_line(color = "grey"),
        panel.grid.minor  = element_line(color = NA),
        panel.grid.major.x = element_line(color = NA),
        strip.background = element_rect(fill = "white", color = "black"),
        strip.text = element_text(size = 13, face = "bold"),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(face = "bold", size = 13),
        legend.position = "bottom",
        title = element_text(size = 11)) 
# Save the plot
ggsave("sweep_results.png", p, width = 15.9, height = 18, dpi = 1200, units = "cm")
